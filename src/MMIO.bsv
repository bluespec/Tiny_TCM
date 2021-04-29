// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package MMIO;

// ================================================================
// This module handles all load, store, and AMO ops for MMIO.
// - Loads and Stores go directly to mem
// - LR/SC are not supported: LR is treated like Load; SC always fails
// - AMO ops do a read-modify-write across the fabric
//    (CAVEAT: there is no 'locking' of the location at memory during
//     the operation, so it may not really be atomic.)
//
// The MMIO implements a simple req-rsp protocol for reads and
// writes and is independent of the NoC protocol used to implement
// the fabric (for instance, AXI4)

// ================================================================
// BSV lib imports

import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ----------------
// BSV additional libs

import Cur_Cycle     :: *;
import GetPut_Aux    :: *;

// ================================================================
// Project imports

import ISA_Decls        :: *;
import MMU_Cache_Common :: *;
import Near_Mem_IFC     :: *;

// ================================================================
// MODULE INTERFACE

interface IMMIO_IFC;
   method Action start;

   method Tuple2 #(Bool, Instr) result;

   interface Get #(Single_Req)  g_mem_req;
   interface Put #(Read_Data)   p_mem_read_data;
endinterface

interface DMMIO_IFC;
   method Action start;

   // method Tuple3 #(Bool, Bit #(64), Bit #(64)) result;

   interface Get #(Single_Req)  g_mem_req;
   interface Get #(Bit #(32))   g_write_data;
   interface Put #(Read_Data)   p_mem_read_data;
endinterface

// ================================================================

typedef enum {FSM_IDLE,
	      FSM_START,
	      FSM_READ_RSP} FSM_State
deriving (Bits, Eq, FShow);

// ================================================================
// MODULE IMPLEMENTATION -- non-TCM memory access support for data
// fetches (loads, stores, AMO)

module mkDMMIO #(
     FIFOF #(MMU_Cache_Req) f_req
   , FIFOF #(Bit #(32)) f_rsp_word32
   , FIFOF #(Bit #(32)) f_rsp_final_st_val
   , FIFOF #(Exc_Code)  f_rsp_exc_code
   , FIFOF #(Bool)      f_rsp_exc
   , Bit#(2) verbosity
) (DMMIO_IFC);
   
   Reg #(FSM_State) rg_fsm_state <- mkReg (FSM_IDLE);

   // Non-VM MMIO: PA = VA 
   let req = f_req.first;
   let req_pa  = fn_WordXL_to_PA (req.va);

   // ----------------
   // Memory interface

   FIFOF #(Single_Req)  f_single_reqs  <- mkFIFOF1;
   FIFOF #(Read_Data)   f_read_data    <- mkFIFOF1;
   FIFOF #(Bit #(32))   f_write_data   <- mkFIFOF1;

   // ----------------------------------------------------------------
   // Help-function for single-writes to mem

   function Action fa_mem_single_write (Bit #(32) st_value);
      action
	 // Lane-align the outgoing data
	 Bit #(5)  shamt_bits = { req_pa [1:0], 3'b000 };
	 Bit #(32) data       = (st_value << shamt_bits);

	 let r   = Single_Req {is_read:   False,
			       addr:      zeroExtend (req_pa),
			       size_code: req.f3 [1:0]};
	 f_single_reqs.enq (r);
	 f_write_data.enq (data);
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Issue read request to mem for load, LR, and AMO Read-Modify-Write
   // (all ops other than store and SC)

   rule rl_read_req ((rg_fsm_state == FSM_START)
		     && (req.op != CACHE_ST)
`ifdef ISA_A
		     && (! fv_is_AMO_SC (req))
`endif
                     );
      if (verbosity >= 1)
	 $display ("%0d: %m.rl_read_req: f3 %0h vaddr %0h  paddr %0h",
		   cur_cycle, req.f3, req.va, req_pa);
      let r   = Single_Req {is_read:   True,
			    addr:      zeroExtend (req_pa),
			    size_code: req.f3 [1:0]};
      f_single_reqs.enq (r);
      rg_fsm_state <= FSM_READ_RSP;
   endrule

   // ----------------------------------------------------------------
   // Receive read response from mem for Load, LR and AMO Read-Modify-Write
   // (all ops other than store and SC)

   rule rl_read_rsp (rg_fsm_state == FSM_READ_RSP);
      let read_data <- pop (f_read_data);

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_read_rsp: vaddr %0h  paddr %0h", cur_cycle, req.va, req_pa);
	 $display ("    ", fshow (read_data));
      end

      // the outgoing response fields
      Bool rsp_exc      = False;
      let  rsp_exc_code = fv_exc_code_access_fault (req);
      Bit #(32) rsp_word = ?;

      // Bus error
      if (! read_data.ok) begin
	 if (verbosity >= 1)
	    $display ("    MEM_RSP_ERR");

         rsp_exc       = True;
      end

      // Successful read
      else begin
	 let ld_val_bits = fv_from_byte_lanes (zeroExtend (req_pa), req.f3 [1:0], read_data.data);

	 // Loads and LR
	 if ((req.op == CACHE_LD) || fv_is_AMO_LR (req)) begin
	    let ld_val = fv_extend (req.f3, ld_val_bits);
            rsp_word = ld_val;
	    if (verbosity >= 1)
	      $display ("    Load or LR: f3 %0h ld_val %08h", req.f3, ld_val);
	 end
`ifdef ISA_A
	 // AMO read-modify-write
	 else begin
	    match {.final_ld_val,
		   .final_st_val} = fv_amo_op (req.f3 [1:0],
					       req.amo_funct7 [6:2],
					       ld_val_bits,
					       req.st_value);
	    // Write back final_st_val
	    fa_mem_single_write (final_st_val);
	    if (verbosity >= 1) begin
	      $display ("    AMO: f3 %0d  f7 %0h  ld_val %08h st_val %08h",
			req.f3, req.amo_funct7, ld_val_bits, req.st_value);
	      $display ("    => final_ld_val %0h final_st_val %08h",
			final_ld_val, final_st_val);
	    end
	    rsp_word = final_ld_val;
            f_rsp_final_st_val.enq (final_st_val);
	 end
`endif
      end

      rg_fsm_state    <= FSM_IDLE;
      f_rsp_word32.enq (rsp_word);
      f_rsp_exc.enq (rsp_exc);
      f_rsp_exc_code.enq (rsp_exc_code);
      f_req.deq;
   endrule

   // ----------------------------------------------------------------
   // Store requests

   rule rl_write_req ((rg_fsm_state == FSM_START) && (req.op == CACHE_ST));
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_write_req; f3 %0h  vaddr %0h  paddr %0h  word64 %0h",
		   cur_cycle, req.f3, req.va, req_pa, req.st_value);

      let data = fv_to_byte_lanes (zeroExtend (req_pa), req.f3 [1:0], req.st_value);

      fa_mem_single_write (data);

      // rg_final_st_val <= req.st_value;
      rg_fsm_state    <= FSM_IDLE;

      // the outgoing response
      f_rsp_exc.enq (False);
      f_rsp_exc_code.enq (fv_exc_code_access_fault (req));
      f_rsp_final_st_val.enq (req.st_value);
      f_req.deq;
   endrule

`ifdef ISA_A
   // ----------------------------------------------------------------
   // Memory-mapped I/O AMO_SC requests. Always fail (and never do the write)

   rule rl_AMO_SC ((rg_fsm_state == FSM_START) && fv_is_AMO_SC (req));

      // rg_ld_val    <= 1;    // 1 is LR/SC failure value
      rg_fsm_state <= FSM_IDLE;

      // the outgoing response
      f_rsp_exc.enq (False);
      f_rsp_exc_code.enq (fv_exc_code_access_fault (req));
      f_rsp_word64.enq (1);

      f_req.deq;

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_AMO_SC; f3 %0h  vaddr %0h  paddr %0h  st_value %0h",
		   cur_cycle, req.f3, req.va, req_pa, req.st_value);
	 $display ("    FAIL due to I/O address.");
      end
   endrule
`endif

   // ================================================================
   // INTERFACE

   method Action start;
      // rg_err <= False;
      rg_fsm_state <= FSM_START;
   endmethod

   // ----------------
   // Memory interface (for refills, writebacks)

   interface Get g_mem_req       = toGet (f_single_reqs);
   interface Get g_write_data    = toGet (f_write_data);
   interface Put p_mem_read_data = toPut (f_read_data);
endmodule

// ================================================================

endpackage
