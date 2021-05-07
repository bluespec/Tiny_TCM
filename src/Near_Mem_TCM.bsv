// Copyright (c) 2016-2021 Bluespec, Inc. All Rights Reserved.

// Near_Mem_IFC is an abstraction of two alternatives: caches or TCM
// (TCM = Tightly Coupled Memory).  Both are memories that are
// 'near' the CPU (1-cycle access in common case).

// On the CPU side it directly services instruction fetches and DMem
// reads and writes.

// On the Fabric side it has two Master sub-interfaces and one Slave
// sub-interface.  The Master sub-interfaces are used for memory and
// memory-mapped I/O requests/responses from the CPU to the fabric.
// There are two Master interfaces, for concurrent IMem and DMem
// access.  The Slave sub-interface is used in the TCM variant for
// back-door access from the fabric to the TCM.

// This implementation of Near_Mem contains a TCM (Tightly Coupled Memory).
// - TCM is not a cache; it's just an SRAM/BRAM servicing a segment of
//     the address space. Accesses to other addresses (other memory, and
//     memory-mapped I/O) are still serviced by the Fabric. TCMs:
//     - have a 100% 'hit rate' for CPU access
//     - have a latency of exactly 1 cycle, and
//     - have a throughput of exactly 1 access/cycle.
//     and thus deliver best-case CPI performance (Cycles per Instruction).
//     Overall CPI can still be > 1 for reasons other than memory access
//     - Stalls due to pipeline dependencies (branches, register hazards, ...)
//     - Accesses to the Fabric (non-TCM memory and memory-mapped I/O)

// In this implementation, Instruction-Fetches are assumed always to
// be serviced by the TCM, and so the the Near_Mem_IFC sub-interface
// imem_to_fabric is unused (stubbed out).

// The sub-interface 'near_mem_slave' enables 'back-door' access of
// TCM memory by devices and debuggers.

// ----------------
// NOTE: "tohost"
// Special (fragile) ad hoc support for standard ISA tests during
// simulation: watch writes to physical addr <tohost> and stop on
// non-zero write.  This activity is done here rather than at memory
// because, in the standard ISA tests, the <tohost> addr is within the
// cacheable memory region, and therefore may never be written back to
// memory.  The actual address is supplied via the 'set_watch_tohost'
// method.  Standard ISA tests terminate by writing a non-zero value
// to the <tohost> addr. Bit [0] is always 1. Bits [n:1] specify which
// specific sub-test within the test failed.
//
// This logic is not meant to be included in the synthesizable version.
// ----------------


package Near_Mem_TCM;

// ================================================================
// BSV lib imports

import ConfigReg        :: *;
import SpecialFIFOs     :: *;
import FIFOF            :: *;
import GetPut           :: *;
import ClientServer     :: *;
import BRAMCore         :: *;
import Connectable      :: *;

// ----------------
// Additional libs

import Cur_Cycle        :: *;
import GetPut_Aux       :: *;
import Semi_FIFOF       :: *;
import ByteLane         :: *;

// ================================================================
// Project imports

import ISA_Decls        :: *;
import TCM_Decls        :: *;
import Near_Mem_IFC     :: *;
import MMU_Cache_Common :: *;
import MMIO             :: *;
`ifdef FABRIC_AXI4
import TCM_AXI4_Adapter :: *;
`endif

import Fabric_Defs      :: *;
import AXI4_Types       :: *;

`ifdef FABRIC_AHBL
import AHBL_Types       :: *;
import AHBL_Defs        :: *;
import TCM_AHBL_Adapter :: *;
`endif

import SoC_Map          :: *;

// ================================================================
// Exports

export mkNear_Mem;

// ================================================================
// BRAM config constants

Bool config_output_register_BRAM = False;    // i.e., no output register
Bool load_file_is_binary_BRAM = False;       // file to be loaded is in hex format

// ================================================================
// Dummy server interfaces to stub off fence requests
function Server #(Token, Token) fv_dummy_server_stub;
   return (
      interface Server
         interface Put request;
            method Action put (Token t);
               noAction;
            endmethod
         endinterface
      interface Get response;
         method ActionValue #(Token) get;
            noAction;
            return (?);
         endmethod
      endinterface
   endinterface);
endfunction

// ================================================================
// TCM interfaces
interface DTCM_IFC;
   method Action  reset;

   // CPU side
   // interface Server #(Near_Mem_DReq, Near_Mem_DRsp)  dmem;
   interface DMem_IFC  dmem;

   // Fabric side
   // For accesses outside TCM (fabric memory, and memory-mapped I/O)
`ifdef FABRIC_AXI4
   interface Near_Mem_Fabric_IFC mem_master;
`endif
`ifdef FABRIC_AHBL
   interface AHBL_Master_IFC #(AHB_Wd_Data) mem_master;
`endif

   // Inform core that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;

`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Fabric_Addr tohost_addr);
   method Fabric_Data mv_tohost_value;
`endif

   // Misc. status; 0 = running, no error
   (* always_ready *)
   method Bit #(8) mv_status;
endinterface

// ================================================================
// Near_Mem_TCM module

(* synthesize *)
module mkNear_Mem (Near_Mem_IFC);

   // Verbosity: 0: quiet
   //            1: Requests and responses
   //            2: rule firings
   //            3: + detail
   Bit #(2) verbosity = 3;

   FIFOF #(Token) f_reset_rsps <- mkFIFOF1;

   // ----------------
   // The RAM (used by IMem_Port, DMem_Port and Fabric_Port)

`ifdef SYNTHESIS
`ifdef TCM_BACK_DOOR
   BRAM_DUAL_PORT_BE #(Addr, TCM_Word, Bytes_per_TCM_Word) ram
      <- mkBRAMCore2BE (n_words_BRAM, config_output_register_BRAM);
`else
   BRAM_PORT_BE #(Addr, TCM_Word, Bytes_per_TCM_Word) ram
      <- mkBRAMCore1BE (n_words_BRAM, config_output_register_BRAM);
`endif
`else
`ifdef TCM_BACK_DOOR
   BRAM_DUAL_PORT_BE #(Addr, TCM_Word, Bytes_per_TCM_Word) ram
      <- mkBRAMCore2BELoad (n_words_BRAM, config_output_register_BRAM, "tcm.hex", load_file_is_binary_BRAM);
`else
   BRAM_PORT_BE #(Addr, TCM_Word, Bytes_per_TCM_Word) ram
      <- mkBRAMCore1BELoad (n_words_BRAM, config_output_register_BRAM, "tcm.hex", load_file_is_binary_BRAM);
`endif
`endif

   // ----------------
   // Connections into the RAM

`ifdef TCM_BACK_DOOR
   let dmem_port <- mkDTCM   (ram.a, verbosity); // Uses port A only
   let dma_port  <- mkTCM_DMA_AXI4_Adapter (ram.b, verbosity); // Uses port B only, at lower priority
`else
   let dmem_port <- mkDTCM   (ram, verbosity); // Uses port A only
`endif

   // Fence request/response queues
   FIFOF #(Token) f_fence_req_rsp <- mkFIFOF1;

   // ================================================================
   // INTERFACE

   // ----------------
   // Reset
   interface Server server_reset;
      interface Put request;
         method Action put (Token t);
            dmem_port.reset;
`ifdef TCM_BACK_DOOR
            dma_port.reset;
`endif

            f_reset_rsps.enq (?);
         endmethod
      endinterface

      interface Get response = toGet (f_reset_rsps);
   endinterface

   // ----------------
   // IMem

   // CPU side
   interface IMem_IFC imem;
      // CPU interface: request
      method Action req (Bit#(3) f3, WordXL addr);
         dmem_port.dmem.req (
              CACHE_LD
            , f3
            , addr
            , ?
`ifdef ISA_A
            , amo_funct7   : ?
`endif
`ifdef ISA_S
            , priv         : req.priv
            , sstatus_SUM  : req.sstatus_SUM
            , mstatus_MXR  : req.mstatus_MXR
            , satp         : req.satp
`endif
         );
      endmethod

      // CPU interface: response
      interface Get exc = dmem_port.dmem.exc;
      interface Get instr;
         method ActionValue#(Instr) get;
            let dmem_rsp_word32 <- dmem_port.dmem.word32.get();
            return (dmem_rsp_word32);
         endmethod
      endinterface
      interface Get exc_code;
         method ActionValue#(Exc_Code) get;
            let dmem_rsp_exc_code <- dmem_port.dmem.exc_code.get();
            return (exc_code_INSTR_ADDR_MISALIGNED);
         endmethod
      endinterface
        
      method Bool is_i32_not_i16 = True;
   endinterface

   // Fabric side
   interface imem_master = dummy_AXI4_Master_ifc;

   // ----------------
   // DMem

   // CPU side
   interface dmem = dmem_port.dmem;

   // Fabric side
   interface mem_master = dmem_port.mem_master;

   // ----------------
   // XXX Fence.I, Fence -- all fences are nops, right?
   interface server_fence_i = fv_dummy_server_stub ();

   interface Server server_fence;
      interface Put request;
         method Action put (Fence_Ordering fo);
            f_fence_req_rsp.enq (?);
         endmethod
      endinterface
      interface response = toGet (f_fence_req_rsp);
   endinterface

`ifdef ISA_PRIV_S
   // ----------------
   // SFENCE_VMA: flush TLBs (no op in this module)
   method Action sfence_vma;
      noAction;
   endmethod
`endif

`ifdef TCM_BACK_DOOR
   // ----------------
   // Back-door from fabric into Near_Mem
   interface dma_server = dma_port.dma_server;
`else
   interface dma_server = dummy_AXI4_Slave_ifc;
`endif

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr
`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Fabric_Addr tohost_addr);
      dmem_port.set_watch_tohost (watch_tohost, tohost_addr);
   endmethod

   method Fabric_Data mv_tohost_value = dmem_port.mv_tohost_value;
`endif

   // ----------------
   // Indication that the DDR4 is ready to both AXI4 adapters
   method Action ma_ddr4_ready = dmem_port.ma_ddr4_ready;

   // Misc. status; 0 = running, no error. Since it reports only write errors,
   // the imem_port will never report a non-zero status
   method Bit #(8) mv_status = dmem_port.mv_status;

endmodule: mkNear_Mem

// ================================================================
// DMem

// DMem_Port into the TCM
module mkDTCM #(
       BRAM_PORT_BE #(Addr, TCM_Word, Bytes_per_TCM_Word) ram
     , Bit #(2)                                           verbosity) (DTCM_IFC);

   // Verbosity: 0: quiet
   //            1: Requests and responses
   //            2: rule firings
   //            3: + detail
   Bit#(2) verbosity_mmio = 3;
   Bit#(2) verbosity_fabric = 3;

   // Module state
   Reg #(Bool)                rg_rsp_from_mmio  <- mkReg (False);
   Reg #(Bool)                rg_exc            <- mkReg (False);

   SoC_Map_IFC soc_map <- mkSoC_Map;

   // ----------------
   // Reservation regs for AMO LR/SC (Load-Reserved/Store-Conditional)

`ifdef ISA_A
   Reg #(Bool)                rg_lrsc_valid     <- mkReg (False);
   Reg #(PA)                  rg_lrsc_pa        <- mkRegU; // PA for an active LR
   Reg #(MemReqSize)          rg_lrsc_size      <- mkRegU;
`endif

   // Current request from the CPU
   FIFOF #(MMU_Cache_Req) f_req    <- mkPipelineFIFOF;
   // FIFOF #(MMU_Cache_Req) f_req    <- mkFIFOF1;

   // Response to the CPU
   FIFOF #(Bit #(32)) f_rsp_word32        <- mkBypassFIFOF;
   FIFOF #(Bit #(32)) f_rsp_final_st_val  <- mkBypassFIFOF;
   FIFOF #(Exc_Code)  f_rsp_exc_code      <- mkBypassFIFOF;
   FIFOF #(Bool)      f_rsp_exc           <- mkBypassFIFOF;

   // FIFOs to interact with external fabric (MMIO <-> AHB/AXI)
   FIFOF #(Single_Req)        f_mem_req         <- mkFIFOF1;
   FIFOF #(Bit #(32))         f_mem_wdata       <- mkFIFOF1;
   FIFOF #(Read_Data)         f_mem_rdata       <- mkFIFOF1;

`ifndef SYNTHESIS
`ifdef WATCH_TOHOST
   // See NOTE: "tohost" above.
   // "tohost" addr on which to monitor writes, for standard ISA tests.
   // These are set by the 'set_watch_tohost' method but are otherwise read-only.
   Reg #(Bool)      rg_watch_tohost <- mkReg (False);
   Reg #(Fabric_Addr) rg_tohost_addr  <- mkReg ('h_8000_1000);
   Reg #(Fabric_Data) rg_tohost_value <- mkReg (0);
`endif
`endif

   // Access to fabric for non-TCM requests
   DMMIO_IFC        mmio            <- mkDMMIO (  f_req
                                                , f_rsp_word32
                                                , f_rsp_final_st_val
                                                , f_rsp_exc_code
                                                , f_rsp_exc
                                                , f_mem_req
                                                , f_mem_wdata
                                                , f_mem_rdata
                                                , verbosity_mmio);

`ifdef FABRIC_AXI4
   TCM_AXI4_Adapter_IFC fabric_adapter<- mkTCM_AXI4_Adapter (verbosity_fabric);
`endif
`ifdef FABRIC_AHBL
   TCM_AHBL_Adapter_IFC fabric_adapter<- mkTCM_AHBL_Adapter (
      verbosity_fabric, f_mem_req, f_mem_wdata, f_mem_rdata);
`endif

   // ----------------------------------------------------------------
   // BEHAVIOR
   // This function generates the store word for the TCM depending
   // on the opcode. For AMO ops might involve some computation
   // with read data from the RAM. In case of SC fail, it returns
   // a valid value for the Bool maybe type
   function ActionValue #(
`ifdef ISA_A
      Tuple2 #(
`endif
           Bit #(32)
`ifdef ISA_A
         , Maybe #(Bool))
`endif
   ) fav_write_to_ram (MMU_Cache_Req req, Bit #(32) ram_data);

      actionvalue
         Fabric_Addr fabric_va = fv_Addr_to_Fabric_Addr (req.va);
         Addr tcm_byte_addr = fv_Fabric_Addr_to_Addr (
            fabric_va - soc_map.m_tcm_addr_base);
         let st_value  = req.st_value;
         let f3        = req.f3;

`ifdef ISA_A
         Maybe #(Bool) lrsc_fail = tagged Invalid;
         Bool sc_fail = False;

         // AMO SC request
         if (fv_is_AMO_SC (req)) begin
            if (rg_lrsc_valid && (rg_lrsc_pa == req.va)) begin
               if (verbosity >= 1) begin
                  $display ("%0d: %m.fav_write_to_ram: SC success", cur_cycle);
                  $display ("      (va %08h) (data %08h)", req.va, st_value);
               end
               // SC success: cancel LR/SC reservation
               rg_lrsc_valid <= False;
               lrsc_fail = tagged Valid False;// the response word should be 0
            end
            else begin 
               if (verbosity >= 1) begin
                  $display ("%0d: %m.fav_write_to_ram: SC fail", cur_cycle);
                  $display ("      (va %08h) (data %08h)", req.va, st_value);
               end
               lrsc_fail = tagged Valid True; // the response word should be 1
               sc_fail = True;
            end
         end

         // All AMO read-modify-writes (i.e., AMO other than LR and SC)
         else if (fv_is_AMO_RMW (req)) begin
            Fmt fmt_op = fshow_f5_AMO_op (req.amo_funct7 [6:2]);
            if (verbosity >= 1) begin
               $display ("%0d: %m.fav_write_to_ram: AMO ", cur_cycle, fmt_op);
               $display ("      (va %08h) (rs2_val %08h) (f3 %03b)", req.va, st_value, f3);
               $display ("      (load-result %08h)", ram_data);
            end

            let size_code  = f3 [1:0];
            // Do the AMO op on the loaded value and recalculate the st_value
            match {.new_ld_val, .value_after_op} = fv_amo_op (
               size_code, req.amo_funct7 [6:2], ram_data, st_value);

            if (verbosity >= 1)
               $display ("      ", fmt_op, " (%016h, %08h) -> %08h"
                  , ram_data, st_value, value_after_op);

            st_value = pack (value_after_op);

            // Cancel LR/SC reservation if this store is for this addr
            if (rg_lrsc_pa == req.va) rg_lrsc_valid <= False;
         end

         // CPU store request
         else if (req.op == CACHE_ST) begin
`endif
            if (verbosity >= 1) begin
               $display ("%0d: %m.fav_write_to_ram: ST", cur_cycle);
               $display ("      (va %08h) (data %08h)", req.va, st_value);
            end

`ifdef ISA_A
            // Cancel LR/SC reservation if this store is for this addr
            // TODO : should we cancel it on ANY store?
            if (rg_lrsc_pa == req.va) rg_lrsc_valid <= False;
         end
`endif

         // arrange the store bits in the appropriate byte lanes
         match {.byte_en, .ram_st_value} = fn_byte_adjust_write (
            f3, tcm_byte_addr, st_value);
         Addr tcm_word_addr = (tcm_byte_addr >> bits_per_byte_in_tcm_word);

         if (verbosity >= 1)
            $display ("      (RAM byte_en %08b) (RAM data %08h)"
               , byte_en, ram_st_value);

         // the actual write to the RAM - the only case when we
         // don't write is if there was a SC fail
`ifdef ISA_A
         if (! sc_fail)
`endif
            ram.put (byte_en, tcm_word_addr, ram_st_value);
`ifdef ISA_A
         Bit #(32) final_st_val = sc_fail ? 0 : ram_st_value;
`else
         Bit #(32) final_st_val = ram_st_value;
`endif

`ifndef SYNTHESIS
`ifdef WATCH_TOHOST
         // ----------------
         // "tohost" addr on which to monitor writes, for standard ISA tests.
         // See NOTE: "tohost" above.
         if (  (rg_watch_tohost)
            && (req.op == CACHE_ST)
            && (zeroExtend (req.va) == rg_tohost_addr)
            && (ram_st_value != 0)) begin
            rg_tohost_value <= ram_st_value;
            if (verbosity >= 1) begin
               let test_num = (ram_st_value >> 1);
               $display ("%0d: %m.fa_watch_tohost", cur_cycle);
               if (test_num == 0) $write ("    PASS");
               else               $write ("    FAIL <test_%0d>", test_num);
               $display ("  (<tohost>  addr %08h  data %08h)"
                  , req.va, ram_st_value);
            end
         end
`endif
`endif
         return (
`ifdef ISA_A
            tuple2 (
`endif
                 final_st_val
`ifdef ISA_A
               , lrsc_fail)
`endif
               );
      endactionvalue
   endfunction 
   
   (* mutually_exclusive = "mmio_rl_read_rsp, rl_tcm_rsp" *)
   (* mutually_exclusive = "mmio_rl_write_req, rl_tcm_rsp" *)
`ifdef ISA_A
   (* mutually_exclusive = "imem_rl_rl_AMO_SC, rl_tcm_rsp" *)
`endif
   // Drive response from TCM -- loads, LR, exceptions
   rule rl_tcm_rsp (!rg_rsp_from_mmio);
      // the incoming request
      let req = f_req.first; f_req.deq;

      // For CACHE_LD and LR, simply forward the RAM output
      let ram_out  = fn_extract_and_extend_bytes (
         req.f3, req.va, pack (ram.read));

      // the outgoing response
      let word32 = ram_out;

      // If the request involves a store, initiate the write
      // In the case of RMWs, it will involve the current RAM output as well.
      if (  (req.op == CACHE_ST)
`ifdef ISA_A
         || fv_is_AMO_SC (req)
         || fv_is_AMO_RMW (req)
`endif
         ) begin
`ifdef ISA_A
         match {.final_st_val, .lrsc_fail} <- fav_write_to_ram (req, ram_out);
         if (isValid (lrsc_fail)) word32 = extend (pack(lrsc_fail.Valid));
`else
         let final_st_val <- fav_write_to_ram (req, ram_out);
`endif
         f_rsp_final_st_val.enq (final_st_val);
      end

`ifdef ISA_A
      // For LR ops, update reservation regs
      if (fv_is_AMO_LR (req)) begin
         if (verbosity >= 1) $display ("%0d: %m.rl_tcm_rsp: LR-hit", cur_cycle);
         rg_lrsc_valid <= True;
         rg_lrsc_pa    <= req.va;
         rg_lrsc_size  <= req.f3 [1:0];
      end
`endif

      f_rsp_word32.enq (word32);
      f_rsp_exc_code.enq (fv_exc_code_misaligned (req));
      f_rsp_exc.enq (rg_exc);
      if (verbosity >= 1)
         $display ("%0d: %m.rl_tcm_rsp: (va %08h) (word32 %016h)"
            , cur_cycle, req.va, word32);
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset ();
      rg_rsp_from_mmio <= False;
      rg_exc <= False;

      if (verbosity > 1)
         $display ("%0d: %m.reset", cur_cycle);
   endmethod

   // CPU side
   interface DMem_IFC dmem;
      // CPU interface: request
      // interface Put request;
      method Action req (
           CacheOp op
         , Bit #(3) f3
         , WordXL addr
         , Bit #(32) store_value
      );
         // Note: ignoring all VM args for this version of Near_Mem_TCM
         // if (verbosity > 1) $display ("%0d: %m.req: ", cur_cycle, fshow (req));

         // This method is used by both ifetches and ld/st. In its present form it
         // does not distinguish between the two and makes the entire TCM
         // accessible to both which can be dangerous. If extra checks and
         // safeguards are needed to protect ifetches then an extra flag needs to
         // be added to the request which indicates that the request is an ifetch.

         // Some possible extra checks for ifetches (will cost resources):
         // 1. Only use a certain region in the TCM constituting the "itcm"
         // 2. Do not allow use of the MMIO

         // register the request for the response stage
         let nm_req = MMU_Cache_Req {
              op        : op
            , f3        : f3
            , va        : addr
            , st_value  : store_value
`ifdef ISA_A
            , amo_funct7: amo_funct7
`endif
         };
         f_req.enq (nm_req);

         // for all the checks relating to the soc-map
         Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (addr);

         // Check if f3 is legal, and if f3 and addr are compatible
         if (! fn_is_aligned (f3 [1:0], addr)) begin
            // Misaligned accesses not supported
            rg_exc            <= True;
            rg_rsp_from_mmio  <= False;
         end

         // TCM reqs
         else if (soc_map.m_is_tcm_addr (fabric_addr)) begin
            rg_exc            <= False;
            rg_rsp_from_mmio  <= False;

            // The read to the RAM is initiated here. If it is a
            // CACHE_ST or AMO store, the actual write happens in
            // the response phase or AMO phase
            Addr word_addr = fv_Fabric_Addr_to_Addr (
               (fabric_addr - soc_map.m_tcm_addr_base) >> bits_per_byte_in_tcm_word);
            ram.put (0, word_addr, ?);
         end

         // non-TCM request (outside TCM addr range: could be memory or I/O on the fabric )
         else begin
            rg_exc            <= False;
            rg_rsp_from_mmio  <= True;
            mmio.start;
         end
      endmethod
      // endinterface

      // CPU interface: response
      interface Get  word32 = toGet (f_rsp_word32);
      interface Get  final_st_val = toGet (f_rsp_final_st_val);
      interface Get  exc_code = toGet (f_rsp_exc_code);
      interface Get  exc = toGet (f_rsp_exc);
   endinterface

   // Fabric side
   // For accesses outside TCM (fabric memory, and memory-mapped I/O)
   interface mem_master = fabric_adapter.mem_master;

   // ----------------------------------------------------------------
   // Misc. control and status

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr (see NOTE: "tohost" above)

`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Fabric_Addr tohost_addr);
      rg_watch_tohost <= watch_tohost;
      rg_tohost_addr  <= tohost_addr;
      $display ("%0d: %m.set_watch_tohost: watch %0d, addr %08h",
                cur_cycle, watch_tohost, tohost_addr);
   endmethod

   method Fabric_Data mv_tohost_value;
      return rg_tohost_value;
   endmethod
`endif

   method Action ma_ddr4_ready = fabric_adapter.ma_ddr4_ready;

   // Misc. status; 0 = running, no error
   method Bit#(8) mv_status = fabric_adapter.mv_status;
endmodule

// ================================================================

endpackage : Near_Mem_TCM
