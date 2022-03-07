// Copyright (c) 2022 Bluespec, Inc. All Rights Reserved.
//
// This package implements the ITCM and was hived off from
// Tiny_TCM's Near_Mem_TCM for maintainability reasons. Please
// refer to the introduction in Near_Mem_TCM for details.
//
// ----------------

package DTCM;

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

`ifdef FABRIC_APB
import APB_Types        :: *;
import APB_Defs         :: *;
import APB_Adapter      :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import DM_Common        :: *;
import DM_CPU_Req_Rsp   :: *;
`endif

// import SoC_Map          :: *;

// ================================================================
// BRAM config constants

Bool config_output_register_BRAM = False;    // i.e., no output register
Bool load_file_is_binary_BRAM = False;       // file to be loaded is in hex format

// ================================================================
// TCM interfaces
interface DTCM_IFC;
   // CPU side
   // interface Server #(Near_Mem_DReq, Near_Mem_DRsp)  dmem;
   interface DMem_IFC  dmem;

   // Fabric side
   // For accesses outside TCM (fabric memory, and memory-mapped I/O)
   interface Near_Mem_Fabric_IFC mem_master;

`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Fabric_Addr tohost_addr);
   method Fabric_Data mv_tohost_value;
`endif
endinterface

(* synthesize *)
module mkDTCM #(Bit #(2) verbosity) (DTCM_IFC);

   // Verbosity: 0: quiet
   //            1: Requests and responses
   //            2: rule firings
   //            3: + detail
   Bit#(2) verbosity_mmio = 0;
   Bit#(2) verbosity_fabric = 0;

   // Module state
   Reg #(Bool)                rg_rsp_from_mmio  <- mkReg (False);
   Reg #(Maybe #(Exc_Code))   rg_exc            <- mkReg (tagged Invalid);

   // SoC_Map_IFC soc_map <- mkSoC_Map;

   // ----------------
   // The RAM (used by DMem_Port and Fabric_Port). We could go for a DP
   // RAM when the BACK_DOOR is enabled. From a concurrency point-of-view the
   // extra port is not necessary as back-door access and regular accesses are
   // mutually exclusive. The only reason to go with DPRAMs is if we can move the
   // muxing between the two channels to hardened logic inside the BRAM cell.

   // The TCM RAM
`ifdef MICROSEMI
// BRAM_DUAL_PORT_BE #(  TCM_INDEX
   BRAM_PORT_BE #(  TCM_INDEX
                       , TCM_Word
//                     , Bytes_per_TCM_Word) mem  <- mkBRAMCore2BE (  n_words_BRAM
                       , Bytes_per_TCM_Word) mem  <- mkBRAMCore1BE (  n_words_BRAM
                                                                    , config_output_register_BRAM);
`else
// BRAM_DUAL_PORT_BE #(  TCM_INDEX
   BRAM_PORT_BE #(  TCM_INDEX
                       , TCM_Word
//                     , Bytes_per_TCM_Word) mem  <- mkBRAMCore2BELoad (  n_words_BRAM
                       , Bytes_per_TCM_Word) mem  <- mkBRAMCore1BELoad (  n_words_BRAM
                                                                        , config_output_register_BRAM
                                                                        , "/tmp/dtcm.mem"
                                                                        , load_file_is_binary_BRAM);
`endif

   // The "front-door" to the dtcm (port A)
   // let ram  = mem.a;
   let ram  = mem;

   // ----------------
   // Reservation regs for AMO LR/SC (Load-Reserved/Store-Conditional)

`ifdef ISA_A
   Reg #(Bool)                rg_lrsc_valid     <- mkReg (False);
   Reg #(PA)                  rg_lrsc_pa        <- mkRegU; // PA for an active LR
   Reg #(MemReqSize)          rg_lrsc_size      <- mkRegU;
`endif

   // Current request from the CPU
   // FIFOF #(MMU_Cache_Req) f_req    <- mkPipelineFIFOF;
   FIFOF #(MMU_Cache_Req) f_req    <- mkFIFOF1;

   // Response to the CPU
   FIFOF #(Bit #(32)) f_rsp_word32        <- mkFIFOF1;
`ifdef ISA_A
   FIFOF #(Bit #(32)) f_rsp_final_st_val  <- mkFIFOF1;
`endif
   FIFOF #(Maybe #(Exc_Code))  f_rsp_exc  <- mkFIFOF1;

`ifdef FABRIC_APB
   // The request and write data FIFOs need explicit EMPTY checking on the DEQ
   // side. This allows us to directly drive the APB signals from these FIFOs
   // removing the need for extra registers in the adapter
   FIFOF #(Single_Req)        f_mem_req   <- mkGFIFOF1 (False, True);
   FIFOF #(Bit #(32))         f_mem_wdata <- mkGFIFOF1 (False, True);
   FIFOF #(Read_Data)         f_mem_rdata <- mkFIFOF1;
`else
   // FIFOs to interact with external fabric (MMIO <-> AHB/AXI/APB)
   FIFOF #(Single_Req)        f_mem_req   <- mkFIFOF1;
   FIFOF #(Bit #(32))         f_mem_wdata <- mkFIFOF1;
   FIFOF #(Read_Data)         f_mem_rdata <- mkFIFOF1;
`endif

`ifndef SYNTHESIS
`ifdef WATCH_TOHOST
   // See NOTE: "tohost" above.
   // "tohost" addr on which to monitor writes, for standard ISA tests.
   // These are set by the 'set_watch_tohost' method but are otherwise read-only.
   Reg #(Bool)      rg_watch_tohost <- mkReg (True);
   Reg #(Fabric_Addr) rg_tohost_addr  <- mkRegU;
   Reg #(Fabric_Data) rg_tohost_value <- mkReg (0);
`endif
`endif

   // Access to fabric for non-TCM requests
   DMMIO_IFC        mmio            <- mkDMMIO (  f_req
                                                , f_rsp_word32
`ifdef ISA_A
                                                , f_rsp_final_st_val
`endif
                                                , f_rsp_exc
                                                , f_mem_req
                                                , f_mem_wdata
                                                , f_mem_rdata
                                                , rg_rsp_from_mmio
                                                , verbosity_mmio);

`ifdef FABRIC_AXI4
   TCM_AXI4_Adapter_IFC fabric_adapter <- mkTCM_AXI4_Adapter (
      verbosity_fabric, f_mem_req, f_mem_wdata, f_mem_rdata);
`endif
`ifdef FABRIC_AHBL
   TCM_AHBL_Adapter_IFC fabric_adapter <- mkTCM_AHBL_Adapter (
      verbosity_fabric, f_mem_req, f_mem_wdata, f_mem_rdata);
`endif
`ifdef FABRIC_APB
   APB_Adapter_IFC fabric_adapter <- mkAPB_Adapter (
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
         Addr byte_addr = req.va;
         let st_value  = req.st_value;
         let f3        = req.f3;

`ifdef ISA_A
         Maybe #(Bool) lrsc_fail = tagged Invalid;
         Bool sc_fail = False;

         // AMO SC request
         if (fv_is_AMO_SC (req)) begin
            if (rg_lrsc_valid && (rg_lrsc_pa == req.va)) begin
               if (verbosity >= 1) begin
                  $display ("%6d:[D]:%m.fav_write_to_ram:SC success", cur_cycle);
                  $display ("            (va 0x%08h) (data 0x%08h)", req.va, st_value);
               end
               // SC success: cancel LR/SC reservation
               rg_lrsc_valid <= False;
               lrsc_fail = tagged Valid False;// the response word should be 0
            end
            else begin 
               if (verbosity >= 1) begin
                  $display ("%6d:[D]:%m.fav_write_to_ram:SC fail", cur_cycle);
                  $display ("            (va 0x%08h) (data 0x%08h)", req.va, st_value);
               end
               lrsc_fail = tagged Valid True; // the response word should be 1
               sc_fail = True;
            end
         end

         // All AMO read-modify-writes (i.e., AMO other than LR and SC)
         else if (fv_is_AMO_RMW (req)) begin
            Fmt fmt_op = fshow_f5_AMO_op (req.amo_funct7 [6:2]);
            if (verbosity >= 1) begin
               $display ("%6d:[D]:%m.fav_write_to_ram: AMO ", cur_cycle, fmt_op);
               $display ("            (va 0x%08h) (rs2_val 0x%08h) (f3 %03b)", req.va, st_value, f3);
               $display ("            (load-result 0x%08h)", ram_data);
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
            f3, byte_addr, st_value);
         TCM_INDEX word_addr = truncate (byte_addr >> bits_per_byte_in_tcm_word);

         if (verbosity >= 1)
            $display ("      (RAM byte_en %08b) (RAM data %08h)"
               , byte_en, ram_st_value);

         // the actual write to the RAM - the only case when we
         // don't write is if there was a SC fail
`ifdef ISA_A
         if (! sc_fail)
`endif
            ram.put (byte_en, word_addr, ram_st_value);
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
   
// (* mutually_exclusive = "mmio_rl_read_rsp, rl_tcm_rsp" *)
// (* mutually_exclusive = "mmio_rl_write_req, rl_tcm_rsp" *)
//`ifdef ISA_A
//   (* mutually_exclusive = "imem_rl_rl_AMO_SC, rl_tcm_rsp" *)
//`endif
   // Drive response from TCM -- loads, LR, exceptions
   rule rl_tcm_rsp (!rg_rsp_from_mmio);
      // the incoming request
      let req = f_req.first; f_req.deq;

      // For CACHE_LD and LR, simply forward the RAM output
      let ram_out  = fn_extract_and_extend_bytes (
         req.f3, req.va, pack (ram.read));

      // the outgoing response
      let word32 = ram_out;

      // If the request involves a store (and there is nothing wrong with the
      // address), initiate the write.
      // In the case of RMWs, it will involve the current RAM output as well.
      if ((  (req.op == CACHE_ST)
`ifdef ISA_A
          || fv_is_AMO_SC (req)
          || fv_is_AMO_RMW (req)
`endif
          ) && (!isValid(rg_exc))) begin
`ifdef ISA_A
         match {.final_st_val, .lrsc_fail} <- fav_write_to_ram (req, ram_out);
         if (isValid (lrsc_fail)) word32 = extend (pack(lrsc_fail.Valid));
         f_rsp_final_st_val.enq (final_st_val);
`else
         let final_st_val <- fav_write_to_ram (req, ram_out);
`endif
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
      f_rsp_exc.enq (rg_exc);
      if (verbosity >= 1)
         $display ("%0d: %m.rl_tcm_rsp: (va %08h) (word32 %016h)"
            , cur_cycle, req.va, word32);
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

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

         // The read to the RAM is initiated here speculatively.
         // This read is specualtive because we still don't know if the
         // address is good and is indeed meant for the TCM. Since it is a
         // read, there is no side-effect and can be safely initiated without
         // waiting for all the results to come in about the address.
         // If it is a CACHE_ST or AMO store, the actual write
         // happens in the response phase or AMO phase
         TCM_INDEX word_addr = truncate (addr >> bits_per_byte_in_tcm_word);
         ram.put (0, word_addr, ?);

         // for all the checks relating to the soc-map
         Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (addr);

         // Check if f3 is legal, and if f3 and addr are compatible
         if (! fn_is_aligned (f3 [1:0], addr)) begin
            // Misaligned accesses not supported
            rg_exc            <= tagged Valid (fv_exc_code_misaligned (nm_req));
            rg_rsp_from_mmio  <= False;
         end

         // TCM reqs
         else if (fn_is_dtcm_addr (fabric_addr)) begin
            rg_exc            <= tagged Invalid;
            rg_rsp_from_mmio  <= False;
         end

         // non-TCM request (outside TCM addr range: could be memory or I/O on the fabric )
         else begin
            rg_exc            <= tagged Invalid;
            rg_rsp_from_mmio  <= True;
            mmio.start;
         end
      endmethod
      // endinterface

      // CPU interface: response
      interface Get  word32 = toGet (f_rsp_word32);
`ifdef ISA_A
      interface Get  final_st_val = toGet (f_rsp_final_st_val);
`endif
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

endmodule


endpackage : DTCM
