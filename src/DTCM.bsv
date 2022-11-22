// Copyright (c) 2022 Bluespec, Inc. All Rights Reserved.
//
// This package implements the DTCM and was hived off from
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

`ifdef FABRIC_GPIO
import GPIO_Decls       :: *;
import GPIO_Adapter     :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import DM_Common        :: *;
import DM_CPU_Req_Rsp   :: *;
`endif

import Core_Map         :: *;

// ================================================================
// BRAM config constants

Bool config_output_register_BRAM = False;    // i.e., no output register
Bool load_file_is_binary_BRAM = False;       // file to be loaded is in hex format

// ================================================================
// Interface and local type definition
typedef enum { RST, RDY } DTCM_State deriving (Bits, Eq, FShow);

interface DTCM_IFC;
   interface Server #(Token, Token) server_reset;

   // CPU side
   // interface Server #(Near_Mem_DReq, Near_Mem_DRsp)  dmem;
   interface DMem_IFC  dmem;

   // Fabric side
   // For accesses outside TCM (fabric memory, and memory-mapped I/O)
   interface Near_Mem_Fabric_IFC mem_master;

`ifdef TCM_LOADER
   interface TCM_DMA_IFC dma;
`endif
endinterface


`ifdef TCM_DP_SINGLE_MEM
module mkDTCM #(
        BRAM_PORT_BE #(TCM_INDEX, TCM_Word, Bytes_per_TCM_Word) dmem_cpu
      , Bit #(2) verbosity) (DTCM_IFC);
`else
(* synthesize *)
module mkDTCM #(Bit #(2) verbosity) (DTCM_IFC);
`endif

   // Verbosity: 0: quiet
   //            1: Requests and responses
   //            2: rule firings
   //            3: + detail
   Bit#(2) verbosity_mmio = 0;
   Bit#(2) verbosity_fabric = 0;

   // Module state
   Reg #(Bool)                rg_rsp_from_mmio  <- mkReg (False);
   Reg #(Maybe #(Exc_Code))   rg_exc            <- mkReg (tagged Invalid);

   Core_Map_IFC addr_map <- mkCore_Map;

   // ----------------
   // The RAM (used by DMem_Port and Fabric_Port). We could go for a DP
   // RAM when the BACK_DOOR is enabled. From a concurrency point-of-view the
   // extra port is not necessary as back-door access and regular accesses are
   // mutually exclusive. The only reason to go with DPRAMs is if we can move the
   // muxing between the two channels to hardened logic inside the BRAM cell.


`ifdef TCM_DP_SINGLE_MEM
   let dmem_dbg = dmem_cpu;
`else
`ifdef MICROSEMI
// Microsemi devices do not have BRAMs that can be loaded with a file
`ifdef INCLUDE_GDB_CONTROL
   // The TCM RAM - dual-ported to allow backdoor debug access
   BRAM_DUAL_PORT_BE #(
        TCM_INDEX
      , TCM_Word
      , Bytes_per_TCM_Word) mem <- mkBRAMCore2BE ( n_words_BRAM 
                                                 , config_output_register_BRAM);
`else
`ifdef TCM_LOADER
   // The TCM RAM - dual-ported to allow backdoor loader access
   BRAM_DUAL_PORT_BE #(
        TCM_INDEX
      , TCM_Word
      , Bytes_per_TCM_Word) mem <- mkBRAMCore2BE ( n_words_BRAM 
                                                 , config_output_register_BRAM);
`else
   // The TCM RAM - single-ported - no GDB, no loader
   BRAM_PORT_BE #(
        TCM_INDEX
      , TCM_Word
      , Bytes_per_TCM_Word) mem <- mkBRAMCore1BE ( n_words_BRAM
                                                 , config_output_register_BRAM);
`endif   // TCM_LOADER
`endif   // GDB_CONTROL

`else
// Xilinx device BRAMs can be loaded with a file
`ifdef INCLUDE_GDB_CONTROL
   // The TCM RAM - dual-ported to allow backdoor debug access
   BRAM_DUAL_PORT_BE #(
        TCM_INDEX
      , TCM_Word
      , Bytes_per_TCM_Word) mem <- mkBRAMCore2BELoad ( n_words_BRAM 
                                                     , config_output_register_BRAM
                                                     , dtcmname
                                                     , load_file_is_binary_BRAM);
`else
`ifdef TCM_LOADER
   // The TCM RAM - dual-ported to allow backdoor loader access
   BRAM_DUAL_PORT_BE #(
        TCM_INDEX
      , TCM_Word
      , Bytes_per_TCM_Word) mem <- mkBRAMCore2BELoad ( n_words_BRAM 
                                                     , config_output_register_BRAM
                                                     , dtcmname
                                                     , load_file_is_binary_BRAM);
`else
   // The TCM RAM - single-ported with file loading - no GDB, no loader
   BRAM_PORT_BE #(
        TCM_INDEX
      , TCM_Word
      , Bytes_per_TCM_Word) mem <- mkBRAMCore1BELoad ( n_words_BRAM
                                                     , config_output_register_BRAM
                                                     , dtcmname
                                                     , load_file_is_binary_BRAM);
`endif   // TCM_LOADER
`endif   // GDB_CONTROL
`endif   // MICROSEMI

`ifdef INCLUDE_GDB_CONTROL
   // GDB defined
   let dmem_cpu = mem.a; 
   let dmem_dbg = mem.b;
`else
`ifdef TCM_LOADER
   // GDB not defined, Loader is defined
   let dmem_cpu = mem.a; 
   let dmem_dbg = mem.b;
`else
   // GDB and Loader not defined
   let dmem_cpu = mem;
`endif   // LOADER
`endif   // GDB_CONTROL
`endif   // TCM_DP_SINGLE_MEM

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
   // FIFOs to interact with external fabric (MMIO <-> APB)
   FIFOF #(Single_Req)        f_mem_req   <- mkGFIFOF1 (False, True);
   FIFOF #(Write_Data)        f_mem_wdata <- mkGFIFOF1 (False, True);
   FIFOF #(Read_Data)         f_mem_rdata <- mkFIFOF1;
`else
   // FIFOs to interact with external fabric (MMIO <-> AHB/AXI/GPIO)
   FIFOF #(Single_Req)        f_mem_req   <- mkFIFOF1;
   FIFOF #(Write_Data)        f_mem_wdata <- mkFIFOF1;
   FIFOF #(Read_Data)         f_mem_rdata <- mkFIFOF1;
`endif

   FIFOF #(Token) f_reset_rsps <- mkFIFOF1;
   Reg #(DTCM_State) rg_state <- mkReg (RST);

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
`ifdef FABRIC_GPIO
   GPIO_Adapter_IFC fabric_adapter <- mkGPIO_Adapter (
      verbosity_fabric, f_mem_req, f_mem_wdata, f_mem_rdata);
`endif

   rule rl_reset (rg_state == RST);
      f_req.clear;
      f_rsp_word32.clear;
      f_rsp_exc.clear;
      rg_rsp_from_mmio  <= False;
      rg_exc            <= tagged Invalid;
`ifdef ISA_A
      f_rsp_final_st_val.clear;
      rg_lrsc_valid <= False;
`endif

      f_mem_req.clear;
      f_mem_wdata.clear;
      f_mem_rdata.clear;
      fabric_adapter.reset;
      mmio.reset;

      rg_state <= RDY;
   endrule

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
            dmem_cpu.put (byte_en, word_addr, ram_st_value);
`ifdef ISA_A
         Bit #(32) final_st_val = sc_fail ? 0 : ram_st_value;
`else
         Bit #(32) final_st_val = ram_st_value;
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
         req.f3, req.va, pack (dmem_cpu.read));

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

   interface Server server_reset;
      interface Put request;
         method Action put (Token token);
            rg_state <= RST;
            // This response is placed here, and not in rl_reset, because
            // reset_loop can happen on power-up, where no response is expected.
            f_reset_rsps.enq (?);
         endmethod
      endinterface
      interface Get response;
         method ActionValue #(Token) get if (rg_state == RDY);
            let token <- pop (f_reset_rsps);
            return token;
         endmethod
      endinterface
   endinterface


   // CPU side
   interface DMem_IFC dmem;
      // CPU interface: request
      // interface Put request;
      method Action req (
           CacheOp op
         , Bit #(3) f3
         , WordXL addr
         , Bit #(32) store_value
      ) if (rg_state == RDY);
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
         dmem_cpu.put (0, word_addr, ?);

         // for all the checks relating to the soc-map
         Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (addr);

         // Check if f3 is legal, and if f3 and addr are compatible
         if (! fn_is_aligned (f3 [1:0], addr)) begin
            // Misaligned accesses not supported
            rg_exc            <= tagged Valid (fv_exc_code_misaligned (nm_req));
            rg_rsp_from_mmio  <= False;
         end

         // TCM reqs
`ifdef TCM_DP_SINGLE_MEM
         else if (addr_map.m_is_tcm_addr (fabric_addr)) begin
`else
         else if (addr_map.m_is_dtcm_addr (fabric_addr)) begin
`endif
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
      interface Get word32;
         method ActionValue# (Bit #(32)) get if (rg_state == RDY);
            f_rsp_word32.deq;
            return (f_rsp_word32.first);
         endmethod
      endinterface
`ifdef ISA_A
      interface Get final_st_val;
         method ActionValue# (Bit #(32)) get if (rg_state == RDY);
            f_rsp_final_st_val.deq;
            return (f_rsp_final_st_val.first);
         endmethod
      endinterface
`endif
      interface Get exc;
         method ActionValue# (Maybe #(Exc_Code)) get if (rg_state == RDY);
            f_rsp_exc.deq;
            return (f_rsp_exc.first);
         endmethod
      endinterface
   endinterface

   // Fabric side
   // For accesses outside TCM (fabric memory, and memory-mapped I/O)
`ifdef FABRIC_GPIO
   interface mem_master = fabric_adapter.gpio;
`else
   interface mem_master = fabric_adapter.mem_master;
`endif

`ifdef TCM_LOADER
   interface TCM_DMA_IFC dma;
      method Action req (Bit #(32) addr, Bit #(32) wdata) if (rg_state == RDY);

         // Assuming that all DMA accesses to the DTCM are full word only
         TCM_INDEX word_addr = truncate (addr >> bits_per_byte_in_tcm_word);
         // match {.byte_en, .st_val} = fn_byte_adjust_write (f3, addr, wdata);

         dmem_dbg.put ('hF, word_addr, wdata);

         if (verbosity > 1) begin
            $display ("%06d:[D]:%m.backdoor.req", cur_cycle);
            if (verbosity > 2) begin
               $display ("           (addr 0x%08h) (wdata 0x%08h)"
                  , addr, wdata);
            end
         end
      endmethod
   endinterface
`endif
   // ----------------------------------------------------------------
   // Misc. control and status

endmodule

endpackage : DTCM
