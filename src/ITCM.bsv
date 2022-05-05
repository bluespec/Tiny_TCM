// Copyright (c) 2022 Bluespec, Inc. All Rights Reserved.
//
// This package implements the ITCM and was hived off from
// Tiny_TCM's Near_Mem_TCM for maintainability reasons. Please
// refer to the introduction in Near_Mem_TCM for details.
//
// The interface of the ITCM makes certain assumptions about the
// state of the CPU. It expects that after the CPU requests for an
// instruction, it will wait to receive the response before it
// presents a new request.
//
// ----------------
  
package ITCM;

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
import Fabric_Defs      :: *;
import Core_Map         :: *;

// ================================================================
// BRAM config constants
`ifdef REG_I_OUT
Bool config_output_register_BRAM = True;  // registered output
`else
Bool config_output_register_BRAM = False; // no output register
`endif
Bool load_file_is_binary_BRAM = False;    // load file is in hex format

// ================================================================
// Interface and local type definition
typedef enum { RST, RDY } ITCM_State deriving (Bits, Eq, FShow);

interface ITCM_IFC;
   interface Server #(Token, Token) server_reset;

   // CPU side
   interface IMem_IFC  imem;

`ifdef INCLUDE_GDB_CONTROL
   // Server interface for back-door debug access to the ITCM
   interface IMem_Dbg_IFC backdoor;
`endif

`ifdef TCM_LOADER
   // DMA server interface for loader access to the ITCM
   interface TCM_DMA_IFC dma;
`endif
endinterface

// ================================================================
// Here begins the module
//
(* synthesize *)
module mkITCM #(Bit #(2) verbosity) (ITCM_IFC);

   // Verbosity: 0: quiet
   //            1: requests and responses
   //            2: rule firings
   //            3: + detail

`ifdef MICROSEMI
// Microsemi devices do not have BRAMs that can be loaded with a file
`ifdef INCLUDE_GDB_CONTROL
   // The TCM RAM - dual-ported to allow backdoor debug access
   BRAM_DUAL_PORT #(TCM_INDEX
                  , TCM_Word) mem  <- mkBRAMCore2 (  n_words_BRAM 
                                                   , config_output_register_BRAM);
`else
`ifdef TCM_LOADER
   // The TCM RAM - dual-ported to allow backdoor loader access
   BRAM_DUAL_PORT #(TCM_INDEX
                  , TCM_Word) mem  <- mkBRAMCore2 (  n_words_BRAM 
                                                   , config_output_register_BRAM);
`else
   // The TCM RAM - single-ported - no GDB, no loader
   BRAM_PORT #(     TCM_INDEX
                  , TCM_Word) mem  <- mkBRAMCore1 (  n_words_BRAM
                                                   , config_output_register_BRAM);
`endif
`endif

`else
// Xilinx device BRAMs can be loaded with a file
`ifdef INCLUDE_GDB_CONTROL
   // The TCM RAM - dual-ported to allow backdoor debug access
   BRAM_DUAL_PORT #(TCM_INDEX
                  , TCM_Word) mem  <- mkBRAMCore2Load (  n_words_BRAM 
                                                       , config_output_register_BRAM
                                                       , "/tmp/itcm.mem"
                                                       , load_file_is_binary_BRAM);
`else
`ifdef TCM_LOADER
   // The TCM RAM - dual-ported to allow backdoor loader access
   BRAM_DUAL_PORT #(TCM_INDEX
                  , TCM_Word) mem  <- mkBRAMCore2Load (  n_words_BRAM 
                                                       , config_output_register_BRAM
                                                       , "/tmp/itcm.mem"
                                                       , load_file_is_binary_BRAM);
`else
   // The TCM RAM - single-ported with file loading - no GDB, no loader
   BRAM_PORT #(     TCM_INDEX
                  , TCM_Word) mem  <- mkBRAMCore1Load (  n_words_BRAM
                                                       , config_output_register_BRAM
                                                       , "/tmp/itcm.mem"
                                                       , load_file_is_binary_BRAM);
`endif
`endif
`endif

   Reg #(ITCM_State) rg_state <- mkReg (RST);
   Reg #(Maybe #(Exc_Code)) rg_rsp_exc <- mkReg (tagged Invalid);
   FIFOF #(Token) rg_rsp_valid <- mkFIFOF;
`ifdef REG_I_OUT
   // to absorb the extra cycle of latency due to registered BRAM
   // output
   FIFOF #(Token) rg_rsp_valid_d <- mkFIFOF;
`endif
`ifdef INCLUDE_GDB_CONTROL
   Reg #(Bool) rg_dbg_rsp_valid <- mkReg (False);
   Reg #(Bool) rg_dbg_rsp_err   <- mkReg (False);
   Reg #(Bool) rg_read_not_write<- mkRegU;
   Reg #(Bit #(32)) rg_dbg_wdata<- mkRegU;
   Reg #(Bit #(32)) rg_dbg_addr <- mkRegU;
   Reg #(Bit #(3))  rg_dbg_f3   <- mkRegU;
`ifdef REG_I_OUT
   // to absorb the extra cycle of latency due to registered BRAM
   // output
   Reg #(Bool) rg_dbg_rsp_valid_d <- mkReg (False);
`endif
`endif
   Core_Map_IFC addr_map <- mkCore_Map;
   FIFOF #(Token) f_reset_rsps <- mkFIFOF1;

`ifdef INCLUDE_GDB_CONTROL
   // GDB defined
   let irom = mem.a; 
   let iram = mem.b;
`else
`ifdef TCM_LOADER
   // GDB not defined, Loader is defined
   let irom = mem.a; 
   let iram = mem.b;
`else
   // GDB and Loader not defined
   let irom = mem;
`endif
`endif

   rule rl_reset (rg_state == RST);
      rg_state <= RDY;
      rg_rsp_valid.clear;
      rg_rsp_exc <= tagged Invalid;
`ifdef REG_I_OUT
      rg_rsp_valid_d.clear;
`endif

`ifdef INCLUDE_GDB_CONTROL
      rg_dbg_rsp_valid <= False;
      rg_dbg_rsp_err   <= False;
`endif
   endrule

`ifdef REG_I_OUT
   // When the BRAM output is registered, an extra cycle is needed for
   // response to be ready. This applies to both dbg and non-dbg reqs
   rule rl_schedule_rsp;
      rg_rsp_valid.deq;
      rg_rsp_valid_d.enq (?);
   endrule

`ifdef INCLUDE_GDB_CONTROL
   rule rl_schedule_dbg_rsp (rg_dbg_rsp_valid);
      rg_dbg_rsp_valid_d  <= rg_dbg_rsp_valid;
      rg_dbg_rsp_valid    <= False;
   endrule
`endif
`endif

`ifdef INCLUDE_GDB_CONTROL
`ifdef REG_I_OUT
   rule rl_dbg_write_completion (
      rg_dbg_rsp_valid_d && !rg_read_not_write && !rg_dbg_rsp_err);
`else
   rule rl_dbg_write_completion (
      rg_dbg_rsp_valid && !rg_read_not_write && !rg_dbg_rsp_err);
`endif
      // A write request. Complete the read-modify-write
      // arrange the store bits in the appropriate byte lanes
      let ram_st_value = fn_byte_adjust_rmw (rg_dbg_f3, rg_dbg_addr, rg_dbg_wdata, iram.read);
      TCM_INDEX word_addr = truncate (rg_dbg_addr >> bits_per_byte_in_tcm_word);
      iram.put (True, word_addr, ram_st_value);
   endrule
`endif

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

   // CPU side -- can only be invoked after earlier response has
   // been consumed
   interface IMem_IFC imem;
      // CPU interface: request
      // interface Put request;
      method Action req (WordXL addr) if ((rg_state == RDY));
         // This method is used by ifetches only and the cache-op
         // is assumed to be always read and the size always 32-bit

         // The read to the RAM is initiated here.
         // We still don't know if the address is good and is meant
         // for the TCM. Since it is a read, there is no side-
         // effect and can be safely initiated without waiting for
         // all the results to come in on the address. 
         TCM_INDEX word_addr = truncate (addr >> bits_per_byte_in_tcm_word);
         irom.put (False, word_addr, ?);

         // for all the checks relating to the soc-map
         Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (addr);

         // Check if addr is word-aligned
         Bool is_aligned = fn_is_w_aligned (addr);

         // Report exceptions
         Maybe #(Exc_Code) exc;
         if (!is_aligned) begin
            exc = tagged Valid exc_code_INSTR_ADDR_MISALIGNED;
            $display ("%06d:[E]:%m.req: INSTR_ADDR_MISALIGNED", cur_cycle);
         end
         else if (!(addr_map.m_is_itcm_addr (fabric_addr))) begin
            exc = tagged Valid exc_code_INSTR_ACCESS_FAULT;
            $display ("%06d:[E]:%m.req: INSTR_ACCESS_FAULT", cur_cycle);
         end
         else exc = tagged Invalid;

         rg_rsp_exc   <= exc;
         rg_rsp_valid.enq (?);

         if (verbosity > 0) begin
            $display ("%06d:[D]:%m.imem.req", cur_cycle);
            if (verbosity > 1) begin
               $display ("           (addr 0x%08h)", addr);
            end
         end
      endmethod

`ifdef REG_I_OUT
      method ActionValue #(Tuple2 #(Instr, Maybe #(Exc_Code))) instr
         if ((rg_state == RDY));
         rg_rsp_valid_d.deq;
`else
      method ActionValue #(Tuple2 #(Instr, Maybe #(Exc_Code))) instr
         if ((rg_state == RDY));
         rg_rsp_valid.deq;
`endif
         if (verbosity > 0) begin
            $display ("%06d:[D]:%m.imem.instr", cur_cycle);
            if (verbosity > 1) begin
               $display ("           (instr 0x%08h) (exc "
                  , irom.read, fshow (rg_rsp_exc), ")");
            end
         end
         return (tuple2 (irom.read, rg_rsp_exc));
      endmethod
   endinterface

`ifdef INCLUDE_GDB_CONTROL
   interface IMem_Dbg_IFC backdoor;
      method Action req (
           Bool read_not_write
         , Bit #(32) addr
         , Bit #(32) wdata
         , Bit #(3)  f3)
`ifdef REG_I_OUT
         if ((rg_state == RDY) && (!rg_dbg_rsp_valid) && (!rg_dbg_rsp_valid_d));
`else
         if ((rg_state == RDY) && (!rg_dbg_rsp_valid));
`endif

         // read the RAM
         TCM_INDEX word_addr = truncate (addr >> bits_per_byte_in_tcm_word);
         iram.put (False, word_addr, ?);

         // Alignment check
         let is_aligned  = fn_is_aligned (f3 [1:0], addr);
         rg_dbg_rsp_err <= !is_aligned;

         // Read responses are available depending on RAM latency, 
         // write responses take a cycle more
         rg_dbg_rsp_valid <= True;
         rg_read_not_write <= read_not_write;
         rg_dbg_wdata <= wdata;
         rg_dbg_f3 <= f3;
         rg_dbg_addr <= addr;

         if (verbosity > 1) begin
            $display ("%06d:[D]:%m.backdoor.req", cur_cycle);
            if (verbosity > 1) begin
               $display ("           (addr 0x%08h) (wdata 0x%08h) (f3 0x%03b) (read "
                  , addr, wdata, f3, fshow (read_not_write), ")");
            end

            if (!is_aligned) 
               $display ("%06d:[E]:%m.backdoor.req: ADDR_MISALIGNED", cur_cycle);
         end
      endmethod

`ifdef REG_I_OUT
      method ActionValue #(Tuple2 #(Bit #(32), Bool)) rsp
         if ((rg_state == RDY) && (rg_dbg_rsp_valid_d));
         rg_dbg_rsp_valid_d <= False;
`else
      method ActionValue #(Tuple2 #(Bit #(32), Bool)) rsp
         if ((rg_state == RDY) && (rg_dbg_rsp_valid));
         rg_dbg_rsp_valid <= False;
`endif
         let ram_out  = fn_extract_and_extend_bytes (
            rg_dbg_f3, rg_dbg_addr, iram.read);

         if (verbosity > 0) begin
            $display ("%06d:[D]:%m.backdoor.rsp", cur_cycle);
            if (verbosity > 1) begin
               $display ("           (ram_out 0x%08h) (err "
                  , ram_out, fshow (rg_dbg_rsp_err), ")");
            end
         end

         return (tuple2 (ram_out, rg_dbg_rsp_err));
      endmethod
   endinterface
`endif
`ifdef TCM_LOADER
   interface TCM_DMA_IFC dma;
      method Action req (Bit #(32) addr, Bit #(32) wdata) if (rg_state == RDY);
         // Assuming that all DMA accesses to the ITCM are full word only
         TCM_INDEX word_addr = truncate (addr >> bits_per_byte_in_tcm_word);
         iram.put (True, word_addr, wdata);

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
endmodule
endpackage : ITCM
