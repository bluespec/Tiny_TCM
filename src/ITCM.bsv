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
// import SoC_Map          :: *;

// ================================================================
// BRAM config constants
`ifdef REG_I_OUT
Bool config_output_register_BRAM = True;  // registered output
`else
Bool config_output_register_BRAM = False; // no output register
`endif
Bool load_file_is_binary_BRAM = False;    // load file is in hex format

// ================================================================
// Interface Definition

interface ITCM_IFC;
   // CPU side
   interface IMem_IFC  imem;

   // DMA server interface for back-door access to the ITCM
   // interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  dma_server;
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
   // The TCM RAM - dual-ported to allow backdoor to change IMem contents
   BRAM_PORT #(TCM_INDEX
                  , TCM_Word) mem  <- mkBRAMCore1 (  n_words_BRAM
/* BRAM_DUAL_PORT #(TCM_INDEX
                  , TCM_Word) mem  <- mkBRAMCore2 (  n_words_BRAM */
                                                   , config_output_register_BRAM);
`else
   // The TCM RAM - dual-ported to allow backdoor to change IMem contents
   BRAM_PORT #(TCM_INDEX
             , TCM_Word) mem  <- mkBRAMCore1Load (  n_words_BRAM
/* BRAM_DUAL_PORT #(TCM_INDEX
                  , TCM_Word) mem  <- mkBRAMCore2Load (  n_words_BRAM */
                                                       , config_output_register_BRAM
                                                       , "/tmp/itcm.mem"
                                                       , load_file_is_binary_BRAM);
`endif

   Reg #(Maybe #(Exc_Code)) rg_rsp_exc <- mkReg (tagged Invalid);
   Reg #(Bool) rg_rsp_valid <- mkReg (False);
`ifdef REG_I_OUT
   // to absorb the extra cycle of latency due to registered BRAM output
   Reg #(Bool) rg_rsp_valid_d <- mkReg (False);
`endif

   // SoC_Map_IFC soc_map <- mkSoC_Map;

   // The "front-door" to the itcm (port A)
   // let irom = mem.a;
   let irom = mem;

`ifdef REG_I_OUT
   // When the BRAM output is registered, an extra cycle is needed for the
   // response to be ready
   rule rl_schedule_rsp (rg_rsp_valid);
      rg_rsp_valid_d <= rg_rsp_valid;
      rg_rsp_pnd <= False;
   endrule
`endif

   // ----------------------------------------------------------------
   // INTERFACE

   // CPU side -- can only be invoked after earlier response has
   // been consumed
   interface IMem_IFC imem;
      // CPU interface: request
      // interface Put request;
      method Action req (WordXL addr) if (!rg_rsp_valid);
         // This method is used by ifetches only and the cache-op is assumed
         // to be always read and the size always 32-bit

         // The read to the RAM is initiated here.
         // We still don't know if the address is good and is indeed meant for
         // the TCM. Since it is a read, there is no side-effect and can be
         // safely initiated without waiting for all the results to come in
         // about the address.  If it is a CACHE_ST or AMO store, the
         // actual write happens in the response phase or AMO phase
         TCM_INDEX word_addr = truncate (addr >> bits_per_byte_in_tcm_word);
         irom.put (False, word_addr, ?);

         // for all the checks relating to the soc-map
         Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (addr);

         // Check if addr is word-aligned
         Bool is_aligned = fn_is_w_aligned (addr);

         // Report exceptions
         Maybe #(Exc_Code) exc;
         if (!is_aligned)
            exc = tagged Valid exc_code_INSTR_ADDR_MISALIGNED;
         else if (!(fn_is_itcm_addr (fabric_addr)))
            exc = tagged Valid exc_code_INSTR_ACCESS_FAULT;
         else exc = tagged Invalid;

         rg_rsp_exc   <= exc;
         rg_rsp_valid <= True;
      endmethod

`ifdef REG_I_OUT
      method ActionValue #(Tuple2 #(Instr, Maybe #(Exc_Code))) instr if (rg_rsp_valid_d);
         rg_rsp_valid_d <= False;
`else
      method ActionValue #(Tuple2 #(Instr, Maybe #(Exc_Code))) instr if (rg_rsp_valid);
         rg_rsp_valid <= False;
`endif
         return (tuple2 (irom.read, rg_rsp_exc));
      endmethod
   endinterface
endmodule
endpackage : ITCM
