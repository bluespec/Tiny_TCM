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
`ifdef NM_AXI4_LITE
import AXI4_Lite_Types  :: *;
`endif

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

import DM_Common        :: *;    // for fn_sbaccess_to_f3
import DM_CPU_Req_Rsp   :: *;    // for SB_Sys_Req

import Core_Map         :: *;
import ITCM             :: *;
import DTCM             :: *;

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
// Near_Mem_TCM module

(* synthesize *)
module mkNear_Mem (Near_Mem_IFC);

   // Verbosity: 0: quiet
   //            1: Requests and responses
   //            2: rule firings
   //            3: + detail
   Bit #(2) verbosity = 0;

   FIFOF #(Token) f_reset_reqs <- mkFIFOF1;
   FIFOF #(Token) f_reset_rsps <- mkFIFOF1;
   
   // don't need this read-vs-write record any more as we got rid of final_st_val
`ifdef INCLUDE_GDB_CONTROL
   FIFOF #(Bool) f_sb_read_not_write <- mkFIFOF1;
   FIFOF #(Bool) f_sb_imem_not_dmem  <- mkFIFOF1;
`endif

`ifdef ISA_X
`ifdef X_MEM
   FIFOF #(Bool) f_x_write <- mkFIFOF1;
`endif
`endif

`ifdef TCM_LOADER
   // Indicates error for a loader request
   FIFOF #(Bool) f_loader_err <- mkFIFOF;
`endif

`ifdef TCM_DP_SINGLE_MEM
   BRAM_DUAL_PORT_BE #(
        TCM_INDEX
      , TCM_Word
      , Bytes_per_TCM_Word) mem <- mkBRAMCore2BELoad ( n_words_BRAM 
                                                     , config_output_register_BRAM
                                                     , "/tmp/tcm.mem"
                                                     , load_file_is_binary_BRAM);
`endif

   // ----------------
   // Connections into the RAM

`ifdef TCM_DP_SINGLE_MEM
   DTCM_IFC dtcm <- mkDTCM   (mem.b, verbosity);
   ITCM_IFC itcm <- mkITCM   (mem.a, verbosity);
`else
   DTCM_IFC dtcm <- mkDTCM   (verbosity);
   ITCM_IFC itcm <- mkITCM   (verbosity);
`endif
   Core_Map_IFC addr_map <- mkCore_Map;

   rule rl_reset_start;
      dtcm.server_reset.request.put (?);
      itcm.server_reset.request.put (?);
      f_reset_reqs.deq;
      if (verbosity > 1) $display ("%06d:[D]:%m.rl_reset_start", cur_cycle);
   endrule

   rule rl_reset_complete;
      let d <- dtcm.server_reset.response.get ();
      let i <- itcm.server_reset.response.get ();
      f_reset_rsps.enq (?);
      if (verbosity > 1) $display ("%06d:[D]:%m.rl_reset_complete", cur_cycle);
   endrule

   // ================================================================
   // INTERFACE

   interface Server server_reset = toGPServer (f_reset_reqs, f_reset_rsps);

   // ----------------
   // IMem

   // CPU side
   interface imem = itcm.imem;

   // ----------------
   // DMem

   // CPU side
   interface dmem = dtcm.dmem;

   // Fabric side
   interface dmem_master = dtcm.mem_master;

`ifdef ISA_PRIV_S
   // ----------------
   // SFENCE_VMA: flush TLBs (no op in this module)
   method Action sfence_vma;
      noAction;
   endmethod
`endif

`ifdef INCLUDE_GDB_CONTROL
   // ----------------
   // Back-door from DM/System into Near_Mem
   interface Server dbg_server;
      interface Put request;
         method Action put (SB_Sys_Req req);
            // for all the checks relating to the soc-map
            Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (req.addr);
            Bool imem_not_dmem = True;

            // In the case of the single-ported implementation for
            // the MCU-L, all TCM debug accesses are serviced by
            // the ITCM while non-TCM debug accesses are sent to
            // the DTCM so that they can be serviced by the MMIO
`ifdef TCM_DP_SINGLE_MEM
            if (addr_map.m_is_tcm_addr (fabric_addr))
`else
            if (addr_map.m_is_itcm_addr (fabric_addr))
`endif
               itcm.backdoor.req (
                    req.read_not_write
                  , req.addr
                  , req.wdata
                  , fn_sbaccess_to_f3 (req.size)
               );
            else begin
               dtcm.dmem.req (
                    (req.read_not_write ? CACHE_LD : CACHE_ST)
                  , fn_sbaccess_to_f3 (req.size)
                  , truncate (req.addr)
                  , truncate (req.wdata)
`ifdef ISA_A
                  , amo_funct7   : ?
`endif
               );
               imem_not_dmem = False;
            end

            // Record read or write for the response path
            f_sb_read_not_write.enq (req.read_not_write);
            f_sb_imem_not_dmem.enq (imem_not_dmem);
         endmethod
      endinterface

      interface Get response;
         method ActionValue #(SB_Sys_Rsp) get;
            // Is it a read or a write?
            let read_not_write <- pop (f_sb_read_not_write);

            // Is the response expected from the IMem or DMem?
            let imem_not_dmem <- pop (f_sb_imem_not_dmem);

            // The response packet to the debug module
            let rsp = SB_Sys_Rsp {
                 rdata           : ?
               , read_not_write  : read_not_write
               , err             : False
            };

            if (imem_not_dmem) begin
               match {.rsp_imem, .err_imem} <- itcm.backdoor.rsp ();
               rsp.rdata = rsp_imem;
               rsp.err = err_imem;
            end

            else begin
               let rsp_dmem <- dtcm.dmem.word32.get ();
               let err_dmem <- dtcm.dmem.exc.get ();
               rsp.rdata = rsp_dmem;
               rsp.err = isValid (err_dmem);
            end

            return (rsp);
         endmethod
      endinterface
   endinterface
`endif

`ifdef TCM_LOADER
   // ----------------
   // DMA access into Near_Mem
   interface Server dma_server;
      interface Put request;
         method Action put (SB_Sys_Req req);
            // for all the checks relating to the soc-map
            Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (req.addr);
            let imem_not_dmem = addr_map.m_is_itcm_addr (fabric_addr);

            // Goodness checks - range, alignment, access size
            let addr_aligned = fn_is_aligned (
               (fn_sbaccess_to_f3 (req.size))[1:0], req.addr);

            let addr_in_range = (
                  addr_map.m_is_itcm_addr (fabric_addr)
               || addr_map.m_is_dtcm_addr (fabric_addr));

            let full_word_access = (fn_sbaccess_to_f3 (req.size) == f3_LW);
            let loader_err = !(addr_aligned && addr_in_range && full_word_access);

            if (addr_aligned && addr_in_range && full_word_access) begin
               // Write ITCM
               if (imem_not_dmem) itcm.dma.req (req.addr, req.wdata);

               // Write DTCM
               else dtcm.dma.req (req.addr, req.wdata);
            end

            f_loader_err.enq (loader_err);

            if (verbosity > 1) begin
               $display ("%06d:[D]:%m.dma_server.request", cur_cycle);
               if (verbosity > 2) begin
                  $display ("           ", fshow (req));
                  $display ("           (err ", fshow (loader_err), ")");
               end
            end
         endmethod
      endinterface

      interface Get response = toGet (f_loader_err);
   endinterface
`endif

`ifdef ISA_X
`ifdef X_MEM
   // ----------------
   // Back-door from DM/System into Near_Mem
   interface Server x_server;
      interface Put request;
         method Action put (X_M_Req req);
            // size fixed to full word read
            dtcm.dmem.req (
                 (req.write ? CACHE_ST : CACHE_LD)
               , 3'b010    // always 32-bit
               , truncate (req.address)
               , truncate (req.wdata)
`ifdef ISA_A
               , amo_funct7   : ?
`endif
            );

            // Record read or write for the response path
            f_x_write.enq (req.write);
         endmethod
      endinterface

      interface Get response;
         method ActionValue #(X_M_Rsp) get;
            // Is it a read or a write?
            let write <- pop (f_x_write);
            let rsp_dmem <- dtcm.dmem.word32.get ();
            let err_dmem <- dtcm.dmem.exc.get ();

            // The response packet to the accelerator
            let rsp = X_M_Rsp {
`ifdef RV32
                 rdata  : rsp_dmem
`else
                 rdata  : extend (rsp_dmem)
`endif
               , write  : write
               , err    : isValid (err_dmem)
            };

            return (rsp);
         endmethod
      endinterface
   endinterface
`endif
`endif

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr
`ifdef MEM_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Fabric_Addr tohost_addr);
      dtcm.set_watch_tohost (watch_tohost, tohost_addr);
   endmethod
`endif

endmodule: mkNear_Mem

`ifdef NM_AXI4_LITE
// ================================================================
//
// Near-Mem wrapped to provide AXI4 target interface to the CPU
// This wrapped version is to easily interface the Near-Mem to
// other CPUs that may not have our near-mems but speak a
// standard protocol like AXI4.
// 
// The wrapper uses the ARPROT value to differentiate between
// imem and dmem loads. All stores are to dmem only.
//
// TODO : Add pipelining -- currently processes one req at a time

function Bit #(3) fn_axi4size_to_nmf3 (AXI4_Size size);
   return (extend (size[1:0]));
endfunction

interface Near_Mem_AXI4_Wrap_IFC;
   // "CPU" side which talks AXI4
   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) nm_fe;

   // Fabric side (MMIO initiator interface)
   interface Near_Mem_Fabric_IFC dmem_master;
endinterface

(* synthesize *)
module mkNear_Mem_AXI4L_FE (Near_Mem_AXI4_Wrap_IFC);
   Bit #(2) verbosity = 0;
   Near_Mem_IFC  near_mem <- mkNear_Mem;
   AXI4_Lite_Slave_Xactor_IFC #(
      Wd_Addr, Wd_Data, Wd_User
   ) fe_xactor <- mkAXI4_Lite_Slave_Xactor;

   FIFOF #(Bool)           f_isI    <- mkFIFOF1;

   // --------
   // Forward read requests to the server
   rule rl_rd_req;
      let rra <- pop_o (fe_xactor.o_rd_addr);

      f_isI.enq (unpack (rra.arprot[2]));

      if (rra.arprot[2] == axprot_2_instr)
         near_mem.imem.req (rra.araddr);
      else
         near_mem.dmem.req (
              CACHE_LD
            , f3_LW     // always 32-bit since AXI4-lite
            , rra.araddr
            , ?
         );

      if (verbosity > 1) begin
         if (rra.arprot[2] == axprot_2_instr)
            $display ("%06d:[D]:%m.rl_rd_req:[I]", cur_cycle);
         else
            $display ("%06d:[D]:%m.rl_rd_req:[D]", cur_cycle);
         $display ("    ", fshow (rra));
      end
   endrule

   // Forward read responses to the client
   rule rl_rd_rsp;
      let rdr = AXI4_Lite_Rd_Data {
           rdata: ?
         , rresp: AXI4_LITE_OKAY
         , ruser: 0};

      let isI <- pop (f_isI);

      if (isI)  begin
         match {.instr, .iexc} <- near_mem.imem.instr;
         rdr.rdata = instr;
         if (isValid (iexc)) rdr.rresp = AXI4_LITE_SLVERR;
      end
      else begin
         let word32 <- near_mem.dmem.word32.get ();
         let dexc <- near_mem.dmem.exc.get ();
         rdr.rdata = word32;
         if (isValid (dexc)) rdr.rresp = AXI4_LITE_SLVERR;
      end

      // Send response
      fe_xactor.i_rd_data.enq (rdr);

      if (verbosity > 1) begin
         if (isI) $display ("%06d:[D]:%m.rl_rd_rsp:[I]", cur_cycle);
         else     $display ("%06d:[D]:%m.rl_rd_rsp:[D]", cur_cycle);
         $display ("    ", fshow (rdr));
      end
   endrule

   // --------
   // Forward write requests to the server
   rule rl_wr_req;
      let wra <- pop_o (fe_xactor.o_wr_addr);
      let wrd <- pop_o (fe_xactor.o_wr_data);

      // Based on write strobe create write data for near-mem
      Bit #(2)    offset  = 0;   // byte offset in 32-bit word
      Bit #(3)    f3      = f3_SB;
      Bit #(Wd_Data) mask = 'hff;
      // The offset shift to get the final st_value is only
      // required if the source does *not* replicate the write data
      // on all byte lanes. If the source replicates the write
      // data, simply anding with the mask will suffice.
      case (wrd.wstrb)
	 'hF:  begin offset=0; mask = 'hFFFF_FFFF; f3=f3_SW; end
	 'hC:  begin offset=2; mask =      'hFFFF; f3=f3_SH; end
	 'h3:  begin offset=0; mask =      'hFFFF; f3=f3_SH; end
	 'h8:  begin offset=3; mask =        'hFF; f3=f3_SB; end
	 'h4:  begin offset=2; mask =        'hFF; f3=f3_SB; end
	 'h2:  begin offset=1; mask =        'hFF; f3=f3_SB; end
	 'h1:  begin offset=0; mask =        'hFF; f3=f3_SB; end
      endcase

      let st_value = ((wrd.wdata >> {offset, 3'b0}) & mask);
      near_mem.dmem.req (
           CACHE_ST
         , f3
         , wra.awaddr + extend (offset)
         , st_value
      );

      if (verbosity > 1) begin
         $display ("%06d:[D]:%m.rl_wr_req", cur_cycle);
         $display ("    ", fshow (wra));
         $display ("    ", fshow (wrd));
      end
   endrule

   // Forward write responses to the client
   rule rl_wr_rsp;
      let word32 <- near_mem.dmem.word32.get ();
      let exc <- near_mem.dmem.exc.get ();

      // Send response
      let wrr = AXI4_Lite_Wr_Resp {
           bresp: (isValid (exc) ? AXI4_LITE_SLVERR : AXI4_LITE_OKAY)
         , buser: 0};
      fe_xactor.i_wr_resp.enq (wrr);

      if (verbosity > 1) begin
         $display ("%06d:[D]:%m.rl_wr_rsp", cur_cycle);
      end
   endrule

   interface nm_fe = fe_xactor.axi_side;
   interface dmem_master = near_mem.dmem_master;
endmodule
`endif

endpackage : Near_Mem_TCM
