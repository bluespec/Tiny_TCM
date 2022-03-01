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

`ifdef FABRIC_APB
import APB_Types        :: *;
import APB_Defs         :: *;
import APB_Adapter      :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import DM_Common        :: *;
import DM_CPU_Req_Rsp   :: *;
`endif

import SoC_Map          :: *;

import ITCM             :: *;
import DTCM             :: *;

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

   // FIFOF #(Token) f_reset_rsps <- mkFIFOF1;
   // don't need this read-vs-write record any more as we got rid of final_st_val
`ifdef INCLUDE_GDB_CONTROL
      FIFOF #(Bool) f_sb_read_not_write <- mkFIFOF1;
`endif

   // ----------------
   // Connections into the RAM

   DTCM_IFC dtcm <- mkDTCM   (verbosity);
   ITCM_IFC itcm <- mkITCM   (verbosity);

   // ================================================================
   // INTERFACE

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
   // XXX pending with the new disjointed TCM
   // Back-door from DM/System into Near_Mem
   interface Server dma_server;
      interface Put request;
         method Action put (SB_Sys_Req req);
            dmem_port.dmem.req (
                 (req.read_not_write ? CACHE_LD : CACHE_ST)
               , fn_sbaccess_to_f3 (req.size)
               , truncate (req.addr)
               , truncate (req.wdata)
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
            // Record read or write for the response path
            f_sb_read_not_write.enq (req.read_not_write);
         endmethod
      endinterface
      interface Get response;
         method ActionValue #(SB_Sys_Rsp) get;
            let rsp_rdata <- dmem_port.dmem.word32.get ();
            let rsp_exc <- dmem_port.dmem.exc.get ();

            // Drop the store value if this was a write
            let read_not_write <- pop (f_sb_read_not_write);
            // if (!read_not_write) dmem_port.dmem.final_st_val.get ();

            // Compose the response packet
            let rsp = SB_Sys_Rsp {
                 rdata           : rsp_rdata
               , read_not_write  : read_not_write
               , err             : isValid (rsp_exc)
            };
            return (rsp);
         endmethod
      endinterface
   endinterface
`endif

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr
`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Fabric_Addr tohost_addr);
      dmem_port.set_watch_tohost (watch_tohost, tohost_addr);
   endmethod

   method Fabric_Data mv_tohost_value = dmem_port.mv_tohost_value;
`endif

endmodule: mkNear_Mem

// ================================================================
// DMem

// DMem_Port into the TCM
// ================================================================

endpackage : Near_Mem_TCM
