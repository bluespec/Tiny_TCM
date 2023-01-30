// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

// Near_Mem_IFC encapsulates the MMU and L1 cache.
// It is 'near' the CPU (1-cycle access in common case).

// On the CPU side it directly services instruction fetches and DMem
// reads and writes.

// On the Fabric side it has two Master sub-interfaces.
// One master sub-interface is used for instruction-memory access.
// The other master sub-interface is used for data-memory and I/O access.

// It can have various implementations:
//  - As an almost empty pass-through to the fabric
//  - As a cache (unified or separate I- and D-)
//        Fabric-side Server interface is not used (no back door to caches)
//  - As a TCM (Tightly-Coupled Memory)
//        Fabric-side IMem Client is not used (all fabric traffic is data or I/O mem)

// Macros Supported:
//    ISA_PRIV_S
//    FABRIC_AXI4 or FABRIC_AHBL or FABRIC_APB
//    SYNTHESIS

package Near_Mem_IFC;

// ================================================================
// BSV lib imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ----------------
// BSV additional libs

import Cur_Cycle :: *;

// ================================================================
// Project imports

import ISA_Decls        :: *;
import TCM_Decls        :: *;

import MMU_Cache_Common :: *;
import AXI4_Types       :: *;
import Fabric_Defs      :: *;

`ifdef FABRIC_AHBL
import AHBL_Types       :: *;
import AHBL_Defs        :: *;
`endif

`ifdef FABRIC_APB
import APB_Types        :: *;
import APB_Defs         :: *;
`endif

`ifdef FABRIC_GPIO
import GPIO_Decls       :: *;
import GPIO_Adapter     :: *;
`endif

import DM_CPU_Req_Rsp   :: *;   // for SB_Sys_Req

`ifdef ISA_X
import XTypes           :: *;
`endif


// ================================================================
// Near-Mem parameters (statically defined)

// Fabric parameters for DMA channel into the near-mem. For a TCM
// based system this serves as the backdoor loading mechanism.

typedef Wd_Id     Wd_Id_Dma;
typedef Wd_Addr   Wd_Addr_Dma;
typedef Wd_Data   Wd_Data_Dma;
typedef Wd_User   Wd_User_Dma;

// ================================================================
// Fabric parameters for memory channel into the near-mem. 

typedef Wd_Id    Wd_Id_Mem;
typedef Wd_Addr  Wd_Addr_Mem;
typedef Wd_Data  Wd_Data_Mem;
typedef Wd_User  Wd_User_Mem;

// Define the interface that the near-mem offers to the fabric based on the
// fabric protocol (AXI4/AHBL/APB)
`ifdef FABRIC_AXI4
typedef AXI4_Master_IFC #( Wd_Id_Mem
                         , Wd_Addr_Mem
                         , Wd_Data_Mem
                         , Wd_User_Mem) Near_Mem_Fabric_IFC;
`endif
`ifdef FABRIC_AHBL
typedef AHBL_Master_IFC #(AHB_Wd_Data) Near_Mem_Fabric_IFC;
`endif
`ifdef FABRIC_APB
typedef APB_Initiator_IFC Near_Mem_Fabric_IFC;
`endif
`ifdef FABRIC_GPIO
typedef GPIO_Fabric_IFC Near_Mem_Fabric_IFC;
`endif


// The interface type for loader access to the ITCM (always AXI4 for now)
typedef AXI4_Slave_IFC #(  Wd_Id_Dma
                         , Wd_Addr_Dma
                         , Wd_Data_Dma
                         , Wd_User_Dma) Near_Mem_DMA_IFC;

// ================================================================
// IMem and DMem Interfaces
interface IMem_IFC;
   method Action req (WordXL addr);
   method ActionValue #(Tuple2 #(Instr, Maybe #(Exc_Code))) instr;
endinterface

`ifdef INCLUDE_GDB_CONTROL
interface IMem_Dbg_IFC;
   method Action req (Bool read, Bit #(32) addr, Bit #(32) wdata, Bit #(3) f3);
   method ActionValue #(Tuple2 #(Instr, Bool)) rsp;
endinterface
`endif

interface DMem_IFC;
   method Action req (CacheOp op, Bit#(3) f3, WordXL addr, Bit#(32) store_value);
   interface Get #(Bit #(32))          word32;
`ifdef ISA_A
   interface Get #(Bit #(32))          final_st_val;
`endif
   interface Get #(Maybe #(Exc_Code))  exc;
endinterface

`ifdef TCM_LOADER
interface TCM_DMA_IFC;
   method Action req (Bit #(32) addr, Bit #(32) wdata);
endinterface
`endif

interface Near_Mem_IFC;
   // ----------------
   // Reset
   interface Server #(Token, Token) server_reset;

   // ----------------
   // IMem

   // CPU side
   interface IMem_IFC  imem;

   // ----------------
   // DMem

   // CPU side
   interface DMem_IFC  dmem;

   // Fabric side (MMIO initiator interface)
   interface Near_Mem_Fabric_IFC dmem_master;

   // ----------------
   // Fences

`ifdef ISA_PRIV_S
   interface Server #(Token, Token) sfence_vma_server;
`endif

`ifdef INCLUDE_GDB_CONTROL
   // ----------------------------------------------------------------
   // AXI4 DMA target interface (for backdoor/debug access of TCMs)
 
   interface Server #(SB_Sys_Req, SB_Sys_Rsp) dbg_server;
`endif

`ifdef TCM_LOADER
   interface Server #(SB_Sys_Req, Bool) dma_server;
`endif

`ifdef ISA_X
   interface Server #(X_M_Req, X_M_Rsp) x_server;
`endif

   // ----------------------------------------------------------------
   // Misc. control and status

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr

`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Fabric_Addr tohost_addr);
`endif

endinterface
   
`ifndef Near_Mem_TCM
// ================================================================
// Cache flush specs

Bit #(1) flush_to_invalid = 0;
Bit #(1) flush_to_clean   = 1;
`endif

// ================================================================
// Dummy tie-off interfaces for FIFOF

// dummy_FIFO that never accepts anything (always "full") or
// yields anything (always "empty")

FIFOF #(t) dummy_FIFOF = interface FIFOF;
   method Action enq (x) if (False);
      noAction;
   endmethod
   method notFull;
      return False;
   endmethod
   method first () if (False);
      return ?;
   endmethod
   method Action deq () if (False);
      noAction;
   endmethod
   method notEmpty;
      return False;
   endmethod
   method Action clear if (False);
      noAction;
   endmethod
endinterface;

// ================================================================
// Extract bytes from raw word read from near-mem.
// The bytes of interest are offset according to LSBs of addr.
// Arguments:
//  - a RISC-V LD/ST f3 value (encoding LB, LH, LW, LD, LBU, LHU, LWU)
//  - a byte-address
//  - a load-word (loaded from cache/mem)
// result:
//  - word with correct byte(s) shifted into LSBs and properly extended
// 
// Tiny TCM: Changed to handle 32-bit raw words only to reduce mux sizes

function Bit #(32) fn_extract_and_extend_bytes (Bit #(3) f3, WordXL byte_addr, Bit #(32) mem_word);
   Bit #(32) result    = 0;
   Bit #(2)  addr_lsbs = byte_addr [1:0];

   case (f3)
      f3_LB: case (addr_lsbs)
                'h0: result = signExtend (mem_word [ 7: 0]);
                'h1: result = signExtend (mem_word [15: 8]);
                'h2: result = signExtend (mem_word [23:16]);
                'h3: result = signExtend (mem_word [31:24]);
             endcase
      f3_LBU: case (addr_lsbs)
                'h0: result = zeroExtend (mem_word [ 7: 0]);
                'h1: result = zeroExtend (mem_word [15: 8]);
                'h2: result = zeroExtend (mem_word [23:16]);
                'h3: result = zeroExtend (mem_word [31:24]);
             endcase

      f3_LH: case (addr_lsbs)
                'h0: result = signExtend (mem_word [15: 0]);
                'h2: result = signExtend (mem_word [31:16]);
             endcase
      f3_LHU: case (addr_lsbs)
                'h0: result = zeroExtend (mem_word [15: 0]);
                'h2: result = zeroExtend (mem_word [31:16]);
             endcase

      f3_LW: result = mem_word;

      // the following cases are only possible in RV64 or with ISA-D. In either
      // case the assumption is that packing to 64-bits is handled outside this
      // near-mem.
      f3_LWU: result = mem_word;
      f3_LD: result = mem_word;
   endcase
   return result;
endfunction


// ================================================================
// Adjust byte for writes to TCM
// Arguments
//  - a RISC-V LD/ST f3 value (encoding LB, LH, LW, LD, LBU, LHU, LWU)
//  - a byte-address
//  - a store-word
// result: 2-tuple containing
//  - a byte-enable (0 if any error, e.g., misaligned)
//  - adjusted word (store bits shifted into correct byte positions)

function Tuple2 #(Bit #(Bytes_per_TCM_Word), // byte-enable
                  TCM_Word)                  // adjusted word
         fn_byte_adjust_write (Bit #(3) f3, Addr byte_addr, Bit#(32) word);

   Bit #(Bytes_per_TCM_Word) byte_en  = 0;   // If misaligned or illegal
   TCM_Word out_word = ?;

   Byte_in_TCM_Word addr_lsbs = byte_addr [(bits_per_byte_in_tcm_word-1):0];

   case ({1'b0, f3 [1:0]})
      // Bytes
      f3_LB: case (addr_lsbs)
         'h0: begin out_word [ 7: 0] = word [7:0]; byte_en = 'h1; end
         'h1: begin out_word [15: 8] = word [7:0]; byte_en = 'h2; end
         'h2: begin out_word [23:16] = word [7:0]; byte_en = 'h4; end
         'h3: begin out_word [31:24] = word [7:0]; byte_en = 'h8; end
      endcase

      // Halfwords (16b)
      f3_LH: case (addr_lsbs)
         'h0: begin out_word [15: 0] = word [15:0]; byte_en = 'h3; end
         'h2: begin out_word [31:16] = word [15:0]; byte_en = 'hC; end
      endcase

      // Words (32b)
      f3_LW: begin out_word = word; byte_en = 'hf; end

      // Doublewords (64b) -- XXX Unsupported
      f3_LD: begin out_word = word; byte_en = 'hf; end

   endcase
   return tuple2 (byte_en, out_word);
endfunction

function TCM_Word                  // adjusted word
         fn_byte_adjust_rmw (Bit #(3) f3, Addr byte_addr, Bit#(32) word, TCM_Word orig);

   TCM_Word out_word = orig;

   Byte_in_TCM_Word addr_lsbs = byte_addr [(bits_per_byte_in_tcm_word-1):0];

   case ({1'b0, f3 [1:0]})
      // Bytes
      f3_LB: case (addr_lsbs)
         'h0: begin out_word [ 7: 0] = word [7:0]; end
         'h1: begin out_word [15: 8] = word [7:0]; end
         'h2: begin out_word [23:16] = word [7:0]; end
         'h3: begin out_word [31:24] = word [7:0]; end
      endcase

      // Halfwords (16b)
      f3_LH: case (addr_lsbs)
         'h0: begin out_word [15: 0] = word [15:0]; end
         'h2: begin out_word [31:16] = word [15:0]; end
      endcase

      // Words (32b)
      f3_LW: begin out_word = word; end

      // Doublewords (64b) -- XXX Unsupported
      f3_LD: begin out_word = word; end

   endcase
   return out_word;
endfunction

// ================================================================
// Extract bytes from word read from fabric.
// The bytes of interest are already in the LSBs of 'word',
// they just have to be suitably extended.
// Arguments:
//  - a RISC-V LD/ST f3 value (encoding LB, LH, LW, LD, LBU, LHU, LWU)
//  - a byte-address
//  - a load-word (loaded from fabric)
// result:
//  - word with correct byte(s), properly extended.

function Bit #(64) fn_extend_bytes (Bit #(3) f3, Bit #(64) word64);
   Bit #(64) result = 0;
   case (f3)
      f3_LB:  result = signExtend (word64 [ 7: 0]);
      f3_LBU: result = zeroExtend (word64 [ 7: 0]);

      f3_LH:  result = signExtend (word64 [15: 0]);
      f3_LHU: result = zeroExtend (word64 [15: 0]);

      f3_LW:  result = signExtend (word64 [31: 0]);
      f3_LWU: result = zeroExtend (word64 [31: 0]);

      f3_LD:  result = word64;
   endcase

   return result;
endfunction

// ================================================================

endpackage: Near_Mem_IFC
