// Copyright (c) 2020- Bluespec, Inc. All Rights Reserved.
// This package captures definitions used by the TCM logic

package TCM_Decls;
import Vector        :: *;
import ISA_Decls     :: *;
import Fabric_Defs   :: *; // Only for type Fabric_Addr

// TCM related type definitions
//
// --- USER CONFIGURABLE
typedef 32 TCM_XLEN;          // TCM Width

// TCM Sizing
`ifdef TCM_16K
typedef 16 KB_PER_TCM;
`elsif TCM_24K
typedef 24 KB_PER_TCM;
`elsif TCM_32K
typedef 32 KB_PER_TCM;
`elsif TCM_64K
typedef 64 KB_PER_TCM;
`elsif TCM_128K
typedef 128 KB_PER_TCM;
`elsif TCM_256K
typedef 256 KB_PER_TCM;
`elsif TCM_512K
typedef 512 KB_PER_TCM;
`elsif TCM_1024K
typedef 1024 KB_PER_TCM;
`endif
// --- USER CONFIGURABLE
//

typedef Bit #(TCM_XLEN)                   TCM_Word;
typedef TDiv #(TCM_XLEN, Bits_per_Byte)   Bytes_per_TCM_Word;
typedef TLog #(Bytes_per_TCM_Word)        Bits_per_Byte_in_TCM_Word;
typedef Bit #(Bits_per_Byte_in_TCM_Word)  Byte_in_TCM_Word;
typedef Vector #(Bytes_per_TCM_Word, Byte) TCM_Word_B;
Integer bytes_per_tcm_word        = valueOf (Bytes_per_TCM_Word);
Integer bits_per_byte_in_tcm_word = valueOf (Bits_per_Byte_in_TCM_Word);
Integer addr_lo_byte_in_tcm_word = 0;
Integer addr_hi_byte_in_tcm_word = addr_lo_byte_in_tcm_word + bits_per_byte_in_tcm_word - 1;

function  Byte_in_TCM_Word fn_addr_to_byte_in_tcm_word (Addr a);
   return a [addr_hi_byte_in_tcm_word : addr_lo_byte_in_tcm_word ];
endfunction

Integer kb_per_tcm =   valueOf (KB_PER_TCM);   // TCM Sizing
Integer bytes_per_TCM = kb_per_tcm * 'h400;

// LSBs to address a byte in the TCMs
typedef TAdd# (TLog# (KB_PER_TCM), TLog #(1024)) TCM_Addr_LSB;
Integer tcm_addr_lsb = valueOf (TCM_Addr_LSB);

// Indices into the TCM
typedef Bit #(TAdd #(TLog #(KB_PER_TCM), 8)) TCM_INDEX;//(KB*1024)/ bytes_per_tcm_word

// size of the BRAM in TCM_Word(s). Only handles powers of two.
Integer n_words_BRAM = (bytes_per_TCM / bytes_per_tcm_word);

   // ----------------------------------------------------------------
   // Tightly-coupled memory address definitions
   // When TCMs are enabled, the iTCM address base is at the address usually
   // used for the mem0_controller. This avoids changing the start location
   // of bare-metal programs.
   //
   // The "main" memory now starts from 0x1000_0000 later, effectively
   // leaving 256 MB for the two TCMs
   //
   // Currently the TCMs are of the same size, controlled by a
   // single tcm_addr_size value.
   Fabric_Addr itcm_addr_base = 'h_C000_0000;
   Fabric_Addr itcm_addr_size = fromInteger (bytes_per_TCM);
   Fabric_Addr itcm_addr_lim  = itcm_addr_base + itcm_addr_size;

   function Bool fn_is_itcm_addr (Fabric_Addr addr);
      Bit #(TSub #(Wd_Addr, TCM_Addr_LSB)) tcm_base_msb = truncate (
         itcm_addr_base >> tcm_addr_lsb); 
      Bit #(TSub #(Wd_Addr, TCM_Addr_LSB)) addr_msb = truncate (
         addr >> tcm_addr_lsb); 
      return (tcm_base_msb == addr_msb);
   endfunction

   Fabric_Addr dtcm_addr_base = 'h_C800_0000;
   Fabric_Addr dtcm_addr_size = fromInteger (bytes_per_TCM);
   Fabric_Addr dtcm_addr_lim  = dtcm_addr_base + dtcm_addr_size;

   function Bool fn_is_dtcm_addr (Fabric_Addr addr);
      Bit #(TSub #(Wd_Addr, TCM_Addr_LSB)) tcm_base_msb = truncate (
         dtcm_addr_base >> tcm_addr_lsb); 
      Bit #(TSub #(Wd_Addr, TCM_Addr_LSB)) addr_msb = truncate (
         addr >> tcm_addr_lsb); 
      return (tcm_base_msb == addr_msb);
   endfunction
endpackage

