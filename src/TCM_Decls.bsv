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
`ifdef TCM_4K
typedef 4 KB_PER_TCM;
`elsif TCM_8K
typedef 8 KB_PER_TCM;
`elsif TCM_16K
typedef 16 KB_PER_TCM;
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
`elsif TCM_2048K
typedef 2048 KB_PER_TCM;
`elsif TCM_4096K
typedef 4096 KB_PER_TCM;
`else
typedef 4 KB_PER_TCM;   // Place holder default value
`endif
// --- USER CONFIGURABLE
//

// Naming of TCM hex file(s)
`ifdef HEXFILEPREFIX
String itcmname = `HEXFILEPREFIX + "itcm.hex";
String dtcmname = `HEXFILEPREFIX + "dtcm.hex";
`else
String itcmname = "/tmp/itcm.mem";
String dtcmname = "/tmp/dtcm.mem";
`endif

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

endpackage

