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

Bit #(TCM_XLEN) bad_value = truncate(64'h_4B1D4B1D_4B1D4B1D);
/*
DTCMKB = 96
DTCMKB = 128
DTCMKB = 160
DTCMKB = 192
DTCMKB = 256

ITCMKB = 64
ITCMKB = 80
ITCMKB = 96
ITCMKB = 128
ITCMKB = 144
ITCMKB = 160
ITCMKB = 176
ITCMKB = 192
ITCMKB = 256
*/

// TCM Sizing
`ifdef ITCM_4K
typedef 4 KB_PER_ITCM;
`elsif ITCM_8K
typedef 8 KB_PER_ITCM;
`elsif ITCM_16K
typedef 16 KB_PER_ITCM;
`elsif ITCM_32K
typedef 32 KB_PER_ITCM;
`elsif ITCM_64K
typedef 64 KB_PER_ITCM;
`elsif ITCM_96K
typedef 96 KB_PER_ITCM;
`elsif ITCM_128K
typedef 128 KB_PER_ITCM;
`elsif ITCM_144K
typedef 144 KB_PER_ITCM;
`elsif ITCM_160K
typedef 160 KB_PER_ITCM;
`elsif ITCM_176K
typedef 176 KB_PER_ITCM;
`elsif ITCM_192K
typedef 192 KB_PER_ITCM;
`elsif ITCM_256K
typedef 256 KB_PER_ITCM;
`elsif ITCM_512K
typedef 512 KB_PER_ITCM;
`elsif ITCM_1024K
typedef 1024 KB_PER_ITCM;
`elsif ITCM_2048K
typedef 2048 KB_PER_ITCM;
`elsif ITCM_4096K
typedef 4096 KB_PER_ITCM;
`elsif ITCM_8192K
typedef 8192 KB_PER_ITCM;
`elsif ITCM_16384K
typedef 16384 KB_PER_ITCM;
`elsif ITCM_128M
typedef 131072 KB_PER_ITCM;
`else
typedef 4 KB_PER_ITCM;   // Place holder default value
`endif
/*
`ifdef DTCM_4K
typedef 4 KB_PER_DTCM;
`elsif DTCM_8K
typedef 8 KB_PER_DTCM;
`elsif DTCM_16K
typedef 16 KB_PER_DTCM;
`elsif DTCM_32K
typedef 32 KB_PER_DTCM;
`elsif DTCM_64K
typedef 64 KB_PER_DTCM;
`elsif DTCM_128K
typedef 128 KB_PER_DTCM;
`elsif DTCM_256K
typedef 256 KB_PER_DTCM;
`elsif DTCM_512K
typedef 512 KB_PER_DTCM;
`elsif DTCM_1024K
typedef 1024 KB_PER_DTCM;
`elsif DTCM_2048K
typedef 2048 KB_PER_DTCM;
`elsif DTCM_4096K
typedef 4096 KB_PER_DTCM;
`elsif DTCM_8192K
typedef 8192 KB_PER_DTCM;
`elsif DTCM_16384K
typedef 16384 KB_PER_DTCM;
`elsif DTCM_128M
typedef 131072 KB_PER_DTCM;
`else
typedef 4 KB_PER_DTCM;   // Place holder default value
`endif
*/
`ifdef DTCM_4K
typedef 4 KB_PER_DTCM;
`elsif DTCM_8K
typedef 8 KB_PER_DTCM;
`elsif DTCM_16K
typedef 16 KB_PER_DTCM;
`elsif DTCM_32K
typedef 32 KB_PER_DTCM;
`elsif DTCM_64K
typedef 64 KB_PER_DTCM;
`elsif DTCM_96K
typedef 96 KB_PER_DTCM;
`elsif DTCM_128K
typedef 128 KB_PER_DTCM;
`elsif DTCM_144K
typedef 144 KB_PER_DTCM;
`elsif DTCM_160K
typedef 160 KB_PER_DTCM;
`elsif DTCM_176K
typedef 176 KB_PER_DTCM;
`elsif DTCM_192K
typedef 192 KB_PER_DTCM;
`elsif DTCM_256K
typedef 256 KB_PER_DTCM;
`elsif DTCM_512K
typedef 512 KB_PER_DTCM;
`elsif DTCM_1024K
typedef 1024 KB_PER_DTCM;
`elsif DTCM_2048K
typedef 2048 KB_PER_DTCM;
`elsif DTCM_4096K
typedef 4096 KB_PER_DTCM;
`elsif DTCM_8192K
typedef 8192 KB_PER_DTCM;
`elsif DTCM_16384K
typedef 16384 KB_PER_DTCM;
`elsif DTCM_128M
typedef 131072 KB_PER_DTCM;
`else
typedef 4 KB_PER_DTCM;   // Place holder default value
`endif

// --- USER CONFIGURABLE
//

// Naming of TCM hex file(s)
`ifdef HEXFILEPREFIX
String itcmname = `HEXFILEPREFIX + "itcm.hex";
String dtcmname = `HEXFILEPREFIX + "dtcm.hex";
`else
String itcmname = "./itcm.hex";
String dtcmname = "./dtcm.hex";
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

Integer kb_per_itcm =   valueOf (KB_PER_ITCM);   // TCM Sizing
Integer bytes_per_ITCM = kb_per_itcm * 'h400;
// LSBs to address a byte in the TCMs
typedef TAdd# (TLog# (KB_PER_ITCM), TLog #(1024)) ITCM_Addr_LSB;
Integer itcm_addr_lsb = valueOf (ITCM_Addr_LSB);
// Indices into the TCM
typedef Bit #(TAdd #(TLog #(KB_PER_ITCM), 8)) ITCM_INDEX;//(KB*1024)/ bytes_per_itcm_word
// size of the BRAM in TCM_Word(s). Only handles powers of two.
Integer n_words_IBRAM = (bytes_per_ITCM / bytes_per_tcm_word);

Integer kb_per_dtcm =   valueOf (KB_PER_DTCM);   // TCM Sizing
Integer bytes_per_DTCM = kb_per_dtcm * 'h400;
// LSBs to address a byte in the TCMs
typedef TAdd# (TLog# (KB_PER_DTCM), TLog #(1024)) DTCM_Addr_LSB;
Integer dtcm_addr_lsb = valueOf (DTCM_Addr_LSB);
// Indices into the TCM
typedef Bit #(TAdd #(TLog #(KB_PER_DTCM), 8)) DTCM_INDEX;//(KB*1024)/ bytes_per_dtcm_word
// size of the BRAM in TCM_Word(s). Only handles powers of two.
Integer n_words_DBRAM = (bytes_per_DTCM / bytes_per_tcm_word);
endpackage
