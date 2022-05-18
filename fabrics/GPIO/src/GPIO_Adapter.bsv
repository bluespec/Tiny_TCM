// Copyright (c) 2021 Bluespec, Inc.  All Rights Reserved

package GPIO_Adapter;

// ================================================================
// Adapter converting generic 32b-wide read/write requests from a
// MMIO client into reads/writes to a GPIO register. Each bit in
// the GPIO is accessible via an inependent read/write request
//
// 'Client' upstream:
// - MMIO device: requests/responses are for 8/16/32-bit reads and
//                writes and where lane-alignment is already done.
//
// NOTES:
// =====
// This implementation only supports accesses upto 32-bits for
// reads or writes. 
//
// This module discards write-responses. There is no write response
// to the client. Errors on writes are not checked or reported.
// 
// Read responses are passed up to the client. Errors on reads are
// not checked or reported.
//
// ================================================================
// TODO:
//
// ================================================================
// Macros:
// 
// ================================================================
//
// BSV lib imports
import DefaultValue        :: *;
import FIFOF               :: *;
import Vector              :: *;

// ----------------
// Other lib imports

// ================================================================
// Project imports

import ISA_Decls           :: *;
import MMU_Cache_Common    :: *; // definitions for req/rsp types
import Cur_Cycle           :: *;
import GPIO_Decls          :: *;


// ================================================================
// MODULE INTERFACE

// GPIO Input Interface
interface GPIO_In_IFC;
   (* always_ready, always_enabled *)
   method Action  m_in (Bit #(1) x);
endinterface

// GPIO Output Interface
interface GPIO_Out_IFC;
   (* always_ready *)
   method Bit #(1)  m_out;
endinterface

// GPIO interface presented to the top-level
interface GPIO_Fabric_IFC;
   interface Vector #(NUM_GPIO, GPIO_In_IFC)    v_in;
   interface Vector #(NUM_GPIO, GPIO_Out_IFC)   v_out;
endinterface

interface GPIO_Adapter_IFC;
   // Reset
   method Action  reset;

   // ----------------
   // Fabric interface
   interface GPIO_Fabric_IFC gpio;
endinterface

// ================================================================
// Misc. help functions

// ================================================================
// MODULE IMPLEMENTATION
module mkGPIO_Adapter #(
   parameter Bit #(2) verbosity   // 0=quiet, 1=rule firings
   , FIFOF #(Single_Req)   f_single_reqs
   , FIFOF #(Bit #(1))     f_single_write_data
   , FIFOF #(Read_Data)    f_single_read_data) (GPIO_Adapter_IFC);

   // The GPIO register (in and out maps to the same addresses)
   Vector #(NUM_GPIO, Reg #(Bit #(1))) vrg_gpio_i  <- replicateM (mkRegU);
   Vector #(NUM_GPIO, Reg #(Bit #(1))) vrg_gpio_o  <- replicateM (mkReg (0));

   // ================================================================
   // BEHAVIOR

   // --------
   // Incoming transfer request
   let req = f_single_reqs.first;
   let addr = req.addr;
   GPIO_Index idx = truncate (addr >> 2);

   // A read request is waiting
   Bool read_request = req.is_read;

   // A write request is waiting. For writes, we need wr_addr and wr_data
   // available as we compute HSIZE from wr_data.wstrb
   Bool write_request = !req.is_read;

   // --------
   // New Write Request
   rule rl_gpio_write (write_request);
      vrg_gpio_o [idx] <= f_single_write_data.first;

      // Advance request queue
      f_single_reqs.deq; f_single_write_data.deq;

      if (verbosity > 0) begin
         $display ("%06d:[D]:%m.rl_gpio_write: ", cur_cycle);
         if (verbosity > 1)
            $display ("          (gpio[%0d] <- %01b)"
               , idx, f_single_write_data.first);
      end
   endrule

   // --------
   // New Read Request
   rule rl_gpio_read (read_request);
      // Response handling and packing
      let data = vrg_gpio_i[idx];

      let rsp = Read_Data { ok: True, data: data };
      f_single_read_data.enq (rsp);

      // Advance request queue
      f_single_reqs.deq;
      if (verbosity > 0) begin
         $display ("%06d:[D]:%m.rl_read_response: ", cur_cycle, fshow (rsp));
      end
   endrule

   // ================================================================
   // INTERFACE

   // Creator of each output interface
   function GPIO_Out_IFC fn_mk_GPIO_Out_IFC (Integer i);
      return (
         interface GPIO_Out_IFC;
            method Bit #(1) m_out = vrg_gpio_o [i];
         endinterface);
   endfunction

   function GPIO_In_IFC fn_mk_GPIO_In_IFC (Integer i);
      return (
         interface GPIO_In_IFC;
            method Action m_in (Bit #(1) x);
               vrg_gpio_i [i] <= x;
            endmethod
         endinterface);
   endfunction

   method Action reset;
      writeVReg (vrg_gpio_o, replicate (0));
      if (verbosity > 1) $display ("%06d:[D]:%m.reset", cur_cycle);
   endmethod

   // ----------------
   // GPIO side
   interface GPIO_Fabric_IFC gpio;
      interface v_in   = genWith (fn_mk_GPIO_In_IFC);
      interface v_out  = genWith (fn_mk_GPIO_Out_IFC);
   endinterface
endmodule


// ================================================================

endpackage
