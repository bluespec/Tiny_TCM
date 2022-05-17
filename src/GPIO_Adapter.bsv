// Copyright (c) 2021 Bluespec, Inc.  All Rights Reserved

package GPIO_Adapter;

// ================================================================
// Adapter converting generic 32b-wide read/write requests from a
// MMIO client into reads/writes to a GPIO register
//
// 'Client' upstream:
// - MMIO device: requests/responses are for 8/16/32-bit reads and
//                writes and where lane-alignment is already done.
//
// Expectations between client and adapter:
// - Write data from Client aligned to appropriate bytelane in 32-
//   bit word. GPIO adapter will derive appropriate write strobes
//   from size code.
//
// - Read response from APB Adapter will have the desired word or
//   sub-word aligned to the appropriate bytelane. All reads issued
//   on the APB will be for 32-bits and with addresses that are
//   32-bit aligned.
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
import GetPut              :: *;

// ----------------
// Other lib imports

// import Semi_FIFOF :: *;
// import EdgeFIFOFs :: *;

// ================================================================
// Project imports

import ISA_Decls           :: *;
import MMU_Cache_Common    :: *; // definitions for req/rsp types
import Cur_Cycle           :: *;

// ================================================================
// Local type definition - upto 32-bits for RV32

typedef Bit #(32) GPIO_Data;

// ================================================================
// MODULE INTERFACE

interface GPIO_IFC;
   (* always_ready *)
   method GPIO_Data out;
   (* always_ready, always_enabled *)
   method Action in (GPIO_Data x);
endinterface

interface GPIO_Adapter_IFC;
   // Reset
   method Action  reset;

   // ----------------
   // Fabric interface
   interface GPIO_IFC gpio;
endinterface

// ================================================================
// Misc. help functions

// ================================================================
// MODULE IMPLEMENTATION
module mkGPIO_Adapter #(
   parameter Bit #(2) verbosity   // 0=quiet, 1=rule firings
   , FIFOF #(Single_Req) f_single_reqs
   , FIFOF #(Bit #(32))  f_single_write_data
   , FIFOF #(Read_Data)  f_single_read_data) (GPIO_Adapter_IFC);

   // Input signals from APB bus
   Reg  #(GPIO_Data)     rg_gpio_in  <- mkRegU;
   Reg  #(GPIO_Data)     rg_gpio_out <- mkReg(0);

   // ================================================================
   // BEHAVIOR

   // --------
   // Incoming transfer request
   let req = f_single_reqs.first;

   // A read request is waiting
   Bool read_request = req.is_read;

   // A write request is waiting. For writes, we need wr_addr and wr_data
   // available as we compute HSIZE from wr_data.wstrb
   Bool write_request = !req.is_read;

   // --------
   // New Write Request
   rule rl_gpio_write (write_request);
      rg_gpio_out <= f_single_write_data.first;

      // Advance request queue
      f_single_reqs.deq; f_single_write_data.deq;
      if (verbosity > 0) begin
         $display ("%06d:[D]:%m.rl_gpio_write: ", cur_cycle);
         if (verbosity > 1)
            $display ("          (gpio: %08h)", f_single_write_data.first);
      end
   endrule

   // --------
   // New Read Request
   rule rl_gpio_read (read_request);
      // Response handling and packing
      Bit #(32) data = extend (rg_gpio_in);

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
   method Action reset;
      rg_gpio_out <= 0;
      if (verbosity > 1) $display ("%06d:[D]:%m.reset", cur_cycle);
   endmethod

   // ----------------
   // APB side
   interface GPIO_IFC gpio;
      method out = rg_gpio_out;
      method Action in (GPIO_Data x);
         rg_gpio_in <= x;
      endmethod
   endinterface
endmodule


// ================================================================

endpackage
