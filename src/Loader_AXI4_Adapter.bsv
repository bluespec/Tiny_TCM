// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package Loader_AXI4_Adapter;

// ================================================================
// BSV lib imports

import Vector           :: *;
import BRAMCore         :: *;
import ConfigReg        :: *;
import FIFOF            :: *;
import GetPut           :: *;
import ClientServer     :: *;
import Assert           :: *;

// ----------------
// BSV additional libs

import Cur_Cycle        :: *;
import GetPut_Aux       :: *;
import Semi_FIFOF       :: *;

// ================================================================
// Project imports

import ISA_Decls        :: *;
import TCM_Decls        :: *;
import MMU_Cache_Common :: *;

import AXI4_Types       :: *;
import Fabric_Defs      :: *;
import Core_Map         :: *;
import DM_CPU_Req_Rsp   :: *;   // for SB_Sys_Req

// ================================================================
// Adapter converting AXI4 slave requests into commands to a RAM. 
// 'Server' downstream:
// - a TCM RAM: requests/responses are for fabric-width only.

// The AXI4 bus slave can be used with 32b buses, and manages
// byte-lane alignment and write-strobes. However, this slave
// implementation only handles requests with a single beat.

// ** WARNING **
// FABRIC64 is not supported.
// Bursts not supported.
// Only full word writes are supported from/to the ITCM
//
// ================================================================
// Fabric Port
// Enables 'back-door' access of TCM by devices and debuggers.
// Supports only word-size requests.

interface Loader_AXI4_Adapter_IFC;
   // Reset
   method Action  reset;

   // Back-door slave interface from fabric into Near_Mem
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) axi;
endinterface

// ----------------------------------------------------------------
module mkLoader_AXI4_Adapter #(
     Server #(SB_Sys_Req, Bool) dma_server) (Loader_AXI4_Adapter_IFC);

   // Verbosity: 0: quiet
   //            1: Requests and responses
   //            2: rule firings
   //            3: + detail
   Bit #(2) verbosity = 2;

   // Requests from/responses to fabric. AXI4 Write Transactor.
   AXI4_Slave_WXactor_IFC #(
      Wd_Id, Wd_Addr, Wd_Data, Wd_User
   ) slave_xactor <- mkAXI4_Slave_WXactor;

   Core_Map_IFC core_map <- mkCore_Map;

   FIFOF #(Bit #(Wd_Id)) f_awid <- mkFIFOF;
   FIFOF #(Bit #(Wd_User)) f_awuser <- mkFIFOF;

   // ----------------------------------------------------------------
   // BEHAVIOR

   // ----------------
   // Reset

   function Action fa_reset;
      action
         slave_xactor.reset;
         f_awid.clear;
         f_awuser.clear;
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Handle fabric write request
   // Only full-word requests (32b in NM32 and 64b in NM64)

   // Forward write requests to the server
   rule rl_wr_req;
      slave_xactor.o_wr_addr.deq;
      slave_xactor.o_wr_data.deq;
      let wra = slave_xactor.o_wr_addr.first;
      let wrd = slave_xactor.o_wr_data.first;

      f_awid.enq (wra.awid);
      f_awuser.enq (wra.awuser);

      dma_server.request.put (SB_Sys_Req {
           addr : wra.awaddr
         , wdata: pack (wrd.wdata)
         , size : unpack (wra.awsize)  // AXI4_Types::AXI4_Size -> DM_Common::DM_sbaccess
         , read_not_write: False
      });

      if (verbosity > 1) begin
         $display ("%06d:[D]:%m.rl_wr_req", cur_cycle);
         $display ("    ", fshow (wra));
         $display ("    ", fshow (wrd));
      end
   endrule

   rule wr_rsp;
      let rsp <- dma_server.response.get ();
      f_awid.deq; f_awuser.deq;
      // Send response
      let wrr = AXI4_Wr_Resp {
           bid:   f_awid.first
         , bresp: (rsp ? axi4_resp_slverr : axi4_resp_okay)
         , buser: f_awuser.first};
      slave_xactor.i_wr_resp.enq (wrr);

      if (verbosity > 1) begin
         $display ("%06d:[D]:%m.rl_wr_rsp", cur_cycle);
         $display ("    ", fshow (wrr));
      end
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      fa_reset;
      if (verbosity > 1)
         $display ("%0d: %m.reset", cur_cycle);
   endmethod

   interface axi = slave_xactor.axi_side;
endmodule

endpackage
