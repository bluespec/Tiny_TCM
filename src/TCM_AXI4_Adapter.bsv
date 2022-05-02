// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package TCM_AXI4_Adapter;

// ================================================================
// Adapter converting generic 64b-wide read/write requests into an
// AXI4 bus master. 'Client' upstream:
// - an MMIO: requests/responses are for 64b word or sub-word,
//            and where lane-alignment is already done.

// The AXI4 bus master can be used with 32b or 64b buses, and manages
// byte-lane alignment, number of beats in a burst, write-strobes,
// etc. accordingly.
//
// The STANDALONE macro should be enabled when the Adapter is to be
// instantiated in its own hierarchy or for standalone verification
//
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


// ================================================================
// This module collects and discards write-responses.
// There is no write-data response to the client.
// Errors on writes are reported on a separate method that can be used
// to trigger an interrupt.

// This module avoids interleaving read and write requests,
// i.e., it launches a read request only when no write-responses from
// the AXI4 fabric are pending.  

// ================================================================
// MODULE INTERFACE

interface TCM_AXI4_Adapter_IFC;
   // Reset
   method Action  reset;

`ifdef STANDALONE
   // ----------------
   // interface for word/sub-word read/write client

   interface Put #(Single_Req) p_mem_single_req;
   interface Put #(Bit #(32))  p_mem_single_write_data;
   interface Get #(Read_Data)  g_mem_single_read_data;
`endif

   // ----------------
   // Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) mem_master;

   // ----------------------------------------------------------------
   // Misc. control and status

`ifndef Near_Mem_TCM
   // Signal that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;

   // Misc. status; 0 = running, no error
   (* always_ready *)
   method Bit #(8) mv_status;
`endif


endinterface

// ================================================================
// Misc. help functions

// ----------------------------------------------------------------
// Byte-width of the fabric

Integer bytes_per_fabric_data = ((valueOf (Wd_Data) == 32) ? 4 : 8);

// ----------------------------------------------------------------
// Convert size code into AXI4_Size code (number of bytes in a beat).
// It just so happens that our coding coincides with AXI4's coding.

function AXI4_Size  fv_size_code_to_AXI4_Size (Bit #(2) size_code);
   return { 1'b0, size_code };
endfunction

// ================================================================
// MODULE IMPLEMENTATION

`ifdef STANDALONE
(* synthesize *)
`endif
module mkTCM_AXI4_Adapter #(
     parameter Bit #(2) verbosity
`ifdef STANDALONE
   // Structures to interace with the MMIO (sub-modules)
   FIFOF #(Single_Req) f_single_reqs <- mkFIFOF1;
   FIFOF #(Bit #(32))  f_single_write_data <- mkFIFOF1;
   FIFOF #(Read_Data)  f_single_read_data <- mkFIFOF1;
) (TCM_AXI4_Adapter_IFC);
`else
   // Structures to interace with the MMIO (interface args)
   , FIFOF #(Single_Req) f_single_reqs
   , FIFOF #(Bit #(32))  f_single_write_data
   , FIFOF #(Read_Data)  f_single_read_data) (TCM_AXI4_Adapter_IFC);
`endif

   // Verbosity: 0=quiet, 1 = rule firings
   // Integer verbosity = 1;

   // Identifiers for requestor client
   Bit #(1) client_id_line   = 0;
   Bit #(1) client_id_single = 1;

   // Limit the number of reads/writes outstanding to 1
   Reg #(Bool) rg_rd_rsps_pending <- mkReg (False);
   Reg #(Bool) rg_wr_rsps_pending <- mkReg (False);

   // Record errors on write-responses from mem
   Reg #(Bool) rg_write_error <- mkReg (False);

   // AXI4 fabric request/response
   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Master_Xactor;


   // ****************************************************************
   // BEHAVIOR: READ RESPONSES (for line and single clients)
   // The following registers identify the client, the beat, etc.

   Reg #(Bit #(1)) rg_rd_client_id <- mkRegU;

   // TODO: change mkFIFOF to mkSizedFIFOF (allowing multiple outstanding reads)
   // may reads can be outstanding
   // For Magritte synthesis reduced mkFIFOF to mkFIFOF1 as in Magritte only one
   // request can be outstanding
   FIFOF #(Tuple2 #(Bit #(2),     // size_code
		    Bit #(1)))    // addr bit [2]
`ifdef SYNTHESIS
         f_rd_rsp_control <- mkFIFOF1;
`else
         f_rd_rsp_control <- mkFIFOF;
`endif
   match { .rd_req_size_code, .rd_req_addr_bit_2 } = f_rd_rsp_control.first;

   // The following are needed for FABRIC32 during each burst response
   // to assemble lower and upper 32b into a 64b response to client
   Reg #(Bool)       rg_rd_data_lower32_ok <- mkRegU;
   Reg #(Bit #(32))  rg_rd_data_lower32    <- mkRegU;

   // All responses are single beat as these are all single requests whose
   // widths are less than or equal to bus-width
   rule rl_read_data (rg_rd_rsps_pending);
      f_rd_rsp_control.deq;
      rg_rd_rsps_pending <= False;
      let rd_data <- pop_o (master_xactor.o_rd_data);

      Bool      ok   = (rd_data.rresp == axi4_resp_okay);
`ifdef FABRIC32
      let rsp = Read_Data {ok: ok, data: rd_data.rdata};
`endif

`ifdef FABRIC64
      Bit #(32) data = (rd_req_addr_bit_2 == 1'b1) ? rd_data.rdata[63:32]
                                                   : rd_data.rdata[31:0];
      let rsp = Read_Data {ok: ok, data: data};
`endif
      f_single_read_data.enq (rsp);

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_read_data: ", cur_cycle);
	 $write ("     single");
	 $write (" ok %0d data %08h", pack (rsp.ok), rsp.data);
	 $display ("");
      end
   endrule: rl_read_data

   // ****************************************************************
   // BEHAVIOR: Single read requests (not a burst)

   rule rl_single_read_req (f_single_reqs.first.is_read
			    && (!rg_rd_rsps_pending)
			    && (!rg_wr_rsps_pending));
      let         req        <- pop (f_single_reqs);
      Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (req.addr);
      AXI4_Size   fabric_size = fv_size_code_to_AXI4_Size (req.size_code);

      if (verbosity >= 1)
	 $display ("%0d: %m.rl_single_read_req:\n    AXI4_Rd_Addr{araddr %0h arlen 0 (burst length 1) ",
		   cur_cycle, fabric_addr,  fshow_AXI4_Size (fabric_size), "}");

      let mem_req_rd_addr = AXI4_Rd_Addr {arid:     fabric_default_id,
					  araddr:   fabric_addr,
					  arlen:    0, // burst len = arlen+1
					  arsize:   fabric_size,
					  arburst:  fabric_default_burst,
					  arlock:   fabric_default_lock,
					  arcache:  fabric_default_arcache,
					  arprot:   fabric_default_prot,
					  arqos:    fabric_default_qos,
					  arregion: fabric_default_region,
					  aruser:   fabric_default_user};
      master_xactor.i_rd_addr.enq (mem_req_rd_addr);

      f_rd_rsp_control.enq (tuple2 (req.size_code, req.addr [2]));
      rg_rd_client_id    <= client_id_single;
      rg_rd_rsps_pending <= True;
   endrule

   // ****************************************************************
   // BEHAVIOR: WRITE RESPONSES (for line and single clients)
   // The following register identifies the client

   rule rl_write_rsp;
      let wr_resp <- pop_o (master_xactor.o_wr_resp);

      Bool err = False;
      if (!rg_wr_rsps_pending) begin
	 rg_write_error <= True;

	 $display ("%0d: %m.rl_write_rsp: ERROR not expecting any write-response:"
            , cur_cycle);
	 $display ("    ", fshow (wr_resp));
      end
      else begin
	 rg_wr_rsps_pending <= False;
	 if (wr_resp.bresp != axi4_resp_okay) begin
	    rg_write_error <= True;
	    if (verbosity >= 1) begin
	       $display ("%0d: %m.rl_write_rsp: FABRIC RESPONSE ERROR", cur_cycle);
	       $display ("    ", fshow (wr_resp));
	    end
	 end
	 else if (verbosity >= 1) begin
	    $display ("%0d: %m.rl_write_rsp: pending=%0d, ",
		      cur_cycle, rg_wr_rsps_pending, fshow (wr_resp));
	 end
      end
   endrule

   // ****************************************************************
   // BEHAVIOR: Write data (for both line and single)
   // Assume that data is already lane-aligned for 32b data width.
   // For Magritte synthesis reduced mkFIFOF to mkFIFOF1 as in Magritte only one
   // request can be outstanding

   FIFOF #(Tuple3 #(Bit #(1),    // client_id
		    Bit #(2),    // size_code
		    Bit #(3)     // addr lsbs
		   ))            // Num beats in write-data
`ifdef SYNTHESIS
         f_wr_data_control <- mkFIFOF1;
`else
         f_wr_data_control <- mkFIFOF;
`endif

   // All write data bursts are single beat as these are all single requests whose
   // widths are less than or equal to bus-width
   rule rl_write_data;
      match {.wr_client_id,
             .wr_req_size_code,
             .wr_req_addr_lsbs} = f_wr_data_control.first;

      f_wr_data_control.deq;

      Bit #(32) data = f_single_write_data.first;

      // Compute strobe from size and address
      Bit #(4)  strb = case (wr_req_size_code)
			  2'b00: 4'h_1;
			  2'b01: 4'h_3;
			  2'b10: 4'h_F;
                          2'b11: 4'h_F;   // XXX Not supported
		       endcase;
      Bit #(4) lsbs = extend (wr_req_addr_lsbs[1:0]);
      strb = (strb << lsbs);

`ifdef FABRIC32
      let mem_req_wr_data = AXI4_Wr_Data {
           wdata: data
         , wstrb: strb
         , wlast: True
         , wuser: fabric_default_user};
      
`endif

`ifdef FABRIC64
      let upper_32 = (wr_req_addr_lsbs[2] == 1'b1);
      let mem_req_wr_data = AXI4_Wr_Data {
           wdata: (upper_32 ? {data, 32'b0} : extend (data))
         , wstrb: (upper_32 ? {strb, 4'b0}  : extend (strb))
         , wlast:  True
         , wuser:  fabric_default_user};
`endif

      master_xactor.i_wr_data.enq (mem_req_wr_data);
      f_single_write_data.deq;

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_write_data", cur_cycle);
	 $display ("    AXI4_Wr_Data{%0h strb %0h last %0d}"
            , mem_req_wr_data.wdata, mem_req_wr_data.wstrb
            , pack (mem_req_wr_data.wlast));
      end
   endrule

   // ****************************************************************
   // Scheduling

   (* descending_urgency = "rl_read_data,       rl_write_rsp" *)
   (* descending_urgency = "rl_write_data,      rl_single_read_req" *)
   (* descending_urgency = "rl_single_read_req, rl_single_write_req" *)

   // ****************************************************************
   // BEHAVIOR: Single write requests (not a burst)

   rule rl_single_write_req ((! f_single_reqs.first.is_read)
			     && (!rg_rd_rsps_pending)
			     && (!rg_wr_rsps_pending));
      let req <- pop (f_single_reqs);

      Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (req.addr);
      AXI4_Size   fabric_size = fv_size_code_to_AXI4_Size (req.size_code);

      // AXI4 Write-Address channel
      let mem_req_wr_addr = AXI4_Wr_Addr {awid:     fabric_default_id,
					  awaddr:   fabric_addr,
					  awlen:    0, // burst len = arlen+1
					  awsize:   fabric_size,
					  awburst:  fabric_default_burst,
					  awlock:   fabric_default_lock,
					  awcache:  fabric_default_awcache,
					  awprot:   fabric_default_prot,
					  awqos:    fabric_default_qos,
					  awregion: fabric_default_region,
					  awuser:   fabric_default_user};
      master_xactor.i_wr_addr.enq (mem_req_wr_addr);

      f_wr_data_control.enq (tuple3 (client_id_single,
				     req.size_code,
				     req.addr [2:0]));
      rg_wr_rsps_pending <= True;

      // Debugging
      if (verbosity >= 1)
	 $display ("%0d: %m.rl_single_write_req: AXI4_Wr_Addr{awaddr %0h awlen %0d ",
		   cur_cycle,
		   mem_req_wr_addr.awaddr,
		   mem_req_wr_addr.awlen,
		   fshow_AXI4_Size (fabric_size),
		   " incr}");
   endrule

   // ================================================================
   // INTERFACE

   method Action reset;
      f_single_reqs.clear;
      f_single_write_data.clear;
      f_single_read_data.clear;
      master_xactor.reset;
      if (verbosity > 1)
         $display ("%0d: %m.reset", cur_cycle);
   endmethod


`ifdef STANDALONE
   // ----------------
   // interface for word/sub-word read/write client

   interface Put p_mem_single_req        = toPut (f_single_reqs);
   interface Put p_mem_single_write_data = toPut (f_single_write_data);
   interface Get g_mem_single_read_data  = toGet (f_single_read_data);
`endif


   // ----------------
   // Fabric master interface
   interface mem_master = master_xactor.axi_side;

`ifndef Near_Mem_TCM
   // Signal that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready = noAction;

   // Misc. status; 0 = running, no error
   method Bit #(8) mv_status = 0;
`endif

endmodule

endpackage
