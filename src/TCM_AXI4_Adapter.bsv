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
import SoC_Map          :: *;

import AXI4_Types       :: *;
import Fabric_Defs      :: *;

// ----------------------------------------------------------------
// Address converters

`ifdef ISA_PRIV_S
// Convert a 64-bit PA to an AXI4 Fabric Address
// For FABRIC64 this does nothing.
// For FABRIC32 it discards the upper 32 bits.

function Fabric_Addr fv_Addr_to_Fabric_Addr (Bit #(64) addr);
   return truncate (addr);
endfunction

`else

// Convert a XLEN Address to an AXI4 Fabric Address 
function Fabric_Addr fv_Addr_to_Fabric_Addr (Addr addr);
`ifdef RV32
`ifdef FABRIC32
   return (pack (addr));
`elsif FABRIC64
   return zeroExtend (addr);
`endif
`elsif RV64
`ifdef FABRIC32
   return truncate (addr);
`elsif FABRIC64
   return (addr);
`endif
`endif
endfunction

`endif   // `else ISA_PRIV_S

// Convert a AXI4 Fabric Address to an XLen Address
function Addr fv_Fabric_Addr_to_Addr (Fabric_Addr a);
`ifdef RV32
`ifdef FABRIC32
   return (pack (a));
`elsif FABRIC64
   return truncate (a);
`endif
`elsif RV64
`ifdef FABRIC32
   return extend (a);
`elsif FABRIC64
   return (a);
`endif
`endif
endfunction


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

   // ----------------
   // interface for word/sub-word read/write client

   interface Put #(Single_Req) p_mem_single_req;
   interface Put #(Bit #(32))  p_mem_single_write_data;
   interface Get #(Read_Data)  g_mem_single_read_data;

   // ----------------
   // Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) mem_master;

   // ----------------------------------------------------------------
   // Misc. control and status

   // Signal that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;

   // Misc. status; 0 = running, no error
   (* always_ready *)
   method Bit #(8) mv_status;


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

(* synthesize *)
module mkTCM_AXI4_Adapter #(
     parameter Bit #(2) verbosity
   ) (TCM_AXI4_Adapter_IFC);

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

   // Memory request from MMIO
   FIFOF #(Single_Req)  f_single_reqs        <- mkFIFOF1;
   FIFOF #(Bit #(32))   f_single_write_data  <- mkFIFOF1;
   FIFOF #(Read_Data)   f_single_read_data   <- mkFIFOF1;
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

	 $display ("%0d: %m.rl_write_rsp: ERROR not expecting any write-response:", cur_cycle);
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

   // ----------------
   // interface for word/sub-word read/write client

   interface Put p_mem_single_req        = toPut (f_single_reqs);
   interface Put p_mem_single_write_data = toPut (f_single_write_data);
   interface Get g_mem_single_read_data  = toGet (f_single_read_data);

   // ----------------
   // Fabric master interface
   interface mem_master = master_xactor.axi_side;

   // ----------------------------------------------------------------
   // Misc. control and status

   // Signal that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready = noAction;

   // Misc. status; 0 = running, no error
   method Bit #(8) mv_status = 0;

endmodule

// ================================================================
// Adapter converting AXI4 slave requests into commands to a RAM. 
// 'Server' downstream:
// - a TCM RAM: requests/responses are for fabric-width only.

// The AXI4 bus slave can be used with 32b or 64b buses, and manages
// byte-lane alignment and write-strobes. However, this slave
// implementation only handles requests with a single beat.

// ================================================================
// Fabric Port
// Enables 'back-door' access of TCM by devices and debuggers.
// Supports only word-size requests.

interface TCM_DMA_AXI4_Adapter_IFC;
   // Reset
   method Action  reset;

   // Back-door slave interface from fabric into Near_Mem
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  dma_server;
endinterface

// ----------------------------------------------------------------
// Module state
typedef enum {STATE_READY,
              STATE_READ_RESPONDING,
              STATE_BURST_WRITE} State deriving (Bits, Eq, FShow);
/*
// ----------------------------------------------------------------
module mkTCM_DMA_AXI4_Adapter #(
     BRAM_PORT_BE #(Addr, TCM_Word, Bytes_per_TCM_Word) ram
   , Bit #(2)                                           verbosity) (TCM_DMA_AXI4_Adapter_IFC);

   // Module state
   Reg #(State) rg_state <- mkReg (STATE_READY);

   // Requests from/responses to fabric
   AXI4_Slave_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Slave_Xactor;

   SoC_Map_IFC soc_map <- mkSoC_Map;

   // ----------------------------------------------------------------
   // BEHAVIOR

   // ----------------
   // Reset

   function Action fa_reset;
      action
         slave_xactor.reset;
         rg_state <= STATE_READY;
      endaction
   endfunction

   function Bool fn_is_tcm_addr (Fabric_Addr addr);
      return ((soc_map.m_tcm_addr_base <= addr) && (addr < soc_map.m_tcm_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Handle fabric read request
   // Only full-word requests (32b in Fabric_32 and 64b in Fabric_64)

   // The head of the read request queue, and some functions on it
   let rda              = slave_xactor.o_rd_addr.first;
   let rd_byte_addr     = rda.araddr;
   let rd_ram_word_addr = ((rd_byte_addr - soc_map.m_tcm_addr_base) >> bits_per_byte_in_tcm_word);

   Bool rd_addr_valid   = fn_is_tcm_addr (rd_byte_addr);

   Byte_in_TCM_Word rd_byte_in_tcm_word = rd_byte_addr [(bits_per_byte_in_tcm_word - 1) : 0];

   // Check alignment
`ifdef FABRIC32
   Bool rd_addr_aligned = (
         (rd_byte_in_tcm_word == 0)
      || (rd_byte_in_tcm_word == 4));
   Bool lower_word = (rd_byte_in_tcm_word == 0);
`elsif FABRIC64
   Bool rd_addr_aligned = (rd_byte_in_tcm_word == 0);
`endif

   // Invalid read address: send error response
   rule rl_bad_rd_addr (   (rg_state == STATE_READY)
                        && ((! rd_addr_valid) || (! rd_addr_aligned)));
      slave_xactor.o_rd_addr.deq;

      let rdr = AXI4_Rd_Data {
           rid:   rda.arid
         , rresp: axi4_resp_slverr
         , rdata: rd_byte_addr
         , ruser: rda.aruser
         , rlast: True
      };
      slave_xactor.i_rd_data.enq (rdr);

      if (verbosity > 0)
         $display ("%0d: %m.rl_bad_rd_addr 0x%0h", cur_cycle, rd_byte_addr);
   endrule

   // Legal, well-formed read requests: initiate RAM read
   rule rl_rd_req ((rg_state == STATE_READY) && rd_addr_valid && rd_addr_aligned);
      // Initiate word read from ram
      ram.put (0, fv_Fabric_Addr_to_Addr (rd_ram_word_addr), ?);
      rg_state <= STATE_READ_RESPONDING;

      if (verbosity > 1)
         $display ("%0d: %m.rl_rd_req: addr 0x%0h", cur_cycle, rd_byte_addr);
   endrule

   // Read responses: get word from RAM and respond
   rule rl_rd_rsp (rg_state == STATE_READ_RESPONDING);
`ifdef FABRIC32
      TCM_Word_B words = unpack (ram.read);
      Bit#(Wd_Data) word = lower_word ? pack (words[3:0]) : pack (words [7:4]);
`elsif FABRIC64
      Bit#(Wd_Data) word = pack(ram.read);
`endif
      
      let rdr = AXI4_Rd_Data {
           rid  : rda.arid
         , rresp: axi4_resp_okay
         , rdata: word
         , rlast: True
         , ruser: rda.aruser
      };
      slave_xactor.i_rd_data.enq (rdr);
      slave_xactor.o_rd_addr.deq;
      rg_state <= STATE_READY;

      if (verbosity > 1)
         $display ("%0d: %m.rl_rd_rsp: addr 0x%0h => data 0x%0h",
                   cur_cycle, rd_byte_addr, word);
   endrule

   // ----------------------------------------------------------------
   // Handle fabric write request
   // Only full-word requests (32b in RV32 and 64b in RV64)

   // The head of the write request queue, and some functions on it
   let wra = slave_xactor.o_wr_addr.first;
   let wrd = slave_xactor.o_wr_data.first;

   let wr_byte_addr     = wra.awaddr;
   let wr_ram_word_addr = ((wr_byte_addr - soc_map.m_tcm_addr_base) >> bits_per_byte_in_tcm_word);

   Bool wr_addr_valid   = fn_is_tcm_addr (wr_byte_addr);

   Byte_in_TCM_Word wr_byte_in_tcm_word = wr_byte_addr [(bits_per_byte_in_tcm_word - 1) : 0];

   // Check alignment and strobe
`ifdef FABRIC32
   Bool wr_addr_aligned = (
         (wr_byte_in_tcm_word == 0)
      || (wr_byte_in_tcm_word == 4));
   Bool w_lower_word = (wr_byte_in_tcm_word == 0);
`elsif FABRIC64
   Bool wr_addr_aligned = (wr_byte_in_tcm_word == 0);
`endif
   Bool wr_data_size_ok = (wrd.wstrb == '1);

   // Invalid write address or data size: send error response
   rule rl_bad_wr_addr (
         (rg_state == STATE_READY)
      && (   (! wr_addr_valid)
          || (! wr_addr_aligned)
          || (! wr_data_size_ok)));

      slave_xactor.o_wr_addr.deq;
      slave_xactor.o_wr_data.deq;

      let wrr = AXI4_Wr_Resp {
           bid:   wra.awid
         , bresp: axi4_resp_slverr
         , buser: wra.awuser
      };
      slave_xactor.i_wr_resp.enq (wrr);

      if (verbosity > 0) begin
         $display ("%0d: %m.rl_bad_wr_addr", cur_cycle);
         $display ("    ", fshow (wra));
         $display ("    ", fshow (wrd));
         $display ("    => ", fshow (wrr));
      end
   endrule

   // Legal, well-formed write requests
   rule rl_wr_req (
         (rg_state == STATE_READY)
      && (wr_addr_valid)
      && (wr_addr_aligned)
      && (wr_data_size_ok));

      slave_xactor.o_wr_addr.deq;
      slave_xactor.o_wr_data.deq;

      // Strobe generation
`ifdef FABRIC32
      Bit #(Bytes_per_TCM_Word) strobe = w_lower_word ? 8'hf : 8'hf0; 
`elsif FABRIC64
      Bit #(Bytes_per_TCM_Word) strobe = 8'hff;
`endif

      // Write data generation
      TCM_Word tcm_wdata = ?;
`ifdef FABRIC32
      tcm_wdata = {wrd.wdata, wrd.wdata};
`elsif FABRIC64
      tcm_wdata = wrd.wdata;
`endif

      // Write word to ram
      ram.put (strobe, fv_Fabric_Addr_to_Addr (wr_ram_word_addr), tcm_wdata);

      // Send response
      let wrr = AXI4_Wr_Resp {
           bid:   wra.awid
         , bresp: axi4_resp_okay
         , buser: wra.awuser};
      slave_xactor.i_wr_resp.enq (wrr);

      if (verbosity > 1) begin
         $display ("%0d: %m.rl_wr_req", cur_cycle);
         $display ("    ", fshow (wra));
         $display ("    ", fshow (wrd));
      end
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      fa_reset;
      if (verbosity > 1)
         $display ("%0d: %m.reset", cur_cycle);
   endmethod

   interface dma_server = slave_xactor.axi_side;
endmodule
*/

endpackage
