`timescale 1ns / 1ps
module mycpu_top
(
	input	[5:0] int,
    input         aclk,
    input         aresetn, 

	//------debug------
	output [31:0] 	debug_wb_pc,
	output [3:0] 	debug_wb_rf_wen,
	output [4:0] 	debug_wb_rf_wnum,
	output [31:0] 	debug_wb_rf_wdata,

    //ar
    output  [3 :0] arid   ,
    output  [31:0] araddr,
    output  [7 :0] arlen  ,
    output  [2 :0] arsize ,
    output  [1 :0] arburst,
    output  [1 :0] arlock ,
    output  [3 :0] arcache,
    output  [2 :0] arprot ,
    output         arvalid,
    input          arready,
    //r
    input [3 :0] rid    ,
    input [31:0] rdata  ,
    input [1 :0] rresp ,
    input        rlast ,
    input        rvalid ,
    output       rready ,
    //aw
    output  [3 :0] awid   ,
    output  [31:0] awaddr ,
    output  [7 :0] awlen  ,
    output  [2 :0] awsize ,
    output  [1 :0] awburst,
    output  [1 :0] awlock ,
    output  [3 :0] awcache,
    output  [2 :0] awprot ,
    output         awvalid,
    input          awready,
    //w
    output  [3 :0] wid    ,
    output  [31:0] wdata  ,
    output  [3 :0] wstrb  ,
    output         wlast  ,
    output         wvalid ,
    input          wready ,
    //b
    input [3 :0] bid    ,
    input [1 :0] bresp  ,
    input        bvalid ,
    output       bready 
);

    //------inst sram-like-------
    wire          inst_req    ;
    wire          inst_wr     ;
    wire   [1 :0] inst_size   ;
    wire   [31:0] inst_addr   ;
    wire   [31:0] inst_wdata  ;
    wire  [31:0] inst_rdata  ;
    wire         inst_addr_ok;
    wire         inst_data_ok;
    
    //------data sram-like-------
    wire            data_req    ;
    wire            data_wr     ;
    wire     [1 :0] data_size   ;
    wire     [31:0] data_addr   ;
    wire     [31:0] data_wdata  ;
    wire     [3:0 ] data_wstrb  ;
    wire    [31:0] data_rdata  ;
    wire           data_addr_ok;
    wire           data_data_ok;


mycpu mycpu(
    aresetn		,
	aclk		,

    //------inst sram-like-------
    inst_req    ,
    inst_wr     ,
    inst_size   ,
    inst_addr   ,
    inst_rdata  ,
	inst_wdata  ,
    inst_addr_ok,
    inst_data_ok,
    
    //------data sram-like-------
    data_req    ,
    data_wr     ,
    data_size   ,
    data_addr   ,
    data_wdata  ,
    data_wstrb  ,
    data_rdata  ,
    data_addr_ok,
    data_data_ok,

	debug_wb_pc,
    debug_wb_rf_wen,
    debug_wb_rf_wnum,
    debug_wb_rf_wdata
	);

bridge brideg(
    aclk		,
	aresetn		,

    //------inst sram-like-------
    inst_req    ,
    inst_wr     ,
    inst_size   ,
    inst_addr   ,
    inst_wdata  ,
	inst_rdata  ,
    inst_addr_ok,
    inst_data_ok,
    
    //------data sram-like-------
    data_req    ,
    data_wr     ,
    data_size   ,
    data_addr   ,
    data_wdata  ,
    data_wstrb  ,
    data_rdata  ,
    data_addr_ok,
    data_data_ok,

	arid   ,
    araddr ,
    arlen  ,
    arsize ,
    arburst,
    arlock ,
    arcache,
    arprot ,
    arvalid,
    arready,
    //r
    rid    ,
    rdata  ,
    rresp ,
    rlast ,
    rvalid ,
    rready ,
    //aw
    awid   ,
    awaddr ,
    awlen  ,
    awsize ,
    awburst,
    awlock ,
    awcache,
    awprot ,
    awvalid,
    awready,
    //w
    wid    ,
    wdata  ,
    wstrb  ,
    wlast  ,
    wvalid ,
    wready ,
    //b
    bid    ,
    bresp  ,
    bvalid ,
    bready 

);
    
endmodule