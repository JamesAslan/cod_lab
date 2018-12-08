`timescale 1ns / 1ps
module cpu_axi_interface
(
    input         clk,
    input         resetn, 

    //------inst sram-like-------
    input          inst_req    ,
    input          inst_wr     ,
    input   [1 :0] inst_size   ,
    input   [31:0] inst_addr   ,
    input   [31:0] inst_wdata  ,
    output  [31:0] inst_rdata  ,
    output         inst_addr_ok,
    output         inst_data_ok,
    
    //------data sram-like-------
    input            data_req    ,
    input            data_wr     ,
    input     [1 :0] data_size   ,
    input     [31:0] data_addr   ,
    input     [31:0] data_wdata  ,
    output    [31:0] data_rdata  ,
    output           data_addr_ok,
    output           data_data_ok,

    //ar
    output  [3 :0] arid   ,
    output  [31:0] araddr,
    output  [7 :0] arlen  ,
    output  [2 :0] arsize ,
    output  [1 :0] arburst,
    output  [1 :0] arlock ,
    output  [3 :0] arcache,
    output  [2 :0] arprot ,
    output  reg    arvalid,
    input          arready,
    //r
    input [3 :0] rid    ,
    input [31:0] rdata  ,
    input [1 :0] rresp ,
    input        rlast ,
    input        rvalid ,
    output  reg  rready ,
    //aw
    output  [3 :0] awid   ,
    output  [31:0] awaddr ,
    output  [7 :0] awlen  ,
    output  [2 :0] awsize ,
    output  [1 :0] awburst,
    output  [1 :0] awlock ,
    output  [3 :0] awcache,
    output  [2 :0] awprot ,
    output  reg    awvalid,
    input          awready,
    //w
    output  [3 :0] wid    ,
    output  [31:0] wdata  ,
    output  [3 :0] wstrb  ,
    output         wlast  ,
    output  reg    wvalid ,
    input          wready ,
    //b
    input [3 :0] bid    ,
    input [1 :0] bresp  ,
    input        bvalid ,
    output  reg  bready 
);

    assign arlen    = 8'b0;
    assign arburst   = 2'b01;
    assign arlock   = 2'b0;
    assign arcache  = 4'b0;
    assign arprot   = 3'b0;

    assign rresp   = 2'b0;
    assign rlast   = 1'b0;

    assign awid     = 3'b1;
    assign awlen    = 8'b0;
    assign awburst  = 2'b0;
    assign awlock   = 2'b0;
    assign awcache  = 4'b0;
    assign awprot   = 3'b0;

    assign wid      = 4'b1;
    assign wlast    = 1'b1;

    assign bid      = 4'b0;/////????????
    assign bresp    = 2'b0;

    //rst
    assign rst = !resetn;

    //////sram //////data
    //mode
    reg [1:0] data_mode;
    always @(posedge clk)
    begin
        if(rst)
        begin
            data_mode <= 1'b0;
        end
        else if(mode_read)
        begin
            data_mode <= 1'b0;
        end
        else if(mode_write)
        begin
            data_mode <= 1'b1;     
        end
    end

    reg [2:0] data_work_state;
    always @(posedge clk)
    begin
        if(rst)
        begin
            data_work_state <= 3'b0;
        end
        else if(data_work_state == 3'b0)
        begin
            if(data_addr_ok == 1'b1)
            begin
                data_work_state <= 3'b1;
            end
        end
        else
        begin
            data_work_state <= 3'b0;
        end
    end

    //reg [2:0] data_work_state_record;

    wire    mode_read;
    wire    mode_write;
    assign  mode_read   = data_req && ~data_wr;
    assign  mode_write  = data_req && data_wr;

    assign  data_addr_ok    = data_req;
    assign  data_data_ok    = data_mode ? bvalid : ((source_reg2 == 2'd1) && rvalid && rready);
    assign  data_rdata      = rdata;

    reg [31:0] data_addr_reg;
    always @(posedge clk)
    begin
        if(rst)
        begin
            data_addr_reg <= 32'b0;
        end
        else if(data_addr_ok)
        begin
            data_addr_reg <= data_addr;
        end
    end

    reg [1:0] data_size_reg;
    always @(posedge clk)
    begin
        if(rst)
        begin
            data_size_reg <= 2'b0;
        end
        else if(data_addr_ok)
        begin
            data_size_reg <= data_size;
        end
    end

    wire[3:0] task_wstrb;
    assign task_wstrb = ({4{(data_size == 2'b00) && (data_addr[1:0] == 2'b00)}} & 4'b0001) |
                        ({4{(data_size == 2'b00) && (data_addr[1:0] == 2'b01)}} & 4'b0010) |
                        ({4{(data_size == 2'b00) && (data_addr[1:0] == 2'b10)}} & 4'b0100) |
                        ({4{(data_size == 2'b00) && (data_addr[1:0] == 2'b11)}} & 4'b1000) |
                        ({4{(data_size == 2'b01) && (data_addr[1:0] == 2'b00)}} & 4'b0011) |
                        ({4{(data_size == 2'b01) && (data_addr[1:0] == 2'b10)}} & 4'b1100) |
                        ({4{(data_size == 2'b10) && (data_addr[1:0] == 2'b00)}} & 4'b1111);

    //////sram //////inst
    reg [2:0] inst_work_state;
    always @(posedge clk)
    begin
        if(rst)
        begin
            inst_work_state <= 3'b0;
        end
        else if(inst_work_state == 3'b0)
        begin
            if(inst_addr_ok == 1'b1)
            begin
                inst_work_state <= 3'b1;
            end
        end
        else 
        begin
            inst_work_state <= 3'b0;
        end
    end

    assign  inst_addr_ok    = inst_req;
    assign  inst_data_ok    = rready && rvalid && (source_reg2 == 2'd0);
    assign  inst_rdata      = rdata;

    reg [31:0] inst_addr_reg;
    always @(posedge clk)
    begin
        if(rst)
        begin
            inst_addr_reg <= 32'b0;
        end
        else if(inst_addr_ok)
        begin
            inst_addr_reg <= inst_addr;
        end
    end

    reg [1:0] inst_size_reg;
    always @(posedge clk)
    begin
        if(rst)
        begin
            inst_size_reg <= 2'b0;
        end
        else if(inst_addr_ok)
        begin
            inst_size_reg <= inst_size;
        end
    end

    ///////axi //////write
    wire write_allow;
    assign write_allow = 1'b1;
    
    reg axi_write_addr;
    reg [31:0] data_reg1;
    reg [3:0] wstrb_reg1;
    reg axi_write_data;
    reg [31:0] data_reg2;
    reg [3:0] wstrb_reg2;
    reg axi_write_finish;
    always @(posedge clk)
    begin
        if(rst)
        begin
            axi_write_addr <= 1'b0;
            data_reg1 <= 32'b0;
            wstrb_reg1 <= 4'b0;
        end
        else if(data_addr_ok == 1'b1 && mode_write)
        begin
            axi_write_addr <= 1'b1;
            data_reg1 <= data_wdata;
            wstrb_reg1 <= task_wstrb;
        end
        else if((awvalid == 1'b1) && (awready == 1'b1))
        begin
            axi_write_addr <= 1'b0;
        end
    end

    always @(posedge clk)
    begin
        if(rst)
        begin
            axi_write_data <= 1'b0;
            data_reg2 <= 32'b0;
            wstrb_reg2 <= 4'b0;
        end
        else if((axi_write_addr == 1'b1) && (awvalid == 1'b1) && (awready == 1'b1))
        begin
            axi_write_data <= 1'b1;
            data_reg1 <= data_reg2;
            wstrb_reg2 <= wstrb_reg1;
        end
        else if((wvalid == 1'b1) && (wready == 1'b1))
        begin
            axi_write_data <= 1'b0;
        end
    end

    always @(posedge clk)
    begin
        if(rst)
        begin
            axi_write_finish <= 1'b0;
        end
        else if((axi_write_data == 1'b1) && (wvalid == 1'b1) && (wready == 1'b1))
        begin
            axi_write_finish <= 1'b1;
        end
        else if((bready == 1'b1) && (bvalid == 1'b1))
        begin
            axi_write_finish <= 1'b0;
        end
    end

    assign awaddr   =   data_addr_reg;
    assign awsize   =   (data_size_reg == 2'd0) ? 3'd1 :
                        (data_size_reg == 2'd1) ? 3'd2 :
                        3'd4;

    always @(posedge clk)
    begin
        if(rst)
        begin
            awvalid <= 1'b0;
        end
        else if(axi_write_addr == 1'd1)
        begin
            awvalid <= 1'b1;
        end
        else if(awready == 1'b1)
        begin
            awvalid <= 1'b0;
        end
    end

    assign wdata    = data_reg2;
    assign wstrb    = wstrb_reg2;

    always @(posedge clk)
    begin
        if(rst)
        begin
            wvalid <= 1'b0;
        end
        else if(axi_write_data == 1'b1)
        begin
            wvalid <= 1'b1;
        end
        else if(wready == 1'b1)
        begin
            wvalid <= 1'b0;
        end
    end

    always @(posedge clk)
    begin
        if(rst)
        begin
            bready <= 1'b0;
        end
        else if(axi_write_finish == 1'b1)
        begin
            bready <= 1'b1;
        end
        else if(bvalid == 1'b1)
        begin
            bready <= 1'b0;
        end
    end

    ///////axi //////read
    reg axi_read_addr;
    reg source_reg1;
    reg [31:0] read_addr_reg;
    reg [1:0] read_size_reg;
    reg axi_read_finish;
    reg source_reg2;

    always @(posedge clk)
    begin
        if(rst)
        begin
            axi_read_addr <= 1'b0;
            source_reg1 <= 2'd2;
            read_addr_reg <= 32'b0;
            read_size_reg <= 2'b2;
        end
        else if(source != 2'd2)
        begin
            axi_read_addr <= 1'b1;
            source_reg1 <= source;
            read_addr_reg <= (source == 2'd0) ? inst_addr_reg : data_addr_reg;
            read_size_reg <= (source == 2'd0) ? inst_size_reg : data_size_reg;
        end
        else if((arvalid == 1'b1) && (arready == 1'b1))
        begin
            axi_read_addr <= 1'b0;
        end
    end

    always @(posedge clk)
    begin
        if(rst)
        begin
            axi_read_finish <= 1'b0;
            source_reg2 <= 2'd2;
        end
        else if((axi_read_addr == 1'b1) && (arvalid == 1'b1) && (arready == 1'b1))
        begin
            axi_read_finish <= 1'b1;
            source_reg2 <= source_reg1;
        end
        else if((rvalid == 1'b1) && (rready == 1'b1))
        begin
            axi_read_finish <= 1'b0;
        end
    end

    wire [1:0]source;
    assign source = (inst_addr_ok == 1'b1) ? 2'd0 :
                    (((data_addr_ok == 1'b1) && mode_read) || ((data_work_state == 1'b1) && (data_mode == 1'b0))) ? 2'd1 :
                    2'd2;

    assign arid     = source_reg1[0];
    assign araddr  = read_addr_reg;
    assign arsize   = read_size_reg;

    always @(posedge clk)
    begin
        if(rst)
        begin
            arvalid <= 1'b0;
        end
        else if(axi_read_addr == 1'b1)
        begin
            arvalid <= 1'b1;
        end
        else if(arready == 1'b1)
        begin
            arvalid <= 1'b0;
        end
    end

    always @(posedge clk)
    begin
        if(rst)
        begin
            rready <= 1'b0;
        end
        else if(axi_read_finish == 1'b1s)
        begin
            rready <= 1'b1;
        end
        else if(rvalid == 1'b1)
        begin
            rready <= 1'b0;
        end
    end

endmodule