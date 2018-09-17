`timescale 10ns / 1ns

`define ADDR_WIDTH 5
`define DATA_WIDTH 32

`define SW 6'b101011
`define SB 6'b101000
`define SH 6'b101001
`define SWL 6'b101010
`define SWR 6'b101110
`define NOP 6'b000000
`define LW 6'b100011
`define LB 6'b100000
`define LBU 6'b100100
`define LH 6'b100001
`define LHU 6'b100101
`define LWL 6'b100010
`define LWR 6'b100110
`define BNE 6'b000101
`define ADDIU 6'b001001
`define ADDU 6'b100001
`define SUBU 6'b000000
`define BEQ 6'b000100
`define REGIMM 6'b000001
`define BLEZ 6'b000110
`define BGTZ 6'b000111
`define J 6'b000010
`define JAL 6'b000011
`define LUI 6'b001111
`define SLTIU 6'b001011
`define SLTI 6'b001010
`define ANDI 6'b001100
`define ORI 6'b001101
`define XORI 6'b001110

module mycpu_top(
	input  resetn,
	input  clk,

	output [31:0] inst_sram_addr,
	input  [31:0] inst_sram_rdata,
	output [31:0] inst_sram_wdata,
	output [3:0] inst_sram_wen,
	output inst_sram_en,
	
	output data_sram_en,
	output [31:0] data_sram_addr,
	output [31:0] data_sram_wdata,
	output [3:0] data_sram_wen,
	input  [31:0] data_sram_rdata,
	
	output [31:0] debug_wb_pc,
	output [3:0] debug_wb_rf_wen,
	output [4:0] debug_wb_rf_wnum,
	output [31:0] debug_wb_rf_wdata
);

	wire rst;
	assign rst=!resetn;

	wire [31:0] PC;
	wire Inst_Req_Valid;
	wire Inst_Req_Ack;

	wire [31:0] Instruction;
	wire Inst_Valid;
	wire Inst_Ack;

	wire [31:0] Address;
	wire MemWrite;
	wire [31:0] Write_data;
	wire [3:0] Write_strb;
	wire MemRead;
	wire Mem_Req_Ack;

	wire [31:0] Read_data;
	wire Read_data_Valid;
	wire Read_data_Ack;

	/////////////
	assign Inst_Req_Ack = 1'b1;
	assign Inst_Valid =1'b1;
	assign Read_data_Valid = 1'b1;
	assign Mem_Req_Ack = 1'b1;


	assign inst_sram_wdata = 32'b0;
	assign inst_sram_wen =4'b0;
	assign inst_sram_addr = PC;
	assign Instruction = inst_sram_rdata;
	assign inst_sram_en = 1'b1;

	assign data_sram_en = MemRead | MemWrite;
	assign data_sram_wen = {MemWrite,MemWrite,MemWrite,MemWrite}& Write_strb;
	assign data_sram_addr = Address;
	assign Read_data = data_sram_rdata;
	assign data_sram_wdata = Write_data;

	assign debug_wb_pc = PC_reg_default;
	assign debug_wb_rf_wen = {wen,wen,wen,wen};
	assign debug_wb_rf_wnum = waddr;
	assign debug_wb_rf_wdata = wdata;


	////////////////////////////////////////////////////////////////////
	//  pipe1

	reg pipe1_valid;
	wire pipe1_allowin;
	wire pipe1_readyout;
	wire pipe1_outvalid;
	assign pipe1_allowin = pipe2_allowin && (!pipe1_valid || pipe1_readyout);
	assign pipe1_readyout = counter;
	assign pipe1_outvalid = pipe1_valid && pipe1_ready_go;
	always @(posedge clk)
	begin
		if(rst)
		begin
			pipe1_valid <= 1'b0;
		end
		else if(pipe1_allowin)
		begin
			pipe1_valid <= datainvalid;
		end
		else
		begin
			pipe1_valid <= pipe1_valid;
		end
		if(validin && pipe_allowin)
		begin
			pipe1_data <=  ;
		end
		else
		begin
			pipe1_data <= pipe1_data;
		end
	end

	reg counter;
	always@(posedge clk)
	begin
		if(rst || pipe1_allowin)
		begin
			counter <= 1'b0;
		end
		else if(pipe1_valid)
		begin
			counter <= 1'b1;
		end
		else 
		begin
			counter <= counter;
		end
	end

	////////////////////////////////////////////////////////////////////
	//  pipe2

	reg pipe2_valid;
	wire pipe2_allowin;
	wire pipe2_readyout;
	wire pipe2_outvalid;
	assign pipe2_allowin = pipe3_allowin && (!pipe2_valid || pipe2_readyout);
	assign pipe2_readyout = 1'b1;
	assign pipe2_outvalid = pipe2_valid && pipe2_ready_go;
	always @(posedge clk)
	begin
		if(rst)
		begin
			pipe2_valid <= 1'b0;
		end
		else if(pipe2_allowin)
		begin
			pipe2_valid <= pipe1_outvalid;
		end
		else
		begin
			pipe2_valid <= pipe2_valid;
		end
		if(validin && pipe_allowin)
		begin
			pipe2_data <=  pipe1_data;
		end
		else
		begin
			pipe2_data <= pipe2_data;
		end
	end









	reg	[11:0] alu_control;
	reg [31:0] alu_A;
	reg [31:0] alu_B;
	reg [31:0] alu_result;
	reg overflow;
	reg carryout;
	reg zero;

	alu simple_alu(alu_control,alu_A,alu_B,alu_result,overflow,carryout,zero);







	//PHASE 2

	wire op_add;   
    wire op_sub;
    wire op_slt;
    wire op_sltu;
    wire op_and;
    wire op_nor;
    wire op_or;
    wire op_xor;
    wire op_sll;
    wire op_srl;
    wire op_sra;
    wire op_lui;

	assign alu_control[11] = op_add; 
	assign alu_control[10] = op_sub; 
	assign alu_control[ 9] = op_slt; 
	assign alu_control[ 8] = op_sltu; 
	assign alu_control[ 7] = op_and; 
	assign alu_control[ 6] = op_nor; 
	assign alu_control[ 5] = op_or; 
	assign alu_control[ 4] = op_xor; 
	assign alu_control[ 3] = op_sll; 
	assign alu_control[ 2] = op_srl; 
	assign alu_control[ 1] = op_sra;  
	assign alu_control[ 0] = op_lui; 

	assign op_add = (inst_constent == LUI  ) |
					(inst_constent == ADDU ) |
					(inst_constent == ADDIU) |
					(inst_constent == LW   ) |
					(inst_constent == SW   ) |
					(inst_constent == JAL  ) |
					(inst_constent == JR   ) |
					1'd0;
	assign op_sub =	(inst_constent == SUBU ) |
					(inst_constent == BEQ  ) |
					(inst_constent == BNE  ) |
					1'd0;
	assign op_slt =	(inst_constent == SLT  ) |
					(inst_constent == SLTU ) |
					1'd0;
	assign op_and =	(inst_constent == AND  ) |
					1'd0;
	assign op_or  =	(inst_constent == OR   ) |
					1'd0;
	assign op_xor =	(inst_constent == XOR  ) |
					1'd0;
	assign op_nor =	(inst_constent == NOR  ) |
					1'd0;
	assign op_sll =	(inst_constent == SLL  ) |
					1'd0;
	assign op_srl =	(inst_constent == SRL  ) |
					1'd0;
	assign op_sra =	(inst_constent == SRA  ) |
					1'd0;
	
	
	
	
	
	// PHASE 3


	wire [5:0] inst_content;
	assign inst_content = Instruction[31:26];

	//PC

	reg [31:0] PC_reg;

	always @(posedge clk)
	begin
		if(rst)
		begin 
			PC_reg <= 32'hbfc00000;	
		end
		else if (PC_write )
		begin
			if (PC_choose)	PC_reg <= PHASE2_PC;
			else PC_reg <= PC_reg + 32'd4;
		end
		else
		begin
			PC_reg <= PC_reg;
		end		
	end


endmodule
