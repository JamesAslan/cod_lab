`timescale 10ns / 1ns

`define ADDR_WIDTH 5
`define DATA_WIDTH 32

`define SPECIAL_TWO  6'b011100

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
`define ADDI 6'b001000
`define ADDU 6'b100001
`define ADD 6'b100000
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
`define SUBU 6'b100011
`define SUB 6'b100010
`define JR 6'b001000
`define OR 6'b100101
`define AND 6'b100100
`define XOR 6'b100110
`define NOR 6'b100111
`define SLL 6'b000000
`define SLLV 6'b000100
`define SRL 6'b000010
`define SRLV 6'b000110
`define SRA 6'b000011
`define SRAV 6'b000111
`define SLT 6'b101010
`define SLTU 6'b101011
`define MFHI 6'b010000
`define MFLO 6'b010010
`define MTHI 6'b010001
`define MTLO 6'b010011
`define MULT 6'b011000
`define MULTU 6'b011001

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
	wire validin;
	assign validin = 1'b1;


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

	assign debug_wb_pc = pipe4_PC;
	assign debug_wb_rf_wen = {wen,wen,wen,wen} & {debug ,debug ,debug ,debug };///////////////?????????
	assign debug_wb_rf_wnum = waddr;
	assign debug_wb_rf_wdata = wdata;

	reg[31:0] debug_decide;
	always @(posedge clk)
	begin
		if(rst)
		begin
			debug_decide <= 32'b0;
		end
		else 
		begin
			debug_decide <= pipe4_PC;
		end
	end
	wire debug;
	assign debug = (pipe4_PC == debug_decide) ? 1'b0 : 1'b1;

	////////////////////////////////////////////////////////////////////
	//  pipe1
	reg [31:0] pipe1_data;
	reg [31:0] pipe1_PC;
	reg pipe1_valid;
	wire pipe1_allowin;
	wire pipe1_readyout;
	wire pipe1_outvalid;
	assign pipe1_allowin = !pipe1_valid || (pipe2_allowin && pipe1_readyout);
	assign pipe1_readyout = 1'b1;
	assign pipe1_outvalid = pipe1_valid && pipe1_readyout;
	always @(posedge clk)
	begin
		if(rst)
		begin
			pipe1_valid <= 1'b0;
		end
		else if(pipe1_allowin)
		begin
			pipe1_valid <= validin;
		end
		else
		begin
			pipe1_valid <= pipe1_valid;
		end
		if(rst)
		begin
			pipe1_PC <= 32'b0;
		end
		else if(validin && pipe1_allowin)//////////////////////////////????????????????????//
		begin
			pipe1_PC <= PC;
		end
		else
		begin
			pipe1_PC <= pipe1_PC;
		end
		if(rst)
		begin
			pipe1_data <=32'b0;
		end
		else if((validin && pipe1_allowin) || ( counter_forwarding))//////////////////////////////????????????????????//
		begin
			pipe1_data <= Instruction;
		end
		else
		begin
			pipe1_data <= pipe1_data;
		end
	end

	reg [1:0]counter_choke;
	always@(posedge clk)
	begin
		if(rst || pipe1_allowin )
		begin
			counter_choke <= 2'b0;
		end
		else if(!pipe1_allowin)
		begin
			counter_choke <= 2'b1;
		end
		else 
		begin
			counter_choke <= counter_choke;
		end
	end

	reg [1:0]counter;
	always@(posedge clk)
	begin
		if(rst || pipe1_allowin )
		begin
			counter <= 2'b0;
		end
		else if(counter[1] == 1'b1)
		begin
			counter <= counter;
		end
		else if(pipe1_valid)
		begin
			counter <= counter + 2'd1;
		end
		else 
		begin
			counter <= counter;
		end
	end

	////////////////////////////////////////////////////////////////////
	//  pipe2
	reg [31:0] pipe2_PC;
	reg pipe2_valid;
	wire pipe2_allowin;
	wire pipe2_readyout;
	wire pipe2_outvalid;
	assign pipe2_allowin = !pipe2_valid || (pipe3_allowin && pipe2_readyout);
	assign pipe2_readyout = !counter_forwarding;
	assign pipe2_outvalid = pipe2_valid && pipe2_readyout;
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
		//数据段
		if(rst)
		begin
			aluop_decode_reg <= 12'b0;
			op_mul_reg <= 1'b0;
			mul_signed_reg <= 1'b0;
			pipe2_PC <= 32'b0;
			pipe2_data <= 32'b0;
			sign_extend_reg <= 32'b0;
			Shift_left2_reg <= 32'b0;
			alu_B_mux_reg <= 2'b0;
			alu_A_mux_reg <= 2'b0;
			reg_write2 <= 1'b0;
			memtoreg2 <= 2'b0;
			regDst2 <=1'b0;
			memRead2 <= 1'b0;
			memWrite2 <= 1'b0;
			branch_or_not_reg <= 1'b0;
			mthi_reg <= 1'b0;
			mtlo_reg <= 1'b0;
		end
		else if(pipe1_outvalid && pipe2_allowin)
		begin
			aluop_decode_reg <= aluop_decode;
			op_mul_reg <= op_mul;
			mul_signed_reg <= mul_signed;
			pipe2_PC <= pipe1_PC;
			pipe2_data <= pipe2_data_in;
			sign_extend_reg <= sign_extend;
			Shift_left2_reg <= Shift_left2;
			alu_B_mux_reg <= alu_B_mux;
			alu_A_mux_reg <= alu_A_mux;
			reg_write2 <= reg_write;
			memtoreg2 <= memtoreg;
			regDst2 <=regDst;
			memRead2 <= memRead;
			memWrite2 <= memWrite;
			branch_or_not_reg <= branch_or_not;
			mthi_reg <= mthi;
			mtlo_reg <= mtlo;
		end
		else
		begin
			aluop_decode_reg <= aluop_decode_reg;
			op_mul_reg <= op_mul_reg;
			mul_signed_reg <= mul_signed_reg;
			pipe2_PC <= pipe2_PC;
			pipe2_data <= pipe2_data;
			sign_extend_reg <= sign_extend_reg;
			Shift_left2_reg <= Shift_left2_reg;
			alu_B_mux_reg <= alu_B_mux_reg;
			alu_A_mux_reg <= alu_A_mux_reg;
			reg_write2 <= reg_write2;
			memtoreg2 <= memtoreg2;
			regDst2 <= regDst2;
			memRead2 <= memRead2;
			memWrite2 <= memWrite2;
			branch_or_not_reg <= branch_or_not_reg;
			mthi_reg <= mthi_reg;
			mtlo_reg <= mtlo_reg;
		end
		if(rst)
		begin
			rdata1_reg <= 32'b0;
			rdata2_reg <= 32'b0;
		end
		else if((pipe1_outvalid && pipe2_allowin) || counter_forwarding)
		begin
			if((pipe1_outvalid && pipe2_allowin) || forward_pipe3_r1_reg || forward_pipe4_r1_reg || forward_wait5_r1_reg)
			begin
				rdata1_reg <= rdata1_input;	
			end
			else
			begin
				rdata1_reg <= rdata1_reg;
			end
			if((pipe1_outvalid && pipe2_allowin) || forward_pipe3_r2_reg || forward_pipe4_r2_reg || forward_wait5_r2_reg)
			begin				
				rdata2_reg <= rdata2_input;	
			end
			else
			begin
				rdata2_reg <= rdata2_reg;
			end
		end
		else
		begin
			rdata1_reg <= rdata1_reg;
			rdata2_reg <= rdata2_reg;
		end
	end

	wire [31:0] pipe2_data_in;
	assign pipe2_data_in = (counter_choke ) ? pipe1_data : Instruction; //////////////////????????????!!!!!!!!!!


	//////////////////////////////////////////////////////
	//  pipe3
	reg [31:0] pipe3_PC;
	reg pipe3_valid;
	wire pipe3_allowin;
	wire pipe3_readyout;
	wire pipe3_outvalid;
	assign pipe3_allowin = !pipe3_valid || (pipe4_allowin && pipe3_readyout);
	assign pipe3_readyout = 1'b1;
	assign pipe3_outvalid = pipe3_valid && pipe3_readyout;
	always @(posedge clk)
	begin
		if(rst)
		begin
			pipe3_valid <= 1'b0;
		end
		else if(pipe3_allowin)
		begin
			pipe3_valid <= pipe2_outvalid;
		end
		else
		begin
			pipe3_valid <= pipe3_valid;
		end
		if(rst)
		begin
			pipe3_data <=  32'b0;
			pipe3_PC <= 32'b0;
			reg_write3 <= 1'b0;
			memtoreg3 <= 2'b0;
			regDst3 <= 1'b0;
			memRead3 <= 1'b0;
			memWrite3 <= 1'b0;
			Data <= 32'b0;
		end
		else if(pipe2_outvalid && pipe3_allowin)
		begin
			pipe3_data <=  pipe2_data;
			pipe3_PC <= pipe2_PC;
			reg_write3 <= reg_write2;
			memtoreg3 <= memtoreg2;
			regDst3 <= regDst2;
			memRead3 <= memRead2;
			memWrite3 <= memWrite2;
			Data <= rdata2_reg;
		end
		else
		begin
			pipe3_data <= pipe3_data;
			pipe3_PC <= pipe3_PC;
			reg_write3 <= reg_write3;
			memtoreg3 <= memtoreg3;
			regDst3 <= regDst3;
			memRead3 <= memRead3;
			memWrite3 <= memWrite3;
			Data <= Data;
		end
		if(rst)
		begin
			ALU_out3 <= 32'b0;
		end
		else if((pipe2_outvalid && pipe3_allowin))
		begin
			ALU_out3 <= ALU_out;
		end
		else
		begin
			ALU_out3 <= ALU_out3;
		end
	end

	////////////////////////////////////////////////////////
	//  pipe4
	reg [31:0] pipe4_data;
	reg [31:0] pipe4_PC;
	reg counter4;
	always@(posedge clk)
	begin
		if(rst || pipe1_allowin)
		begin
			counter4 <= 1'b0;
		end
		else if(pipe1_valid)
		begin
			counter4 <= 1'b1;
		end
		else 
		begin
			counter4 <= counter4;
		end
	end

	reg pipe4_valid;
	wire pipe4_allowin;
	wire pipe4_readyout;
	wire pipe4_outvalid;
	assign pipe4_allowin = !pipe4_valid || pipe4_readyout;
	assign pipe4_readyout = 1'b1;
	assign pipe4_outvalid = pipe4_valid && pipe4_readyout;
	always @(posedge clk)
	begin
		if(rst)
		begin
			pipe4_valid <= 1'b0;
		end
		else if(pipe4_allowin)
		begin
			pipe4_valid <= pipe1_outvalid;
		end
		else
		begin
			pipe4_valid <= pipe3_valid;
		end
		if(rst)
		begin
			pipe4_data <= 32'b0;
			pipe4_PC <= 32'b0;
			reg_write4 <= 1'b0;
			memtoreg4 <= 1'b0;
			regDst4 <= 1'b0;
			ALU_out4 <= 32'b0;
		end
		else if(pipe3_outvalid && pipe4_allowin)
		begin
			pipe4_data <=  pipe3_data;
			pipe4_PC <= pipe3_PC;
			reg_write4 <= reg_write3;
			memtoreg4 <= memtoreg3;
			regDst4 <= regDst3;
			ALU_out4 <= ALU_out3;
		end
		else
		begin
			pipe4_data <= pipe4_data;
			pipe4_PC <= pipe4_PC;
			reg_write4 <= reg_write4;
			memtoreg4 <= memtoreg4;
			regDst4 <= regDst4;
			ALU_out4 <= ALU_out4;
		end
	end





	////////////////////////////////////////////////////////
	//PHASE 2
	reg [11:0] aluop_decode_reg;
	reg [31:0] pipe2_data;
	reg [31:0] sign_extend_reg;
	reg [31:0] Shift_left2_reg;
	reg [1:0] alu_B_mux_reg;
	reg [1:0] alu_A_mux_reg;
	
	//////alu操作指令
	wire [11:0] aluop_decode;				//alu的操作指令
	assign aluop_decode[11]=op_add;
	assign aluop_decode[10]=op_sub;
	assign aluop_decode[ 9]=op_slt;
	assign aluop_decode[ 8]=op_sltu;
	assign aluop_decode[ 7]=op_and;
	assign aluop_decode[ 6]=op_nor;
	assign aluop_decode[ 5]=op_or;
	assign aluop_decode[ 4]=op_xor;
	assign aluop_decode[ 3]=op_sll;
	assign aluop_decode[ 2]=op_srl;
	assign aluop_decode[ 1]=op_sra;
	assign aluop_decode[ 0]=op_lui;

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

	assign alu_control[11] = aluop_decode_reg[11]; 
	assign alu_control[10] = aluop_decode_reg[10]; 
	assign alu_control[ 9] = aluop_decode_reg[9]; 
	assign alu_control[ 8] = aluop_decode_reg[8]; 
	assign alu_control[ 7] = aluop_decode_reg[7]; 
	assign alu_control[ 6] = aluop_decode_reg[6]; 
	assign alu_control[ 5] = aluop_decode_reg[5]; 
	assign alu_control[ 4] = aluop_decode_reg[4]; 
	assign alu_control[ 3] = aluop_decode_reg[3]; 
	assign alu_control[ 2] = aluop_decode_reg[2]; 
	assign alu_control[ 1] = aluop_decode_reg[1];  
	assign alu_control[ 0] = aluop_decode_reg[0]; 

	assign op_add = (inst_op == 6'b0 && inst_constent == `ADDU ) |
					(inst_op == 6'b0 && inst_constent == `ADD && inst_sa == 5'b0 ) |
					(inst_op == `ADDIU) |
					(inst_op == `ADDI) |
					(inst_op == `LW   ) |
					(inst_op == `SW   ) |
					(inst_op == `JAL  ) |
					(inst_op == 6'b0 && inst_constent == `JR   ) |
					1'd0;
	assign op_lui = (inst_op == `LUI  ) |
					1'd0;
	assign op_sub =	(inst_op == 6'b0 && inst_constent == `SUBU ) |
					(inst_op == 6'b0 && inst_constent == `SUB && inst_sa == 5'b0 ) |
					(inst_op == `BEQ  ) |
					(inst_op == `BNE  ) |
					1'd0;
	assign op_slt =	(inst_op == 6'b0 && inst_constent == `SLT  ) |
					(inst_op == `SLTI  ) |
					1'd0;
	assign op_sltu =(inst_op == 6'b0 && inst_constent == `SLTU ) |
					(inst_op == `SLTIU ) |
					1'd0;
	assign op_and =	(inst_op == 6'b0 && inst_constent == `AND  ) |
					(inst_op == `ANDI  ) |
					1'd0;
	assign op_or  =	(inst_op == 6'b0 && inst_constent == `OR   ) |
					(inst_op == `ORI ) |
					1'd0;
	assign op_xor =	(inst_op == 6'b0 && inst_constent == `XOR  ) |
					(inst_op == `XORI ) |
					1'd0;
	assign op_nor =	(inst_op == 6'b0 && inst_constent == `NOR  ) |
					1'd0;
	assign op_sll =	(inst_op == 6'b0 && inst_constent == `SLL && pipe2_data_in[25:21]==5'b0) |
					(inst_op == 6'b0 && inst_constent == `SLLV && inst_sa == 5'b0 ) |
					1'd0;
	assign op_srl =	(inst_op == 6'b0 && inst_constent == `SRL  ) |
					(inst_op == 6'b0 && inst_constent == `SRLV && inst_sa == 5'b0 ) |
					1'd0;
	assign op_sra =	(inst_op == 6'b0 && inst_constent == `SRA  ) |
					(inst_op == 6'b0 && inst_constent == `SRAV && inst_sa == 5'b0 ) |
					1'd0;
	
	
	

	wire [5:0] inst_constent;
	assign inst_constent = pipe2_data_in[5:0]; //从pipe1中取得指令

	wire [5:0] inst_op;
	assign inst_op = pipe2_data_in[31:26]; //从pipe1中取得指令

	wire [4:0] inst_sa;
	assign inst_sa = pipe2_data_in[10:6];
	
	//////sign_extend
	wire [31:0] sign_extend; 
	assign sign_extend = (pipe2_data_in[31:26]==6'b001100 || pipe2_data_in[31:26]==6'b001101 || pipe2_data_in[31:26]==6'b001110)?{16'b0,pipe2_data_in[15:0]}:
						 (pipe2_data_in[15])?{16'hffff,pipe2_data_in[15:0]}:{16'b0,pipe2_data_in[15:0]};

	//////Shift_left2
	wire [31:0] Shift_left2;
	assign Shift_left2= {sign_extend,2'b0};

	//////alu_B数据来源选择; 00为寄存器，10为sign_extend，11为Shift_left2
	wire [1:0] alu_B_mux;
	assign alu_B_mux =  (inst_op == 6'b0 || inst_op ==`BEQ || inst_op == `BNE) ? 2'b00:
						(inst_op == `JAL) ? 2'b01:
						(inst_op == `LUI || inst_op == `ADDIU  || inst_op == `ADDI  || inst_op == `LW || inst_op == `SW || inst_op == `SLTI || inst_op == `SLTIU || inst_op == `ANDI || inst_op == `ORI || inst_op == `XORI) ? 2'b10:
						//() ? 2'b11:
						2'b00;
	wire [1:0]alu_A_mux;
	assign alu_A_mux = (inst_op == `JAL) ? 2'b01 : 
						(op_sll || op_srl || op_sra) ? 2'b11 :         //sll  sra   srl
						2'b00;

	//////数据旁路
	wire [1:0]forwarding;
	wire forward_pipe3;
	wire forward_pipe4;
	wire forward_wait5;

	assign forwarding = forward_pipe3 ? 2'd1 : 
						forward_pipe4 ? 2'd2 :
						forward_wait5 ? 2'd3 :
						2'd0;
	assign forward_pipe3 = forward_pipe3_r1 || forward_pipe3_r2;
	assign forward_pipe4 = forward_pipe4_r1 || forward_pipe4_r2;
	assign forward_wait5 = forward_wait5_r1 || forward_wait5_r2;

	wire forward_pipe3_r1;
	wire forward_pipe3_r2;
	assign forward_pipe3_r1 = (((raddr1 == waddr_dst2) && (raddr1 != 5'b0)) ? 1'b1:1'b0) && (pipe2_data[31:26] != `LW) && reg_write2 && !counter_forwarding;
	assign forward_pipe3_r2 = (((raddr2 == waddr_dst2) && (raddr2 != 5'b0)) ? 1'b1:1'b0) && (pipe2_data[31:26] != `LW) && (pipe2_data_in[31:26] == 6'b0 || pipe2_data_in[31:26] == `BNE || pipe2_data_in[31:26] == `BEQ || pipe2_data_in[31:26] == `SW) && reg_write2 && !counter_forwarding;

	wire forward_pipe4_r1;
	wire forward_pipe4_r2;
	assign forward_pipe4_r1 = (((raddr1 == waddr_dst3) && (raddr1 != 5'b0)) ? 1'b1:1'b0) && reg_write3 && !counter_forwarding;
	assign forward_pipe4_r2 = (((raddr2 == waddr_dst3) && (raddr2 != 5'b0)) ? 1'b1:1'b0) && (pipe2_data_in[31:26] == 6'b0 || pipe2_data_in[31:26] == `BNE || pipe2_data_in[31:26] == `BEQ || pipe2_data_in[31:26] == `SW) && reg_write3 && !counter_forwarding;

	wire forward_wait5_r1;
	wire forward_wait5_r2;
	assign forward_wait5_r1 = (((raddr1 == waddr_dst4) && (raddr1 != 5'b0)) ? 1'b1:1'b0) && reg_write4 && !counter_forwarding;
	assign forward_wait5_r2 = (((raddr2 == waddr_dst4) && (raddr2 != 5'b0)) ? 1'b1:1'b0) && (pipe2_data_in[31:26] == 6'b0 || pipe2_data_in[31:26] == `BNE || pipe2_data_in[31:26] == `BEQ || pipe2_data_in[31:26] == `SW) && reg_write4 && !counter_forwarding;

	wire [31:0] rdata1_input;          ///////////////////////??????????????????????????!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	wire [31:0] rdata2_input;

	reg forward_pipe3_r1_reg;
	reg forward_pipe3_r2_reg;
	reg forward_pipe4_r1_reg;
	reg forward_pipe4_r2_reg;
	reg forward_wait5_r1_reg;
	reg forward_wait5_r2_reg;
	reg[1:0] forwarding_reg;
	always@(posedge clk)
	begin
		if(rst )
		begin
			forward_pipe3_r1_reg <= 1'b0;
			forward_pipe3_r2_reg <= 1'b0;
			forward_pipe4_r1_reg <= 1'b0;
			forward_pipe4_r2_reg <= 1'b0;
			forward_wait5_r1_reg <= 1'b0;
			forward_wait5_r2_reg <= 1'b0;
			forwarding_reg <= 2'b0;
		end
		else
		begin
			forward_pipe3_r1_reg <= forward_pipe3_r1;
			forward_pipe3_r2_reg <= forward_pipe3_r2;
			forward_pipe4_r1_reg <= forward_pipe4_r1;
			forward_pipe4_r2_reg <= forward_pipe4_r2;
			forward_wait5_r1_reg <= forward_wait5_r1;
			forward_wait5_r2_reg <= forward_wait5_r2;
			forwarding_reg <= forwarding;
		end
	end

	assign rdata1_input = ({32{forwarding_reg == 2'd0}} & rdata1) |
						  ({32{(forwarding_reg != 2'd0) && forward_pipe3_r1_reg }} & ALU_out3) |
						  ({32{(forwarding_reg != 2'd0) && forward_pipe4_r1_reg && !forward_pipe3_r1_reg}} & wdata) |
						  ({32{(forwarding_reg != 2'd0) && forward_wait5_r1_reg && !forward_pipe3_r1_reg && !forward_pipe4_r1_reg}} & ALU_out5);
	assign rdata2_input = ({32{forwarding_reg == 2'd0}} & rdata2) |
						  ({32{(forwarding_reg != 2'd0) && forward_pipe3_r2_reg }} & ALU_out3) |
						  ({32{(forwarding_reg != 2'd0) && forward_pipe4_r2_reg && !forward_pipe3_r2_reg}} & wdata) |
						  ({32{(forwarding_reg != 2'd0) && forward_wait5_r2_reg && !forward_pipe3_r2_reg && !forward_pipe4_r2_reg}} & ALU_out5);

	reg [31:0] ALU_out5;
	always@(posedge clk)
	begin
		if(rst)
		begin
			ALU_out5 <= 32'b0;
		end
		else
		begin
			ALU_out5 <= wdata;   //////////
		end
	end


	wire forward_r1;
	reg forward_r1_reg;
	wire forward_r2;
	reg forward_r2_reg;
	assign forward_r1 = forward_pipe3_r1 || forward_pipe4_r1 || forward_wait5_r1;
	assign forward_r2 = forward_pipe3_r2 || forward_pipe4_r2 || forward_wait5_r2;

	reg [1:0]counter_forwarding;
	always@(posedge clk)
	begin
		if(rst || ((forwarding == 2'd0) && pipe2_valid))
		begin
			counter_forwarding <= 2'b0;
			forward_r1_reg <= 1'b0;
			forward_r2_reg <= 1'b0;
		end
		else if(forwarding)
		begin
			counter_forwarding <= 2'b1;
			forward_r1_reg <= forward_r1;
			forward_r2_reg <= forward_r2;
		end
		else 
		begin
			counter_forwarding <= counter_forwarding;
			forward_r1_reg <= forward_r1_reg;
			forward_r2_reg <= forward_r2_reg;
		end
	end





	////////////////////////////////////////////////////////
	// PHASE 3
	reg [31:0] pipe3_data;
	wire [31:0] ALU_out;
	reg [31:0] ALU_out3;
	reg [31:0] ALU_out4;
	reg [31:0] Data;

	//////ALU
	wire [11:0] alu_control;
	wire [31:0] alu_A;
	wire [31:0] alu_B;
	wire [31:0] alu_result;
	wire overflow;
	wire carryout;
	wire zero;

	alu simple_alu(alu_control,alu_A,alu_B,alu_result,overflow,carryout,zero);

	assign alu_A = ({32{alu_A_mux_reg == 2'b01}} & pipe1_PC ) | 
					({32{alu_A_mux_reg == 2'b11}} & {27'b0,pipe2_data[10:6]}) | 
					({32{alu_A_mux_reg == 2'b00}} & rdata1_reg);
	assign alu_B = (alu_B_mux_reg == 2'b00) ? rdata2_reg:
					(alu_B_mux_reg == 2'b10) ? sign_extend_reg:
					(alu_B_mux_reg == 2'b11) ? Shift_left2_reg:
					32'd4;  


	assign ALU_out = alu_result;


	////////////////////////////////////////////////////////
	// PHASE 4
	assign MemRead = memRead3;
	assign MemWrite = memWrite3;

	wire memRead;
	reg memRead2;
	reg memRead3;
	assign memRead = (inst_op == `LW) ? 1'b1 : 1'b0;
	
	wire memWrite;
	reg memWrite2;
	reg memWrite3;
	assign memWrite = (inst_op == `SW) ? 1'b1 : 1'b0;

	assign Address = ALU_out3;

	assign Write_data = Data;


	////////////////////////////////////////////////////////
	// PHASE 5
	//////  register file 
	wire [`ADDR_WIDTH - 1:0] waddr;
	wire [`ADDR_WIDTH - 1:0] raddr1;
	wire [`ADDR_WIDTH - 1:0] raddr2;
	wire wen;
	wire [`DATA_WIDTH - 1:0] wdata;
	wire [`DATA_WIDTH - 1:0] rdata1;
	wire [`DATA_WIDTH - 1:0] rdata2;
	reg_file registers(clk,rst,waddr,raddr1,raddr2,wen,wdata,rdata1,rdata2);
	
	assign raddr1 = pipe2_data_in[25:21]; ///////////////
	assign raddr2 = pipe2_data_in[20:16]; ////////////////

	reg [31:0] rdata1_reg;
	reg [31:0] rdata2_reg;

	assign wdata =  ({32{memtoreg4 == 2'b1}} & Read_data) |
					({32{memtoreg4 == 2'b0}} & ALU_out4) |
					({32{memtoreg4 == 2'b10}} & HI) |
					({32{memtoreg4 == 2'b11}} & LO);/////////////??????????????????????????????????


	reg reg_write2;
	reg reg_write3;
	reg reg_write4;
	wire reg_write;
	assign reg_write = ((inst_op == 6'b0 && inst_constent == `JR) || inst_op ==`BEQ || inst_op == `BNE || inst_op == `SW || (inst_op == 6'b0 && inst_constent == `MULT && pipe2_data_in[15:6]==10'b0) || (inst_op == 6'b0 && inst_constent == `MULTU && pipe2_data_in[15:6]==10'b0) || mthi || mtlo) ? 1'b0:
						1'b1;

	assign wen = reg_write4 && pipe4_readyout;//////////////?????????????????????????????????????

	reg regDst2;
	reg regDst3;
	reg regDst4;
	wire regDst;
	assign regDst = (inst_op == `LUI || inst_op == `ADDIU || inst_op == `ADDI || inst_op == `LW || inst_op == `SW || inst_op == `JAL || inst_op == `SLTI || inst_op == `SLTIU || inst_op == `ANDI || inst_op == `ORI || inst_op == `XORI) ? 1'b0 : 1'b1;

	//assign waddr = (pipe4_data[31:26] == `JAL)?5'd31://JAL&JALR
	//			 	waddr_dst4;
	assign waddr = waddr_dst4;

	wire [4:0]waddr_dst4;
	assign waddr_dst4 = (pipe4_data[31:26] == `JAL) ? 5'd31 :
						(regDst4)? pipe4_data[15:11] : pipe4_data[20:16];

	wire [4:0]waddr_dst3;
	assign waddr_dst3 = (pipe3_data[31:26] == `JAL) ? 5'd31 :
						(regDst3)? pipe3_data[15:11] : pipe3_data[20:16];

	wire [4:0]waddr_dst2;
	assign waddr_dst2 = (pipe2_data[31:26] == `JAL) ? 5'd31 :
						(regDst2)? pipe2_data[15:11] : pipe2_data[20:16];

	reg[1:0] memtoreg2;
	reg[1:0] memtoreg3;
	reg[1:0] memtoreg4;
	wire[1:0] memtoreg;
	assign memtoreg = 	(inst_op == 6'b0 && inst_constent == `MFHI && pipe2_data_in[25:16]==10'b0 && pipe2_data_in[10:6]==5'b0) ? 2'b10 :
						(inst_op == 6'b0 && inst_constent == `MFLO && pipe2_data_in[25:16]==10'b0 && pipe2_data_in[10:6]==5'b0) ? 2'b11 :
						(inst_op == `LW) ? 2'b1 : 2'b0;



	//PC

	reg [31:0] PC_reg;
	wire PC_write;
	wire PC_choose;
	wire PC_change;

	assign PC = ({32{branch_or_not && pipe2_valid && !counter_forwarding}} & (pipe1_PC + Shift_left2_reg)) |
				({32{jump_or_not && pipe2_valid && !counter_forwarding}} & (jump_target)) |
	 			({32{!PC_change}} & PC_reg);

	wire [31:0] PC_branchafter;
	assign PC_branchafter = counter_choke ? pipe1_PC + Shift_left2_reg : pipe1_PC + Shift_left2_reg +32'd4;

	always @(posedge clk)
	begin
		if(rst)
		begin 
			PC_reg <= 32'hbfc00000;	
		end
		else if (branch_or_not && pipe2_valid && pipe1_allowin && !counter_forwarding) ////////////////?????????????????????!!!!!!!!!!!!!!!!!!!!
		begin
			PC_reg <= pipe1_PC + Shift_left2_reg +32'd4;
		end
		else if (branch_or_not && pipe2_valid && !counter_forwarding) ////////////////?????????????????????!!!!!!!!!!!!!!!!!!!!
		begin
			PC_reg <= pipe1_PC + Shift_left2_reg;
		end
		else if (jump_or_not && pipe2_valid && pipe1_allowin && !counter_forwarding)
		begin
			PC_reg <= jump_target +32'd4;
		end
		else if (jump_or_not && pipe2_valid && !counter_forwarding) ////////////////?????????????????????!!!!!!!!!!!!!!!!!!!!
		begin
			PC_reg <= jump_target;
		end
		else if (PC_write )
		begin
		
			if (PC_choose)	PC_reg <= 32'b0;
			else PC_reg <= PC_reg + 32'd4;
		end
		else
		begin
			PC_reg <= PC_reg;
		end		
	end

	assign PC_write = pipe1_allowin;
	assign PC_choose = 1'b0;

	wire branch_or_not;
	reg branch_or_not_reg;
	wire eq_or_not;
	assign beq = (pipe2_data[31:26] == `BEQ ) ? 1'b1 : 1'b0;
	assign bne = (pipe2_data[31:26] == `BNE ) ? 1'b1 : 1'b0;
	assign eq_or_not = (rdata1_reg == rdata2_reg) ? 1'b1 : 1'b0; 
	assign branch_or_not= (beq & eq_or_not) ||
							(bne & !eq_or_not);
	 
	wire jump_or_not;
	wire jal;
	assign jal = (pipe2_data[31:26] == `JAL ) ? 1'b1 : 1'b0;
	assign jr = (pipe2_data[31:26] == 6'b0 && pipe2_data[5:0] == `JR  ) ? 1'b1 : 1'b0;
	assign jump_or_not = jal || jr ;
	
	wire[31:0] jump_target;
	assign jump_target = ({32{jal == 1'b1}} & {pipe2_PC[31:28],pipe2_data[25:0],2'b0}) |
						({32{jr == 1'b1}} & rdata1_reg) ;
	
	assign PC_change = (jump_or_not || branch_or_not) && pipe2_valid && (!counter_forwarding);
	

	
	assign Write_strb = 4'b1111;

	//HI LO
	reg [31:0] HI;
	reg [31:0] LO;
	wire HI_wen;
	wire LO_wen;
	wire [31:0] HI_data;
	wire [31:0] LO_data;
	always@(posedge clk) 
	begin
		if(rst)
		begin 
			HI <= 32'b0;
		end	
		else if(HI_wen)
		begin 
			HI <= HI_data;
		end
		else
		begin 
			HI <= HI;
		end
    end

	always@(posedge clk) 
	begin
		if(rst)
		begin 
			LO <= 32'b0;
		end	
		else if(LO_wen)
		begin 
			LO <= LO_data;
		end
		else
		begin 
			LO <= LO;
		end
    end

	assign LO_wen = op_mul_reg || mtlo_reg;
	assign HI_wen = op_mul_reg || mthi_reg;

	assign LO_data = mtlo_reg ? rdata1_reg : mul_result[31:0];
	assign HI_data = mthi_reg ? rdata1_reg : mul_result[63:32];

	wire mthi;
	wire mtlo;
	reg mthi_reg;
	reg mtlo_reg;
	assign mthi = (inst_op == 6'b0 && inst_constent == `MTHI && pipe2_data_in[20:6]==15'b0) ? 1'b1 : 1'b0;
	assign mtlo = (inst_op == 6'b0 && inst_constent == `MTLO && pipe2_data_in[20:6]==15'b0) ? 1'b1 : 1'b0;

	//////////////////////////////

	wire [63:0] mul_result;
	wire mul_signed;
	reg mul_signed_reg;
	wire [31:0] mul_x;
	wire [31:0] mul_y;
	wire op_mul;
	reg op_mul_reg;

	assign op_mul = (inst_op == 6'b0 && inst_constent == `MULT && pipe2_data_in[15:6]==10'b0) |
					(inst_op == 6'b0 && inst_constent == `MULTU && pipe2_data_in[15:6]==10'b0) |
					1'b0;
	assign mul_signed = (inst_op == 6'b0 && inst_constent == `MULT && pipe2_data_in[15:6]==10'b0) |
						1'b0;

	assign mul_x = {32{op_mul_reg}} & rdata1_reg;
	assign mul_y = {32{op_mul_reg}} & rdata2_reg;


	mul mul(clk, resetn, mul_signed_reg, mul_x, mul_y, mul_result); 

endmodule
