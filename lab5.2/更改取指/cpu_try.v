`timescale 10ns / 1ns

`define ADDR_WIDTH 5
`define DATA_WIDTH 32

`define SPECIAL_TWO  6'b011100
`define REGIMM 6'b000001

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
`define BLTZ 5'b00000
`define BLTZAL 5'b10000
`define BGTZ 6'b000111
`define BGEZ 5'b00001
`define BGEZAL 5'b10001
`define J 6'b000010
`define JAL 6'b000011
`define JALR 6'b001001
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
`define DIV 6'b011010
`define DIVU 6'b011011
`define SYSCALL 6'b001100
`define BREAK 6'b001101
`define MFC0 5'b00000
`define MTC0 5'b00100
`define ERET 6'b011000

module mycpu(
	input  resetn,
	input  clk,

    output inst_sram_en,  
    output inst_ram_wr,
    output [1:0] inst_ram_size,   
	output [31:0] inst_sram_addr,
	input  [31:0] inst_sram_rdata,
	output [31:0] inst_sram_wdata,
    input  inst_sram_addr_ok,  
    input  inst_sram_data_ok,  
	
	output data_sram_en,  
    output data_sram_wr,  
    output [1:0] data_sram_size,  ///////////////////
	output [31:0] data_sram_addr,
	output [31:0] data_sram_wdata,
	output [3:0] data_sram_wen,
	input  [31:0] data_sram_rdata,
    input  data_sram_addr_ok,  /////////////////
    input  data_sram_data_ok,  ////////////////
	
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

	assign Inst_Req_Ack = 1'b1;
	assign Inst_Valid =1'b1;
	assign Read_data_Valid = 1'b1;
	assign Mem_Req_Ack = 1'b1;
	wire validin;
	assign validin = 1'b1;


	assign inst_sram_wdata = 32'b0;
	assign inst_sram_addr = PC;
	assign Instruction = inst_sram_rdata;
	assign inst_sram_en = ((inst_irp_choke == 2'd2) || (inst_state[0] && ((!syscall_info2 && !break_info2 && !irp_ip1_2 && !irp_ip0_2  && !irp_inst2 && !irp_time2 && irp_support2 && !(irp_overflow || irp_overflow3 )&& !irp_address_lw && !irp_address_sw) || pipe1_allowin)))&& resetn;

	assign data_sram_en = (MemRead | MemWrite) && data_state[0] && resetn;
    assign data_sram_wr = MemWrite;
	assign data_sram_wen = {MemWrite,MemWrite,MemWrite,MemWrite}& Write_strb;
	assign data_sram_addr = Address ;//& 32'h1fffffff;
	assign Read_data = Read_data_reg;
	assign data_sram_wdata = Write_data;

	assign debug_wb_pc = pipe4_PC;
	assign debug_wb_rf_wen = {wen,wen,wen,wen} & {debug_input ,debug_input ,debug_input ,debug_input };///////////////?????????
	assign debug_wb_rf_wnum = waddr;
	assign debug_wb_rf_wdata = wdata;

    ///////////////////////////////sram
    assign inst_ram_wr = 1'b0;
    assign inst_ram_size = 2'b10;
    assign data_sram_size = (pipe3_data[31:26] == `SW) ? 2'b10 :
                            (pipe3_data[31:26] == `SH) ? 2'b01 :
                            (pipe3_data[31:26] == `SB) ? 2'b00 : 
                            2'b10;

    reg [2:0] inst_state;
    always @(posedge clk)
	begin
		if(rst)
		begin
			inst_state <= 3'b001;
		end
		else if((inst_state == 3'b000) && (!inst_state_choke || inst_irp_choke == 2'd2))
		begin
			inst_state <= 3'b001;
		end
        else if((inst_state == 3'b001) && (inst_sram_addr_ok))
        begin
            inst_state <= 3'b010;
        end
        else if((inst_state == 3'b010) && (inst_sram_data_ok) )
        begin
            inst_state <= 3'b000;
        end
	end

	reg [1:0]inst_state_choke;
    always @(posedge clk)
	begin
		if(rst)
		begin
			inst_state_choke <= 2'b0;
		end
		else if(inst_sram_data_ok && !pipe1_allowin && (inst_irp_choke != 2'd1))
		begin
			inst_state_choke <= 2'b1;
		end
        else if((inst_state_choke == 2'b1) && !branch_choke_must )
        begin
            inst_state_choke <= 2'd2;
        end
		else if(inst_state_choke == 2'd2)
        begin
            inst_state_choke <= 2'b0;
        end
	end

	reg [1:0]inst_irp_choke;
    always @(posedge clk)
	begin
		if(rst)
		begin
			inst_irp_choke <= 2'd0;
		end
		else if((irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3 || eret)&& pipe3_valid)
		begin
			inst_irp_choke <= 2'd1;
		end
        else if((inst_irp_choke == 2'd1) && inst_sram_data_ok)
        begin
            inst_irp_choke <= 2'd2;
        end
		else if(inst_irp_choke == 2'd2 && inst_sram_data_ok)
		begin
            inst_irp_choke <= 2'd0;
        end
	end

	reg inst_once_choke;
	always @(posedge clk)
	begin
		if(rst)
		begin
			inst_once_choke <= 1'b0;
		end
		else if(pipe1_allowin)
		begin
			inst_once_choke <= 1'b0;
		end
        else if(inst_sram_data_ok && !pipe1_allowin)
        begin
            inst_once_choke <= 1'b1;
        end
	end

	reg [31:0] inst_remain;
	always @(posedge clk)
	begin
		if(rst)
		begin
			inst_remain <= 32'b0;
		end
		else if(inst_sram_data_ok)
		begin
			inst_remain <= inst_sram_rdata;
		end
	end

    reg [2:0] data_state;
    always @(posedge clk)
	begin
		if(rst)
		begin
			data_state <= 3'b000;
		end
		else if(data_state == 3'b000)
		begin
			data_state <= 3'b001;
		end
        else if((data_state == 3'b001) && (data_sram_addr_ok))
        begin
            data_state <= 3'b010;
        end
        else if((data_state == 3'b010) && (data_sram_data_ok))
        begin
            data_state <= 3'b000;
        end
	end

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
    wire debug_input;
    assign debug_input = debug ;

	////////////////////////////////////////////////////////////////////
	//  pipe1
	reg [31:0] pipe1_data;
	reg [31:0] pipe1_PC;
	reg pipe1_valid;
	wire pipe1_allowin;
	wire pipe1_readyout;
	wire pipe1_outvalid;
	assign pipe1_allowin = !pipe1_valid || (pipe2_allowin && pipe1_readyout);
	assign pipe1_readyout = (inst_sram_data_ok || inst_state_choke==2'd2) && !branch_choke_must && (inst_irp_choke != 2'd1);
	assign pipe1_outvalid = pipe1_valid && pipe1_readyout;
	always @(posedge clk)
	begin
		if(rst)
		begin
			pipe1_valid <= 1'b1;
		end
		else if((irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3 || ((pipe1_data[31:26] == 6'b010000) && (pipe1_data[25] == 1'b1) && (pipe1_data[24:6] == 19'b0) &&(pipe1_data[5:0] == `ERET))) && pipe3_valid)
		begin
			pipe1_valid <= 1'b0;
		end
		else if(pipe1_allowin)
		begin
			pipe1_valid <= validin;
		end
		if(rst)
		begin
			pipe1_PC <= 32'b0;
			irp_inst1 <= 1'b0;
		end
		else if(validin && pipe1_allowin)//////////////////////////////????????????????????//
		begin
			pipe1_PC <= PC;
			irp_inst1 <= irp_inst;
		end
		if(rst)
		begin
			pipe1_data <=32'b0;
		end
		else if(inst_sram_data_ok && inst_irp_choke == 1'd1)
		begin
			pipe1_data <= Instruction;
		end
		else if(validin && pipe1_allowin) //|| (irp_counter != 2'd1))//////////////////////////////????????????????????//
		begin
			if(inst_once_choke && inst_irp_choke != 2'd2)
				pipe1_data <= inst_remain;
			else
				pipe1_data <= Instruction;
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
	reg syscall_info2;
	reg [31:0] pipe2_PC;
	reg pipe2_valid;
	wire pipe2_allowin;
	wire pipe2_readyout;
	wire pipe2_outvalid;
	assign pipe2_allowin = !pipe2_valid || (pipe3_allowin && pipe2_readyout);
	assign pipe2_readyout = (!counter_lw_choke ) && (!counter_div_choke)&& (!(op_div_reg == 1'b1 && op_div_last_reg ==1'b0) || syscall_info3) && (!(op_mul_reg == 1'b1 && op_mul_last_reg ==1'b0)|| syscall_info3);
	assign pipe2_outvalid = pipe2_valid && pipe2_readyout;
	always @(posedge clk)
	begin
		if(rst)
		begin
			pipe2_valid <= 1'b0;
		end
		else if((irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3)&& pipe3_valid)
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
			branch_op_reg <= 1'b0;
			slot_or_not_reg2 <= 1'b0;
			irp_ip2 <= 8'b0;
			irp_ip0_2 <= 1'b0;
			irp_ip1_2 <= 1'b0;
			irp_ip2_2 <= 1'b0;
			irp_ip3_2 <= 1'b0;
			irp_ip4_2 <= 1'b0;
			irp_ip5_2 <= 1'b0;
			irp_ip6_2 <= 1'b0;
			irp_ip7_2 <= 1'b0;
			irp_inst2 <= 1'b0;
			irp_support2 <= 1'b1;
			irp_time2 <= 1'b0;
			break_info2 <= 1'b0;
			syscall_info2 <= 1'b0;
			syscall_in_slot2 <= 1'b0;
			cp0_wen_epc2 <= 1'b0;
			cp0_wen_status2 <= 1'b0;
			cp0_wen_cause2 <= 1'b0;
			cp0_wen_count2 <= 1'b0;
			cp0_wen_compare2 <= 1'b0;
			aluop_decode_reg <= 12'b0;
			op_mul_reg <= 1'b0;
			mul_signed_reg <= 1'b0;
			op_div_reg <= 1'b0;
			div_signed_reg <= 1'b0;
			pipe2_PC <= 32'b0;
			pipe2_data <= 32'b0;
			sign_extend_reg <= 32'b0;
			Shift_left2_reg <= 32'b0;
			alu_B_mux_reg <= 2'b0;
			alu_A_mux_reg <= 2'b0;
			reg_write2 <= 1'b0;
			memtoreg2 <= 3'b0;
			regDst2 <=1'b0;
			memRead2 <= 1'b0;
			memWrite2 <= 1'b0;
			mthi_reg <= 1'b0;
			mtlo_reg <= 1'b0;
			forward_mfhi_reg <= 1'b0;
			forward_mflo_reg <= 1'b0;
			forward_pipe3_r1_reg <= 1'b0;
			forward_pipe3_r2_reg <= 1'b0;
			forward_pipe4_r1_reg <= 1'b0;
			forward_pipe4_r2_reg <= 1'b0;
			forward_wait5_r1_reg <= 1'b0;
			forward_wait5_r2_reg <= 1'b0;
            lw_choke_r1_reg <= 1'b0;
			lw_choke_r2_reg <= 1'b0;
		end
		else if(pipe1_outvalid && pipe2_allowin)
		begin
			branch_op_reg <= branch_op || jump_or_not;
			slot_or_not_reg2 <= slot_or_not_reg;
			irp_ip2 <= irp_ip;
			irp_ip0_2 <= irp_ip0_reg || irp_ip[0];
			irp_ip1_2 <= irp_ip1_reg || irp_ip[1];
			irp_ip2_2 <= irp_ip[2]; //irp_ip2_reg || irp_ip[2];
			irp_ip3_2 <= irp_ip[3]; //irp_ip3_reg || irp_ip[3];
			irp_ip4_2 <= irp_ip[4]; //irp_ip4_reg || irp_ip[4];
			irp_ip5_2 <= irp_ip[5]; //irp_ip5_reg || irp_ip[5];
			irp_ip6_2 <= irp_ip[6]; //irp_ip6_reg || irp_ip[6];
			irp_ip7_2 <= irp_ip[7]; //irp_ip7_reg || irp_ip[7];
			irp_inst2 <= irp_inst1;
			irp_support2 <= irp_support;
			irp_time2 <= irp_time || irp_time_reg;
			break_info2 <= break_info;
			syscall_info2 <=syscall_info;
			syscall_in_slot2 <= syscall_in_slot;
			cp0_wen_epc2 <= cp0_wen_epc;
			cp0_wen_status2 <= cp0_wen_status;
			cp0_wen_cause2 <= cp0_wen_cause;
			cp0_wen_count2 <= cp0_wen_count;
			cp0_wen_compare2 <= cp0_wen_compare;
			aluop_decode_reg <= aluop_decode;
			op_mul_reg <= (op_mul && !(irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3));
			mul_signed_reg <= mul_signed;
			op_div_reg <= (op_div && !(irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3) && !(irp_ip2_2 || irp_ip3_2 || irp_ip4_2 || irp_ip5_2 || irp_ip6_2 || irp_ip7_2 || irp_ip1_2|| irp_ip0_2 || irp_address_sw || irp_address_lw || irp_inst2 || !irp_support2 || irp_time2 || irp_overflow || break_info2 || syscall_info2));//////////////////////////////////////////////?????????????????????????????????????
			div_signed_reg <= div_signed;
			pipe2_PC <= pipe1_PC;
			pipe2_data <= /*(inst_irp_choke == 2'd2 && inst_sram_data_ok) ? inst_remain:*/pipe1_data;
			sign_extend_reg <= sign_extend;
			Shift_left2_reg <= Shift_left2;
			alu_B_mux_reg <= alu_B_mux;
			alu_A_mux_reg <= alu_A_mux;
			reg_write2 <= reg_write;
			memtoreg2 <= memtoreg;
			regDst2 <=regDst;
			memRead2 <= memRead;
			memWrite2 <= memWrite;
			mthi_reg <= mthi;
			mtlo_reg <= mtlo;
			forward_mfhi_reg <= forward_mfhi;
			forward_mflo_reg <= forward_mflo;
			forward_pipe3_r1_reg <= forward_pipe3_r1;
			forward_pipe3_r2_reg <= forward_pipe3_r2;
			forward_pipe4_r1_reg <= forward_pipe4_r1;
			forward_pipe4_r2_reg <= forward_pipe4_r2;
			forward_wait5_r1_reg <= forward_wait5_r1;
			forward_wait5_r2_reg <= forward_wait5_r2;
            lw_choke_r1_reg <= lw_choke_r1;
			lw_choke_r2_reg <= lw_choke_r2;
		end
		if(rst)
		begin
			rdata1_reg <= 32'b0;
			rdata2_reg <= 32'b0;
			HI_reg2 <= 32'b0;
			LO_reg2 <= 32'b0;
		end
		else if(counter_lw_choke == 2'd3)
		begin
			if(lw_choke_r1_reg)
			begin
				rdata1_reg <= Read_data_result;
			end
			else
			begin
				rdata1_reg <= rdata1_reg;
			end
			if(lw_choke_r2_reg)
			begin
				rdata2_reg <= Read_data_result;
			end
			else
			begin
				rdata2_reg <= rdata2_reg;
			end
			HI_reg2 <= HI_reg2;
			LO_reg2 <= LO_reg2;
		end
		else if(pipe1_outvalid && pipe2_allowin) 
		begin
			if((pipe1_outvalid && pipe2_allowin) || forward_pipe3_r1_reg || forward_pipe4_r1_reg || forward_wait5_r1_reg)
			begin
				rdata1_reg <= rdata1_input;	
			end
			else
			begin
				rdata1_reg <= rdata1_reg;
			end
			if(((pipe1_outvalid && pipe2_allowin) || forward_pipe3_r2_reg || forward_pipe4_r2_reg || forward_wait5_r2_reg) && (!counter_div_choke || div_complete))
			begin				
				rdata2_reg <= rdata2_input;	
			end
			else
			begin
				rdata2_reg <= rdata2_reg;
			end
			if((pipe1_outvalid && pipe2_allowin) || forward_mfhi_reg)
			begin				
				HI_reg2 <= HI_data;	
			end
			else
			begin
				HI_reg2 <= HI_reg2;
			end
			if((pipe1_outvalid && pipe2_allowin) || forward_mflo_reg)
			begin				
				LO_reg2 <= LO_data;	
			end
			else
			begin
				LO_reg2 <= LO_reg2;
			end
		end
		else
		begin
			rdata1_reg <= rdata1_reg;
			rdata2_reg <= rdata2_reg;
			HI_reg2 <= HI_reg2;
			LO_reg2 <= LO_reg2;
		end
		if(rst)
		begin
			cp0_read_data2 <= 32'b0;
			cp0_destination2 <= 8'b0;
		end
		else if(pipe1_outvalid && pipe2_allowin)
		begin
			cp0_read_data2 <= cp0_read_data;
			cp0_destination2 <= cp0_destination;
		end
		else
		begin
			cp0_read_data2 <= cp0_read_data2;
			cp0_destination2 <= cp0_destination2;
		end
	end

	//////////////////////////////////////////////////////
	//  pipe3
	reg syscall_info3;
	reg [31:0] pipe3_PC;
	reg pipe3_valid;
	wire pipe3_allowin;
	wire pipe3_readyout;
	wire pipe3_outvalid;
	assign pipe3_allowin = !pipe3_valid || (pipe4_allowin && pipe3_readyout);
	assign pipe3_readyout = (MemRead | MemWrite) ? ((data_state == 3'b000) && !eret) : 1'b1;
	assign pipe3_outvalid = pipe3_valid && pipe3_readyout;
	always @(posedge clk)
	begin
		if(rst)
		begin
			pipe3_valid <= 1'b0;
		end
		else if((irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3)&& pipe3_valid)
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
			slot_or_not_reg3 <= 1'b0;
			irp_ip3 <= 1'b0;
			irp_ip0_3 <= 1'b0;
			irp_ip1_3 <= 1'b0;
			irp_ip2_3 <= 1'b0;
			irp_ip3_3 <= 1'b0;
			irp_ip4_3 <= 1'b0;
			irp_ip5_3 <= 1'b0;
			irp_ip6_3 <= 1'b0;
			irp_ip7_3 <= 1'b0;
			irp_address_sw3 <= 1'b0;
			irp_address_lw3 <= 1'b0;
			irp_inst3 <= 1'b0;
			irp_support3 <= 1'b0;
			irp_time3 <= 1'b0;
			irp_overflow3 <= 1'b0;
			break_info3 <= 1'b0;
			syscall_info3 <= 1'b0;
			syscall_in_slot3 <= 1'b0;
			pipe3_data <=  32'b0;
			pipe3_PC <= 32'b0;
			reg_write3 <= 1'b0;
			memtoreg3 <= 3'b0;
			regDst3 <= 1'b0;
			memRead3 <= 1'b0;
			memWrite3 <= 1'b0;
			Data <= 32'b0;
			HI_reg3 <= 32'b0;
			LO_reg3 <= 32'b0;
			forward_pipe3_r1_reg3 <= 1'b0;
			forward_pipe3_r2_reg3 <= 1'b0;
			forward_pipe4_r1_reg3 <= 1'b0;
			forward_pipe4_r2_reg3 <= 1'b0;
			forward_wait5_r1_reg3 <= 1'b0;
			forward_wait5_r2_reg3 <= 1'b0;
		end
		else if(pipe2_outvalid && pipe3_allowin)
		begin
			slot_or_not_reg3 <= slot_or_not_reg2;
			irp_ip3 <= irp_ip2;
			irp_ip0_3 <= irp_ip0_2;
			irp_ip1_3 <= irp_ip1_2;
			irp_ip2_3 <= irp_ip2_2;
			irp_ip3_3 <= irp_ip3_2;
			irp_ip4_3 <= irp_ip4_2;
			irp_ip5_3 <= irp_ip5_2;
			irp_ip6_3 <= irp_ip6_2;
			irp_ip7_3 <= irp_ip7_2;
			irp_address_sw3 <= irp_address_sw;
			irp_address_lw3 <= irp_address_lw;
			irp_inst3 <= irp_inst2;
			irp_support3 <= ~irp_support2;
			irp_time3 <= irp_time2;
			irp_overflow3 <= irp_overflow;
			break_info3 <= break_info2;
			syscall_info3 <= syscall_info2;
			syscall_in_slot3 <= syscall_in_slot2;
			pipe3_data <=  pipe2_data;
			pipe3_PC <= pipe2_PC;
			reg_write3 <= reg_write2;
			memtoreg3 <= memtoreg2;
			regDst3 <= regDst2;
			memRead3 <= memRead2;
			memWrite3 <= memWrite2;
			Data <= Data_input;
			HI_reg3 <= HI;
			LO_reg3 <= LO;
			forward_pipe3_r1_reg3 <= forward_pipe3_r1_reg;
			forward_pipe3_r2_reg3 <= forward_pipe3_r2_reg;
			forward_pipe4_r1_reg3 <= forward_pipe4_r1_reg;
			forward_pipe4_r2_reg3 <= forward_pipe4_r2_reg;
			forward_wait5_r1_reg3 <= forward_wait5_r1_reg;
			forward_wait5_r2_reg3 <= forward_wait5_r2_reg;
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
		if(rst)
		begin
			cp0_read_data3 <= 32'b0;
			cp0_destination3 <= 8'b0;
		end
		else if((pipe2_outvalid && pipe3_allowin))
		begin
			cp0_read_data3 <= cp0_read_data2;
			cp0_destination3 <= cp0_destination2;
		end
		else
		begin
			cp0_read_data3 <= cp0_read_data3;
			cp0_destination3 <= cp0_destination3;
		end
	end

	////////////////////////////////////////////////////////
	//  pipe4
	reg [31:0] pipe4_data;
	reg [31:0] pipe4_PC;

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
		else if(irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3)
		begin
			pipe4_valid <= 1'b0;
		end
		else if(pipe4_allowin)
		begin
			pipe4_valid <= pipe3_outvalid;
		end
		else
		begin
			pipe4_valid <= pipe4_valid;
		end
		if(rst)
		begin
			pipe4_data <= 32'b0;
			pipe4_PC <= 32'b0;
			reg_write4 <= 1'b0;
			memtoreg4 <= 3'b0;
			regDst4 <= 1'b0;
			ALU_out4 <= 32'b0;
			Data4 <= 32'b0;
			HI_reg4 <= 32'b0;
			LO_reg4 <= 32'b0;
			cp0_read_data4 <=32'b0;
		end
		else if(pipe3_outvalid && pipe4_allowin)
		begin
			pipe4_data <=  pipe3_data;
			pipe4_PC <= pipe3_PC;
			reg_write4 <= reg_write3;
			memtoreg4 <= memtoreg3;
			regDst4 <= regDst3;
			ALU_out4 <= ALU_out3;
			Data4 <= Data;
			HI_reg4 <= HI;
			LO_reg4 <= LO;
			cp0_read_data4 <= cp0_read_data;
		end
		if(rst)
		begin
			Read_data_reg <= 32'b0;
		end
		else if(data_sram_data_ok)
		begin
			Read_data_reg <= data_sram_rdata;
		end
	end

	reg [31:0] Data4;

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

	assign op_add = (pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `ADDU ) |
					(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `ADD && pipe1_data[10:6] == 5'b0 ) |
					(pipe1_data[31:26] == `ADDIU) |
					(pipe1_data[31:26] == `ADDI) |
					(pipe1_data[31:26] == `LW   ) |
					(pipe1_data[31:26] == `LB   ) |
					(pipe1_data[31:26] == `LBU  ) | 
					(pipe1_data[31:26] == `LH   ) |
					(pipe1_data[31:26] == `LHU  ) |
					(pipe1_data[31:26] == `LWL  ) |
					(pipe1_data[31:26] == `LWR  ) |
					(pipe1_data[31:26] == `SW   ) |
					(pipe1_data[31:26] == `SB   ) |
					(pipe1_data[31:26] == `SH   ) |
					(pipe1_data[31:26] == `SWL  ) |
					(pipe1_data[31:26] == `SWR  ) |
					(pipe1_data[31:26] == `JAL  ) |
					(pipe1_data[31:26] == `J    ) |
					(pipe1_data[31:26] == 6'b0 && pipe1_data[20:16] == 5'b0 && pipe1_data[5:0] == `JALR ) |
					(pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZAL) |
					(pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZAL) |
					(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `JR   ) |
					1'd0;
	assign op_lui = ((pipe1_data[31:26] == `LUI  && pipe1_data[25:21] == 5'b0)) |
					1'd0;
	assign op_sub =	(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SUBU ) |
					(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SUB && pipe1_data[10:6] == 5'b0 ) |
					(pipe1_data[31:26] == `BEQ  ) |
					(pipe1_data[31:26] == `BNE  ) |
					1'd0;
	assign op_slt =	(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SLT  ) |
					(pipe1_data[31:26] == `SLTI  ) |
					1'd0;
	assign op_sltu =(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SLTU ) |
					(pipe1_data[31:26] == `SLTIU ) |
					1'd0;
	assign op_and =	(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `AND  ) |
					(pipe1_data[31:26] == `ANDI  ) |
					1'd0;
	assign op_or  =	(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `OR   ) |
					(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SYSCALL) |
					(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `BREAK) |
					(pipe1_data[31:26] == 6'b0 && pipe1_data[25:16] == 10'b0 && pipe1_data[10:6] == 5'b0 && pipe1_data[5:0] == `MFHI) |
					(pipe1_data[31:26] == 6'b0 && pipe1_data[25:16] == 10'b0 && pipe1_data[10:6] == 5'b0 && pipe1_data[5:0] == `MFLO) |
					(pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZ) |
					(pipe1_data[31:26] == `BLEZ && pipe1_data[20:16] == 5'b0) |
					(pipe1_data[31:26] == `BGTZ && pipe1_data[20:16] == 5'b0) |
					(pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZ) |
					(pipe1_data[31:26] == `ORI ) |
					1'd0;
	assign op_xor =	(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `XOR  ) |
					(pipe1_data[31:26] == `XORI ) |
					1'd0;
	assign op_nor =	(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `NOR  ) |
					1'd0;
	assign op_sll =	(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SLL && pipe1_data[25:21]==5'b0) |
					(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SLLV && pipe1_data[10:6] == 5'b0 ) |
					1'd0;
	assign op_srl =	(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SRL && pipe1_data[25:21]==5'b0) |
					(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SRLV && pipe1_data[10:6] == 5'b0 ) |
					1'd0;
	assign op_sra =	(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SRA && pipe1_data[25:21]==5'b0) |
					(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SRAV && pipe1_data[10:6] == 5'b0 ) |
					1'd0;
	
	assign irp_support = op_add || op_lui || op_sub || op_slt || op_sltu || op_and || op_or || op_xor || op_nor || op_sll || op_srl || op_sra || op_div || op_mul || cp0_wen || cp0_read || mthi || mtlo || eret || nop;
	reg irp_support2;
	reg irp_support3;

	wire nop;
	assign nop = pipe1_data[31:0] == 32'b0 ;
	
	//////sign_extend
	wire [31:0] sign_extend; 
	assign sign_extend = (pipe1_data[31:26]==6'b001100 || pipe1_data[31:26]==6'b001101 || pipe1_data[31:26]==6'b001110)?{16'b0,pipe1_data[15:0]}:
						 (pipe1_data[15])?{16'hffff,pipe1_data[15:0]}:{16'b0,pipe1_data[15:0]};

	//////Shift_left2
	wire [31:0] Shift_left2;
	assign Shift_left2= {sign_extend,2'b0};

	//////alu_B数据来源选择; 00为寄存器，10为sign_extend，11为Shift_left2
	wire [1:0] alu_B_mux;
	assign alu_B_mux =  /////////////////////////////////????????????????????????????????!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
						(pipe1_data[31:26] == `JAL || (pipe1_data[31:26] == 6'b0 && pipe1_data[20:16] == 5'b0 && pipe1_data[5:0] == `JALR) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZAL) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZAL)) ? 2'b01:
						(pipe1_data[31:26] == `LUI || pipe1_data[31:26] == `ADDIU  || pipe1_data[31:26] == `ADDI  || pipe1_data[31:26] == `LW || pipe1_data[31:26] == `LB || pipe1_data[31:26] == `LBU || pipe1_data[31:26] == `LH || pipe1_data[31:26] == `LHU || pipe1_data[31:26] == `LWL || pipe1_data[31:26] == `LWR || pipe1_data[31:26] == `SW || (pipe1_data[31:26] == `SB) || (pipe1_data[31:26] == `SH) || (pipe1_data[31:26] == `SWL) || (pipe1_data[31:26] == `SWR) || pipe1_data[31:26] == `SLTI || pipe1_data[31:26] == `SLTIU || pipe1_data[31:26] == `ANDI || pipe1_data[31:26] == `ORI || pipe1_data[31:26] == `XORI) ? 2'b10:
						(pipe1_data[31:26] == 6'b0 || pipe1_data[31:26] ==`BEQ || pipe1_data[31:26] == `BNE) ? 2'b00://() ? 2'b11:
						2'b00;
	wire [1:0]alu_A_mux;
	assign alu_A_mux = (pipe1_data[31:26] == `JAL || (pipe1_data[31:26] == 6'b0 && pipe1_data[20:16] == 5'b0 && pipe1_data[5:0] == `JALR) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZAL) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZAL)) ? 2'b01 : 
						((pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SLL && pipe1_data[25:21]==5'b0)|| (pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SRL && pipe1_data[25:21]==5'b0) || (pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SRA && pipe1_data[25:21]==5'b0)) ? 2'b11 :         //sll  sra   srl
						2'b00;

	//////数据旁路
	wire [1:0]forwarding;
	wire forward_pipe3;
	wire forward_pipe4;
	wire forward_wait5;

	assign forwarding = (!(op_mul_reg == 1'b1 && op_mul_last_reg ==1'b0)) && (forward_pipe3 || forward_pipe4 || forward_wait5);

	assign forward_pipe3 = forward_pipe3_r1 || forward_pipe3_r2;
	assign forward_pipe4 = forward_pipe4_r1 || forward_pipe4_r2;
	assign forward_wait5 = forward_wait5_r1 || forward_wait5_r2;

	wire forward_mfhi;
	wire forward_mflo;
	assign forward_mfhi = HI_wen && ((pipe1_data[31:26] == 6'b0) && (pipe1_data[25:16] == 10'b0) && (pipe1_data[10:6] == 5'b0) && (pipe1_data[25:16] == 10'b0) && (pipe1_data[5:0] == `MFHI));
	assign forward_mflo = LO_wen && ((pipe1_data[31:26] == 6'b0) && (pipe1_data[25:16] == 10'b0) && (pipe1_data[10:6] == 5'b0) && (pipe1_data[25:16] == 10'b0) && (pipe1_data[5:0] == `MFLO));

	wire forward_pipe3_r1;
	wire forward_pipe3_r2;
	assign forward_pipe3_r1 = ((raddr1 == waddr_dst2) && (raddr1 != 5'b0)) && ((pipe2_data[31:26] != `LW) && (pipe2_data[31:26] != `LB) && (pipe2_data[31:26] != `LBU) && (pipe2_data[31:26] != `LH) && (pipe2_data[31:26] != `LHU) && (pipe2_data[31:26] != `LWL) && (pipe2_data[31:26] != `LWR)) && reg_write2 ;
	assign forward_pipe3_r2 = ((raddr2 == waddr_dst2) && (raddr2 != 5'b0)) && ((pipe2_data[31:26] != `LW) && (pipe2_data[31:26] != `LB) && (pipe2_data[31:26] != `LBU) && (pipe2_data[31:26] != `LH) && (pipe2_data[31:26] != `LHU) && (pipe2_data[31:26] != `LWL) && (pipe2_data[31:26] != `LWR)) && (cp0_wen || (pipe1_data[31:26] == 6'b0) || (pipe1_data[31:26] == `BEQ) || (pipe1_data[31:26] == `BNE) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZ) || (pipe1_data[31:26] == `BLEZ && pipe1_data[20:16] == 5'b0) || (pipe1_data[31:26] == `BGTZ && pipe1_data[20:16] == 5'b0) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZ) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZAL) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZAL) || (pipe1_data[31:26] == `SW) || (pipe1_data[31:26] == `SB) || (pipe1_data[31:26] == `SH) || (pipe1_data[31:26] == `SWL) || (pipe1_data[31:26] == `SWR)) && reg_write2 ;

	wire forward_pipe4_r1;
	wire forward_pipe4_r2;
	assign forward_pipe4_r1 = ((raddr1 == waddr_dst3) && (raddr1 != 5'b0)) && reg_write3;
	assign forward_pipe4_r2 = ((raddr2 == waddr_dst3) && (raddr2 != 5'b0)) && (cp0_wen || (pipe1_data[31:26] == 6'b0) || (pipe1_data[31:26] == `BEQ) || (pipe1_data[31:26] == `BNE) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZ) || (pipe1_data[31:26] == `BLEZ && pipe1_data[20:16] == 5'b0) || (pipe1_data[31:26] == `BGTZ && pipe1_data[20:16] == 5'b0) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZ) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZAL) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZAL) || (pipe1_data[31:26] == `SW) || (pipe1_data[31:26] == `SB) || (pipe1_data[31:26] == `SH) || (pipe1_data[31:26] == `SWL) || (pipe1_data[31:26] == `SWR)) && reg_write3;

	wire forward_wait5_r1;
	wire forward_wait5_r2;
	assign forward_wait5_r1 = ((raddr1 == waddr_dst4) && (raddr1 != 5'b0)) && reg_write4;
	assign forward_wait5_r2 = ((raddr2 == waddr_dst4) && (raddr2 != 5'b0)) && (cp0_wen || (pipe1_data[31:26] == 6'b0) || (pipe1_data[31:26] == `BEQ) || (pipe1_data[31:26] == `BNE) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZ) || (pipe1_data[31:26] == `BLEZ && pipe1_data[20:16] == 5'b0) || (pipe1_data[31:26] == `BGTZ && pipe1_data[20:16] == 5'b0) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZ) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZAL) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZAL) || (pipe1_data[31:26] == `SW) || (pipe1_data[31:26] == `SB) || (pipe1_data[31:26] == `SH) || (pipe1_data[31:26] == `SWL) || (pipe1_data[31:26] == `SWR)) && reg_write4;

	wire [31:0] rdata1_input;          ///////////////////////??????????????????????????!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	wire [31:0] rdata2_input;

	reg forward_mfhi_reg;
	reg forward_mflo_reg;
	reg forward_pipe3_r1_reg;
	reg forward_pipe3_r2_reg;
	reg forward_pipe4_r1_reg;
	reg forward_pipe4_r2_reg;
	reg forward_wait5_r1_reg;
	reg forward_wait5_r2_reg;

	reg forward_mfhi_reg3;
	reg forward_mflo_reg3;
	reg forward_pipe3_r1_reg3;
	reg forward_pipe3_r2_reg3;
	reg forward_pipe4_r1_reg3;
	reg forward_pipe4_r2_reg3;
	reg forward_wait5_r1_reg3;
	reg forward_wait5_r2_reg3;

	assign rdata1_input = /*(forward_pipe3_r1_reg ) ? choose_data3 :
						  (forward_pipe4_r1_reg ) ? wdata :
						  (forward_wait5_r1_reg ) ? ALU_out5 :*/
						  rdata1;
	assign rdata2_input = /*(forward_pipe3_r2_reg ) ? choose_data3 :
						  (forward_pipe4_r2_reg ) ? wdata :
						  (forward_wait5_r2_reg ) ? ALU_out5 :*/
						  rdata2;

	wire[31:0] choose_data3;
	assign choose_data3  = 		(memtoreg3 == 3'b001) ? ALU_out3 :
								(memtoreg3 == 3'b000) ? ALU_out3 :
								(memtoreg3 == 3'b010) ? HI :
								(memtoreg3 == 3'b011) ? LO :
								/*(memtoreg3 == 3'd4) ?*/ cp0_read_data;
	
	
	reg [31:0] ALU_out5;
	always@(posedge clk)
	begin
		if(rst)
		begin
			ALU_out5 <= 32'b0;
		end
		else if(pipe4_allowin)
		begin
			ALU_out5 <= wdata_history;   //////////
		end
	end

	reg[31:0] wdata_history;
	always@(posedge clk)
	begin
		if(rst)
		begin
			wdata_history <= 32'b0;
		end
		else
		begin
			wdata_history <= wdata;   //////////
		end
	end

	reg [1:0]counter_forwarding;
	always@(posedge clk)
	begin
		if(rst || ((forwarding == 2'd0) && pipe2_valid))
		begin
			counter_forwarding <= 2'b0;
		end
		else if(forwarding)
		begin
			counter_forwarding <= 2'b1;
		end
		else 
		begin
			counter_forwarding <= counter_forwarding;
		end
	end

	wire lw_choke;
	wire lw_choke_r1;
	wire lw_choke_r2;
	reg lw_choke_r1_reg;
	reg lw_choke_r2_reg;
	assign lw_choke = lw_choke_r1 || lw_choke_r2;
	assign lw_choke_r1 = ((raddr1 == waddr_dst2) && (raddr1 != 5'b0)) && (pipe2_data[31:26] == `LW || pipe2_data[31:26] == `LB || pipe2_data[31:26] == `LBU || pipe2_data[31:26] == `LH || pipe2_data[31:26] == `LHU || pipe2_data[31:26] == `LWL || pipe2_data[31:26] == `LWR) && reg_write2;
	assign lw_choke_r2 = ((raddr2 == waddr_dst2) && (raddr2 != 5'b0)) && (pipe2_data[31:26] == `LW || pipe2_data[31:26] == `LB || pipe2_data[31:26] == `LBU || pipe2_data[31:26] == `LH || pipe2_data[31:26] == `LHU || pipe2_data[31:26] == `LWL || pipe2_data[31:26] == `LWR) && (pipe1_data[31:26] == 6'b0 || (pipe1_data[31:26] == `BEQ) || (pipe1_data[31:26] == `BNE) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZ) || (pipe1_data[31:26] == `BLEZ && pipe1_data[20:16] == 5'b0) || (pipe1_data[31:26] == `BGTZ && pipe1_data[20:16] == 5'b0) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZ) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZAL) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZAL) || pipe1_data[31:26] == `SW || pipe1_data[31:26] == `SB || pipe1_data[31:26] == `SH || pipe1_data[31:26] == `SWL || pipe1_data[31:26] == `SWR) && reg_write2;

	reg [1:0]counter_lw_choke;

    always@(posedge clk)
	begin
		if(rst )
		begin
			counter_lw_choke <= 2'b0;
		end
        else if(!pipe3_readyout)
		begin
			counter_lw_choke <= counter_lw_choke;
		end
		else if(lw_choke)
		begin
			counter_lw_choke <= counter_lw_choke + 2'b1;
		end
		else if(counter_lw_choke == 2'b0)
		begin
			counter_lw_choke <= 2'b0;
		end
        else
        begin
            counter_lw_choke <= counter_lw_choke + 2'b1;
        end
	end
	
	//cp0-related
	wire cp0_read;
	wire cp0_wen;
	wire [7:0]cp0_destination;
	reg  [7:0]cp0_destination2;
	reg  [7:0]cp0_destination3;
	wire [31:0]cp0_read_data;
	reg	 [31:0]cp0_read_data2;
	reg	 [31:0]cp0_read_data3;
	reg	 [31:0]cp0_read_data4;
	wire [31:0]cp0_write_data;

	wire cp0_wen_epc;
	wire cp0_wen_status;
	wire cp0_wen_compare;
	wire cp0_wen_count;
	reg	cp0_wen_epc2;
	reg cp0_wen_status2;
	reg cp0_wen_cause2;
	reg cp0_wen_compare2;
	reg cp0_wen_count2;

	assign cp0_read = (pipe1_data[31:26] == 6'b010000) && (pipe1_data[25:21] == `MFC0) && (pipe1_data[10:3] == 8'b0);
	assign cp0_wen = (pipe1_data[31:26] == 6'b010000) && (pipe1_data[25:21] == `MTC0) && (pipe1_data[10:3] == 8'b0);

	assign cp0_destination = {pipe1_data[15:11],pipe1_data[2:0]};
	assign cp0_read_data = 	(cp0_destination3 == 8'b01110000) ? cp0_epc :
							(cp0_destination3 == 8'b01101000) ? cp0_cause :
							(cp0_destination3 == 8'b01100000) ? cp0_status:
							(cp0_destination3 == 8'b01011000) ? cp0_compare:
							(cp0_destination3 == 8'b01001000) ? cp0_count:
							(cp0_destination3 == 8'b01000000) ? cp0_badvaddr:
							32'b0;
	assign cp0_wen_epc = cp0_wen && (pipe1_data[15:11] == 8'b01110);
	assign cp0_wen_status = cp0_wen && (pipe1_data[15:11] == 8'b01100);
	assign cp0_wen_cause = cp0_wen && (pipe1_data[15:11] == 8'b01101);
	assign cp0_wen_compare = cp0_wen && (pipe1_data[15:11] == 8'b01011);
	assign cp0_wen_count = cp0_wen && (pipe1_data[15:11] == 8'b01001);
	assign cp0_write_data = alu_B_input;//(forward_pipe3_r2_reg) ? rdata2_input : rdata2_reg;

	// interupt-related
	wire syscall_info;
	assign syscall_info =  (pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `SYSCALL) ? 1'b1 :  1'b0;

	wire syscall_in_slot;
	reg syscall_in_slot2;
	reg syscall_in_slot3;
	assign syscall_in_slot = syscall_info && slot_or_not_reg;

	wire break_info;
	reg break_info2;
	reg break_info3;
	assign break_info = (pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `BREAK) ? 1'b1 :  1'b0;

	wire irp_overflow;
	reg irp_overflow3;
	assign irp_overflow = ((pipe2_data[31:26] == 6'b0 && pipe2_data[10:6] == 5'b0 && pipe2_data[5:0] == `ADD) || 
							(pipe2_data[31:26] == `ADDI) ||
							(pipe2_data[31:26] == 6'b0 && pipe2_data[10:6] == 5'b0 && pipe2_data[5:0] == `SUB))
							? overflow : 1'b0 ;

	wire irp_time;
	reg irp_time2;
	reg irp_time3;
	reg irp_time_reg;
	assign irp_time = (cp0_count == cp0_compare) && (cp0_compare != 32'b0) && cp0_status_ie && cp0_status_im[7];
	always @(posedge clk)
	begin
		if(rst)
		begin
			irp_time_reg <= 1'b0;
		end
		else if(irp_time && (!pipe2_valid))
		begin
			irp_time_reg <= 1'b1;
		end
		else if((irp_time_reg == 1'b1) && pipe2_valid)
		begin
			irp_time_reg <= 1'b0;
		end
	end

	wire irp_inst;
	reg	 irp_inst1;
	reg	 irp_inst2;
	reg	 irp_inst3;
	assign irp_inst = (PC[1:0] != 2'b0) ? 1'b1 : 1'b0;

	wire irp_address_lw;
	wire irp_address_lw_lw;
	wire irp_address_lw_lh;
	wire irp_address_sw;
	wire irp_address_sw_sw;
	wire irp_address_sw_sh;
	reg  irp_address_lw3;
	reg  irp_address_sw3;
	assign irp_address_lw_lw = (ALU_out[1:0] != 2'b0) && (pipe2_data[31:26] == `LW);
	assign irp_address_lw_lh = (ALU_out[0] != 1'b0) &&  (pipe2_data[31:26] == `LH || pipe2_data[31:26] == `LHU);
	assign irp_address_lw  = irp_address_lw_lw || irp_address_lw_lh;
	assign irp_address_sw_sw = (ALU_out[1:0] != 2'b0) && (pipe2_data[31:26] == `SW);
	assign irp_address_sw_sh = (ALU_out[0] != 1'b0) && (pipe2_data[31:26] == `SH);
	assign irp_address_sw = irp_address_sw_sw || irp_address_sw_sh;

	//irp_soft;
	wire[7:0] 	 irp_ip;
	reg[7:0]	 irp_ip2;
	reg[7:0]	 irp_ip3;
	reg 	irp_ip0_2;
	reg 	irp_ip0_3;
	reg 	irp_ip1_2;
	reg 	irp_ip1_3;
	reg 	irp_ip2_2;
	reg 	irp_ip2_3;
	reg 	irp_ip3_2;
	reg 	irp_ip3_3;
	reg 	irp_ip4_2;
	reg 	irp_ip4_3;
	reg 	irp_ip5_2;
	reg 	irp_ip5_3;
	reg 	irp_ip6_2;
	reg 	irp_ip6_3;
	reg 	irp_ip7_2;
	reg 	irp_ip7_3;
	reg	 irp_ip0_reg;
	reg	 irp_ip1_reg;
	reg	 irp_ip2_reg;
	reg	 irp_ip3_reg;
	reg	 irp_ip4_reg;
	reg	 irp_ip5_reg;
	reg	 irp_ip6_reg;
	reg	 irp_ip7_reg;
	assign irp_ip = {cp0_cause_ip7,cp0_cause_ip6,cp0_cause_ip5,cp0_cause_ip4,cp0_cause_ip3,cp0_cause_ip2,cp0_cause_ip1,cp0_cause_ip0} & cp0_status_im & {8{!cp0_status_exl}} & {8{cp0_status_ie}};

	always @(posedge clk)
	begin
		//0
		if(rst)
		begin
			irp_ip0_reg <= 1'b0;
		end
		else if(irp_ip[0] && !pipe2_valid && !irp_ip0_3)
		begin
			irp_ip0_reg <= 1'b1;
		end
		else if((irp_ip0_reg == 1'b1) && pipe2_valid)
		begin
			irp_ip0_reg <= 1'b0;
		end
		//1
		if(rst)
		begin
			irp_ip1_reg <= 1'b0;
		end
		else if(irp_ip[1] && !pipe2_valid && !irp_ip1_3)
		begin
			irp_ip1_reg <= 1'b1;
		end
		else if((irp_ip1_reg == 1'b1) && pipe2_valid)
		begin
			irp_ip1_reg <= 1'b0;
		end
		//2
		if(rst)
		begin
			irp_ip2_reg <= 1'b0;
		end
		else if(irp_ip[2] && !pipe2_valid)
		begin
			irp_ip2_reg <= 1'b1;
		end
		else if((irp_ip2_reg == 1'b1) && pipe2_valid)
		begin
			irp_ip2_reg <= 1'b0;
		end
		//3
		if(rst)
		begin
			irp_ip3_reg <= 1'b0;
		end
		else if(irp_ip[3] && !pipe2_valid)
		begin
			irp_ip3_reg <= 1'b1;
		end
		else if((irp_ip3_reg == 1'b1) && pipe2_valid)
		begin
			irp_ip3_reg <= 1'b0;
		end
		//4
		if(rst)
		begin
			irp_ip4_reg <= 1'b0;
		end
		else if(irp_ip[4] && !pipe2_valid)
		begin
			irp_ip4_reg <= 1'b1;
		end
		else if((irp_ip4_reg == 1'b1) && pipe2_valid)
		begin
			irp_ip4_reg <= 1'b0;
		end
		//5
		if(rst)
		begin
			irp_ip5_reg <= 1'b0;
		end
		else if(irp_ip[5] && !pipe2_valid)
		begin
			irp_ip5_reg <= 1'b1;
		end
		else if((irp_ip5_reg == 1'b1) && pipe2_valid)
		begin
			irp_ip5_reg <= 1'b0;
		end
		//6
		if(rst)
		begin
			irp_ip6_reg <= 1'b0;
		end
		else if(irp_ip[6] && !pipe2_valid)
		begin
			irp_ip6_reg <= 1'b1;
		end
		else if((irp_ip6_reg == 1'b1) && pipe2_valid)
		begin
			irp_ip6_reg <= 1'b0;
		end
		//7
		if(rst)
		begin
			irp_ip7_reg <= 1'b0;
		end
		else if(irp_ip[7] && !pipe2_valid)
		begin
			irp_ip7_reg <= 1'b1;
		end
		else if((irp_ip7_reg == 1'b1) && pipe2_valid)
		begin
			irp_ip7_reg <= 1'b0;
		end
	end


	wire eret;
	assign eret= (pipe1_data[31:26] == 6'b010000) && (pipe1_data[25] == 1'b1) && (pipe1_data[24:6] == 19'b0) &&(pipe1_data[5:0] == `ERET);

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

	assign alu_A = 	({32{alu_A_mux_reg == 2'b01}} & pipe1_PC ) 					| 
					({32{alu_A_mux_reg == 2'b11}} & {27'b0,pipe2_data[10:6]}) 	| 
					({32{alu_A_mux_reg == 2'b00}} & alu_A_input);
	assign alu_B =  (alu_B_mux_reg == 2'b00) ? alu_B_input		:
					(alu_B_mux_reg == 2'b10) ? sign_extend_reg	:
					(alu_B_mux_reg == 2'b11) ? Shift_left2_reg	:
					32'd4;  

	wire [31:0]alu_A_input;
	wire [31:0]alu_B_input;
	assign alu_A_input =  (lw_choke_r1_reg 		) ? rdata1_reg 		:
						  (forward_pipe3_r1_reg ) ? choose_data3 	:
						  (forward_pipe4_r1_reg ) ? wdata_input 	:
						  (forward_wait5_r1_reg ) ? rdata1_reg 		:
						  rdata1_reg;

	assign alu_B_input =  (lw_choke_r2_reg 		) ? rdata2_reg 		:
						  (forward_pipe3_r2_reg ) ? choose_data3 	:
						  (forward_pipe4_r2_reg ) ? wdata_input 	:
						  (forward_wait5_r2_reg ) ? rdata2_reg 		:
						  rdata2_reg;

	assign ALU_out = alu_result;

	wire[31:0] Data_input;
	assign Data_input = forward_pipe3_r2_reg ? ALU_out3 :
						forward_pipe4_r2_reg ? ALU_out4 :
						forward_wait5_r2_reg ? ALU_out5 :
						rdata2_reg;

	////////////////////////////////////////////////////////
	// PHASE 4
	assign MemRead = memRead3 && pipe3_valid && !irp_address_lw3;
	assign MemWrite = memWrite3 && pipe3_valid && !irp_address_sw3;

	wire memRead;
	reg memRead2;
	reg memRead3;
	assign memRead = (pipe1_data[31:26] == `LW || pipe1_data[31:26] == `LB || pipe1_data[31:26] == `LBU || pipe1_data[31:26] == `LH || pipe1_data[31:26] == `LHU || pipe1_data[31:26] == `LWL || pipe1_data[31:26] == `LWR) ? 1'b1 : 1'b0;
	
	wire memWrite;
	reg memWrite2;
	reg memWrite3;
	assign memWrite = (pipe1_data[31:26] == `SW || (pipe1_data[31:26] == `SB) || (pipe1_data[31:26] == `SH) || (pipe1_data[31:26] == `SWL) || (pipe1_data[31:26] == `SWR)) ? 1'b1 : 1'b0;

	assign Address = ALU_out3;

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
	
	assign raddr1 = pipe1_data[25:21]; ///////////////
	assign raddr2 = pipe1_data[20:16]; ////////////////

	reg [31:0] rdata1_reg;
	reg [31:0] rdata2_reg;

	assign wdata =  ({32{memtoreg4 == 3'b1}} 	& Read_data_result)    	|
					({32{memtoreg4 == 3'b0}} 	& ALU_out4)            	|
					({32{memtoreg4 == 3'b10}} 	& HI_reg4)            	|
					({32{memtoreg4 == 3'b11}} 	& LO_reg4)            	|
					({32{memtoreg4 == 3'd4}} 	& cp0_read_data4);/////////////??????????????????????????????????

	wire [31:0] wdata_input;
	assign wdata_input = pipe3_valid ? wdata : wdata_reg;
	reg [31:0] wdata_reg;
	always @(posedge clk)
	begin
		if(rst)
		begin 
			wdata_reg <= 32'b0;	
		end
		else if(pipe3_outvalid && pipe4_allowin)
		begin
			wdata_reg <= wdata;
		end
	end



	reg reg_write2;
	reg reg_write3;
	reg reg_write4;
	wire reg_write;
	assign reg_write = ( ((pipe1_data[31:26] == 6'b010000) && (pipe1_data[25] == 1'b1) && (pipe1_data[24:6] == 19'b0) &&(pipe1_data[5:0] == `ERET)) || syscall_info || cp0_wen || pipe1_data[31:26] == `J || (pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `JR) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZ) || (pipe1_data[31:26] == `BLEZ && pipe1_data[20:16] == 5'b0) || (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZ)|| (pipe1_data[31:26] == `BGTZ && pipe1_data[20:16] == 5'b0) || pipe1_data[31:26] ==`BEQ || pipe1_data[31:26] == `BNE || pipe1_data[31:26] == `SW || (pipe1_data[31:26] == `SB) || (pipe1_data[31:26] == `SH) || (pipe1_data[31:26] == `SWL) || (pipe1_data[31:26] == `SWR) || ((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `MULT) && (pipe1_data[15:6]==10'b0)) || ((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `MULTU) && (pipe1_data[15:6]==10'b0))|| ((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `DIV) && (pipe1_data[15:6]==10'b0)) || ((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `DIVU) && (pipe1_data[15:6]==10'b0))|| mthi || mtlo) ? 1'b0:
						1'b1;

	assign wen = reg_write4 && pipe4_readyout && pipe4_valid;//////////////?????????????????????????????????????

	reg regDst2;
	reg regDst3;
	reg regDst4;
	wire regDst;
	assign regDst = (cp0_wen || cp0_read|| (pipe1_data[31:26] == 6'b0 && pipe1_data[20:16] == 5'b0 && pipe1_data[5:0] == `JALR ) || pipe1_data[31:26] == `LUI || pipe1_data[31:26] == `ADDIU || pipe1_data[31:26] == `ADDI || pipe1_data[31:26] == `LW || pipe1_data[31:26] == `LB || pipe1_data[31:26] == `LBU || pipe1_data[31:26] == `LH || pipe1_data[31:26] == `LHU || pipe1_data[31:26] == `LWL || pipe1_data[31:26] == `LWR || pipe1_data[31:26] == `SW || (pipe1_data[31:26] == `SB) || (pipe1_data[31:26] == `SH) || (pipe1_data[31:26] == `SWL) || (pipe1_data[31:26] == `SWR) || pipe1_data[31:26] == `JAL || pipe1_data[31:26] == `SLTI || pipe1_data[31:26] == `SLTIU || pipe1_data[31:26] == `ANDI || pipe1_data[31:26] == `ORI || pipe1_data[31:26] == `XORI) ? 1'b0 : 1'b1;

	//assign waddr = (pipe4_data[31:26] == `JAL)?5'd31://JAL&JALR
	//			 	waddr_dst4;
	assign waddr = waddr_dst4;

	wire [4:0]waddr_dst4;
	assign waddr_dst4 = (pipe4_data[31:26] == `JAL || (pipe4_data[31:26] == 6'b0 && pipe4_data[20:16] == 5'b0 && pipe4_data[5:0] == `JALR) || (pipe4_data[31:26] == `REGIMM && pipe4_data[20:16] == `BLTZAL) || (pipe4_data[31:26] == `REGIMM && pipe4_data[20:16] == `BGEZAL)) ? 5'd31 :
						(regDst4)? pipe4_data[15:11] : pipe4_data[20:16];

	wire [4:0]waddr_dst3;
	assign waddr_dst3 = (pipe3_data[31:26] == `JAL || (pipe3_data[31:26] == 6'b0 && pipe3_data[20:16] == 5'b0 && pipe3_data[5:0] == `JALR) || (pipe3_data[31:26] == `REGIMM && pipe3_data[20:16] == `BLTZAL) || (pipe3_data[31:26] == `REGIMM && pipe3_data[20:16] == `BGEZAL)) ? 5'd31 :
						(regDst3)? pipe3_data[15:11] : pipe3_data[20:16];

	wire [4:0]waddr_dst2;
	assign waddr_dst2 = (pipe2_data[31:26] == `JAL || (pipe2_data[31:26] == 6'b0 && pipe2_data[20:16] == 5'b0 && pipe2_data[5:0] == `JALR) || (pipe2_data[31:26] == `REGIMM && pipe2_data[20:16] == `BLTZAL) || (pipe2_data[31:26] == `REGIMM && pipe2_data[20:16] == `BGEZAL)) ? 5'd31 :
						(regDst2)? pipe2_data[15:11] : pipe2_data[20:16];

	reg[2:0] memtoreg2;
	reg[2:0] memtoreg3;
	reg[2:0] memtoreg4;
	wire[2:0] memtoreg;
	assign memtoreg = 	
						(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `MFHI && pipe1_data[25:16]==10'b0 && pipe1_data[10:6]==5'b0) ? 3'b010 :
						(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `MFLO && pipe1_data[25:16]==10'b0 && pipe1_data[10:6]==5'b0) ? 3'b011 :
						(pipe1_data[31:26] == `LW || pipe1_data[31:26] == `LB || pipe1_data[31:26] == `LBU || pipe1_data[31:26] == `LH || pipe1_data[31:26] == `LHU || pipe1_data[31:26] == `LWL || pipe1_data[31:26] == `LWR) ? 3'b001 : 
						((pipe1_data[31:26] == 6'b010000) && (pipe1_data[25:21] == `MFC0)) ? 3'd4:3'b000;



	//PC
	assign PC = PC_reg;
	reg [31:0] PC_reg;

	always @(posedge clk)
	begin
		if(rst)
		begin 
			PC_reg <= 32'hbfc00000;	
		end
		else if((irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3 || irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3)&& pipe3_valid)
		begin
			PC_reg <= 32'hbfc00380;
		end
		else if (((pipe1_data[31:26] == 6'b010000) && (pipe1_data[25] == 1'b1) && (pipe1_data[24:6] == 19'b0) &&(pipe1_data[5:0] == `ERET)) && pipe2_valid  && !counter_lw_choke)
		begin
			PC_reg <= cp0_epc;
		end
		else if (pipe1_allowin)
		begin
			if(branch_or_not && pipe1_valid)
			begin
				PC_reg <= PC_branchafter;
			end
			else if(jump_or_not && pipe1_valid)
			begin
				PC_reg <= jump_target;
			end
			else if(eret)
			begin
				PC_reg <= PC_reg;
			end
			else
			begin
				PC_reg <= PC_reg + 32'd4;
			end
		end
	end

    reg[1:0] irp_counter;
    always@(posedge clk)
	begin
		if(rst )
		begin
			irp_counter <= 2'b01;
		end
		else if((irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3)&& pipe3_valid)
		begin
			irp_counter <= 2'b11;
		end
        else if(((pipe1_data[31:26] == 6'b010000) && (pipe1_data[25] == 1'b1) && (pipe1_data[24:6] == 19'b0) &&(pipe1_data[5:0] == `ERET)) && pipe2_valid  && !counter_lw_choke)
        begin
			irp_counter <= 2'b11;
		end
		else if((irp_counter == 2'b11) && inst_sram_data_ok)
		begin
			irp_counter <= 2'b10;
		end
        else if((irp_counter == 2'b10) && inst_sram_data_ok)
		begin
			irp_counter <= 2'b01;
		end
	end

	wire [31:0]branch_A_input;
	wire [31:0]branch_B_input;
	assign branch_A_input = (lw_choke_r1) 		? rdata1		:
						  	(forward_pipe3_r1 ) ? choose_data3 	:
						  	(forward_pipe4_r1 ) ? wdata 		:
						  	(forward_wait5_r1 ) ? ALU_out5 		:
						  	rdata1;

	assign branch_B_input = (lw_choke_r2 ) 		? rdata2 		:
						  	(forward_pipe3_r2 ) ? choose_data3 	:
						  	(forward_pipe4_r2 ) ? wdata 		:
						  	(forward_wait5_r2 ) ? ALU_out5 		:
						 	rdata2;

	assign branch_choke_must = (lw_choke_r1 || lw_choke_r2) && branch_op && pipe3_valid;

	wire [31:0] PC_branchafter;
	assign PC_branchafter = pipe1_PC + Shift_left2 + 32'd4;
	wire branch_or_not;
	wire eq_or_not;
	assign beq = (pipe1_data[31:26] == `BEQ );
	assign bne = (pipe1_data[31:26] == `BNE );
	assign bgez = (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZ);
	assign blez = (pipe1_data[31:26] == `BLEZ && pipe1_data[20:16] == 5'b0);
	assign bgtz = (pipe1_data[31:26] == `BGTZ && pipe1_data[20:16] == 5'b0);
	assign bltz = (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZ);
	assign bltzal = (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BLTZAL);
	assign bgezal = (pipe1_data[31:26] == `REGIMM && pipe1_data[20:16] == `BGEZAL);
	assign eq_or_not = (branch_A_input == branch_B_input); 
	assign neg_or_not = branch_A_input[31];
	assign pos_or_not = ((branch_A_input[31] == 1'b1) || (branch_A_input == 32'b0));
	assign branch_or_not= 	(beq & eq_or_not) ||
							(bne & !eq_or_not)||
							(bgez & !neg_or_not) ||
							(bgtz & !pos_or_not) ||
							(blez & pos_or_not) ||
							(bltz & neg_or_not) ||
							(bltzal & neg_or_not) ||
							(bgezal & !neg_or_not);
	wire branch_op;
	assign branch_op = beq || bne || bgez || bgtz || blez || bltz || bltzal || bgezal;
	wire branch_choke = branch_op && forwarding;

	wire jump_or_not;
	wire jal;
	wire jr;
	wire j;
	wire jalr;
	assign jal 	= 	(pipe1_data[31:26] == `JAL );
	assign jr 	= 	(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `JR );
	assign j 	= 	(pipe1_data[31:26] == `J );
	assign jalr = 	(pipe1_data[31:26] == 6'b0 && pipe1_data[20:16] == 5'b0 && pipe1_data[5:0] == `JALR );
	assign jump_or_not = jal || jr || j || jalr;
	
	wire[31:0] jump_target;
	assign jump_target = 	({32{jal || j }} & {pipe1_PC[31:28],pipe1_data[25:0],2'b0}) |
							({32{jr || jalr}} & branch_A_input) ;
	
	wire slot_or_not_reg;
	reg slot_or_not_reg2;
	reg slot_or_not_reg3;
	assign slot_or_not_reg = branch_op_reg;

	reg branch_op_reg;

	//////////////
	wire [3:0]sb_wen;
	wire [3:0]sh_wen;
	wire [3:0]swl_wen;
	wire [3:0]swr_wen;
	assign mem_sw = (pipe3_data[31:26] == `SW) ? 1'b1 : 1'b0;

	assign mem_sb = (pipe3_data[31:26] == `SB) ? 1'b1 : 1'b0;
	assign sb_wen =	({4{ALU_out3[1:0] == 2'b00}} & 4'b0001) |
					({4{ALU_out3[1:0] == 2'b01}} & 4'b0010) |
					({4{ALU_out3[1:0] == 2'b10}} & 4'b0100) |
					({4{ALU_out3[1:0] == 2'b11}} & 4'b1000) |
					4'b0;

	assign mem_sh = (pipe3_data[31:26] == `SH) ? 1'b1 : 1'b0;
	assign sh_wen = ({4{ALU_out3[1] == 1'b0}} & 4'b0011) |
					({4{ALU_out3[1] == 1'b1}} & 4'b1100) |
					4'b0;

	assign mem_swr = (pipe3_data[31:26] == `SWR) ? 1'b1 : 1'b0;
	assign swr_wen =({4{ALU_out3[1:0] == 2'b00}} & 4'b1111) |
					({4{ALU_out3[1:0] == 2'b01}} & 4'b1110) |
					({4{ALU_out3[1:0] == 2'b10}} & 4'b1100) |
					({4{ALU_out3[1:0] == 2'b11}} & 4'b1000) |
					4'b0;				

	assign mem_swl = (pipe3_data[31:26] == `SWL) ? 1'b1 : 1'b0;
	assign swl_wen =({4{ALU_out3[1:0] == 2'b00}} & 4'b0001) |
					({4{ALU_out3[1:0] == 2'b01}} & 4'b0011) |
					({4{ALU_out3[1:0] == 2'b10}} & 4'b0111) |
					({4{ALU_out3[1:0] == 2'b11}} & 4'b1111) |
					4'b0;	
	
	assign Write_strb = ({4{mem_sb}} & sb_wen) |
						({4{mem_sh}} & sh_wen) |
						({4{mem_swr}} & swr_wen) |
						({4{mem_swl}} & swl_wen) |
						({4{mem_sw}} & 4'b1111) |
						4'b0000;

wire[31:0] swl_data;
wire[31:0] swr_data;
wire[31:0] sb_data;
wire[31:0] sh_data;

assign swl_data = 	({32{ALU_out3[1:0] == 2'b00}} & {24'b0,Data[31:24]}) 	|
					({32{ALU_out3[1:0] == 2'b01}} & {16'b0,Data[31:16]}) 	|
					({32{ALU_out3[1:0] == 2'b10}} & {8'b0,Data[31:8]})		|
					({32{ALU_out3[1:0] == 2'b11}} & Data);

assign swr_data =	({32{ALU_out3[1:0] == 2'b00}} & Data) |
					({32{ALU_out3[1:0] == 2'b01}} & {Data[23:0],8'b0}) 	|
					({32{ALU_out3[1:0] == 2'b10}} & {Data[15:0],16'b0}) |
					({32{ALU_out3[1:0] == 2'b11}} & {Data[7:0],24'b0});	

assign sb_data = 	({32{ALU_out3[1:0] == 2'b00}} & Data) |
					({32{ALU_out3[1:0] == 2'b01}} & {Data[23:0],8'b0}) 	|
					({32{ALU_out3[1:0] == 2'b10}} & {Data[15:0],16'b0}) |
					({32{ALU_out3[1:0] == 2'b11}} & {Data[7:0],24'b0}) ;

assign sh_data =    ({32{ALU_out3[1] == 1'b0}} & Data) |
					({32{ALU_out3[1] == 1'b1}} & {Data[15:0],16'b0});		

assign Write_data =		({32{mem_sb}} & sb_data) 	|
						({32{mem_sh}} & sh_data) 	|
						({32{mem_swr}} & swr_data) 	|
						({32{mem_swl}} & swl_data) 	|
						({32{mem_sw}} & Data) 		|
						32'b0;
	

	wire [31:0] Read_data_extend_b;
	wire [31:0] Read_data_extend_b1;
	wire [31:0] Read_data_extend_b2;
	wire [31:0] Read_data_extend_b3;
	wire [31:0] Read_data_extend_b4;
	wire [31:0] Read_data_extend_bu;
	wire [31:0] Read_data_extend_h;
	wire [31:0] Read_data_extend_hu;
	wire [31:0] Read_data_extend_h1;
	wire [31:0] Read_data_extend_h2;
	wire [31:0] Read_data_extend_lwl;
	wire [31:0] Read_data_extend_lwr;

	
	assign Read_data_extend_b1 = (Read_data[7])?{24'hffffff,Read_data[7:0]} : {24'h0,Read_data[7:0]};
	assign Read_data_extend_b2 = (Read_data[15])?{24'hffffff,Read_data[15:8]} : {24'h0,Read_data[15:8]};
	assign Read_data_extend_b3 = (Read_data[23])?{24'hffffff,Read_data[23:16]} : {24'h0,Read_data[23:16]};
	assign Read_data_extend_b4 = (Read_data[31])?{24'hffffff,Read_data[31:24]} : {24'h0,Read_data[31:24]};

	assign Read_data_extend_b = ({32{ALU_out4[1:0] == 2'b00}} &  Read_data_extend_b1) |
								({32{ALU_out4[1:0] == 2'b01}} &  Read_data_extend_b2) |
								({32{ALU_out4[1:0] == 2'b10}} &  Read_data_extend_b3) |
								({32{ALU_out4[1:0] == 2'b11}} &  Read_data_extend_b4) |
								32'b0;

	assign Read_data_extend_bu = ({32{ALU_out4[1:0] == 2'b00}} &  {24'h0,Read_data[7:0]}) |
								({32{ALU_out4[1:0] == 2'b01}} &  {24'h0,Read_data[15:8]}) |
								({32{ALU_out4[1:0] == 2'b10}} &  {24'h0,Read_data[23:16]}) |
								({32{ALU_out4[1:0] == 2'b11}} &  {24'h0,Read_data[31:24]}) |
								32'b0;
	

	assign Read_data_extend_h = (ALU_out4[1])?Read_data_extend_h1:Read_data_extend_h2;
	assign Read_data_extend_hu = (ALU_out4[1])?{16'h0,Read_data[31:16]}: {16'h0,Read_data[15:0]};
	assign Read_data_extend_h1 = (Read_data[31])?{16'hffff,Read_data[31:16]} : {16'h0,Read_data[31:16]};
	assign Read_data_extend_h2 = (Read_data[15])?{16'hffff,Read_data[15:0]} : {16'h0,Read_data[15:0]};

	assign Read_data_extend_lwl = ({32{ALU_out4[1:0] == 2'b00}} &  {Read_data[7:0],Data4[23:0]}) |
								  ({32{ALU_out4[1:0] == 2'b01}} &  {Read_data[15:0],Data4[15:0]}) |
								  ({32{ALU_out4[1:0] == 2'b10}} &  {Read_data[23:0],Data4[7:0]}) |
								  ({32{ALU_out4[1:0] == 2'b11}} &  Read_data[31:0]) |
								  32'b0;
	assign Read_data_extend_lwr = ({32{ALU_out4[1:0] == 2'b00}} &  Read_data[31:0]) |
								  ({32{ALU_out4[1:0] == 2'b01}} &  {Data4[31:24],Read_data[31:8]}) |
								  ({32{ALU_out4[1:0] == 2'b10}} &  {Data4[31:16],Read_data[31:16]}) |
								  ({32{ALU_out4[1:0] == 2'b11}} &  {Data4[31:8],Read_data[31:24]}) |
								  32'b0;

	wire[31:0] Read_data_result;
	reg [31:0] Read_data_reg;

	assign Read_data_result = ({32{pipe4_data[31:26] == `LB}} 	& Read_data_extend_b) 	|
							  ({32{pipe4_data[31:26] == `LBU}} 	& Read_data_extend_bu) 	|
							  ({32{pipe4_data[31:26] == `LH}} 	& Read_data_extend_h) 	|
							  ({32{pipe4_data[31:26] == `LHU}} 	& Read_data_extend_hu) 	|
							  ({32{pipe4_data[31:26] == `LWL}} 	& Read_data_extend_lwl) |
							  ({32{pipe4_data[31:26] == `LWR}} 	& Read_data_extend_lwr) |
							  ({32{pipe4_data[31:26] == `LW}} 	& Read_data) 			|
							  32'b0;

	//HI LO
	reg [31:0] HI;
	reg [31:0] LO;
	reg [31:0] HI_reg2;
	reg [31:0] HI_reg3;
	reg [31:0] HI_reg4;
	reg [31:0] LO_reg2;
	reg [31:0] LO_reg3;
	reg [31:0] LO_reg4;
	wire HI_wen;
	wire LO_wen;
	wire [31:0] HI_data;
	wire [31:0] LO_data;
	always@(posedge clk) 
	begin
		/*if(rst)
		begin 
			HI <= 32'b0;
		end	
		else */if(HI_wen)
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
		/*if(rst)
		begin 
			LO <= 32'b0;
		end	
		else */if(LO_wen)
		begin 
			LO <= LO_data;
		end
		else
		begin 
			LO <= LO;
		end
    end

    reg mtlo_reg_history;
    reg mthi_reg_history;
    always@(posedge clk) 
	begin
		if(rst)
		begin 
			mtlo_reg_history <= 1'b0;
            mthi_reg_history <= 1'b0;
		end	
		else
		begin 
			mtlo_reg_history <= mtlo_reg;
            mthi_reg_history <= mthi_reg;
		end
    end


	assign LO_wen = (op_mul_reg && counter_mul_choke && pipe2_valid && !mul_complete) || (mtlo_reg && !mtlo_reg_history && !(irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3)) || (op_div_reg && div_complete );//&& pipe2_valid);
	assign HI_wen = (op_mul_reg && counter_mul_choke && pipe2_valid && !mul_complete) || (mthi_reg && !mthi_reg_history && !(irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3)) || (op_div_reg && div_complete );//&& pipe2_valid);

	assign LO_data = ({32{mtlo_reg}} & alu_A_input) |
					 ({32{op_mul_last_reg}} & mul_result[31:0]) |
					 ({32{op_div_reg}} & div_q) |
					 32'b0;
	assign HI_data = ({32{mthi_reg}} & alu_A_input) |
					 ({32{op_mul_last_reg}} & mul_result[63:32]) |
					 ({32{op_div_reg}} & div_r) |
					 32'b0;

	wire mthi;
	wire mtlo;
	reg mthi_reg;
	reg mtlo_reg;
	assign mthi = ((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `MTHI) && (pipe1_data[20:6]==15'b0)) ? 1'b1 : 1'b0;
	assign mtlo = ((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `MTLO) && (pipe1_data[20:6]==15'b0)) ? 1'b1 : 1'b0;

	//////////////////////////////

	wire [63:0] mul_result;
	wire mul_signed;
	reg mul_signed_reg;
	wire [31:0] mul_x;
	wire [31:0] mul_y;
	wire op_mul;
	reg op_mul_reg;

	assign op_mul = ((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `MULT) && (pipe1_data[15:6]==10'b0)) |
					((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `MULTU) && (pipe1_data[15:6]==10'b0)) |
					1'b0;
	assign mul_signed = ((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `MULT) && (pipe1_data[15:6]==10'b0)) |
						1'b0;

	assign mul_x = {32{op_mul_reg}} & alu_A_input;
	assign mul_y = {32{op_mul_reg}} & alu_B_input;


	mul mul(clk, resetn, mul_signed_reg, mul_x, mul_y, mul_result); 

	reg [1:0]counter_mul_choke;
	always@(posedge clk)
	begin
		if(rst || !op_mul_reg)
		begin
			counter_mul_choke <= 2'b0; 
		end
		else if (counter_mul_choke == 2'b01)///////????div???????????
		begin
			counter_mul_choke <= 2'b0; 
		end
		else if(op_mul_reg)
		begin
			counter_mul_choke <= counter_mul_choke + 2'b1;
		end
		else 
		begin
			counter_mul_choke <= counter_mul_choke;
		end
	end

	reg mul_complete;
	always@(posedge clk)
	begin
		if(rst)
		begin
			mul_complete <= 1'b0; 
		end
		else if (mul_complete == 1'b1)
		begin
			if	(op_mul_reg != 1'b1)
				mul_complete <= 1'b0;
		end
		else if (counter_mul_choke == 2'b01 && mul_complete == 1'b0)///////????div???????????
		begin
			mul_complete <= 1'b1; 
		end
	end 

	reg op_mul_last_reg;
	always@(posedge clk)
	begin
		if(rst)
		begin
			op_mul_last_reg <= 1'b0; 
		end
		else 
		begin
			op_mul_last_reg <= op_mul_reg;
		end
	end

	//////////////////////////////
	reg [31:0] div_x;
	reg [31:0] div_y;
	wire [31:0] div_q;
	wire [31:0] div_r;
	wire div_complete;
	wire div_signed;
	reg div_signed_reg;
	wire op_div;
	reg op_div_reg;
	assign op_div = ((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `DIV )&& (pipe1_data[15:6]==10'b0)) |
					((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `DIVU )&& (pipe1_data[15:6]==10'b0)) |
					1'b0;
	assign div_signed = ((pipe1_data[31:26] == 6'b0) && (pipe1_data[5:0] == `DIV) && (pipe1_data[15:6]==10'b0)) |
						1'b0;

	//assign div_x = {32{op_div_reg}} & alu_A_input;
	//assign div_y = {32{op_div_reg}} & alu_B_input;
	always@(posedge clk)
	begin
		if(rst)
		begin
			div_x <= 32'b0; 
		end
		else if(op_div || div_history)//(div_history && (forward_pipe3_r1_reg || forward_pipe4_r1_reg || forward_wait5_r1_reg)))
		begin
			div_x <= alu_A_input; 
		end
	end

	always@(posedge clk)
	begin
		if(rst)
		begin
			div_y <= 32'b0;
		end
		else if(op_div || div_history)//( div_history && (forward_pipe3_r2_reg || forward_pipe4_r2_reg || forward_wait5_r2_reg)))
		begin
			div_y <= alu_B_input;
		end
	end

	reg div_history;
	always@(posedge clk)
	begin
		if(rst)
		begin
			div_history <= 1'b0; 
		end
		else if(counter_div_choke!=1'b0)
		begin
			div_history <= 1'b0;
		end
		else
		begin
			div_history <= op_div;
		end
	end

    reg div_history_reg;
	always@(posedge clk)
	begin
		if(rst)
		begin
			div_history_reg <= 1'b0; 
		end
		else
		begin
			div_history_reg <= div_history;
		end
	end

	wire div_choke_input;
	assign div_choke_input = div_history;// || div_history_reg; //&& (forward_pipe3_r1_reg || forward_pipe4_r1_reg || forward_wait5_r1_reg || forward_pipe3_r2_reg || forward_pipe4_r2_reg || forward_wait5_r2_reg);

	wire div_input;
	assign div_input = op_div_reg; //&& !counter_forwarding;

	div div(clk, resetn,div_input,div_signed_reg,div_x,div_y,div_choke_input,div_q,div_r,div_complete); 

	reg div_complete_reg;
	always@(posedge clk)
	begin
		if(rst)
		begin
			div_complete_reg <= 1'b0; 
		end
		else 
		begin
			div_complete_reg <= div_complete;
		end
	end

	reg [5:0]counter_div_choke;
	always@(posedge clk)
	begin
		if(rst || div_complete || !op_div_reg)
		begin
			counter_div_choke <= 6'b0; 
		end
		else if (counter_div_choke == 6'd34)
		begin
			counter_div_choke <= 6'b0; 
		end
		else if(/*(counter_div_choke == 1'b0) &&*/ op_div_reg && !div_complete_reg && !div_finished)
		begin
			counter_div_choke <= counter_div_choke + 6'b1;
		end
		/*else if(op_div_reg && !div_complete_reg && !div_finished && !div_history)
		begin
			counter_div_choke <= counter_div_choke + 6'b1;
		end*/
		else 
		begin
			counter_div_choke <= counter_div_choke;
		end
	end

	reg op_div_last_reg;
	always@(posedge clk)
	begin
		if(rst)
		begin
			op_div_last_reg <= 1'b0; 
		end
		else 
		begin
			op_div_last_reg <= op_div_reg;
		end
	end

    reg div_finished;
	always@(posedge clk)
	begin
		if(rst)
		begin
			div_finished <= 1'b0; 
		end
        else if( pipe1_allowin)
        begin
			div_finished <= 1'b0;
		end
		else if( div_complete)
		begin
			div_finished <= 1'b1;
		end
	end

	////////////////////cp0//////////////////////////
	//cp0_status
	wire[31:0] cp0_status;

	wire 		cp0_status_bev;
	reg [7:0]	cp0_status_im;
	reg			cp0_status_exl;
	reg			cp0_status_ie;

	assign	cp0_status_bev	=	1'b1;
	assign 	cp0_status 	={	{9{1'b0}}		,//31:23
							cp0_status_bev	,//22	bev
							6'd0			,//21:16
							cp0_status_im	,//15:8
							6'd0			,//7:2
							cp0_status_exl	,//1
							cp0_status_ie	//0
	};

	always @(posedge clk)
	begin
		if(rst)
		begin
			cp0_status_exl	<=	1'b0;
		end
		else if((irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3)&& pipe3_valid)///////////???????????
		begin
			cp0_status_exl	<=	1'b1;
		end
		else if(cp0_wen_status2 && pipe2_valid) ///////////??????????/
		begin
			cp0_status_exl	<=	cp0_write_data[1];
		end
		else if(eret)/////////??????????
		begin
			cp0_status_exl	<=	1'b0;
		end
		else
		begin
			cp0_status_exl	<= 	cp0_status_exl;
		end
	end

	always @(posedge clk)
	begin
		if(rst)
		begin
			cp0_status_ie	<=	1'b0;
		end
		else if(cp0_wen_status2 && pipe2_valid)
		begin
			cp0_status_ie	<=	cp0_write_data[0];
		end
		else
		begin
			cp0_status_ie	<= 	cp0_status_ie;
		end
	end

	always @(posedge clk)
	begin
		if(rst)
		begin
			cp0_status_im	<=	8'b0;
		end
		else if(cp0_wen_status2 && pipe2_valid)
		begin
			cp0_status_im	<=	cp0_write_data[15:8];
		end
		else
		begin
			cp0_status_im	<= 	cp0_status_im;
		end
	end

	//cp0_cause
	wire[31:0] cp0_cause;
	reg cp0_cause_bd;
	reg cp0_cause_ti;
	reg [4:0]cp0_cause_exccode;
	reg cp0_cause_ip0;
	reg cp0_cause_ip1;
	reg cp0_cause_ip2;
	reg cp0_cause_ip3;
	reg cp0_cause_ip4;
	reg cp0_cause_ip5;
	reg cp0_cause_ip6;
	reg cp0_cause_ip7;
	wire[5:0] int_input;
	assign int_input = 5'b0;

	assign 	cp0_cause 	={	cp0_cause_bd	,//31	bd
							cp0_cause_ti	,//30	ti
							14'd0			,//29:16
							cp0_cause_ip7	,//15
							cp0_cause_ip6	,//14
							cp0_cause_ip5	,//13
							cp0_cause_ip4	,//12
							cp0_cause_ip3	,//11
							cp0_cause_ip2	,//10
							cp0_cause_ip1	,//9
							cp0_cause_ip0	,//8
							1'd0			,//7
							cp0_cause_exccode	,//6:2
							2'd0			//1:0
	};

	always@(posedge	clk)
	begin
		if(rst)
		begin
			cp0_cause_exccode 	<= 5'b0;
		end
		else if(cp0_wen_cause2 && pipe2_valid)
		begin
			cp0_cause_exccode 	<= cp0_write_data[6:2];
		end
		else if(syscall_info3 && pipe3_valid)
		begin
			cp0_cause_exccode	<= 5'h08;
		end
		else if(break_info3 && pipe3_valid)
		begin
			cp0_cause_exccode	<= 5'h09;
		end
		else if(irp_overflow3 && pipe3_valid)
		begin
			cp0_cause_exccode	<= 5'h0c;
		end
		else if((irp_time3  || irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3)&& pipe3_valid)
		begin
			cp0_cause_exccode	<= 5'h00;
		end
		else if (irp_inst3 && pipe3_valid)
		begin
			cp0_cause_exccode	<= 5'h04;
		end
        else if(irp_support3 && pipe3_valid)
		begin
			cp0_cause_exccode	<= 5'h0a;
		end
        else if(irp_address_lw3 && pipe3_valid)
        begin
			cp0_cause_exccode	<= 5'h04;
		end
        else if (irp_address_sw3 && pipe3_valid)
		begin
			cp0_cause_exccode	<= 5'h05;
		end
	end

	always@(posedge	clk)
	begin
		if(rst)
		begin
			cp0_cause_bd 	<= 1'b0;
		end
		else if(((irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3 || irp_address_lw3 || irp_inst3 || irp_support3 || irp_time3 || irp_overflow3 || break_info3 || syscall_info3)&& pipe3_valid))
		begin
			cp0_cause_bd	<= slot_or_not_reg3;
		end
	end

	always@(posedge	clk)
	begin
		if(rst)
		begin
			cp0_cause_ti 	<= 1'b0;
		end
		else if(cp0_wen_compare2 && pipe2_valid)
		begin
			cp0_cause_ti	<= 1'b0;
		end
		else if(irp_time3)
		begin
			cp0_cause_ti	<= 1'b1;
		end
	end

	always@(posedge	clk)
	begin
		//ip0
		if(rst)
		begin
			cp0_cause_ip0 	<= 1'b0;
		end
		else if(cp0_wen_cause2 && pipe2_valid)
		begin
			cp0_cause_ip0 	<= cp0_write_data[8];
		end
		//ip1
		if(rst)
		begin
			cp0_cause_ip1 	<= 1'b0;
		end
		else if(cp0_wen_cause2 && pipe2_valid)
		begin
			cp0_cause_ip1 	<= cp0_write_data[9];
		end
		//ip2
		if(rst)
		begin
			cp0_cause_ip2 	<= 1'b0;
		end
		else
		begin
			cp0_cause_ip2   <= int_input[0];
		end
		//ip3
		if(rst)
		begin
			cp0_cause_ip3 	<= 1'b0;
		end
		else
		begin
			cp0_cause_ip3   <= int_input[1];
		end
		//ip4
		if(rst)
		begin
			cp0_cause_ip4 	<= 1'b0;
		end
		else
		begin
			cp0_cause_ip4   <= int_input[2];
		end
		//ip5
		if(rst)
		begin
			cp0_cause_ip5 	<= 1'b0;
		end
		else
		begin
			cp0_cause_ip5   <= int_input[3];
		end
		//ip6
		if(rst)
		begin
			cp0_cause_ip6 	<= 1'b0;
		end
		else
		begin
			cp0_cause_ip6   <= int_input[4];
		end
		//ip7
		if(rst)
		begin
			cp0_cause_ip7 	<= 1'b0;
		end
		else
		begin
			cp0_cause_ip7   <= int_input[5] || irp_time3;
		end
	end

	//epc
	reg[31:0]	cp0_epc;
	always @(posedge clk)
	begin
		if(rst)
		begin
			cp0_epc <= 32'b0;
		end
		else if((irp_time3 || irp_ip2_3 || irp_ip3_3 || irp_ip4_3 || irp_ip5_3 || irp_ip6_3 || irp_ip7_3 || irp_ip1_3|| irp_ip0_3 || irp_address_sw3|| irp_address_lw3 || irp_support3 ||  irp_overflow3 || break_info3 || syscall_info3)&& pipe3_valid)
		begin
			if(slot_or_not_reg3)
			begin
				cp0_epc <= pipe4_PC;
			end
			else
			begin
				cp0_epc <= pipe3_PC;
			end
		end
		/*else if(irp_time3 && pipe3_valid)
		begin
			cp0_epc <= pipe1_PC;
		end*/
		else if(irp_inst3 && pipe3_valid)
		begin
			if(slot_or_not_reg3)
			begin
				cp0_epc <= pipe4_PC;
			end
			else
			begin
				cp0_epc <= pipe3_PC;
			end
		end
		else if(cp0_wen_epc2 && pipe2_valid) ///////////????????????????
		begin
			cp0_epc <= cp0_write_data;
		end
		else
		begin
			cp0_epc <= cp0_epc;
		end
	end

	//count
	reg cp0_count_add;
	reg [31:0]		cp0_count;
	always@(posedge	clk)
	begin
		if(rst)
		begin
			cp0_count_add	<= 1'b0;
		end
		else if(clk)
		begin
			cp0_count_add	<= ~cp0_count_add;
		end
		//count
		if(rst)
		begin
			cp0_count 	<= 32'b0;
		end
		else if(cp0_wen_count2 && pipe2_valid)
		begin
			cp0_count 	<= cp0_write_data;
		end
		else if(cp0_count_add)
		begin
			cp0_count 	<= cp0_count + 1'b1;
		end
	end

	//compare
	reg [31:0]		cp0_compare;
	always@(posedge	clk)
	begin
		if(rst)
		begin
			cp0_compare 	<= 32'b0;
		end
		else if(cp0_wen_compare2 && pipe2_valid)
		begin
			cp0_compare 	<= cp0_write_data;
		end
	end

	//badvaddr
	reg [31:0]		cp0_badvaddr;
	always@(posedge	clk)
	begin
		if(rst)
		begin
			cp0_badvaddr 	<= 32'b0;
		end
		else if(irp_inst3 && pipe3_valid)
		begin
			cp0_badvaddr 	<= pipe3_PC;
		end
		else if(irp_address_sw3 || irp_address_lw3)
		begin
			cp0_badvaddr 	<= ALU_out3;
		end
	end
endmodule