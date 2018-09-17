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

	wire next;
	reg next_state;
	reg next_reg;
	reg Inst_Req_Valid_reg;
	reg Inst_Ack_reg;
	reg Read_data_Ack_reg;
	reg PCWrite1_reg;
	reg read;
	reg read_state;

	assign next = next_reg;
	assign Inst_Req_Valid = Inst_Req_Valid_reg;
	assign Inst_Ack = Inst_Ack_reg;
	assign Read_data_Ack = Read_data_Ack_reg;
	
	always@(posedge clk)
	begin
		if(rst || symbol == 3'd0 )
		begin
			read_state <= 1'b0;
		end
		else if(symbol == 3'd3 && next_state)
		begin
			read_state <= 1'b1;
		end
		/*else if (symbol == 3'd0)
		begin
			read_state <= 1'b0;
		end*/
		else 
		begin
			read_state <= read_state;
		end
	end

	reg counter;
	always@(posedge clk)
	begin
		if(rst || symbol == 3'd0 )
		begin
			counter <= 1'b0;
		end
		else if(instruction[31:26] == `LW || instruction[31:26] == `LB || instruction[31:26] == `LH || instruction[31:26] ==`LWL || instruction[31:26] == `LWR || instruction[31:26] == `LBU || instruction[31:26] == `LHU)
		begin
			counter <= 1'b1;
		end
		else 
		begin
			counter <= counter;
		end
	end

	always@(*)
	begin
		if(rst)
		begin
			Inst_Req_Valid_reg <= 1'b0;
			Inst_Ack_reg <= 1'b1;
			Read_data_Ack_reg <= 1'b0;
			next_reg <= 1'b0;
		end
		else
		begin
			if(symbol == 3'b0)
			begin
				read <= 1'b0;
				Read_data_Ack_reg <= 1'b0;
				Inst_Req_Valid_reg <= 1'b1;
				Inst_Ack_reg <= 1'b0;
				if(Inst_Req_Ack)
				begin
					next_reg <= 1'b1;
				end
				else
				begin
					next_reg <= 1'b0;
				end
			end
			else if (symbol == 3'd5)
			begin
				read <= 1'b0;
				Read_data_Ack_reg <= 1'b0;
				Inst_Req_Valid_reg <= 1'b0;
				if(Inst_Valid == 1'b0)
				begin
					Inst_Ack_reg <= 1'b0;
					next_reg <= 1'b0;
					PCWrite1_reg <= 1'b0;
				end
				else
				begin
					Inst_Ack_reg <= 1'b1;
					next_reg <= 1'b1;
					PCWrite1_reg <= 1'b1;
				end
			end
			else if (symbol == 3'd2)
			begin
				read <= 1'b0;
				Read_data_Ack_reg <= 1'b0;
				Inst_Req_Valid_reg <= 1'b0;
				Inst_Ack_reg <= 1'b0;
				next_reg <= 1'b1;
			end
			else if (symbol == 3'd3)
			begin
				Inst_Req_Valid_reg <= 1'b0;
				Inst_Ack_reg <= 1'b0;
				if(instruction[31:26] == `SW || instruction[31:26] == `SB || instruction[31:26] == `SH || instruction[31:26] ==`SWL || instruction[31:26] == `SWR )
				begin
					read <= 1'b0;
					Read_data_Ack_reg <= 1'b0;
					if(Mem_Req_Ack == 1'b1)
					begin
						next_reg <= 1'b1;
					end
					else
					begin
						next_reg <= 1'b0;
					end
				end
				else if(instruction[31:26] == `LW || instruction[31:26] == `LB || instruction[31:26] == `LH || instruction[31:26] ==`LWL || instruction[31:26] == `LWR || instruction[31:26] == `LBU || instruction[31:26] == `LHU)
				begin

					if (read_state == 1'b0)
					begin
						next_reg <= 1'b0;
						read <= 1'b0;
						if (Mem_Req_Ack == 1'b1)
						begin
							next_state <= 1'b1;
						end
						else
						begin
							next_state <= 1'b0;
						end
					end
					else
					begin
						read <= 1'b1;
						Read_data_Ack_reg <= 1'b1;
						if (Read_data_Valid == 1'b0)
						begin
							next_reg <= 1'b0;
						end
						else
						begin
							next_reg <= 1'b1;
						end
					end
				end
				else
				begin
					read <= 1'b0;
					Read_data_Ack_reg <= 1'b0;
					next_reg <= 1'b1;
				end
			end
			else
			begin
				read <= 1'b0;
				Read_data_Ack_reg <= 1'b0;
				Inst_Req_Valid_reg <= 1'b0;
				Inst_Ack_reg <= 1'b0;
				next_reg <= 1'b1;
			end			
		end
	end



	////// output
	assign Address = (IorD) ? ALUOut : PC_wire ;

	reg [31:0] Write_data_reg;
	always@(*)
	begin
		if(instruction[31:26] == 6'b101010)//SWL
		begin
			case(Address[1:0])
			2'b00: Write_data_reg <= {24'b0,B_reg[31:24]};
			2'b01: Write_data_reg <= {16'b0,B_reg[31:16]};
			2'b10: Write_data_reg <= {8'b0,B_reg[31:8]};
			2'b11: Write_data_reg <= B_reg;
			endcase
		end
		else if(instruction[31:26] == 6'b101110)//SWR
		begin
			case(Address[1:0])
			2'b00: Write_data_reg <= B_reg;
			2'b01: Write_data_reg <= {B_reg[23:0],8'b0};
			2'b10: Write_data_reg <= {B_reg[15:0],16'b0};
			2'b11: Write_data_reg <= {B_reg[7:0],24'b0};
			endcase
		end
		else if(instruction[31:26] == 6'b101000)//SB
		begin
			case(Address[1:0])
			2'b00: Write_data_reg <= B_reg;
			2'b01: Write_data_reg <= {B_reg[23:0],8'b0};
			2'b10: Write_data_reg <= {B_reg[15:0],16'b0};
			2'b11: Write_data_reg <= {B_reg[7:0],24'b0};
			endcase
		end
		else if(instruction[31:26] == 6'b101001)//SH
		begin
			case(Address[1])
			1'b0: Write_data_reg <= B_reg;
			1'b1: Write_data_reg <= {B_reg[15:0],16'b0};
			endcase
		end
		else
		begin
			Write_data_reg <= B_reg;
		end
	end
	assign Write_data = Write_data_reg;




	wire [33:0] Shift_left2_b;
	wire [31:0] sign_extend;

	///////ALU
	wire [`DATA_WIDTH - 1:0] A;
	wire [`DATA_WIDTH - 1:0] B;
	wire [2:0] ALUop;
	wire Overflow;
	wire CarryOut;
	wire Zero;
	wire [`DATA_WIDTH - 1:0] Result;
	alu ALU(A,B,ALUop,Overflow,CarryOut,Zero,Result);


	reg [31:0] A_reg;
	reg [31:0] B_reg;
	reg [31:0] B_insert;
	wire [31:0] B_middle;
	always @(posedge clk)
	begin
		if(rst)
		begin 
			A_reg <= 32'b0;
			B_reg <= 32'b0;		
		end
		else
		begin
			A_reg <= rdata1;
			B_reg <= rdata2;	
		end		
	end
	assign A = (ALUSrcA && ~(symbol == 3'd2 && instruction[31:26] == 6'b0 && instruction[5:0] == 6'b001001)) ? A_reg : PC_wire; // JALR
	always @(*)
	begin
		case(ALUSrcB)
		2'b00: B_insert <= B_reg;
		2'b01: B_insert <= 32'd4;
		2'b10: B_insert <= sign_extend;
		2'b11 : B_insert <= Shift_left2_b;
		endcase
	end
	assign B_middle = (symbol == 3'd2 && instruction[31:26] == 6'b0 && (instruction[5:0]== 6'b001011 || instruction[5:0]== 6'b001010)) ? 32'd0 : B_insert;//MOVN'MOVZ
	assign B = (symbol == 3'd2 && instruction[31:26] == 6'b0 && instruction[5:0] == 6'b001001) ? 32'd4 : B_middle; //JALR

	
	reg [31:0] ALUOut_reg;
	wire [31:0] ALUOut;
	always @(posedge clk)
	begin
		if(rst)
		begin 
			ALUOut_reg <= 32'b0;
		end
		else if (symbol == 3'd2)
		begin
			if(instruction[31:26] == 6'b001111)//LUI
			begin
				ALUOut_reg <= {instruction[15:0],16'b0};	
			end
			else if (instruction[31:26] == 6'b001011)//SLTIU
			begin
				ALUOut_reg <= {31'b0,CarryOut};
			end
			else if (instruction[31:26] == 6'b000000)
			begin 
				if(instruction[5:0] == 6'b101011 )//SLTU
				begin
					ALUOut_reg <= {31'b0,CarryOut};
				end
				else if(instruction[5:0] == 6'b000000)//SLL
				begin
					ALUOut_reg <= sll;
				end
				else if (instruction[5:0] == 6'b000100)//SLLV
				begin
					ALUOut_reg <= sll;
				end
				else if (instruction[5:0] == 6'b000011)//SRA
				begin
					ALUOut_reg <= sra_result;
				end
				else if (instruction[5:0] == 6'b000111)//SRAV
				begin
					ALUOut_reg <= sra_result;
				end
				else if (instruction[5:0] == 6'b000010)//SRL
				begin
					ALUOut_reg <= srl;
				end
				else if (instruction[5:0] == 6'b000110)//SRLV
				begin
					ALUOut_reg <= srl;
				end	
				else if (instruction[5:0] == 6'b100111)//NOR
				begin
					ALUOut_reg <= ~Result;
				end
				//else if (instruction[5:0] == 6'b001001)//JALR
				//begin
				//	ALUOut_reg <= ~Result;
				//end
				else
				begin
					ALUOut_reg <= Result;	
				end
			end	
			else
			begin
				ALUOut_reg <= Result;	
			end
		end	
		else
		begin
			ALUOut_reg <= Result;	
		end		
	end
	assign ALUOut = ALUOut_reg;


	reg [2:0] ALUop_reg_I;
	reg [2:0] ALUop_reg_R;
	always @(*)
	begin
		case(ALUOp)
		3'b010:
			ALUop_reg_I <= 3'b010;
		3'b110:
			ALUop_reg_I <= 3'b110;
		3'b111:
			ALUop_reg_I <= 3'b111;		
		3'b000:
			ALUop_reg_I <= 3'b000;
		3'b001:
			ALUop_reg_I <= 3'b001;
		3'b000:
			ALUop_reg_I <= 3'b000;
		3'b011:
			ALUop_reg_I <= 3'b011;
		default:
			ALUop_reg_I <= 3'b010;
		endcase
	end
	always @(*)
	begin
		case(instruction[5:0])
		6'b100011://SUBU
			ALUop_reg_R <= 3'b110;
		6'b001000://JR
			ALUop_reg_R <= 3'b010;
		6'b001001://JALR
			ALUop_reg_R <= 3'b010;
		6'b100101://OR
			ALUop_reg_R <= 3'b001;
		6'b100001://ADDU
			ALUop_reg_R <= 3'b010;	
		6'b101010:
			ALUop_reg_R <= 3'b111;
		6'b100100://AND
			ALUop_reg_R <= 3'b000;	
		6'b001011://MOVN
			ALUop_reg_R <= 3'b010;
		6'b001010://MOVZ
			ALUop_reg_R <= 3'b010;		
		6'b100110://XOR
			ALUop_reg_R <= 3'b011;
		6'b101011://SLTU
			ALUop_reg_R <= 3'b110;	
		6'b100111://NOR
			ALUop_reg_R <= 3'b001;	
		default:
			ALUop_reg_R <= 3'b000;
		endcase
	end
	assign ALUop = (instruction[31:26] == 6'b000000 && symbol == 3'd2)?ALUop_reg_R:ALUop_reg_I;


	assign Shift_left2_b = {sign_extend ,2'b0};

	
	wire [31:0] sign_extend_middle;
	reg [31:0] sign_extend_reg;
	assign sign_extend_middle = (instruction[15])?{16'hffff,instruction[15:0]}:{16'b0,instruction[15:0]};
	always @(*)
	begin
		if(instruction[31:26]==6'b001100 || instruction[31:26]==6'b001101 || instruction[31:26]==6'b001110)
		begin
			sign_extend_reg <= {16'b0,instruction[15:0]};
		end
		else
		begin
			sign_extend_reg <= sign_extend_middle;
		end
		
	end
	assign sign_extend = sign_extend_reg;


	
	/////////  register file 
	wire [`ADDR_WIDTH - 1:0] waddr;
	wire [`ADDR_WIDTH - 1:0] raddr1;
	wire [`ADDR_WIDTH - 1:0] raddr2;
	wire wen;
	wire [`DATA_WIDTH - 1:0] wdata;
	wire [`DATA_WIDTH - 1:0] rdata1;
	wire [`DATA_WIDTH - 1:0] rdata2;
	reg_file registers(clk,rst,waddr,raddr1,raddr2,wen,wdata,rdata1,rdata2);

	assign wdata = (MemtoReg) ? Memory_data : ALUOut;
	
	wire movn;
	wire movz;
	wire wen_plus1;
	wire wen_plus2;
	assign movn = (B_reg == 32'b0)?1'b0:1'b1;
	assign movz = (B_reg == 32'b0)?1'b1:1'b0;
	assign wen_plus1 = (symbol == 3'd3 && instruction[31:26] == 6'b000000 && instruction[5:0] == 6'b001011)? movn : 1'b1;
	assign wen_plus2 = (symbol == 3'd3 && instruction[31:26] == 6'b000000 && instruction[5:0] == 6'b001010)? movz : wen_plus1;
	assign wen = RegWrite & wen_plus2;

	
	assign raddr1 = instruction[25:21];
	assign raddr2 = instruction[20:16];
	
	wire[4:0] waddr_middle;
	assign waddr = (symbol == 3'd3 && (instruction[31:26] == 6'b000011 || (instruction[31:26] == 6'b000000 && instruction[5:0] == 6'b001001)))?5'd31:waddr_middle;//JAL&JALR
	assign waddr_middle = (RegDst)?instruction[15:11]:instruction[20:16];




	////////      control_unit 
	wire RegDst;
	wire Branch;
	wire MemtoReg;
	wire [2:0]ALUOp;

	wire [1:0]ALUSrcB;
	wire RegWrite;
	wire PCWriteCond;
	wire PCWrite;
	wire IorD;
	wire IRwrite;
	wire ALUSrcA;
	wire [1:0]PCSource;

	reg RegDst_reg;
	reg Branch_reg;
	reg MemRead_reg;
	reg MemtoReg_reg;
	reg [2:0]ALUOp_reg;
	reg MemWrite_reg;
	reg [1:0]ALUSrcB_reg;
	reg RegWrite_reg;
	reg PCWriteCond_reg;
	reg PCWrite_reg;
	reg IorD_reg;
	reg IRwrite_reg;
	reg ALUSrcA_reg;
	reg [1:0]PCSource_reg;

	assign RegDst = RegDst_reg;
	assign MemRead = (read == 1'b1) ? 1'b0 : MemRead_reg;
	assign MemtoReg = MemtoReg_reg;
	assign ALUOp = ALUOp_reg;
	assign MemWrite = MemWrite_reg;
	assign ALUSrcB = ALUSrcB_reg;
	assign RegWrite = RegWrite_reg;
	assign PCWriteCond = PCWriteCond_reg;
	assign PCWrite = (symbol == 3'd5) ? PCWrite1_reg : PCWrite_reg;
	assign IorD = IorD_reg;
	assign IRwrite = IRwrite_reg;
	assign ALUSrcA = ALUSrcA_reg;
	assign PCSource = PCSource_reg;


	reg [2:0]symbol;
	always @(posedge clk)
	begin
		if(rst)
		begin 
			symbol <= 3'b0;	
		end
		else if (symbol == 3'd0)
		begin
			if(next == 1'b1)	
			begin			
				symbol <= 3'd5;
			end
			else
			begin
				symbol <= 3'd0;
			end
		end
		else if (symbol == 3'd5)
		begin				
			if(next == 1'b1)	
			begin			
				symbol <= 3'd1;
			end
			else
			begin
				symbol <= 3'd5;
			end
		end
		else if (symbol == 3'd1)
		begin			
			if(next == 1'b1)	
			begin			
				symbol <= 3'd2;
			end
			else
			begin
				symbol <= 3'd1;
			end
		end	
		else if (symbol == 3'd2)
		begin
			if (index && next)
			begin
				symbol <= 3'd3;
			end
			else if (next == 1'b1)
			begin
				symbol <= 3'd0;
			end
			else
			begin
				symbol <= 3'd2;
			end
		end		
		else if (symbol == 3'd3)
		begin
			if (counter == 1'b1)
			begin
				symbol <= 3'd6;
			end
			else if (index && next)
			begin
				symbol <= 3'd4;
			end
			else if (next == 1'b1)
			begin
				symbol <= 3'd0;
			end
			else
			begin
				symbol <= 3'd3;
			end

		end		
		else if(symbol == 3'd6)
		begin
			symbol <= 3'd4;
		end
		else //if(symbol == 3'd4)
		begin
			symbol <= 3'd0;
		end			
	end

	wire index;
	reg index_reg;
	assign index = index_reg;	

	always @(*)
	begin
		if(symbol == 3'd0)
		begin
			RegDst_reg <= 1'b0;
			MemRead_reg <= 1'b0;///!!!!!!!!!!!!!!!!!!!!!!!!!!!
			MemtoReg_reg <= 1'b0;
			ALUOp_reg <= 3'b010;
			MemWrite_reg <= 1'b0;
			ALUSrcB_reg <= 2'b01;
			ALUSrcA_reg <= 1'b0;
			RegWrite_reg <= 1'b0;
			PCWriteCond_reg <= 1'b0;
			PCWrite_reg <= 1'b0;
			IorD_reg <= 1'b0;
			IRwrite_reg <= 1'b1;
			PCSource_reg <= 2'b00;
			index_reg <= 1'b0;
		end
		else if(symbol == 3'd6)
		begin
			RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
		end
		else if (symbol == 3'd5)
		begin
			RegDst_reg <= 1'b0;
			MemRead_reg <= 1'b0;///!!!!!!!!!!!!!!!!!!!!!!!!!!!
			MemtoReg_reg <= 1'b0;
			ALUOp_reg <= 3'b010;
			MemWrite_reg <= 1'b0;
			ALUSrcB_reg <= 2'b01;
			ALUSrcA_reg <= 1'b0;
			RegWrite_reg <= 1'b0;
			PCWriteCond_reg <= 1'b0;
			PCWrite_reg <= 1'b0;
			IorD_reg <= 1'b0;
			IRwrite_reg <= 1'b1;
			PCSource_reg <= 2'b00;
			index_reg <= 1'b0;
		end
		else if (symbol == 3'd1)
		begin
			RegDst_reg <= 1'b0;
			MemRead_reg <= 1'b0;///!!!!!!!!!!!!!!!!!!!!!!!!!!!
			MemtoReg_reg <= 1'b0;
			ALUOp_reg <= 3'b010;
			MemWrite_reg <= 1'b0;
			ALUSrcB_reg <= 2'b11;
			ALUSrcA_reg <= 1'b0;
			RegWrite_reg <= 1'b0;
			PCWriteCond_reg <= 1'b0;
			PCWrite_reg <= 1'b0;
			IorD_reg <= 1'b0;
			IRwrite_reg <= 1'b0;
			PCSource_reg <= 2'b00;
			index_reg <= 1'b0;
		end
		else if (symbol == 3'd2)
		begin
			case(instruction[31:26])
			`LW:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LB:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LBU:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LH:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LHU:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LWL:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LWR:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`SW:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`SB:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`SH:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`SWL:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`SWR:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`ADDIU:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`ANDI:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b000;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`ORI:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b001;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`XORI:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b011;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LUI:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`NOP:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b00;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`BNE:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b110;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b00;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b1;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b01;
				index_reg <= 1'b0;
			end
			`BEQ:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b110;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b00;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b1;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b01;
				index_reg <= 1'b0;
			end
			`J:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b110;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b00;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b1;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b10;
				index_reg <= 1'b0;
			end
			`JAL:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b01;
				ALUSrcA_reg <= 1'b0;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b1;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b10;
				index_reg <= 1'b1;
			end
			`SLTI:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b111;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`SLTIU:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b110;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`REGIMM:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b11;
				ALUSrcA_reg <= 1'b0;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b1;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`BLEZ:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b11;
				ALUSrcA_reg <= 1'b0;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b1;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`BGTZ:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b11;
				ALUSrcA_reg <= 1'b0;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b1;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			default:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b11;
				ALUSrcA_reg <= 1'b0;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b1;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			endcase
		end
		else if (symbol == 3'd3)
		begin
			case(instruction[31:26])
			`NOP:
			begin
				RegDst_reg <= 1'b1;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b1;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`ADDIU:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b1;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`ANDI:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b1;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`ORI:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b1;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`XORI:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b1;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`SLTI:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b1;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`SLTIU:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b1;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`LUI:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b1;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`SW: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b1;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`SB: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b1;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`SH: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b1;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`SWL: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b1;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`SWR: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b1;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b0;
			end
			`LW: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b1;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LB: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b1;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LBU: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b1;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LH: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b1;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LHU: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b1;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LWL: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b1;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`LWR: 
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b1;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b010;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b10;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b0;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b1;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b00;
				index_reg <= 1'b1;
			end
			`JAL:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b110;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b00;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b1;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b11;
				index_reg <= 1'b0;
			end
			default:
			begin
				RegDst_reg <= 1'b0;
				MemRead_reg <= 1'b0;
				MemtoReg_reg <= 1'b0;
				ALUOp_reg <= 3'b110;
				MemWrite_reg <= 1'b0;
				ALUSrcB_reg <= 2'b00;
				ALUSrcA_reg <= 1'b1;
				RegWrite_reg <= 1'b1;
				PCWriteCond_reg <= 1'b0;
				PCWrite_reg <= 1'b0;
				IorD_reg <= 1'b0;
				IRwrite_reg <= 1'b0;
				PCSource_reg <= 2'b11;
				index_reg <= 1'b0;
			end
			endcase			
		end
		else//symbol == 3'd4
		begin
			RegDst_reg <= 1'b0;
			MemRead_reg <= 1'b0;///!!!!!!!!!!!!!!!!!!!!!!!!!!!
			MemtoReg_reg <= 1'b1;
			ALUOp_reg <= 3'b010;
			MemWrite_reg <= 1'b0;
			ALUSrcB_reg <= 2'b00;
			ALUSrcA_reg <= 1'b0;
			RegWrite_reg <= 1'b1;
			PCWriteCond_reg <= 1'b0;
			PCWrite_reg <= 1'b0;
			IorD_reg <= 1'b0;
			IRwrite_reg <= 1'b0;
			PCSource_reg <= 2'b00;
			index_reg <= 1'b0;
		end
	end



	/////////      pc
	reg [31:0] PC_reg;
	reg [31:0] PC_reg_old;
	reg [31:0] PC_reg_default;
	wire [31:0] PC_wire;
	wire [33:0] Shift_left2_1;
	wire PC_wen;
	wire Zero_insert;
	reg Zero_insert_reg;
	wire [31:0] PC_mux;
	reg [31:0]PC_mux_reg;
	always @(posedge clk)
	begin
		if(rst)
		begin 
			PC_reg <= 32'hbfc00000;	
		end
		else if (PC_wen)
		begin
			PC_reg <= PC_mux;
		end
		else if (symbol == 3'd3 && (instruction[31:26]==6'b0 && (instruction[5:0]==6'b001000 || instruction[5:0]==6'b001001)))//JR'JALR
		begin
			PC_reg <= A_reg;
		end
		else
		begin
			PC_reg <= PC_reg;
		end		
	end
	always @(posedge clk)
	begin
		if(rst)
		begin 
			PC_reg_old <= 32'hbfc00000;	
		end
		else if (symbol == 3'd2)
		begin
			PC_reg_old <= PC_reg;
		end
		else
		begin
			PC_reg_old <= PC_reg_old;
		end		
	end
	always @(posedge clk)
	begin
		if(rst)
		begin 
			PC_reg_default <= 32'hbfc00000;	
		end
		else if (symbol == 3'd5)
		begin
			PC_reg_default <= PC_reg;
		end
		else
		begin
			PC_reg_default <= PC_reg_default;
		end		
	end

	assign PC_wire = ((instruction[31:26] == 6'b000001 || instruction[31:26] == 6'b000110) && symbol == 3'd2) ? PC_reg_old : PC_reg;//blez REGIMM
	assign PC = PC_reg;
	assign PC_wen = (Zero_insert & PCWriteCond) | PCWrite ;
	always @(*)
	begin
		case(PCSource)
		2'b00: PC_mux_reg <= Result;
		2'b01: PC_mux_reg <= ALUOut;
		2'b10: PC_mux_reg <= Shift_left2_1;
		default: PC_mux_reg <= Result;
		endcase
	end
	assign PC_mux = PC_mux_reg;

	

	assign Shift_left2_1 = {PC_wire[31:28],{instruction[25:0],2'b0}};


	always @(*)
	begin
		if(instruction[31:26]==6'b000101)
		begin
			Zero_insert_reg <= ~Zero;
		end
		else if(instruction[31:26]==6'b000001 && instruction[20:16]==5'b00000)//BLTZ
		begin
			if(A_reg[31])
			begin
				Zero_insert_reg <= 1'b1;
			end
			else
			begin
				Zero_insert_reg <= 1'b0;
			end
		end
		else if(instruction[31:26]==6'b000001 && instruction[20:16]==5'b00001)//BGEZ
		begin
			if(~A_reg[31])
			begin
				Zero_insert_reg <= 1'b1;
			end
			else
			begin
				Zero_insert_reg <= 1'b0;
			end
		end
		else if(instruction[31:26]==6'b000110 )//BLEZ
		begin
			if(A_reg[31] | (A_reg == 32'b0))
			begin
				Zero_insert_reg <= 1'b1;
			end
			else
			begin
				Zero_insert_reg <= 1'b0;
			end
		end
		else if(instruction[31:26]==6'b000111 )//BGTZ
		begin
			if((A_reg[31]==1'b0) && (A_reg != 32'b0))
			begin
				Zero_insert_reg <= 1'b1;
			end
			else
			begin
				Zero_insert_reg <= 1'b0;
			end
		end
		else if(instruction[31:26]==6'b000010 || instruction[31:26]==6'b000011)
		begin
			Zero_insert_reg <= 1'b1;
		end
		else
		begin
			Zero_insert_reg <= Zero;
		end
	end
	assign Zero_insert = Zero_insert_reg;


	//////// instruction_reg
	reg [31:0] Instruction_reg;
	wire [31:0] instruction;
	wire [31:0] MemData;
	always @(posedge clk)
	begin
		if(rst)
		begin 
			Instruction_reg <= 32'b0;	
		end
		else if (IRwrite)
		begin
			Instruction_reg <= MemData;
		end
		else
		begin
			Instruction_reg <= Instruction_reg;
		end		
	end
	assign instruction = Instruction_reg;
	assign MemData = Instruction;////////////


	//////// MEMORY DATA REGISTER
	reg [31:0] Memory_data_reg;

	wire [31:0] Memory_data;
	always @(posedge clk)
	begin
		if(rst)
		begin 
			Memory_data_reg <= 32'b0;	
		end
		else if(instruction[31:26] == 6'b100000)//LB
		begin
			Memory_data_reg <= Read_data_extend_b;
		end
		else if(instruction[31:26] == 6'b100001)//LH
		begin
			Memory_data_reg <= Read_data_extend_h;
		end
		else if (instruction[31:26] == 6'b100100)//LBU
		begin
			Memory_data_reg <= Read_data_extend_bu;
		end
		else if (instruction[31:26] == 6'b100101)//LHU
		begin
			Memory_data_reg <= Read_data_extend_hu;
		end
		else if (instruction[31:26] == 6'b100010)//LWL
		begin
			Memory_data_reg <= LWL;
		end
		else if (instruction[31:26] == 6'b100110)//LWR
		begin
			Memory_data_reg <= LWR;
		end
		else
		begin
			Memory_data_reg <= Read_data;
		end		
	end
	assign Memory_data = Memory_data_reg;

	wire [31:0] Read_data_extend_b;
	reg [31:0] Read_data_extend_b_reg;
	wire [31:0] Read_data_extend_b1;
	wire [31:0] Read_data_extend_b2;
	wire [31:0] Read_data_extend_b3;
	wire [31:0] Read_data_extend_b4;
	wire [31:0] Read_data_extend_bu;
	reg [31:0] Read_data_extend_bu_reg;
	wire [31:0] Read_data_extend_h;
	wire [31:0] Read_data_extend_hu;
	wire [31:0] Read_data_extend_h1;
	wire [31:0] Read_data_extend_h2;
	wire [31:0] LWL;
	reg [31:0] LWL_reg;
	wire [31:0] LWR;
	reg [31:0] LWR_reg;
	reg [31:0] Read_data_process_reg;
	
	assign Read_data_extend_b1 = (Read_data[7])?{24'hffffff,Read_data[7:0]} : {24'h0,Read_data[7:0]};
	assign Read_data_extend_b2 = (Read_data[15])?{24'hffffff,Read_data[15:8]} : {24'h0,Read_data[15:8]};
	assign Read_data_extend_b3 = (Read_data[23])?{24'hffffff,Read_data[23:16]} : {24'h0,Read_data[23:16]};
	assign Read_data_extend_b4 = (Read_data[31])?{24'hffffff,Read_data[31:24]} : {24'h0,Read_data[31:24]};
	always @(*)
	begin
		case(Address[1:0])
		2'b00:Read_data_extend_b_reg <= Read_data_extend_b1;
		2'b01:Read_data_extend_b_reg <= Read_data_extend_b2;
		2'b10:Read_data_extend_b_reg <= Read_data_extend_b3;
		2'b11:Read_data_extend_b_reg <= Read_data_extend_b4;
		endcase	
	end
	assign Read_data_extend_b = Read_data_extend_b_reg;
	always @(*)
	begin
		case(Address[1:0])
		2'b00:Read_data_extend_bu_reg <= {24'h0,Read_data[7:0]};
		2'b01:Read_data_extend_bu_reg <= {24'h0,Read_data[15:8]};
		2'b10:Read_data_extend_bu_reg <= {24'h0,Read_data[23:16]};
		2'b11:Read_data_extend_bu_reg <= {24'h0,Read_data[31:24]};
		endcase	
	end
	assign Read_data_extend_bu = Read_data_extend_bu_reg;


	assign Read_data_extend_h = (Address[1])?Read_data_extend_h1:Read_data_extend_h2;
	assign Read_data_extend_hu = (Address[1])?{16'h0,Read_data[31:16]}: {16'h0,Read_data[15:0]};
	assign Read_data_extend_h1 = (Read_data[31])?{16'hffff,Read_data[31:16]} : {16'h0,Read_data[31:16]};
	assign Read_data_extend_h2 = (Read_data[15])?{16'hffff,Read_data[15:0]} : {16'h0,Read_data[15:0]};
	always @(*)
	begin
		case(Address[1:0])
		2'b00:LWL_reg <= {Read_data[7:0],rdata2[23:0]};
		2'b01:LWL_reg <= {Read_data[15:0],rdata2[15:0]};
		2'b10:LWL_reg <= {Read_data[23:0],rdata2[7:0]};
		2'b11:LWL_reg <= Read_data[31:0];
		endcase
	end
	assign LWL = LWL_reg;
	always @(*)
	begin
		case(Address[1:0])
		2'b00:LWR_reg <= Read_data[31:0];
		2'b01:LWR_reg <= {rdata2[31:24],Read_data[31:8]};
		2'b10:LWR_reg <= {rdata2[31:16],Read_data[31:16]};
		2'b11:LWR_reg <= {rdata2[31:8],Read_data[31:24]};
		endcase
	end
	assign LWR = LWR_reg;



	reg [3:0] Write_strb_reg;
	always @(*)
	begin
		if(instruction[31:26] == 6'b101000)//SB
		begin
			if(Address[1:0] == 2'b00)
			begin
				Write_strb_reg = 4'b0001;
			end
			else if(Address[1:0] == 2'b01)
			begin
				Write_strb_reg = 4'b0010;
			end
			else if(Address[1:0] == 2'b10)
			begin
				Write_strb_reg = 4'b0100;
			end
			else
			begin
				Write_strb_reg = 4'b1000;
			end
		end
		else if(instruction[31:26] == 6'b101001 && Address[1] == 1'b0)//SH
		begin
			Write_strb_reg = 4'b0011;
		end
		else if(instruction[31:26] == 6'b101001 && Address[1] == 1'b1)//SH
		begin
			Write_strb_reg = 4'b1100;
		end
		else if(instruction[31:26] == 6'b101110)//SWR
		begin
			if(Address[1:0]==2'b00)
			begin
				Write_strb_reg = 4'b1111;
			end
			else if(Address[1:0]==2'b01)
			begin
				Write_strb_reg = 4'b1110;
			end
			else if(Address[1:0]==2'b10)
			begin
				Write_strb_reg = 4'b1100;
			end
			else
			begin
				Write_strb_reg = 4'b1000;
			end
		end
		else if(instruction[31:26] == 6'b101010)//SWL
		begin
			if(Address[1:0]==2'b00)
			begin
				Write_strb_reg = 4'b0001;
			end
			else if(Address[1:0]==2'b01)
			begin
				Write_strb_reg = 4'b0011;
			end
			else if(Address[1:0]==2'b10)
			begin
				Write_strb_reg = 4'b0111;
			end
			else
			begin
				Write_strb_reg = 4'b1111;
			end
		end
		else
		begin
			Write_strb_reg = 4'b1111;
		end
		
	end

	assign Write_strb = Write_strb_reg;


	

	////////shifter
	reg [31:0]shift_result_reg;
	wire [4:0]shift_amount;
	reg [4:0]shift_amount_reg;
	wire [31:0]sra_result;
	wire [93:0]sll_middle;
	wire [62:0]sra_middle;
	wire [62:0]srl_middle;
	wire [31:0]sll;
	wire [31:0]sra;
	wire [31:0]srl;
	
	assign sll_middle = {31'b0,B_reg,31'b0};
	assign srl_middle = {31'b0,B_reg};
	assign sra_middle = {31'hffffffff,B_reg};
	assign sll = sll_middle[62 - shift_amount -: 32];
	assign sra = sra_middle[31 + shift_amount -: 32];
	assign srl = srl_middle[31 + shift_amount -: 32];
	assign sra_result = (B_reg[31])?sra:srl;

	always @(*)
	begin
		if(instruction[31:26]==6'b000000 && (instruction[5:0]==6'b000100||instruction[5:0]==6'b000111||instruction[5:0]==6'b000110))
		begin
			shift_amount_reg<= A_reg[4:0];
		end
		else
		begin
			shift_amount_reg<= instruction[10:6];
		end
	end
	assign shift_amount = shift_amount_reg;

endmodule
