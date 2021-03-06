module alu(   
    input  [11:0] alu_control,   
    input  [31:0] alu_A,   
    input  [31:0] alu_B,   
    output [31:0] alu_result,
    output overflow,
    output carryout,
    output zero
     ); 
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

assign op_add  = alu_control[11]; 
assign op_sub  = alu_control[10]; 
assign op_slt  = alu_control[ 9]; 
assign op_sltu = alu_control[ 8]; 
assign op_and  = alu_control[ 7]; 
assign op_nor  = alu_control[ 6]; 
assign op_or   = alu_control[ 5]; 
assign op_xor  = alu_control[ 4]; 
assign op_sll  = alu_control[ 3]; 
assign op_srl  = alu_control[ 2]; 
assign op_sra  = alu_control[ 1]; 
assign op_lui  = alu_control[ 0]; 
 
wire [31:0] add_sub_result;  
wire [31:0] slt_result;  
wire [31:0] sltu_result; 
wire [31:0] and_result; 
wire [31:0] nor_result; 
wire [31:0] or_result; 
wire [31:0] xor_result; 
wire [31:0] sll_result;  
wire [63:0] sr64_result; 
wire [31:0] sr_result;  
wire [31:0] lui_result;  
 
assign and_result = alu_A & alu_B; 
assign or_result  = alu_A | alu_B; 
assign nor_result = ~or_result; 
assign xor_result = alu_A ^ alu_B; 
assign lui_result = {alu_B[15:0], 16'b0}; 
 
wire [31:0] adder_a; 
wire [31:0] adder_b; 
wire        adder_cin; 
wire [31:0] adder_result; 
wire        adder_cout; 
assign adder_a   = alu_A; 
assign adder_b   = alu_B ^ {32{op_sub | op_slt | op_sltu}}; 
assign adder_cin = op_sub | op_slt | op_sltu; 
assign {adder_cout, adder_result} = adder_a + adder_b + adder_cin; 
 
assign add_sub_result = adder_result; 
assign slt_result[31:1] = 31'b0; 
assign slt_result[0]    = (alu_A[31] & ~alu_B[31])                         
                        | (~(alu_A[31]^alu_B[31]) & adder_result[31]); 
 
assign sltu_result[31:1] = 31'b0; 
assign sltu_result[0]    = ~adder_cout; 
 
assign sll_result = alu_B << alu_A[4:0]; 
 
assign sr64_result = {{32{op_sra&alu_B[31]}}, alu_B[31:0]}>>alu_A[4:0]; 
assign sr_result   = sr64_result[31:0]; 
 
assign alu_result = ({32{op_add|op_sub }} & add_sub_result)                   
                    | ({32{op_slt        }} & slt_result)                   
                    | ({32{op_sltu       }} & sltu_result)                   
                    | ({32{op_and        }} & and_result)                   
                    | ({32{op_nor        }} & nor_result)                   
                    | ({32{op_or         }} & or_result)                   
                    | ({32{op_xor        }} & xor_result)                   
                    | ({32{op_sll        }} & sll_result)                   
                    | ({32{op_srl|op_sra }} & sr_result)                   
                    | ({32{op_lui        }} & lui_result); 

assign zero = (alu_result == 32'b0) ? 1'b1 : 1'b0;
assign carryout = adder_cout;
assign overflow = (adder_a[31]^~adder_b[31]) && (adder_a[31]^adder_result[31]);

                    
endmodule 
 