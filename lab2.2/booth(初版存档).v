module simple_alu(   
    input  mul_clk,   
    input  rst,   
    input  mul_sign,   
    input [31:0] x,
    input [31:0] y,
    output [63:0] result,
    ); 

    wire [32:0] A;
    wire [32:0] B;
    assign A = mul_sign ? {x[31],x} : {1'b0,x};
    assign B = mul_sign ? {y[31],y} : {1'b0,y};

    wire [32:0]B_neg;
    assign B_neg = ~B;

    wire [63:0] booth_result01;
    wire [63:0] booth_result02;
    wire [63:0] booth_result03;
    wire [63:0] booth_result04;
    wire [63:0] booth_result05;
    wire [63:0] booth_result06;
    wire [63:0] booth_result07;
    wire [63:0] booth_result08;
    wire [63:0] booth_result09;
    wire [63:0] booth_result10;
    wire [63:0] booth_result11;
    wire [63:0] booth_result12;
    wire [63:0] booth_result13;
    wire [63:0] booth_result14;
    wire [63:0] booth_result15;
    wire [63:0] booth_result16;
    wire [63:0] booth_result17;

    wire [32:0] booth_choose01;
    wire [32:0] booth_choose02;
    wire [32:0] booth_choose03;
    wire [32:0] booth_choose04;
    wire [32:0] booth_choose05;
    wire [32:0] booth_choose06;
    wire [32:0] booth_choose07;
    wire [32:0] booth_choose08;
    wire [32:0] booth_choose09;
    wire [32:0] booth_choose10;
    wire [32:0] booth_choose11;
    wire [32:0] booth_choose12;
    wire [32:0] booth_choose13;
    wire [32:0] booth_choose14;
    wire [32:0] booth_choose15;
    wire [32:0] booth_choose16;
    wire [32:0] booth_choose17;

    //
    wire s1_neg1;
    wire s1_pos1;
    wire s1_neg2;
    wire s1_pos2;
    wire s1_zero;

    assign s1_neg1 = !((!(B_neg[0]&B[1]&B[2]))&(!(B[0] & B_neg[1] & B[2]));
    assign s1_pos1 = !((!(B_neg[0]&B[1]&B_neg[2]))&(!(B[0] & B_neg[1] & B_neg[2]));
    assign s1_neg2 = !(!(B_neg[0]&B_neg[1]&B[2]));
    assign s1_pos2 = !(!(B[0]&B[1]&B_neg[2]));
    assign s1_zero = !(s1_neg1 | s1_pos1 | s1_neg2 | s1_pos2);

    assign booth_choose01 =({33{s1_neg1}} & !A) |
                            ({33{s1_pos1}} & A) |
                            ({33{s1_neg2}} & {!A, 1'b1}) |  //////////////????????????????
                            ({33{s1_pos2}} & {A, 1'b0}) |   /////////////?????????????
                            ({33{s1_zero}} & 33'b0);
                        
    assign booth_result01 = {{31{booth_choose01[32]}},booth_choose01};



    


                    
endmodule 
 