module boothpart(     
    input [63:0] A,
    input y0,
    input y1,
    input y2,
    output [63:0] result,
    output C
    ); 

    //wire [32:0] booth_choose;

    ///////
    wire s_neg1;
    wire s_pos1;
    wire s_neg2;
    wire s_pos2;
    wire s_zero;

    assign s_neg1 = !((!(!y0 & y1 & y2))&(!(y0 & !y1 & y2)));
    assign s_pos1 = !((!(!y0 & y1 & !y2))&(!(y0 & !y1 & !y2)));
    assign s_neg2 = !(!(!y0 & !y1 & y2));
    assign s_pos2 = !(!(y0 & y1 & !y2));
    assign s_zero = !(s_neg1 | s_pos1 | s_neg2 | s_pos2);

    assign result =({64{s_neg1}} & ~A) |
                            ({64{s_pos1}} & A) |
                            ({64{s_neg2}} & {~A, 1'b1}) |  //////////////????????????????
                            ({64{s_pos2}} & {A, 1'b0}) |   /////////////?????????????
                            ({64{s_zero}} & 64'b0);

    assign C = s_neg1 || s_neg2;



    


                    
endmodule 
 