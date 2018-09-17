module reg_a(
    input clk,
    input rst,
    input en,
    input din,
    output reg q
);

always @(posedge clk)
begin
    if(rst) q<= 1'b0;
    else if(en) q<= din;
    else q<=q;
end

endmodule