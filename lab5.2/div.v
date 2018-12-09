module div(     
    input div_clk,
    input resetn,
    input div,
    input div_signed,
    input [31:0] x,
    input [31:0] y,
    input choke,
    output [31:0] s,
    output [31:0] r,
    output complete
    ); 

    wire s_sign;
    wire r_sign;
    assign s_sign = (x[31] ^ y[31])&& div_signed;
    assign r_sign = x[31] && div_signed;


    wire [31:0] x_unsigned;
    wire [31:0] y_unsigned;
    assign x_unsigned = x[31] ? (~x + 32'b1) : x;
    assign y_unsigned = y[31] ? (~y + 32'b1) : y;

    wire [31:0] A;
    wire [31:0] B;
    assign A = (div_signed) ? x_unsigned : x;
    assign B = (div_signed) ? y_unsigned : y;
 
    wire [63:0] A_extend;
    wire [32:0] B_extend;
    assign A_extend = {32'b0,A};
    assign B_extend = {1'b0,B};

    reg [63:0] A_reg;
    
    
    reg [31:0] s_reg;
    reg [31:0] r_reg;
    wire[32:0] middle;
    reg [5:0] count;
    
    always @(posedge div_clk)
    begin
        if(!resetn)
        begin
            A_reg <= 64'b0;
        end
        else if (count == 6'd63)
        begin
            A_reg <= A_extend;
        end
        else if (count == 6'd0)
        begin
            if (! middle[32])
            begin
                A_reg[63:31] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd1)
        begin
            if (! middle[32])
            begin
                A_reg[62:30] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd2)
        begin
            if (! middle[32])
            begin
                A_reg[61:29] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd3)
        begin
            if (! middle[32])
            begin
                A_reg[60:28] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd4)
        begin
            if (! middle[32])
            begin
                A_reg[59:27] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd5)
        begin
            if (! middle[32])
            begin
                A_reg[58:26] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd6)
        begin
            if (! middle[32])
            begin
                A_reg[57:25] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd7)
        begin
            if (! middle[32])
            begin
                A_reg[56:24] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd8)
        begin
            if (! middle[32])
            begin
                A_reg[55:23] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd9)
        begin
            if (! middle[32])
            begin
                A_reg[54:22] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd10)
        begin
            if (! middle[32])
            begin
                A_reg[53:21] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd11)
        begin
            if (! middle[32])
            begin
                A_reg[52:20] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd12)
        begin
            if (! middle[32])
            begin
                A_reg[51:19] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd13)
        begin
            if (! middle[32])
            begin
                A_reg[50:18] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd14)
        begin
            if (! middle[32])
            begin
                A_reg[49:17] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd15)
        begin
            if (! middle[32])
            begin
                A_reg[48:16] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd16)
        begin
            if (! middle[32])
            begin
                A_reg[47:15] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd17)
        begin
            if (! middle[32])
            begin
                A_reg[46:14] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd18)
        begin
            if (! middle[32])
            begin
                A_reg[45:13] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd19)
        begin
            if (! middle[32])
            begin
                A_reg[44:12] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd20)
        begin
            if (! middle[32])
            begin
                A_reg[43:11] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd21)
        begin
            if (! middle[32])
            begin
                A_reg[42:10] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd22)
        begin
            if (! middle[32])
            begin
                A_reg[41:9] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd23)
        begin
            if (! middle[32])
            begin
                A_reg[40:8] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd24)
        begin
            if (! middle[32])
            begin
                A_reg[39:7] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd25)
        begin
            if (! middle[32])
            begin
                A_reg[38:6] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd26)
        begin
            if (! middle[32])
            begin
                A_reg[37:5] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd27)
        begin
            if (! middle[32])
            begin
                A_reg[36:4] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd28)
        begin
            if (! middle[32])
            begin
                A_reg[35:3] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd29)
        begin
            if (! middle[32])
            begin
                A_reg[34:2] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd30)
        begin
            if (! middle[32])
            begin
                A_reg[33:1] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else if (count == 6'd31)
        begin
            if (! middle[32])
            begin
                A_reg[32:0] <= middle;
            end
            else
            begin
                A_reg <= A_reg;
            end
        end
        else
        begin
            A_reg <= A_reg;
        end
    end

    always @(posedge div_clk)
    begin
        if(!resetn)
        begin
            count <= 6'd63;
        end
        else if (choke)
        begin
            count <= 6'd62;
        end
        else if (div)
        begin
            if(count >= 6'd32 && count < 6'd63)
            begin
                count <= 6'd63;
            end
            else
            begin
                count <= count + 6'd1;
            end
        end
        else if (count == 6'd0)
        begin
            count <= 6'd63;
        end
        else
        begin
            count <= count;
        end
    end

    wire [32:0] A_input;
    wire [32:0] B_input;
    assign B_input = ~B_extend + 32'b1;
    assign A_input = ({33{count == 6'd0}} & A_reg[63:31]) |
                     ({33{count == 6'd1}} & A_reg[62:30]) |
                     ({33{count == 6'd2}} & A_reg[61:29]) |
                     ({33{count == 6'd3}} & A_reg[60:28]) |
                     ({33{count == 6'd4}} & A_reg[59:27]) |
                     ({33{count == 6'd5}} & A_reg[58:26]) |
                     ({33{count == 6'd6}} & A_reg[57:25]) |
                     ({33{count == 6'd7}} & A_reg[56:24]) |
                     ({33{count == 6'd8}} & A_reg[55:23]) |
                     ({33{count == 6'd9}} & A_reg[54:22]) |
                     ({33{count == 6'd10}} & A_reg[53:21]) |
                     ({33{count == 6'd11}} & A_reg[52:20]) |
                     ({33{count == 6'd12}} & A_reg[51:19]) |
                     ({33{count == 6'd13}} & A_reg[50:18]) |
                     ({33{count == 6'd14}} & A_reg[49:17]) |
                     ({33{count == 6'd15}} & A_reg[48:16]) |
                     ({33{count == 6'd16}} & A_reg[47:15]) |
                     ({33{count == 6'd17}} & A_reg[46:14]) |
                     ({33{count == 6'd18}} & A_reg[45:13]) |
                     ({33{count == 6'd19}} & A_reg[44:12]) |
                     ({33{count == 6'd20}} & A_reg[43:11]) |
                     ({33{count == 6'd21}} & A_reg[42:10]) |
                     ({33{count == 6'd22}} & A_reg[41:9]) |
                     ({33{count == 6'd23}} & A_reg[40:8]) |
                     ({33{count == 6'd24}} & A_reg[39:7]) |
                     ({33{count == 6'd25}} & A_reg[38:6]) |
                     ({33{count == 6'd26}} & A_reg[37:5]) |
                     ({33{count == 6'd27}} & A_reg[36:4]) |
                     ({33{count == 6'd28}} & A_reg[35:3]) |
                     ({33{count == 6'd29}} & A_reg[34:2]) |
                     ({33{count == 6'd30}} & A_reg[33:1]) |
                     ({33{count == 6'd31}} & A_reg[32:0]) ;

    
    assign middle = A_input + B_input;

    

    always @(posedge div_clk)
	begin
        if(!resetn)
        begin
            s_reg <= 32'b0;
        end
        else if(count == 6'd63)
        begin 
            s_reg <= 32'b0;
        end
        else if (count == 6'd0)
        begin
            s_reg[31] <= ! middle[32];
        end
        else if (count == 6'd1)
        begin
            s_reg[30] <= ! middle[32];
        end
        else if (count == 6'd2)
        begin
            s_reg[29] <= ! middle[32];
        end
        else if (count == 6'd3)
        begin
            s_reg[28] <= ! middle[32];
        end
        else if (count == 6'd4)
        begin
            s_reg[27] <= ! middle[32];
        end
        else if (count == 6'd5)
        begin
            s_reg[26] <= ! middle[32];
        end
        else if (count == 6'd6)
        begin
            s_reg[25] <= ! middle[32];
        end
        else if (count == 6'd7)
        begin
            s_reg[24] <= ! middle[32];
        end
        else if (count == 6'd8)
        begin
            s_reg[23] <= ! middle[32];
        end
        else if (count == 6'd9)
        begin
            s_reg[22] <= ! middle[32];
        end
        else if (count == 6'd10)
        begin
            s_reg[21] <= ! middle[32];
        end
        else if (count == 6'd11)
        begin
            s_reg[20] <= ! middle[32];
        end
        else if (count == 6'd12)
        begin
            s_reg[19] <= ! middle[32];
        end
        else if (count == 6'd13)
        begin
            s_reg[18] <= ! middle[32];
        end
        else if (count == 6'd14)
        begin
            s_reg[17] <= ! middle[32];
        end
        else if (count == 6'd15)
        begin
            s_reg[16] <= ! middle[32];
        end
        else if (count == 6'd16)
        begin
            s_reg[15] <= ! middle[32];
        end
        else if (count == 6'd17)
        begin
            s_reg[14] <= ! middle[32];
        end
        else if (count == 6'd18)
        begin
            s_reg[13] <= ! middle[32];
        end
        else if (count == 6'd19)
        begin
            s_reg[12] <= ! middle[32];
        end
        else if (count == 6'd20)
        begin
            s_reg[11] <= ! middle[32];
        end
        else if (count == 6'd21)
        begin
            s_reg[10] <= ! middle[32];
        end
        else if (count == 6'd22)
        begin
            s_reg[9] <= ! middle[32];
        end
        else if (count == 6'd23)
        begin
            s_reg[8] <= ! middle[32];
        end
        else if (count == 6'd24)
        begin
            s_reg[7] <= ! middle[32];
        end
        else if (count == 6'd25)
        begin
            s_reg[6] <= ! middle[32];
        end
        else if (count == 6'd26)
        begin
            s_reg[5] <= ! middle[32];
        end
        else if (count == 6'd27)
        begin
            s_reg[4] <= ! middle[32];
        end
        else if (count == 6'd28)
        begin
            s_reg[3] <= ! middle[32];
        end
        else if (count == 6'd29)
        begin
            s_reg[2] <= ! middle[32];
        end
        else if (count == 6'd30)
        begin
            s_reg[1] <= ! middle[32];
        end
        else if (count == 6'd31)
        begin
            s_reg[0] <= ! middle[32];
        end
    end

    assign s = s_sign ? ~s_reg + 32'b1 : s_reg;
    assign r = r_sign ? ~A_reg[31:0] + 32'b1 : A_reg[31:0];

    assign complete = (count == 6'd32) ? 1'b1 : 1'b0; /////////////?????????????????
                    
endmodule 
 