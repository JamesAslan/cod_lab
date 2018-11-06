wire[31:0] swl_data;
wire[31:0] swr_data;
wire[31:0] sb_data;
wire[31:0] sh_data;

assign swl_data = 	({32{ALU_out3[1:0] == 2'b00}} & {24'b0,Data[31:24]}) |
					({32{ALU_out3[1:0] == 2'b01}} & {16'b0,Data[31:16]}) |
					({32{ALU_out3[1:0] == 2'b10}} & {8'b0,Data[31:8]}) |
					({32{ALU_out3[1:0] == 2'b11}} & Data) ;

assign swr_data =	({32{ALU_out3[1:0] == 2'b00}} & Data) |
					({32{ALU_out3[1:0] == 2'b01}} & {Data[23:0],8'b0}) |
					({32{ALU_out3[1:0] == 2'b10}} & {Data[15:0],16'b0}) |
					({32{ALU_out3[1:0] == 2'b11}} & {Data[7:0],24'b0}) ;	

assign sb_data = 	({32{ALU_out3[1:0] == 2'b00}} & Data) |
					({32{ALU_out3[1:0] == 2'b01}} & {Data[23:0],8'b0}) |
					({32{ALU_out3[1:0] == 2'b10}} & {Data[15:0],16'b0}) |
					({32{ALU_out3[1:0] == 2'b11}} & {Data[7:0],24'b0}) ;

assign sh_data =    ({32{ALU_out3[1]}} & Data) |
					({32{ALU_out3[1]}} & {Data[15:0],16'b0});		
