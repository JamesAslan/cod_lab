assign jal 	= 	(pipe1_data[31:26] == `JAL );
	assign jr 	= 	(pipe1_data[31:26] == 6'b0 && pipe1_data[5:0] == `JR );
	assign j 	= 	(pipe1_data[31:26] == `J );
	assign jalr = 	(pipe1_data[31:26] == 6'b0 && pipe1_data[20:16] == 5'b0 && pipe1_data[5:0] == `JALR );
	assign jump_or_not = jal || jr || j || jalr;
	
	wire[31:0] jump_target;
	assign jump_target = 	({32{jal || j }} & {pipe2_PC[31:28],pipe1_data[25:0],2'b0}) |
							({32{jr || jalr}} & branch_A_input) ;