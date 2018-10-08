    wire lw_choke;
	assign lw_choke = ((((raddr1 == waddr_dst2) && (raddr1 != 5'b0)) ? 1'b1:1'b0) && (pipe2_data[31:26] == `LW) && reg_write2 && !counter_forwarding ) ||
					  ((((raddr2 == waddr_dst2) && (raddr2 != 5'b0)) ? 1'b1:1'b0) && (pipe2_data[31:26] == `LW) && (pipe2_data_in[31:26] == 6'b0 || pipe2_data_in[31:26] == `BNE || pipe2_data_in[31:26] == `BEQ || pipe2_data_in[31:26] == `SW) && reg_write2 && !counter_forwarding);


	reg [1:0]counter_lw_choke;
	always@(posedge clk)
	begin
		if(rst )
		begin
			counter_lw_choke <= 2'b0;
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