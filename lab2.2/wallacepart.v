module wallacepart(     
    input A,
    input B,
    input C,
    output cout,
    output S
    ); 

    assign S = C ? !(A ^ B) : A ^ B;
    assign cout = (A && B) || (A && C) || (B && C);
                    
endmodule 
 