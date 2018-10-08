module wallace(
    input[16:0] w,     
    input[13:0] cin,
    output[13:0] cout,
    output C,
    output S
    ); 

    wire S0;
    wire S1;
    wire S2;
    wire S3;
    wire S4;
    wire S5;
    wire S6;
    wire S7;
    wire S8;
    wire S9;
    wire S10;
    wire S11;
    wire S12;
    wire S13;

    //lace1
    wallacepart wallace01(w[2],w[3],w[4],cout[0],S0);
    wallacepart wallace02(w[5],w[6],w[7],cout[1],S1);
    wallacepart wallace03(w[8],w[9],w[10],cout[2],S2);
    wallacepart wallace04(w[11],w[12],w[13],cout[3],S3);
    wallacepart wallace05(w[14],w[15],w[16],cout[4],S4);
    //lace2
    wallacepart wallace06(cin[0],cin[1],cin[2],cout[5],S5);
    wallacepart wallace07(cin[3],cin[4],w[0],cout[6],S6);
    wallacepart wallace08(w[1],S0,S1,cout[7],S7);
    wallacepart wallace09(S2,S3,S4,cout[8],S8);
    //LACE3
    wallacepart wallace10(cin[5],cin[6],S5,cout[9],S9);
    wallacepart wallace11(S6,S7,S8,cout[10],S10);
    //lace4
    wallacepart wallace12(cin[7],cin[8],cin[9],cout[11],S11);
    wallacepart wallace13(cin[10],S9,S10,cout[12],S12);
    //lace5
    wallacepart wallace14(cin[11],S11,S12,cout[13],S13);
    //lace6
    wallacepart wallace15(cin[12],cin[13],S13,C,S);
                    
endmodule 