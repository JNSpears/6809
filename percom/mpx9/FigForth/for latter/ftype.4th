$FD58 CONSTANT OUTCH

CODE FTYPE                      ( ADDR, COUNT ... )
    D. Y. PSHS,  X. Y. PULU,    ( X=COUNT, Y=ADDR )
    0,X LEAX,  NE IF,
        X. D. TFR,  OUT ADDD,  OUT STD,    ( OUT:=OUT+COUNT )
        BEGIN,
            ,Y+ LDA,  OUTCH JSR,  ,-X LEAX,
            EQ UNTIL,
        THEN,
    D. Y. PULS,  NEXT,

' FTYPE CFA ' TYPE  CFA  !
