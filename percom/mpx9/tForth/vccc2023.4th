4 :BUFFER CARROT
2 BASE !
0001000 CARROT C! 0010100 CARROT 1+ C!
0100010 CARROT 2+ C! 1000001 CARROT 2+ 1+ C!
HEX
: BAR ( PAT N -- )
  0 DO 
    DUP 1 AND IF ASCII * ELSE BL THEN EMIT /2
    LOOP DROP ;
: FIE CARROT + C@ SWAP BAR ;
: FOO 
  4 0 DO 6 I FIE 6 I FIE 7 I FIE CR LOOP 
  0 2 DO 6 I FIE 6 I FIE 7 I FIE CR -1 +LOOP ; 
: VCCC2023
  CR FOO FOO FOO 
  6 0 FIE 6 0 FIE 6 0 FIE CR ;             ;S