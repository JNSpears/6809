( COMMON HANDY THINGS TO FINISH FORTH - jns 03/23/84 )
." LOADING COMMON HANDY THINGS TO FINISH FORTH " CR
: ASCII ( create ascii character literal:  ascii A )             
    BL WORD HERE 1+ C@ [COMPILE] LITERAL ; IMMEDIATE            
: CTRL ( create control character literal:  ctrl x )          
    BL WORD HERE 1+ C@ ASCII @ - [COMPILE] LITERAL ; IMMEDIATE     

: SCALERS 0 DO I CONSTANT LOOP ;
2 SCALERS FALSE TRUE  ( DEFINE STANDARD TRUE=1 FALSE=0 )
: FLAG  ( CREATE A FLAG )
    FALSE VARIABLE ;
: ON   TRUE  SWAP C! ;  ( SET FLAG; ADDR -- )
: OFF  FALSE SWAP C! ;  ( CLEAR FLAG; ADDR -- )
: ON?  C@  ;            ( IS FLAG SET; ADDR -- F )  
: OFF? C@ 0= ;          ( IS FLAG CLEAR; ADDR -- F )
                                                        -->
: \ ( END OF LINE COMMENT )
    IN @ 64  MOD MINUS 64  1- + IN +! ; IMMEDIATE

: NOT 0= ;

CODE U<    ( UNSIGNED LESS THAN )
    ,U++ LDD,  0,U CMPD,
    .LO. IF,  0 ## LDD,  ELSE,  1 ## LDD,  THEN,
    0,U STD,  NEXT,

CODE U>    ( UNSIGNED GREATER THAN )
    ,U++ LDD,  0,U CMPD,
    .HI. IF,  0 ## LDD,  ELSE,  1 ## LDD,  THEN,
    0,U STD,  NEXT,

                                                        -->
: CASE  ?COMP CSP @ !CSP 4 ; IMMEDIATE                          
                                                                
: OF 4 ?PAIRS COMPILE OVER COMPILE = COMPILE 0BRANCH            
    HERE 0 , COMPILE DROP 5 ; IMMEDIATE                         
                                                                
: ENDOF 5 ?PAIRS COMPILE BRANCH HERE 0 ,                        
    SWAP 2 [COMPILE] THEN 4 ; IMMEDIATE                         
                                                                
: ENDCASE 4 ?PAIRS COMPILE DROP BEGIN SP@                       
    CSP @ = 0= WHILE 2 [COMPILE] THEN REPEAT                    
    CSP ! ; IMMEDIATE                                           
: RNG   ( N HI LO .. N F )
  ROT DUP >R DUP ROT U> ROT ROT U> AND R> SWAP  ;
: RNG.OF 4 ?PAIRS COMPILE RNG COMPILE 0BRANCH 
    HERE 0 , COMPILE DROP 5 ; IMMEDIATE        -->

: 2@ ( DOUBLE FETCH; ADDR ... D )
    DUP @ SWAP @ ;
: 2! ( DOUBLE STORE; D ADDR ... )
    DUP >R ! R> ! ;
: 2DROP  DROP  DROP  ;
: 2DUP   OVER  OVER  ;
: 2SWAP  ROT  >R  ROT  R>  ;
: 2OVER ( dl d2 - dl d2 dl ) >R >R 2DUP R> R> 2SWAP ;
: 2ROT ( dl d2 d3 - d2 d3 dl ) >R >R 2SWAP R> R> 2SWAP ;

: 2VARIABLE VARIABLE 0 , ;
: 2CONSTANT <BUILDS HERE 2! 4 ALLOT DOES> 2@ ; -->




: DEPTH  ( ... N )
    S0 @ SP@ - 2 / 1- ;
: .STK ( PRINT STACK )
    DEPTH 0 < IF ." Stack underflow" CR QUIT THEN
    DEPTH 0= IF ." Stack empty" ELSE 
        DEPTH 0 DO I 1+ 2 * SP@ + @ . LOOP THEN CR  ;
CODE U>    ( UNSIGNED GREATER THAN )
    ,U++ LDD,  0,U CMPD,
    .HI. IF,  0 ## LDD,  ELSE,  1 ## LDD,  THEN,
    0,U STD,  NEXT,

CODE /2 ( N .. N/2 )
    0,U LDD, ASRA, RORB, 0,U STD, NEXT,
CODE *2 ( N .. N/2 )
    0,U LDD, ASLB, ROLA, 0,U STD, NEXT,
;S