( Misc tools.  JNS 1982-2014 )
: HERE?
  ." SCR=" BLK @ 4 / . ." DP=$" BASE @ HEX HERE . BASE ! CR ;

: DEPTH  ( ... N )
    S0 @ SP@ - 2 / 1- ;

: .STACK ( PRINT STACK )
    DEPTH 0 < IF ." Stack underflow" QUIT THEN
    DEPTH 0= IF ." Stack empty" QUIT THEN
    DEPTH 0 DO I 1+ 2 * SP@ + @ . LOOP  ;

( UTILITIES - OVERLAY                             JNS 08/28/83 )
( VLIST SHORT VERSION, STOPS AT VOCABULARY HEADER )

CODE BSWAP ( BYTE SWAP TOS; AABB ... BBAA )
    0,U  LDD,  .A .B EXG,  0,U STD,  NEXT,

CODE 1+!  ( INCR WORD AT ADDR; ADDR ... )
    ,U++ LDX,  0,X LDD,  1 ## ADDD,  0,X STD,  NEXT,

CODE 1-!  ( DECR WORD AT ADDR; ADDR ... )
    ,U++ LDX,  0,X LDD,  1 ## SUBD,  0,X STD,  NEXT,

6400 CONSTANT %DELAY

: 1SEC.DELAY ( ... )
    %DELAY BEGIN 1- DUP 0= UNTIL DROP ;

: SEC.DELAY ( #SECONDS ... )
    BEGIN DUP WHILE 1- 1SEC.DELAY REPEAT DROP ;

: CASE  ?COMP CSP @ !CSP 4 ; IMMEDIATE

: OF 4 ?PAIRS COMPILE OVER COMPILE = COMPILE 0BRANCH
    HERE 0 , COMPILE DROP 5 ; IMMEDIATE

: ENDOF 5 ?PAIRS COMPILE BRANCH HERE 0 ,
    SWAP 2 [COMPILE] THEN 4 ; IMMEDIATE

: ENDCASE 4 ?PAIRS COMPILE DROP BEGIN SP@
    CSP @ = 0= WHILE 2 [COMPILE] THEN REPEAT
    CSP ! ; IMMEDIATE

( FORTH EXTENSIONS                                JNS 03/23/84 )
: \ ( END OF LINE COMMENT )
    IN @ 64  MOD MINUS 64  1- + IN +! ; IMMEDIATE

: ASCII ( CREATE ASCII CHARACTER LITERAL:  ASCII A )
    BL WORD HERE 1+ C@ [COMPILE] LITERAL ; IMMEDIATE

: CTRL ( CREATE CONTROL CHARACTER LITERAL:  CTRL X )
    BL WORD HERE 1+ C@ 64 - [COMPILE] LITERAL ; IMMEDIATE

: CUPSHIFT ( C ... C' )
    DUP ASCII ` > OVER ASCII { < AND
    IF $20 - THEN ;

: UPSHIFT    ( ADDR COUNT ... )
    OVER + SWAP DO
        I C@ $60 > IF I C@ 32 - I C! THEN  LOOP ;

: C+! ( ADD VALUE TO CHAR; N ADDR ... )
    DUP C@ ROT + SWAP C! ;

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
: 2CONSTANT <BUILDS HERE 2! 4 ALLOT DOES> 2@ ;

: W    WORD HERE COUNT UPSHIFT ;
( ' W CFA  ' -FIND 2+ !  SMUDGE )

CODE (-TEXT)    ( ADDR1 ADDR2 COUNT ... T/F )
    .Y PSHS,  .D .X .Y PULU,  ,-U CLR,  ,-U CLR,
    27 MPX,  EQ IF,  0,U INC,  THEN,  .Y PULS,  NEXT,

( DEBUGGING TOOLS: BREAK & GO                  FORTH DIM. V5#1 )
  0 VARIABLE CHECK      ( CHECKS IF RSTACK CHANGED SINCE BREAK )
                               ( CONTAINS RP@ AT TIME OF BREAK )
CODE RP@   ( ... N )  ( LEAVE RETURN STACK ADDRESS ON STACK )
    .S PSHU,  NEXT,

: CAN     ( CANCEL )                                            
    ." ***"  CR  ; 

: BREAK    ( ... )  ( COMPILE BREAK INTO : DEF )
    CAN
    RP@ 4 - CHECK !  0 BLK !
    BEGIN  QUERY  INTERPRET  ." AOK" CR  AGAIN  ;

: GO       ( ... )  ( ENTER GO TO RESUME )
    RP@  CHECK  @  -
         IF  ." CAN'T RESUME" QUIT THEN  R> DROP R> DROP  ;

: U.       ( UNSIGNED DOT )
    0  FORTH  D.  ASSEMBLER ;
(  ' U.  CFA  $16ED !    ( PATCH DUMP )


CODE U<    ( UNSIGNED LESS THAN )
    ,U++ LDD,  0,U CMPD,
    LS IF,  0 ## LDD,  ELSE,  1 ## LDD,  THEN,
    0,U STD,  NEXT,

123 123 < . 123 123 U< . 
123 222 < . 123 222 U< . 
123 111 < . 123 111 U< . CR
-123 -123 < . -123 -123 U< . 
-123 -222 < . -123 -222 U< . 
-123 -111 < . -123 -111 U< . CR
-123  123 < . -123  123 U< . 
-123  222 < . -123  222 U< . 
-123  111 < . -123  111 U< . CR


: U> SWAP U< ;

: SCALERS 0 DO I CONSTANT LOOP ;

2 SCALERS FALSE TRUE  ( DEFINE STANDARD TRUE=1 FALSE=0 )

: FLAG  ( CREATE A FLAG )
    FALSE VARIABLE ;

: ON   TRUE  SWAP C! ;
: OFF  FALSE SWAP C! ;

: ON?  C@  ;
: OFF? C@ 0= ;

( UTILITIES - OVERLAY                             JNS 08/28/83 )
( OUTLINE + DOCUMENTATION TOOLS  )

: 1OUTLINE    ( OUTLINE ONE SCREEN; N ... )
    DUP  CR  ." SCREEN# "  .  CR
    16  0  DO  I  OVER  (LINE)  OVER  C@  BL  -
             IF  3  SPACES  -TRAILING  TYPE  CR
                 ELSE  DROP  DROP  THEN
             LOOP  DROP  ;

: OUTLINE     ( OUTLINE SEVERAL SCREENS; FROM TO ... )
    1+  SWAP  CR  DO  I  1OUTLINE  LOOP  ;

: TRIAD                                                         
    3 / 3 * 3 OVER + SWAP                      
    DO  CR  I  LIST  LOOP  CR ;

: WORDS
    CR CONTEXT  @  @  
    BEGIN
      DUP  C@  $20  AND  0=
          ( IF DUP ID. OUT @  -1 XOR 15 AND  SPACES THEN )
          IF ID. SPACE THEN
      OUT  @  $40  >  IF  CR  THEN
      PFA  LFA  @  DUP  CONTEXT  @  U<  ?TERMINAL  OR  UNTIL
    CR  DROP  
    ." CURRENT = "  CURRENT  @  6  -  NFA  ID.  CR  
    ." CONTEXT = "  CONTEXT  @  6  -  NFA  ID.  CR  ;

( UTILITIES - OVERLAY                             JNS 08/28/83 )

: DICT-INFO  ( PRINT;  NFA  LENGTH-BYTE  NAME  )
  HEX  CONTEXT  @  @  CR
  BEGIN  DUP  U.  DUP  C@  U.  DUP  ID.  CR
         PFA  LFA  @
         DUP  0=  ?TERMINAL  OR  UNTIL  DROP  ;

: .BASE
    BASE @ DUP DECIMAL . BASE ! ;

 ;S ( STOP HERE FOR NOW... )
