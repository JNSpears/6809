( VEDIT )   

: NOT 0= ;

: DEPTH  ( ... N )
    S0 @ SP@ - 2 / 1- ;

: .STACK ( PRINT STACK )
    DEPTH 0 < IF ." Stack underflow" CR QUIT THEN
    DEPTH 0= IF ." Stack empty" ELSE 
    DEPTH 0 DO I 1+ 2 * SP@ + @ . LOOP THEN CR  ;

$40 CONSTANT C/L                  ( TEXT CHARACTERS PER LINE *)

CODE U<    ( UNSIGNED LESS THAN )
    ,U++ LDD,  0,U CMPD,
    LS IF,  0 ## LDD,  ELSE,  1 ## LDD,  THEN,
    0,U STD,  NEXT,

CODE U>    ( UNSIGNED GREATER THAN )
    ,U++ LDD,  0,U CMPD,
    GT IF,  0 ## LDD,  ELSE,  1 ## LDD,  THEN,
    0,U STD,  NEXT,

: 2DROP DROP DROP ; ( DROP DOUBLE NUMBER )

: 2DUP OVER OVER ; ( DUPLICATE A DOUBLE NUMBER )

: 2SWAP ROT >R ROT R> ;
       ( BRING SECOND DOUBLE TO TOP OF STACK )

: LINE        ( RELATIVE TO SCR, LEAVE ADDRESS OF LINE *)
      DUP $FFF0 AND $17 ?ERROR  ( KEEP ON THIS SCREEN )
      SCR @       (LINE)       DROP ;

: #LOCATE              ( LEAVE CURSOR OFFSET-2, LINE-1 *)
         R# @ C/L /MOD ;
: #LEAD          ( LINE ADDRESS-2, OFFSET-1 TO CURSOR *)
        #LOCATE LINE SWAP ;
: #LAG       ( CURSOR ADDRESS-2, COUNT-1 AFTER CURSOR *)
        #LEAD DUP >R + C/L R> - ;

(  2/TFORTH09  GO  FLOAD 2/VEDIT2.4T  )

5 :BUFFER BB 
0 VARIABLE AA 

: WKEY ( WAIT FOR MULTIPLE CHARACTER KEY INPUT: -- ADDR #CHARS )
    BB 5 0 FILL
    0 AA !  
    KEY BB AA @ + C!  1 AA +! 
    0 BEGIN
        1+ DUP 200 < 
        WHILE
            ?TERMINAL -DUP IF BB AA @ + C!  1 AA +! THEN
        REPEAT DROP BB AA @ ;

FLAG EXIT.FLAG                                                  
FLAG INS

VT100

27 CONSTANT <ESC>  CTRL S CONSTANT <KR>  $08 CONSTANT <BS>      
13 CONSTANT <CR>   CTRL T CONSTANT <KL>  $7F CONSTANT <DEL>     


: .STATUS.LINE ( N.SECONDS TEXT.ADDR ... )
    19 0 VT100 XY CLRL           
    COUNT TYPE                           
    4096 * BEGIN 1- ?TERMINAL OVER 0= OR UNTIL DROP             
    19 0 VT100 XY CLRL           
 ;                        


: PROCESS.FORTH.COMMAND  ( VIDEO   )                               
(    BOTTOMLINE@ PAD VC/L + SAVE.LINE )                           
(    XY@  15 BLANKLINE  HOMEDOWN       )                          
(    ." >>" TIB @ VC/L EXPECT           )                         
(    BOTTOMLINE@ PAD VC/L + RESTORE.LINE  XY! )                   
(    0 IN ! INTERPRET ) ;                                         

: VXY 5 3 D+ VT100 XY ; ( Y X -- )

: VTOP 0 0 VXY ;
: VBOTTOM 0 15 VXY ;

: VEND VT100 1 20 XY ;

: UPDATE.FULL ( SCR# -- )
    VT100  HOME  CLRS  LIST  5 " WELCOME" .STATUS.LINE VTOP ;

: UPDATE.EOS
    VT100   ;

: UPDATE.EOL ( LINE# -- )
    VT100 DUP 3 .R SPACE SCR @ .LINE ;

: VINIT 0 R# ! UPDATE.FULL ;

: ABC-DELETE.CH ( ... )                                          
    #LOCATE DROP C/L 1- = NOT         HEX        
    IF VEND #LAG          .STACK ( -- CUR@ TRAILING# ) 
        OVER 1+           .STACK ( CUR@ TRAILING# FROM@ )
        ROT  ROT 1-       .STACK ( FROM@ TO@ CNT# )
                        CR ." BEFORE CMOVE:" .STACK  
                        #LAG DUMP
        CMOVE  ( CMOVE: FROM-3, TO-2, QUAN-1 ... )
                        CR ." AFTER CMOVE:" .STACK    
        ( 1 R# +! )        
                        CR ." #LOCATE:" #LOCATE .STACK 
                        CR ." #LAG:" #LAG .STACK  
                        #LAG DUMP
        #LOCATE VXY   ( ." [[[[" )
        #LAG TYPE   ( ." {{{{" )
    ELSE
        10 " EB" .STATUS.LINE                                      
    THEN #LOCATE VXY ;                                                 

    ( GO FLOAD 2/VEDIT2.4T 7 VEDIT )

: BLANK-LAST-CH
    BL #LAG + 1- C! ;

: DELETE.CH ( ... )                                          
    #LOCATE DROP C/L 1- = NOT       
    IF VEND #LAG            ( -- CUR@ TRAILING# ) 
        OVER 1+             ( CUR@ TRAILING# FROM@ )
        ROT  ROT 1-         ( FROM@ TO@ CNT# )
        CMOVE               ( CMOVE: FROM-3, TO-2, QUAN-1 ... )
        #LOCATE VXY  
        BLANK-LAST-CH  
        #LAG TYPE 
    ELSE
        10 " EB" .STATUS.LINE                                      
    THEN #LOCATE VXY ;                   

: BACKSPACE.CH ( ... )                                          
    #LOCATE DROP IF #LAG    ( -- CUR@ TRAILING# ) 
        OVER                ( CUR@ TRAILING# FROM@ )
        ROT 1- ROT          ( FROM@ TO@ CNT# )
        CMOVE               ( CMOVE: FROM-3, TO-2, QUAN-1 ... )   
        -1 R# +!        
        #LOCATE VXY 
        BLANK-LAST-CH 
        #LAG TYPE 
    ELSE
        10 " BB" .STATUS.LINE                                      
    THEN #LOCATE VXY ;                                                 

CODE CMOVE>  ( c-addr1 c-addr2 u --   ; move from top 1->2 )    
    .X .Y    PSHS,
    .D .X .Y PULU, ( D:u X:dst Y:src )
    .U      PSHS,
    D,X     LEAX,
    D,Y     LEAY,
    .Y .U   TFR,
    .D .Y   TFR,       ( Y:u U:src' X:dst' )
    1 ,Y    LEAY,      ( Y:u' U:src' X:dst' )
    BEGIN,
        -1 ,Y LEAY, 
        NE WHILE,
            ,-U LDA,
            ,-X STA, 
    REPEAT,
    .U      PULS,
    .X .Y   PULS,
    NEXT,

: CH.CH 
   ( VEND .STACK)
    DUP ( C .. C C ) 
    #LAG DROP ( C C .. C C @ )
    INS ON? 
    IF                  
       #LAG 1- OVER 1+ ROT SWAP ROT ( VEND .STACK) CMOVE> ( #LOCATE VXY)
    THEN
    C! ( C C @ .. C ) 
    #LOCATE VXY 
    EMIT ( C .. )
    1 R# +!
    #LAG TYPE 
    #LOCATE VXY 
  ;

: RNG   ( N HI LO .. N F )
  ROT   ( .. HI LO N ) DUP >R
  SWAP  ( .. HI N LO )
  -     ( .. HI N-LO )
  U>    ( .. F )
  R> SWAP ( .. N F )
  ;

: RNG.OF 4 ?PAIRS COMPILE RNG COMPILE 0BRANCH 
    HERE 0 , COMPILE DROP 5 ; IMMEDIATE 

: VEDIT VINIT EXIT.FLAG OFF ( SCR# -- )                
    BEGIN VT100
      6 0 XY INS ON? IF ." INS" ELSE 3 SPACES THEN
      12 0 XY #LOCATE . . 
      #LOCATE VXY
      WKEY                  ( ADDR #CHAR )
      DUP
      CASE
        1 OF
            DROP C@ DUP                                                  
              CASE CTRL L OF DROP SCR @ UPDATE.FULL ENDOF                          
                  CTRL [  OF DROP EXIT.FLAG ON ENDOF                         
                  <BS>    OF DROP BACKSPACE.CH ENDOF                         
                  <DEL>   OF DROP DELETE.CH ENDOF                            
                  <ESC>   OF DROP PROCESS.FORTH.COMMAND ENDOF      
                  ASCII Z ASCII A RNG.OF CH.CH ENDOF          
                  DROP 5 " WHAT?" .STATUS.LINE                               
              ENDCASE 
            ENDOF                                                  
        3 OF 
            DROP DUP @ $1B5B = IF
              2 + C@  CASE
                ASCII A OF ( UP ) #LOCATE IF C/L MINUS R# +! VT100 UP THEN DROP ENDOF
                ASCII B OF ( DOWN ) #LOCATE 15 = NOT IF C/L R# +! VT100 DOWN THEN DROP ENDOF
                ASCII C OF ( RIGHT ) #LAG 1- IF  1 R# +! VT100 RIGHT THEN DROP ENDOF
                ASCII D OF ( LEFT ) #LEAD IF -1 R# +! VT100 LEFT  THEN DROP ENDOF
                ASCII H OF ( HOME )       0 R# ! VTOP    ENDOF
                ASCII F OF ( END ) C/L 15 * R# ! VBOTTOM ENDOF
              ENDCASE
            THEN
        ENDOF                                                  
        4 OF 
            DROP DUP @ $1B5B = IF
              2 + C@  CASE
                  ASCII 5 OF ( PGUP )   SCR @ 1- VINIT  ENDOF
                  ASCII 6 OF ( PGDOWN ) SCR @ 1+ VINIT  ENDOF
                  ASCII 2 OF ( INS )    INS TOGGLE      ENDOF
              ENDCASE
            THEN
        ENDOF                                                  
        6 OF 
            DROP DUP @ $1B5B = IF
              5 + C@  CASE
                  ASCII H OF ( CTRL-HOME )
                                R# @ C/L / C/L * R# !
                                #LOCATE VXY ENDOF
                  ASCII F OF ( CTRL-END )
                                R# @ C/L / C/L * C/L 1- + R# !
                                #LOCATE VXY  ENDOF
              ENDCASE
            THEN
        ENDOF                                                  
      ENDCASE                                                   
      EXIT.FLAG ON? UNTIL FLUSH VEND ;                                     


( CTRL HOME)
( WKEY .STACK CR DUMP 6 30CE) 
( 30CE 1B 5B 31 3B 35 48   _[1;5H OK)

( CTRL END)
( WKEY .STACK CR DUMP 6 30CE) 
( 30CE 1B 5B 31 3B 35 46   _[1;5F OK)

( INS)
( WKEY .STACK DUMP 4 30CE)
( 30CE 1B 5B 32 7E         _[2~   OK)



( GO FLOAD 2/VEDIT2.4T 7 VEDIT )