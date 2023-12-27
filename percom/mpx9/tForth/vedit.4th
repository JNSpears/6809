( VEDIT )   
." LOADING VEDIT" CR
EDITOR DEFINITIONS

5 :BUFFER KEY-BUFF     0 VARIABLE KEY-COUNT 

: WKEY ( WAIT FOR MULTIPLE CHAR KEY INPUT: -- ADDR #CHARS )
    KEY-BUFF 5 0 FILL
    0 KEY-COUNT !  
    KEY KEY-BUFF KEY-COUNT @ + C!  1 KEY-COUNT +! 
    0 BEGIN
      1+ DUP 200 < 
      WHILE
        ?TERMINAL -DUP IF 
            KEY-BUFF KEY-COUNT @ + C!  1 KEY-COUNT +! THEN
      REPEAT DROP KEY-BUFF KEY-COUNT @ ;                -->
FLAG EXIT.FLAG                                                  
FLAG INS
27 CONSTANT <ESC>  CTRL S CONSTANT <KR>  $08 CONSTANT <BS>      
13 CONSTANT <CR>   CTRL T CONSTANT <KL>  $7F CONSTANT <DEL>     
: .STATUS.LINE ( N.SECONDS TEXT.ADDR ... )
    19 0 VT100 XY CLRL           
    COUNT TYPE                           
    1000 * BEGIN 1- ?TERMINAL OVER 0= OR UNTIL DROP             
    19 0 VT100 XY CLRL ;                        

: VXY ( POSITION CURSOR RELATIVE EO SCREEN; Y X -- )
    5 3 D+ VT100 XY ; 
: VTOP 0 0 VXY ;
: VBOTTOM 0 15 VXY ;
: VEND ( POSITION CURSOR BELOW SCREEN AREA )
    VT100 1 20 XY ;                                     -->
: PROCESS.FORTH.COMMAND  ( ACCEPT A COMMAND AND DO IT )                               
    VEND ." >>" TIB @ C/L EXPECT                      
    0 IN ! INTERPRET 
    #LOCATE VXY ;       

: UPDATE.FULL ( SCR# -- )
    VT100  HOME  CLRS  LIST  2 " WELCOME" .STATUS.LINE VTOP ;

: UPDATE.EOS
    VT100   ;

: UPDATE.EOL ( LINE# -- )
    VT100 DUP 3 .R SPACE SCR @ .LINE ;

: VINIT 0 R# ! UPDATE.FULL ;
                                                        -->
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
                                                        -->
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
                                                        -->



CODE CMOVE>  ( c-addr1 c-addr2 u --   ; move from top 1->2 )    
    .X .Y    PSHS,
    .D .X .Y PULU,   ( D:u X:dst Y:src )
    .U      PSHS,
    D,X     LEAX,
    D,Y     LEAY,
    .Y .U   TFR,
    .D .Y   TFR,       ( Y:u U:src' X:dst' )
    1 ,Y    LEAY,      ( Y:u' U:src' X:dst' )
    BEGIN,
        -1 ,Y LEAY, 
        NE WHILE,
            ,-U LDA,        ,-X STA, 
    REPEAT,
    .U      PULS,       .X .Y   PULS,
    NEXT,                                               -->
: CH.CH 
    DUP ( C .. C C ) 
    #LAG DROP ( C C .. C C @ )
    INS ON? 
    IF                  
       #LAG 1- OVER 1+ ROT SWAP ROT CMOVE> 
    THEN
    C! ( C C @ .. C ) 
    #LOCATE VXY 
    EMIT ( C .. )
    1 R# +!
    #LAG TYPE 
    #LOCATE VXY ;
                                                        -->


: 1-CHAR-KEYS ( PROCESS SINGLE CHAR KEY; CH1 .. )
  DROP C@ DUP CASE          
    CTRL L          OF DROP SCR @ UPDATE.FULL ENDOF                          
    CTRL [ ( ESC )  OF DROP EXIT.FLAG ON ENDOF                         
    <BS>            OF DROP BACKSPACE.CH UPDATE ENDOF                         
    <DEL>           OF DROP DELETE.CH UPDATE ENDOF                            
    <ESC>           OF DROP PROCESS.FORTH.COMMAND ENDOF  
    ASCII ~ BL      RNG.OF CH.CH UPDATE ENDOF     
                    DROP 5 " WHAT?" .STATUS.LINE                               
  ENDCASE ;                                             -->






: 3-CHAR-KEYS ( PROCESS 3 CHAR KEY; CH3 .. )
  DROP DUP @ $1B5B = IF 2 + C@  CASE ASCII A ( UP )
        OF #LOCATE IF C/L MINUS R# +! VT100 UP THEN ENDOF
       ASCII B ( DOWN )
        OF #LOCATE 15 = NOT IF C/L R# +! VT100 DOWN THEN ENDOF
       ASCII C ( RIGHT )
        OF #LAG 1- IF  1 R# +! VT100 RIGHT THEN ENDOF
       ASCII D ( LEFT )
        OF  #LEAD IF -1 R# +! VT100 LEFT THEN ENDOF
       ASCII H ( HOME )
        OF 0 R# ! VTOP ENDOF
       ASCII F ( END )
        OF C/L 15 * R# ! VBOTTOM ENDOF
       ASCII E ( NUMPAD-5 )
        OF PROCESS.FORTH.COMMAND  ENDOF
  ENDCASE DROP THEN ;                                   -->
: 4-CHAR-KEYS  ( PROCESS 4 CHAR KEY; CH3 .. )
    DROP DUP @ $1B5B = IF
              2 + C@  CASE
                  ASCII 5 OF ( PGUP )   SCR @ 1- VINIT  ENDOF
                  ASCII 6 OF ( PGDOWN ) SCR @ 1+ VINIT  ENDOF
                  ASCII 2 OF ( INS )    INS 1 TOGGLE    ENDOF
              ENDCASE
            THEN ;

: UPDATE-STATUS VT100
      6 0 XY INS ON? IF ." INS" ELSE 3 SPACES THEN
      12 0 XY #LOCATE DECIMAL . . 
      40 0 XY PAD COUNT TYPE
      #LOCATE VXY ;

                                                        -->
: 6-CHAR-KEYS  ( PROCESS 6 CHAR KEY; CH3 .. )
    DROP DUP @ $1B5B = IF
      5 + C@ 
      CASE
        ASCII H OF ( CTRL-HOME )
                    R# @ C/L / C/L * R# !
                    #LOCATE VXY ENDOF
        ASCII F OF ( CTRL-END; MOVE TO LAST USED CHAR OF LINE )
                    R# @ C/L / C/L * R# !
                    #LAG  -TRAILING R# +! DROP
                    #LOCATE VXY ENDOF 
      ENDCASE
    THEN ;
                                                        -->


: VEDIT VINIT EXIT.FLAG OFF ( SCR# -- )                
    BEGIN UPDATE-STATUS VT100
      WKEY DUP                 ( ADDR #CHAR #CHAR )
      CASE
        1 OF 1-CHAR-KEYS ENDOF                                                  
        3 OF 3-CHAR-KEYS ENDOF    
        4 OF 4-CHAR-KEYS ENDOF                                                  
        6 OF 6-CHAR-KEYS ENDOF                                                  
      ENDCASE                                               
      EXIT.FLAG ON? UNTIL FLUSH VEND ;                          

FORTH DEFINITIONS DECIMAL

;S                                  -->         