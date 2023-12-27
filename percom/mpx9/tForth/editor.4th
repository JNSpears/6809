( PROGRAM DOCUMENTATION                           WFR-79APR20 )
 HEX
: INDEX  ( PRINT FIRST LINE OF EACH SCREEN FROM-2, TO-1 *)
        0C EMIT ( FORM FEED ) CR 1+ SWAP
        DO CR I 3 .R SPACE
            0 I .LINE ?TERMINAL IF LEAVE ENDIF LOOP ;

: TRIAD ( PRINT 3 SCREENS ON PAGE, CONTAINING # ON STACK )
        0C EMIT ( FF ) 3 / 3 * 3 OVER + SWAP
        DO CR I LIST LOOP CR
        0F MESSAGE CR ; DECIMAL

: BLOCKS  ( DISPLAY BLOCK#'S IN BUFFERS )                       
    LIMIT FIRST                                                 
    DO I @ DUP 0< IF $75 EMIT $7FFF AND THEN .            
        B/BUF 4 + +LOOP CR ;                            -->                  
( OUTLINE + DOCUMENTATION TOOLS  )                              
                                                             
: 1OUTLINE    ( OUTLINE ONE SCREEN; N ... )                     
 DUP  CR  ." SCREEN# "  .  CR                                
 16  0  DO  I  OVER  (LINE)  OVER  C@  BL  -                 
              IF  3  SPACES  -TRAILING  TYPE  CR                 
              ELSE  DROP  DROP  THEN                         
          LOOP  DROP  ;                                      

: OUTLINE     ( OUTLINE SEVERAL SCREENS; FROM TO ... )          
 1+  SWAP  CR  DO  I  1OUTLINE  LOOP    ;                 
                                                             
                                                        -->



(  TEXT, LINE                              WFR-79MAYO1 )
FORTH DEFINITIONS HEX
$40 CONSTANT C/L                 ( TEXT CHARACTERS PER LINE *)

: TEXT                  ( ACCEPT FOLLOWING TEXT TO PAD *)
     HERE C/L 1+ BLANKS WORD HERE PAD C/L 1+ CMOVE ;

: LINE        ( RELATIVE TO SCR, LEAVE ADDRESS OF LINE *)
      DUP FFF0 AND 17 ?ERROR  ( KEEP ON THIS SCREEN )
      SCR @       (LINE)       DROP ;
ASCII / CONSTANT DEL ( DELIMITER / )
." LOADING EDITOR" CR
                                                        -->
( ---------------------------------------------------------- )


( LINE EDITOR                               WFR-79MAYO3 )
VOCABULARY EDITOR IMMEDIATE HEX
: WHERE            ( PRINT SCREEN # AND IMAGE OF ERROR *)
    DUP B/SCR / DUP SCR 1 ." SCR # " DECIMAL .
    SWAP C/L /MOD C/L * ROT BLOCK + CR C/L TYPE
    CR HERE C@ - SPACES $5E EMIT [COMPILE] EDITOR QUIT ;

EDITOR DEFINITIONS
: #LOCATE              ( LEAVE CURSOR OFFSET-2, LINE-1 *)
         R# @ C/L /MOD ;
: #LEAD          ( LINE ADDRESS-2, OFFSET-1 TO CURSOR *)
        #LOCATE LINE SWAP ;
: #LAG       ( CURSOR ADDRESS-2, COUNT-1 AFTER CURSOR *)
        #LEAD DUP >R + C/L R> - ;
: -MOVE ( MOVE IN BLOCK BUFFER ADDR FROM-2, LINE TO-1 *)
        LINE C/L CMOVE UPDATE ;                          -->
( SCR # 89 )
( LINE EDITING COMMANDS                     WFR-79MAY03 )
: H                        ( HOLD NUMBERED LINE AT PAD *)
     LINE PAD 1+ C/L DUP PAD C! CMOVE ;

: E                         ( ERASE LINE-1 WITH BLANKS *)
     LINE C/L BLANKS UPDATE ;

: S                       ( SPREAD MAKING LINE # BLANK *)
     DUP 1 - ( LIMIT ) 0E ( FIRST TO MOVE )
     DO I LINE I 1+ -MOVE -1 +LOOP E ;

: D                  ( DELETE LINE-1, BUT HOLD IN PAD *)
    DUP H 0F DUP ROT
    DO I 1+ LINE I -MOVE LOOP E ;
                                                        -->
( SCR # 90 )
( LINE EDITING COMMANDS                          WFR-79MAY03 )

: M ( MOVE CURSOR BY SIGNED AMOUNT-1, PRINT ITS LINE *)
     R# +! CR SPACE #LEAD TYPE 5F EMIT
                    #LAG TYPE #LOCATE . DROP ;

: DISP?  ( DISPLAY LINE IF LAST COMMAND IN INPUT STREAM )       
    TIB @  IN @  +  C@  0=  IF  0 M  THEN ; 

: T     ( TYPE LINE N, SAVE; N ... )                            
    DUP C/L * R# ! DUP H DISP? ;                                
                                                                
: L     ( LIST SCREEN )                                         
    SCR @ LIST DISP? ;   
                                                        -->
( SCR # 91 )
( LINE EDITING COMMANDS                           WFR-790105 )
: R                         ( REPLACE ON LINE #-1, FROM PAD *)
      PAD 1+ SWAP -MOVE ;

: P                          ( PUT FOLLOWING TEXT ON LINE-1 *)
      1 TEXT R ;

: I                      ( INSERT TEXT FROM PAD ONTO LINE # *)
      DUP S R ;

: TOP                   ( HOME CURSOR TO TOP LEFT OF SCREEN *)
      0 R# ! DISP? ;
                                                        -->


( SCR # 92 )
( SCREEN EDITING COMMANDS                        WFR-79APR27 )
: CLEAR                          ( CLEAR SCREEN BY NUMBER-1 *)
      SCR ! 10 0 DO FORTH I EDITOR E LOOP ;

( : FLUSH                ( WRITE ALL UPDATED BLOCKS TO DISC *)
(   [ LIMIT FIRST - B/BUF 4 + / ]         ( NUMBER OF BUFFERS)
(    LITERAL 0 DO 7FFF BUFFER DROP LOOP ;                    )

: COPY                  ( DUPLICATE SCREEN-2, ONTO SCREEN-1 *)
   B/SCR * OFFSET @ + SWAP B/SCR * B/SCR OVER + SWAP
   DO DUP FORTH I BLOCK 2 - ! 1+ UPDATE LOOP
   DROP FLUSH ;
                                                        -->


( STRING MATCH FOR EDITOR                      PM-WFR-80APR25)
: -TEXT                  ( ADDRESS-3, COUNT-2, ADDRESS-1 --- )
 SWAP -DUP IF    ( LEAVE BOOLEAN MATCHED=NON-zERO, NOPE=ZERO )
           OVER + SWAP         ( NEITHER ADDRESS MAY BE ZERO!)
      DO DUP C@ FORTH I C@ -
         IF 0= LEAVE ELSE 1+ THEN LOOP
      ELSE DROP 0= THEN ;
: MATCH   ( CURSOR ADDRESS-4, BYTES LEFT-3, STRING ADDRESS-2,)
          ( STRING COUNT-1, --- BOOLEAN-2, CURSOR MOVEMENT-1 )
  >R >R 2DUP R> R> 2SWAP OVER + SWAP
 ( CADDR-6, BLEFT-5, $ADDR-4, SLEN-3, CADDR+BLEFT-2, CADDR-1 )
  DO 2DUP FORTH I -TEXT
    IF >R 2DROP R> - I SWAP - 0 SWAP 0 0 LEAVE
    ( CADDR BLEFT SADDR SLEN OR ELSE O OFFSET O O )
      THEN LOOP 2DROP    ( CADDR-2, BLEFT-1, OR O-2, OFFSET-1)
    SWAP 0= SWAP ;                                      -->
( STRING EDITING COMMANDS                         WFR-79MAR24)
: 1LINE      ( SCAN LINE WITH CURSOR FOR MATCH TO PAD TEXT, *)
                            ( UPDATE CURSOR, RETURN BOOLEAN *)
       #LAG PAD COUNT MATCH R# +! ;

: FIND   ( STRING AT PAD OVER FULL SCREEN RANGE, ELSE ERROR *)
     BEGIN 3FF R# @ <
        IF TOP PAD HERE C/L 1+ CMOVE 0 ERROR ENDIF
        1LINE UNTIL ;

: DELETE                   ( BACKWARDS AT CURSOR BY COUNT-1 *)
     >R #LAG + FORTH R -        ( SAVE BLANK FILL LOCATION )
     #LAG R MINUS R# +!         ( BACKUP CURSOR )
     #LEAD + SWAP CMOVE
     R> BLANKS UPDATE ;         ( FILL FROM END OF TEXT )
                                                        -->
( STRING EDITOR COMMANDS                         WFR-79MAR24 )
: N     ( FIND NEXT OCCURANCE OF PREVIOUS TEXT *)
      FIND DISP? ;
: F     ( FIND OCCURANCE OF FOLLOWING TEXT *)
       DEL TEXT N ;

: B     ( BACKUP CURSOR BY TEXT IN PAD *)
       PAD  C@  MINUS  R# +!  DISP? ;

: X     ( DELETE FOLLOWING TEXT *)
       DEL TEXT FIND PAD C@ DELETE DISP? ;

: TILL ( DELETE ON CURSOR LINE, FROM CURSOR TO TEXT END *)
       #LEAD + DEL TEXT 1LINE 0= 0 ?ERROR
       #LEAD + SWAP - DELETE DISP? ;
                                                        -->
( STRING EDITOR COMMANDS                          WFR-79MAR23)
: C       ( SPREAD AT CURSOR AND COPY IN THE FOLLOWING TEXT *)
   DEL TEXT PAD COUNT
   #LAG ROT OVER MIN >R
   FORTH R R# +!   ( BUMP CURSOR )
   R - >R          ( CHARS T0 SAVE )
   DUP HERE R CMOVE  ( FROM OLD CURSOR T0 HERE )
   HERE #LEAD + R> CMOVE ( HERE T0 CURSOR LOCATION )
   R> CMOVE UPDATE ( PAD T0 OLD CURSOR )
   DISP?   ( LOOK AT NEW LINE ) ;
                                                        -->




( FORTH EDITOR    SCREENS 6-17                   JNS 8/28/83 )
: +S                                      ( NEXT SCREEN; ... )
    SCR @ 1+ LIST TOP ;                                       
: -S                                      ( PREV SCREEN; ... )
    SCR @ 1- LIST TOP ;                                       
: SUB                               ( SUB <OLD>[BSLASH]<NEW> )                            
    DEL  TEXT  FIND  PAD  C@  DELETE  C  ;                      
: H-                                ( HOLD LINE AFTER CURSOR )                             
    PAD 1+ C/L BLANKS  C/L PAD C!                               
    #LAG PAD 1+ SWAP CMOVE ;                                    
: E-                  ( ERASE REMAINDER OF LINE AFTER CURSOR )               
    #LAG  BLANKS  UPDATE  DISP?  ;                              
: -E     ( DELETE TEST BEFORE COUSOR )                          
    #LEAD  DELETE  DROP  DISP?  ;                               
: R-     ( REPLACE REMAINDER OF LINE AFTER CURSOR )             
    PAD  1+  #LAG  CMOVE  UPDATE  DISP?  ;
                                                        -->
( FORTH EDITOR    SCREENS 6-17                   JNS 8/28/83 )
: +L  C/L R# +!  DISP?  ;                ( MOVE TO NEXT LINE )                                    
: -L  C/L MINUS R# +!  DISP?  ;      ( MOVE TO PREVIOUS LINE )                         
: AA  R# @ C/L / C/L * R# ! ;     ( MOVE TO BEGINING OF LINE )                           
: ZZ  AA #LAG -TRAILING R# +! DROP ;   ( MOVE TO END OF LINE )                      
                                                                
: A  AA DISP? ;    : Z  ZZ DISP? ;                              
                                                                
: RJ                          ( RIGHT JUST TEXT AFTER CURSOR )                        
    H- E- AA PAD COUNT -TRAILING                                
    C/L SWAP - R# +! R- DROP DISP? ;                            
                                                                
FORTH DEFINITIONS DECIMAL

;S