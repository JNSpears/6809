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
: TOGGLE DUP C@ 0= SWAP C! ;             -->

: CASE  ?COMP CSP @ !CSP 4 ; IMMEDIATE                          
                                                                
: OF 4 ?PAIRS COMPILE OVER COMPILE = COMPILE 0BRANCH            
    HERE 0 , COMPILE DROP 5 ; IMMEDIATE                         
                                                                
: ENDOF 5 ?PAIRS COMPILE BRANCH HERE 0 ,                        
    SWAP 2 [COMPILE] THEN 4 ; IMMEDIATE                         
                                                                
: ENDCASE 4 ?PAIRS COMPILE DROP BEGIN SP@                       
    CSP @ = 0= WHILE 2 [COMPILE] THEN REPEAT                    
    CSP ! ; IMMEDIATE                                           
( : RNG.OF 4 ?PAIRS  COMPILE ROT COMPILE SWAP COMPILE )
 (   - COMPILE > COMPILE 0BRANCH )
  (  HERE 0 , XXXXXX 5 ; IMMEDIATE )
;S