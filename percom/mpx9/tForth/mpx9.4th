( MPX/9 INTERFACE   JNS 8/17/83 )                               
HEX                                                             
: :FCB             ( CREATE FILE CONTROL BLOCK; :FCB <NAME> )   
    <BUILDS 20 HERE OVER ERASE ALLOT                            
    DOES> ;                                                     
                                                                
CODE (INIT)         ( FCB@ FILENAME@ ... ERR# )                 
    SAVEIP,  ,U++ LDX,  INX,  0,U LDY,  012 MPX,  CLRA,         
    0,U STD,  RESTOREIP,  RTS,                                  

: INIT        ( FCB INIT <FILENAME> )                           
    BL WORD HERE (INIT) ;                                       
                                                                
CODE (RPTER)                                                    
    ,U++ LDD,  0D MPX,  RTS,                                    

: RPTER       ( PRINT MPX/9 ERROR MESSAGE; N ... )              
    -DUP  IF  (RPTER)  QUIT  THEN  ;    ;S                      

( MPX/9 FILE INTERFACE  OPEN,CLOSE,DELETE )       HEX           
CODE OPEN    ( 1=R,2=W,3=RW BUFFER@ FCB@ ... ERR# )             
    SAVEIP,  ,U++ LDY,  ,U++ LDX,  1 ,U LDA,  013 MPX,          
    CLRA,  0,U STD,  RESTOREIP, RTS,                            
                                                                
CODE CLOSE   ( FCB ... ERROR# )                                 
    0,U LDY,  014 MPX,  CLRA,  0,U STD,  RTS,                   
                                                                
CODE DELETE  ( FCB ... ERROR# )                                 
    0,U LDY,  020 MPX,  CLRA,  0,U STD,  RTS,                   
                                                                
:FCB FILE                                                       
                                                                
: SETSCREEN         ( SETSCREEN <FILENAME> )                    
    FILE  INIT  RPTER  3  8000  FILE  OPEN  RPTER  ;            
