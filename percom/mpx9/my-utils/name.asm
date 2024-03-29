*********************************************************
* name.CM                          JNS 3/2/1982         *
*                                                       *
* Usage:                                                *
* 1. [#/]name<cr>                                       *
*       Display old name of disk in drive #             *
* 2. [#/]name [<d>] <text><cr>                          *
*       set the name of disk in drive <d> to <text>     *
*                                                       *
*********************************************************

**************************************************
* SWI3 PARAMETER DEFINITIONS
**************************************************
* PSYMON (tm) ROUTINE REFERENCES
MONITR EQU 0 ;RETURN TO MONITOR MODE
REQIO EQU 1 ;REQUEST I/O
OUTCHR EQU 2 ;OUTPUT CHARACTER TO TERMINAL
INCHR EQU 3 ;INPUT CHARACTER FROM TERMINAL
PSTRNG EQU 4 ;PRINT STRING
GETHEX EQU 5 ;GET HEX NUMBER
DSPDBY EQU 6 ;DISPLAY DOUBLE BYTE
DSPSBY EQU 7 ;DISPLAY SINGLE BYTE
 spc 1
* MPX/9 ROUTINE REFERENCES
MPX    EQU 8    ;RETURN TO MPX/9
GETLIN EQU 9    ;GET * LINE OF INPUT
SKPSPC EQU 10   ;SKIP SPACES IN LINE BUFFER
GETWRD EQU 11   ;GET THE NEXT WORD IN LINE
PROCMD EQU 12   ;PROCESS COMMAND LINE
RPTERR EQU 13   ;REPORT ERROR
LOCFIL EQU 14   ;LOCATE FILE IN DIRECTORY
LOCSPC EQU 15   ;LOCATE SPACE IN DIRECTORY
RDDRCT EQU 16   ;READ DISK DIRECTORY
WTDRCT EQU 17   ;WRITE DISK DIRECTORY
INTFCB EQU 18   ;INITIALIZE FCB
OPNFIL EQU 19   ;OPEN FILE
CLSFIL EQU 20   ;CLOSE FILE
RDFIL  EQU 21   ;READ A FILE (BYTE)
WTFIL  EQU 22   ;WRITE A FILE (BYTE)
RDBLK  EQU 23   ;READ A BLOCK
WTBLK  EQU 24   ;WRITE A BLOCK
MEMLOD EQU 25   ;LOAD A MEMORY SEGMENT
MEMSAV EQU 26   ;SAVE A MEMORY SEGMENT
COMPAR EQU 27   ;COMPARE STRINGS
BLKMOV EQU 28   ;BLOCK MOVE
DECNUM EQU 29   ;GET DECIMAL NUMBER
HEXNUM EQU 30   ;GET HEXADECIMAL NUMBER
DSPDEC EQU 31   ;DISPLAY DECIMAL NUMBER & SPACE
DELFIL EQU 32   ;DELETE A DISK FILE
LOCDCB EQU 33   ;LOCATE DCB FOR DEVICE
ADDDCB EQU 34   ;ADD DCB TO DEVICE LIST
DELDCB EQU 35   ;DELETE DCB FROM DEVICE LIST
 spc 1
SYSLIM EQU 35   ;LAST VALID CALL
 spc 1
**************************************************
* ASCII CHARACTER CONSTANTS
**************************************************
CR EQU $0D ;CARRIAGE RETURN
LF EQU $0A ;LINE FEED
NUL EQU $00 ;NULL
BS EQU $08 ;BACKSPACE
CAN EQU $18 ;CANCEL
SP EQU $20 ;SPACE
MaxDrv EQU 4


        NAM     NAME.CM JNS 3/2/1982
        ORG     $1000
Name:   SWI3
        FCB     SKPSPC          ; skip spaces at X
        beq     NameDsp
        lda     ,x              ; get first char of this invocation of name
        cmpa    #'0
        blt     NoDrive
        cmpa    #MaxDrv
        bgt     NoDrive
        sta     <SaveCmd+2,PCR
NoDrive LEAY    NameBuf,PCR     ; point Y to name buffer
Name1   lda     ,x+             ; get next char
        sta     ,y+             ; store next char
;        cmpY    NameEnd,PCR     ; check for buffer overflow
;        beq     Name2
        cmpa    #CR             ; end of command?
        bne     Name1
Name2   ora     #$80            ; set MSbit
        sta     -1,Y            ; put it in the buffer
        LEAX    SaveCmd,PCR        ; load X with address of save command
        swi3
        fcb     PROCMD          ; execute the save command
        bra     NameX
NameDsp LEAX    NameBuf,PCR     ; Get address of name
        swi3
        fcb     PSTRNG          ; print the old name        
NameX   clrb
        tstb
        rts

SaveCmd FCC     "S 1/NAME.CM 1000 1090 1000"
        FCB     CR
NameBuf FCB     CR+$80
        RMB     64-1      ; name buffer
NameEnd EQU     *-1
