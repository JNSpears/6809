 NAM OVCTR
 TTL VECTOR ALL OUTPUT TO A DEVICE
 
****************************************
*
* OUTVECTR WILL ALLOW YOU TO VECTOR CONSOLE
* OUTPUT TO ANY DEVICE WITH AN ACTIVE DCB, 
* OR TO A FILE. IF THE UTILITY IS CALLED 
* WITH NO PARAMETER GIVEN, OUTPUT WILL BE 
* RETURNED TO THE DEVICE 'CN'
*
* MPX? OUTVECTR #LP
* MPX? OUTVECTR #DK:<FILENAME>
* MPX? OUTVECTR
*
* THE FIRST EXAMPLE WILL ROUTE ALL OUTPUT TO 
* THE DEVICE LP WHICH SHOULD BE A LINE PRINTER 
*
* THE SECOND EXAMPLE WILL ROUT ALL OUTPUT TO A 
* FILE. NOTE THAT YOU MUST SPECIFY '#DK:' BEFORE 
* THE FILENAME. THERE MUST NOT BE ANY SPACES 
* AFTER THE '#' SIGN THE FILE WILL BE CLOSED 
* AWHEN A EOT (4) IS WRITTEN TO THE FILE, AND 
* OUTPUT WILL BE RETURNED TO THE CONSOLE. OUTPUT 
* WILL RETURN TO THE CONSOLE ON ANAY DISK ERROR.
*
* THE THIRD EXAMPLE WILL RETURN ALL OUTPUT TO 
* THE DEVICE 'CN'. IF THE PREVIOUS VECTOR WAS TO 
* A FILE, IT WILL BE CLOSED.
*
* WRITTEN BY TIM MCKEE ON SEPT 4, 1980
*
****************************************

* Version 1.00

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


; SYSTEM ADDRESS CONSTANTS

ROM1	EQU $FC00 ; BASE ADDRESS OF PSYMON ROM
ROM2	EQU $F800 ; BASE ADDRESS OF EXTENSION ROM
RAM	EQU $F380 ; BASE ADDRESS OF SCRATCHPAD RAM;
FREE	EQU $F000 ; ADDRESS OF FREE RAM


; PSYMON RAM DEFINITIONS
	ORG RAM
; PSYMON INTERNAL STACK & REGISTER SPACE
; OFFSETS TO RAM BASE IN PARENTHESES

	RMB 55 ; STACK SPACE r1
STACK	EQU * ; (55) TOP OF STACK

REGC	RMB 1 ; (55) CONDITION CODE REGISTER

REGA	RMB 1 ; (56) A REGISTER I-‘
REGB	RMB 1 ; (57) B REGISTER
REGD	RMB 1 ; (58) DIRECT PAGE REGISTER
REGX	RMB 2 ; (59) X REGISTER
REGY	RMB 2 ; (61) Y REGISTER M
REGU	RMB 2 ; (63) U STACK POINTER
REGP	RMB 2 ; (65) PROGRAM COUNTER

; PSYMON BREAKPOINT TABLE
BpTabl	RMB 15 ; (67) SPACE FOR 5 BREAKPOINTS
BpTEnd	EQU * ; (82) END OF BREAKPOINT TABLE

; PSYMON WORK AREAS
MemPtr	RMB 2 ; (82) MEMORY POINTER FOR ‘M’ COMMAN1
UsrTbl	RMB 2 ; (84) ADDRESS OF USER COMMAND TABLE)
Comand	RMB 1 ; (86) COMMAND CHARACTER STORAGE
CkSum	RMB 1 ; (87) CHECKSUM FOR LOAD AND SAVE
BegAdd	RMB 2 ; (88) BEGIN ADDRESS FOR SAVE
EndAdd	RMB 2 ; (90) END ADDRESS FOR SAVE
StkPtr	RMB 2 ; (92) CONTENTS OF STACK POINTER

; THE PSYMON CONSOLE DCB
ConDCB	RMB 10 ; (94) STANDARD DCB

; PSYMON DCB POINTERS
DCBCHN	RMB 2 ; (104) BASE OF DCB CHAIN
CIDCB	RMB 2 ; (106) CONSOLE INPUT DCB
CEDCB	RMB 2 ; (108) CONSOLE ECHO DCB
CODCB	RMB 2 ; (110) CONSOLE OUTPUT DCB
TPDCB	RMB 2 ; (112) CASSETTE TAPE DCB

; PSYMON VECTORS
SWI3v	RMB 2 ; (114) SOFTWARE INTERRUPT 3
SWI2v	RMB 2 ; (116) SOFTWARE INTERRUPT 2
FIRQv	RMB 2 ; (118) FAST INTERRUPT REQUEST
IRQv	RMB 2 ; (120) INTERRUPT REQUEST
SWIv	RMB 2 ; (122) SOFTWARE INTERRUPT
NMIv	RMB 2 ; (124) NON-MASKABLE INTERRUPT
FRERAM	RMB 2 ; (126) ADDRESS OF FREE RAM


; DCB offsets
DCBLnk	equ 0	; pointer to next dcb in chain
DCBDId	equ 2	; ascii 2 char device Id
DCBDvr	equ 4	; device driver addr
DCBIOA	equ 6	; device i/o addr
DCBErr	equ 8	; error status code
DCBExt	equ 9	; number of extension bytes in dcb
DCBApp	equ 10	; dcb extension for driver

DCBDRV EQU 10 ;DISK DRIVE # (I-4)
DCBBLK EQU 11 ;DISK BLOCK
DCBBUF EQU 13 ;BUFFER ADDRESS

; DCB function Codes
ReadFn	equ 1	; Read function code
WritFn	equ 2	; Write function code
StatFn	equ 4	; Status function code
CntlFn	equ 8	; Device Control function code

*****************************
* Software Vectors
*****************************
RAMv		equ $ffde
DspSByv		equ $ffe0
DspDByv		equ $ffe2
GetHexv		equ $ffe4
PStringv	equ $ffe6
InChrv		equ $ffe8
OutChrv		equ $ffea
ReqIOv		equ $ffec
MonEntv		equ $ffee

**************************************************
* ASCII CHARACTER CONSTANTS
**************************************************
CR      EQU $0D ; CARRIAGE RETURN
LF      EQU $0A ; LINE FEED
NUL     EQU $00 ; NULL
BS      EQU $08 ; BACKSPACE
CAN     EQU $18 ; CANCEL
SP      EQU $20 ; SPACE
FF      EQU $0C ; FORM Feed for Printer
BRK     EQU $03 ; Crtl-c

 ORG 0 ;FORMAT AFTER INIT OR CLOSE
FCBOPN RMB 1 ;SET TO 0 BY INIT OR CLOSE
FCBDRN RMB 1 ;FILE DRIVE
FCBDBA RMB 2 ;DISK BUFFER ADDRESS
FCBNAM RMB 8 ;FILE NAME
FCBSUF RMB 2 ;FILE SUFFIX
FCBSIZ RMB 2 ;FILE SIZE
 RMB 15 ; RESERVED
FCBUFF RMB 1 ;UNOPENED FILE FLAG (CLEARED BY INIT, CLOSE)
 spc 1
 ORG 0 ;FORMAT AFTER OPEN
FCBUSE RMB 1 ;FILE USAGE (1=R, 2=W, 3=R/W)
FCBDRV RMB 1 ;FILE DRIVE
FCBBUF RMB 2 ;BUFFER ADDRESS
FCBCNT RMB 1 ;CURRENT BYTE COUNT
FCBADD RMB 2 ;ADDRESS VECTOR
FCBTYP RMB 1 ;BLOCK TYPE CODE (USER DEFINED)
FCBSTR RMB 2 ;FILE START BLOCK
FCBEND RMB 2 ;FILE END BLOCK
FCBCUR RMB 2 ;CURRENT RELATIVE BLOCK
FCBPRV RMB 2 ;PREVIOUS RELATIVE BLOCK
FCBNXT RMB 2 ;NEXT RELATIVE BLOCK
FCBEXT RMB 2 ;DIRECTORY EXTENSION BYTES
FCBPTR RMB 2 ;DATA POINTER
 RMB 9 ;RESERVED SPACE
FCBSTS RMB 1 ;FILE STATUS (SET BY OPEN)


CoOfst  equ     $6e ; offset from, PSYRAM for CODCB
CeOfst  equ     $6c ; offset from, PSYRAM for CEDCB

**************************************************
* Program (Position independant)
**************************************************
        ORG     $2000
Vector
        pshs    x		; Get Console Out and Echo DCB's from Psymon.
        ldx     RAMv		; and save them in local memory.
        leay    CoOfst,X
        STY     >CoDcb,PCR
        leay    CeOfst,X
        STY     >CeDcb,PCR
        puls    X
        
*        ldy     [>CODCB,PCR]      ; Get current output dcb.
        ldy     >CODCB,PCR      ; Get current output dcb. JNS removed indirection []
        ldy     DCBDvr,y        ; get driver address of that dcb
        sty     ,--s		; save on stack.
        leay    >filout,pcr     ; point to the file output routine
        cmpy    ,s++            ; see if they are the same
        bne     vectr1          ; skip if not
        lda     #4              ; setup for an end to file
        pshs    x
	lbsr    filout          ; and close it
	puls	x
vectr1
        swi3
        fcb     SKPSPC
        beq     consol		; no arguments then revert to output --> console
        cmpa    #'#
        bne     synerr
        ldd     1,x
        leay    3,x
        cmpa    #'A
        blo     synerr
        cmpa    #'Z
        bhi     synerr
        cmpb    #'0
        blo     synerr
        cmpb    #'Z
        bls     setup

synerr  ldb     #17
error   
	LEAX    >MSG,PCR     ; Get address of name
        swi3
        fcb     PSTRNG 
	
	swi3
        fcb     RPTERR
        rts

MSG	FCC	/))))/
	fcb	SP+$80
ConDev  FCC     /CN/
DskDev  FCC     /RD/
     
consol  ldd     >ConDev,PCR    
setup   swi3
        fcb     LOCDCB
        beq     notfnd
        cmpd    >DskDev,PCR     ; see if device is the disk device (DK or RD as the case is)
        bne     setup1
        lda     ,y+             ; get next char
        cmpa    #':             ; should be an ':'
        bne     synerr          ; syntax error if not
        tfr     y,x    
        leay    >outfcb,pcr
        swi3
        fcb     INTFCB          ; get filename into FCB X->Filename Y->FCB
        bne     error
        leax    >outbuf,pcr
        lda     #WritFn		; file open for write.
        swi3
        fcb     OPNFIL          ; Open File
        bne     error
        leax    >dummy,pcr      ; point to dummy DCB
        leay    >filout,pcr
        sty     DCBDvr,x        ; setup for file driver
setup1  stx     [>CoDcb,pcr]
        stx     [>CeDcb,pcr]
        clrb                    ; set 'z' in cc reg.
        rts
	
notfnd  ldb     #10
        bra     error
        
	; DCB output routine patched into dummy DCB that is used inplace of CoDcb and CeDcb
filout  pshs    a,b,y
        leay    >outfcb,pcr
        ldb     #WritFn
        SWI3
        fcb     WTFIL           ; write byte to file
        bne     filerr
        cmpa    #4
        bne     filret
filerr  swi3
        fcb     CLSFIL          ; close file on error
        bsr     consol          ; and return output to console
filret  puls    a,b,y,pc

        
endcod  equ *-1

        ORG     $2100

*
** Working Variables
*

CoDcb   rmb     2	; Pointer to Psymon's Console Output DCB 
CeDcb   rmb     2	; Pointer to Psymon's Console Echo DCB 
outfcb  rmb     32
outbuf  rmb     256
dummy   rmb     10

endpgm  equ     *-1

        
