 nam LIST9
*********************************************************
* LIST9.CM                            JNS 9/1/1982      *
*                                                       *
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


**************************************************
* Program (Position independant)
**************************************************
        ORG     $1000

        ldd     #0  
        std     >altout,pcr      ; initialize variables

        ldd     #0  
        std     >NoCRLF,pcr      ; initialize variables

        ldd     #0  
        std     >conflg,pcr      ; initialize variables
        
;;
;; any of the above option may be set to default ON by changeing the
;; LDA #0 to an LDA #1
;;

        swi3
        fcb     SKPSPC
        lbeq    synerr
        cmpa    #'#     ; Check for alt device
        bne     DfltIO  ; Default IO if not
        leax    1,X     ; x -> device name
        ldd     ,x++    ; load Device name into D
        cmpa    #'A     ; check validity
        lblo    synerr  ; report error
        cmpb    #'0     ; check validity
        lblo    synerr  ; report error
        tfr     x,y     ; save x for now
        swi3
        fcb     LOCDCB  ; find device dcb
        stx     >altout,pcr ; Store dcb address
        tfr     y,x     ; restore x
        lbeq    notfnd  ; report dcb not found
        swi3
        fcb     SKPSPC  ; x -> words in the command line
DfltIO  
option  swi3
        fcb     SKPSPC  ; point to the next word
        beq     echo
        lda     ,x+
        cmpa    #'/     ; look for option flags
        bne     echo
        lda     ,x+     ; get option char and bump pointer
        cmpa    #'C     ; is a option 'C'?
        bne     opti00  ; skip if not
        com     >conflg,pcr ; toggle option 'C'
        bra     option  ; get next option

opti00  cmpa    #'F     ; is a option 'F'?
        bne     opti01  ; skip if not
;        com     >ffflg,pcr ; toggle option 'F'
        bra     option  ; get next option

opti01  cmpa    #'N     ; is a option 'N'? No CRLF
        lbne    synerr  ; syntax error if not
        com     >NoCRLF,pcr ; toggle option 'N'
        bra     option  ; get next option
        
echo:
        leax    -1,x    ; backup from option parsing
;
; X -> command line text.
;

scan:	lda	,x+
	cmpa	#CR
	beq	echo1
	bsr     output
	bra	scan
echo1:  
        lda     NoCRLF,pcr        ; check to see if we are supressing the CRLF at the end.
        bne     echoX      
        lda     #CR
        bsr     output
        lda     #LF
        bsr     output
echoX:        
        clrb
        tstb
        rts
        
output  pshs    x
        ldx     >altout,pcr     ; check for alternate output vector
        beq     outcns          ; default to console if not there
        ldb     #WritFn
        swi3
        fcb     REQIO           ; send character to alt. device
        tst     >conflg,pcr     ; check for console also flag
        beq     outret          ; return if not set
outcns  swi3
        fcb     OUTCHR          ; send character to console
outret  puls    x,pc

notfnd  ldb     #10
        bra     dskerr
        
synerr  ldb     #16
dskerr  swi3
        fcb     RPTERR
        tstb
        rts

        
        
endcod  equ *-1

*
** Working Variables
*

conflg  rmb     1
NoCRLF  rmb     1
altout  rmb     2

endpgm  equ     *-1

        
