 nam LIST
*********************************************************
* LIST.CM                         TIM MCKEE  Aug. 27 1980
*
* This utility will allow You list the contents of a file to
* either the console device, or to an alternate device, or to
* both. The listins may be specified to have decimal line
* numbers to facilitate editing. The command is called by:
*
* MPX? [N/]LIST [#DV] <filename> [options]
*
* The [N/] option specifies from which drive to load the LIST
* utility. The [#DV] option will allow you to vector the
* output to any device with an active DCB, default output is
* to the console device. The <filename> should be specified
* as a standard MPX-9 filename. There are three (3) options
* available with this command.
*
*   /N - will toggle the line number printins function.
*	 normally OFF
*
*   /C - will print to the console as well as to the alternate
*	 device. normally OFF
*
*   /F - will toggle the flag which indicates that the printer
*	 accepts hardware form feed characters. normallY OFF
*
*   /P - suppress polling keyboard for break. (JNS)
* 
*   /R:n - display fixed size records (JNS)
*
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

**************************************************
* ERROR Codes                                    *
**************************************************
ERR_OK  EQU      0 ; OK - NO ERROR
ERR_FN  EQU      1 ; FN - ILLEGAL FUNCTION ATTEMPTED
ERR_ID  EQU      2 ; ID - ILLEGAL DISK DRIVE #
ERR_IB  EQU      3 ; IB - ILLEGAL BLOCK # USED
ERR_DM  EQU      4 ; DM - DISK MISSING OR INOPERATIVE
ERR_NB  EQU      5 ; NB - NULL (EMPTY) BLOCK READ
ERR_SK  EQU      6 ; SK - SEEK ERROR
ERR_RD  EQU      7 ; RD - DISK READ ERROR
ERR_VF  EQU      8 ; VF - DISK VERIFY ERROR
ERR_WP  EQU      9 ; WP - WRITE PROTECTED DISK
ERR_NF  EQU     10 ; NF - FILE NOT FOUND
ERR_DF  EQU     11 ; DF - DISK FULL OR NO SPACE FOR FILE
ERR_IF  EQU     12 ; IF - INVALID FILE SPEC
ERR_NC  EQU     13 ; NC - FILE NOT CLOSED
ERR_UF  EQU     14 ; UF - ACCESS TO UNOPENED FILE
ERR_IA  EQU     15 ; IA - ILLEGAL ACCESS TO FILE
ERR_EF  EQU     16 ; EF - END OF FILE
ERR_SN  EQU     17 ; SN - SYNTAX ERROR IN COMMAND


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
        ORG $1000
        sts     >stack,pcr       ;save the stack for error recovery
        ldd     #0  
        std     >altout,pcr      ; initialize variables
        std     >linnum,pcr      ; clear line number
        lda     #0      ; initalize FormFeed flag off
        sta     >ffflg,pcr       
        lda     #0      ; initalize Console flag off
        sta     >conflg,pcr      
        lda     #0      ; initalize Number flag off
        sta     >numflg,pcr      
        lda     #$ff 	; initalize Poll flag off ; JNS added to help with automated testing
        sta     >polflg,pcr      
        lda     #0      ; initalize Record Size flag off
        sta     >recflg,pcr  
        clr     >reccnt,pcr

;;
;; any of the above option may be set to default ON by changeing the
;; LDA #0 to an LDA #1
;;

        ldy     RAMv
        leay    CIDCB-RAM,y
        sty     >_cidcb,pcr
        
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
        fcb     SKPSPC  ; x -> filename in command line
DfltIO  leay    >infcb,pcr ; point to the fcb
        swi3
        fcb     INTFCB  ; initialize the fcb
        lbne    dskerr  ; report disk error
        tst     FCBSUF,y ; check for suffix
        bne     nodfsf  ; skip if specified
        ldd     #$5458  ; 'TX' is the default suffix
        std     FCBSUF,Y
nodfsf  pshs    x               ; save x for a moment
        leax    >inbufr,pcr      ; point to file buffer
        lda     #ReadFn   ; setup for read
        swi3
        fcb     OPNFIL  ; open the file
        puls    x       ; recvover x
        lbne    dskerr  ; report disk error
option  swi3
        fcb     SKPSPC  ; point to the next word
        beq     listit
        lda     ,x+
        cmpa    #'/     ; look for option flags
        bne     listit
        lda     ,x+     ; get option chanr and bump pointer

        cmpa    #'C     ; is a option 'C'?
        bne     opti00  ; skip if not
        com     >conflg,pcr ; toggle option 'C'
        bra     option  ; get next option

opti00  cmpa    #'F     ; is a option 'F'?
        bne     opti01  ; skip if not
        com     >ffflg,pcr ; toggle option 'F'
        bra     option  ; get next option

opti01  cmpa    #'N     ; is a option 'N'?
        bne    	opti02  ; skip if not
        com     >numflg,pcr ; toggle option 'N'
        bra     option  ; get next option

opti02  
        cmpa    #'P     ; is a option 'P'?
        bne     opti03     ; skip if not
        com     >polflg,pcr ; toggle option 'p'
        bra     option  ; get next option
        
opti03  
        cmpa    #'R     ; is a option 'R'?
        lbne    synerr  ; syntax error if not

        lda     ,x+             ; get next char
        cmpa    #':             ; should be an ':'
        lbne    synerr          ; syntax error if not

        swi3                    ; convert record size value
        fcb     DECNUM    

        stb     >recflg,pcr     ; save record size value

        bra     option  ; get next option
        
listit  clr     >lincnt,pcr     ; set line counter to zero
newlin  ldd     >linnum,pcr     ; get current line number
        addb    #1              ; increment it
        exg     a,b             ; exchange a,b
        daa                     ; adjust a
        exg     a,b             ; exchange a,b again
        adca    #0              
        daa                     ; adjust a
        std     >linnum,pcr     ; store result
        tst     >numflg,pcr     ; check status of number flag
        beq     listln          ; skip if numbers are not to be printed
        clr     >lzflg,pcr      ; initialize leading zero flag
        pshs    a               ; save acc 'a'
        lbsr    outhl           ; output first digit
        puls    a               ; restore acc 'a'
        lbsr    outhr           ; output second digit
        tfr     b,a             ; put digits 3,4 in acc 'a'
        lbsr    outhl           ; output third digit
        tfr     b,a             ; put digits 3,4 in acc 'a'
        lbsr    outhr           ; output fourth digit
        lbsr    outspc          ; output space
        
listln  leay    >infcb,pcr      ; point to fcb
        swi3
        fcb     RDFIL           ; get a character of input
        cmpb    #ERR_EF
        lbeq    eject
        tstb
        lbne    dskerr          ; report disk error
        anda    #$7f            ; adjust for ascii value
        cmpa    #$7f            ; check for nulls
        beq     listln          ; reloop when found
        cmpa    #4              ; check for EOF
        lbeq    eject           ; eject listing and return when eof found
        cmpa	#LF
        bne	listln1
        bra	listln
listln1 cmpa    #CR             ; check for end of line
        beq     nxtlin0
        bsr     output          ; send character to device
; check for record count.
        lda     >recflg,pcr     ; JNS
        beq     listln          ; JNS if recflg is zero then no fixed record size.
        cmpa    >reccnt,pcr     ; JNS
        blo     nxtlin          ; JNS

        bra     listln          ; get next character

nxtlin0
;        lda     >recflg,pcr     ; JNS
;        beq     nxtlin          ; JNS if recflg is zero then no fixed record size.
;        lda     #'.             ; JNS
;        bsr     output          ; send character to device
;        bra     listln          ; JNS

nxtlin  
;	lda	#'\		; for helping debug.
;	bsr 	output		;;
;	lda	#'r		;;
;	bsr	output		;;
        lda     >recflg,pcr     ; JNS
        bne     listln          ; JNS if recflg is zero then no fixed record size.

        bsr     crlf            ; send crlf
        cmpa    #54             ; check for page full
        blo     newlin          ; get next line if not full
        bsr     eject           ; eject when full
        lbra     listit          ; and continue

eject   ldx     >altout,pcr
        beq     eject1          ; skip ejects on default console output
        tst     >ffflg,pcr      ; check if form feed on
        beq     eject0          ; skip if no
        lda     #FF             ; send form feed character and return
        bsr     output 
        bra     eject1
eject0  bsr     crlf            ; output CRLF until end of page
        cmpa    #66             ; check for end of page
        bne     eject0          ; loop until done
eject1  clrb                    ; set z flag in cc
        rts
        
crlf    lda     #CR
        bsr     output
        lda     #LF
        bsr     output
;        lda     #0              ; output nulls to allow for slow printer to do CRLF
;        bsr     output
;        bsr     output
;        bsr     output
        clr     >reccnt,pcr     ; JNS clear char count for fixed record.
        inc     >lincnt,pcr
        lda     >lincnt,pcr     ; bump line count and get results
        rts
        
outhl   asra
        asra
        asra
        asra
outhr   anda    #$0f
        bne     sndnum
        tst     >lzflg,pcr
        bne     sndnum
outspc  lda     #SP
        bra     output
        
sndnum  ora     #'0             ; make number ascii
        sta     >lzflg,pcr      ; and set lzflag
        
output  pshs    b
        inc     >reccnt,pcr ; JNS increment char count for fixed record.
        tst	>polflg,pcr	; JNS added, check to see if we are going to poll for break/cancel
        beq	nopoll
        pshs    a	
        ldx     [>_cidcb,pcr]   ; get input dcb pointer
        ldb     #StatFn     	; check port status
        swi3
        fcb     REQIO
        bita    #1
        beq     outpt1
        ldb     #ReadFn
        swi3
        fcb     REQIO
        cmpa    #BRK
        beq     return
outpt1  puls    a

nopoll
        ldx     >altout,pcr     ; check for alternate output vector
        beq     outcns          ; default to console if not there
        ldb     #WritFn
        swi3
        fcb     REQIO           ; send character to alt. device
        tst     >conflg,pcr     ; check for console also flag
        beq     outret          ; return if not set
outcns  swi3
        fcb     OUTCHR          ; send character to console
outret  puls    b,pc

notfnd  ldb     #ERR_NF
        bra     dskerr
synerr  ldb     #ERR_SN
dskerr  swi3
        fcb     RPTERR
return  lds     >stack,pcr
        rts
        
        
endcod  equ *-1

*
** Working Variables
*

_cidcb  rmb     2
lincnt  rmb     1
lzflg   rmb     1
conflg  rmb     1
ffflg   rmb     1
numflg  rmb     1
polflg 	rmb	1   ; JNS
recflg  rmb     1   ; JNS
reccnt  rmb     1   ; JNS
linnum  rmb     2
stack   rmb     2
altout  rmb     2
infcb   rmb     32
inbufr  rmb     256

endpgm  equ     *-1

        
