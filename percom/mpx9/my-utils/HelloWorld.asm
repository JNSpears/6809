; **********************************************
; HelloWorld.asm
;
; **********************************************

; ASCII CHARACTER CONSTANTS

CR EQU $D       ; CARRIAGE RETURN
LF EQU $A       ; LINE FEED
SP EQU $20      ; SPACE

*****************************
* Software Vectors
*****************************
RAMv            equ $ffde
DspSByv         equ $ffe0
DspDByv         equ $ffe2
GetHexv         equ $ffe4
PStringv        equ $ffe6
InChrv          equ $ffe8
OutChrv         equ $ffea
ReqIOv          equ $ffec
MonEntv         equ $ffee

**************************************************
* SWI3 PARAMETER DEFINITIONS
**************************************************
* PSYMON (tm) ROUTINE REFERENCES
MONITR EQU 0 ;RETURN TO MONITOR MODE
REQIO  EQU 1 ;REQUEST I/O
OUTCHR EQU 2 ;OUTPUT CHARACTER TO TERMINAL
INCHR  EQU 3 ;INPUT CHARACTER FROM TERMINAL
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

*****************************
* Software Vectors
*****************************
 spc 1
**************************************************
* Main ENTRY POINT
**************************************************
 ORG $1000
HelloWorld:
	pshs	x
	leax	HWSTR,PCR
	SWI3
	FCB	PSTRNG

	ldx	,s
	clrb
scan	lda	b,x
	cmpa	#CR
	beq	done
	incb
	bra	scan

done	tstb
	beq	HelloWorldX	
	ora	#$80
	sta	b,x

	leax	HWSTR1,PCR
	SWI3
	FCB	PSTRNG

	ldx	,s

	SWI3
	FCB	PSTRNG
	
	leax	HWSTR2,PCR
	SWI3
	FCB	PSTRNG

HelloWorldX:
	CLRB	; No Errors
	PULS	pc,x

HWSTR	FCC /Hello World!/
	FCB CR,LF+$80
HWSTR1	FCC /Hi/
	FCB SP+$80
HWSTR2	FCB CR,LF+$80
