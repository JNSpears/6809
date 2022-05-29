; **********************************************
; PSYMON Extensions VERSION 0.10
;  Re-implemented from Old listings (Ca. 1981)
;
; **********************************************

; SYSTEM ADDRESS CONSTANTS

ROM1    EQU $FC00 ; BASE ADDRESS OF PSYMON ROM
ROM2    EQU $F800 ; BASE ADDRESS OF EXTENSION ROM
RAM     EQU $F380 ; BASE ADDRESS OF SCRATCHPAD RAM;
FREE    EQU $F000 ; ADDRESS OF FREE RAM

; ASCII CHARACTER CONSTANTS

CR EQU $D       ; CARRIAGE RETURN
LF EQU $A       ; LINE FEED
SP EQU $20      ; SPACE


; PSYMON RAM DEFINITIONS
        ORG RAM
; PSYMON INTERNAL STACK & REGISTER SPACE
; OFFSETS TO RAM BASE IN PARENTHESES

        RMB 55 ; STACK SPACE r1
STACK   EQU * ; (55) TOP OF STACK

REGC    RMB 1 ; (55) CONDITION CODE REGISTER

REGA    RMB 1 ; (56) A REGISTER I-‘
REGB    RMB 1 ; (57) B REGISTER
REGD    RMB 1 ; (58) DIRECT PAGE REGISTER
REGX    RMB 2 ; (59) X REGISTER
REGY    RMB 2 ; (61) Y REGISTER M
REGU    RMB 2 ; (63) U STACK POINTER
REGP    RMB 2 ; (65) PROGRAM COUNTER

; PSYMON BREAKPOINT TABLE
BpTabl  RMB 15 ; (67) SPACE FOR 5 BREAKPOINTS
BpTEnd  EQU * ; (82) END OF BREAKPOINT TABLE

; PSYMON WORK AREAS
MemPtr  RMB 2 ; (82) MEMORY POINTER FOR ‘M’ COMMAN1
UsrTbl  RMB 2 ; (84) ADDRESS OF USER COMMAND TABLE)
Comand  RMB 1 ; (86) COMMAND CHARACTER STORAGE
CkSum   RMB 1 ; (87) CHECKSUM FOR LOAD AND SAVE
BegAdd  RMB 2 ; (88) BEGIN ADDRESS FOR SAVE
EndAdd  RMB 2 ; (90) END ADDRESS FOR SAVE
StkPtr  RMB 2 ; (92) CONTENTS OF STACK POINTER

; THE PSYMON CONSOLE DCB
ConDCB  RMB 10 ; (94) STANDARD DCB

; PSYMON DCB POINTERS
DCBCHN  RMB 2 ; (104) BASE OF DCB CHAIN
CIDCB   RMB 2 ; (106) CONSOLE INPUT DCB
CEDCB   RMB 2 ; (108) CONSOLE ECHO DCB
CODCB   RMB 2 ; (110) CONSOLE OUTPUT DCB
TPDCB   RMB 2 ; (112) CASSETTE TAPE DCB

; PSYMON VECTORS
SWI3v   RMB 2 ; (114) SOFTWARE INTERRUPT 3
SWI2v   RMB 2 ; (116) SOFTWARE INTERRUPT 2
FIRQv   RMB 2 ; (118) FAST INTERRUPT REQUEST
IRQv    RMB 2 ; (120) INTERRUPT REQUEST
SWIv    RMB 2 ; (122) SOFTWARE INTERRUPT
NMIv    RMB 2 ; (124) NON-MASKABLE INTERRUPT
FRERAM  RMB 2 ; (126) ADDRESS OF FREE RAM


; DCB offsets
DCBLNK  equ 0   ; pointer to next dcb in chain
DCBDID  equ 2   ; ascii 2 char device Id
DCBDVR  equ 4   ; device driver addr
DCBIOA  equ 6   ; device i/o addr
DCBERR  equ 8   ; error status code
DCBEXT  equ 9   ; number of extension bytes in dcb
DCBAPP  equ 10  ; dcb extension for driver

DCBDRV EQU 10 ;DISK DRIVE # (1-4)
DCBBLK EQU 11 ;DISK BLOCK
DCBBUF EQU 13 ;BUFFER ADDRESS

; Header = Cur(2) + Bak(2) + Nxt(2) + CNT(1) + Addr(2) + TYP(1)

DCBTRK EQU 15 ;TRACK
DCBSEC EQU 16 ;SECTOR
DCBCUR EQU 15 ;CURRENT TRACK/SECTOR
DCBPRV EQU 17 ;PREVIOUS BLOCK
DCBNXT EQU 19 ;NEXT BLOCK
DCBCNT EQU 21 ;BYTE COUNT IN THIS BLOCK
DCBADD EQU 22 ;DATA ADDRESS FOR THIS BLOCK
DCBTYP EQU 24 ;BLOCK TYPE CODE
DCBCRC EQU 25 ;DATA CRC

DCB_Lo EQU 27   ; JNS place to save ending address of Ramdisk
DCB_Hi EQU 29   ; JNS place to save ending address of Ramdisk


; DCB function Codes
ReadFn  equ 1   ; Read function code
WritFn  equ 2   ; Write function code
StatFn  equ 4   ; Status function code
CntlFn  equ 8   ; Device Control function code

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
* ADDRESS OFFSETS USED BY MPX/9
**************************************************
MPXRAM  EQU 256         ; START OF MPX RAM IN 4K BLOCK
M9RAM   EQU 0           ; MINIDOS/9 RAM OFFSET
PSYRAM  EQU $FFDE       ; POINTER TO PSYMON RAM VECTOR
K       EQU 1024        ; CONVENIENCE CONSTANT
DIRSIZ  EQU 2           ; NUMBER OF DIRECTORY BLOCKS
MaxDrv  EQU 4           ; Max number of drives supported
 spc 1

**************************************************
* MPX/9 RAM DEFINITIONS A
**************************************************
 ORG MPXRAM
 spc 1
 RMB 256 ;RESERVED SPACE FOR MPX/9 SYSTEM STACK
STACK9 EQU *
 spc 1
* RESERVED SPACE FOR BOOT PROGRAM
BOOT RMB 128 ;BOOT LIMIT IS 128 BYTES
 spc 1
* DISK PARAMETERS (CAPACITY 3 STEP TIME)
DSKPAR RMB 16
 spc 1
* SYSTEM POINTERS
SYSDCB RMB 2 ;POINTER TO SYSTEM DISK DCB
MDSBAS RMB 2 ;MINIDOS/9 RAM BASE ADDRESS
SCLVEC RMB 2 ;EXTENDED SWI3 CALL VECTOR
DEVLST RMB 2 ;SYSTEM DEVICE LIST POINTER
MPXLOC RMB 2 ;MPX/9 DISK LOCATION
 spc 1
* CONFIGURATION PARAMETERS
SYSBS RMB 1 ;SYSTEM BACKSPACE CODE
SYSBSE RMB 4 ;SYSTEM BACKSPACE ECHO STRING
SYSCAN RMB 1 ;SYSTEM CANCEL CODE
 spc 1
* SYSTEM FILE CONTROL BLOCK
SYSFCB RMB 32 
 spc 1
* MPX/9 LINE BUFFER
LINBUF RMB 64 ;SYSTEM LINE BUFFER
 spc 1
DIRBUF RMB DIRSIZ*256 ;DIRECTORY BUFFER
 spc 1
MPXBAS EQU * ;BASE OF MPX/9 CODE
 spc 1

 spc 1
DCBSIZ EQU 32 ;MAXIMUM DCB SIZE
 spc 1
* FUNCTION CODES
DSKRD EQU 1 ;READ DISK BLOCK
DSKWT EQU 2 ;WRITE DISK BLOCK
DSKWV EQU 3 ;WRITE & VERIFY DISK BLOCK
DSKSL EQU 4 ;SELECT DISK & RETURN STATUS
DSKCT EQU 8 ;DISK CONTROL FUNCTION
 spc 1

**************************************************
* MINIDOS/9 RAM DEFINITIONS (128 BYTES MAX)
**************************************************
 ORG M9RAM
 spc 1
* DRIVE DESCRIPTION TABLE
DDT     RMB 32
 spc 1
* ONE ENTRY PER DRIVE DEFINED AS FOLLOWS!
CURTRK  EQU 0 ;CURRENT TRACK # (HEAD POSITION)
NUMTRK  EQU 1 ;NUMBER OF TRACKS FOR THIS DRIVE
NUMSEC  EQU 2 ;NUMBER OF SECTORS PER TRACK
STEPTM  EQU 3 ;STEP TIME (IN MSEC) FOR THIS DRIVE
SETLTM  EQU 4 ;SETTLE TIME (IN MSEC) FOR THIS DRIVE
SILTBL  EQU 5 ;ADDRESS IN SECTOR INTERLACE TABLE
* ONE BYTE RESERVED FOR EXPANSION
 spc 1
* MINIDOS DISK DCB
DKDCB   RMB  DCBSIZ ;RESERVE SPACE FOR DISK DCB
 spc 1
SYSVEC  RMB 2 ;SYSTEM CALL VECTOR
DBGFLG  RMB 1 ;Debug flag
        IFDEF NEWSYSDCB
SYSDBCn RMB 2 ;NUMBER OF SYSDCB'S (DRIVES) IN SYSTEM
SYSDCBv RMB 2*MaxDrv ; POINTER TO DCB FOR EACH DRIVE
        ENDC
 spc 1

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
* MPX/9 BOOT ENTRY POINT
**************************************************
 ORG $3800
 
RamDiskInit:
        lbra    start
DevID   FCC     'R1'
; size
; drive
; ...

start:  
        PSHS  U,Y,B,A

        ; Init Vars.
	lda	#2
	sta	Drive,pcr

	leay   	RamDisk,pcr     
        STY     RD_Start,pcr   	; set RD_Start to point to RAM for RamDisk

        leay  	RamDiskX,pcr
        STY     RD_End,pcr   	; set DCB_Hi to point to the end of the ramdisk
	
	; swi3
	; fcb	GETWRD
	; beq	noargs
	
	; swi3
	; fcb	HEXNUM		; get Drive #
	; stb	Drive,pcr
	
	; swi3
	; fcb	HEXNUM		; get starting address
	; stb	RD_Start,pcr

	; swi3
	; fcb	HEXNUM		; get ending address
	; stb	RD_End,pcr

noargs:
        leax    RD_DCB,pcr      ; setup and clear the DCB
        LDB     #DCBSIZ         ; Set #bytes in DCB to clear
        TFR     x,y
ClearDCB:
        CLR     ,Y+
        DECB
        BNE     ClearDCB

        ; init the dcb with real data.
        LDD     DevID           ; load d with 'R1' 
        STD     DCBDID,X        ; Store in DCBDID
        LEAU    RamDskDr,PCR    ; load driver address
        STU     DCBDVR,X        ; store in DCBDrv
        LDA     #$16
        STA     DCBEXT,X        ; set DCBEXT bytes to $16
        LEAY    RamDisk,pcr     ; Ramdisk address
        STY     DCBIOA,X        ; set DCBIOA
; make for multiple Ramdisk drives (so I can test copy :-)
        ldy     RD_Start,pcr     
        STY     DCB_Lo,X        ; set DCB_Lo to point to RAM for RamDisk
        ldy     RD_End,pcr
        STY     DCB_Hi,X      	; set DCB_Hi to point to the end of the ramdisk

        SWI3
        FCB     ADDDCB          ; add to DCB list

        IFDEF NEWSYSDCB
; initialize dcb table, used to allow for different drivers for each logical disk drive.
; dcb table should be a WORD length (number of drives)
;       followed by number of drives pointers to the dcb for that driver (or zero)
        LDY     RAMv            ; Y -> BASE OF PSYMON RAM
        LDY     FRERAM-RAM,Y    ; FRERAM ; (126) ADDRESS OF FREE RAM
;        LDD     MonEntv         ; MONITOR RE-ENTRY (from $F380+...)
;not needed done by bootsector        STY     MDSBAS          ; Y -> BASE OF minidos/9 (PSYMON RAM = f000) ; JNS
;        STD     SYSVEC,Y        ; save MonEntv in FRERAM
        CLR     DBGFLG,Y        ; Clear Debug Flag
        IFDEF DBG
;        INC     DBGFLG,Y        ; set debug (comment out)
        ENDC

        leay  	SYSDBCn,y     	; point to space after DKDCB + SYSCAL + DBGFLG. (ie the count(w) of DCB pointers)
	lda	Drive,pcr
	lsla
        stx     a,y 		; set drive 2 to ramdisk ; JNS  make configurable!
        ENDC    

        clrb
        PULS    A,B,Y,U,PC      ; (PUL? PC=RTS)
 spc 1
*** *************************
*** * RamDsk Driver
*** *************************
*** * Entry: A - Driver Parameter
*** *        B - Function Code
*** *        X -> DBC address
*** *           Bytes to read/write DCBCNT,X (0==256)
*** *           data type DCBTYP,X
*** *           ??DCBADD  DATA ADDRESS FOR THIS BLOCK
*** *           DCBBUF,X where to get/put data
*** * Exit: A - Driver Result
*** *       ALL regs perserved, except C
*** *************************
RamDskDr:
        tfr   X,U             ; save DCB for in U for later

        IFDEF DBG
        tst $F000+DBGFLG
        beq RamDskDr_NoDebug
 
        pshs  a,b
 
        lda   #CR
        jsr   [OutChrv]
        lda   #LF
        jsr   [OutChrv]
 
        lda   #'F
        jsr   [OutChrv]
        lda   #'=
        jsr   [OutChrv]
        tfr   B,A
        jsr   [DspSByv]       ; display Function Code
 
        lda   #'B
        jsr   [OutChrv]
        lda   #'=
        jsr   [OutChrv]
        ldd   1,S
        jsr   [DspSByv]       ; display Driver Prameter
 
        lda   #'U
        jsr   [OutChrv]
        lda   #'@
        jsr   [OutChrv]
        lda   #'=
        jsr   [OutChrv]
        LDD   DCBBUF,X        ; D = user buffer
        jsr   [DspDByv]       ; display D (UserBuffer)
 
        lda   #'b
        jsr   [OutChrv]
        lda   #'#
        jsr   [OutChrv]
        lda   #'=
        jsr   [OutChrv]
        LDD   DCBBLK,X        ; D = DCBBLK
        jsr   [DspDByv]       ; display disk block
 
        lda   #CR
        jsr   [OutChrv]
        lda   #LF
        jsr   [OutChrv]

        puls  a,b
        ENDC
        
RamDskDr_NoDebug:
        CLR   DCBERR,X        ; NO ERRORS POSSIBLE
        LDY   DCBBUF,X        ; y -> user buffer
        TFR   X,U

        cmpb  #DSKWT          ; 2 WRITE DISK BLOCK
        lbeq   RamDskWr

        cmpb  #DSKRD          ; 1 READ DISK BLOCK?
        lBEQ   RamDskRd

        cmpb  #DSKWV          ; 3 WRITE & VERIFY DISK BLOCK
        lbeq   RamDskWr        ; JNS For now just write.

        cmpb  #DSKSL          ; 4 SELECT DISK & RETURN STATUS
        beq   RamDskX

        cmpb  #DSKCT          ; 8 DISK CONTROL FUNCTION

;  STA   ,X             ; STORE CONTROL CODE
RamDskX:
        RTS


; Sector = Header + data + CRC
; Header = Cur(2) + Bak(2) + Nxt(2) + CNT(1) + Addr(2) + TYP(1)
; Cnt == 0 --> 265

; the following are sequintial for a reason. it matches the sector header layout.
; DCBCUR EQU 15 ;CURRENT TRACK/SECTOR
; DCBPRV EQU 17 ;PREVIOUS BLOCK
; DCBNXT EQU 19 ;NEXT BLOCK
; DCBCNT EQU 21 ;BYTE COUNT IN THIS BLOCK
; DCBADD EQU 22 ;DATA ADDRESS FOR THIS BLOCK
; DCBTYP EQU 24 ;BLOCK TYPE CODE



; in: u->dcb
; out x->ram disk sector
;       a,b, cc changed
;       CC status GT if disk full.
RamDiskSectorAddr:
; d = a*(256+10+2) 10 bytes header, 2 bytes crc
; d = (a*2) * (256+10+2)/2
; ** only works for small sectors #'s (0-127)
        pshs    Y
        lda     DCBBLK+1,U      ; a <- low byte of block (sector) address
        lsla                    ; a <- a*2
        ldb     #134            ; b <- (256+10+2)/2
        mul                     ; d < a * b
        LDX     DCB_Lo,U        ; get start of ramdisk
        leax    D,X             ; x -> sector in ramdisk

        IFDEF DBG1
        tst $F000+DBGFLG
        beq RamDiskSectorAddrX
                
        pshs A,B
        lda   #'r
        jsr   [OutChrv]
        lda   #'=
        jsr   [OutChrv]
        tfr     X,D
        jsr   [DspDByv]       ; display ramdriver sector address
        lda   #CR
        jsr   [OutChrv]
        lda   #LF
        jsr   [OutChrv]
        puls    A,B

        tst DBGFLG,U
        beq RamDiskSectorAddrX
        pshs A,B
        lda   #'r
        jsr   [OutChrv]
        lda   #'=
        jsr   [OutChrv]
        tfr     X,D
        jsr   [DspDByv]       ; display ramdriver sector address

        lda   #'E
        jsr   [OutChrv]
        lda   #'=
        jsr   [OutChrv]
        LDD     DCB_Hi,U
        jsr   [DspDByv]       ; display ramdriver sector address

        lda   #CR
        jsr   [OutChrv]
        lda   #LF
        jsr   [OutChrv]
        puls    A,B
        ENDC

RamDiskSectorAddrX:
        CMPX    DCB_Hi,U        ; check and see if after RamDiskX
        puls    Y,PC

RamDskRd:
; In: U,X -> DCB, Y -> Userbuffer
        Lbsr   RamDiskSectorAddr         ; ret x-> ramdisk sector

        ; READ HEADER
        LEAY  DCBCUR,U
        LDD   #DCBCRC-DCBCUR  ; length of header (10 bytes)
        bsr   BLKMV           ; block move X -> Y length D
        LEAX  D,X             ; increment X to point to data in sector;

        ; READ DATA
        clra
        ldb   DCBCNT,U
        bne   RamDskRd1
        INCA
RamDskRd1:
        LDY   DCBBUF,U        ; Y -> user buffer
        CMPY  #$FFFF
        BNE   RamDskRd2
        LDY   DCBADD,U
RamDskRd2:
        bsr   BLKMV           ; block move X -> Y length D
        LEAX  D,X             ; increment X to point to CRC in sector;

        ; READ CRC
        LEAY  DCBCRC,U        ; X -> CRC value
        LDD   #2              ; length of CRC Value
        bsr   BLKMV           ; block move X -> Y length D

        ; Calc crc
        bsr   CRC
        CLRA

        ; D == CRC
        LEAX  DCBCRC,U        ; X -> CRC value
;JNS    jsr   [DspDByv]         ; JNS
        CMPD  ,X
        BEQ   RamDskRdX
        ; set error
        ldd     DCBCRC,U
;JNS    jsr   [DspDByv]         ; JNS
        lda     #7      ; Disk Read Error
        sta     DCBERR,U
RamDskRdX:
        RTS

; Header = Cur(2) + Bak(2) + Nxt(2) + CNT(1) + Addr(2) + TYP(1)

SetUpPtrUserBuffer:
        LDY   DCBBUF,U
        CMPY  #$FFFF
        BNE   SetUpPtrUserBufferX
        LDY   DCBADD,U
SetUpPtrUserBufferX:
        RTS

*** ; Calculate CRC code
; In X->DCB
; Out B = crc value
CRC:
        BSR   SetUpPtrUserBuffer
        CLRA           ; clear TempValue
        PSHS  A        ; make space on stack for TempValue
        LDB   DCBCNT,U ; Get number of bytes in uffer
        BSR   CX       ; crc Y->UserBuffer, B=Length

        LEAY  DCBCUR,U
        LDB   #DCBCRC-DCBCUR
        BSR   CX       ; crc Y->header, B=HeaderLength
        PULS  B,PC ;(PUL? PC=RTS)

; B=length (0=265); Y -> buffer; S -> <retaddr>, BYTE TempValue
CX:
        EORA  ,Y+
        ASLA
        ROL   2,S
        BCC   C1
        INCA
C1:
        DECB
        BNE   CX
        RTS


**************************************************
* SYSTEM CALL 28 - BLOCK MOVE
*
* ENTRY REQUIREMENTS: X POINTS TO SOURCE FIELD
*                     Y POINTS TO DEST. FIELD
*                     D CONTAINS LENGTH
*
* EXIT CONDITIONS: CC REGISTER CHANGED
*                  OTHERS PRESERVED
**************************************************
BLKMV PSHS D,X,Y SAVE REGISTERS

;*****************************
;* Software Vectors
;*****************************
;RAMv           equ $ffde
;DspSByv                equ $ffe0
;DspDByv                equ $ffe2
;GetHexv                equ $ffe4
;PStringv       equ $ffe6
;InChrv         equ $ffe8
;OutChrv                equ $ffea
;ReqIOv         equ $ffec
;MonEntv                equ $ffee

        ; pshs A,B
        ; lda   #'X
        ; jsr   [OutChrv]
        ; lda   #'=
        ; jsr   [OutChrv]
        ; tfr   X,D
        ; jsr   [DspDByv]       ; display ramdriver sector address
 
        ; lda   #'Y
        ; jsr   [OutChrv]
        ; lda   #'=
        ; jsr   [OutChrv]
        ; tfr   Y,D
        ; jsr   [DspDByv]       ; display ramdriver sector address
 
        ; lda   #'D
        ; jsr   [OutChrv]
        ; lda   #'=
        ; jsr   [OutChrv]
        ; LDD   ,S
        ; jsr   [DspDByv]       ; display ramdriver sector address
 
        ; lda   #CR
        ; jsr   [OutChrv]
        ; lda   #LF
        ; jsr   [OutChrv]
        ; puls  A,B

 CMPD #256 LESS THAN 1 PAGE TO MOVE?
 BLS BLKMV2 GO IF YES
 TSTB IS LENGTH 0 MOD 256?
 BNE BLKMV1 GO IF NOT
 DECA ADJUST PAGE COUNTER
BLKMV1 PSHS D SAVE COUNTERS
 CLRB MOVE 256 BYTES
 BSR MOVSTR
 PULS D
 DECA LOOP THROUGH PAGES
 BNE BLKMV1
BLKMV2 BSR MOVSTR MOVE LAST STRING
 PULS D,X,Y,PC RESTORE & EXIT
 spc 1
*
* MOVE STRING 1 (X) TO STRING 2 (Y) LENGTH B
*
MOVSTR:
 LDA ,X+ MOVE A BYTE
 STA ,Y+
 DECB LOOP THROUGH B
 BNE MOVSTR
 RTS
 spc 1


RamDskWr:
; In: U,X -> DCB, Y -> Userbuffer
        lbsr    RamDiskSectorAddr         ; ret x-> ramdisk sector
        bhi     RamDskWrFull
        TFR     x,y

        ; WRITE HEADER
        LEAX  DCBCUR,U
        LDD   #DCBCRC-DCBCUR  ; length of header (10 bytes)
        bsr   BLKMV           ; block move X -> Y length D
        LEAY  D,Y             ; increment Y to point to data in sector;

        ; WRITE DATA
        clra
        ldb   DCBCNT,U
        bne   RamDskWr1
        inca
RamDskWr1:
        LDX   DCBBUF,U        ; X -> user buffer
        CMPY  #$FFFF
        BNE   RamDskWr2
        LDY   DCBADD,U
RamDskWr2:
        lbsr   BLKMV          ; block move X -> Y length D
        LEAY  D,Y             ; increment Y to point to CRC in sector;

        ; Calc crc
        PSHS    Y
        lbsr    CRC
        PULS    Y
        CLRA
        STD     DCBCRC,U
        
;JNS    jsr   [DspDByv]         ; JNS
        
        ; WRITE CRC
        LEAX    DCBCRC,U        ; X -> CRC value
        LDD     #2              ; length of CRC Value
        lbsr    BLKMV         ; block move X -> Y length D
        bra     RamDskWrX
RamDskWrFull:
        lda     #11             ; ramdisk full (DF)
        sta     DCBERR,U
RamDskWrX:
        RTS

RamDskSt:
        clra                  ; GET STATUS
        RTS

RD_Start	rmb	2
RD_End		rmb	2
Drive		rmb	1



StartRamDisk    EQU     $D000 ; (*|$ff)+1
                ORG     StartRamDisk
RD_DCB          rmb     DCBSIZ

RamDisk:        RMB     $2000-(StartRamDisk&$0fff)
RamDiskX        EQU *-1
RamDiskSizeB    EQU RamDiskX-RamDisk
RamDiskSizeSect EQU RamDiskSizeB/(256+12+2)