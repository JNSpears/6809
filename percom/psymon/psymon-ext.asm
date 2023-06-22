; **********************************************
; PSYMON Extensions VERSION 0.10
;  Re-implemented from Old listings (Ca. 1981)
;
; **********************************************

; SYSTEM ADDRESS CONSTANTS

ROM1    EQU $FC00 ; BASE ADDRESS OF PSYMON ROM
ROM2    EQU $F800 ; BASE ADDRESS OF EXTENSION ROM
;RAM    EQU $F380 ; BASE ADDRESS OF SCRATCHPAD RAM;
FREE    EQU $F000 ; ADDRESS OF FREE RAM

; ASCII CHARACTER CONSTANTS

CR EQU $D   ; CARRIAGE RETURN
LF EQU $A   ; LINE FEED
SP EQU $20  ; SPACE

; ACIA CONTROL CONFIGURATIONS

RESET   EQU $03 ; RESET ACIA
CONFIG  EQU $51 ; SET FOR 8 DATA, 2 STOP, NO PARITY,
RDRON   EQU CONFIG-$40  ; READER ON (RTS ON)
RDROFF  EQU CONFIG  ; READER OFF (RTS OFF)

; PSYMON RAM DEFINITIONS
    ORG RAM
; PSYMON INTERNAL STACK & REGISTER SPACE
; OFFSETS TO RAM BASE IN PARENTHESES

    RMB 55 ; STACK SPACE r1
STACK   EQU * ; (55) TOP OF STACK

REGC    RMB 1 ; (55) CONDITION CODE REGISTER

REGA    RMB 1 ; (56) A REGISTER I-�
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
MemPtr  RMB 2 ; (82) MEMORY POINTER FOR �M� COMMAN1
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
DCBLnk  equ 0   ; pointer to next dcb in chain
DCBDId  equ 2   ; ascii 2 char device Id
DCBDvr  equ 4   ; device driver addr
DCBIOA  equ 6   ; device i/o addr
DCBErr  equ 8   ; error status code
DCBExt  equ 9   ; number of extension bytes in dcb
DCBApp  equ 10  ; dcb extension for driver

DCBDRV EQU 10 ;DISK DRIVE # (1-4)
DCBBLK EQU 11 ;DISK BLOCK
DCBBUF EQU 13 ;BUFFER ADDRESS

; DCB function Codes
ReadFn  equ 1   ; Read function code
WritFn  equ 2   ; Write function code
StatFn  equ 4   ; Status function code
CntlFn  equ 8   ; Device Control function code

*****************************
* Software Vectors
*****************************
RAMv        equ     $ffde
DspSByv     equ     $ffe0
DspDByv     equ     $ffe2
GetHexv     equ     $ffe4
PStringv    equ     $ffe6
InChrv      equ     $ffe8
OutChrv     equ     $ffea
ReqIOv      equ     $ffec
MonEntv     equ     $ffee

RAM             equ     $F380   ; BASE OF PSYMON RAM
DspSBy          equ     $FD73   ; DISPLAY SINGLE BYTE ON CONSOLE
DspDBy          equ     $FD6A   ; DISPLAY DOUBLE BYTE ON CONSOLE
PsyGetHex          equ     $FD0E   ; GET HEX NUMBER FROM CONSOLE
PString         equ     $FD97   ; PRINT STRING TO CONSOLE
InChr           equ     $FD44   ; INPUT CHARACTER FROM CONSOLE
OutChr          equ     $FD58   ; OUTPUT CHARACTER TO CONSOLE
ReqIO           equ     $FD63   ; PERFORM I/O REQUEST
MonEnt          equ     $FC32   ; MONITOR RE-ENTRY

OutSp           equ     $FD75
CRLF            equ     $FDA2
; for new S1 loader
InByte          equ $FCF4

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

* FUNCTION CODES
DSKRD EQU 1 ;READ DISK BLOCK
DSKWT EQU 2 ;WRITE DISK BLOCK
DSKWV EQU 3 ;WRITE & VERIFY DISK BLOCK
DSKSL EQU 4 ;SELECT DISK & RETURN STATUS
DSKCT EQU 8 ;DISK CONTROL FUNCTION

; PSYMON-ext ROM CODE
    IFDEF   RAMTGT ; Build for Ram on SBC/9

UsrTblv EQU $F3D4
    ORG UsrTblv
    FDB ExtTbl
BUFFER  EQU $0000
    ORG $F050

    ELSE
    IFDEF MPX9 ; Build for MPX/9
S19LOADER       SET 1
DSKUTL          SET 1
MEMTST1         SET 1
UsrTblv EQU $F3D4
    ORG UsrTblv
    FDB ExtTbl
BUFFER  EQU $DE00
    ORG $0000

    ELSE ; build for EPROM on SBC/9
;S19LOADER   SET 0
DSKUTL      SET 1
;MEMTST1     SET 0
HELPHELP    SET 1
LIST_DCB    SET 1
NULL_DCB    SET 1
            ORG     ROM2
BUFFER      EQU FREE+$0100
    ENDC ; build for EPROM on SBC/9

    ENDC ; RAMTGT

begcod  equ *

**********************************************************
* Psymon-Ext Initialization
**********************************************************
Init:
    JMP Setup
; Begin Vectors
    FDB DumpMem2
; End Vectors
Setup:
    leax    <ExtTbl,pcr ; Install user command table
    STX     UsrTbl

    ifdef   NULL_DCB
;AddNullDev:
    leax     >NullDCB,pcr
    leay    <NullDCB_INIT,pcr

    ldd     ,Y++
    std     ,X++
    ldd     ,Y++
    std     ,X++
    ldd     ,Y++
    std     ,X++
    ldd     ,Y++
    std     ,X++
    ldd     ,Y++
    std     ,X++

    leay     >NullDCB,pcr
    ldx     DCBCHN
    stx     DCBLnk,y
    sty     DCBCHN
    endc

    RTS


    IFDEF   NULL_DCB
NullDCB_INIT:
    FDB 0
    FCC 'NL'    ; DCB ID
    FDB NullDr  ; DRIVER
    FDB 0       ; I/O ADDRESS
    FDB 0       ; STATUS, EXT
    ENDC

*** *************************
*** * Null Driver
*** *************************
NullDr:
    CLR     DCBErr,X    ; NO ERRORS POSSIBLE
    CLRB                ; CLR STATUS
    CLRA                ; GET CHARACTER
    RTS
    endc
*** *************************
*** * PSYMON-Ext CMD TABLE
*** *************************
ExtTbl:

    fcb 1       ; Length of command string

    fcb 'C
    fdb CopyMem
    fcb 'F
    fdb FillMem
    fcb 'D
    fdb DmpMem

    ; fcb   'X
    ; FDB   DESEMB

    fcb 'H
    FDB HexMath

    ifdef LIST_DCB
    fcb '#
    fdb ListDCBs
    endc ; ifdef LIST_DCB

    fcb 'J
    fdb JumpSubr

    IFDEF DSKUTL
    fcb 'T
    fdb ReadSect
    ; fcb   'W
    ; FDB   WriteSect
    ENDC

    ; fcb   '!
    ; FDB   AddNullDev
    IFDEF   MEMTST1
    FCB 'Q
    FDB MEMTST
    ENDC

    ifdef   S19LOADER
    FCB     'L
    FDB     TLoad
    endc

    ifdef   HELPHELP
    FCB     '?
    FDB     Help
    endc

    FCB     'Q
    FDB     QUIT

    FCB     'Z
    FDB     $C000

    fcb $ff     ; End of table

**********************************************************
* Psymon-Ext memory dump command
**********************************************************
DmpMem:
    JSR GetBegEndAddr   ; results inBegAdd and EndAdd
;   LDD BegAdd
;   ANDB    #$f0        ; make 16 byte allined
;   STD BegAdd
;   TFR D,X
    BRA DmpMemStart
; todo add alt entry point takes x-> memory, D=length

DumpMem2:
        ; entry
        ;       x-> memory
        ;       d=length

        stx     BegAdd
        leax    D,X
        leax    -1,X
        stx     EndAdd
;        ldx     BegAdd
DmpMemStart:
        ldx     BegAdd          ; put starting address in X
    ldd EndAdd
    pshs    D       ; put ending address on stack

LineLoop:
    jsr CRLF
    tfr X,D
    jsr DspDBy          ; Display Address
    lda #':
    jsr OutChr          ; display ':'
    jsr OutSp
    pshs    X
    ldb #16
HexLoop:
    lda ,X+
    jsr     DspSBy
    cmpb    #9
    bne NotHalfway
    jsr OutSp
NotHalfway:
    decb
    bne HexLoop
    jsr OutSp
    puls    x
    ldb #16
AsciiLoop:
    lda ,X+
    cmpa    #SP
    blt NonPrintable
    cmpa    #$7f
    blt Printable
NonPrintable:
    lda #'.
Printable:
    jsr     OutChr
    decb
    bne AsciiLoop
;   leay    -1,X

        ; check for kb char.
;        ldb #StatFn
;        ldx CIDCB
;       JSR ReqIO ; check driver status
;       LSRB         ; read buffer full BIT TO C
;        BCs   dumpx  ; if so we are done.

    cmpx    ,s
    blo LineLoop
dumpx
    leas    2,s
    RTS

*****************************************
; accept [ <hexnumber> | X | Y | U | S ] from console.
; return actual value entered or register value.
;
GetHex:
    jsr PsyGetHex
    bne GetHexX ; a hex number was entered (hex digits <> 0)
    ldx StkPtr  ; x -> stack register image
    cmpa    #'X
    bne n1
    ldx <4,x    ; get X register from stack
    bra GetHexXok
n1:
    cmpa    #'Y
    bne n2
    ldx <6,x    ; get Y register from stack
    bra GetHexXok
n2:
    cmpa    #'U
    bne n3
    ldx <8,x    ; get U register from stack
    bra GetHexXok
n3:
    cmpa    #'S
    beq GetHexXok
    clrb
    rts
GetHexXok
    jsr OutSp
    ldb #1  ; make system think user entered hex number.
GetHexX:
    rts

*****************************************
;   JSR GetHex  ; get Drive and Sector --> X
;   BNE foo
;   cmpa    #',
;   bne ReadSectX
;   ldx #1      ; flag to read next sector.


GetBegEndAddr:
    bsr     GetHex      ; get start address (or index reg value)
    bne     SaveBegAdd  ; if HEX DIGITS ENTERED then go get other addresses
    CMPA    #CR
    BEQ     XNoAddr
    cmpa    #',         ; comma?
    bne     ChkEndAddr  ; no? then check for end addr.
    ; add length of last D command to the Beg/End addr's
    ldd     EndAdd      ; get the old ending address
    tfr     D,X         ; save in X
    subd    BegAdd      ; calc size of old dump
    addd    EndAdd      ; calc new ending address
    stx     BegAdd      ; BegAdd <-- old EndAdd
    std     EndAdd      ; EndAdd <-- old EndAdd+(old EndAdd - old BegAdd)
    rts                 ; we are done, return
SaveBegAdd
    STX     BegAdd      ; save as beginning address
ChkEndAddr
    cmpa    #'.         ; delimiter a '.' repeat current dump...
    beq     XNoAddr
    cmpa    #SP         ; delimiter a space (or less)
    blt     XNoEndAddr
    JSR     GetHex      ; get hex value
    BNE     XEndAddr    ; if none then go
XNoEndAddr:
    LDX     BegAdd      ; default ending address to be beginning address + 1
    INCB
XEndAddr:
    STX     EndAdd      ; save as ending address
XNoAddr:
    RTS

*****************************************
QUIT:
    FCB     $02         ; Invalid instruction causes trap to USIM debuger.
    RTS

;;
;; Disassembler (Sortof) lifted from PETER A. STARK's HumBug09
;;

; DESEMB:
    ; BSR   GetBegEndAddr
    ; LDX   BegAdd
    ; PSHS  X,B,A       ; MAKE ROOM on stack for local vars.
; DES2  BSR     PRNTOP      ; GO TO PRINT CURRENT LINE
    ; LDD   EndAdd
    ; SUBD  2,S
    ; BCC   DES2        ; RETURN IF NEXT <= LAST
    ; PULS  A,B,X
    ; RTS           ; OTHERWISE EXIT

; PRNTOP - SUBR TO PRINT ADDRESS AND CURRENT INSTR (opcodes and Operands)

; PRNTOP:   JSR     CRLF
    ; LDX   4,S         ; POINT TO NEXT ADDRESS
    ; TFR   X,D
    ; JSR   DspDBy  ; print address
    ; JSR   SPACE
    ; CLRB
    ; CLR   2,S         ; PREFIX
    ; LDX   4,S         ; GET ADDRESS OF INSTR
    ; LDA   0,X         ; GET OPERATION CODE
; PROP1     STA     3,S         ; SAVE OPCODE
    ; PSHS  B
    ; JSR   DspSBy  ; print opcode byte
    ; LEAX  1,X     ; Inc X
    ; STX   5,S         ; SAVE ADDRESS
    ; PULS  B
    ; LDA   3,S         ; GET OP CODE
    ; CMPA  #$10
    ; BCS   NOT10
    ; CMPA  #$12
    ; BCC   NOT10
    ; STA   2,S         ; STORE 10 OR 11 PREFIX
    ; LDA   ,X      ; GET NEXT opcode byte
    ; CMPA  #$30
    ; BCC   PROP1       ; IF NOT LONG BRANCH
    ; INCB
    ; BRA   PROP1       ; FOR LONG BRANCH
; NOT10 ANDA    #$FC
    ; CMPA  #$30
    ; BEQ   INDEX       ; INDEXED INSTR
    ; LSRA
    ; LSRA
    ; LSRA
    ; LSRA
    ; BEQ LENTH2        ; OP 0X
    ; DECA
    ; BEQ LOOK1         ; 1X
    ; DECA
    ; BEQ LENTH2        ; 2X
    ; DECA
    ; BEQ LOOK3         ; 3X
    ; DECA
    ; BEQ LENTH1        ; 4X
    ; DECA
    ; BEQ LENTH1        ; 5X
    ; DECA
    ; ANDA #$03
    ; BEQ INDEX         ; 6X,AX,EX
    ; DECA
    ; BEQ LENTH3        ; 7X,BX,FX
    ; DECA
    ; BEQ LOOK8C        ; 8X,CX
    ; BRA LENTH2        ; 9X,DX
; LOOK1 LDA 3,S             ; GET OP CODE
    ; ANDA #$0F
    ; CMPA #$05
    ; BCS LENTH1
    ; CMPA #$09
    ; BCS LENTH3
    ; BEQ LENTH1
    ; CMPA #$0D
    ; BEQ LENTH1
    ; BRA LENTH2
; LOOK3 LDA 3,S         ; GET OP CODE
    ; BITA #$08
    ; BEQ LENTH2
    ; ANDA #$03
    ; BEQ LENTH2
    ; BRA LENTH1
; LOOK8C    LDA 3,S         ; GET OP CODE
    ; ANDA #$0F
    ; CMPA #$03
    ; BEQ LENTH3
    ; ANDA #$0D
    ; CMPA #$0C
    ; BEQ LENTH3
    ; BRA LENTH2
; INDEX LDA 0,X         ; GET POST-BYTE
    ; CMPA #$9F
    ; BEQ LENTH4
    ; ANDA #$8B
    ; CMPA #$88
    ; BEQ LENTH3
    ; CMPA #$89
    ; BNE LENTH2
; LENTH4    INCB            ; 4-BYTE
; LENTH3    INCB            ; 3-BYTE
; LENTH2    INCB            ; 2-BYTE
; LENTH1    TSTB            ; 1-BYTE
    ; BEQ   POP3        ; No bytes after opcode
    ; DECB
    ; BEQ   POP1        ; 1 Byte after opcode
    ; DECB
    ; BEQ   POP4        ; 2 bytes after opcode
    ; LDA   ,X+
    ; JSR   DspSBy  ; print operand byte
; POP4  LDD     ,X++
    ; JSR   DspDBy  ; print operand word
    ; BRA   POP2
; POP1  LDA     ,X+
    ; JSR   DspSBy  ; print operand byte
; POP2  STX     4,S     ; Store new address
; POP3  RTS

*****************************************
;
; C <FROM> <TO> <length>
;
CopyMem:
    bsr    GetBegEndAddr ; get from, to
    jsr     GetHex      ; Get length
    beq     BLKMVx      ; error? then bail

    tfr     X,D         ; set up LENGTH
    ldx     BegAdd      ; set up x FROM address
    ldy     EndAdd      ; set up y TO address

; MoveMem   move memory from X to Y len D
; lifted from mpx9
BLKMV:
    CMPD    #256 ; LESS THAN 1 PAGE TO MOVE?
    BLS     BLKMV2  ; GO IF YES
    TSTB        ; IS LENGTH 0 MOD 256?
    BNE     BLKMV1  ; GO IF NOT
    DECA        ; ADJUST PAGE COUNTER
BLKMV1
    PSHS    D   ; SAVE COUNTERS
    CLRB        ; MOVE 256 BYTES
    BSR     MOVSTR
    PULS    D
    DECA        ; LOOP THROUGH PAGES
    BNE     BLKMV1
BLKMV2  
    ; fall through for move final 256 or less bytes.
*
* MOVE STRING 1 (X) TO STRING 2 (Y) LENGTH B
*
MOVSTR:
    LDA     ,X+     ; MOVE A BYTE
    STA     ,Y+
    DECB        ; LOOP THROUGH B
    BNE     MOVSTR
BLKMVx
    rts


*****************************************
;
; F <start> <end> <value1> [ <value2> ... ]
;
FillMem:

    jsr     GetHex      ; Get start address arg
    beq     FillMemX    ; error? then bail - no cleanup needed
    clra
    pshs    A,X,Y       ;  push A(+0;counter), X(+1;start addr) Y(+3;ending addr) on stack -- stack cleanup is +5 now.
    tfr     x,y         ; save srarting address in Y

    jsr     GetHex      ; Get end address arg
    beq     FillMemX1   ; error? then bail
    stx     3,S         ; save on stack


FillMem1:
    jsr     GetHex  ; Get value arg
        ; *** * Exit: A - last char input
        ; *** *       B - hex digit count
        ; *** *       X - Hex number
        ; *** *       C - set according to B
    beq     FillMem2
    pshs    A
    tfr     x,d
    stb     ,y+     ; save the value byte in the dest buffer
    inc     1,s     ; increment count
    puls    A
    cmpa    #CR
    bne     FillMem1
FillMem2:
    ldd     3,s ; get end address
    subd    1,s ; sub start address
    subb    ,s  ; sub number of bytes loaded already
    sbca    #0
    addd    #1
    BEQ     FillMemX1 ; all data in memory already
    ldx     1,s ; x -> from for copy
            ; y already points to the right place.
            ; move x -> y length D
    bsr     BLKMV
FillMemX1:
    leas    5,s ; clean up stack.
FillMemX:
    rts

*****************************************
;
; jump to sub. setup all registers same as G, on return save all registers for user.
; J [ addr ]
;
JumpSubr:
    leas    2,s         ; drop psymon return address
    puls    cc,a,b,dp,x,y,u ; temporaraly restore registers
    pshs    d           ; make space for return address (Rt2Mon)
    pshs    cc,a,b,dp,x,y,u ; put all registers back.
    ldd     12,s        ; get pc from stack
    std     10,s        ; duplicate into pc that will be used by the RTI
    jsr     GetHex      ; Get start address arg
    beq     JumpSubr1   ; go if not data entered in X
    stx     10,s        ; put in stack image
JumpSubr1
    leax    <Rt2Mon,pcr
    stx     12,s        ; put in stack, for user code to use fw/ rts
    LDA     ,S          ; SET 'E' FLAG IN CC
    ORA     #$80
    STA     ,S
    RTI
    ; rts from user code puts us here. without PC on stack
Rt2Mon:
    pshs    cc,a,b,dp,x,y,u,pc  ; put all registers back.
    jmp $fc36       ; goto main command loop.

;TEST:
;    LDA     #$77
;    LDX #$8888
;    RTS

*****************************************
;
; list dcb's
; #
;
    ifdef LIST_DCB
ListDCBs:
    ldx DCBCHN
    jsr CRLF
ListDCB1:
    lda DCBDId,x    ; display DCB ID
    jsr OutChr
    lda DCBDId+1,x
    jsr OutChr

    tfr x,y
    leax    <prefix,pcr
    jsr PString

    tfr y,d
    jsr DspDBy  ; display DCB address

    jsr PString
    lda DCBErr,y
    jsr DspSBy  ; display DCB Err Status

    jsr PString
    ldD DCBDvr,y
    jsr DspDBy  ; display DCB Dvr address

    jsr PString
    ldd DCBIOA,y
    jsr DspDBy  ; display DCB I/O address

    ldb DCBExt,y
    beq noDbcExt

    leaX DCBApp,y   ; point to extension bytes
    LDA #'[
    jsr OutChr

DbcExtLoop
    lda ,X+
    jsr DspSBy  ; display DCB extension bytes
    decb
    bne DbcExtLoop ; not done then loop
    LDA #']
    jsr OutChr

noDbcExt:
    jsr CRLF
    ldx DCBLnk,y
    bne ListDCB1
    rts

prefix:
    fcs / @=/
    fcs /St=/
    fcs /Dr@=/
    fcs /IO@=/

    endc ; ifdef LIST_DCB

*****************************************
;
; h <number> <number> (+|-)
;

HexMath:
    JSR     GetHex  ; Get first arg
    PSHS    X       ; save on stack
    JSR     GetHex  ; get second arg
    CMPA    #'-     ; subtract? or add by default
    BEQ     HexMathSub
    tfr     X,D
    addd    ,S++    ; add D + [S] => D
    bra     HexMathDsp
HexMathSub:
;    tfr     x,d
;    puls    X
;    pshs    D
;    TFR     X,D
    puls    d
    pshs    x
    subd    ,s++    ; sub D - [S] => D
HexMathDsp:
;    JSR     CRLF
    jmp     DspDBy  ;display data in D & return


        IFDEF DSKUTL
*** *************************
*** * T dsss [ dss ] : Read sector sss drive d into BUFFER and display it f
*** *************************
; WriteSect:
    ; JSR   GetHex  ; get Drive and Sector --> X
    ; BEQ   ReadSectX
    ; ldb   #DSKWT
    ; ; note confirm write.
    ; bra   commSect

ReadSect:
    LBSR    GetBegEndAddr
    BEQ     ReadSectX2
    LDD     BegAdd
    LDX     EndAdd
    pshs    D,X     ; x(EndAdd) Next-on-Stack (2,s) d(BegAdd) TOS (,s)

;   JSR GetHex  ; get Drive and Sector --> XQ
;   BNE foo
;   cmpa    #',
;   bne ReadSectX
;   ldx #1      ; flag to read next sector.
;foo
ReadSectLoop:
    LEAX    SectMsg,PCR
    JSR     PString
    LDD     ,S
    jsr     DspDBy
    ldb     #DSKRD
;commSect:
    pshs    b

    LDY     RAMv            ; Y -> BASE OF PSYMON RAM
    LDY     FRERAM-RAM,Y    ; FRERAM ; (126) ADDRESS OF FREE RAM

    leay    $20,Y           ; adjust offset for #DK DCB

    ; set the sector (block) to load
    LDD     1,s ; get sector from <start> DSSS format
    anda    #$0f    ; mask to be lower 12 bits
    std     DCBBLK,Y ; save i

    ; set the drive to load from
    LDD     1,s ; get drive from <start> DSSS format
    lsra
    lsra
    lsra
    lsra
;   tsta        ; if a == 0 then special ',' case
;   beq fum ; skip storing drive number
    sta     DCBDRV,Y
;fum
    ; point to the buffer to load to
    leax    >buffer,pcr
    stx     DCBBUF,Y

    clra
    puls    b   ; get dskrd/dskwr parameter

    tfr     y,x
    CLR     DCBErr,Y    ; DCBErr ERROR STATUS CODE
    JSR     ReqIO
    TST     DCBErr,Y    ; DCBErr ERROR STATUS CODE

    BEQ     OK      ; No then we are good
ERROR:
    LEAX    <ErrorString,PCR
    JSR     PString
    LDA     DCBErr,Y
    jsr     DspDBy
    ; now display data anyway.
OK:
    tfr     y,x
    ldd     #32
    lbsr    DumpMem2
    lbsr    CRLF

    leax    >buffer,pcr
    ldd     #256
    lbsr    DumpMem2
    lbsr    CRLF

    ; increment sector and check to see if we reached the end.
    ldd     ,s  ; Begining sector address
    addd    #1
    std     ,s
    cmpd    2,s ; ending sector address
    BLE     ReadSectLoop
ReadSectX:
    leas    4,s
ReadSectX2:
    rts

ErrorString:
    fcb CR,LF
    fcc "*** DISK Error"
    fcb SP+$80
SectMsg fcs ">>> Sector:"
        ENDC

    IFDEF   MEMTST1
; ***** "Q" MEMORY TEST *****
; lifted from SBUG http://www.swtpc.com/mholley/MP_09/SBUG_Listing.pdf
MEMTST      
    CLR ,-S     ; CLEAR BYTE ON STACK
    CLR ,-S     ; CLEAR ANOTHER BYTE
    LBSR GetBegEndAddr  ; GET BEGIN(Y) & END(X) ADDR. LIMITS
    ldy BegAdd
    ldx EndAdd
    PSHS X,Y    SAVE ADDRESSES ON STACK
    tstb
    beq ADJSK6  EXIT IF NOT VALID HEX
    CMPX 2,S    COMPARE BEGIN TO END ADDR.
    BCS ADJSK6  EXIT IF BEGIN > END ADDR.
    LBSR OutSp  OUTPUT SPACE
MEMSET  TFR Y,D     PUT BEGIN ADDR. IN 'D'-ACCUM.
    ADDD 4,S    ADD PASS COUNT TO BEGIN ADDR
    PSHS B      ADD LS BYTE TO MS BYTE OF BEGIN ADDR
    ADDA ,S+
    STA ,Y+     SAVE THIS DATA BYTE AT BEGIN ADDR
    CMPY ,S     COMPARE END TO BEGIN ADDR
    BCS MEMSET  IF BEGIN LOWER, CONTINUE TO SET MEMORY
    LDY 2,S     RELOAD BEGIN ADDRESS
TEST1   TFR Y,D     PUT BEGIN ADDR IN 'D'-ACC.
    ADDD 4,S    ADD PASS COUNT TO ADDRESS
    PSHS A      ADD MS BYTE TO LS BYTE OF ADDRESS
    ADDB ,S+
    EORB ,Y+    EX-OR THIS DATA WITH DATA IN MEMORY LOC.
    BEQ GUDPAS  IF (Z) SET, MEMORY BYTE OK
    LDX #MSG5   POINT TO MSG " - "
    JSR PString

    LEAX -1,Y   GET ERROR ADDRESS IN X-REG
    JSR DspDBy  OUTPUT IT
    PSHS X      PUSH ERROR ADDR ON STACK
    LDX #MSG8   POINT TO MSG " =>"
    JSR PString

    PULS X      POP ERROR ADDR FROM STACK
;   LBSR LRA    GET PHYSICAL ADDR FROM LRA
;   LBSR XASCII     OUTPUT EXTENDED 4 BITS OF PHYSICAL ADDR
    jsr DspDBy  OUTPUT ADDR
    LDX #MSG6   POINT TO MSG ", PASS "
    JSR PString

    LDX 4,S     LOAD PASS COUNT
    jsr DspDBy  OUTPUT IT
    LDX #MSG7   POINT TO MSG ", BITS IN ERROR
    JSR PString

    TFR B,A     GET ERROR BYTE INTO A-ACC
;   LDX #MSG9   POINT TO MSG "76543210"
;   LBSR BIASCI     OUTPUT IN BINARY/ASCII FORMAT
;   LBSR INCHEK     CHECK FOR INPUT FROM KEYBOARD $FA56
;   BNE ADJSK6  IF SO, EXIT MEMORY TEST
GUDPAS  CMPY ,S     COMPARE END ADDR TO BEGIN ADDR
    BCS TEST1
    LDA #'+     GET "PASS" SYMBOL IF MEMORY PASS OK
    jsr OutChr  OUTPUT SYMBOL TO TERMINAL
;   LBSR INCHEK     INPUT FROM KEYBOARD?
;   BNE ADJSK6  IF SO, EXIT MEMORY TEST
    LDY 2,S     LOAD BEGIN ADDRESS
    INC 5,S     INCREMENT LS BYTE OF PASS COUNT
    BNE MEMSET  IF NOT ZERO, SET NEXT MEMORY BYTE
;   INC 4,S     INCREMENT MS BYTE OF PASS COUNT
;   BNE MEMSET  DONE WITH 65,535 PASSES OF MEMORY?
ADJSK6  LEAS 6,S    ADJ STACK POINTER BY 6
    RTS

MSG5    FCC ' -'
    fcb SP+$80
MSG6    FCC ', PASS'
    fcb SP+$80
MSG7    FCC ', BITS IN ERROR:'
    fcb SP+$80
MSG8    FCC ' =>'
    fcb SP+$80
MSG9    FCC '7654321'
    fcb '0+$80

    ENDC ; IFDEF MEMTST



 ifdef S19LOADER
*** ***********************
*** * Load program from Tape
*** ***********************
TLoad:
  LDD   CIDCB   ; SAVE CONSOLE DCBS
  LDX   CEDCB
  PSHS  X,B,A
  LDX   TPDCB   ; POINT TO TAPE DCB
  CLRA
  CLRB
  STX   CIDCB   ; SET TAPE IN, NO ECHO
  STD   CEDCB
  LDD   #RDRON*256+CntlFn ; RAISE READER CONTROL
  JSR   ReqIO

        CLR     BegAdd
        CLR     BegAdd+1

  BSR   Load    ; LOAD THE TAPE

  LDD   #RDROFF*256+CntlFn ; DROP READ CONTROL
  LDX   TPDCB
  JSR   ReqIO
  PULS  A,B,X   ;  RESTORE CONSOLE DCBS
  STD   CIDCB
  STX   CEDCB
  TST   CkSum   ; ANY ERRORS?
  BEQ   Ok  ; GO IF NOT

*** ***********************
*** * Display error inicator '?'
*** ***********************
Error:
  LDA   #$3F    ; DISPLAY ERROR INDICATOR
  JMP   OutChr

Ok:
        ldd   BegAdd
        jsr   DspDBy
        ldd   EndAdd
        JMP   DspDBy & RTS



*** ***********************
*** * Load program in hex format.
*** *
*** * ENTRY REQUIREMENTS: NONE
*** *
*** * EXIT CONDITIONS: ALL REGISTERS CHANGED
*** *
*** ***********************
Load:
  TFR   S,Y ; MARK STACK FOR ERROR RECOVERY
  leas  -2,s
Load1:
  JSR   InChr   ; GET A CHARACTER
load2:
  CMPA  #'S ; START OF RECORD?
  BNE   Load1   ; LOOP IF NOT
  JSR   InChr   ; GET ANOTHER CHARACTER
  CMPA  #'9 ; END OF LOAD?
  lBEQ   loadx  ; GO IF YES
  CMPA  #'1 ; START OF RECORD?
  BNE   load2   ; LOOP IF NOT
  CLR   CkSum   ; INIT CHECKSUM
  JSR   InByte  ; READ LENGTH
  SUBA  #2  ; ADJUST IT
  TFR   A,B ; SAVE IN B
  JSR   InByte  ; GET ADDRESS HI
  STA   ,S  ; SAVE ON STACK
  JSR   InByte  ; GET ADDRESS LO
  STA   1,S ; PUT ON STACK
  ldx   ,s  ; ADDRESS NOW IN X

        lda     BegAdd
        ora     BegAdd+1
        bne     load3
        stx     BegAdd

load3:
  JSR   InByte  ; READ A BYTE
  DECB      ; DECREMENT COUNT
  BEQ   load4   ; Go IF DONE
  STA   ,X  ; STORE BYTE

        stx     EndAdd

  CMPA  ,X+ ; VERIFY GOOD STORE
  BNE   load5   ; GO IF ERROR
  BRA   load3
load4:
  INC   CkSum   ; CHECK CHECKSUM
  BEQ   Load1   ; LOOP IF GOOD

; checksum is bad display S1 record target address, cleanup and exit.
  ldd   ,s
  jsr   DspDBy
  lda   #'c ; DISPLAY checksum ERROR INDICATOR
  bsr   NearOutChr
  lda   #'k ; DISPLAY checksum ERROR INDICATOR
  bsr   NearOutChr
  bra load6
load5:
  ; recheck the write.
  cmpa  -1,x
  beq   load3
  ;re do the write and verify
  sta   -1,x
  nop
  cmpa  -1,x
  beq   load3

  ; write verify failed display actual address, cleanup and exit.
  leax  -1,x
  pshs  a
  lda   #'@ ; DISPLAY checksum ERROR INDICATOR
  bsr   NearOutChr
  tfr   x,d
  jsr   DspDBy
  lda   #'w ; DISPLAY checksum ERROR INDICATOR
  bsr   NearOutChr
  puls  a
  jsr   DspSBy
  lda   #'r ; DISPLAY checksum ERROR INDICATOR
  bsr   NearOutChr
  lda   ,x
  jsr   DspSBy
  lda   #'v ; DISPLAY Verify ERROR INDICATOR
  bsr   NearOutChr
  lda   #'r ; DISPLAY Verify ERROR INDICATOR
  bsr   NearOutChr

  LDA   #$FF    ; SET ERROR FLAG
  STA   CkSum
  bra load3 ; jns for now continue
load6:
  LDA   #$FF    ; SET ERROR FLAG
  STA   CkSum
loadx:
  TFR   Y,S ; RESTORE STACK
  RTS

NearOutChr  jmp   OutChr
 endc

    ifdef   HELPHELP
Help:
        leay    <HelpTable,pcr
HelpL
        jsr     CRLF

        lda     ,y+         ; A == Command Char.
        beq     Helpx

        jsr     OutSp
        jsr     OutSp
        jsr     OutChr      ; display command char.
        jsr     OutSp
        bsr     Help9
        bsr     Help9
        bsr     Help9
        bra     HelpL

Help9   lda     ,y+
        leax     <strings,pcr
        leax    a,x
        tsta
        bmi     Helpx
        jsr     PString
Helpx:  rts

strings:
HelpAddr        FCS     '<addr> '
HelpReg         FCS     '<reg>'
HelpStartEnd    FCS     '<start> <end> '
HelpDotDotDot   FCS     '...'
HelpLen         FCS     '<len>'
HelpValue       FCS     '<value> '
HelpOp          FCS     '<op> '
                IFDEF DSKUTL
HelpDrSect      FCS     '<DSSS>'
                ENDC
        endc ; ifdef   HELPHELP

HelpTable:
        fcb     'M
        fcb     HelpAddr-strings,$ff,$ff
        fcb     'G
        fcb     HelpAddr-strings,$ff,$ff
        fcb     'R
        fcb     HelpReg-strings,$ff,$ff
        fcb     'L
        fcb     $ff,$ff,$ff
        fcb     'S
        fcb     HelpStartEnd-strings,$ff,$ff
        fcb     'B
        fcb     HelpAddr-strings,$ff,$ff
        fcb     'U
        fcb     HelpAddr-strings,$ff,$ff


        fcb     'C
        fcb     HelpAddr-strings,HelpAddr-strings,HelpLen-strings
        fcb     'F
        fcb     HelpStartEnd-strings,HelpValue-strings,HelpDotDotDot-strings
        fcb     'D
        fcb     HelpStartEnd-strings,$ff,$ff
        fcb     'H
        fcb     HelpValue-strings,HelpValue-strings,HelpOp-strings
        ifdef LIST_DCB
        fcb     '#
        fcb     $ff,$ff,$ff
        ENDC
        fcb     'J
        fcb     HelpAddr-strings,$ff,$ff

        IFDEF DSKUTL
        fcb 'T
        fcb     HelpDrSect-strings,$ff,$ff
        ENDC
        IFDEF   MEMTST1
        FCB 'Q
        fcb     HelpAddr-strings,HelpAddr-strings,$ff
        ENDC

;        ifdef   S19LOADER
;        FCB     'L
;        fcb     $ff,$ff,$ff
;        endc

        fcb     'Z
        fcb     $ff,$ff,$ff

        fcb     0       ; end of table


endcod  equ *
sizcod  equ endcod-begcod
        if sizcod&~$3ff
            ERROR image must fit in 1k ROM
        endc

    org BUFFER ; use ram at 0 for buffer
begdat  equ *
buffer  rmb 256
; *****************************************

    IFDEF   NULL_DCB
NullDCB:
    RMB 10
    ENDC

enddat  equ *
sizdat  equ enddat-begdat

