        PRAGMA cescapes

        nam     MEMEDIT
        ttl     MEMORY EDITOR
****************************************
*       medit   10/12/81
*       james   SPEARS
****************************************


        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i


        org     0
BEGCOD  equ     *

        ldy     #$1234
        ; usim
MLOOP   bsr     MDISP

        lbsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B

        cmpa    #CTRL_X
        beq     MLOOP

        tstb 
        Lbsr    MCOMAND

        jsr     CRLF

        cmpa    #CTRL_E CTRL-E EXIT
        bne     MLOOP

        clrb
        ; tstb
        rts

; VT100 cursor sequences.
; Cursor Home           <ESC>[{ROW};{COLUMN}H
; Erase End of Line     <ESC>[K
; Erase Down            <ESC>[J
; Erase Screen          <ESC>[2J
; Cursor Up             <ESC>[{COUNT}A

CursorHome      FCB     ESC,'[',';','H'+$80
; EraseEol        FCB     ESC,'[','K'+$80
EraseDown       fcb     ESC,'[','J'+$80
LF2AT       fcb     LF,LF,'@'+$80
; EraseScreen     fcb     ESC,'[','H',LF,LF,'@'+$80

_PString   jmp     [PStringv]

****************************************
MDISP   leax    <CursorHome,PCR
        bsr     _PString
        leax    <EraseDown,PCR
        bsr     _PString

        bsr     MDMP4

        leax    <LF2AT,PCR
        bsr     _PString
        tfr     Y,D
        lbsr     _DspDBy
        lda     #'>
        bsr     _OutChr
        rts

****************************************
MDMP4   pshs    D,X,Y
        tfr     Y,D
        subd    #32
        andb    #$F0
        tfr     D,X
        ldb     #6
MDMP41: bsr     MDMPLINE
        decb
        bne     MDMP41
        puls    D,X,Y,PC

_OutChr jmp [OutChrv]

****************************************
MDMPLINE:
        pshs    D    SAVE, STARTING ADDRESS
        pshs    X    SAVE, STARTING ADDRESS
        tfr     X,D
        lbsr     _DspDBy DISPLAY ADDRESS.

        ldb     #16 NUMBER OF BYTES
        pshs    Y
HEXLOOP: 
        cmpx   ,S
        bne     NCURBY IF NOT CURRENT BYTE
        lda     #BS
        bsr     _OutChr
        lda     #'>
        bsr     _OutChr
NCURBY: lda     ,X+ ** GET NEXT BYTE.
        jsr     [DspSByv] ** AND DISPLAY IT.
        decb    ** COUNT DOWN TO ZERO.
        bne     HEXLOOP

        leas    2,S DROP CURRENT BYTE POINTER
        jsr     OutSp
        puls    X GET STARTING ADDRESS BACK.
        ldb     #16 BYTE COUNT.

ASCIILOOP:
        lda     ,X+ ** NEXT BYTE,
        cmpa    #SP ** IF A>= " " THEN PRINT A ELSE PRINT
        bge     PRINTABLE **
        lda     #'. **
PRINTABLE bsr     _OutChr ** PRINT IT.
        decb    **
        bne     ASCIILOOP
        jsr     CRLF START NEW LINE.
        puls    D,PC

*
* MOVE BP COMMANDS
*

MOVEPTR cmpa    #SP MOVE CP (UP, DOWN, RIGHT, LEFT)
        beq     MP1
        cmpa    #$0C CTRL-L +1
        bne     MP2
MP1     leay    1,Y
        rts
MP2     cmpa    #$08 CTRL-H -1
        bne     MP3
        leay    -1,Y
        rts
MP3     cmpa    #$0A CTRL-J +16 DOWN
        bne     MP4
        leay    16,Y
        rts
MP4     cmpa    #$0B CTRL-K -16 UP
        bne     MP_X
        leay    -16,Y
MP_X    rts


NEWADDR bsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
        beq     N1
        cmpa    #CTRL_X
        beq     N0
        tfr     X,Y
N0      rts
N1      cmpa    #'@
        bne     N2
        bsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
        bne     N11
        ldy     ,Y
        rts
N11     ldy     ,X
        rts

N2      cmpa    #'+
        bne     N3
        bsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
        beq     NEWADDRX
        pshs    X
        tfr     Y,D
        addd    ,S++
N77     tfr     D,Y
        rts

N3      cmpa    #'-
        bne     N4
        bsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
        beq     NEWADDRX
        pshs    X
        tfr     Y,D
        subd    ,S++
        bra     N77
N4

; WONT WORK IF RUNNING FROM DISK UNDER MPX9
;         ldx     StkPtr
;         cmpa    #'X
;         bne     N5
;         lda     <4,X
;         bra     N99
; N5      cmpa    #'Y
;         bne     N6
;         ldx     <6,X
;         bra     N99
; N6      cmpa    #'Y
;         bne     N7

;         ldx     <8,X
;         bra     N99
; N7      cmpa    #'P
;         bne     N8
;         ldx     <10,X
;         bra     N99
; N8      cmpa    #'S
;         bne     NEWADDRX
; N99     tfr     X,Y
NEWADDRX  rts

* CP --> ( MEM[?] == IMM )
MSEARCH bsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
        beq     S11 NO HEX NUMBER WAS ENTERED
        tfr     X,D
        exg     A,B
        bra     S1
S11     cmpa    #'"
        bne     S12 NOT A QUOTE
        bsr     _InChr

        bra     S1
S12     cmpa    #SP SPACE (SEARCH FOR NEXT == CURRENT)
        blt     S99
        tfr     Y,X
        lda     ,X+
        bra     S2
S1      tfr     Y,X
S2      clrb
S21:    cmpa    ,X+
        beq     S9
        decb
        bne     S21
        rts
S9      leay    -1,X
S99     rts

; ADDOFFS bsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
;         pshs    X
;         bsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
;         tfr     X,D
;         subd    ,S++
;         bra     _DspDBy

*
* MISC COMMANDS
*

; PSYCMD  bsr     _InChr
;         jsr     LOOKUP
;         bne     PSYCMDX
;         jsr     OutSp
;         pshs    Y
;         jsr     [1,X]
;         puls    Y RECOVER Y
; PSYCMDX rts     RETURN

_DspDBy jmp [DspDByv]
_GetHex jmp [GetHexv]

; INTERPET COMMAND
MCOMAND beq     MC1
        bsr    NEWNUM
        ; nop
        ; nop
        ; nop
MC1     cmpa    #'.'   PERIOD
        lbeq    NEWADDR
        cmpa    #'"
        beq    CHAR
        cmpa    #'! OR #
        beq    ORMEM
        cmpa    #'& AND #
        beq    ANDMEM
        cmpa    #'+ ADD TO MEM
        beq    ADDTO
        cmpa    #'-   SUBFROM
        beq    SUBFROM
        cmpa    #'S SEARCH
        beq    MSEARCH
        ; cmpa    #':   CALC OFFSET
        ; beq    ADDOFFS
        ; cmpa    #'* MULT
        ; beq    MULT
        ; cmpa    #'; SEMI-COLAN PSYMON COMMAND
        ; lbeq    PSYCMD
        cmpa    #SP A SPACE OR LESS.
        lble    MOVEPTR
        ; fcb     $12,$12,$12,$12,$12,$12 NOP'S ?
        ; fcb     $12,$12,$12,$12,S12,$12
        lbsr     HELP
RETURN  rts

_InChr  jmp [InChrv]

*
* MODIFY MEMORY COMMANDS
*
NEWNUM  cmpa    #',    STORE AS 16 BIT WORD.
        beq     NEW1
        exg     D,X
        stb     ,Y+
        ; exg     D,X
        rts
NEW1    stx     ,Y++
        rts

CHAR    bsr     _InChr STORE CHARACTER AT MEM..
        sta     ,Y
        rts

* MEM [CP] = MEM[CP] OR IMM
ORMEM   bsr     _GetHex  Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
        beq     RETURN
        exg     X,D
        orb     ,Y
MemOp2  stb     ,Y
        ; exg     D,X
        bra     MC1

* MEM[CP] = MEM[CP] AND IMM
ANDMEM  bsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
        beq     RETURN
        exg     X,D
        andb    ,Y
        bra     MemOp2 DO COMMON FINISHUP AND PROC MORE COMMANDS IF

* MEM[CP] = MEM[CP] + IMM
ADDTO   bsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
        beq     RETURN
        exg     X,D
        addb    ,Y
        bra     MemOp2 DO COMMON FINISHUP AND PROC MORE COMMANDS IF

* MEM[CP] = MEM[CP] - IMM
SUBFROM bsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
        beq     RETURN
        exg     X,D
        negb
        addb    ,Y
        bra     MemOp2 DO COMMON FINISHUP AND PROC MORE COMMANDS IF

; MULT    lbsr     _GetHex Exit: A - last char input, B - hex digit count, X - Hex number, C - set according to B
;         beq     MC1
;         exg     X,D
;         lda     ,Y
;         mul
;         bra     MemOp2 DO COMMON FINISHUP AND PROC MORE COMMANDS IF

****************************************
HELP:   leax    <HelpTxt,PCR
        lbsr     _PString
        bsr     _InChr
        rts 

HelpTxt fcb CR,LF,LF
        fcc 'help text'
        fcb CR,LF

        fcc /CTRL-L +1 RIGHT\r\n/
        fcc /CTRL-H -1 LEFT\r\n/
        fcc /CTRL-J +16 DOWN\r\n/
        fcc /CTRL-K -16 UP\r\n/
        fcc /\r\n/

        fcc /.nnnn NEW ADDRESS\r\n/
        fcc /.@nnnn NEW ADDRESS INDIRECT\r\n/
        fcc /.+nnnn ADD TO ADDRESS\r\n/
        fcc /.-nnnn SUB FROM ADDRESS\r\n/
        fcc /\r\n/

        fcc /nn   NEW BYTE DATA\r\n/
        fcc /NNNN, NEW WORD DATA\r\n/
        fcc /"C CHARACTER VALUE\r\n/
        fcc /!NN OR VALUE\r\n/
        fcc /&NN AND VALUE\r\n/
        fcc /+NN ADD VALUE\r\n/
        fcc /-NN SUB VALUE\r\n/

        fcc /SNN SEARCH\r\n/
        fcc /S<SPACE> REPEAT SEARCH\r\n/
        fcc /\r\n/

        fcc /CTRL-E Exit\r\n/
        fcc /\r\n/

        fcc /CTRL-X CANCEL\r\n/
        fcc /\r\n/

        fcS 'ENTER ANY KEY TO CONTINUE:'
        ; fcb LF+$80

; _PString   jmp     [PStringv]


ENDCOD          equ *
SIZCOD          equ ENDCOD-BEGCOD

****************************************
        end
