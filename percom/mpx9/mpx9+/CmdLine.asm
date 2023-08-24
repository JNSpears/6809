 PRAGMA cescapes
 nam CmdLine
 ttl New Comandline processor for MPX9+
*********************************************************
* CmdLine                               JNS 7/22/2023   *
*                                                       *
*                                                       *
*********************************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i

        section .text

**************************************************
* Program (Position independant)
**************************************************

        ORG     $0

; ************************************************
CmdLineInit EXPORT
CmdLineInit:

        LEAU    CmdLineInit,pcr
        pshs    U
        leau    data,PCR
        tfr     U,D
        subd    ,S++
        tfr     D,U

        leax    VAR.HistBuff,U
        stx     <<VAR.HBegin,U

        tfr     X,D
        addd    #MaxHistBuff
        std     <<VAR.HEnd,U

        stx     <<VAR.HHead,U

        stx     <<VAR.HCurr,U

        clra
        clrb
        std     <<VAR.histix,U

ClrBuffer2:
        CLR     ,x+
        cmpx    <<VAR.HEnd,U
        BNE     ClrBuffer2

        clrb
        rts

; END INIT

; ************************************************
CmdLine EXPORT
CmdLine:

LOOP:
        ; setup U to point to the data structure
        LEAU    CmdLineInit,pcr
        pshs    U
        leau    data,PCR
        tfr     U,D
        subd    ,S++
        tfr     D,U
        
        ; print prompt
        LEAX    CmdLineInit,pcr
        pshs    X
        leaX    prompt,PCR
        tfr     X,D
        subd    ,S++
        tfr     D,X
 	MPX9 	PSTRNG

;  	; Initialize buffer and other variables.
;  	LDB 	#MaxCmdLineLen 		; SET SIZE IN B	
;  	LEAX 	<<VAR.CmdLineBuff-1,U ; POINT X AT LINE BUFFER
; ClrCmdBuffer:
;  	CLR 	B,X
;  	DECB 
;  	BNE 	ClrCmdBuffer

 	; setup and get a line.
 	LDB 	#MaxCmdLineLen 		; SET SIZE IN B	
 	LEAX 	<<VAR.CmdLineBuff,U ; POINT X AT LINE BUFFER
	LBSR  	getline				; x=buffer, b=bufferlen --> a=firstchar, b=bufferlen, x=buffer

        ; check for history action (!h display history, TBD: !n re-execute command #n, !text re-execute command starting with text) 
 	LEAX 	<<VAR.CmdLineBuff,U ; POINT X AT LINE BUFFER
 	lda 	,X
 	cmpa 	#'!'
 	bne 	NotHistCmd

        lbsr    DoHistAction            ; returns z if nothing to execute
        beq 	NoError

NotHistCmd
        ; Execute command.
        jsr     CRLF
	MPX9	PROCMD
	beq 	NoError
	MPX9 	RPTERR
NoError:
	; check to see if we are done for now.
	lda 	<<VAR.left,U
	adda 	<<VAR.right,U
	lbne   	LOOP

	clrb 		; no error
	rts		; Return to OS

**************************************************
* GetCharNoEcho - GET INPUT CHAR WITH NO ECHO
*
* Entry: None
* 
* Exit: A - char with Parity removed
*		 ALL other regs perserved, except C
* 
**************************************************
GetCharNoEcho:
 pshs X,B	; SAVE REGISTERS
 LDX  CIDCB	; POINT TO INPUT DCB
 LDB  #ReadFn	; SET UP FOR READ
 MPX9 REQIO	; READ A CHARACTER
 ANDA #$7F	; REMOVE PARITY
 PULS B,X,PC 	; RESTORE & RETURN

**************************************************
* GetConStat - GET CONSOLE STATUS
*
* Entry: None
* 
* Exit: A - ACIA STATUS
*		 ALL other regs perserved, except C
* 
**************************************************
GetConStat:
 pshs X,B	; SAVE REGISTERS
 LDX  CIDCB	; POINT TO INPUT DCB
 LDB  #StatFn	; SET UP FOR GET STATUS
 MPX9 REQIO	; GET STATUS
 PULS B,X,PC 	; RESTORE & RETURN

**************************************************
* OutEscBrkNChr - Output Vt100 escape sequ.
*
* Entry: A - Final char in Esc sequence
* 	 B - count (if 0 then no count in Esc sequence)
* 			only works for 0-99
* 
* Exit:  A - ACIA STATUS
*	 ALL other regs perserved, except C
* 
**************************************************
OutEscBrkNChr:
 pshs B,A               ; save registers
 LDA #ESC
 MPX9 OUTCHR            ; send Escape to console
 LDA #'['
 MPX9 OUTCHR            ; send [ to console
 tstb 			; check to see if there is a count to send 
 BEQ OutEscBrkNChr1	; if not go
; TWO DIGIT DECIMAL CONVERSION, NO LEADING SPACE OR ZERO
 tfr B,A
 ADDA #0
 daa                    ; convert to BCD
 cmpa #9                ; 2 digits?
 ble OutEscBrkNChr2     ; if not go
 TFR A,B                ; save BCD count
 asra                   ; shift MSD to LSD
 ASRA
 ASRA
 ASRA
 BSR OUTDIGIT           ; convert MSD to ascii and send to console
 TFR b,A                ; restore BCD count
OutEscBrkNChr2:
 BSR OUTDIGIT           ; convert LSD to ascii and send to console
OutEscBrkNChr1:
 LDA ,S                 ; get Final char in Esc sequence
 MPX9 OUTCHR            ; send to console
 PULS A,B,PC            ; RESTORE & RETURN

; Mask digit, convert to ascii and send to console
OUTDIGIT:
  ANDA #$F
  ADDA #'0'
  MPX9 OUTCHR
  rts 

**************************************************
* OUTNCHR - Output a character N times
*
* Entry: A - Char to output
* 		 B - count 
* 
* Exit: B - zero
*		 ALL other regs perserved, except C
* 
**************************************************
OUTNCHR: ; IN: A-CHAR B-COUNT OUT: A,B MODIFIED.
 MPX9 OUTCHR
 decb 
 bne OUTNCHR
 RTS

**************************************************
* SYSTEM CALL 9 (GETLIN) - GET INPUT LINE 
*                                                
* ENTRY REQUIREMENTS:  X POINTS AT LINE BUFFER   
*                      B CONTAINS BUFFER LENGTH  
*                      U POINTS AT WORKING STORAGE   
*                                                
* EXIT CONDITIONS:  A CONTAINS FIRST CHARACTER,  
; *                     NUL => LINE CANCELED       ?
; *                     Z FLAG IN CC SET PER A     ?
*                   B CONTAINS LINE LENGTH LENGTH  
*                   OTHERS UNCHANGED             
**************************************************
getline:
GETLN:
	CLR  	<<VAR.dirty,U
	stb 	<<VAR.max,U
	PSHS 	B,X,Y SAVE REGISTERS
	clr 	<<VAR.left,U  RESET CHARACTER COUNT
	clr 	<<VAR.right,U

GetCharLoop:
	BSR 	GetCharNoEcho GET NEXT CHARACTER

	CMPA 	#CR END OF INPUT?
	BEQ 	Done GO IF YES

	CMPA 	#ESC START OF ESCAPE SEQ?
	LBEQ 	CheckEscapeSeq GO IF YES

	CMPA 	#SP CONTROL CODE?
	LBLO 	CheckOneChar 

	CMPA 	#TILDE Invalide Ascii
	LBHI 	CheckOneChar 

	; save this character in buffer, output char, update left count
	ldb 	<<VAR.left,U 
	STA 	B,X SAVE THIS CHARACTER
	MPX9 	OUTCHR
	INC 	<<VAR.left,U UPDATE COUNT

	CLR  	<<VAR.dirty,U 
	INC  	<<VAR.dirty,U ; Mark as dirty

	; if characters to the right
	ldb 	<<VAR.right,U
	beq  	GetCharLoop2

	bsr 	OutRightChars

	; and back space to return cursor

	ldb 	<<VAR.right,U
	LBSR 	MoveCurLeft

GetCharLoop2:

	CMPB 	,S PAST LIMIT?
	BLO 	GetCharLoop LOOP IF NOT

	LDA 	#BS ; OUTPUT A BACKSPACE
	MPX9 	OUTCHR
	BRA 	GetCharLoop

Done:
	PSHS 	A,X
	lbsr  	NormBuffer
	PULS 	X,A
	LDB 	<<VAR.left,U 
	STA 	B,X ; MARK END OF LINE W/ CR

 	tst 	<<VAR.dirty,U 
 	beq 	CopyHist2Buff3		; skip add to history if not dirty

 	LEAX 	<<VAR.CmdLineBuff,U ; POINT X AT LINE BUFFER
 	lda 	,X
 	cmpa 	#'!'
 	beq 	CopyHist2Buff3
        ldy 	<<VAR.HHead,U  		; Points to next line to overwrite
CopyHist2Buff:
	lda 	,X+
	beq  	CopyHist2Buff1
	cmpa 	#CR
	beq  	CopyHist2Buff1
	sta   	,Y+
	cmpy 	<<VAR.HEnd,U 		; need to Wrap?
	BLT 	CopyHist2Buff		; NO go...
	ldy 	<<VAR.HBegin,U  	; 
	bra  	CopyHist2Buff

CopyHist2Buff1:
	clr 	,Y+ 				; Term line in history buffer with null
	cmpy 	<<VAR.HEnd,U 		; need to Wrap?
	BLT 	NO_WRAP0			; NO go...
	ldy 	<<VAR.HBegin,U  	; wrap
NO_WRAP0:
	sty 	<<VAR.HHead,U  		; save for next line.
	sty 	<<VAR.HCurr,U  		; save for next line.
	
	; inc history index
	ldd 	<<VAR.histix,U
	addd 	#1
	std 	<<VAR.histix,U 	

CopyHist2Buff2:
	lda 	,Y
	beq 	CopyHist2Buff3
	CLR 	,Y+ 				; Term line in history buffer with null

	cmpy 	<<VAR.HEnd,U 		; need to Wrap?
	BNE 	CopyHist2Buff2		; NO go...
	ldy 	<<VAR.HBegin,U  	; 

	bra  	CopyHist2Buff2

CopyHist2Buff3:
	LDA 	[1,S] GET FIRST CHARACTER OF LINE
	LDB 	<<VAR.left,U 

GETLNX 
	TSTA 	; SET Z FLAG PER A
	PULS 	B,X,Y,PC RETURN

**************************************************
* OutRightChars - Output chars to the right of the cursor
*                                                
* ENTRY REQUIREMENTS:  X POINTS AT LINE BUFFER   
*                      U POINTS AT WORKING STORAGE   
*                                                
* EXIT CONDITIONS:     A, B MODIFIED
* 		       OTHERS UNCHANGED             
**************************************************
OutRightChars:
        tst     <<VAR.right,U
        beq     OutRightCharsX
        clrb
OutRightChars1:
        ; USIM
        tfr    b,a
        adda   <<VAR.max,U
        suba   <<VAR.right,U
        lda    a,x
        MPX9   OUTCHR
        incb
        cmpb   <<VAR.right,U
        blt    OutRightChars1
OutRightCharsX:
        rts

**********************************
* CheckOneChar
* ENTER:	A - CHAR
* EXIT TO SERVICE ROUTINE
*	OR TO GetCharLoop
**********************************
CheckOneChar:
 pshs 	D,X
 leax 	TBL1,pcr
CheckOneChar1:
 TST 	,X
 beq 	NOTFOUND
 cmpa 	,X+
 bne 	NEXT
FOUND: LDD ,X 	GET OFFSET TO ROUTINE
 LEAY 	D,X 	CALC ROUTINE ADDRESS
 puls 	D,X
 jmp 	,Y 		JUMP TO SPECIAL CHAR HANDLER
 
NEXT:
 leax 	2,X
 bra    CheckOneChar1
NOTFOUND:
 puls 	D,X
GetCharLoop_2:
 LBRA	GetCharLoop

TBL1:
 FCB 'Z-'@	; CTRL-Z
 FDB Cmd_ESC-*

 FCB ESC
 FDB Cmd_ESC-*

 FCB BS
 FDB Cmd_BS-*

 FCB $7F	; DEL
 FDB Cmd_DEL-*

 FCB 0 END OF TABLE MARK

**************************************************
Cmd_ESC0: 
Cmd_ESC:
 Lbsr 	CancelCurrLine
 BRA	GetCharLoop_2

**************************************************
Cmd_BS:
 ; USIM
 tst  	<<VAR.left,U 
 beq 	Cmd_BSX

 dec   	<<VAR.left,U 
 lda 	#BS
 MPX9  	OUTCHR
 ; USIM

 ; output right chars
 bsr 	OutRightChars
 ; USIM

 lda 	#SP
 MPX9  	OUTCHR

 ldb    <<VAR.right,U
 incb
 lda 	#BS
 Lbsr 	OUTNCHR	         ; output N BS characters, return cursor ot original location

 CLR  	<<VAR.dirty,U 
 INC  	<<VAR.dirty,U    ; Mark as dirty

Cmd_BSX:
 BRA	GetCharLoop_2


**************************************************
Cmd_DEL:
 ldb <<VAR.right,U
 beq Cmd_DELX	; nothing to the right of cursor

 DEC <<VAR.right,U
 beq Cmd_DEL2	; nothing left to the right

 ; output right chars
 clrb 
Cmd_DEL1:
 tfr b,a
 adda <<VAR.max,U
 suba <<VAR.right,U
 lda a,x
 MPX9 OUTCHR
 incb
 cmpb <<VAR.right,U
 blt Cmd_DEL1

Cmd_DEL2:
 BSR 	EraseEOL

 ldb 	<<VAR.right,U
 BSR 	MoveCurLeft

 CLR  <<VAR.dirty,U 
 INC  <<VAR.dirty,U ; Mark as dirty

Cmd_DELX:
 BRA	GetCharLoop_2

**************************************************
MoveCurRight:
 LDA 	#'C
 LBRA 	OutEscBrkNChr

MoveCurLeft:
 LDA 	#'D
 LBRA 	OutEscBrkNChr

EraseEOL:
 CLRB
 LDA 	#'K
 LBRA 	OutEscBrkNChr

**************************************************
CheckEscapeSeq:
 pshs 	X
 ldx 	#0
CheckEscapeSeq1:
 LEAX 	1,X
 cmpx 	#$200
 BLE 	CheckEscapeSeq2
 puls 	X
 bra 	Cmd_ESC0
CheckEscapeSeq2:
 lbsr 	GetConStat
 LSRa  		; BIT TO C
 BCC   	CheckEscapeSeq1		; LOOP IF NO INPUT
 PULS 	X
 LBSR 	GetCharNoEcho GET NEXT CHARACTER SHOULD BE '['
 cmpa 	#'[
 bne 	CheckEscapeSeqX
 tfr 	A,B
CheckEscapeSeq5
 LBSR 	GetCharNoEcho GET NEXT CHARACTER

 cmpa 	#'0'
 blt 	CheckEscapeSeq4
 cmpa 	#';'
 bgt 	CheckEscapeSeq4
 tfr 	A,B
 bra 	CheckEscapeSeq5

CheckEscapeSeq4
 exg 	a,b
 ; d = 2 CHAR SEQUENCE
 pshs 	X
 leax 	TBL2,pcr
CheckEscapeSeq3:
 TST 	,X
 beq 	NOTFOUND1
 cmpd 	,X++
 bne 	NEXT1
FOUND1: LDD ,X 	GET OFFSET TO ROUTINE
 LEAY 	D,X 	CALC ROUTINE ADDRESS
 puls 	X
 jmp 	,Y 		JUMP TO SPECIAL CHAR HANDLER
NEXT1:
 leax 	2,X 	; STEP OVER THE ROUTINE OFFSET
 bra    CheckEscapeSeq3
NOTFOUND1:
 puls 	X

CheckEscapeSeqX:
 LBRA	GetCharLoop


TBL2:
 ; [D 5B44 CURSOR LEFT
 FCC /[D/
 FDB Cmd_CurLeft-*

 ; [C 5B43 CURSOR RIGHT
 FCC /[C/
 FDB Cmd_CurRight-*

 ; [H 5B48 CURSOR HOME
 FCC /[H/
 FDB Cmd_CurHome-*

 ; [F 5B46 CURSOR END
 FCC /[F/
 FDB Cmd_CurEnd-*

 ; [1;5C CTRL CURSOR RIGHT
 FCC /5C/
 FDB Cmd_CtrlCurRight-*

 ; [1;5D CTRL CURSOR LEFT
 FCC /5D/
 FDB Cmd_CtrlCurLeft-*

 ; [A CURSOR UP
 FCC /[A/
 FDB Cmd_CurUp-*

 ; [B CURSOR DOWN
 FCC /[B/
 FDB Cmd_CurDown-*

 FCB 0 END OF TABLE MARK

; *****************************************************
Cmd_CurLeft:
 bsr 	Do_CurLeft
 LBRA	GetCharLoop

Do_CurLeft:
 LDB   	<<VAR.left,U
 beq 	Cmd_CurLeftX

 CLRB 
 LBSR	MoveCurLeft

 DEC 	<<VAR.left,U
 INC 	<<VAR.right,U

 LDB 	<<VAR.left,U
 LDA 	B,X
 LDB 	<<VAR.max,U
 SUBB   <<VAR.right,U
 STA 	B,X

Cmd_CurLeftX:
 rts

; *****************************************************
Cmd_CurRight:
 BSR 	Do_CurRight
 LBRA	GetCharLoop

Do_CurRight:
 ldb   	<<VAR.right,U
 beq 	Do_CurRightX
 clrb 
 LBSR	MoveCurRight

 ldb 	<<VAR.max,U
 subb   <<VAR.right,U
 lda 	b,x
 dec 	<<VAR.right,U
 
 ldb 	<<VAR.left,U
 sta 	b,x
 inc  	<<VAR.left,U

Do_CurRightX:
 RTS

XCURRIGHT1: FCB ESC
 FCS /[C/

; *****************************************************
Cmd_CurHome:
 ldb   	<<VAR.left,U
 beq 	Cmd_CurHomeX

 LBSR	MoveCurLeft

 ; MOVE from(X)=X, to(Y)=X+max-left, count(D)=left
 
* ENTRY REQUIREMENTS:  X POINTS TO SOURCE FIELD  *
*                      Y POINTS TO DEST. FIELD   *
*                      D CONTAINS LENGTH         *
 lda 	<<VAR.max,U
 suba 	<<VAR.left,U
 leay 	a,x
 clra 
 ldb 	<<VAR.left,U
 MPX9 	BLKMOV

 LDA 	<<VAR.right,U
 adda 	<<VAR.left,U
 sta 	<<VAR.right,U

 Clr  	<<VAR.left,U

Cmd_CurHomeX:
 LBRA	GetCharLoop

; *****************************************************
Cmd_CurEnd:
 ldb   	<<VAR.right,U
 beq 	Cmd_CurEndX
 ldb   	<<VAR.right,U
 LBSR	MoveCurRight
 bsr  	NormBuffer
Cmd_CurEndX:
 LBRA	GetCharLoop

; *****************************************************
; MOVE from(X)=X+max-right, to(Y)=X+left, count(D)=right
NormBuffer:
 pshs   X
 lda 	<<VAR.left,U
 leay  	a,x

 lda 	<<VAR.max,U
 suba   <<VAR.right,U
 leax 	a,x
 clra 
 ldb 	<<VAR.right,U

 beq NormBuffer1
* ENTRY REQUIREMENTS:  X POINTS TO SOURCE FIELD  *
*                      Y POINTS TO DEST. FIELD   *
*                      D CONTAINS LENGTH         *
 MPX9 	BLKMOV
NormBuffer1:

 lda 	<<VAR.left,U
 adda 	<<VAR.right,U
 sta 	<<VAR.left,U
 clr    <<VAR.right,U
 
 puls   X
 rts

; *****************************************************
Cmd_CtrlCurRight

Cmd_CtrlCurRight1:
 TST 	<<VAR.right,U
 BEQ 	Cmd_CtrlCurRight12
 Lbsr 	Do_CurRight ; EXIT A=CHAR UNDER CURSOR
 bsr 	IsAlphaNumUnder
 BEQ 	Cmd_CtrlCurRight1
Cmd_CtrlCurRight12:
 TST 	<<VAR.right,U
 BEQ 	Cmd_CtrlCurRightX
 Lbsr 	Do_CurRight ; EXIT A=CHAR UNDER CURSOR
 bsr 	IsAlphaNumUnder
 bne  	Cmd_CtrlCurRight12

 Lbsr 	Do_CurLeft ; EXIT A=CHAR UNDER CURSOR


Cmd_CtrlCurRightX:
 LBRA	GetCharLoop

; *****************************************************
; in a=char out b=0 AlphaNumUnder or 1 if puncuation etc. CC set per B
IsAlphaNumUnder:
	ldb #1

	; is it a '_'
	cmpa #'_'
	beq  IsAlphaNumUnderTrue
	
	; is it '0'-'9'
	suba #'0'
	bmi  IsAlphaNumUnderX
	cmpa #9
	ble  IsAlphaNumUnderTrue

	; is it 'A'-'Z'
	suba #'A'-'0'
	bmi  IsAlphaNumUnderX
	cmpa #26
	ble  IsAlphaNumUnderTrue

	; is it 'a'-'z'
	suba #'a'-'A'
	bmi  IsAlphaNumUnderX
	cmpa #26
	ble  IsAlphaNumUnderTrue

	bra  IsAlphaNumUnderX

IsAlphaNumUnderTrue:
	clrb
IsAlphaNumUnderX:
 	tstb 
	rts

; *****************************************************
Cmd_CtrlCurLeft

Cmd_CtrlCurLeft1:
 TST 	<<VAR.left,U
 BEQ 	Cmd_CtrlCurLeft2
 Lbsr 	Do_CurLeft ; EXIT A=CHAR UNDER CURSOR
 bsr 	IsAlphaNumUnder
 BNE 	Cmd_CtrlCurLeft1
Cmd_CtrlCurLeft2:
 TST 	<<VAR.left,U
 BEQ 	Cmd_CtrlCurLeftX
 Lbsr 	Do_CurLeft ; EXIT A=CHAR UNDER CURSOR
 bsr 	IsAlphaNumUnder
 BEQ  	Cmd_CtrlCurLeft2

Cmd_CtrlCurLeftX:
 LBRA	GetCharLoop

; *****************************************************

Cmd_CurUp:

; SCAN BACKTO HEAD IF ALL 0'S THEN NO MORE HIST DO NOTHING

 ldy 	<<VAR.HCurr,U

 cmpy 	<<VAR.HBegin,U
 bne  	NoWrap1
 ldy 	<<VAR.HEnd,U
 leay   <<-1,Y
NoWrap1:
 lda 	,-Y ; this should load the null at end of prev line.
 clrb

ScanBackForNull:
 ; step back to find begining and count chars of prev line.
 cmpy 	<<VAR.HBegin,U
 bhi 	NoWrap2
 ldy 	<<VAR.HEnd,U
NoWrap2:
 incb
 lda 	,-Y 
 BNE  	ScanBackForNull

 leay 	1,Y	; y-> first char of this line.

 cmpy 	<<VAR.HEnd,U
 BLO 	NoWrap3
 ldy 	<<VAR.HBegin,U
NoWrap3:

 decb  
 beq 	Cmd_CurUpX

; DONE SCAN BACKTO HEAD, IF ALL 0'S THEN NO MORE HIST DO NOTHING
 
 Lbsr 	CancelCurrLine ; Position curston at beging of input line, EOL, clear right and left counts.

 sty 	<<VAR.HCurr,U
 clrb 

CopyCharsHist2Buffer:
 lda 	,Y+
 beq 	Cmd_CurUp9

 cmpy 	<<VAR.HEnd,U
 bne  	NoWrap4
 ldy 	<<VAR.HBegin,U
NoWrap4:

 sta 	B,X
 incb 
 inc 	<<VAR.left,U
 MPX9	OUTCHR
 BRA 	CopyCharsHist2Buffer

Cmd_CurUp9:
Cmd_CurUpX:
 LBRA	GetCharLoop


; *****************************************************

Cmd_CurDown:

 ldy 	<<VAR.HCurr,U

 cmpy 	<<VAR.HHead,U
 beq  	Cmd_CurDownX

ScanForwardForNull:
 cmpy 	<<VAR.HEnd,U
 bne  	NoWrap1a
 ldy 	<<VAR.HBegin,U
NoWrap1a:
 lda 	,Y+
 bne 	ScanForwardForNull

 Lbsr 	CancelCurrLine ; Position curston at beging of input line, EOL, clear right and left counts.

 sty 	<<VAR.HCurr,U

 BRA 	CopyCharsHist2Buffer

Cmd_CurDown9:
Cmd_CurDownX:
 LBRA	GetCharLoop

; *****************************************************
CancelCurrLine: 
 ldb   	<<VAR.left,U
 BEQ 	CancelCurrLine1
 LBSR 	MoveCurLeft
CancelCurrLine1:
 LBSR 	EraseEOL
 clr   	<<VAR.left,U
 clr  	<<VAR.right,U
 clr  	<<VAR.dirty,U
 rts

; *****************************************************

; History History History History History History History History History History History History History History History History
; History History History History History History History History History History History History History History History History
; History History History History History History History History History History History History History History History History
DoHistAction

        ; MPX9    $41
        ; fcs     /\n\rDHA ent S=%Sx\n\r/

        ldy     <<VAR.HHead,U
        ldd     <<VAR.histix,U
        pshs    D       ; create temp var. for history ix := current history ix

SkipNulls:     ; skip null(s) between commands
        cmpy    <<VAR.HEnd,U
        bne     @NoWrap
        ldy     <<VAR.HBegin,U
@NoWrap lda     ,Y+
        beq     SkipNulls

        pshs    A,Y     save the pointer to the first command in the history buffer and the first character in that command.
        clrb            zero counter of number of commands in the history buffer.

CountCmds:
        lda     ,Y+
        bne     NotEndOfCmd
        incb 
NotEndOfCmd:    ; Wrap at end of history buffer.
        cmpy    <<VAR.HEnd,U
        bne     @NoWrap
        ldy     <<VAR.HBegin,U
@NoWrap cmpy    <<VAR.HHead,U
        bne     CountCmds

        jsr     CRLF

        clra 
        coma
        comb
        addd    #1
        addd    <<3,S ; set temp to current line's histix
        std     <<3,S

        ldd     <<3,s
        MPX9    DSPDEC
        addd    #1
        std     <<3,s
        puls    A,Y
        tsta
        bra     HistOutCmdLoop1

HistOutCmdLoop:
        ; Y -> first non null char.
        lda     ,Y+
HistOutCmdLoop1:   
        beq     DspNxtCmd
        ; Wrap at end of history buffer.
        cmpy    <<VAR.HEnd,U
        bne     @NoWrap
        ldy     <<VAR.HBegin,U
@NoWrap: 
        MPX9    OUTCHR
        BRA     HistOutCmdLoop
DspNxtCmd:      ; Y -> first char of next command 
        cmpy    <<VAR.HHead,U   is this the current next command?
        beq     ExitNoCommand
        jsr     CRLF
        ldd     ,s      GET IX
        MPX9    DSPDEC  DISPLAY IX
        addd    #1      INCREMENT IX
        std     ,s      SAVE IT
        bra     HistOutCmdLoop

ExitNoCommand:
        clrb

DoHistActionX
        leas    2,S
        ; MPX9    $41
        ; fcs     /\n\rDHA ent S=%Sx\n\r/

        ; note !n and !text will return B <> 0
        tstb
        rts

; Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute
; Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute
; Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute Execute







; endcod  equ *-1


        endsection


*
** Constants.
*

        section data

prompt FCB CR,LF
 fcs '> '

        endsection

*
** Uninitialiazed Working Variables.
*

        section .bss

MaxCmdLineLen	equ 64
MaxHistBuff	equ 256

VAR	STRUCT
max 		rmb 1
left		rmb 1
right		rmb 1
dirty		rmb 1 ; true if: user input or edit, false if: canceled, or from history
HBegin		rmw 1
HEnd		rmw 1
HHead		rmw 1
HCurr		rmw 1
histix		rmw 1

CmdLineBuff	rmb MaxCmdLineLen
HistBuff 	rmb MaxHistBuff
	ENDSTRUCT

 	; ALIGN 16
data    VAR

        endsection

; PGMEND  equ *-1
; PGMSIZ  EQU PGMEND-BGNPGM

 END



    

	
ANSI/VT100 Terminal Control Escape Sequences
[ Status | Setup | Fonts | Cursor | Scrolling | Tabs | Erasing | Printing | Keyboard | Colours ]

Many computer terminals and terminal emulators support colour and cursor control through a system of escape sequences. One such standard is commonly referred to as ANSI Colour. Several terminal specifications are based on the ANSI colour standard, including VT100.

The following is a partial listing of the VT100 control set.

<ESC> represents the ASCII "escape" character, 0x1B. Bracketed tags represent modifiable decimal parameters; eg. {ROW} would be replaced by a row number.

Device Status
The following codes are used for reporting terminal/display settings, and vary depending on the implementation:

Query Device Code	<ESC>[c

    Requests a Report Device Code response from the device.

Report Device Code	<ESC>[{code}0c

    Generated by the device in response to Query Device Code request.

Query Device Status	<ESC>[5n

    Requests a Report Device Status response from the device.

Report Device OK	<ESC>[0n

    Generated by the device in response to a Query Device Status request; indicates that device is functioning correctly.

Report Device Failure	<ESC>[3n

    Generated by the device in response to a Query Device Status request; indicates that device is functioning improperly.

Query Cursor Position	<ESC>[6n

    Requests a Report Cursor Position response from the device.

Report Cursor Position	<ESC>[{ROW};{COLUMN}R

    Generated by the device in response to a Query Cursor Position request; reports current cursor position.

Terminal Setup
The h and l codes are used for setting terminal/display mode, and vary depending on the implementation. Line Wrap is one of the few setup codes that tend to be used consistently:

Reset Device		<ESC>c

    Reset all terminal settings to default.

Enable Line Wrap	<ESC>[7h

    Text wraps to next line if longer than the length of the display area.

Disable Line Wrap	<ESC>[7l

    Disables line wrapping.

Fonts
Some terminals support multiple fonts: normal/bold, swiss/italic, etc. There are a variety of special codes for certain terminals; the following are fairly standard:

Font Set G0		<ESC>(

    Set default font.

Font Set G1		<ESC>)

    Set alternate font.

Cursor Control

Cursor Home 		<ESC>[{ROW};{COLUMN}H

    Sets the cursor position where subsequent text will begin. If no row/column parameters are provided (ie. <ESC>[H), the cursor will move to the home position, at the upper left of the screen.

Cursor Up		<ESC>[{COUNT}A

    Moves the cursor up by COUNT rows; the default count is 1.

Cursor Down		<ESC>[{COUNT}B

    Moves the cursor down by COUNT rows; the default count is 1.

Cursor Forward		<ESC>[{COUNT}C

    Moves the cursor forward by COUNT columns; the default count is 1.

Cursor Backward		<ESC>[{COUNT}D

    Moves the cursor backward by COUNT columns; the default count is 1.

Force Cursor Position	<ESC>[{ROW};{COLUMN}f

    Identical to Cursor Home.

Save Cursor		<ESC>[s

    Save current cursor position.

Unsave Cursor		<ESC>[u

    Restores cursor position after a Save Cursor.

Save Cursor & Attrs	<ESC>7

    Save current cursor position.

Restore Cursor & Attrs	<ESC>8

    Restores cursor position after a Save Cursor.

Scrolling

Scroll Screen		<ESC>[r

    Enable scrolling for entire display.

Scroll Screen		<ESC>[{start};{end}r

    Enable scrolling from row {start} to row {end}.

Scroll Down		<ESC>D

    Scroll display down one line.

Scroll Up		<ESC>M

    Scroll display up one line.

Tab Control

Set Tab 		<ESC>H

    Sets a tab at the current position.

Clear Tab 		<ESC>[g

    Clears tab at the current position.

Clear All Tabs 		<ESC>[3g

    Clears all tabs.

Erasing Text

Erase End of Line	<ESC>[K

    Erases from the current cursor position to the end of the current line.

Erase Start of Line	<ESC>[1K

    Erases from the current cursor position to the start of the current line.

Erase Line		<ESC>[2K

    Erases the entire current line.

Erase Down		<ESC>[J

    Erases the screen from the current line down to the bottom of the screen.

Erase Up		<ESC>[1J

    Erases the screen from the current line up to the top of the screen.

Erase Screen		<ESC>[2J

    Erases the screen with the background colour and moves the cursor to home.

Printing
Some terminals support local printing:

Print Screen		<ESC>[i

    Print the current screen.

Print Line		<ESC>[1i

    Print the current line.

Stop Print Log		<ESC>[4i

    Disable log.

Start Print Log		<ESC>[5i

    Start log; all received text is echoed to a printer.

Define Key

Set Key Definition	<ESC>[{key};"{string}"p

    Associates a string of text to a keyboard key. {key} indicates the key by its ASCII value in decimal.

Set Display Attributes

Set Attribute Mode	<ESC>[{attr1};...;{attrn}m

    Sets multiple display attribute settings. The following lists standard attributes:

    0	Reset all attributes
    1	Bright
    2	Dim
    4	Underscore	
    5	Blink
    7	Reverse
    8	Hidden

    	Foreground Colours
    30	Black
    31	Red
    32	Green
    33	Yellow
    34	Blue
    35	Magenta
    36	Cyan
    37	White

    	Background Colours
    40	Black
    41	Red
    42	Green
    43	Yellow
    44	Blue
    45	Magenta
    46	Cyan
    47	White

[ Top | Status | Setup | Fonts | Cursor | Scrolling | Tabs | Erasing | Printing | Keyboard | Colours ]
	

    

