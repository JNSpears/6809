 nam nCmdLine
*********************************************************
* nCmdLine.CM                           JNS 7/22/2023   *
*                                                       *
*                                                       *
*********************************************************

	INCLUDE mpx9.i
	INCLUDE psymon-ext.i


MPX9 MACRO   NOEXPAND
	SWI3
	FCB \1
	ENDM

USIM MACRO   NOEXPAND
	FCB 2
	ENDM

**************************************************
* Program (Position independant)
**************************************************
        ORG     $0
BGNPGM	EQU		*
; *****************************************************
Main:
	clr     >verbose,pcr    ; initialize variables

    
option:
	swi3
	fcb     SKPSPC          ; point to the next word
	beq     MainStart  		; No arguments
	lda     ,x+
	cmpa    #'/             ; look for option flags
	bne     MainStart       ; Not a switch

option_V:
	lda     ,x+             ; get option char and bump pointer
	cmpa    #'V             ; is a option 'V'?
	bne     Option_S        ; bad switch
	com     >verbose,pcr    ; toggle option 'V'
	bra     option          ; get next option

Option_S:
; 	cmpa    #'S             ; is a option 'S'?
; 	bne     synerr          ; bad switch
; 	com     >sysdcbs,pcr    ; toggle option 'S'
; 	bra     option          ; get next option

synerr:
	ldb     #ERR_SN         ; Error Syntax
	rts
	
; *****************************************************
MainStart:
    tst     >verbose,pcr
    ; beq     Nosysdcbs


LOOP:
	LEAX 	>prompt,pcr			; DISPLAY PROMPT
 	MPX9 	PSTRNG

 	; Initialize buffer and other variables.
 	LDB 	#MaxCmdLineLen 		; SET SIZE IN B	
 	LEAU    data,pcr
 	LEAX 	<<VAR.CmdLineBuffer,U 	; POINT X AT LINE BUFFER
ClrBuffer:
 	CLR 	B,X
 	DECB 
 	BNE 	ClrBuffer

 	; setup and get a line.
 	LDB 	#MaxCmdLineLen 		; SET SIZE IN B	
	BSR  	getline				; x=buffer, b=bufferlen --> a=firstchar, b=bufferlen, x=buffer


	; dump buffer just for debug
	CLRA
 	LDB 	#MaxCmdLineLen 		; SET SIZE IN B	
	JSR     [DumpMem2v]			; dump buffer DCB data.

	CLRA
 	LDB 	#3 		; SET SIZE IN B	
 	LEAX 	<<VAR.max,U
	JSR     [DumpMem2v]			; dump buffer DCB data.

	; USIM

	; check to see if we are done for now.
	lda 	<<VAR.left,U
	adda 	<<VAR.right,U
	bne   	LOOP



; exec command.
; bra loop

	clrb 		; no error
	rts			; Return to OS

; *****************************************************
InChrNEcho:
  PSHS  X,B		; SAVE REGISTERS
  LDX   CIDCB	; POINT TO INPUT DCB
  LDB   #ReadFn	; SET UP FOR READ
  MPX9  REQIO	; READ A CHARACTER
  ANDA  #$7F	; REMOVE PARITY
  PULS  B,X,PC 	; RESTORE & RETURN

; *****************************************************
GetConStat:
  PSHS  X,B		; SAVE REGISTERS
  LDX   CIDCB	; POINT TO INPUT DCB
  LDB   #StatFn	; SET UP FOR GET STATUS
  MPX9  REQIO	; READ A CHARACTER
  PULS  B,X,PC 	; RESTORE & RETURN

; *****************************************************
OutEscBrkNChr: ; IN A=CHAR, B=NUMBER (0 SUPPRESS)
  pshs B,A
  ; usim
  LDA #ESC
  MPX9 OUTCHR
  LDA #'['
  MPX9 OUTCHR
  tstb
  BEQ OutEscBrkNChr1
; TWO DIGIT DECIMAL CONVERSION, NO LEADING SPACE OR ZERO
  tfr B,A
  ADDA #0
  daa
  cmpa #9
  ble OutEscBrkNChr2
  TFR A,B
  ASRA
  ASRA
  ASRA
  ASRA
  BSR OUTDIGIT
  TFR b,A
OutEscBrkNChr2:
  BSR OUTDIGIT
OutEscBrkNChr1:
  LDA ,S
  MPX9 OUTCHR
  PULS A,B,PC

OUTDIGIT:
  ANDA #$F
  ADDA #'0'
  MPX9 OUTCHR
  rts 

; *****************************************************
OUTNCHR: ; IN: A-CHAR B-COUNT OUT: A,B MODIFIED.
 MPX9 OUTCHR
 decb 
 bne OUTNCHR
 RTS

**************************************************
* SYSTEM CALL 9 (GETLIN) - GET INPUT LINE        *
*                                                *
* ENTRY REQUIREMENTS:  X POINTS AT LINE BUFFER   *
*                      B CONTAINS BUFFER LENGTH  *
*                                                *
* EXIT CONDITIONS:  A CONTAINS FIRST CHARACTER,  *
*                     NUL => LINE CANCELED       *
*                     Z FLAG IN CC SET PER A     *
*                   OTHERS UNCHANGED             *
**************************************************
getline:
GETLN:
 stb <<VAR.max,U
 PSHS B,X,Y SAVE REGISTERS
 clr <<VAR.left,U  RESET CHARACTER COUNT
 clr <<VAR.right,U

GetCharLoop:
 BSR InChrNEcho GET NEXT CHARACTER

 CMPA #CR END OF INPUT?
 BEQ Done GO IF YES
 
 CMPA #ESC START OF ESCAPE SEQ?
 LBEQ CheckEscapeSeq GO IF YES
 
 CMPA #SP CONTROL CODE?
 BLO CheckOneChar 

 CMPA #TILDE Invalide Ascii
 BHI CheckOneChar 

 ; save this character in buffer, output char, update left count
 ldb <<VAR.left,U 
 STA B,X SAVE THIS CHARACTER
 MPX9 OUTCHR
 INC <<VAR.left,U UPDATE COUNT

 ; if characters to the right
 ldb <<VAR.right,U
 beq  GetCharLoop2

 bsr OutRightChars

 ; and back space to return cursor
 lda #BS
 ldb <<VAR.right,U
 ; OUTNCHR: IN: A-CHAR B-COUNT OUT: A,B MODIFIED.
 bsr OUTNCHR

GetCharLoop2:

 CMPB ,S PAST LIMIT?
 BLO GetCharLoop LOOP IF NOT

 LDA #BS OUTPUT A BACKSPACE
 MPX9 OUTCHR
 BRA GetCharLoop

Done:

 PSHS A,X
 lbsr  	NormBuffer
 PULS X,A
 ldb <<VAR.left,U 
 STA B,X MARK END OF LINE W/ CR
 LDA [1,S] GET FIRST CHARACTER OF LINE

GETLNX 
 TSTA SET Z FLAG PER A
 PULS B,X,Y,PC RETURN

**********************************
 ; output right chars
OutRightChars:
 clrb 
OutRightChars1:
 tfr b,a
 adda <<VAR.max,U
 suba <<VAR.right,U
 lda a,x
 MPX9 OUTCHR
 incb
 cmpb <<VAR.right,U
 blt OutRightChars1
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
 BRA	GetCharLoop

TBL1:
 FCB 'Z-'@
 FDB ESCcmd-*

 FCB ESC
 FDB ESCcmd-*

 FCB BS
 FDB BScmd-*

 FCB $7F
 FDB DELcmd-*

 FCB 0 END OF TABLE MARK

**************************************************
ESCcmd0: 
ESCcmd:
 ldb   	<<VAR.left,U

 BSR 	MoveCurLeft

 BSR 	EraseEOL

 Clr  	<<VAR.left,U
 Clr  	<<VAR.right,U

 BRA	GetCharLoop_2


**************************************************
BScmd:
 tst  	<<VAR.left,U 
 beq 	BScmdX

 dec   	<<VAR.left,U 
 lda 	#BS
 MPX9  	OUTCHR

 ; output right chars
 bsr OutRightChars

 lda 	#SP
 MPX9  	OUTCHR

 ldb   <<VAR.right,U
 incb
 lda 	#BS
 Lbsr 	OUTNCHR		; output N BS characters, return cursor ot original location

BScmdX:
 BRA	GetCharLoop_2


**************************************************
DELcmd:
 ldb <<VAR.right,U
 beq DELcmdX

 DEC <<VAR.right,U
 beq DELcmdX

 ; output right chars
 clrb 
DELcmd1:
 tfr b,a
 adda <<VAR.max,U
 suba <<VAR.right,U
 lda a,x
 MPX9 OUTCHR
 incb
 cmpb <<VAR.right,U
 blt DELcmd1

DELcmdX:
 BSR 	EraseEOL

 ldb 	<<VAR.right,U
 BSR 	MoveCurLeft
 BRA	GetCharLoop_2

**************************************************
MoveCurRight:
 ; tstb 
 ; beq 	CommonX
 LDA 	#'C
 LBRA 	OutEscBrkNChr
CommonX:
 rts

MoveCurLeft:
 ; tstb 
 ; beq 	CommonX
 LDA 	#'D
 LBRA OutEscBrkNChr

EraseEOL:
 CLRB
 LDA 	#'K
 LBRA OutEscBrkNChr

**************************************************
CheckEscapeSeq:
 pshs X
 ldx #0
CheckEscapeSeq1:
 LEAX 1,X
 cmpx #$200
 BLE CheckEscapeSeq2
 puls X
 bra ESCcmd0
CheckEscapeSeq2:
 lbsr GetConStat
 LSRa  		; BIT TO C
 BCC   CheckEscapeSeq1		; LOOP IF NO INPUT
 ; MPX9 DSPDBY
 PULS X

 LBSR InChrNEcho GET NEXT CHARACTER SHOULD BE '['
 cmpa #'[
 bne CheckEscapeSeqX
 tfr A,B
 LBSR InChrNEcho GET NEXT CHARACTER
 exg a,b
 ; d = 2 CHAR SEQUENCE

 pshs X
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
 FDB CLEFTcmd-*

 ; [C 5B43 CURSOR RIGHT
 FCC /[C/
 FDB CRIGHTcmd-*

 ; [H 5B48 CURSOR HOME
 FCC /[H/
 FDB CHOMEcmd-*

 ; [F 5B46 CURSOR END
 FCC /[F/
 FDB CENDcmd-*

 FCB 0 END OF TABLE MARK

; *****************************************************
CLEFTcmd: 
 LDB   	<<VAR.left,U
 beq 	CLEFTcmdX

 CLRB 
 BSR	MoveCurLeft

 ; PSHS X
 ; LEAX ABC,PCR
 ; MPX9 PSTRNG
 ; PULS X

 DEC 	<<VAR.left,U
 INC 	<<VAR.right,U

 LDB 	<<VAR.left,U
 LDA 	B,X
 LDB 	<<VAR.max,U
 SUBB   <<VAR.right,U
 STA 	B,X

CLEFTcmdX:
 LBRA	GetCharLoop

; ABC: FCS /<<</

; *****************************************************
CRIGHTcmd:
 ldb   	<<VAR.right,U
 beq 	CRIGHTcmdX
 clrb 
 LBSR	MoveCurLeft

 ldb 	<<VAR.max,U
 subb   <<VAR.right,U
 lda 	b,x
 dec 	<<VAR.right,U
 
 ldb 	<<VAR.left,U
 sta 	b,x
 inc  	<<VAR.left,U

CRIGHTcmdX:
 LBRA	GetCharLoop

XCURRIGHT1: FCB ESC
 FCS /[C/

; *****************************************************
CHOMEcmd:
 ldb   	<<VAR.left,U
 beq 	CHOMEcmdX

 ldb    <<VAR.left,U
 lda 	#BS
 Lbsr 	OUTNCHR		; output N BS characters, return cursor ot original location

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

CHOMEcmdX:
 LBRA	GetCharLoop

; *****************************************************
CENDcmd:
 ldb   	<<VAR.right,U
 beq 	CENDcmdX


 ; clrb 
 ; lda #'D'
 ; LJSR	OutEscBrkNChr


 pshs 	X
 ldb   	<<VAR.right,U
CENDcmd1:
 LEAX 	>XCURRIGHT1,pcr
 MPX9 	PSTRNG
 decb 
 BNE CENDcmd1
 
 ldx 	,S

 bsr  	NormBuffer

 PULS 	X

CENDcmdX:
 LBRA	GetCharLoop

; *****************************************************
NormBuffer: 
 ; MOVE from(X)=X+max-right, to(Y)=X+left, count(D)=right
 
* ENTRY REQUIREMENTS:  X POINTS TO SOURCE FIELD  *
*                      Y POINTS TO DEST. FIELD   *
*                      D CONTAINS LENGTH         *
 lda 	<<VAR.left,U
 leay  	a,x

 lda 	<<VAR.max,U
 suba   <<VAR.right,U
 leax 	a,x
 clra 
 ldb 	<<VAR.right,U
 MPX9 	BLKMOV

 LDA 	<<VAR.left,U
 adda 	<<VAR.right,U
 sta 	<<VAR.left,U
 
 Clr  	<<VAR.right,U
 rts

; *****************************************************

endcod  equ *-1

*
** Constants.
*

prompt FCB CR,LF
 fcs '> '

*
** Uninitialiazed Working Variables.
*

verbose	rmb	1


MaxCmdLineLen	equ	64
VAR	STRUCT
max 		rmb 1
left		rmb 1
right		rmb	1
CmdLineBuffer	rmb	MaxCmdLineLen
	ENDSTRUCT

; tstruct2.f1 (0),
; tstruct2.f2 (1), 
; sizeof{tstruct2} (2),
; tstruct.field1 (0), 
; tstruct.field2 (2), 
; tstruct.field3 (5), 
; tstruct.field3.f1 (5), 
; tstruct.field3.f2 (6), 
; sizeof{tstruct.field3} (2), 
; sizeof{tstruct} (7), 
; var1 {$2000}, 
; var1.field1 {$2000}, 
; var1.field2 {$2002}, 
; var1.field3 {$2005}, 
; var1.field3.f1 {$2005}, 
; var1.field3.f2 {$2006}, 
; sizeof(var1.field3} (2), 
; sizeof{var1} (7),
; var2 ($2007), 
; var2.f1 ($2007), 
; var2.f2 ($2008), 
; sizeof{var2} (2). 

data VAR

; * CONFIGURATION PARAMETERS
; SYSBS RMB 1 SYSTEM BACKSPACE CODE
; SYSBSE RMB 4 SYSTEM BACKSPACE ECHO STRING
; SYSCAN RMB 1 SYSTEM CANCEL CODE



PGMEND  equ *-1
PGMSIZ  EQU PGMEND-BGNPGM

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
	

    

