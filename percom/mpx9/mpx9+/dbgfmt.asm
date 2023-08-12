 PRAGMA cescapes
 nam abc
 ttl abc test
*********************************************************
* abc.CM                              JNS 7/29/2023     *
* 	TEST FOR USING LWAR & LWLINK, ALSO HAVING CODE  *
* RELOCATE IT SELF TO JUST BELOW MPX9                   *
*                                                       *
*********************************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i

 	section	code

FOURTYONE EXPORT

**************************************************
** Program (Position independant)
**************************************************

FOURTYONE:
	PSHS 	Y,U,X,DP,B,A,CC 

	LDX 	10,S GET RETURN ADDRESS
	; USIM
FOURTYONE1:
	LDA   	,X	; GET A CHARACTER
	ANDA  	#$7F	; MASK OFF

	CMPA 	#'%' if format string.
	lbne 	PRINTIT
	; USIM
	LDA 	1,X get next char, '%' OR REG-ID LETTER
	CMPA 	#'%' if % then render %% as %
	bne 	@ckregA
	leax 	1,X adjust for 2 char format string
	lBRA 	PRINTIT
@ckregA
	; USIM
	CMPA 	#'A'	A register?
	BNE 	@ckregB
	lda 	REGA-STACK,S load reg value from stack
	bra 	@FMTBREG
@ckregB
	CMPA 	#'B'	B register?
	BNE 	@ckregCC
	lda 	REGB-STACK,S load reg value from stack
	bra 	@FMTBREG
@ckregCC
	CMPA 	#'C'	CC register?
	BNE 	@ckregDP
	lda 	REGC-STACK,S load reg value from stack
	bra 	@FMTBREG
@ckregDP
	CMPA 	#'Z'	DP register?
	BNE 	@ckregX
	lda 	REGD-STACK,S load reg value from stack
	bra 	@FMTBREG
@ckregX
	CMPA 	#'X'	X register?
	BNE 	@ckregY
	ldd 	REGX-STACK,S load reg value from stack
	bra 	@FMTWREG
@ckregY
	CMPA 	#'Y'	Y register?
	BNE 	@ckregU
	ldd 	REGY-STACK,S load reg value from stack
	bra 	@FMTWREG
@ckregU
	CMPA 	#'U'	U register?
	BNE 	@ckregS
	ldd 	REGU-STACK,S load reg value from stack
	bra 	@FMTWREG
@ckregS
	CMPA 	#'S'	S register?
	BNE 	@ckregD
	ldd 	REGX-STACK,S load reg value from stack
	bra 	@FMTWREG
@ckregD
	CMPA 	#'D'	D register?
	BNE 	@ckregPC
	ldd 	REGA-STACK,S load reg value from stack
	bra 	@FMTWREG
@ckregPC
	CMPA 	#'P'	PC register?
	BNE 	fmterr
	ldd 	REGA-STACK,S load reg value from stack
	bra 	@FMTWREG
@FMTBREG
	leax 	2,X 	adjust pointer
	ldb 	,x+	get format char
	cmpb 	#'c'
	beq 	PRINTIT2
	cmpb 	#'x'
	bne 	@ckdec
	MPX9 	DSPSBY deal with the space?
	BRA 	contfmt
@ckdec
	cmpb 	#'d'
	bne 	fmterr
	exg 	a,b
	clra
	MPX9	DSPDEC deal with the space?
	BRA 	contfmt2
@FMTWREG
	; USIM
	pshs	d
	leax 	2,X 	adjust pointer
	ldb 	,x+	get format char
	cmpb 	#'x'
	bne 	@Wckdec
	puls 	D
	MPX9 	DSPDBY deal with the space?
	BRA 	contfmt2
@Wckdec
	cmpb 	#'d'
	bne 	@Wckstr
	puls 	D
	MPX9	DSPDEC deal with the space?
	BRA 	contfmt2
@Wckstr
	cmpb 	#'s'
	bne 	fmterr
	pshs 	X
	LDX 	2,S
	MPX9	PSTRNG
	puls 	X
	leas 	2,S
	BRA 	contfmt2

PRINTIT2
	MPX9   	OUTCHR	; DISPLAY IT
contfmt2
	TST   	,X	; WAS IT LAST?
	bra 	contfmt1

PRINTIT
	MPX9   	OUTCHR	; DISPLAY IT
contfmt
	TST   	,X+	; WAS IT LAST?
contfmt1	lBPL   	FOURTYONE1	; LOOP IF NOT
endfmt
	STX 	10,S UPDATE RETURN ADDRESS
	PULS 	Y,U,X,DP,B,A,CC,PC
fmterr
	TST   	,X+	; WAS IT LAST?
	BPL   	fmterr	; LOOP IF NOT
	bra 	endfmt


**************************************************

	endsection

 	; section	cend
	; endsection	

**************************************************
** Constants.
**************************************************

 	; section	data

	; endsection	

**************************************************
** Uninitialiazed Working Variables.
**************************************************

;  	section	data

; ; verbose	rmb	1

; 	endsection	

; PGMEND  equ *-1
; PGMSIZ  EQU PGMEND-BGNPGM

 END

