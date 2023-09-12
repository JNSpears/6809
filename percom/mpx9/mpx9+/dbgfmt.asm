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

 	section	.text

KernalDbgFmt EXPORT

**************************************************
** Program (Position independant)
**************************************************

KernalDbgFmt:
	PSHS 	Y,U,X,DP,B,A,CC 
	LDX 	10,S GET RETURN ADDRESS

LOOP:	LDA   	,X	; GET A CHARACTER
	ANDA  	#$7F	; MASK OFF
	CMPA 	#'%' if format string.
	lbne 	PRINTIT	; Go print the char
	leax    1,X
	LDA 	,X get next char, '%' OR REG-ID LETTER
	CMPA 	#'%' if % then render %% as %
	lbeq 	PRINTIT
	leax  	1,X
	; A = register id, X --> format char
ckregA
	CMPA 	#'A'	A register?
	BNE 	ckregB
	lda 	REGA-STACK,S load reg value from stack
	bra 	FMTBREG
ckregB
	CMPA 	#'B'	B register?
	BNE 	ckregCC
	lda 	REGB-STACK,S load reg value from stack
	bra 	FMTBREG
ckregCC
	CMPA 	#'C'	CC register?
	BNE 	ckregDP
	lda 	REGC-STACK,S load reg value from stack
	bra 	FMTBREG
ckregDP
	CMPA 	#'Z'	DP register?
	BNE 	ckregX
	lda 	REGD-STACK,S load reg value from stack
	bra 	FMTBREG
ckregX
	CMPA 	#'X'	X register?
	BNE 	ckregY
	ldd 	REGX-STACK,S load reg value from stack
	bra 	FMTWREG
ckregY
	CMPA 	#'Y'	Y register?
	BNE 	ckregU
	ldd 	REGY-STACK,S load reg value from stack
	bra 	FMTWREG
ckregU
	CMPA 	#'U'	U register?
	BNE 	ckregS
	ldd 	REGU-STACK,S load reg value from stack
	bra 	FMTWREG
ckregS
	CMPA 	#'S'	S register?
	BNE 	ckregD
	; ldd 	REGX-STACK,S load reg value from stack
	TFR 	S,D load register value with stack register
	bra 	FMTWREG
ckregD
	CMPA 	#'D'	D register?
	BNE 	ckregPC
	ldd 	REGA-STACK,S load reg value from stack
	bra 	FMTWREG
ckregPC
	CMPA 	#'P'	PC register?
	BNE 	fmterr
	ldd 	REGP-STACK,S load reg value from stack
	bra 	FMTWREG

	; A = Register Value, X --> format char
FMTBREG
	ldb 	,X	Get format character
	cmpb 	#'c' 	Format as Character
	beq 	PRINTIT
	cmpb 	#'x'	Format as Hex number
	bne 	Bckdec
	MPX9 	DSPSBY 	deal with the space?
	BRA 	contfmt
Bckdec
	cmpb 	#'d'	Format as Decimal number
	bne 	fmterr
	exg 	a,b
	clra
	MPX9	DSPDEC 	deal with the space?
	BRA 	contfmt

	; D = Register Value, X --> format char
FMTWREG
	pshs	d
	ldb 	,x	Get format character
	cmpb 	#'x'	Format as Hex number
	bne 	Wckdec
	puls 	D
	MPX9 	DSPDBY deal with the space?
	BRA 	contfmt
Wckdec
	cmpb 	#'d'	Format as Decimal number
	bne 	Wckstr
	puls 	D
	MPX9	DSPDEC deal with the space?
	BRA 	contfmt
Wckstr
	cmpb 	#'s'	Format as pointer to String
	bne 	fmterr
	pshs 	X
	LDX 	2,S 	Get saved D from stack
	MPX9	PSTRNG
	puls 	X
	leas 	2,S 	drop saved D
	BRA 	contfmt

PRINTIT
	MPX9   	OUTCHR	; DISPLAY IT

	; X --> a character or last character of a format
contfmt
	TST   	,X+	; WAS IT LAST?
	lBPL   	LOOP	; LOOP IF NOT
endfmt
	STX 	10,S UPDATE RETURN ADDRESS
	PULS 	Y,U,X,DP,B,A,CC,PC
fmterr
	TST   	,X+	; WAS IT LAST?
	BPL   	fmterr	; LOOP IF NOT
	bra 	endfmt


**************************************************

	endsection

**************************************************
** Constants.
**************************************************

	; section	.data

	; endsection 	; section .data	

**************************************************
** Uninitialiazed Working Variables.
**************************************************

	; section	.bss

	; endsection 	; section .bss	

; PGMEND  equ *-1
; PGMSIZ  EQU PGMEND-BGNPGM

 END

