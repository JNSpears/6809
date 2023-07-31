 nam abc
 ttl abc test
*********************************************************
* abc.CM                              JNS 7/29/2023     *
* 	TEST FOR USING LWAR & LWLINK, ALSO HAVING CODE  *
* RELOCATE IT SELF TO JUST BELOW MPX9                   *
*                                                       *
*********************************************************

        include mpx9.i
        include ascii.i
        include jns.i

 	section	code

BEGCOD  equ     *

**************************************************
** Program (Position independant)
**************************************************

Abc1 EXTERN
Abc2 EXTERN

_Start EXPORT
_Start:
Abc:
	pshs	x

	LEAX 	atabc,PCR
	MPX9	PSTRNG
	LEAX 	Abc,PCR
	tfr 	X,D
	MPX9	DSPDBY
	jsr 	CRLF

	LEAX 	_e_s,PCR
	MPX9	PSTRNG
	ldd 	#_End-_Start
	MPX9	DSPDBY
	jsr 	CRLF

	MPX9	GETBAS
	leax 	-(_End-_Start),X
	tfr  	X,Y
	leax 	foo,PCR
	ldd 	#_End-_Start
	MPX9	BLKMOV	* X->src, Y->DST, D=LEN (REGISTERS PRESERVED)

	jmp 	,Y

foo:
	LEAX 	newfoo,PCR
	MPX9	PSTRNG
	tfr 	X,D
	MPX9	DSPDBY
	jsr 	CRLF

	leax	GreetingsMsg,PCR
	MPX9	PSTRNG

	LBSR 	Abc1
	LBSR 	Abc2

AbcX:
	CLRB	; No Errors
	PULS	pc,x


**************************************************

	endsection

 	section	cend
_End equ *
	endsection	

**************************************************
** Constants.
**************************************************

 	section	data

GreetingsMsg:
	FCC /Greetings!/
	FCB CR,LF+$80
atabc	fcs /@abc:/
_e_s	fcs /_End-_Start:/
newfoo	fcs /New @foo:/

	endsection	

**************************************************
** Uninitialiazed Working Variables.
**************************************************

 	section	data

; verbose	rmb	1

	endsection	

PGMEND  equ *-1
PGMSIZ  EQU PGMEND-BGNPGM

 END

