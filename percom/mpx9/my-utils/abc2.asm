 nam Abc2
 ttl Abc2 test
*********************************************************
* Abc2.CM                             JNS 7/29/2023     *
*                                                       *
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

Abc2 EXPORT
Abc2:
	pshs	x

	LEAX 	<Abc2,PCR
	tfr 	X,D
	MPX9	DSPDBY

	leax	<GreetingsMsg,PCR
	MPX9	PSTRNG

Abc2X:
	CLRB	; No Errors
	PULS	pc,x


**************************************************

endcod  equ *-1

	endsection	

**************************************************
** Constants.
**************************************************

 	section	data

GreetingsMsg:
	FCC /Greetings! from Abc2/
	FCB CR,LF+$80

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

