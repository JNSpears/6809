 PRAGMA cescapes
 nam Abc2
 ttl Abc2 test
*********************************************************
* Abc2.CM                             JNS 7/29/2023     *
*                                                       *
*                                                       *
*********************************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i

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

	leax	GreetingsMsg,PCR
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
	FCS /Greetings! from Abc2\r\n/

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

