 PRAGMA cescapes
 nam Abc3
 ttl Abc3 test
*********************************************************
* Abc3.CM                             JNS 7/29/2023     *
*                                                       *
*                                                       *
*********************************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i

 	section	code

; BEGCOD  equ     *

**************************************************
** Program (Position independant)
**************************************************

Abc3 EXPORT
Abc3:
	pshs	x

	tfr 	pc,d
	leax	GreetingsMsg,PCR
	MPX9	$41
	fcs	/TEST3 PC=%Px msg='%Xs'\r\n/

Abc3X:
	CLRB	; No Errors
	PULS	pc,x


**************************************************

	endsection	

**************************************************
** Constants.
**************************************************

 	section	data

GreetingsMsg:
	FCS /Greetings! from Abc3\r\n/

	endsection	

**************************************************
** Uninitialiazed Working Variables.
**************************************************

 	section	data

; verbose	rmb	1

	endsection	

; PGMEND  equ *-1
; PGMSIZ  EQU PGMEND-BGNPGM

 END

