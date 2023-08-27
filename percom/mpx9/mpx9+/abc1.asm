 PRAGMA cescapes
 nam Abc1
 ttl Abc1 test
*********************************************************
* Abc1.CM                             JNS 7/29/2023     *
*                                                       *
*                                                       *
*********************************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i

 	section	.text

; BEGCOD  equ     *

**************************************************
** Program (Position independant)
**************************************************

Abc1 EXPORT
Abc1:
	pshs	x

	LEAX 	<Abc1,PCR
	tfr 	X,D
	MPX9	DSPDBY

	leax	GreetingsMsg,PCR
	MPX9	PSTRNG

Abc1X:
	CLRB	; No Errors
	PULS	pc,x


**************************************************

; endcod  equ *-1

	endsection	

**************************************************
** Constants.
**************************************************

 	section .data

GreetingsMsg:
	FCS /Greetings! from Abc1\r\n/

	endsection	; section .data	

**************************************************
** Uninitialiazed Working Variables.
**************************************************

 	; section .bss

	; endsection 	; section .bss	

; PGMEND  equ *-1
; PGMSIZ  EQU PGMEND-BGNPGM

 END

