 PRAGMA cescapes
 nam Abc4
 ttl Abc4 test
*********************************************************
* Abc4.CM                             JNS 7/29/2023     *
*                                                       *
*                                                       *
*********************************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i

 	; section	.text
	
	ORG 0
BEGCODE equ     *

**************************************************
** Program (Position independant)
**************************************************
; NOTE: make a Macro & Structure for this
	; TWO BYTE SIGNATURE FOR MODULE, AND SAFE TO EXECUTE AS A PROGRAM.
	nop
	rts 
	FDB 	Abc4 		; Entry point
	FDB	ENDCODE-BEGCODE	; code size (copied from disk)
	FDB	ENDDATA-BEGDATA	; data size (allocated un initialiazed)
	FCS	'Abc4'		; Module Name


; Abc4 EXPORT
Abc4:
	pshs	x

	LEAX 	<Abc4,PCR
	tfr 	X,D
	MPX9	DSPDBY

	leax	GreetingsMsg,PCR
	MPX9	PSTRNG

Abc4X:
	CLRB	; No Errors
	PULS	pc,x

**************************************************
** Constants.
**************************************************

 	; section .data

GreetingsMsg:
	FCS /Greetings! from Abc4\r\n/

	; endsection	; section .data	


**************************************************

ENDCODE  equ *

	; endsection	

**************************************************
** Uninitialiazed Working Variables.
**************************************************


BEGDATA	equ	*
FOO 	rmb	1
ENDDATA	equ 	*

 END

