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

BEGCOD  equ     *

**************************************************
** Program (Position independant)
**************************************************

Abc1 EXTERN
Abc2 EXTERN
Abc3 EXTERN

_Start EXPORT
_Start:
Abc:
	pshs	x

	LEAX 	atabc,PCR
	MPX9	PSTRNG
	LEAX 	Abc,PCR
	tfr 	X,D
	MPX9	DSPDBY

	LEAX 	_e_f,PCR
	MPX9	PSTRNG
	ldd 	#(_End-foo)
	MPX9	DSPDBY

	MPX9	GETBAS		GET MPX/9 BASE
	pshs  	X

	LEAX 	_MPXBAS,PCR
	MPX9	PSTRNG
	ldd 	,S
	MPX9	DSPDBY
	PULS X
	leax 	-(_End-foo),X 	STEP BACK BY SIZE OF MPX9+
	leax 	-$100,x 	make sure to leave room for stack (to be DP latter.)
	tfr  	X,Y
	leax 	foo,PCR
	ldd 	#(_End-foo)
	MPX9	BLKMOV	* X->src, Y->DST, D=LEN (REGISTERS PRESERVED)

	jmp 	,Y

foo:
	LEAX 	newfoo,PCR
	MPX9	PSTRNG
	LEAX 	foo,PCR
	tfr 	X,D
	MPX9	DSPDBY
	jsr 	CRLF

	; MPX9	GETBAS
	; USIM

	LDX 	[RAMv] POINT TO PSYMON RAM
	LDX 	FRERAM,X POINT TO MINIDOS/9 RAM

	LDY 	SYSVEC,X 	$B1E6: 81 25 23 04 6e 9d fc a6  80 08 48 30 8c 09 ec 86 |.%#.n.....H0....|
	STY 	SCLVEC,PCR

	LEAY 	SYSCAL,PCR ESTABLISH SYSTEM CALL VECTOR
	STY 	SYSVEC,X
	; LEAX 	SYSCLX,PCR ESTABLISH SYSTEM CALL EXT VECTOR
	; STX 	SCLVEC,PCR

	leax	GreetingsMsg,PCR
	MPX9	PSTRNG

	LBSR 	Abc1
	LBSR 	Abc2
	; USIM
	LBSR 	Abc3

	lda 	#'@'
	MPX9	$41
	fcs	/test a=%Ac\n\r/
AbcX:
	CLRB	; No Errors
	PULS	pc,x

; **************************************************
; * SYSTEM CALL DISPATCHER - Lifted from mpx9      *
; **************************************************
SYSCAL ; CMPA #SYSLIM IS CALL VALID FOR MPX/9?

* CHECK FOR SYSTEM CALL IN MPX9ii LSIT
 LEAY SYSOFF,PCR POINT Y AT OFFSET TABLE
RESCM1 TST ,Y END OF TABLE?
 BEQ NotFound GO IF YES
 CMPA ,Y+ FIND COMMAND?
 BEQ RESCM2 GO IF YES
 LEAY 2,Y ADVANCE POINTER
 BRA RESCM1 LOOP

RESCM2
 LDD ,Y GET OFFSET TO ROUTINE
 LEAX SYSOFF,PCR POINT X AT OFFSET TABLE
 LEAX D,X GET ROUTINE ADDRESS
 STX 8,S
 PULS CC,A,B,DP,X,Y,PC TURN SWI CALL INTO JSR
 SPC 1
NotFound

 ; USIM
 leay 	SCLVEC,PCR
 ; LDY 	#SCLVEC                   ,PCR

 ldy 	,y
 jmp 	,y


 ; JMP [SCLVEC,PCR] EXIT TO NEXT LEVEL ROUTINE

 SPC 1
; SYSCLX RTI DUMMY ROUTINE
 SPC 1

FOURTYONE EXTERN

 SPC 1
* SYSTEM CALL OFFSET LOOKUP TABLE
SYSOFF:

 FCB $41
 FDB FOURTYONE-SYSOFF - GET A LINE OF INPUT

 FCB 0 END OF TABLE MARK



**************************************************

	endsection

 	section	cend
_End equ *
	endsection	

**************************************************
** Constants.
**************************************************

 	section	data

SYSVEC EQU 64 SYSTEM CALL VECTOR
SCLVEC RMB 2 SYSTEM CALL VECTOR

GreetingsMsg:
	fcs /Greetings!\r\n/
atabc	fcs /@abc:/
_e_f	fcs /\r\n_End-foo:/
_MPXBAS	fcs /\r\n_MPXBAS:/
newfoo	fcs /\r\nNew @foo:/

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

