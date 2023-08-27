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

; BEGCOD  equ     *

**************************************************
** Program (Position independant)
**************************************************

CmdLineInit	EXTERN
CmdLine 	EXTERN
s_.bss 		EXTERN
CmdShellInit 	EXTERN
CmdShell 	EXTERN

MPX9SYSCAL	EXPORT
_Start 		EXPORT

_Start:
Abc:

	; clr     >verbose,pcr    ; initialize variables

    
; option:
; 	swi3
; 	fcb     SKPSPC          ; point to the next word
; 	beq     Init  		; No arguments
; 	lda     ,x+
; 	cmpa    #'/             ; look for option flags
; 	bne     Init       ; Not a switch

; option_V:
; 	lda     ,x+             ; get option char and bump pointer
; 	cmpa    #'V             ; is a option 'V'?
; 	bne     Option_S        ; bad switch
; 	com     >verbose,pcr    ; toggle option 'V'
; 	bra     option          ; get next option

; Option_S:
; ; 	cmpa    #'S             ; is a option 'S'?
; ; 	bne     synerr          ; bad switch
; ; 	com     >sysdcbs,pcr    ; toggle option 'S'
; ; 	bra     option          ; get next option

; synerr:
; 	ldb     #ERR_SN         ; Error Syntax
; 	rts
	
; *****************************************************
; Init:
    ; tst     >verbose,pcr

; START INIT

	; CHECK TO SEE IF MPX9+ IS LOADED ALREADY
	ldb 	#$FF
	MPX9	$40
	tstb 
	bne   	AbcGO
	rts
AbcGO:
	pshs	x ; save comand line arg pointer

	; LEAX 	atabc,PCR
	; MPX9	PSTRNG
	; LEAX 	Abc,PCR
	; tfr 	X,D
	; MPX9	DSPDBY

	; LEAX 	_e_f,PCR
	; MPX9	PSTRNG
	; ldd 	#(s_.bss-foo)
	; MPX9	DSPDBY

	MPX9	GETBAS		GET MPX/9 BASE
	; pshs  	X
	; LEAX 	_MPXBAS,PCR
	; MPX9	PSTRNG
	; ldd 	,S
	; MPX9	DSPDBY
	; PULS 	X

	leax 	-(s_.bss-foo),X 	STEP BACK BY SIZE OF MPX9+
	leax 	-$100,x 	make sure to leave room for stack (to be DP latter.)
	tfr  	X,Y
	leax 	foo,PCR
	ldd 	#(s_.bss-foo)

	MPX9	BLKMOV	* X->src, Y->DST, D=LEN (REGISTERS PRESERVED)

	jmp 	,Y

foo:
	; LEAX 	newfoo,PCR
	; MPX9	PSTRNG
	; LEAX 	foo,PCR
	; tfr 	X,D
	; MPX9	DSPDBY
	; jsr 	CRLF

	LDX 	[RAMv] POINT TO PSYMON RAM
	LDX 	FRERAM,X POINT TO MINIDOS/9 RAM

	LDY 	SYSVEC,X 	$B1E6: 81 25 23 04 6e 9d fc a6  80 08 48 30 8c 09 ec 86 |.%#.n.....H0....|
	STY 	SCLVEC,PCR

	LEAY 	SYSCAL,PCR ESTABLISH SYSTEM CALL VECTOR
	STY 	SYSVEC,X
	; LEAX 	SYSCLX,PCR ESTABLISH SYSTEM CALL EXT VECTOR
	; STX 	SCLVEC,PCR

	LEAX 	foo,PCR
	ldd 	#(s_.bss-foo)
	leay  	D,X
	MPX9 	$41
	fcs	/MPX9+ Loaded @ $%Xx- $%Yxlen $%Dx\n\r/

	lbsr 	CmdLineInit
	; LIFT COMMAND LOOP FROM MPX9.ASM#506
ABC_RET	lbsr 	CmdLine


AbcX:
	CLRB	; No Errors
	PULS	pc,x

; **************************************************
; * SYSTEM CALL DISPATCHER - Lifted from mpx9      *
; **************************************************
SYSCAL ; CMPA #SYSLIM IS CALL VALID FOR MPX/9?
 ; USIM
* CHECK FOR SYSTEM CALL IN MPX9+ LIST
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

 leay 	SCLVEC,PCR
 ldy 	,y
 jmp 	,y

 ; JMP [SCLVEC,PCR] EXIT TO NEXT LEVEL ROUTINE

 SPC 1
; SYSCLX RTI DUMMY ROUTINE
 SPC 1

****************************************************
* MPX9+ go to old SYSTEM CALL handler              *
****************************************************
****************************************************
* ENTRY REQUIREMENTS:  A contains the MPX9 syscal# *
*                      Stack contains complete set *
*                      of registers minus U        *
* EXIT CONDITIONS:  EXITS TO SELECTED ROUTINE      *
*                                                  *
* example                                          *
*  PSHS   CC,A,B,DP,X,Y,PC MOCK SWI                *
*  lda    #PROCMD                                  *
* MPX9SYSCAL EXTERN                                *
*  jmp    MPX9SYSCAL                               *
*                                                  *
*                                                  *
* TODO: Create a Macro for this!!                  *
*                                                  *
****************************************************
MPX9SYSCAL: ; (enter a=syscal#, stack=full set of registers)
 leay 	SCLVEC,PCR A=MPX9 SYSCAL #
 ldy 	,y
 jmp 	,y

; **************************************************
; * MPX9+ SYSTEM CALLS                             *
; **************************************************

FOURTY:
	clrb 
	rts 

**************************************************
* SYSTEM CALL 8 (MPX) - RETURN TO MPX/9          *
**************************************************
MPXRET 
 ; LEAS STACK,PCR RESET THE STACK
 ; LDA #CR FORCE RELOAD OF LINE BUFFER
 ; STA LINBUF,PCR
 BRA ABC_RET


FOURTYONE EXTERN

; **************************************************
; * MPX9+ SYSTEM CALL OFFSET LOOKUP TABLE          *
; **************************************************
SYSOFF:

 FCB $40
 FDB FOURTY-SYSOFF 	- IS MPX9+ LOADED?
 FCB $41
 FDB FOURTYONE-SYSOFF 	- GET A LINE OF INPUT

NPROCM EXTERN
 FCB PROCMD
 FDB NPROCM-SYSOFF 	- New process command.

 FCB MPX		- RETURN TO MPX9+
 FDB MPXRET-SYSOFF 	- New process command.
  


 FCB 0 END OF TABLE MARK



**************************************************

	endsection	; section	.text

**************************************************
** Constants.
**************************************************

 	section	.data

SYSVEC EQU 64 SYSTEM CALL VECTOR
SCLVEC RMB 2 SYSTEM CALL VECTOR

; GreetingsMsg:
; 	fcs /Greetings!\r\n/
atabc	fcs /@abc:/
_e_f	fcs /\r\n_End-foo:/
_MPXBAS	fcs /\r\n_MPXBAS:/
newfoo	fcs /\r\nNew @foo:/

	endsection	; section .data

**************************************************
** Uninitialiazed Working Variables.
**************************************************

;  	section .bss

; ; verbose	rmb	1

; 	endsection	; section .bss

; PGMEND  equ *-1
; PGMSIZ  EQU PGMEND-BGNPGM

 END

