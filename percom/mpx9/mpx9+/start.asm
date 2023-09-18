 PRAGMA cescapes
 nam Start
 ttl Start
*********************************************************
*                                     JNS 7/29/2023     *
*                                                       *
*********************************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i
        INCLUDE mpx9+.i

 	section	.text

; BEGCOD  equ     *

**************************************************
** Program (Position independent)
**************************************************

CmdLine 	EXTERN
CmdLineInit	EXTERN
CmdShell 	EXTERN
CmdShellInit 	EXTERN
KernalAllocInit	EXTERN
KernalAlloc	EXTERN
KernalDbgFmt    EXTERN
NPROCM 		EXTERN
RptErrorInit	EXTERN
s_.bss 		EXTERN
ModulesInit 	EXTERN

MPX9SYSCAL	EXPORT
_Start 		EXPORT

_Start:
	lda 	#$BF
	tfr     a,dp

	clra 
	clrb
	clr     <verbose	; initialize variables
	; std 	<SYSOFF2.pHead 	; NO DYNAMIC SYSTEM CALL TABLE
	; std 	<SYSOFF2.pEnd 	; zero length of allocated DYNAMIC SYSTEM CALL TABLE
	; std 	<SYSOFF2.pNext 	; zero pointer to next entry in the DYNAMIC SYSTEM CALL TABLE

option:
	MPX9    SKPSPC		; point to the next word
	beq     Init		; No arguments
	lda     ,x
	cmpa    #'/		; look for option flags
	bne     Init		; Not a switch
	leax 	1,X 		; move past switch char
	lda     ,x+		; get option char and bump pointer

option_V:
	cmpa    #'V		; is a option 'V'?
	bne     Option_S	; bad switch
	com     <verbose	; toggle option 'V'
	bra     option		; get next option

Option_S:
; ; 	cmpa    #'S             ; is a option 'S'?
; ; 	bne     synerr          ; bad switch
; ; 	com     >sysdcbs,pcr    ; toggle option 'S'
; ; 	bra     option          ; get next option

synerr:
	ldb     #ERR_SN		; Error Syntax
	rts
	
; *****************************************************
Init:
	; CHECK TO SEE IF MPX9+ IS LOADED ALREADY
	ldb 	#$FF
	MPX9	MPX9LOADED
	tstb 
	bne   	Good2Go
	rts
Good2Go:
	pshs	x ; save command line arg pointer

	; LEAX 	atabc,PCR
	; MPX9	PSTRNG
	; LEAX 	Abc,PCR
	; tfr 	X,D
	; MPX9	DSPDBY

	; LEAX 	_e_f,PCR
	; MPX9	PSTRNG
	; ldd 	#(s_.bss-foo)
	; MPX9	DSPDBY

	MPX9	GETBAS		; GET MPX/9 BASE

	; pshs  X
	; LEAX 	_MPXBAS,PCR
	; MPX9	PSTRNG
	; ldd 	,S
	; MPX9	DSPDBY
	; PULS 	X

	leax 	-(s_.bss-foo),X  	; STEP BACK BY SIZE OF MPX9+
	leax 	-$100,x 		; make sure to leave room for stack (to be DP latter.)
	tfr  	X,Y
	leax 	foo,PCR
	ldd 	#(s_.bss-foo)

	MPX9	BLKMOV			; X->src, Y->DST, D=LEN (REGISTERS PRESERVED)

	jmp 	,Y

foo:	
; **********************************************************************
; **********************************************************************
; ***                                                                ***
; *** ALL CODE ABOVE THIS POINT IS NOT RELOCATED TO HIGHER MEMORY.   ***
; ***                                                                ***
; **********************************************************************
; **********************************************************************
	LDX 	[RAMv] 		POINT TO PSYMON RAM
	LDX 	FRERAM,X 	POINT TO MINIDOS/9 RAM

	LDY 	SYSVEC,X
	STY 	<SCLVEC

; ABOVE 4 INSTUCTIONS COULD BE MOVED UP JUST AFTER Good2Go

	LEAY 	SYSCAL,PCR 	ESTABLISH SYSTEM CALL VECTOR
	STY 	SYSVEC,X

	LEAX 	foo,PCR
	ldd 	#(s_.bss-foo)
	leay  	D,X
	MPX9 	DBGFMT
	fcs	/MPX9+ Loaded @ $%Xx- $%Yxlen $%Dx\n\r/

	; debug and diag help.
    	tst     <verbose
    	Lbeq 	@NoDebug
	MPX9	GETBAS	GET MPX/9 MEMORY X:MPXRAM, Y:MPXBAS AND LEN (BASE 2 END)
	leau 	D,Y
	MPX9 	DBGFMT
	fcs	/\tMPX9 MPXRAM:$%Xx MPXBAS:$%Yx len:$%Dx HIGH@:$%Ux\n\r/
@NoDebug

	; initialize Kernel memory allocation.
	LEAX 	foo,PCR
	lbsr 	KernalAllocInit

	; debug and diag help.
    	tst     <verbose
    	beq 	@NoDebug
	MPX9 	DBGFMT
	fcs	/\tMPX9+ KAMemPtr:$%Xx\n\r/
@NoDebug

	; KAlloc memory for dynamic system call table **** MUST BE DONE BEFORE NON KAlloc INIT'S ****
	ldd 	#31		; room for 10 syscal id and offset vectors + 1 for marker
	MPX9 	KALLOC		; Allocate space for dynamic system call table
	stx 	<SYSOFF2.pHead 	; save pointer to dynamic system call table
	stx 	<SYSOFF2.pNext	; save pointer to next available entry in dynamic system call table
	clr 	,x 		; put null at end of dynamic system call table
	leax 	31,X 		; point to end of dynamic system call table (null marker)
	stx 	<SYSOFF2.pEnd	; save pointer to end of dynamic system call table

	; KAlloc memory for CmdLineData and store in pCmdLineData
	lbsr 	CmdLineInit
	lbsr 	CmdShellInit
	lbsr 	RptErrorInit
	lbsr 	ModulesInit
	ldx 	,S 		; get pointer to command line back.
	lda  	,X
	cmpa 	#CR
	beq 	_StartX		; No Modules to load
	MPX9 	ADDMOD		; todo: loop here while not CR, note modules must stop parsing args etc at a comma or semi-colon
	bne 	_StartXE
_StartX:
	clrb 	; No Errors
_StartXE:
	tstb	; No Errors
	PULS	pc,x

; **************************************************
; * SYSTEM CALL DISPATCHER - Lifted from mpx9      *
; **************************************************
SYSCAL
* CHECK FOR SYSTEM CALL IN MPX9+ STATIC OR DYNAMIC SYSTEM CALL TABLE
 LEAY SYSOFF,PCR POINT Y AT OFFSET TABLE
**************************************************
* SEARCH STATIC MPX9+ SYSTEM CALL TABLE
**************************************************
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

NotFound
**************************************************
* SEARCH DYNAMIC MPX9+ SYSTEM CALL TABLE
**************************************************
 LDY <SYSOFF2.pHead POINT Y AT OFFSET TABLE
 BEQ NotFound2 ; GO IF NO DYNAMIC SYSTEM CALL TABLE
RESCM3 TST ,Y END OF TABLE?
 BEQ NotFound2 GO IF YES
 CMPA ,Y+ FIND COMMAND?
 BEQ RESCM4 GO IF YES
 LEAY 2,Y ADVANCE POINTER
 BRA RESCM3 LOOP
RESCM4
 LDx ,Y GET address of ROUTINE
 STX 8,S
 PULS CC,A,B,DP,X,Y,PC TURN SWI CALL INTO JSR

NotFound2
**************************************************
* GO BACK TO PSYMON SYSTEM CALL DISPATCHER
**************************************************
 ldy 	<SCLVEC EXIT TO NEXT LEVEL ROUTINE
 jmp 	,y
 
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
 lDy 	<SCLVEC A=MPX9 SYSCAL #
 ; ldy 	,y
 jmp 	,y

; **************************************************
; * MPX9+ SYSTEM CALLS                             *
; **************************************************

FOURTY:
	clrb 
	rts 

****************************************************
* SYSTEM CALL 8 (MPX) - RETURN TO MPX/9            *
****************************************************
; MPXRET 
;  ; LEAS STACK,PCR RESET THE STACK
;  ; LDA #CR FORCE RELOAD OF LINE BUFFER
;  ; STA LINBUF,PCR
;  BRA ABC_RET

****************************************************
* SYSTEM CALL 43 (ADDSYSCALL) - AddDynSystemCall
****************************************************
* ENTRY REQUIREMENTS:  A new SYSCALL ID 
*                      X POINTS TO ROUTINE TO MAP TO SYSCAL
*
* EXIT CONDITIONS:  REGISTERS  UNCHANGED
*                   ROUTINE SHOULD RETURN ERROR
*                     CODE IN B, 0 IF NO ERROR
**************************************************
AddDynSystemCall

; pSYSOFF2	rmb	2 	; pointer to DYNAMIC SYSTEM CALL TABLE (DSCT).
; pSYSOFF2end 	rmb	2 	; size of pSYSOFF2end DYNAMIC SYSTEM CALL TABLE
; pSYSOFF2next	rmb	2 	; pointer to next entry in the DYNAMIC SYSTEM CALL TABLE
	pshs 	Y
	clrb 				; assume no error
	ldy 	<SYSOFF2.pNext		; null if no more space
	beq 	AddDynSystemCallXErr	; then exit with error
	sta 	,Y+			; store new SYSCALL ID 
	stx 	,Y++			; store 
	cmpy 	<SYSOFF2.pEnd 		; check if at end of allocated DSCT space
	blt 	@ok
	ldy 	#0 			; if so then clear Y
@ok	sty 	<SYSOFF2.pNext
	bra 	AddDynSystemCallX
AddDynSystemCallXErr
	ldb 	#18 			; should be ERR_RN (resource not available)
AddDynSystemCallX
	tstb 
	puls 	Y,PC 



; **************************************************
; * MPX9+ SYSTEM CALL OFFSET LOOKUP TABLE          *
; * STATIC SYSTEM CALL TABLE			   *
; **************************************************
SYSOFF:

 FCB MPX9LOADED
 FDB FOURTY-SYSOFF 	- IS MPX9+ LOADED?

 FCB DBGFMT
 FDB KernalDbgFmt-SYSOFF 	- GET A LINE OF INPUT

 FCB PROCMD
 FDB NPROCM-SYSOFF 	- New process command.

 ; FCB MPX		- RETURN TO MPX9+
 ; FDB MPXRET-SYSOFF 	- New process command.
  
 FCB KALLOC
 FDB KernalAlloc-SYSOFF 	- GET A HUNK OF KERNAL MEMORY

 FCB GETLIN
 FDB CmdLine-SYSOFF 	- GET A LINE OF INPUT

 FCB ADDSYSCALL
 FDB AddDynSystemCall-SYSOFF 	- add a system call to the DYNAMIC SYSTEM CALL TABLE

 FCB 0 END OF TABLE MARK

**************************************************

	endsection	; section	.text

**************************************************
** Constants.
**************************************************

 	section	.data

SYSVEC EQU 64 SYSTEM CALL VECTOR

	endsection	; section .data

**************************************************
** Uninitialized Working Variables.
**************************************************

;  	section .bss
; 	endsection	; section .bss

**************************************************
** Uninitialized Direct Page Working Variables.
**************************************************

 	section .dp

verbose	export

verbose	rmb	1
SCLVEC 	rmb	2	; SYSTEM CALL VECTOR TO MPX9 SYSTEM CALL DISPATCHER.

SYSOFF2 	SimpleList ; MPX9+ DYNAMIC SYSTEM CALL TABLE
; .pHead pointer to DYNAMIC SYSTEM CALL TABLE.
; .pNext pointer to next entry in the DYNAMIC SYSTEM CALL TABLE
; .pEnd pointer to last byte of the DYNAMIC SYSTEM CALL TABLE

	endsection	; section .dp

; PGMEND  equ *-1
; PGMSIZ  EQU PGMEND-BGNPGM

 END

ADD SYSCALL TO ADD NEW SYSCALL TO DYNAMIC SYSTEMCALL TABLE
ADD DYNAMIC ERROR TABLE.
