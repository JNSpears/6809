 nam LoadHi
*********************************************************
* nCmdLine.CM                           JNS 7/22/2023   *
*                                                       *
*                                                       *
*********************************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i

**************************************************
* Program (Position independant)
**************************************************
        ORG     $0
BGNPGM	EQU		*
; *****************************************************
Main:
	clr     >verbose,pcr    ; initialize variables

    
option:
	MPX9	SKPSPC          ; point to the next word
	lbeq    Init  			; No arguments
	lda     ,x
	cmpa    #'/             ; look for option flags
	lbne    Init       		; Not a switch

	leax 	1,X             ; STEP OVER SWITCH CHAR
	lda     ,x+             ; get option char and bump pointer

option_V:
	cmpa    #'V             ; is a option 'V'?
	bne     Option_A        ; bad switch
	com     >verbose,pcr    ; toggle option 'V'
	bra     option          ; get next option

Option_A:
	cmpa    #'A             ; is a option 'S'?
	bne     synerr          ; bad switch

	tst  	>verbose,pcr
	beq  	OPTIONA1
	pshs 	x,D
	lda 	#$80
	sta     16,X
	MPX9	PSTRNG
	puls  	x,D
OPTIONA1

	MPX9 	HEXNUM
	std 	addr,pcr

	tst  	>verbose,pcr
	beq  	OPTIONA2
	pshs 	D,x
	leax    <TxAddr,PCR
	MPX9	PSTRNG
	puls  	x,D
	MPX9	DSPDBY
OPTIONA2:
	bra     option          ; get next option


TxAddr: FCB CR,LF
 FCS 'Addr:'

synerr:
	ldb     #ERR_SN         ; Error Syntax
	rts
	
; *****************************************************
TxFile: FCB CR,LF
 FCS 'Filename:'
 ; FCB CR,LF+$80
TxDcb: FCB CR,LF,CR,LF
 FCS 'DCB:'
 ; FCB CR,LF+$80
TxFcb: FCB CR,LF,CR,LF
 FCS 'FCB:'
 ; FCB CR,LF+$80
TxDir: FCB CR,LF,CR,LF
 FCS 'DIR:'
 ; FCB CR,LF+$80
TxDmy: FCB CR,LF,CR,LF
 FCS 'Dmy:'
 ; FCB CR,LF+$80
TxErr: FCB CR,LF,CR,LF
 FCS '*** Err:'
 ; FCB CR,LF+$80

Init:
    tst     verbose,pcr

; START INIT

; END INIT

* CHECK FOR DISK RESIDENT COMMAND
DSKCMD:
 ; USIM
 PSHS PC,X
 LEAY MYFCB,PCR


	; 		pull  -->  CC, A, B, DP, X, Y, U/S, PC  <--  push 

	tst  	>verbose,pcr
	beq  	DBG1
	pshs 	d,x,y
	leax 	TxFile,pcr
	MPX9	PSTRNG
	puls 	y,x,d
	pshs 	d,x,y
	ldd  	#12   ; SET SIZE IN B 
	JSR  	[DumpMem2v]	; dump buffer DCB data.
	puls 	y,x,d
DBG1:

 MPX9 INTFCB INITIALIZE FCB
 LBNE PROCMXE GO IF ERROR
 STX ,S SAVE LINE POINTER (X in called program)
 LDD FCBSUF,Y GET FILE SUFFIX
 BNE DSKCM1 GO IF NOT NULL
 LDD #$434D FORCE SUFFIX OF 'CM'
 STD FCBSUF,Y
DSKCM1:


	tst  	>verbose,pcr
	beq  	DBG2
  	pshs 	d,x,y
	pshs 	d,x,y
	leax 	TxFcb,pcr
	MPX9	PSTRNG
	puls 	y,x,d

	tfr 	y,x
	ldd 	#$20   ; SET SIZE IN B 
	JSR  	[DumpMem2v]	; dump buffer DCB data.

	puls 	y,x,d
DBG2:
 MPX9 LOCFIL LOOK UP THE FILE
 LBNE PROCMXE GO IF ERROR IN LOOKUP

	tst  	>verbose,pcr
	beq  	DBG3
	pshs 	d,x,y

	pshs 	d,x,y
	leax 	TxDir,pcr
	MPX9	PSTRNG
	puls 	y,x,d

	ldd 	#$10   ; SET SIZE IN B 
	JSR  	[DumpMem2v]	; dump buffer DCB data.

	puls 	y,x,d
DBG3:
 LDD DIRSTR,X GET STARTING BLOCK #
 STD <2,S temporaraly save in PC slot on stack ???????????????????????????
 LDU DIREXT,X SAVE EXECUTION ADDRESS

 LDA FCBDRN,Y GET DRIVE #
 MPX9 GETDCB
 bne PROCMXE

 LDD <2,S get saved from PC slot on stack
 STD DCBBLK,X PLUG BLOCK #
 LDA FCBDRN,Y GET DRIVE #
 STA DCBDRV,X
 ; LDD #$FFFF SET ADDRESS TO NULL
 ldd addr,pcr
 STD DCBBUF,X

	tst  	>verbose,pcr
	beq  	DBG4
	pshs 	d,x,y

	pshs 	d,x,y
	leax 	TxDcb,pcr
	MPX9	PSTRNG
	puls 	y,x,d
	ldd  	#$20   ; SET SIZE IN B 
	JSR  	[DumpMem2v]	; dump buffer DCB data.

	pshs 	d,x,y
	leax 	TxFcb,pcr
	MPX9	PSTRNG
	puls 	y,x,d
	tfr 	y,x
	ldd 	#$20   ; SET SIZE IN B 
	JSR  	[DumpMem2v]	; dump buffer DCB data.

	puls 	y,x,d
DBG4:
 MPX9 MEMLOD LOAD THE PROGRAM ; X -> DCB
 BNE PROCMXE GO IF ERROR IN LOAD

	tst  	>verbose,pcr
	beq  	DBG5
	pshs 	d,x,y
	leax 	TxDmy,pcr
	MPX9	PSTRNG
	puls 	y,x,d

	USIM
DBG5:

 CMPU #$FFFF EXECUTION NULL?
 BEQ PROCMX EXIT IF YES
 LDU addr,pcr
 STU 2,S SET UP EXECUTION ADDRESS ------------- need to adjust by load addr
 SPC 1
* EXECUTE THE COMMAND
PROCMX TSTB SET Z FLAG FOR ERROR DETECTION
 PULS X,PC RESTORE REGISTERS & EXIT

PROCMXE:
	tst  	>verbose,pcr
	beq  	DBG6
	pshs 	d,x,y

	pshs 	d,x,y
	leax 	TxErr,pcr
	MPX9	PSTRNG
	leax 	TxDcb,pcr
	MPX9	PSTRNG
	puls 	y,x,d
	ldd  	#$20   ; SET SIZE IN B 
	JSR  	[DumpMem2v]	; dump buffer DCB data.

	pshs 	d,x,y
	leax 	TxFcb,pcr
	MPX9	PSTRNG
	puls 	y,x,d
	tfr 	y,x
	ldd 	#$20   ; SET SIZE IN B 
	JSR  	[DumpMem2v]	; dump buffer DCB data.

	puls 	y,x,d
DBG6:

 TSTB SET Z FLAG FOR ERROR DETECTION
 LEAS 4,S
 RTS
* DUMMY ROUTINE FOR ERROR RECOVERY
CMDERR RTS GO FROM WHENCE YOU CAME
 SPC 1

 ; LOADHI /V /A2222 LISTDCB.CM
 ; LOADHI /V /A3800 LISTDCB.CM
 ; LOADHI /V /A4000 LISTDCB.CM
 ; LOADHI /V /A6200 LISTDCB.CM
 ; LOADHI /V /A2222 HELLOWOR.CM
 ; LOADHI.CM /V /A3F00 HELLOWOR.CM
 ; LOADHI.CM /V /A4000 HELLOWOR.CM
 ; LOADHI.CM /V /A6100 HELLOWOR.CM

	; clrb 		; no error
	; rts			; Return to OS

**************************************************
* qwerty - GET INPUT CHAR WITH NO ECHO
*
* Entry: None
* 
* Exit: A - char with Parity removed
*		 ALL other regs perserved, except C
* 
**************************************************


; *****************************************************

endcod  equ *-1

*
** Constants.
*

prompt FCB CR,LF
 fcs '> '

*
** Uninitialiazed Working Variables.
*

; XX: FCB 0,1,2,3,4,5,6,7
; 	FCB 0,1,2,3,4,5,6,7
; 	FCB 0,1,2,3,4,5,6,7
; 	FCB 0,1,2,3,4,5,6,7
; 	FCB 0,1,2,3,4,5,6,7
; 	FCB 0,1,2,3,4,5,6,7
; 	FCB 0,1,2,3,4,5,6,7
; 	FCB 0,1,2,3,4,5,6,7
; 	FCB 0,1,2,3,4,5,6,7

verbose	rmb	1
addr 	rmw 1 address to load at.
MYFCB	rmb 32

; VAR	STRUCT
; 	ENDSTRUCT

;  	ALIGN 16
; data VAR

PGMEND  equ *-1
PGMSIZ  EQU PGMEND-BGNPGM

 END
