 PRAGMA cescapes
 nam CmdLine
 ttl New Comandline processor for MPX9+
*********************************************************
* CmdLine                               JNS 7/22/2023   *
*                                                       *
*                                                       *
*********************************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i
        INCLUDE mpx9+.i

        section .text

**************************************************
* Program (Position independant)
**************************************************

MPX9SYSCAL      EXTERN

; ************************************************
CmdShellInit    EXPORT
CmdShellInit:
        clrb

        ; USIM

        LDA     #$66
        LEAX    SYSCALL_66,PCR
        MPX9    ADDSYSCALL

        rts

; ************************************************
CmdShell        EXPORT
CmdShell:
	clrb 		; no error
	rts		; Return to OS

**************************************************
* Override for (PROCMD) - PROCESS COMMAND LINE   *
*                                                *
* If no mathing resident 2 letter command, then  *
* call mpx9 to handle 1 letter commands, and     *
* disk resident commands 
**************************************************
* SYSTEM CALL 12 (PROCMD) - PROCESS COMMAND LINE *
*                                                *
* ENTRY REQUIREMENTS:  X POINTS TO COMMAND LINE  *
*                                                *
* EXIT CONDITIONS:  EXITS TO SELECTED ROUTINE    *
*                   X POINTS TO PARAMETERS IN    *
*                     COMMAND LINE               *
*                   A,Y,U PRESERVED FOR PARAMS   *
*                   ROUTINE SHOULD RETURN ERROR  *
*                     CODE IN B, 0 IF NO ERROR   *
**************************************************
NPROCM          EXPORT
NPROCM 

 ; PSHS X,D
 ; TFR X,D
 ; MPX9 DSPDBY
 ; PULS X,D

 PSHS A,X,Y,U,PC SAVE REGISTERS & LEAVE SLOT
 LEAU CMDERR,PCR SET UP ERROR RETURN ON STACK
 STU 7,S
 CLRB PRESET ERROR CODE
 MPX9 SKPSPC GET TO FIRST WORD IN LINE
 BEQ PROCMX GO IF END OF LINE

 LDA 1,X GET 2nd CHAR IN LINE
 CMPA #SP DELIMITER?
 BEQ GoMpx9ProcCmd GO IF YES
 CMPA #CR END OF LINE?
 BEQ GoMpx9ProcCmd GO IF YES

 LDA 2,X GET 3rd CHAR IN LINE
 CMPA #SP DELIMITER?
 BEQ RESCMD GO IF YES
 CMPA #CR END OF LINE?
 BNE GoMpx9ProcCmd GO IF NOT  ----------------- JNS not sure...
 SPC 1

* CHECK FOR RESIDENT COMMAND
RESCMD 
        ldd     ,X      get the two char command
        leay    ResidentCommands,pcr
lookuploop:
        TST     ,y
        beq     GoMpx9ProcCmd
        cmpd    ,y++
        BEQ     RESCM2 GO IF YES
        leay    2,Y     ; STEP OVER THE ROUTINE OFFSET
        bra     lookuploop

RESCM2 LDD ,Y GET OFFSET TO ROUTINE
 LEAU D,Y PUT ROUTINE ADDRESS ON STACK
 STU 7,S
 MPX9 GETWRD GET THE NEXT WORD IN LINE
 STX 1,S SAVE POINTER
PROCMX TSTB SET Z FLAG FOR ERROR DETECTION
 PULS A,X,Y,U,PC RESTORE REGISTERS & EXIT
CMDERR RTS GO FROM WHENCE YOU CAME

; **************************************************
GoMpx9ProcCmd: ; command not found in new table, go to old command dispatcher...
 PULS   A,X,Y,U   RESTORE REGISTERS
 leas   2,S       drop created slot
 PSHS   CC,A,B,DP,X,Y,PC MOCK SWI
 lda    #PROCMD
 lbra    MPX9SYSCAL

; **************************************************
; lookup:
;         leax 	ResidentCommands,pcr
; lookuploop:
;         TST 	,X
;         beq 	NOTFOUND
;         cmpd 	,X++
;         bne 	NEXT
; FOUND:  LDD     ,X 	GET OFFSET TO ROUTINE
;         LEAY 	D,X 	CALC ROUTINE ADDRESS
;         puls 	X
;         jmp 	,Y 	JUMP TO resident command HANDLER
; NEXT:
;         leax 	2,X 	; STEP OVER THE ROUTINE OFFSET
;         bra    lookuploop
; NOTFOUND:
;         puls 	X
; lookupX:
;         rts


ResidentCommands:
        ; Two letter command, and two byte offset to routine.

        ; XX Command
        FCC /XX/
        FDB Cmd_xx-*

        FCB 0 END OF TABLE MARK

**************************************************
Cmd_xx:
        MPX9  DBGFMT
        fcs   /test XX\n\r/

        clrb 

        MPX9 $66

        MPX9 MPX
        
        rts

**************************************************
SYSCALL_66:

        MPX9  DBGFMT
        fcs   /SYSCALL $66\n\r/

        clrb 

        rts

**************************************************

        endsection      ; section .text

*
** Constants.
*

        ; section .data

        ; endsection      ; section .data

*
** Uninitialiazed Working Variables.
*

        ; section .bss

        ; endsection      ; section .bss

 END

**************************************************
* OutEscBrkNChr - Output Vt100 escape sequ.
*
* Entry: A - Final char in Esc sequence
*        B - count (if 0 then no count in Esc sequence)
*                       only works for 0-99
* 
* Exit:  A - ACIA STATUS
*        ALL other regs perserved, except C
* 
**************************************************
