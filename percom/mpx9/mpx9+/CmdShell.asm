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
        clra 

; NOTE make this into a subroutine!

        std     <ResCmdList.pHead  ; NO DYNAMIC SYSTEM resident command TABLE
        std     <ResCmdList.pEnd   ; zero length of allocated SYSTEM resident command TABLE
        std     <ResCmdList.pNext  ; zero pointer to next entry in the SYSTEM resident command TABLE

        ; debug and diag help.
        tst     <verbose
        beq     @NoDebug
        MPX9    DBGFMT
        fcs     /**** CmdShellInit ****\n\r/
@NoDebug

        LDA     #$66
        LEAX    SYSCALL_66,PCR
        MPX9    ADDSYSCALL


     ; KAlloc memory for SYSTEM resident command TABLE **** MUST BE DONE BEFORE NON KAlloc INIT'S ****
        ldd     #41             ; room for 10 System Resident commands names and pointer to code + 1 for marker
        MPX9    KALLOC          ; alloc space for SYSTEM resident command TABLE
        stx     <ResCmdList.pHead  ; save pointer to SYSTEM resident command TABLE
        stx     <ResCmdList.pNext  ; save pointer to next available entry in SYSTEM resident command TABLE
        clr     ,x              ; put null at end of SYSTEM resident command TABLE
        leax    41,X            ; point to end of SYSTEM resident command TABLE (null marker)
        stx     <ResCmdList.pEnd   ; save pointer to end of SYSTEM resident command TABLE

        LDA     #ADDRESCMD
        LEAX    AddResCommand,PCR
        MPX9    ADDSYSCALL




        LDD     #"YY"
        LEAX    Cmd_yy,PCR
        MPX9    ADDRESCMD


        ; set/clr verbose flag
        LDD     #"VB"
        LEAX    Cmd_vb,PCR
        MPX9    ADDRESCMD
        

        rts

; ; ************************************************
; CmdShell        EXPORT
; CmdShell:
; 	clrb 		; no error
; 	rts		; Return to OS

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

        PSHS    A,X,Y,U,PC SAVE REGISTERS & LEAVE SLOT
        LEAU    CMDERR,PCR SET UP ERROR RETURN ON STACK
        STU     7,S
        CLRB    PRESET ERROR CODE
        MPX9    SKPSPC GET TO FIRST WORD IN LINE
        BEQ     PROCMX GO IF END OF LINE

        LDA     1,X GET 2nd CHAR IN LINE
        CMPA    #SP DELIMITER?
        BEQ     GoMpx9ProcCmd GO IF YES
        CMPA    #CR END OF LINE?
        BEQ     GoMpx9ProcCmd GO IF YES

        LDA     2,X GET 3rd CHAR IN LINE
        CMPA    #SP DELIMITER?
        BEQ     RESCMD GO IF YES
        CMPA    #CR END OF LINE?
        BNE     GoMpx9ProcCmd GO IF NOT  ----------------- JNS not sure...

**************************************************
* SEARCH static MPX9+ resident commands TABLE
**************************************************
RESCMD 
        ldd     ,X              ; get the two char command
        leay    ResidentCommands,pcr
Slookuploop:
        TST     ,y
        beq     SNotFound
        cmpd    ,y++
        BEQ     Found           ; GO IF YES
        leay    2,Y             ; STEP OVER THE ROUTINE OFFSET
        bra     Slookuploop

**************************************************
* SEARCH DYNAMIC MPX9+ resident commands TABLE
**************************************************
SNotFound
        LDY     <ResCmdList.pHead ; POINT Y AT OFFSET TABLE
        BEQ     GoMpx9ProcCmd   ; GO IF NO DYNAMIC SYSTEM CALL TABLE
Dlookuploop:
        TST     ,y
        beq     GoMpx9ProcCmd
        cmpd    ,y++
        BEQ     Found2           ; GO IF YES
        leay    2,Y             ; STEP OVER THE ROUTINE OFFSET
        bra     Dlookuploop



**************************************************
* resident command found, go do it.
**************************************************
Found2  LDD     ,Y              GET OFFSET TO ROUTINE
        STD     7,S
        bra     IJK             GO USE COMMON CODE.

**************************************************
* resident command found, go do it.
**************************************************
Found   LDD     ,Y              GET OFFSET TO ROUTINE
        LEAU    D,Y             PUT ROUTINE ADDRESS ON STACK
        STU     7,S
IJK     MPX9    GETWRD          GET THE NEXT WORD IN LINE
        STX     1,S             SAVE POINTER
PROCMX  TSTB                    SET Z FLAG FOR ERROR DETECTION
        PULS    A,X,Y,U,PC      RESTORE REGISTERS & EXIT
CMDERR  RTS                     GO FROM WHENCE YOU CAME

**************************************************
* resident command not found, go check with old mpx9 command dispatcher...
**************************************************
GoMpx9ProcCmd
        PULS    A,X,Y,U                 RESTORE REGISTERS
        leas    2,S                     drop created slot
        PSHS    CC,A,B,DP,X,Y,PC        MOCK SWI
        lda     #PROCMD
        lbra    MPX9SYSCAL

****************************************************
* SYSTEM CALL 45 (ADDRESCMD) - AddResCommand
****************************************************
* ENTRY REQUIREMENTS:  D new resident command name
*                      X -> command code
*
* EXIT CONDITIONS:  REGISTERS  UNCHANGED
*                   ROUTINE SHOULD RETURN ERROR
*                     CODE IN B, 0 IF NO ERROR
**************************************************
AddResCommand

; pSYSOFF2      rmb     2       ; pointer to DYNAMIC SYSTEM CALL TABLE (DSCT).
; pSYSOFF2end   rmb     2       ; size of pSYSOFF2end DYNAMIC SYSTEM CALL TABLE
; pSYSOFF2next  rmb     2       ; pointer to next entry in the DYNAMIC SYSTEM CALL TABLE
        pshs    b,Y
        clr     ,s                            ; assume no error
        ldy     <ResCmdList.pNext          ; null if no more space
        beq     ResCmdListXErr         ; then exit with error
        std     ,Y++                     ; store new SYSCALL ID 
        stx     ,Y++                    ; store 
        cmpy    <ResCmdList.pEnd           ; check if at end of allocated DSCT space
        blt     @ok
        ldy     #0                      ; if so then clear Y
@ok     sty     <ResCmdList.pNext
        bra     ResCmdListX
ResCmdListXErr
        ldb     #ERR_RN                 ; should be ERR_RN (resource not available)
        stb     ,s
ResCmdListX
        ldb     ,s
        puls    b,Y,PC 


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

        ldb     #ERR_RN

        ; MPX9 MPX
        
        rts

**************************************************
SYSCALL_66:

        MPX9  DBGFMT
        fcs   /SYSCALL $66\n\r/
        clrb
        rts


**************************************************
Cmd_yy:

        MPX9  DBGFMT
        fcs   /test YYYY\n\r/

        clrb 
        rts

**************************************************
Cmd_vb:
        MPX9  DBGFMT
        fcs   /Cmd_vb\n\r/

        clr     <verbose

        MPX9    DECNUM
        tsta
        bne     @true
        tstb
        beq     @false
@true
        inc     <verbose
@false

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

**************************************************
** Uninitialized Direct Page Working Variables.
**************************************************

        section .dp

ResCmdList         SimpleList ; MPX9+ DYNAMIC resident commands TABLE
; .pHead pointer to DYNAMIC SYSTEM resident commands TABLE.
; .pNext pointer to next entry in the DYNAMIC SYSTEM resident commands TABLE
; .pEnd pointer to last byte of the DYNAMIC SYSTEM resident commands TABLE

        endsection      ; section .dp

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
