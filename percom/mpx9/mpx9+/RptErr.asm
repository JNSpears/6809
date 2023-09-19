 PRAGMA cescapes
 nam RptError
 ttl New RptError processor for MPX9+
*********************************************************
* RptError                               JNS 9/7/2023   *
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
KAlloc          EXTERN

; ************************************************
RptErrorInit    EXPORT
RptErrorInit:
        clrb
        clra 

; NOTE make this into a subroutine!

        std     <ErrList.pHead  ; NO DYNAMIC SYSTEM ErrList TABLE
        std     <ErrList.pEnd   ; zero length of allocated DYNAMIC SYSTEM ErrList TABLE
        std     <ErrList.pNext  ; zero pointer to next entry in the DYNAMIC SYSTEM ErrList TABLE

; NOTE make this into a subroutine!

        ; debug and diag help.
        tst     <verbose
        beq     @NoDebug
        MPX9    DBGFMT
        fcs     /**** RptErrorInit ****\n\r/
@NoDebug


       

        ; KAlloc memory for dynamic system ErrList table **** MUST BE DONE BEFORE NON KAlloc INIT'S ****
        ldd     #31             ; room for 10 System Error ID's and 2 char Error text + 1 for marker
        MPX9    KALLOC          ; alloc space for dynamic system ErrList table
        stx     <ErrList.pHead  ; save pointer to dynamic system ErrList table
        stx     <ErrList.pNext  ; save pointer to next available entry in dynamic system ErrList table
        clr     ,x              ; put null at end of dynamic system ErrList table
        leax    31,X            ; point to end of dynamic system ErrList table (null marker)
        stx     <ErrList.pEnd   ; save pointer to end of dynamic system ErrList table

        LDA     #RPTERR
        LEAX    RptError,PCR
        MPX9    ADDSYSCALL

        LDA     #ADDRPTERR
        LEAX    AddRptError,PCR
        MPX9    ADDSYSCALL

        LDA     #ERR_RN
        ldx     #"RN"
        MPX9    ADDRPTERR

        rts

**************************************************
* Override for (RPTERR) - REPORT ERROR           *
*                                                *
* If error code is <= max mpx9 error code then   *
* call mpx9 to handle it. Otherwise handle it    *
* from the dynamic error table                   *
**************************************************
* SYSTEM CALL 13 (RPTERR) - REPORT ERROR         *
*                                                *
* ENTRY REQUIREMENTS:  B CONTAINS ERROR CODE     *
*                                                *
* EXIT CONDITIONS:  ALL REGISTERS PRESERVED      *
**************************************************
ERRLIM EQU 18
RptError
 cmpb #ERRLIM
 bgt NewRptError

 ; **************************************************
 ; Error is a mpx9 system error go to old command dispatcher...
 ; **************************************************
UseOldRptErr
 PSHS   CC,A,B,DP,X,Y,PC MOCK SWI
 lda    #RPTERR
 lbra    MPX9SYSCAL

NotFound
 PULS CC,A,B,X RESTORE SAVED REGISTERS
 LDB #ERRLIM FORCE LIMIT
 bra UseOldRptErr

 ; **************************************************
 ; Lifted from mpx9 with added changes...
 ; **************************************************
NewRptError 
 PSHS CC,A,B,X SAVE REGISTERS USED

 * SEARCH mpx9+ dynamic error table
 LDX <ErrList.pHead POINT X AT OFFSET TABLE
 BEQ NotFound ; GO IF NO DYNAMIC SYSTEM error TABLE
@Loop TST ,X END OF TABLE?
 BEQ NotFound GO IF YES
 CMPB ,X+ FIND COMMAND?
 BEQ @Found GO IF YES
 LEAY 2,X ADVANCE POINTER
 BRA @Loop
@Found

 pshs X
 LEAX <ERRHDR,PCR DISPLAY ERROR HEADER
 MPX9 PSTRNG
 ; DISPLAY ERROR 2 character name
 PULS X
 LDA ,X+ DISPLAY IT
 MPX9 OUTCHR
 LDA ,X
 MPX9 OUTCHR

 ; Add decimal value of error code to message.
 LEAX <ERRMID,PCR DISPLAY middle
 MPX9 PSTRNG
 
 ; add dec repr of error codes
 clra
 ldb    2,S     ; Get error code
 MPX9 DSPDEC
 
 LEAX <ERRTRL,PCR DISPLAY TRAILER
 MPX9 PSTRNG
 PULS CC,A,B,X,PC RESTORE REGISTERS & EXIT

ERRHDR 
 fcs '\r\n*** MPX/9 ERROR '
ERRMID
 fcs ' ('
ERRTRL
 fcs ') ***\r\n'


****************************************************
* SYSTEM CALL 43 (ADDRPTERR) - AddRptError
****************************************************
* ENTRY REQUIREMENTS:  A new SYSTEM ERROR ID 
*                      X contains 2 character ERROR LABLE
*
* EXIT CONDITIONS:  REGISTERS  UNCHANGED
*                   ROUTINE SHOULD RETURN ERROR
*                     CODE IN B, 0 IF NO ERROR
**************************************************
AddRptError

; pSYSOFF2      rmb     2       ; pointer to DYNAMIC SYSTEM CALL TABLE (DSCT).
; pSYSOFF2end   rmb     2       ; size of pSYSOFF2end DYNAMIC SYSTEM CALL TABLE
; pSYSOFF2next  rmb     2       ; pointer to next entry in the DYNAMIC SYSTEM CALL TABLE
        pshs    Y
        clrb                            ; assume no error
        ldy     <ErrList.pNext          ; null if no more space
        beq     AddRptErrorXErr         ; then exit with error
        sta     ,Y+                     ; store new SYSCALL ID 
        stx     ,Y++                    ; store 
        cmpy    <ErrList.pEnd           ; check if at end of allocated DSCT space
        blt     @ok
        ldy     #0                      ; if so then clear Y
@ok     sty     <ErrList.pNext
        bra     AddRptErrorX
AddRptErrorXErr
        ldb     #ERR_RN                 ; should be ERR_RN (resource not available)
AddRptErrorX
        tstb 
        puls    Y,PC 


**************************************************

        endsection      ; section .text

**************************************************
** Constants.
**************************************************

        ; section .data
        ; endsection      ; section .data

**************************************************
** Uninitialized Working Variables.
**************************************************

        ; section .bss
        ; endsection      ; section .bss

**************************************************
** Uninitialized Direct Page Working Variables.
**************************************************

        section .dp

ErrList         SimpleList ; MPX9+ DYNAMIC SYSTEM error code TABLE
; .pHead pointer to DYNAMIC SYSTEM error code TABLE.
; .pNext pointer to next entry in the DYNAMIC SYSTEM error code TABLE
; .pEnd pointer to last byte of the DYNAMIC SYSTEM error code TABLE

        endsection      ; section .dp

 END
