; **********************************************
; hexdump.asm
;
; **********************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i

        INCLUDE ascii.i

**************************************************
* Program (Position independant)
**************************************************
        ORG $1000

begcod  equ *

**************************************************
* Main ENTRY POINT
* Entry:
*       X -> comamnd line arguments
**************************************************
HexDump:
        sts     >stack,pcr       ;save the stack for error recovery
        clr     pageflg,pcr
        clr     fileflg,pcr
        clr     memflg,pcr

        ldy     RAMv
        leay    CIDCB-RAM,y
        sty     >_cidcb,pcr
        
        MPX9    SKPSPC
        beq     synerr
        bra     opti00a

option  MPX9    SKPSPC  ; point to the next word
        beq     doit    ; if end of line go...
opti00a        
        cmpa    #'/     ; look for option flags
        bne     doit
        leax    1,X
        lda     ,x+     ; get option chanr and bump pointer

        cmpa    #'F     ; is a option 'F?
        bne     opti00  ; skip if not
        com     >fileflg,pcr ; toggle option 'F'
        bra     option  ; get next option

opti00  cmpa    #'M    ; is a option 'M'?
        bne     opti01  ; skip if not
        com     >memflg,pcr ; toggle option 'M'
        bra     option  ; get next option

opti01  cmpa    #'P     ; is a option 'P'?
        bne     opti02  ; skip if not
        com     >pageflg,pcr ; toggle option 'P'
        bra     option  ; get next option
opti02:
; opti02  
;         cmpa    #'P     ; is a option 'P'?
;         bne     opti03     ; skip if not
;         com     >polflg,pcr ; toggle option 'p'
;         bra     option  ; get next option
        
; opti03  
;         cmpa    #'R     ; is a option 'R'?
;         lbne    synerr  ; syntax error if not

;         lda     ,x+             ; get next char
;         cmpa    #':             ; should be an ':'
;         lbne    synerr          ; syntax error if not

;         swi3                    ; convert record size value
;         fcb     DECNUM    

;         stb     >recflg,pcr     ; save record size value

        bra     option  ; get next option


doit:
        tst     >fileflg,pcr
        bne     FileDump
        tst     >memflg,pcr
        bne     MemDump
        ldb     #ERR_SN
HexDumpX:
        lds     >stack,pcr
        tstb
        rts

synerr  ldb     #ERR_SN
dskerr  bra     HexDumpX

**************************************************
MemDump:
        MPX9    HEXNUM          ; GET START ADDRESS
        BNE     MemDumpx        ; GO IF ERROR
        PSHS    D               ; SAVE START
        MPX9    HEXNUM          ; GET END ADDRESS
        BNE     MemDumpx        ; GO IF ERROR
        TSTA
        bne     MemDump1
        TSTB
        bne     MemDump1
        ldb     #$10            ; default length
MemDump1:    
        PULS    X
        JSR     [DumpMem2v]
MemDumpx:
        bra     HexDumpX

        

**************************************************
FileDump:        
        leay    infcb,pcr ; point to the fcb
        MPX9    INTFCB          ; initialize the fcb
        bne     dskerr          ; report disk error
        leax    inbufr,pcr      ; point to file buffer
        lda     #ReadFn         ; setup for read
        MPX9    OPNFIL          ; open the file
        bne     dskerr          ; report disk error

        ldd     #0
        std     <offset,pcr

readsector:
        leay    <infcb,pcr      ; point to fcb
        MPX9    RDBLK           ; read the next sector
        cmpb    #ERR_EF
        beq     FileDumpX       ; EOF then done.
        
        tstb
        bne    dskerr           ; report disk error
        
        leax    inbufr,pcr
        ldb     #16
        stb     ,-s
paraloop:
        ldd     offset,pcr
        jsr     [Line16Dump]


        * CHECK FOR CRTL-C
        pshs    X
        ldx     [>_cidcb,pcr]   ; get input dcb pointer
        ldb     #StatFn         ; check port status
        MPX9    REQIO
        bita    #1
        beq     NoBRK
        ldb     #ReadFn
        MPX9    REQIO
        cmpa    #BRK
        beq     FileDumpBrk
NoBRK:  puls    X

        ldd     <offset,pcr
        addd    #16
        std     <offset,pcr
        dec     ,s
        bne     paraloop

        * Paging...
        tst     >pageflg,pcr
        beq     noPaging
        jsr     [InChrv]
        cmpa    #BRK
        beq     FileDumpBrk
noPaging:      

        * Move to next sector...
        leay    <infcb,pcr      ; point to fcb
        LDX     FCBNXT,Y        ; GET THE NEXT BLOCK
        STX     FCBCUR,Y
        BEQ     FileDumpX       ; GO IF END OF FILE

        leas    1,s             ; clean up stack
        bra     readsector      ; get next sector

brksmg: fcb     CR,LF
        FCS     /<Ctrl-C>/
FileDumpBrk:
        leax    brksmg,pcr
        MPX9    PSTRNG
        clrb
FileDumpX:
        Lbra    HexDumpX

endcod  equ *
sizcod  equ endcod-begcod
; frecod  equ $400-sizcod
;         if sizcod&~$3ff
;             ERROR image must fit in 1k ROM
;         endc
*
** Working Variables
*

_cidcb  rmb     2

stack   rmb     2

pageflg rmb     1
fileflg rmb     1
memflg  rmb     1
offset  rmw     1

infcb   rmb     32
inbufr  rmb     256

endpgm  equ     *-1


* HEXDUMP /F /P LIST.AS
* HEXDUMP /F LIST.AS

