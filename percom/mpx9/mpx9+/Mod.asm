 PRAGMA cescapes
 nam Modules
 ttl Modules for MPX9+
*********************************************************
* Modules                              JNS  9/12/2023   *
*                                                       *
*                                                       *
*********************************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i
        INCLUDE mpx9+.i

        section .text

LOCALS  STRUCT
LoadAddr        rmw 1 address to load at.
CmdLineArgs     rmw 1 saved pointer to command line arguments.
pModHdr         rmw 1
ErrStatus       rmb 1 
FCB             rmb 32
Buffer          rmb 256
        ENDSTRUCT

MODHDR  STRUCT  ; Module Header
SIG_LA          rmw 1   ; TWO BYTE SIGNATURE FOR MODULE, AND SAFE TO EXECUTE AS A PROGRAM.
Entry           rmw 1   ; Entry point
CodeSize        rmw 1   ; code size (copied from disk)
DataSize        rmw 1   ; data size (allocated un initialiazed)
ModName         rmb 8   ; Module Name
        ENDSTRUCT

**************************************************
* Program (Position independant)
**************************************************

MPX9SYSCAL      EXTERN
KAlloc          EXTERN

; ************************************************
ModulesInit     EXPORT
ModulesInit:
        clrb
        clra 

; NOTE make this into a subroutine!

        std     <ModList.pHead  ; NO DYNAMIC SYSTEM ModList TABLE
        std     <ModList.pEnd   ; zero length of allocated DYNAMIC SYSTEM ModList TABLE
        std     <ModList.pNext  ; zero pointer to next entry in the DYNAMIC SYSTEM ModList TABLE

; NOTE make this into a subroutine!

        IFDEF DEBUG
        ; debug and diag help.
        tst     <verbose
        beq     @NoDebug
        MPX9    DBGFMT
        fcs     /**** ModulesInit ****\n\r/
@NoDebug
        ENDC

        ; KAlloc memory for dynamic system ModList table **** MUST BE DONE BEFORE NON KAlloc INIT'S ****
        ldd     #11             ; room for 10 System Error ID's and 2 char Error text + 1 for marker
        MPX9    KALLOC          ; alloc space for dynamic system ModList table
        stx     <ModList.pHead  ; save pointer to dynamic system ModList table
        stx     <ModList.pNext  ; save pointer to next available entry in dynamic system ModList table
        clr     ,x              ; put null at end of dynamic system ModList table
        leax    11,X            ; point to end of dynamic system ModList table (null marker)
        stx     <ModList.pEnd   ; save pointer to end of dynamic system ModList table

        LDA     #ADDMOD
        LEAX    AddModule,PCR
        MPX9    ADDSYSCALL

        ; Link in the IMPORT MODULE command to the COMMAND SHELL.
        LDD     #"IM"
        LEAX    Cmd_IM,PCR
        MPX9    ADDRESCMD

        ; Link in the LIST IMPORTED MODULES command to the COMMAND SHELL.
        LDD     #"LM"
        LEAX    Cmd_LM,PCR
        MPX9    ADDRESCMD

        ; Add error ERR_NM (Not Module)
        LDA     #ERR_NM         
        ldx     #"NM"
        MPX9    ADDRPTERR

        rts

**************************************************
* SYSTEM CALL 13 (ADDMOD) - AddModule            *
*                                                *
* ENTRY REQUIREMENTS:  X -> module name to load. *
*                                                *
* EXIT CONDITIONS:  ALL REGISTERS PRESERVED      *
*                   ROUTINE SHOULD RETURN ERROR
*                     CODE IN B, 0 IF NO ERROR
**************************************************
AddModule:

        ; PULL  -->  CC, A, B, DP, X, Y, U/S, PC  <--  PUSH 

        ; MPX9    DBGFMT
        ; fcs     /AddModule\n\r/

        clrb            ;       ;
        leas    -sizeof{LOCALS},S ; Allocate space on stack for temp variabes
        clr     LOCALS.ErrStatus,S
        stx     LOCALS.CmdLineArgs,S

        ; CHECK TO SEE IF MODULE EXISTS ON DISK.

        LEAY    LOCALS.FCB,S

        IFDEF DEBUG
        tst     <verbose
        beq     @NoDebug
        pshs    d,x
        leax    TxFile,pcr
        MPX9    PSTRNG  ; CC,A,X changed
        puls    x,d
        pshs    d,x
        ldd     #12   ; SET SIZE IN B 
        JSR     [DumpMem2v]     ; X & D Changed ; dump command line arguments 
        JSR     CRLF
        puls    x,d
@NoDebug:
        ENDC

        MPX9    INTFCB          INITIALIZE FCB
        LBNE    AddModuleXE     GO IF ERROR
        stx     LOCALS.CmdLineArgs,S SAVE updated command LINE POINTER (X in called program)
        LDD     FCBSUF,Y        GET FILE SUFFIX
        BNE     DSKCM1          GO IF NOT NULL
        LDD     #"MD"           FORCE SUFFIX OF 'MD'
        STD     FCBSUF,Y
DSKCM1:

        MPX9    LOCFIL LOOK UP THE FILE (Y->FCB --- X->DIR ENT, B=ERR, CC)
        LBNE    AddModuleXE GO IF ERROR IN LOOKUP

        tst     <verbose
        beq     @NoDebug
        pshs    d,x
        pshs    d,x
        leax    TxDir,pcr
        MPX9    PSTRNG  ; CC,A,X changed
        puls    x,d
        ldd     #$10   ; SET SIZE IN B 
        JSR     [DumpMem2v]     ; X & D Changed ; dump buffer DCB data.
        JSR     CRLF
        puls    x,d
@NoDebug:

        tfr     X,U     ; U --> DIRECTORY ENTRY
                        ; Y --> FCB

        lda     #ReadFn         ; setup for read
        leax    LOCALS.Buffer,S
        MPX9    OPNFIL          ; OPEN FILE (A=USAGE Y->FCB, X->BUFFER --- B=ERR, CC)

        tst     <verbose
        beq     @NoDebug
        pshs    d,x
        pshs    d,x
        leax    TxBuf,pcr
        MPX9    PSTRNG          ; CC,A,X changed
        puls    x,d
        ldd     #$30            ; SET SIZE IN B 
        JSR     [DumpMem2v]     ; X & D Changed ; dump buffer DCB data.
        JSR     CRLF
        puls    x,d
@NoDebug:

        leax    LOCALS.Buffer,S
        ldd     ,X                      ; GET SIG_LA WORD
        cmpd    #$1239                  ; NOP,RTS
        beq     SIGOK 
        ldb     ERR_NM                  ; NOT A MODULE
        lbra    AddModuleXE
SIGOK

        tst     <verbose
        beq     @NoDebug
        MPX9    DBGFMT
        fcs     /\tSIG OK\n\r/
@NoDebug:

        
        pshs    U
        ldy     <ModList.pHead          ; null if no more space
        beq     ModNotFound             ; then exit with error
LOOP1:  cmpy    <ModList.pNext          ; check if at end of allocated space
        beq     ModNotFound

        leax    LOCALS.Buffer,S         ; X --> LOCALS.Buffer,S
        leax    MODHDR.ModName,X        ; X --> MODHDR.ModName in Buffer

        ldu     ,Y++                    ; U --> Module record in ModList
        leau    MODHDR.ModName,U        ; U --> MODHDR.ModName in ModList
LOOP2       
        lda     ,U+
        cmpa    ,X+
        bne     LOOP1                   ; DID NOT MATCH, GO CHECK NEXT...

        tsta                            ; CHECK IS IT THE END OF STRING?
        BPL     LOOP2                   ; IF NOT END OF STRING, GO CHECK NEXT...

        ; SO WE HAVE A MATCH, THIS IS BAD...
        ldb     #ERR_RN                 ; should be ERR_RN (resource not available)
        puls    U
        Lbra    AddModuleXE


ModNotFound
        puls    U

        tst     <verbose
        beq     @NoDebug
        MPX9    DBGFMT
        fcs     /\tModNotFound OK\n\r/
@NoDebug:

        ; KAlloc memory for Module header
        ldd     #sizeof{MODHDR}
        MPX9    KALLOC                  ; alloc space for Module header

        ; ADD MODULE HEADER TO LIST OF MODULES.
        ldy     <ModList.pNext          ; null if no more space
        beq     AddRptErrorXErr         ; then exit with error
        stx     ,Y++                    ; store
        stx     LOCALS.pModHdr,s
        clr     ,Y
        cmpy    <ModList.pEnd           ; check if at end of allocated space
        blt     @ok
        ldy     #0                      ; if so then clear Y
@ok     sty     <ModList.pNext
        bra     @Next
AddRptErrorXErr
        ldb     #ERR_RN                 ; should be ERR_RN (resource not available)
        Lbra    AddModuleXE
@Next

        ldd     #sizeof{MODHDR}
        ; X -> allocated Module header to save header in
        tfr     X,Y
        leax    LOCALS.Buffer,S
        MPX9    BLKMOV          ; (X->SRC, Y->DST, D=CNT --- CC)

        tst     <verbose
        beq     @NoDebug
        MPX9    DBGFMT
        FCS     /ModHdr:\r\n/
        ldd     #sizeof{MODHDR}
        ldx     LOCALS.pModHdr,s
        JSR     [DumpMem2v]     ; X & D Changed ; dump buffer DCB data.
        JSR     CRLF
@NoDebug:

        LEAY    LOCALS.FCB,S
        MPX9    CLSFIL          ; (X->FCB, Y->DST, D=CNT --- X->FCB, B=ERR, CC)
        LBNE    AddModuleXE     GO IF ERROR

        ; U --> DIRECTORY ENTRY
        ; X --> Allocated Module header
        ; y --> FCB

        ; Figure out how much memory we need and allocate it.
        ldx     LOCALS.pModHdr,S  
        ldd     MODHDR.CodeSize,X
        addd    MODHDR.DataSize,X
        MPX9    KALLOC          ; alloc space for Module code+data
        stx     LOCALS.LoadAddr,S

        ldy     LOCALS.pModHdr,S  
        stx     MODHDR.SIG_LA,Y

        ; X --> Allocated SPACE FOR MODULE CODE & DATA
        leay    LOCALS.FCB,S
        LDA     FCBDRN,Y        ; GET DRIVE #
        MPX9    GETDCB          ; X --> DCB ; Get DCB for drive in filename. CHANGES X,A,B & CC
        lbne    AddModuleXE     

        ; U --> DIRECTORY ENTRY
        ; X --> DCB
        ; y --> FCB

        LDD     DIRSTR,U        ; GET STARTING BLOCK #
        STD     DCBBLK,X        ; PLUG BLOCK #
        LDA     FCBDRN,Y        GET DRIVE #
        STA     DCBDRV,X
        ldd     LOCALS.LoadAddr,S
        STD     DCBBUF,X

        tst     <verbose
        beq     @NoDebug
        MPX9    DBGFMT
        FCS     /ModHdr:\r\n/
        ldd     #sizeof{MODHDR}
        pshs    X
        ldx     LOCALS.pModHdr+2,s
        JSR     [DumpMem2v]     ; X & D Changed ; dump buffer DCB data.
        JSR     CRLF
        puls    X
@NoDebug:

        tst     <verbose
        lbeq    @NoDebug
        ;;
        ; PULL  -->  CC, A, B, DP, X, Y, U/S, PC  <--  PUSH 
        ;;
        ; pshs    d,x
        pshs    d,x
        leax    TxDcb,pcr
        MPX9    PSTRNG  ; CC,A,X changed
; DCBLNK EQU 0 POINTER TO NEXT DCB
; DCBDID EQU 2 ASCII ID ('DK' FOR DISK)
; DCBDVR EQU 4 DEVICE DRIVER ADDRESS
; DCBIOA EQU 6 DEVICE I/O ADDRESS
; DCBERR EQU 8 ERROR STATUS CODE
; DCBEXT EQU 9 DCB EXTENSION BYTE COUNT
; * PART 2 - DEVICE DEPENDENT DATA
; DCBDRV EQU 10 DISK DRIVE # (1-4)
; DCBBLK EQU 11 DISK BLOCK #
; DCBBUF EQU 13 BUFFER ADDRESS
; * PART 3 - DRIVER WORK SPACE
; DCBTRK EQU 15 TRACK #
; DCBSEC EQU 16 SECTOR #
; DCBCUR EQU 15 CURRENT TRACK/SECTOR
; DCBPRV EQU 17 PREVIOUS BLOCK #
; DCBNXT EQU 19 NEXT BLOCK #
; DCBCNT EQU 21 BYTE COUNT IN THIS BLOCK
; DCBADD EQU 22 DATA ADDRESS FOR THIS BLOCK
; DCBTYP EQU 24 BLOCK TYPE CODE
; DCBCRC EQU 25 DATA CRC
; "       Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr"
; F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 00 9C E1 00  ..DK............
; F030 : 04 00 01 00 00 00 B0 00  00 C1 62 00 00 00 00 00  ..........b.....
; "       sc prev_ next_ ct data@  ty crc"
; 
;
; NOTE MAKE THIS A LIB FUNCTION.
        MPX9    DBGFMT
        FCS     /\r\n       Nxt__ ID___ Drv@_ I\/O@_  Er ex Dr blk__ buff@ tr/
        ldd     #$20   ; SET SIZE IN B
        ldx     2,S 
        JSR     [DumpMem2v]     ; X & D Changed ; dump buffer DCB data.
        JSR     CRLF
        MPX9    DBGFMT
        FCS     /       sc prev_ next_ ct data@  ty crc/
        leax    TxFcb,pcr
        MPX9    PSTRNG  ; CC,A,X changed
        tfr     y,x
        ldd     #$20   ; SET SIZE IN B 
        JSR     [DumpMem2v]     ; X & D Changed ; dump buffer DCB data.
        JSR     CRLF
        puls    x,d
@NoDebug:

        tst     <verbose
        beq     @NoDebug
        MPX9    DBGFMT
        fcs     /\tLOADING\n\r/
        pshs    x,d
        ldd     #$80
        ldx     LOCALS.LoadAddr+4,S
        JSR     [DumpMem2v]     ; X & D Changed ; dump buffer DCB data.
        JSR     CRLF  
        puls    x,d
@NoDebug:

        MPX9    MEMLOD          ; LOAD THE PROGRAM to allocated memory; X -> DCB
        BNE     AddModuleXE GO IF ERROR IN LOAD

        tst     <verbose
        beq     @NoDebug
        MPX9    DBGFMT
        fcs     /\tLOADed!\n\r/
        pshs    x,d
        ldd     #$80
        ldx     LOCALS.LoadAddr+4,S
        JSR     [DumpMem2v]     ; X & D Changed ; dump buffer DCB data.
        JSR     CRLF  
        puls    x,d
@NoDebug:

        tst     <verbose
        beq     @NoDebug
        leax    TxLoaded,pcr
        MPX9    PSTRNG  ; CC,A,X changed
        MPX9    DBGFMT
        FCS     /ModHdr:\r\n/
        ldd     #sizeof{MODHDR}
        ldx     LOCALS.pModHdr,s
        JSR     [DumpMem2v]     ; X & D Changed ; dump buffer DCB data.
        JSR     CRLF
@NoDebug:

        ; U --> DIRECTORY ENTRY

        LDD     DIREXT,U        ; Get EXECUTION entry ADDRESS
        CMPD    #$FFFF          ; EXECUTION NULL?
        BEQ     AddModuleXE     ; EXIT IF YES

        ; Calculate actual entry based on LoadAddress
        ldx     LOCALS.pModHdr,S  
        ldd     MODHDR.Entry,X
        addd    LOCALS.LoadAddr,S       ; calc relocated entry address
        TFR     D,Y

        ldx     LOCALS.CmdLineArgs,S    ; SETUP X TO POINT TO COMMAND ARGS

        leas    sizeof{LOCALS},S        ; Deallocate space on stack for temp variabes
                                        ; THE ORIGINAL CALLER RETURN ADDRESS IS STILL ON THE STACK
        jmp     ,Y                      ; GO TO LOADED MODULE, MODULE CODE SHOULD RETURN WITH 'rts' AND ERROR STATUS IN B

***********************************************************
AddModuleX:                             ; ERROR EXIT, ERROR CODE IN LOCALS.ErrStatus,S
        ldb     LOCALS.ErrStatus,S
AddModuleXE:                            ; ERROR EXIT, ERROR CODE IN B
        leas    sizeof{LOCALS},S        ; Deallocate space on stack for temp variabes
        tstb
        rts

***********************************************************
***********************************************************
Cmd_IM: ; IMPORT MODULE

        clrb 

        ; MPX9    DBGFMT
        ; fcs     /IMPORT MODULE\n\r/

        MPX9    ADDMOD

        rts

***********************************************************
***********************************************************
Cmd_LM: ; LIST MODULES

        clrb 

        ; MPX9    DBGFMT
        ; fcs     /LIST MODULES\n\r/

        ldy     <ModList.pHead          ; null if no more space
        beq     Cmd_LMX                 ; then exit with error
LOOP:   cmpy    <ModList.pNext          ; check if at end of allocated space
        beq     Cmd_LMX

        ldx     ,Y++

        tst     <verbose
        beq     @NoDebug
        ldd     #16
        pshs    X
        JSR     [DumpMem2v]             ; X & D Changed ; MODULE HEADDER buffer data.
        puls    X
        JSR     CRLF
@NoDebug

; Entry           rmw 1   ; Entry point
; CodeSize        rmw 1   ; code size (copied from disk)
; DataSize        rmw 2   ; data size (allocated un initialiazed)
; ModName         rmb 8    ; Module Name

        leau    MODHDR.SIG_LA,X ; U --> ModHdr
        leax    prefix,PCR      ; X --> prefix strings

        MPX9    PSTRNG
        ldd     ,U++
        MPX9    DSPDBY

        MPX9    PSTRNG
        ldd     ,U++
        MPX9    DSPDBY

        MPX9    PSTRNG
        ldd     ,U++
        MPX9    DSPDBY

        MPX9    PSTRNG
        ldd     ,U++
        MPX9    DSPDBY

        MPX9    PSTRNG
        tfr     U,X
        MPX9    PSTRNG
        JSR     CRLF

        bra     LOOP

Cmd_LMX
        rts

prefix:
        fcs     /@=$/
        fcs     /Entry=$/
        fcs     /Code=$/
        fcs     /Data=$/
        fcs     /Name=/

***********************************************************
***********************************************************
TxFile: FCB CR,LF
 FCS 'Filename:'
 ; FCB CR,LF+$80
TxDcb: FCB CR,LF
 FCS 'DCB:'
 ; FCB CR,LF+$80
TxFcb: FCB CR,LF
 FCS 'FCB:'
 ; FCB CR,LF+$80
TxDir: FCB CR,LF
 FCS 'DIR:'
 ; FCB CR,LF+$80
TxLoaded: FCB CR,LF
 FCS 'LOADED!\n\r'
 ; FCB CR,LF+$80
TxErr: FCB CR,LF
 FCS '*** Err:'
 ; FCB CR,LF+$80
TxBuf: FCB CR,LF
 FCS 'BUF:'

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


;verbose rmb     1
;addr    rmw 1 address to load at.
;MYFCB   rmb 32

ModList         SimpleList ; MPX9+ DYNAMIC Module TABLE
; .pHead pointer to DYNAMIC Module TABLE.
; .pNext pointer to next entry in the DYNAMIC Module TABLE
; .pEnd pointer to last byte of the DDYNAMIC Module TABLE

        endsection      ; section .dp

 END


ADD     ImportModule IM
        ListModule LM

IM Command opens file reads first sector
  gets ModuleName, entry & mod Static size and BSS size
  allocates mod Static size and BSS size
  loads full module
  call entry, pass in x -> command args, ( WOULD THIS BE USEFUL? y -> bss space)

LM command lists modules by name (/v with entry & mod Static size and BSS size, .bss address etc...)


SIG_LA CHECK

HAS IT BEEN LOADED ALREADY?
