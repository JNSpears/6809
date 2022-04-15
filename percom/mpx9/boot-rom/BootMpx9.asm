DCBLNK EQU $0000
DCBDID EQU $0002
DCBDVR  EQU $0004
DCBIOA  EQU $0006
DCBERR  EQU $0008
DCBEXT  EQU $0009
DCBDRV  EQU $000A
DCBBLK  EQU $000B
DCBBUF  EQU $000D
DCBTRK  EQU $000F
DCBSEC  EQU $0010
DCBCUR  EQU $000F
DCBPRV  EQU $0011
DCBNXT  EQU $0013
DCBCNT  EQU $0015
DCBADD  EQU $0016
DCBTYP  EQU $0018
DCBCRC  EQU $0019


PSYMON_RAM EQU $FFDE
PSYMON_MonEnt EQU $FFEE

**************************************************
* SWI3 PARAMETER DEFINITIONS                     *
**************************************************
* PSYMON (tm) ROUTINE REFERENCES
MONITR EQU 0 RETURN TO MONITOR MODE
REQIO EQU 1 REQUEST I/O
OUTCHR EQU 2 OUTPUT CHARACTER TO TERMINAL
INCHR EQU 3 INPUT CHARACTER FROM TERMINAL
PSTRNG EQU 4 PRINT STRING
GETHEX EQU 5 GET HEX NUMBER
DSPDBY EQU 6 DISPLAY DOUBLE BYTE
DSPSBY EQU 7 DISPLAY SINGLE BYTE
 SPC 1
* MPX/9 ROUTINE REFERENCES
MPX EQU 8 RETURN TO MPX/9
GETLIN EQU 9 GET A LINE OF INPUT
SKPSPC EQU 10 SKIP SPACES IN LINE BUFFER
GETWRD EQU 11 GET THE NEXT WORD IN LINE
PROCMD EQU 12 PROCESS COMMAND LINE
RPTERR EQU 13 REPORT ERROR
LOCFIL EQU 14 LOCATE FILE IN DIRECTORY
LOCSPC EQU 15 LOCATE SPACE IN DIRECTORY
RDDRCT EQU 16 READ DISK DIRECTORY
WTDRCT EQU 17 WRITE DISK DIRECTORY
INTFCB EQU 18 INITIALIZE FCB
OPNFIL EQU 19 OPEN FILE
CLSFIL EQU 20 CLOSE FILE
RDFIL EQU 21 READ A FILE (BYTE)
WTFIL EQU 22 WRITE A FILE (BYTE)
RDBLK EQU 23 READ A BLOCK
WTBLK EQU 24 WRITE A BLOCK
MEMLOD EQU 25 LOAD A MEMORY SEGMENT
MEMSAV EQU 26 SAVE A MEMORY SEGMENT
COMPAR EQU 27 COMPARE STRINGS
BLKMOV EQU 28 BLOCK MOVE
DECNUM EQU 29 GET DECIMAL NUMBER
HEXNUM EQU 30 GET HEXADECIMAL NUMBER
DSPDEC EQU 31 DISPLAY DECIMAL NUMBER & SPACE
DELFIL EQU 32 DELETE A DISK FILE
LOCDCB EQU 33 LOCATE DCB FOR DEVICE
ADDDCB EQU 34 ADD DCB TO DEVICE LIST
DELDCB EQU 35 DELETE DCB FROM DEVICE LIST
 SPC 1
SYSLIM EQU 35 LAST VALID CALL
 SPC 1


**************************************************
* ERROR Codes                                    *
**************************************************
ERR_OK  EQU  0 ; OK - NO ERROR
ERR_FN  EQU  1 ; FN - ILLEGAL FUNCTION ATTEMPTED
ERR_ID  EQU  2 ; ID - ILLEGAL DISK DRIVE #
ERR_IB  EQU  3 ; IB - ILLEGAL BLOCK # USED
ERR_DM  EQU  4 ; DM - DISK MISSING OR INOPERATIVE
ERR_NB  EQU  5 ; NB - NULL (EMPTY) BLOCK READ
ERR_SK  EQU  6 ; SK - SEEK ERROR
ERR_RD  EQU  7 ; RD - DISK READ ERROR
ERR_VF  EQU  8 ; VF - DISK VERIFY ERROR
ERR_WP  EQU  9 ; WP - WRITE PROTECTED DISK
ERR_NF  EQU 10 ; NF - FILE NOT FOUND
ERR_DF  EQU 11 ; DF - DISK FULL OR NO SPACE FOR FILE
ERR_IF  EQU 12 ; IF - INVALID FILE SPEC
ERR_NC  EQU 13 ; NC - FILE NOT CLOSED
ERR_UF  EQU 14 ; UF - ACCESS TO UNOPENED FILE
ERR_IA  EQU 15 ; IA - ILLEGAL ACCESS TO FILE
ERR_EF  EQU 16 ; EF - END OF FILE
ERR_SN  EQU 17 ; SN - SYNTAX ERROR IN COMMAND


; SYSTEM ADDRESS CONSTANTS

ROM1    EQU $FC00 ; BASE ADDRESS OF PSYMON ROM
ROM2    EQU $F800 ; BASE ADDRESS OF EXTENSION ROM
RAM     EQU $F380 ; BASE ADDRESS OF SCRATCHPAD RAM;
FREE    EQU $F000 ; ADDRESS OF FREE RAM

TERMNL  EQU $F7FE ; SYSTEM TERMINAL ACIA
;TERMNL EQU $C000 ; SYSTEM TERMINAL ACIA -- for simulator

; ASCII CHARACTER CONSTANTS

CR EQU $D       ; CARRIAGE RETURN
LF EQU $A       ; LINE FEED
SP EQU $20      ; SPACE

; DCB offsets
;;DCBLnk  equ 0   ; pointer to next dcb in chain
;;DCBDId  equ 2   ; ascii 2 char device Id
;;DCBDrv  equ 4   ; device driver addr
;;DCBIOA  equ 6   ; device i/o addr
;;DCBErr  equ 8   ; error status code
;;DCBExt  equ 9   ; number of extension bytes in dcb
;;DCBApp  equ 10  ; dcb extension for driver
; DCB function Codes
ReadFn  equ 1   ; Read function code
WritFn  equ 2   ; Write function code
StatFn  equ 4   ; Status function code
CntlFn  equ 8   ; Device Control function code

; PSYMON RAM DEFINITIONS
        ORG $0  ; RAM
; PSYMON INTERNAL STACK & REGISTER SPACE
; OFFSETS TO RAM BASE IN PARENTHESES

        RMB 55 ; STACK SPACE r1
STACK   EQU * ; (55) TOP OF STACK

REGC    RMB 1 ; (55) CONDITION CODE REGISTER
REGA    RMB 1 ; (56) A REGISTER I-‘
REGB    RMB 1 ; (57) B REGISTER
REGD    RMB 1 ; (58) DIRECT PAGE REGISTER
REGX    RMB 2 ; (59) X REGISTER
REGY    RMB 2 ; (61) Y REGISTER M
REGU    RMB 2 ; (63) U STACK POINTER
REGP    RMB 2 ; (65) PROGRAM COUNTER

; PSYMON BREAKPOINT TABLE
BpTabl  RMB 15 ; (67) SPACE FOR 5 BREAKPOINTS
BpTEnd  EQU * ; (82) END OF BREAKPOINT TABLE

; PSYMON WORK AREAS
MemPtr  RMB 2 ; (82) MEMORY POINTER FOR ‘M’ COMMAN1
UsrTbl  RMB 2 ; (84) ADDRESS OF USER COMMAND TABLE)
Comand  RMB 1 ; (86) COMMAND CHARACTER STORAGE
CkSum   RMB 1 ; (87) CHECKSUM FOR LOAD AND SAVE
BegAdd  RMB 2 ; (88) BEGIN ADDRESS FOR SAVE
EndAdd  RMB 2 ; (90) END ADDRESS FOR SAVE
StkPtr  RMB 2 ; (92) CONTENTS OF STACK POINTER

; THE PSYMON CONSOLE DCB
ConDCB  RMB 10 ; (94) STANDARD DCB

; PSYMON DCB POINTERS
DCBCHN  RMB 2 ; (104) BASE OF DCB CHAIN
CIDCB   RMB 2 ; (106) CONSOLE INPUT DCB
CEDCB   RMB 2 ; (108) CONSOLE ECHO DCB
CODCB   RMB 2 ; (110) CONSOLE OUTPUT DCB
TPDCB   RMB 2 ; (112) CASSETTE TAPE DCB

; PSYMON VECTORS
SWI3v   RMB 2 ; (114) SOFTWARE INTERRUPT 3
SWI2v   RMB 2 ; (116) SOFTWARE INTERRUPT 2
FIRQv   RMB 2 ; (118) FAST INTERRUPT REQUEST
IRQv    RMB 2 ; (120) INTERRUPT REQUEST
SWIv    RMB 2 ; (122) SOFTWARE INTERRUPT
NMIv    RMB 2 ; (124) NON-MASKABLE INTERRUPT
FRERAM  RMB 2 ; (126) ADDRESS OF FREE RAM

;                      ():01052         *** *************************
;                      ():01053         *** * Software Vectors
;                      ():01054         *** *************************
;FFDE F380             ():01055                 FDB RAM         ; BASE OF PSYMON RAM
;FFE0 FD73             ():01056                 FDB DspSBy      ; DISPLAY SINGLE BYTE ON CONSOLE
;FFE2 FD6A             ():01057                 FDB DspDBy      ; DISPLAY DOUBLE BYTE ON CONSOLE
;FFE4 FD0E             ():01058                 FDB GetHex      ; GET HEX NUMBER FROM CONSOLE
;FFE6 FD97             ():01059                 FDB PString     ; PRINT STRING TO CONSOLE
;FFE8 FD44             ():01060                 FDB InChr       ; INPUT CHARACTER FROM CONSOLE
;FFEA FD58             ():01061                 FDB OutChr      ; OUTPUT CHARACTER TO CONSOLE
;FFEC FD63             ():01062                 FDB ReqIO       ; PERFORM I/O REQUEST
;FFEE FC32             ():01063                 FDB MonEnt      ; MONITOR RE-ENTRY
;                      ():01064

 ORG $C000

 LBRA  MiniDos9Start

 LBRA  RAMSETUP

DriveParams:
 fcb $28,$0A,$19,$0A ;; 4 copies of 4 bytes of data
 fcb $28,$0A,$19,$0A
 fcb $28,$0A,$19,$0A
 fcb $28,$0A,$19,$0A

SectorInterleave:
 fcb 00,02,04,06,08,01,03,05,07,09

MiniDos9Start:
 LDY   PSYMON_RAM     ;; BASE OF PSYMON RAM Y->F380
 LEAX  MiniDos9SWI3,PCR ; ($C33B);; Y -> MiniDos9SWI3
 STX   SWI3v,Y          ;; SWI3v ; (114) SOFTWARE INTERRUPT 3 (f3f2)
 LDY   FRERAM,Y          ;; FRERAM ; (126) ADDRESS OF FREE RAM
 LDD   PSYMON_MonEnt  ;; MONITOR RE-ENTRY
 STD   $40,Y          ;; save PSYMON_MonEnt in freeRam+$40
RetryBoot:
 BSR   RAMSETUP       ;; call with Y -> freeRam Ret x -> #DK DCB
 LDB   #$01           ;; load B with 1, DSKRD function
 STB   DCBDRV,X      ;; DCBDRV (drive #1)
 STB   DCBBUF,X      ;; DCBBUF (DCBBUF is a Word
                                       ;;     so this should set it to $0100)
 SWI3                 ;; REQIO
 FCB   REQIO

 TST   DCBERR,X      ;; DCBERR ERROR STATUS CODE
 BEQ   BootLoadedOK   ;; NoError we are good
BootLoaderERROR:
 PSHS  X
 LEAX  <ErrorString,PCR ; ($C06A)
 SWI3                 ;; PSTRNG
 FCB   PSTRNG

 PULS  X
 LDA   DCBERR,X
 SWI3                 ;; DSPSBY
 FCB   DSPSBY

 SWI3                 ;; return to MONITR
 FCB   MONITR

 LDY   DCBIOA,X
 BRA   RetryBoot

BootLoadedOK:
 LDY   DCBIOA,X
 TFR   PC,D
 ANDA  #$F0
 CLRB
 TFR   D,U
 JSR   [$0D,X]        ;; transfer control to boot loader?
                                       ;;     done via DCBBUF,X
                                       ;; U -> Minidos/9 rom ($c000) 
                                       ;; Y -> DCBIOA
                                       ;; X -> #DK DCB
 BRA   BootLoaderERROR

ErrorString:
 FCB CR,LF,$2A,$2A,$2A,$20,$44,$49,$53,$4B,$20,$45,$52,$52,$4F,$52,SP+$80


*** *************************************
*** * Y-> scratch ram. (from psymon?)
*** *  Creates some table built ( 4 
*** *  occurance of 8 bytes of data, 
*** *  $20 bytes) from data @ C006
*** *  followed by the #DK DCB ($20 bytes)
*** *************************************
RAMSETUP:
 PSHS  U,Y,B,A
 LEAX  <DriveParams,PCR ; ($C006)
 LEAU  <SectorInterleave,PCR ; ($C016)
 LDB   #$04
 STB   ,-S            ;; loop 4 times, loop counter is on top of stack
LC087:
 LDA   #$FF
 STA   ,Y+
 LDD   ,X++
 STD   ,Y++
 LDD   ,X++
 STD   ,Y++
 STU   ,Y++
 CLR   ,Y+
 DEC   ,S             ;; dec loop counter
 BNE   LC087          ;; do it again if not done.
 LEAS  $0001,S        ;; remove loop counter from stack
 TFR   Y,X
 LDB   #$20           ;; Set #bytes in DCB to clear
ClearDCB:
 CLR   ,Y+
 DECB
 BNE   ClearDCB
 LDD   #$444B         ;; load d with 'DK'
 STD   DCBDID,X       ;; Store in DCBDID
 LEAU  <DKDrvr,PCR ; ($C0BC);; load driver address
 STU   DCBDVR,X      ;; store in DCBDrv
 LDA   #$16
 STA   DCBEXT,X      ;; set DCBExt bytes to $16
 LDY   DCBDID,S       ;; really '2,S' so get Y from stack.
 STY   DCBIOA,X      ;; set DCBIOA
 PULS  A,B,Y,U,PC ;(PUL? PC=RTS)

DKDrvr:
 LDY   DCBIOA,X
 PSHS  B,A
 TFR   PC,D
 ANDA  #$F0
 CLRB
 ADDD  #$0C00
 TFR   A,DP
 LDA   #$01           ;; preset #DK DCBErr
 STA   DCBERR,X
 LDA   #$08           ;; load A with function code to test
 LEAU  <DriverBranchTable,PCR ; ($C0E1);; U -> DriverBranchTable
TestFuncCodeBits:
 BITA  $0001,S        ;; test A and Function code on stack
 BNE   Found
 LEAU  $0003,U        ;; point to next entry in DriverBranchTable
 LSRA                 ;; next function (CntlFn, StatFn, WritFn, ReadFn)
 BNE   TestFuncCodeBits
Found:
 PULS  A,B
 JMP   ,U             ;; jump into DriverBranchTable

DriverBranchTable:
 LBRA  DKCtrlFn
 LBRA  DKStatFn
 LBRA  DKWritFn
 LBRA  DKReadFn
 RTS


*** *******************************
*** * Enter with X -> #DK DCB
*** * Y -> [DCBIOA,X] = FreeRam
*** *******************************
DKStatFn:
 INC   DCBERR,X
 LDA   DCBDRV,X
 CMPA  #$04
 BHI   DKStatFnError
 DECA
 LDB   #$08
 MUL
 LEAY  D,Y            ;; Y -> DCB DRIVE TABLE index by Drive# 8 Bytes per entry
 INC   DCBERR,X
 LDA   DCBDID,Y
 STA   ,-S
 CLR   ,-S
 CLR   DCBTRK,X
 LDD   DCBBLK,X
 INC   DCBTRK,X
 SUBD  ,S
 BPL   $C108
 ADDD  ,S++
 DEC   DCBTRK,X
 LDA   DCBTRK,X
 CMPA  $0001,Y
 BCC   DKStatFnError
 CMPB  DCBDID,Y
 BCC   DKStatFnError
 LDU   $0005,Y
 LDA   B,U
 STA   $10,X
 LDA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 ANDA  #$C0
 ASLA
 ROLA
 ROLA
 BNE   $C12E
 LDA   #$04
 CMPA  DCBDRV,X
 BNE   $C137
 LDA   ,Y
 INCA
 BNE   $C14F
 LDA   DCBDRV,X
 ANDA  #$03
 LSRA
 RORA
 RORA
 STA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 LDD   #$4000
 BSR   $C160
 BNE   DKStatFnError
 LDA   ,Y
 INCA
 BNE   $C14F
 LBSR  $C199
 BSR   $C189
 LBSR  $C1AB
 LDA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 ANDA  #$01
 COMA
 INCA
 CLR   DCBERR,X
 RTS

DKStatFnError:
 LDA   #$80
 RTS

 PSHS  X,B,A
 BSR   $C189
 LDX   ,S
 LDB   #$0B
 LDA   #$10
 BITA  $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 BEQ   $C174
 LEAX  ,-X
 BNE   $C16A
 BRA   $C17C

 BITA  $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 BNE   $C182
 LEAX  ,-X
 BNE   $C174
 LDX   DCBDID,S
 BSR   $C1F4
 BRA   $C187

 DECB
 BNE   $C16A
 ORCC  #$04
 PULS  A,B,X,PC ;(PUL? PC=RTS)

 BSR   $C194
 BITA  #$04
 BEQ   $C194
 LDD   #$03E8
 BSR   $C1E5
 LDA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 TST   $05            ;; IO 05 MotorOn/... Turn Motor on
 RTS

 BSR   $C1CE
 BSR   $C1CE
 BSR   $C1CE
 BSR   $C1D6
 LDA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 BITA  #$02
 BNE   $C19F
 CLR   ,Y
 BRA   $C1C9

 LDA   DCBTRK,X
 SUBA  ,Y
 BEQ   $C198
 PSHS  A
 BMI   $C1BF
 BSR   $C1CE
 INC   ,Y
 DEC   ,S
 BNE   $C1B5
 BRA   $C1C7

 BSR   $C1D6
 DEC   ,Y
 INC   ,S
 BNE   $C1BF
 LEAS  $0001,S
 CLRA
 LDB   DCBDVR,Y
 BRA   $C1E5

 BSR   $C194
 ANDA  #$CF
 ORA   #$10
 BRA   $C1DA

 BSR   $C194
 ANDA  #$CF
 ORA   #$20
 STA   $03
 ANDA  #$DF
 STA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 CLRA
 LDB   $0003,Y
 PSHS  X,B,A
 EXG   B,X
 LDA   #$C6
 DECA
 BNE   $C1EB
 LEAX  ,-X
 BNE   $C1E9
 PULS  A,B,X,PC ;(PUL? PC=RTS)

 LDA   #$FF
 STA   ,Y
 LDA   #$04
 STA   DCBERR,X
 RTS

DKReadFn:
 LBSR  DKStatFn
 BNE   DKReadFnX
 BSR   $C214
 BLS   DKReadFnX
 BSR   $C1CE
 BSR   $C1D6
 BSR   $C199
 BSR   $C214
 BLS   DKReadFnX
 BSR   $C199
 BSR   $C1AB
 BSR   $C21C
 BLS   DKReadFnX
 BSR   $C21C
 BLS   DKReadFnX
 BSR   $C299
 BNE   $C262
 ANDB  #$F0
 PSHS  B
 BSR   $C265
 BNE   $C260
 LDA   #$06
 STA   DCBERR,X
 BSR   $C290
 CMPA  DCBTRK,X
 BNE   $C260
 INC   DCBERR,X
 BSR   $C290
 CMPA  $10,X
 BNE   $C260
 LEAU  $11,X
 LDB   #$08
 BSR   $C288
 LBSR  $C2D7
 LDB   $15,X
 BSR   $C288
 LEAU  $19,X
 LDB   #$02
 BSR   $C288
 PULS  CC
 LBSR  CRC
 CMPD  $19,X
 BNE   $C262
 CLR   DCBERR,X
 BRA   $C262

 PULS  CC
 TST   DCBERR,X
DKReadFnX:
 RTS

 LDA   #$FB           ;; set SYNC byte to $FB
 STA   $00            ;; IO 00 Status/Sync
 LDA   #$05
 STA   DCBERR,X
 LDA   #$24
 DECA
 BNE   $C26F
 LDA   $04            ;; IO 04 ReceiverRestartPulse/WritePulse
 LDB   #$01
 LDA   $02            ;; IO 02 SectorCounter/FillWordPort
 ANDA  #$0F
 CMPA  $10,X
 BNE   $C287
 BITB  $00            ;; IO 00 Status/Sync
 BEQ   $C276
 LDA   $01            ;; IO 01 ReceiveData/WriteData
 CLR   DCBERR,X
 RTS

 BSR   $C290
 STA   ,U+
 DECB
 BNE   $C288
 RTS

 LDA   #$01
 BITA  $00            ;; IO 00 Status/Sync
 BEQ   $C292
 LDA   $01            ;; IO 01 ReceiveData/WriteData
 RTS

 CLR   DCBERR,X
 LDA   $10,X
 BNE   $C2A2
 LDA   DCBDID,Y
 DECA
 PSHS  X,A
 TFR   CC,B
 LDX   #$8000
 ORCC  #$50
 LBSR  $C194
 BEQ   $C2C1
 BEQ   $C2C9
 LDA   $02            ;; IO 02 SectorCounter/FillWordPort
 ANDA  #$0F
 CMPA  ,S
 BNE   $C2C9
 LDA   #$10
 BITA  $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 BEQ   $C2C9
 BITA  $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 BNE   $C2C1
 ANDCC #$FE
 PULS  A,X,PC ;(PUL? PC=RTS)

 TFR   B,CC
 LEAX  ,-X
 BNE   $C2AA
 PULS  A,X
 LBSR  $B0F4
 ORCC  #$01
 RTS

 LDU   DCBBUF,X
 CMPU  #$FFFF
 BNE   $C2E2
 LDU   $16,X
 RTS


*** ; Calculate CRC code
CRC:
 BSR   $C2D7
 CLRA
 PSHS  A
 LDB   $15,X
 BSR   CX
 LEAU  DCBTRK,X
 LDB   #$0A
 BSR   CX
 PULS  B,PC ;(PUL? PC=RTS)

CX:
 EORA  ,U+
 ASLA
 ROL   DCBDID,S
 BCC   C1
 INCA
C1:
 DECB
 BNE   CX
 RTS

DKWritFn:
 LSRB
 BCC   $C30A
 LEAU  LC366,PCR ; ($C366)
 PSHS  U
 LBSR  DKStatFn
 BNE   $C354
 LDB   #$09
 STB   DCBERR,X
 BITA  #$02
 BEQ   $C354
 BSR   CRC
 STD   $19,X
 LDA   #$FF
 STA   $02            ;; IO 02 SectorCounter/FillWordPort
 LBSR  $C299
 BNE   $C354
 PSHS  B
 CLR   $04            ;; IO 04 ReceiverRestartPulse/WritePulse
 LDB   #$10
 CLRA
 BSR   $C35F
 DECB
 BNE   $C32C
 LDA   #$FB
 BSR   $C35F
 LEAU  DCBTRK,X
 LDB   #$0A
 BSR   $C357
MiniDos9SWI3:
 BSR   $C2D7
 LDB   $15,X
 BSR   $C357
 LEAU  $19,X
 LDB   #$02
 BSR   $C357
 PULS  CC
 LBSR  $C194
 BITA  #$08
 BEQ   $C34B
 CLR   DCBERR,X
 TST   DCBERR,X
 RTS

 LDA   ,U+
 BSR   $C35F
 DECB
 BNE   $C357
 RTS

 TST   $00
 BPL   $C35F
 STA   $01            ;; IO 01 ReceiveData/WriteData
 RTS

LC366:  ;; JNS ?????
 BNE   $C397
 LBSR  $C299
 BNE   $C397
 PSHS  B
 LBSR  $C265
 BEQ   $C395
 LDA   #$08
 STA   DCBERR,X
 LEAU  DCBTRK,X
 LDB   #$0A
 BSR   $C39A
 BNE   $C395
 LBSR  $C2D7
 LDB   $15,X
 BSR   $C39A
 BNE   $C395
 LEAU  $19,X
 LDB   #$02
 BSR   $C39A
 BNE   $C395
 CLR   DCBERR,X
 PULS  CC
 TST   DCBERR,X
 RTS

 LBSR  $C290
 CMPA  ,U+
 BNE   $C3A4
 DECB
 BNE   $C39A
 RTS

DKCtrlFn:
 TSTA
 BNE   $C3C8
 LDA   #$40
 STA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 LDD   #$4000
 LBSR  $C160
 BNE   $C3BC
 LDA   ,Y
 INCA
 BNE   $C3BC
 LBSR  $C0C9
 LEAY  DCBERR,Y
 LDA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
 ANDA  #$C0
 ADDA  #$40
 CMPA  #$40
 BNE   $C3AA
 RTS

 LDY   #PSYMON_MonEnt
 LDX   DCBDRV,S
 LDA   ,X+
 STX   DCBDRV,S
 LDB   -1,X
 BNE   $C3D9
 JMP   [,Y]

 ASLB
 CMPA  #$07
 BHI   ReturnToPsyMon
 NEGB
 LEAY  [B,Y]
 STY   <<$FFF8,Y
 PULS  CC,A,B,DP,X,Y,PC ;(PUL? PC=RTS)

ReturnToPsyMon:
 LDX   PSYMON_RAM
 LDX   FRERAM,X ;$7E,X          ;; FRERAM ; (126) ADDRESS OF FREE RAM
 JMP   [$40,X]        ;; PSYMON_MonEnt


*** *********************************************************************
*** *                  From Byte mag. May 1980 p. 48                    *
*** *********************************************************************
*** Hexadecimal 		Function When Used as Input
*** Address
*** CC00 	Read USRT status:
*** 		bit 0 = 1 means disk unit ready to send byte to computer
*** 			at address CC01 during read operation
*** 		bit 7 = 1 means disk unit ready to receive byte from computer
*** 			at address CC01 during write operation
*** CC01 	Address used to transmit data from disk drive to computer during read
*** 		operation
*** CC02 	During read operation, bits 0 thru 3 contain current sector number in
*** 		binary
*** CC03 	Drive status byte: see table 2b.
*** CC04 	Accessing this location with a load instruction (LDA) causes a read
*** 		operation to take place
*** *********************************************************************
*** Table 2b Drive status byte (Read)
*** Bit	Value	Meaning
*** 0	1	Write protect notch in disk covered; disk is protected
*** 1	1 	Head is at track 0
*** 2	0	Drive motor is on
*** 3	0	Drive circuit is ready to write to disk
*** 4	1	Sector pulse; drive detects sector hole
*** 5	1	Index pulse; drive detects special index hole
*** 6,7		Binary number of drive selected (01 thru 03)
*** *********************************************************************
*** table 2c
*** Hexadecimal	Function When Used as Output
*** Address
*** CC00		Defines value that controller will recognize as the SYNC byte at the
*** 			beginning of a read operation; hexadecimal FB used in Percom format
*** CC01		Address used to transmit data from computer to disk unit during write
*** 			operation
*** CC02		Defines value that controller will recognize as the filler byte (written
*** 			after trailer until disk motor turns off); hexadecimal FF used in this
*** 			software
*** CC03		Data to select drive and head movement direction:
*** 			bit 4 		direction of head movement: 1 = in, 0 = out
*** 			bit 5 		step pulse bit; causes data�transfer head to
*** 					jump to next track in direction given by bit 4
*** 			bits 6, 7	binary number of drive to be selected
*** CC04 		Accessing this location with a store instruction (STA) causes a write
*** 			operation to take place
*** CC05		Accessing this location with either a load (LDA) or store instruction
*** 			causes a motor�on pulse to be sent to the disk drive
*** CC06		Accessing this location with either a load or store instruction causes a
*** 			motor�off pulse to be sent to the disk drive
*** *********************************************************************


