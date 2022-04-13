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
MiniDos9Start EQU $C020
RetryBoot EQU $C035
BootLoaderERROR EQU $C044
ErrorString EQU $C06A
BootLoadedOK EQU $C05B
RAMSETUP EQU $C07B
LC087 EQU $C087
ClearDCB EQU $C0A1
DKDrvr EQU $C0BC
TestFuncCodeBits EQU $C0D4
Found EQU $C0DD
DriverBranchTable EQU $C0E1
DKCtrlFn EQU $C3A5
DKStatFn EQU $C0EE
DKWritFn EQU $C301
DKReadFn EQU $C1FD
DKStatFnError EQU $C15D
CRC EQU $C2E3
CX EQU $C2F5
C1 EQU $C2FD
DKReadFnX EQU $C264
MiniDos9SWI3 EQU $C33B
ReturnToPsyMon EQU $C3E6

ORG $C000
C000: 16 00 1D    LBRA  MiniDos9Start

C003: 16 00 75    LBRA  RAMSETUP

C006: 28 0A 19 0A 28 0A 19 0A 28 0A 19 0A 28 0A 19 0A ;; 4 copies of 4 bytes of data

C016: 00 02 04 06 08 01 03 05 07 09 

MiniDos9Start:
C020: 10 BE FF DE LDY   PSYMON_RAM     ;; BASE OF PSYMON RAM
C024: 30 8D 03 13 LEAX  $0313,PC ; ($C33B);; Y -> MiniDos9SWI3
C028: AF A8 72    STX   $72,Y          ;; SWI3v ; (114) SOFTWARE INTERRUPT 3
C02B: 10 AE A8 7E LDY   $7E,Y          ;; FRERAM ; (126) ADDRESS OF FREE RAM
C02F: FC FF EE    LDD   PSYMON_MonEnt  ;; MONITOR RE-ENTRY
C032: ED A8 40    STD   $40,Y          ;; save PSYMON_MonEnt in freeRam+$40
RetryBoot:
C035: 8D 44       BSR   RAMSETUP       ;; call with Y -> freeRam Ret x -> #DK DCB
C037: C6 01       LDB   #$01           ;; load B with 1, DSKRD function 
C039: E7 0A       STB   DCBDRV ,X      ;; DCBDRV (drive #1)
C03B: E7 0D       STB   DCBBUF ,X      ;; DCBBUF (DCBBUF is a Word 
                                       ;;     so this should set it to $0100)
C03D: 11 3F       SWI3                 ;; REQIO
C03F: 01 

C040: 6D 08       TST   DCBERR ,X      ;; DCBERR ERROR STATUS CODE
C042: 27 17       BEQ   BootLoadedOK   ;; NoError we are good
BootLoaderERROR:
C044: 34 10       PSHS  X
C046: 30 8C 21    LEAX  $0021,PC ; ($C06A)
C049: 11 3F       SWI3                 ;; PSTRNG
C04B: 04 

C04C: 35 10       PULS  X
C04E: A6 08       LDA   DCBERR ,X
C050: 11 3F       SWI3                 ;; DSPSBY
C052: 07 

C053: 11 3F       SWI3  
C055: 00                               ;; return to MONITR 

C056: 10 AE 06    LDY   DCBIOA ,X
C059: 20 DA       BRA   RetryBoot

BootLoadedOK:
C05B: 10 AE 06    LDY   DCBIOA ,X
C05E: 1F 50       TFR   PC,D
C060: 84 F0       ANDA  #$F0
C062: 5F          CLRB  
C063: 1F 03       TFR   D,U
C065: AD 98 0D    JSR   [$0D,X]        ;; transfer control to boot loader?
                                       ;;     done via DCBBUF,X
                                       ;; U -> Minidos/9 rom ($c000) 
                                       ;; Y -> DCBIOA
                                       ;; X -> #DK DCB
C068: 20 DA       BRA   BootLoaderERROR

ErrorString:
C06A: 0D 0A 2A 2A 2A 20 44 49 53 4B 20 45 52 52 4F 52 
C07A: A0 


*** *************************************
*** * Y-> scratch ram. (from psymon?)
*** *  Creates some table built ( 4 
*** *  occurance of 8 bytes of data, 
*** *  $20 bytes) from data @ C006
*** *  followed by the #DK DCB ($20 bytes)
*** *************************************
RAMSETUP:
C07B: 34 66       PSHS  U,Y,B,A
C07D: 30 8C 86    LEAX  $FF86,PC ; ($C006)
C080: 33 8C 93    LEAU  $FF93,PC ; ($C016)
C083: C6 04       LDB   #$04
C085: E7 E2       STB   ,-S            ;; loop 4 times, loop counter is on top of stack
LC087:
C087: 86 FF       LDA   #$FF
C089: A7 A0       STA   ,Y+
C08B: EC 81       LDD   ,X++
C08D: ED A1       STD   ,Y++
C08F: EC 81       LDD   ,X++
C091: ED A1       STD   ,Y++
C093: EF A1       STU   ,Y++
C095: 6F A0       CLR   ,Y+
C097: 6A E4       DEC   ,S             ;; dec loop counter
C099: 26 EC       BNE   LC087          ;; do it again if not done.
C09B: 32 61       LEAS  $0001,S        ;; remove loop counter from stack
C09D: 1F 21       TFR   Y,X
C09F: C6 20       LDB   #$20           ;; Set #bytes in DCB to clear
ClearDCB:
C0A1: 6F A0       CLR   ,Y+
C0A3: 5A          DECB  
C0A4: 26 FB       BNE   ClearDCB
C0A6: CC 44 4B    LDD   #$444B         ;; load d with 'DK' 
C0A9: ED 02       STD   DCBDID,X       ;; Store in DCBDID
C0AB: 33 8C 0E    LEAU  $000E,PC ; ($C0BC);; load driver address
C0AE: EF 04       STU   DCBDVR ,X      ;; store in DCBDrv
C0B0: 86 16       LDA   #$16
C0B2: A7 09       STA   DCBEXT ,X      ;; set DCBExt bytes to $16
C0B4: 10 AE 62    LDY   DCBDID,S       ;; really '2,S' so get Y from stack.
C0B7: 10 AF 06    STY   DCBIOA ,X      ;; set DCBIOA
C0BA: 35 E6       PULS  A,B,Y,U,PC ;(PUL? PC=RTS)

DKDrvr:
C0BC: 10 AE 06    LDY   DCBIOA ,X
C0BF: 34 06       PSHS  B,A
C0C1: 1F 50       TFR   PC,D
C0C3: 84 F0       ANDA  #$F0
C0C5: 5F          CLRB  
C0C6: C3 0C 00    ADDD  #$0C00
C0C9: 1F 8B       TFR   A,DP
C0CB: 86 01       LDA   #$01           ;; preset #DK DCBErr
C0CD: A7 08       STA   DCBERR ,X
C0CF: 86 08       LDA   #$08           ;; load A with function code to test
C0D1: 33 8C 0D    LEAU  DCBBUF ,PC ; ($C0E1);; U -> DriverBranchTable
TestFuncCodeBits:
C0D4: A5 61       BITA  $0001,S        ;; test A and Function code on stack
C0D6: 26 05       BNE   Found
C0D8: 33 43       LEAU  $0003,U        ;; point to next entry in DriverBranchTable
C0DA: 44          LSRA                 ;; next function (CntlFn, StatFn, WritFn, ReadFn)
C0DB: 26 F7       BNE   TestFuncCodeBits
Found:
C0DD: 35 06       PULS  A,B
C0DF: 6E C4       JMP   ,U             ;; jump into DriverBranchTable

DriverBranchTable:
C0E1: 16 02 C1    LBRA  DKCtrlFn

C0E4: 16 00 07    LBRA  DKStatFn

C0E7: 16 02 17    LBRA  DKWritFn

C0EA: 16 01 10    LBRA  DKReadFn

C0ED: 39          RTS   


*** *******************************
*** * Enter with X -> #DK DCB
*** * Y -> [DCBIOA,X] = FreeRam
*** *******************************
DKStatFn:
C0EE: 6C 08       INC   DCBERR ,X
C0F0: A6 0A       LDA   DCBDRV ,X
C0F2: 81 04       CMPA  #$04
C0F4: 22 67       BHI   DKStatFnError
C0F6: 4A          DECA  
C0F7: C6 08       LDB   #$08
C0F9: 3D          MUL   
C0FA: 31 AB       LEAY  D,Y            ;; Y -> DCB DRIVE TABLE index by Drive# 8 Bytes per entry 
C0FC: 6C 08       INC   DCBERR ,X
C0FE: A6 22       LDA   DCBDID,Y
C100: A7 E2       STA   ,-S
C102: 6F E2       CLR   ,-S
C104: 6F 0F       CLR   DCBTRK ,X
C106: EC 0B       LDD   DCBBLK ,X
C108: 6C 0F       INC   DCBTRK ,X
C10A: A3 E4       SUBD  ,S
C10C: 2A FA       BPL   $C108
C10E: E3 E1       ADDD  ,S++
C110: 6A 0F       DEC   DCBTRK ,X
C112: A6 0F       LDA   DCBTRK ,X
C114: A1 21       CMPA  $0001,Y
C116: 24 45       BCC   DKStatFnError
C118: E1 22       CMPB  DCBDID,Y
C11A: 24 41       BCC   DKStatFnError
C11C: EE 25       LDU   $0005,Y
C11E: A6 C5       LDA   B,U
C120: A7 88 10    STA   $10,X
C123: 96 03       LDA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
C125: 84 C0       ANDA  #$C0
C127: 48          ASLA  
C128: 49          ROLA  
C129: 49          ROLA  
C12A: 26 02       BNE   $C12E
C12C: 86 04       LDA   #$04
C12E: A1 0A       CMPA  DCBDRV ,X
C130: 26 05       BNE   $C137
C132: A6 A4       LDA   ,Y
C134: 4C          INCA  
C135: 26 18       BNE   $C14F
C137: A6 0A       LDA   DCBDRV ,X
C139: 84 03       ANDA  #$03
C13B: 44          LSRA  
C13C: 46          RORA  
C13D: 46          RORA  
C13E: 97 03       STA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
C140: CC 40 00    LDD   #$4000
C143: 8D 1B       BSR   $C160
C145: 26 16       BNE   DKStatFnError
C147: A6 A4       LDA   ,Y
C149: 4C          INCA  
C14A: 26 03       BNE   $C14F
C14C: 17 00 4A    LBSR  $C199
C14F: 8D 38       BSR   $C189
C151: 17 00 57    LBSR  $C1AB
C154: 96 03       LDA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect 
C156: 84 01       ANDA  #$01
C158: 43          COMA  
C159: 4C          INCA  
C15A: 6F 08       CLR   DCBERR ,X
C15C: 39          RTS   

DKStatFnError:
C15D: 86 80       LDA   #$80
C15F: 39          RTS   

C160: 34 16       PSHS  X,B,A
C162: 8D 25       BSR   $C189
C164: AE E4       LDX   ,S
C166: C6 0B       LDB   #$0B
C168: 86 10       LDA   #$10
C16A: 95 03       BITA  $03            ;; IO 03 DriveStatus/Drive&TrackSelect
C16C: 27 06       BEQ   $C174
C16E: 30 82       LEAX  ,-X
C170: 26 F8       BNE   $C16A
C172: 20 08       BRA   $C17C

C174: 95 03       BITA  $03            ;; IO 03 DriveStatus/Drive&TrackSelect
C176: 26 0A       BNE   $C182
C178: 30 82       LEAX  ,-X
C17A: 26 F8       BNE   $C174
C17C: AE 62       LDX   DCBDID,S
C17E: 8D 74       BSR   $C1F4
C180: 20 05       BRA   $C187

C182: 5A          DECB  
C183: 26 E5       BNE   $C16A
C185: 1A 04       ORCC  #$04
C187: 35 96       PULS  A,B,X,PC ;(PUL? PC=RTS)

C189: 8D 09       BSR   $C194
C18B: 85 04       BITA  #$04
C18D: 27 05       BEQ   $C194
C18F: CC 03 E8    LDD   #$03E8
C192: 8D 51       BSR   $C1E5
C194: 96 03       LDA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
C196: 0D 05       TST   $05            ;; IO 05 MotorOn/... Turn Motor on
C198: 39          RTS   

C199: 8D 33       BSR   $C1CE
C19B: 8D 31       BSR   $C1CE
C19D: 8D 2F       BSR   $C1CE
C19F: 8D 35       BSR   $C1D6
C1A1: 96 03       LDA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
C1A3: 85 02       BITA  #$02
C1A5: 26 F8       BNE   $C19F
C1A7: 6F A4       CLR   ,Y
C1A9: 20 1E       BRA   $C1C9

C1AB: A6 0F       LDA   DCBTRK ,X
C1AD: A0 A4       SUBA  ,Y
C1AF: 27 E7       BEQ   $C198
C1B1: 34 02       PSHS  A
C1B3: 2B 0A       BMI   $C1BF
C1B5: 8D 17       BSR   $C1CE
C1B7: 6C A4       INC   ,Y
C1B9: 6A E4       DEC   ,S
C1BB: 26 F8       BNE   $C1B5
C1BD: 20 08       BRA   $C1C7

C1BF: 8D 15       BSR   $C1D6
C1C1: 6A A4       DEC   ,Y
C1C3: 6C E4       INC   ,S
C1C5: 26 F8       BNE   $C1BF
C1C7: 32 61       LEAS  $0001,S
C1C9: 4F          CLRA  
C1CA: E6 24       LDB   DCBDVR ,Y
C1CC: 20 17       BRA   $C1E5

C1CE: 8D C4       BSR   $C194
C1D0: 84 CF       ANDA  #$CF
C1D2: 8A 10       ORA   #$10
C1D4: 20 04       BRA   $C1DA

C1D6: 8D BC       BSR   $C194
C1D8: 84 CF       ANDA  #$CF
C1DA: 8A 20       ORA   #$20
C1DC: 97 03       STA   $03
C1DE: 84 DF       ANDA  #$DF
C1E0: 97 03       STA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
C1E2: 4F          CLRA  
C1E3: E6 23       LDB   $0003,Y
C1E5: 34 16       PSHS  X,B,A
C1E7: 1E 91       EXG   B,X
C1E9: 86 C6       LDA   #$C6
C1EB: 4A          DECA  
C1EC: 26 FD       BNE   $C1EB
C1EE: 30 82       LEAX  ,-X
C1F0: 26 F7       BNE   $C1E9
C1F2: 35 96       PULS  A,B,X,PC ;(PUL? PC=RTS)

C1F4: 86 FF       LDA   #$FF
C1F6: A7 A4       STA   ,Y
C1F8: 86 04       LDA   #$04
C1FA: A7 08       STA   DCBERR ,X
C1FC: 39          RTS   

DKReadFn:
C1FD: 17 FE EE    LBSR  DKStatFn
C200: 26 62       BNE   DKReadFnX
C202: 8D 10       BSR   $C214
C204: 23 5E       BLS   DKReadFnX
C206: 8D C6       BSR   $C1CE
C208: 8D CC       BSR   $C1D6
C20A: 8D 8D       BSR   $C199
C20C: 8D 06       BSR   $C214
C20E: 23 54       BLS   DKReadFnX
C210: 8D 87       BSR   $C199
C212: 8D 97       BSR   $C1AB
C214: 8D 06       BSR   $C21C
C216: 23 4C       BLS   DKReadFnX
C218: 8D 02       BSR   $C21C
C21A: 23 48       BLS   DKReadFnX
C21C: 8D 7B       BSR   $C299
C21E: 26 42       BNE   $C262
C220: C4 F0       ANDB  #$F0
C222: 34 04       PSHS  B
C224: 8D 3F       BSR   $C265
C226: 26 38       BNE   $C260
C228: 86 06       LDA   #$06
C22A: A7 08       STA   DCBERR ,X
C22C: 8D 62       BSR   $C290
C22E: A1 0F       CMPA  DCBTRK ,X
C230: 26 2E       BNE   $C260
C232: 6C 08       INC   DCBERR ,X
C234: 8D 5A       BSR   $C290
C236: A1 88 10    CMPA  $10,X
C239: 26 25       BNE   $C260
C23B: 33 88 11    LEAU  $11,X
C23E: C6 08       LDB   #$08
C240: 8D 46       BSR   $C288
C242: 17 00 92    LBSR  $C2D7
C245: E6 88 15    LDB   $15,X
C248: 8D 3E       BSR   $C288
C24A: 33 88 19    LEAU  $19,X
C24D: C6 02       LDB   #$02
C24F: 8D 37       BSR   $C288
C251: 35 01       PULS  CC
C253: 17 00 8D    LBSR  CRC
C256: 10 A3 88 19 CMPD  $19,X
C25A: 26 06       BNE   $C262
C25C: 6F 08       CLR   DCBERR ,X
C25E: 20 02       BRA   $C262

C260: 35 01       PULS  CC
C262: 6D 08       TST   DCBERR ,X
DKReadFnX:
C264: 39          RTS   

C265: 86 FB       LDA   #$FB           ;; set SYNC byte to $FB
C267: 97 00       STA   $00            ;; IO 00 Status/Sync
C269: 86 05       LDA   #$05
C26B: A7 08       STA   DCBERR ,X
C26D: 86 24       LDA   #$24
C26F: 4A          DECA  
C270: 26 FD       BNE   $C26F
C272: 96 04       LDA   $04            ;; IO 04 ReceiverRestartPulse/WritePulse
C274: C6 01       LDB   #$01
C276: 96 02       LDA   $02            ;; IO 02 SectorCounter/FillWordPort
C278: 84 0F       ANDA  #$0F
C27A: A1 88 10    CMPA  $10,X
C27D: 26 08       BNE   $C287
C27F: D5 00       BITB  $00            ;; IO 00 Status/Sync
C281: 27 F3       BEQ   $C276
C283: 96 01       LDA   $01            ;; IO 01 ReceiveData/WriteData
C285: 6F 08       CLR   DCBERR ,X
C287: 39          RTS   

C288: 8D 06       BSR   $C290
C28A: A7 C0       STA   ,U+
C28C: 5A          DECB  
C28D: 26 F9       BNE   $C288
C28F: 39          RTS   

C290: 86 01       LDA   #$01
C292: 95 00       BITA  $00            ;; IO 00 Status/Sync
C294: 27 FC       BEQ   $C292
C296: 96 01       LDA   $01            ;; IO 01 ReceiveData/WriteData
C298: 39          RTS   

C299: 6F 08       CLR   DCBERR ,X
C29B: A6 88 10    LDA   $10,X
C29E: 26 02       BNE   $C2A2
C2A0: A6 22       LDA   DCBDID,Y
C2A2: 4A          DECA  
C2A3: 34 12       PSHS  X,A
C2A5: 1F A9       TFR   CC,B
C2A7: 8E 80 00    LDX   #$8000
C2AA: 1A 50       ORCC  #$50
C2AC: 17 FE E5    LBSR  $C194
C2AF: 27 10       BEQ   $C2C1
C2B1: 27 16       BEQ   $C2C9
C2B3: 96 02       LDA   $02            ;; IO 02 SectorCounter/FillWordPort
C2B5: 84 0F       ANDA  #$0F
C2B7: A1 E4       CMPA  ,S
C2B9: 26 0E       BNE   $C2C9
C2BB: 86 10       LDA   #$10
C2BD: 95 03       BITA  $03            ;; IO 03 DriveStatus/Drive&TrackSelect
C2BF: 27 08       BEQ   $C2C9
C2C1: 95 03       BITA  $03            ;; IO 03 DriveStatus/Drive&TrackSelect
C2C3: 26 FC       BNE   $C2C1
C2C5: 1C FE       ANDCC #$FE
C2C7: 35 92       PULS  A,X,PC ;(PUL? PC=RTS)

C2C9: 1F 9A       TFR   B,CC
C2CB: 30 82       LEAX  ,-X
C2CD: 26 DB       BNE   $C2AA
C2CF: 35 12       PULS  A,X
C2D1: 17 EE 20    LBSR  $B0F4
C2D4: 1A 01       ORCC  #$01
C2D6: 39          RTS   

C2D7: EE 0D       LDU   DCBBUF ,X
C2D9: 11 83 FF FF CMPU  #$FFFF
C2DD: 26 03       BNE   $C2E2
C2DF: EE 88 16    LDU   $16,X
C2E2: 39          RTS   


*** ; Calculate CRC code
CRC:
C2E3: 8D F2       BSR   $C2D7
C2E5: 4F          CLRA  
C2E6: 34 02       PSHS  A
C2E8: E6 88 15    LDB   $15,X
C2EB: 8D 08       BSR   CX
C2ED: 33 0F       LEAU  DCBTRK ,X
C2EF: C6 0A       LDB   #$0A
C2F1: 8D 02       BSR   CX
C2F3: 35 84       PULS  B,PC ;(PUL? PC=RTS)

CX:
C2F5: A8 C0       EORA  ,U+
C2F7: 48          ASLA  
C2F8: 69 62       ROL   DCBDID,S
C2FA: 24 01       BCC   C1
C2FC: 4C          INCA  
C1:
C2FD: 5A          DECB  
C2FE: 26 F5       BNE   CX
C300: 39          RTS   

DKWritFn:
C301: 54          LSRB  
C302: 24 06       BCC   $C30A
C304: 33 8D 00 5E LEAU  $005E,PC ; ($C366)
C308: 34 40       PSHS  U
C30A: 17 FD E1    LBSR  DKStatFn
C30D: 26 45       BNE   $C354
C30F: C6 09       LDB   #$09
C311: E7 08       STB   DCBERR ,X
C313: 85 02       BITA  #$02
C315: 27 3D       BEQ   $C354
C317: 8D CA       BSR   CRC
C319: ED 88 19    STD   $19,X
C31C: 86 FF       LDA   #$FF
C31E: 97 02       STA   $02            ;; IO 02 SectorCounter/FillWordPort
C320: 17 FF 76    LBSR  $C299
C323: 26 2F       BNE   $C354
C325: 34 04       PSHS  B
C327: 0F 04       CLR   $04            ;; IO 04 ReceiverRestartPulse/WritePulse
C329: C6 10       LDB   #$10
C32B: 4F          CLRA  
C32C: 8D 31       BSR   $C35F
C32E: 5A          DECB  
C32F: 26 FB       BNE   $C32C
C331: 86 FB       LDA   #$FB
C333: 8D 2A       BSR   $C35F
C335: 33 0F       LEAU  DCBTRK ,X
C337: C6 0A       LDB   #$0A
C339: 8D 1C       BSR   $C357
MiniDos9SWI3:
C33B: 8D 9A       BSR   $C2D7
C33D: E6 88 15    LDB   $15,X
C340: 8D 15       BSR   $C357
C342: 33 88 19    LEAU  $19,X
C345: C6 02       LDB   #$02
C347: 8D 0E       BSR   $C357
C349: 35 01       PULS  CC
C34B: 17 FE 46    LBSR  $C194
C34E: 85 08       BITA  #$08
C350: 27 F9       BEQ   $C34B
C352: 6F 08       CLR   DCBERR ,X
C354: 6D 08       TST   DCBERR ,X
C356: 39          RTS   

C357: A6 C0       LDA   ,U+
C359: 8D 04       BSR   $C35F
C35B: 5A          DECB  
C35C: 26 F9       BNE   $C357
C35E: 39          RTS   

C35F: 0D 00       TST   $00
C361: 2A FC       BPL   $C35F
C363: 97 01       STA   $01            ;; IO 01 ReceiveData/WriteData
C365: 39          RTS   

C366: 26 2F       BNE   $C397
C368: 17 FF 2E    LBSR  $C299
C36B: 26 2A       BNE   $C397
C36D: 34 04       PSHS  B
C36F: 17 FE F3    LBSR  $C265
C372: 27 21       BEQ   $C395
C374: 86 08       LDA   #$08
C376: A7 08       STA   DCBERR ,X
C378: 33 0F       LEAU  DCBTRK ,X
C37A: C6 0A       LDB   #$0A
C37C: 8D 1C       BSR   $C39A
C37E: 26 15       BNE   $C395
C380: 17 FF 54    LBSR  $C2D7
C383: E6 88 15    LDB   $15,X
C386: 8D 12       BSR   $C39A
C388: 26 0B       BNE   $C395
C38A: 33 88 19    LEAU  $19,X
C38D: C6 02       LDB   #$02
C38F: 8D 09       BSR   $C39A
C391: 26 02       BNE   $C395
C393: 6F 08       CLR   DCBERR ,X
C395: 35 01       PULS  CC
C397: 6D 08       TST   DCBERR ,X
C399: 39          RTS   

C39A: 17 FE F3    LBSR  $C290
C39D: A1 C0       CMPA  ,U+
C39F: 26 03       BNE   $C3A4
C3A1: 5A          DECB  
C3A2: 26 F6       BNE   $C39A
C3A4: 39          RTS   

DKCtrlFn:
C3A5: 4D          TSTA  
C3A6: 26 20       BNE   $C3C8
C3A8: 86 40       LDA   #$40
C3AA: 97 03       STA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
C3AC: CC 40 00    LDD   #$4000
C3AF: 17 FD AE    LBSR  $C160
C3B2: 26 08       BNE   $C3BC
C3B4: A6 A4       LDA   ,Y
C3B6: 4C          INCA  
C3B7: 26 03       BNE   $C3BC
C3B9: 17 FD 0D    LBSR  $C0C9
C3BC: 31 28       LEAY  DCBERR ,Y
C3BE: 96 03       LDA   $03            ;; IO 03 DriveStatus/Drive&TrackSelect
C3C0: 84 C0       ANDA  #$C0
C3C2: 8B 40       ADDA  #$40
C3C4: 81 40       CMPA  #$40
C3C6: 26 E2       BNE   $C3AA
C3C8: 39          RTS   

C3C9: 10 8E FF EE LDY   #PSYMON_MonEnt
C3CD: AE 6A       LDX   DCBDRV ,S
C3CF: A6 80       LDA   ,X+
C3D1: AF 6A       STX   DCBDRV ,S
C3D3: E6 1F       LDB   $FFFF,X
C3D5: 26 02       BNE   $C3D9
C3D7: 6E B4       JMP   [,Y]

C3D9: 58          ASLB  
C3DA: 81 07       CMPA  #$07
C3DC: 22 08       BHI   ReturnToPsyMon
C3DE: 50          NEGB  
C3DF: 31 B5       LEAY  [B,Y]
C3E1: 10 AF 38    STY   $FFF8,Y
C3E4: 35 BF       PULS  CC,A,B,DP,X,Y,PC ;(PUL? PC=RTS)

ReturnToPsyMon:
C3E6: BE FF DE    LDX   PSYMON_RAM
C3E9: AE 88 7E    LDX   $7E,X          ;; FRERAM ; (126) ADDRESS OF FREE RAM
C3EC: 6E 98 40    JMP   [$40,X]        ;; PSYMON_MonEnt


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
*** 			bit 5 		step pulse bit; causes data·transfer head to
*** 					jump to next track in direction given by bit 4
*** 			bits 6, 7	binary number of drive to be selected
*** CC04 		Accessing this location with a store instruction (STA) causes a write
*** 			operation to take place
*** CC05		Accessing this location with either a load (LDA) or store instruction
*** 			causes a motor·on pulse to be sent to the disk drive
*** CC06		Accessing this location with either a load or store instruction causes a
*** 			motor·off pulse to be sent to the disk drive
*** *********************************************************************
C3EF: FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF 
C3FF: FF 

