 NAM DSKEDT
 TTL 6809 DISK EDITOR FOR LFD 400/800
 SPC 1
**********************************************
*                                            *
* A SIMPLE DISK SECTOR EDITOR FOR THE PERCOM *
* LFD-400 DISK SYSTEM UNDER THE MPX-9 SYSTEM *
*                                            *
* WRITTEN BY TIM MCKEE AUGUST 21,1980        *
*                                            *
**********************************************
 SPC 1
* VERSION 1.00
 SPC 2
**********************************************
* SYSTEM CALL PARAMETER DEFINITIONS
**********************************************
* PSYMON (TM) ROUTINE REFERENCES
REQIO EQU 1 REQUEST I/O
OUTCHR EQU 2 OUTPUT CHARACTER TO CONSOLE
INCHR EQU 3 INPUT CHARACTER FROM CONSOLE
PSTRNG EQU 4 PRINT STRING
GETHEX EQU 5 GET HEX NUMBER
DSPSBY EQU 7 DISPLAY SINGLE BYTE
 SPC 1
* MPX/9 ROUTINE REFERENCES
MPX EQU 8 RETURN TO MPX/9
GETLIN EQU 9 GET A LINE OF INPUT
SKPSPC EQU 10 SKIP SPACES IN LINE BUFFER
RPTERR EQU 13 REPORT DOS ERROR
DECNUM EQU 29 GET DECIMAL NUMBER
HEXNUM EQU 30 GET HEXADECIMAL NUMBER
LOCDCB EQU 33 LOCATE DCB FOR DEVICE
 SPC 2
***********************************************
* MINIDOS/9 DCB DEFINITION
***********************************************
DCBERR EQU 8 ERROR STATUS CODE
DCBDRN EQU 10 DRIVE NUMBER
DCBBLK EQU 11 DISK BLOCK
DCBBUF EQU 13 BUFFER ADDRESS
DCBPRV EQU 17 BACK LINK
DCBNXT EQU 19 FRIND LINK
DCBCNT EQU 21 BYTE COUNT IN THIS BLOCK
DCBADD EQU 22 DATA ADDRESS FOR THIS BLOCK
DCBTYP EQU 24 BLOCK TYPE CODE
DCBCRC EQU 25 DATA CRC
 SPC 1
DCBSIZ EQU 32 MAXIMUM DCB SIZE
 SPC 1
* FUNCTION CODES
DSKRD EQU 1 READ DISK BLOCK
DSKWT EQU 2 WRITE DISK BLOCK
DSKWV EQU 3 WRITE & VERIFY DISK BLOCK
DSKSL EQU 4 SELECT DISK & RETURN STATUS
DSKCT EQU 8 DISK CONTROL FUNCTION
 SPC 2
* PROGRAM EQUATES
 SPC 1
SPACE EQU $20 SPACE CODE
TERMSP EQU SPACE+$80 TERMINAL SPACE FOR PSTRNG
CR EQU $D
LF EQU $A
PSYRAM EQU $FFDE
COOFST EQU $6E
CEOFST EQU $60
 SPC 3
 ORG 0
 SPC 1
* PROGRAM IS POSITION INDEPENDENT
 SPC 2
DSKEDT TFR S,U SAVE STACK FOR ERROR RECOVERY
 LDY PSYRAM
 LEAX COOFST,Y
 STX CODCB,PCR SET UP CODCB POINTER
 LEAX CEOFST,Y
 STX CEDCB,PCR SET UP CEDCB POINTER
 LBSR ERASE
 LDD #0
 STD CURSEC,PCR INITIALIZE VARIABLES
 STA DIVFLG,PCR
 STA ERRFLG,PCR
 LBSR GETDRV GET DRIVE NUMBER
 LBSR CRLF
NEWSEC LBSR GETSEC GET NEW SECTOR NUMBER
READIT LBSR RDSEC 
 BEQ REPLOT
RDERR LBSR ERROR
  BRA NEWSEC
  SPC 2
REPLOT LBSR PLOTSC PLOT THE SECTOR
  TST DIVFLG,PCR CHECK IF OUTPUT DIVERTED
  BEQ GETCMD SKIP IF NOT
  PULS X RECOVER OLD OUTPUT VECTOR
  STX [CODCB,PCR] AND RESTORE IT
  CLR DIVFLG,PCR RESET DIVERT FLAG
  SPC 1
GETCMD LEAX CMDMSG,PCR
 SWI3
 FCB PSTRNG ASK FOR COMMAND
 LEAY CMDCHR,PCR
 LBSR GETCHR WAIT FOR ONE OF THE CHARACTERS
 CMPB #6 CHECK IF COMMAND LEAVES SECTOR
 BLO CHKWRT BRANCH IF SO
 CMPB #7 CHECK IF COMMAND IS WODIFY
 BNE NOTMOD SKIP IF NOT
 LBSR MODIFY
 LBRA REPLOT
NOTMOD LEAX DIVMSG,PCR
 SWI3
 FCB PSTRNG ASK FOR DEVICE FOR REPLOT LISTING
 LEAX LINBUF,PCR
 LDB #3 ALLOW 2 CHARACTERS FOR DEVICE NAME
 SWI3
 FCB GETLIN GET DEVICE NAME
 BEQ NOTMOD LOOP ON LINE CANCEL
 LBSR CRLF
 SWI3
 FCB SKPSPC POINT TO FIRST CHAR
 LBEQ REPLOT DEFAULT TO CONSOLE IF NONE GIVEN
 LDD ,X GET DEVICE NAME
 SWI3
 FCB LOCDCB LOCATE DEVICE
 BEQ NOTMOD LOOP ON DEVICE NOT FOUND
 LDY [CODCB,PCR]
 PSHS Y SAVE OLD OUTPUT VECTOR ON STACK
 STX [CODCB,PCR] SET UP NEW OUTPUT VECTOR
 COM DIVFLG,PCR FLAG OUTPUT AS DIVERTED
 LBRA REPLOT PLOT NEW PAGE
 SPC 2
CHKWRT PSHS B SAVE 'B'
 TST MODFLG,PCR SEE IF CONTNNTS HAVE HFEN MOOIFIM
 BEQ NOWRIT SKIP IA NOT
 LEAX UPDQRY,PCR
 SWI3
 FCB PSTRNG ASK IF UPDATE DESIRED
 LEAY YNCHR,PCR ALLOW 'Y', OR 'N' ANSWERS ONLY
 LBSR GETCHR GET ANSWER
 TSTB B =0 IF 'Y'
 BNE NOWRIT SKIP ON 'N'
 LBSR WRTSEC OIRITE THE NEW SECTOR TO DISK
NOWRIT PULS B RECOVER 'B'
 DECB
 BMI NXTSEC BRANCH IF 'F'
 DECB
 BMI PRVSEC BRANCH IF 'B'
 DECB    
 BMI ENDSEC BRANCH IF '+'
 DECB
 BMI BEGSEC BRANCH IF /-J
 DECB
 LBMI NEWSEC BRANCH IF 'R'
 SPC 1
RETURN TFR U,S ASSUME E'X'IT IF NONE OF ABOVE
 CLRB
 RTS
 SPC 2
NXTSEC LDD CURSEC,PCR
 ADDD #1
NXTSC1 STD CURSEC,PCR
NXTSC2 LBRA READIT
 SPC 1
PRVSEC LDD CURSEC,PCR
 SUBD #1 POINT TO PREVIOUS SECTOR
 BPL NXTSC1 FOR VALID SECTOR NUMBERS
 BRA NXTSC2 FOR INVALID (NEG) SECTOR NUMBERS
 SPC 2
ENDSEC LDD FWDLNK,PCR
 LBEQ REPLOT FINISHED WHEN FWDLNK IS NULL
 STD CURSEC,PCR
 LBSR RDSEC READ THE NEXT SECTOR
 LBNE RDERR REPORT READ ERRORS AND ABORT OPERATION
 BRA ENDSEC LOOP UNTIL FINISHED
 SPC 1
BEGSEC LDD BAKLNK,PCR
 LBEQ REPLOT FINISHED WHEN BAKLNK IS NULL
 STD CURSEC,PCR
 LBSR RDSEC READ THE PREVIOUS SECTOR
 LBNE RDERR REPORT READ ERRORS AND ABORT OPERATION
 BRA BEGSEC LOOP UNTIL FINISHED
 SPC 2
PLOTSC LBSR ERASE ERASE THE SCREEN
 LBSR PDRIVE PRINT DRIVE NUMBER
 LEAX SCTMSG,PCR
 SWI3
 FCB PSTRNG
 LDD CURSEC,PCR
 LBSR OUT3DC PRINT VALUE OF CURRENT SECTOR
 LEAX FWDMSG,PCR
 SWI3
 FCB PSTRNG
 LDD FWDLNK,PCR
 LBSR OUT3DC PRINT VALUE OF FWD LINK
 LEAX BAKMSG,PCR
 SWI3
 FCB PSTRNG
 LDD BAKLNK,PCR
 LBSR OUT3DC PRINT VALUE OF REV LINK
 LEAX BYTMSG,PCR
 SWI3
 FCB PSTRNG
 LDD BYTCNT,PCR
 LBSR OUT3DC PRINT VALUE OF BYTECOUNT
 LEAX TYPMSG,PCR
 SWI3
 FCB PSTRNG
 LDA FILTYP,PCR
 SWI3
 FCB DSPSBY PRINT VALUE OF FILE TYPE
 LEAX RWMSG,PCR
 SWI3
 FCB PSTRNG PRINT ROW HEADER
 CLRB LET B POINT TO FIRST BYTE OF DATA
 SPC 1
NXTROW TFR B,A TRANSFER B => A
 SWI3
 FCB DSPSBY PRINT VALUE OF ROW NUMBER
 LBSR SPACER ADD A SPACE
 CLRA
NXTCL1 CMPD BYTCNT,PCR ARE WE PAST BYTE COUNT?
 BLO DISPLY BRANCH IF NOT
 LBSR SPACER
 LBSR SPACER
 LBSR SPACER PAD FOR FORMATTING
 BRA BUMP1 AND CONTINUE
 SPC 1
DISPLY LEAX DATA,PCR POINT TO DATA BUFFER
 CLRA
 LDA D,X GET DATA BYTE INTO 'A'
 SWI3
 FCB DSPSBY PRINT VALUE OF DATA BYTE
BUMP1 CLRA
 ADDD #1 POINT TO NEXT DATA BYTE
 BITB #$F CHECK FOR END OF ROW
 BNE NXTCL1 CONTINUE IF NOT
 PSHS A,B SAVE POINTER
 SUBB #$10 POINT TO BEGINNING OF THIS RON
 LDA #'! USE A '!' TO MARK BEG F ASCII DATA
 SWI3
 FCB OUTCHR
 CLRA
NXTCL2 CMPD BYTCNT,PCR CHECK FOR END OF DATA
 BLO DSPCH1 BRANCH IF NOT END
 LDA #'< USE '<' TO INDICATE PAST LIMIT
 BRA DSPCHR
 SPC 1
DSPCH1 LEAX DATA,PCR
 LDA D,X GET DATA BYTE FROM BUFFER
 ANDA #$7F MASK OFF MSB
 CMPA #$7F CHECK FOR RUBOUT
 BEQ DSPCTL NO PRINT IF SO
 CMPA #SPACE
 BHS DSPCHR
DSPCTL LDA #'- SEND CONTROL CHARACTERS AS '-' SIGNS
DSPCHR SWI3
 FCB OUTCHR
 CLRA
 ADDD #1 BUMP THE POINTER
 BITB #$F
 BNE NXTCL2 LOOP IF NOT END OF RON
 LEAX ASCEND,PCR
 SWI3
 FCB PSTRNG FINISH UP
 PULS A,B RECOVER POINTER TO NEXR BYTE
 CMPD BYTCNT,PCR SEE IF FINISHED
 LBLO NXTROW LOOP IF NOT
CRLF PSHS X
 LEAX CRLFST,PCR
 SWI3
 FCB PSTRNG
 PULS X,PC FINISHED !!!
 SPC 2
* GETCHR IS CALLED WITH 'Y' POINTING TO A SPRING OF
* CHARACTERS ENDING WITH 0. IT WILL WAIT FOR THE
* USER TO PRESS ONE OF THE KEYS IN THAT STRING.
* THE CHARACTER WILL BE RETURNED IN 'A'
* THE POSITION OF THE CHARACTER IN THE STRING WI LL
*  BE RETURNED IN B. THE NUMBER WILL RANGE FROM
*  0 THRU LIMIT.
 SPC 1
GETCHR LDX [CEDCB,PCR]
 PSHS X SAVE OLD VALUE OF ECHO VECTOR
 LDX #0
 STX [CEDCB,PCR] TURN ECHO OFF
 PSHS Y SAVE POINTER TO DATA
GETCH1 SWI3
 FCB INCHR GET A CHARACTER 
 LDY ,S POINT TO DATA
 LDB #-1
GETCH2 TST ,Y SEE IF END OF DATA
 BEQ GETCH1 WAIT FOR ANOTHER IF SO
 INCB BUMP COUNTER
 CMPA ,Y+ SEE IF MATCH
 BNE GETCH2 LOOP IF NOT
 PULS Y
 PULS X
 STX [CEDCB,PCR] RESTORE OLD ECHO VECTOR
 RTS
 SPC 2
GETDRV LEAX GDVMSG,PCR
 SWI3
 FCB PSTRNG
GTDRV1 LEAY DRVCHR,PCR ALLOW ONLY 1234 AS INPUT
 LBSR GETCHR
 SUBA #'0 CONVERT TO HEX
 STA DRIVE,PCR STORE DRIVE NUMBER
 RTS
 SPC 2
GETSEC TST ERRFLG,PCR DO NOT ERASE ON ERROR
 BNE *+4
 BSR ERASE
 LDD CURSEC,PCR GET CURRENT SECTOR
 BEQ GTSEC1 BRANCH IF NONE
 LEAX LSCMSG,PCR
 SWI3
 FCB PSTRNG DISPLAY LAST' SECTOR MESSAGE
 LDD CURSEC,PCR GET CURRENT SECTOR
 LBSR OUT3DC DISPLAY CURRENT SECTOR NUMBER
GTSEC1 LBSR CRLF
 LBSR PDRIVE PRINT DRIVE NUMBER
 LEAX GSCMSG,PCR
 SWI3
 FCB PSTRNG DISPLAY GET SECTOR MESSAGE
 LEAX LINBUF,PCR POINT TO LINE ENTRY BUFFER
 LDB #4 ALLON 3 CHARACTERS ON ENTRY
 SWI3
 FCB GETLIN GET INPUT LINE
 BEQ GTSEC1 BRANCH ON LINE CANCELLATION
 CMPA #$D CHECK FOR BLANK LINE
 LBEQ RETURN EXIT ON BLANK LINE
 SWI3
 FCB SKPSPC SKIP SPACES
 SWI3
 FCB DECNUM GET DECIMAL NUMBER FROM LINE
 STD CURSEC,PCR SAVE SECTOR NUMBER
 CLR ERRFLG,PCR
 RTS
 SPC 2
ERROR SWI3
 FCB RPTERR REPORT DISK ERROR
 LEAX ERRMSG,PCR
 SWI3
 FCB PSTRNG SEND EDITOR ERROR MESSAGE
 STB ERRFLG,PCR
 LBRA CRLF
 SPC 2
ERASE PSHS A,X
 LEAX ERSMSG,PCR
 SWI3
 FCB PSTRNG SEND IT
 PULS A,X,PC
 SPC 2
WRTSEC LDD #$444B LOOK FOR DCB 'DK'
 SWI3
 FCB LOCDCB
 LDB DRIVE,PCR GET DRIVE NUMBER
 STB DCBDRN,X STORE DRIVE NUMBER IN DCB
 LEAY DATA,PCR POINT TO DATA BUFFER
 STY DCBBUF,X SET BUFFER POINTER IN DCB
 LDD CURSEC,PCR GET SECTOR NUMBER
 STD DCBBLK,X STORE IN DCB
 LDD BAKLNK,PCR GET REVERSE SECTOR LINK
 STD DCBPRV,X AND PUT IN DCB
 LDD FWDLNK,PCR GET FORWAPD LINK
 STD DCBNXT,X AND PUT IN DCB
 LDD BYTCNT,PCR GET BYTE COUNT
 STB DCBCNT,X AND PUT IN DCB
 LDB #DSKWV SET FOR WRITE
 SWI3
 FCB REQIO WRITE THE SECTOR
 LDB DCBERR,X
 BEQ WTSEC1
 SWI3
 FCB RPTERR REPORT DISK ERROR
 TFR U,S
WTSEC1 RTS
 SPC 2
RDSEC LDD #$444B LOOK FOR DCB ,DK,
 SWI3
 FCB LOCDCB
 LDB DRIVE,PCR GET DRIVE NUMBER
 STB DCBDRN,X PUT IT IN DCB
 LDD CURSEC,PCR GET CURRENT SECTOR
 STD DCBBLK,X PUT IT IN BOB
 LEAY DATA,PCR POINT TO DATA BUFFER
 STY DCBBUF,X PUT IT IN BOB
 LDB #DSKRD SET UP FOR READ
 SWI3
 FCB REQIO
 LDD DCBPRV,X GET BACK LINK
 STD BAKLNK,PCR STORE IT
 LDD DCBNXT,X GET FORNARD LINK
 STD FWDLNK,PCR STORE IT
 CLRA
 LDB DCBCNT,X GET BYTE COUNT
 BNE *+3
 INCA ADJUST FOR 256
 STD BYTCNT,PCR STORE BYTE COUNT
 LDB #0
 STB MODFLG,PCR CLEAR MODFLG
 LDB DCBTYP,X GET SECTOR TYPE
 STB FILTYP,PCR STORE IT
 LDB DCBERR,X GET ERROR STATUS
 RTS
 SPC 2
PDRIVE LEAX DVMSG,PCR POINT TO DRIVE STRING
 SWI3
 FCB PSTRNG SEND IT TO 'CN'
 LDA DRIVE,PCR GET DRIVE NUMBER
 LBSR OUT1DG
 SPC 1
SPACER LDA #SPACE
 SPC 1
OUTPUT SWI3
 FCB OUTCHR
 RTS
OUT3DC PSHS Y
 LDY #0
OUT3D1 SUBD #100
 BMI OUT3D2
 LEAY $100,Y
 BRA OUT3D1
OUT3D2 ADDD #100 RESTORE
OUT3D3 SUBD #10
 BMI OUT3D4
 LEAY $10,Y
 BRA OUT3D3
OUT3D4 ADDD #10
 LEAY D,Y
 TFR Y,D
 BSR OUT1DG
 TFR B,A
 SWI3
 FCB DSPSBY
 PULS Y,PC
OUT1DG ANDA #$F
 ADDA #'0
 LBRA OUTPUT
 SPC 2
MODIFY LDA #-1
 STA MODFLG,PCR FLAG SECTOR AS MODIFIED
 LBSR CRLF
 LEAX MODMSG,PCR POINT TO MOD CMD QUERY
 SWI3
 FCB PSTRNG SEND STRING F0 'CN-0
 SWI3
 FCB GETHEX GET ADDRESS TO MODIFY
 CMPA #'> CHECK FOR MASTER ALTERATION
 BEQ MSTRMD
 TFR X,D AND PUT IT IN 'B'
MODL1 LBSR CRLF
 CLRA
 CMPD BYTCNT,PCR
 BLO *+3 SKIP IF NOT PAST END OF SECTOR
 CLRB RESET TO FIRST ELEMENT ON OVERFLOW
 TFR B,A
 SWI3
 FCB DSPSBY DISPLAY RELATIVE LOCATION
 LEAX ML2MSG,PCR POINT TO PRESENT MESSAGE
 SWI3
 FCB PSTRNG SEND IT TO 'CN,
 LEAY DATA,PCR POINT TO DATA
 CLRA
 LEAY D,Y
 LDA ,Y GET DATA BYTE POINTED TO BY JB,
 SWI3
 FCB DSPSBY DISPLAY DATA BYTE
 LEAX ML3MSG,PCR POINT TO NEW MESSAGE
 SWI3
 FCB PSTRNG SEND IT TO 'CN-'
 PSHS B SAVE 'B'
 SWI3
 FCB GETHEX GET HEX NUMBER FROM 'CN'
 PSHS A SAVE TERMINATOR
 BEQ MODL2 SKIP IF NO DIGITS ENTERED
 TFR X,D
 STB ,Y STORE NEN VALUE
MODL2 PULS A,B GET 'AJ & 'B'
 CMPA #CR WAS TERMINATOR A CR?
 BEQ MODLX EXIT IF SO
 CMPA #'' CHECK FOR ASCII DECLARATION
 BNE MODL3 SKIP IF NOT
 SWI3
 FCB INCHR GET CHARACTER
 STA ,Y
 BRA MODL4 BUMP LOCATION COUNTER
MODL3 CMPA #'^
 BNE MODL4 SKIP IF NOT BACKUP
 DECB
 BRA MODL1
MODL4 INCB
 BRA MODL1
MSTRMD LEAY XTMDCH,PCR ALLOW INPUT OF 'FBCT' ONLY
 LBSR GETCHR
 SWI3
 FCB OUTCHR PRINT CHARACTER
 PSHS B
 LBSR SPACER
 SWI3
 FCB GETHEX GET NEW VALUE
 PULS B
 DECB
 BPL MSTR1 BRANCH IF NOR FOTNARD LINK
 STX FWDLNK,PCR
 BRA MODLX
MSTR1 DECB
 BPL MSTR2 BRANCH IF NOT BACK LINK
 STX BAKLNK,PCR
 BRA MODLX
MSTR2 DECB
 BPL MSTR3 BRANCH IF NOT BYTECOUNT
 CMPX #256 BE SURE NOT OVER 256
 BHI MODLX
 CMPX #0 ZERO NOT ALLOWED
 BEQ MODLX
 STX BYTCNT,PCR
 BRA MODLX
MSTR3 DECB
 BPL MODLX
 TFR X,D
 STB FILTYP,PCR
MODLX RTS EXIT FROM MODIFY ROUTINE
 SPC 1
* MESSAGES AND STRINGS
 SPC 2
GSCMSG FCC 'EDIT WHICH'
SCTMSG FCC ' SECTOR:'
 FCB TERMSP
FWDMSG FCC ' FORWARD:'
 FCB TERMSP
BAKMSG FCC ' BACK:'
 FCB TERMSP
BYTMSG FCC ' BYTES:'
 FCB TERMSP
TYPMSG FCC ' TYPE:'
 FCB TERMSP
UPDQRY FCB CR,LF
 FCC '=== WRITE NEV VERSION TO DISK? (Y,N)'
 FCB TERMSP
GDVMSG FCC 'PERCOM LFD-400/MPX-9 DISK EDITOR VERSION 1.00'
 FCB CR,LF,CR,LF
 FCC '>> ENTER '
DVMSG FCC 'DRIVE:'
 FCB TERMSP
DIVMSG FCB $D,$A
 FCC 'OUTPUT TO (CR FOR CURRENT CONSOLE) '
 FCB '*+$80
LSCMSG FCC 'LAST SECTOR WAS NUMBER'
 FCB TERMSP
ERRMSG FCC 'UNABLE TO READ THIS SECTOR'
 FCB '.+$80
ERSMSG FCB CR+$80,0,0,0,0,0,0,0
MODMSG FCC '>> MODIFY LOCATIONS:'
 FCB TERMSP
ML2MSG FCC 'CONTAINS'
 FCB TERMSP
ML3MSG FCC '- NEW VALUE:'
 FCB TERMSP
 SPC 1
RWMSG FCB CR,LF,CR,LF
 FCC 'ROW  0  1  2  3  4  5  6  7  8  9'
 FCC '  A  B  C  D  E  F !------ASCII-----!'
 FCB CR,LF
CRLFST FCB CR,LF+$80
 SPC 1
ASCEND FCB '!,CR,LF+$80
 SPC 1
CMDMSG FCC '>>> COMMAND (F,B,R,+,-,M,X,N):'
 FCB TERMSP
 SPC 1
CMDCHR FCB 'F,'B,'+,'-,'R,'X,'N,'M,0
 SPC 1
YNCHR FCB 'Y,'N,0
 SPC 1
DRVCHR FCB '1,'2,'3,'4,0
 SPC 1
XTMDCH FCB 'F,'B,'C,'T,0
 SPC 2
ENDCOD EQU *-1
 SPC 3
* VORK1NG VARIABLES
 SPC 2
CODCB RMB 2
CEDCB RMB 2
BAKLNK RMB 2 REVERSE SECTOR LINKAGE 
FWDLNK RMB 2 FORWARD SECTOR LINKAGE 
BYTCNT RMB 2 BYTE COUNT FOR CURRENT SECTOR
CURSEC RMB 2 CURRENT SECTOR NUMBER
DRIVE RMB 1 DRIVE NUMBER
DATA RMB 256 DATA BUFFER
MODFLG RMB 1
ERRFLG RMB 1
FILTYP RMB 1 FILE TYPE
DIVFLG RMB 1
LINBUF RMB 10 LINE INPUT BUFFER
 SPC 2
ENDPGM EQU *-1
