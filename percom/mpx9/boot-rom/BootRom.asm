 NAM MDOS9
 TTL MINIDOS/9 (tm) - 6809 DISK DRIVER
 SPC 1
**************************************************
* MINIDOS/9 VERSION 1.00                         *
* 6809 DISK DRIVER FOR THE PERCOM LFD-400 AND    *
* LFD-400 EX FLOPPY DISK SYSTEMS                 *
*                                                *
* COPYRIGHT (c) 1979 PERCOM DATA CO. INC.        *
* ALL RIGHTS RESERVED                            *
*                                                *
* WRITTEN BY JAMES W. STUTSMAN                   *
*                                                *
* THIS PROM PROVIDES THE LOW-LEVEL DISK DRIVERS  *
* FOR DISK I/O.  TWO ENTRY POINTS ARE PROVIDED.  *
* THE FIRST, INTENDED FOR USE IN CONJUNCTION     *
* WITH PERCOM'S PSYMON (tm) MONITOR, ALLOWS THE  *
* USER TO INITIALIZE MINIDOS/9 AND THEN LOAD THE *
* PERCOM MPX/9 DISK OPERATING SYSTEM.  THE       *
* SECOND ENTRY POINT SIMPLY INITIALIZES THE      *
* MINIDOS/9 RAM AREAS AND RETURNS TO THE CALLER. *
*                                                *
* ASIDE FROM THE TWO INITIALIZATION ENTRY POINTS *
* ALL I/O COMMANDS TO MINIDOS/9 ARE EXECUTED     *
* USING A DEVICE CONTROL BLOCK (DCB).  USERS OF  *
* MINIDOS/9 THAT ARE NOT USING THE PSYMON (tm)   *
* MONITOR SHOULD FOLLOW THE FOLLOWING            *
* CONVENTIONS:                                   *
* ---> FOR ALL I/O THE X REGISTER MUST POINT TO  *
*      THE DISK DCB.  THE B REGISTER CONTAINS A  *
*      FUNCTION CODE, WHILE A IS USED FOR AN     *
*      OPTIONAL PARAMETER.  FUNCTION CODES ARE:  *
*         1 - READ A DISK BLOCK                  *
*         2 - WRITE A DISK BLOCK                 *
*         3 - WRITE & VERIFY A DISK BLOCK        *
*         4 - SELECT DRIVE & RETURN STATUS       *
*         8 - SCAN FOR ACTIVE DRIVES             *
* ---> THE DRIVER SHOULD BE CALLED USING THE     *
*      ADDRESS IN DCBDVR.                        *
* ---> THE DRIVER RETURNS WITH X STILL POINTING  *
*      AT THE DCB.  THE ADDRESS OF THE DDT ENTRY *
*      FOR THE SELECTED DRIVE IS RETURNED IN     *
*      DCBIOA.  THE CALLER IS RESPONSIBLE FOR    *
*      SAVING ANY REGISTERS WHICH MUST BE        *
*      PRESERVED.                                *
* ---> ON RETURN THE DRIVER WILL STORE A STATUS  *
*      IN DCBERR, INDICATING THE OUTCOME OF THE  *
*      I/O OPERATION.  THE ERROR CODES USED ARE  *
*      AS FOLLOWS:                               *
*          0 - SUCCESSFUL OPERATION, NO ERROR    *
*          1 - ILLEGAL FUNCTION CODE IN B        *
*          2 - ILLEGAL DRIVE NUMBER IN DCB       *
*          3 - ILLEGAL BLOCK NUMBER IN DCB       *
*          4 - DISK MISSING OR DOOR OPEN         *
*          5 - EMPTY BLOCK, NO DATA TO READ      *
*          6 - SEEK ERROR                        *
*          7 - READ ERROR                        *
*          8 - VERIFY ERROR (AFTER WRITE)        *
*          9 - DISK IS WRITE PROTECTED           *
*                                                *
*                                                *
* INITIAL RELEASE 05/12/80                       *
*                                                *
**************************************************
 PAGE
**************************************************
* RELATIVE ADDRESS OFFSETS                       *
**************************************************
M9ROM EQU $C000 PROM BASE ADDRESS (RELATIVE)
M9IO EQU 3*1024 BASE ADDRESS OF I/O REGISTERS
M9RAM EQU 0 RAM OFFSET IN FREE RAM
 SPC 1
**************************************************
* PSYMON (tm) VECTORS                            *
**************************************************
PSYRAM EQU $FFDE BASE ADDRESS OF PSYMON RAM
FRERAM EQU 126 OFFSET FOR FREE RAM VECTOR
MONVEC EQU $FFEE MONITOR ENTRY VECTOR
SWI3V EQU 114 SWI3 VECTOR OFFSET
 SPC 1
* PSYMON ROUTINE REFERENCES
MONITR EQU 0 RETURN TO MONITOR
REQIO EQU 1 REQUEST I/O
OUTCHR EQU 2 OUTPUT CHARACTER
INCHR EQU 3 INPUT CHARACTER
PSTRNG EQU 4 PRINT STRING
GETHEX EQU 5 GET HEX NUMBER
DSPDBY EQU 6 DISPLAY DOUBLE BYTE
DSPSBY EQU 7 DISPLAY SINGLE BYTE
 SPC 1
**************************************************
* MINIDOS DCB DEFINITION                         *
**************************************************
* PART 1 - BASIC DCB DATA
DCBLNK EQU 0 POINTER TO NEXT DCB
DCBDID EQU 2 ASCII ID ('DK' FOR DISK)
DCBDVR EQU 4 DEVICE DRIVER ADDRESS
DCBIOA EQU 6 DEVICE I/O ADDRESS
DCBERR EQU 8 ERROR STATUS CODE
DCBEXT EQU 9 DCB EXTENSION BYTE COUNT
* PART 2 - DEVICE-DEPENDENT DATA
DCBDRV EQU 10 DISK DRIVE # (1-4)
DCBBLK EQU 11 RELATIVE BLOCK #
DCBBUF EQU 13 BUFFER ADDRESS
* PART 3 - DRIVER WORK SPACE
DCBTRK EQU 15 DESIRED TRACK NUMBER
DCBSEC EQU 16 DESIRED SECTOR NUMBER
DCBCUR EQU 15 CURRENT TRACK/SECTOR
DCBPRV EQU 17 PREVIOUS TRACK/SECTOR
DCBNXT EQU 19 NEXT TRACK/SECTOR
DCBCNT EQU 21 BYTE COUNT
DCBADD EQU 22 DATA ADDRESS
DCBTYP EQU 24 FILE TYPE CODE
DCBCRC EQU 25 CHECKSUM
 SPC 1
DCBSIZ EQU 32 MAXIMUM DCB SIZE
 SPC 1
**************************************************
* MINIDOS/9 RAM DEFINITIONS (128 BYTES MAX)      *
**************************************************
 ORG M9RAM
 SPC 1
* DRIVE DESCRIPTION TABLE
DDT RMB 32
* ONE ENTRY PER DRIVE DEFINED AS FOLLOWS:
CURTRK EQU 0 CURRENT TRACK (HEAD POSITION)
NUMTRK EQU 1 NUMBER OF TRACKS FOR THIS DRIVE
NUMSEC EQU 2 NUMBER OF SECTORS PER TRACK
STEPTM EQU 3 STEP TIME (IN MSEC) FOR THIS DRIVE
SETLTM EQU 4 SETTLE TIME (IN MSEC) FOR THIS DRIVE
SILTBL EQU 5 ADDRESS OF SECTOR INTERLACE TABLE
* ONE BYTE RESERVED FOR EXPANSION
 SPC 1
* MINIDOS DISK DCB
DKDCB RMB DCBSIZ RESERVE SPACE FOR DISK DCB
 SPC 1
SYSVEC RMB 2 SYSTEM CALL VECTOR
 SPC 1
**************************************************
* LFD-400 DISK CONTROLLER REGISTER OFFSETS       *
**************************************************
 ORG M9IO BASE ADDRESS OF I/O
* INPUT PORTS
STATUS RMB 1 CONTROLLER STATUS
RDDATA RMB 1 READ DATA
SECCNT RMB 1 SECTOR COUNTER
DRSTAT RMB 1 DRIVE STATUS
RESTRT RMB 1 RECEIVER RESTART
MTRON RMB 1 MOTOR ON
MTROFF RMB 1 MOTOR OFF
 SPC 1
 ORG M9IO
* OUTPUT PORTS
SYNC RMB 1 SYNC REGISTER
WTDATA RMB 1 WRITE DATA
FILL RMB 1 FILL WORD REGISTER
DRVSEL RMB 1 DRIVE SELECT REGISTER
WRITON RMB 1 WRITE CONTROL REGISTER
 SPC 1
* DISK DRIVE STATUS BITS
WTPROT EQU $01 WRITE PROTECT
TRACK0 EQU $02 TRACK ZERO
MOTOR EQU $04 MOTOR RUNNING
WTGATE EQU $08 WRITE GATE
SECTOR EQU $10 SECTOR PULSE
INDEX EQU $20 INDEX PULSE
INPRDY EQU $01 INPUT READY BIT
 SPC 1
;jns+ 
CRLF	equ $FDA2
DspSBy 	equ	$FD73  	; DISPLAY SINGLE BYTE ON CONSOLE
DspDBy	equ $FD6A	; DISPLAY DOUBLE BYTE ON CONSOLE
GetHex  equ $FD0E	; GET HEX NUMBER FROM CONSOLE
PString	equ $FD97   ; PRINT STRING TO CONSOLE
InChr	equ	$FD44   ; INPUT CHARACTER FROM CONSOLE
OutChr  equ $FD58   ; OUTPUT CHARACTER TO CONSOLE
ReqIO   equ $FD63   ; PERFORM I/O REQUEST
MonEnt  equ $FC32   ; MONITOR RE-ENTRY
;jns-
 SETDP M9IO/256 USE DIRECT PAGE FOR I/O
 PAGE
 ORG M9ROM
 SPC 1
**************************************************
* MINIDOS/9 ENTRY VECTORS                        *
**************************************************
 LBRA BOOT BOOTSTRAP ENTRY
 LBRA INIT INITIALIZATION ENTRY
 SPC 1
**************************************************
* MINIDOS/9 ENVIRONMENTAL PARAMETERS             *
**************************************************
DFAULT FCB 40,10,25,10 DRIVE 1 DEFAULTS
 FCB 40,10,25,10 DRIVE 2 DEFAULTS
 FCB 40,10,25,10 DRIVE 3 DEFAULTS
 FCB 40,10,25,10 DRIVE 4 DEFAULTS
* STANDARD SECTOR INTERLACE TABLE
STDSIL FCB 0,2,4,6,8,1,3,5,7,9
 SPC 1
**************************************************
* BOOTSTRAP MODULE                               *
*                                                *
* ENTRY REQUIREMENTS:  CALLED VIA 'G' COMMAND    *
*                        FROM PSYMON             *
*                                                *
* EXIT CONDITIONS:  EXITS TO MPX/9 IF BOOT READ  *
*                   REPORTS ERROR & RETURNS TO   *
*                     PSYMON IF NOT              *
*                                                *
**************************************************
BOOT LDY PSYRAM POINT Y AT PSYMON RAM
 LEAX SYCALL,PCR POINT X AT SYS CALL HANDLER
 STX SWI3V,Y SET SWI3 VECTOR
 LDY FRERAM,Y GET FREE RAM ADDRESS IN Y
 LDD MONVEC PLUG EXTERNAL CALL VECTOR
 STD SYSVEC,Y
BOOT1 BSR INIT INITIALIZE MINIDOS/9 RAM
 LDB #1 SET UP READ FUNCTION CODE
 STB DCBDRV,X SET UP DRIVE=1
 STB DCBBUF,X SET UP ADDRESS=0100
 SWI3 READ THE DISK BOOT
 FCB REQIO
 TST DCBERR,X CHECK OUTCOME OF READ
 BEQ BOOTX GO IF GOOD
BOOT2 PSHS X SAVE DCB POINTER
 LEAX <BTERR,PCR REPORT BOOT ERROR
 SWI3
 FCB PSTRNG
 PULS X RESTORE DCB POINTER
 LDA DCBERR,X GET THE ERROR CODE
 SWI3 DISPLAY IT
 FCB DSPSBY
 SWI3 RETURN TO MONITOR
 FCB MONITR
 LDY DCBIOA,X POINT Y AT MINIDOS/9 RAM
 BRA BOOT1 TRY THE BOOT AGAIN
BOOTX LDY DCBIOA,X POINT Y AT MINIDOS RAM
 TFR PC,D PUT PC IN D FOR SELF-LOCATION
 ANDA #$F0 GET 4K BOUNDARY ADDRESS
 CLRB
 TFR D,U PUT IN U
 JSR [DCBBUF,X] EXIT TO BOOT ROUTINE
 BRA BOOT2 GO IF ERROR
 SPC 1
BTERR FCB $0D,$0A
 FCC '*** DISK ERROR'
 FCB $A0
 SPC 1
**************************************************
* INITIALIZATION MODULE                          *
*                                                *
* ENTRY REQUIREMENTS:  Y POINTS TO RAM TO BE     *
*                        USED BY MINIDOS/9 (128  *
*                        BYTES MINIMUM)          *
*                                                *
* EXIT CONDITIONS:  X POINTS TO MINIDOS/9 DCB    *
*                   CC CHANGED                   *
*                   ALL OTHERS PRESERVED         *
*                                                *
**************************************************
INIT PSHS A,B,Y,U SAVE WORK REGISTERS
 LEAX DFAULT,PCR POINT X TO DEFAULTS
 LEAU STDSIL,PCR POINT U TO SILTBL
 LDB #4 SET LOOP COUNT ON STACK
 STB ,-S
INIT1 LDA #$FF RESET TRACK HISTORY
 STA ,Y+
 LDD ,X++ SET TRACK, SECTOR LIMITS
 STD ,Y++
 LDD ,X++ SET STEP, SETTLE TIMES
 STD ,Y++
 STU ,Y++ SET SIL TABLE ADDRESS
 CLR ,Y+ CLEAR EXPANSION BYTE
 DEC ,S DECREMENT LOOP COUNT
 BNE INIT1
 LEAS 1,S CLEAN UP THE STACK
 TFR Y,X X NOW POINTS TO DKDCB
 LDB #DCBSIZ SET LOOP COUNT
INIT2 CLR ,Y+ CLEAR DCB TO 0
 DECB
 BNE INIT2

; LDD $F3E8
; STD DCBLNK,X
; STX $F3E8

 LDD #$444B PLUG ID ('DK')
 STD DCBDID,X
 LEAU <DISKDR,PCR PLUG DRIVER ADDRESS
 STU DCBDVR,X
 LDA #DCBSIZ-10 PLUG EXTENSION COUNT
 STA DCBEXT,X
 LDY 2,S GET RAM POINTER OFF STACK
 STY DCBIOA,X SAVE IN DCB
 PULS A,B,Y,U,PC RESTORE & EXIT
 SPC 1
**************************************************
* DISK DRIVER EXECUTIVE                          *
*                                                *
* ENTRY REQUIREMENTS:  X POINTS TO DCB           *
*                      B CONTAINS FUNCTION       *
*                      A CONTAINS PARAMETER      *
*                      CALLED VIA DCBDVR         *
*                                                *
* EXIT CONDITIONS:  A & CC CHANGED               *
*                   DCB CONTAINS OUTCOME         *
*                                                *
**************************************************
DISKDR LDY DCBIOA,X POINT Y TO MINIDOS RAM
 PSHS A,B SAVE FUNCTION BYTES
 TFR PC,D GET CURRENT LOCATION
 ANDA #$F0 SAVE 4K BOUNDARY
 CLRB
 ADDD #M9IO ADD IN BIAS FOR I/O
 TFR A,DP POINT DP TO BASE OF I/O
 LDA #1 SET UP ERROR CODE (1)
 STA DCBERR,X
 LDA #8 SET BIT MASK
 LEAU <DIOTBL,PCR POINT TO BRANCH TABLE
DISKD1 BITA 1,S CHECK BIT SETTING
 BNE DISKD2 GO IF SET
 LEAU 3,U MOVE TO NEXT ENTRY
 LSRA SHIFT BIT MASK
 BNE DISKD1 LOOP IF NOT ZERO
DISKD2 PULS A,B RESTORE CALL PARAMS
 JMP ,U EXIT TO DRIVER ROUTINE
 SPC 1
**************************************************
* DISK DRIVER BRANCH TABLE                       *
**************************************************
DIOTBL LBRA CTLFNC FUNCTION 8 - CONTROL
 LBRA SELECT FUNCTION 4 - SELECT DRIVE
 LBRA WTSEC FUNCTION 2 - WRITE (WITH OPTIONAL VERIFY)
 LBRA RDSEC FUNCTION 1 - READ SECTOR
 RTS INVALID FUNCTION TRAP
 SPC 1
**************************************************
* SELECT DRIVE:                                  *
*   CHECK PARAMETERS                             *
*   START MOTOR                                  *
*   SEEK TRACK                                   *
*   RETURN STATUS IN A                           *
**************************************************
SELECT INC DCBERR,X UPDATE ERROR CODE (2)
 LDA DCBDRV,X GET DRIVE #
 CMPA #4 IS IT > 4?
 BHI SELERR EXIT IF YES
 DECA GET DRIVE # - 1
 LDB #8 DDT ENTRIES 8 BYTES EACH
 MUL GET OFFSET INTO DDT
 LEAY D,Y POINT Y AT DDT ENTRY
 INC DCBERR,X UPDATE ERROR CODE (3)
 LDA NUMSEC,Y PUT 2 BYTE SECTOR COUNT ON STACK
 STA ,-S
 CLR ,-S
 CLR DCBTRK,X CLEAR TRACK COUNTER
 LDD DCBBLK,X GET RELATIVE BLOCK #
SELCT1 INC DCBTRK,X UPDATE TRACK COUNTER
 SUBD ,S SUBTRACT ONE TRACK
 BPL SELCT1 LOOP IF POSITIVE
 ADDD ,S++ CORRECT REMAINDER
 DEC DCBTRK,X CORRECT TRACK #
 LDA DCBTRK,X CHECK TRACK #
 CMPA NUMTRK,Y BIGGER THAN MAX?
 BHS SELERR GO IF YES
 CMPB NUMSEC,Y SECTOR BIGGER THAN MAX?
 BHS SELERR GO IF YES
 LDU SILTBL,Y POINT U AT SIL TABLE
 LDA B,U GET ACTUAL SECTOR #
 STA DCBSEC,X SAVE IT
 LDA <DRSTAT GET DRIVE STATUS
 ANDA #$C0 MASK OFF DRIVE BITS
 ASLA MOVE TO LS POSITION
 ROLA
 ROLA
 BNE SELCT2 GO IF NOT ZERO
 LDA #4 CONVERT 0 TO 4
SELCT2 CMPA DCBDRV,X SAME DRIVE AS BEFORE?
 BNE SELCT3 GO IF NOT
 LDA CURTRK,Y GET CURRENT TRACK
 INCA CHECK INIT STATUS
 BNE SELCT4 GO IF INITIALIZED
SELCT3 LDA DCBDRV,X GET REQUESTED DRIVE
 ANDA #3 MASK OFF LS BITS
 LSRA MOVE TO MS POSITION
 RORA
 RORA
 STA <DRVSEL SELECT NEW DRIVE
 LDD #$4000 WAIT FOR AT LEAST 1 REVOLUTION
 BSR DRVTST CHECK NEW DRIVE
 BNE SELERR GO IF ERROR
 LDA CURTRK,Y IS DRIVE ON LINE?
 INCA
 BNE SELCT4 GO IF YES
 LBSR RESTOR RESTORE THE DRIVE
SELCT4 BSR STARTM START THE MOTOR
 LBSR SEEK SEEK THE TRACK
 LDA <DRSTAT GET DRIVE STATUS
 ANDA #WTPROT MASK OFF WRITE PROTECT
 ASLA MOVE TO WRITE STATUS BIT
 INCA SET READ STATUS BIT
 CLR DCBERR,X RESET ERROR CODE (0)
 RTS
SELERR LDA #$80 REPORT DRIVE INOP
 RTS
 SPC 1
**************************************************
* DRIVE READY CHECK                              *
**************************************************
DRVTST PSHS A,B,X SAVE REGISTERS
 BSR STARTM START MOTOR
 LDX ,S DELAY TIME TO X
 LDB #11 WAIT FOR 11 SECTOR HOLES
 LDA #SECTOR SECTOR BIT MASK
DRVTS1 BITA <DRSTAT SECTOR PULSE=0?
 BEQ DRVTS2 GO IF YES
 LEAX ,-X DECREMENT TIMER
 BNE DRVTS1 LOOP IF NOT ZERO
 BRA DRVTS3
DRVTS2 BITA <DRSTAT SECTOR PULSE=1?
 BNE DRVTS4 GO IF YES
 LEAX ,-X DECREMENT TIMER
 BNE DRVTS2 LOOP IF NOT ZERO
DRVTS3 LDX 2,S RESTORE X (DCB)
 BSR DOWNDR MARK DRIVE DOWN
 BRA DRVTS5
DRVTS4 DECB DECREMENT SECTOR COUNT
 BNE DRVTS1 LOOP IF NOT ZERO
 ORCC #4 SET FOR NO ERROR
DRVTS5 PULS A,B,X,PC RESTORE X & EXIT
 SPC 1
**************************************************
* START DRIVE MOTOR, DELAY IF NOT RUNNING        *
**************************************************
STARTM BSR RETRIG TRIGGER MOTOR & GET STATUS
 BITA #MOTOR MOTOR RUNNING?
 BEQ RETRIG EXIT IF YES
 LDD #1000 DELAY 1 SECOND
 BSR DELAY
 SPC 1
**************************************************
* RETRIGGER MOTOR AND RETURN DRIVE STATUS        *
**************************************************
RETRIG LDA <DRSTAT GET DRIVE STATUS
 TST <MTRON RETRIGGER MOTOR
STARTX RTS
 SPC 1
**************************************************
* RESTORE THE DRIVE                              *
**************************************************
RESTOR BSR STPIN STEP IN 3 TIMES
 BSR STPIN
 BSR STPIN
RSTOR1 BSR STPOUT STEP HEAD OUT 1 TRACK
 LDA <DRSTAT GET DRIVE STATUS
 BITA #TRACK0 AT TRACK ZERO?
 BNE RSTOR1 LOOP IF NOT THERE
 CLR CURTRK,Y RESET CURRENT TRACK
 BRA SETTLE ALLOW HEAD TO SETTLE
 SPC 1
**************************************************
* SEEK DESIRED TRACK                             *
**************************************************
SEEK LDA DCBCUR,X GET DESIRED TRACK
 SUBA CURTRK,Y CALCULATE OFFSET
 BEQ STARTX EXIT IF ON TRACK
 PSHS A SAVE OFFSET ON STACK
 BMI SEEK2 GO IF NEGATIVE OFFSET
SEEK1 BSR STPIN MOVE HEAD IN 1 TRACK
 INC CURTRK,Y UPDATE HEAD POSITION
 DEC ,S DECREMENT COUNTER
 BNE SEEK1 LOOP IF NOT DONE
 BRA SEEK3
SEEK2 BSR STPOUT MOVE HEAD OUT 1 TRACK
 DEC CURTRK,Y UPDATE HEAD POSITION
 INC ,S INCREMENT COUNT
 BNE SEEK2 LOOP IF NOT DONE
SEEK3 LEAS 1,S CLEAN UP STACK
 SPC 1
**************************************************
* ALLOW HEAD TO SETTLE AFTER STEPPING            *
**************************************************
SETTLE CLRA SETTLE TIME TO D
 LDB SETLTM,Y
 BRA DELAY
 SPC 1
**************************************************
* STEP THE HEAD IN 1 TRACK                       *
**************************************************
STPIN BSR RETRIG RETRIGGER MOTOR, GET STATUS
 ANDA #$CF MASK OFF STEP BITS
 ORA #$10 SET DIRECTION
 BRA STEPIO GO STEP
 SPC 1
**************************************************
* STEP THE HEAD OUT 1 TRACK                      *
**************************************************
STPOUT BSR RETRIG RETRIGGER MOTOR, GET STATUS
 ANDA #$CF MASK OFF STEP BITS
 SPC 1
**************************************************
* STEP THE HEAD IN/OUT 1 TRACK                   *
**************************************************
STEPIO ORA #$20 SET STEP BIT
 STA <DRVSEL SET DIRECTION, START STEP PULSE
 ANDA #$DF MASK OUT STEP PULSE
 STA <DRVSEL RESET STEP PULSE (STEP BEGINS NOW)
 CLRA SET UP DELAY TIME IN D
 LDB STEPTM,Y
 SPC 1
**************************************************
* DELAY FOR DURATION IN D (IN MSEC)              *
**************************************************
DELAY PSHS A,B,X SAVE REGISTERS
 EXG D,X DELAY TIME TO X
DELAY1 LDA #198 SET 1 MSEC TIMER (ASSUME 1 MEG CLOCK)
DELAY2 DECA COUNT OUT 1 MSEC
 BNE DELAY2
 LEAX ,-X COUNT DOWN DELAY
 BNE DELAY1
 PULS A,B,X,PC RESTORE REGISTERS & EXIT
 SPC 1
**************************************************
* MARK DRIVE DOWN IN DDT                         *
**************************************************
DOWNDR LDA #$FF MARK DRIVE DOWN
 STA CURTRK,Y
 LDA #4 SET ERROR CODE
 STA DCBERR,X (4)
 RTS
 SPC 1
**************************************************
* READ A SECTOR WITH ERROR RECOVERY              *
**************************************************
RDSEC LBSR SELECT SELECT THE DRIVE
 BNE RDSECX GO IF ERROR
 BSR READX3 READ 3 TIMES
 BLS RDSECX GO IF GOOD OR DISK MISSING
 BSR STPIN JOG THE HEAD
 BSR STPOUT
 BSR SETTLE
 BSR READX3 READ 3 TIMES
 BLS RDSECX GO IF GOOD OR DISK MISSING
 BSR RESTOR RESTORE THE DRIVE
 BSR SEEK SEEK TRACK AGAIN
 SPC 1
**************************************************
* READ THREE TIMES                               *
**************************************************
READX3 BSR READ READ THE SECTOR
 BLS RDSECX GO IF GOOD OR DISK MISSING
 BSR READ TRY IT AGAIN
 BLS RDSECX GO IF GOOD OR DISK MISSING
 SPC 1
**************************************************
* READ A SECTOR                                  *
**************************************************
READ BSR GETSEC GET THE SECTOR
 BNE READX GO IF ERROR
 ANDB #$F0 RESET CONDITION BITS
 PSHS B SAVE CC STATE (NOW IN B)
 BSR RDSYNC FIND & READ THE SYNC BYTE
 BNE READ1 EXIT IF ERROR
 LDA #6 INITIALIZE ERROR CODE
 STA DCBERR,X (6)
 BSR RDBYTE GET TRACK #
 CMPA DCBTRK,X ON PROPER TRACK?
 BNE READ1 EXIT IF NOT
 INC DCBERR,X UPDATE ERROR CODE (7)
 BSR RDBYTE GET SECTOR #
 CMPA DCBSEC,X ON TARGET?
 BNE READ1 EXIT IF NOT
 LEAU DCBPRV,X SET UP TARGET ADDRESS
 LDB #DCBCRC-DCBPRV SET UP LENGTH
 BSR INPUT READ REMAINING HEADER
 LBSR GETBUF GET THE BUFFER ADDRESS
 LDB DCBCNT,X GET BYTE COUNT
 BSR INPUT READ THE SECTOR
 LEAU DCBCRC,X SET UP TO READ CRC
 LDB #2
 BSR INPUT READ CRC
 PULS CC RESTORE CC STATE
 LBSR CALCRC CALCULATE CRC
 CMPD DCBCRC,X DOES IT MATCH?
 BNE READX GO IF NOT
 CLR DCBERR,X RESET ERROR FLAG (0)
 BRA READX EXIT
READ1 PULS CC RESTORE CC REGISTER
READX TST DCBERR,X SET/RESET 'Z' BIT IN CC
RDSECX RTS
 SPC 1
**************************************************
* LOCATE & READ SYNC BYTE                        *
**************************************************
RDSYNC LDA #$FB SET SYNC BYTE
 STA <SYNC
 LDA #5 PRESET ERROR CODE
 STA DCBERR,X (5)
 LDA #36 DELAY 180 USEC (SECTOR GAP)
RDSYN1 DECA
 BNE RDSYN1
 LDA <RESTRT RESTART RECEIVER
 LDB #INPRDY SET UP BIT MASK
RDSYN2 LDA <SECCNT GET SECTOR COUNTER
 ANDA #$0F MASK OFF NUMBER
 CMPA DCBSEC,X STILL IN PROPER SECTOR?
 BNE RDSYNX EXIT IF NOT
 BITB <STATUS FOUND SYNC BYTE?
 BEQ RDSYN2 LOOP IF NOT
 LDA <RDDATA READ IT
 CLR DCBERR,X CLEAR ERROR (0)
RDSYNX RTS
 SPC 1
**************************************************
* LOAD DATA FROM DISK                            *
**************************************************
INPUT BSR RDBYTE READ NEXT BYTE
 STA ,U+ STORE IT
 DECB DECREMENT COUNT
 BNE INPUT
 RTS
 SPC 1
**************************************************
* READ A BYTE FROM DISK                          *
**************************************************
RDBYTE LDA #INPRDY SET STATUS MASK
RDBYT1 BITA <STATUS BYTE READY?
 BEQ RDBYT1 LOOP IF NOT
 LDA <RDDATA GET THE BYTE
 RTS
 SPC 1
**************************************************
* GET ON REQUIRED SECTOR FOR I/O                 *
**************************************************
GETSEC CLR DCBERR,X PRESET ERROR CODE (0)
 LDA DCBSEC,X GET ACTUAL SECTOR #
 BNE GTSEC1 GO IF NOT ZERO
 LDA NUMSEC,Y FORCE MAX SECTOR - 1
GTSEC1 DECA GET SECTOR N-1
 PSHS A,X SAVE N-1, X ON STACK
 TFR CC,B SAVE CC STATE IN B
 LDX #$8000 SET DELAY TIMER
GTSEC2 ORCC #$50 MASK OUT INTERRUPTS
 LBSR RETRIG RETRIGGER MOTOR, GET STATUS
 BITA #SECTOR IS ONE SHOT OFF?
 BEQ GTSEC4 GO IF NOT
 LDA <SECCNT GET CURRENT SECTOR COUNT
 ANDA #$0F MASK IT OFF
 CMPA ,S ON SECTOR N-1?
 BNE GTSEC4 GO IF NOT
 LDA #SECTOR SET UP MASK
 BITA <DRSTAT RECHECK ONE SHOT
 BEQ GTSEC4 GO IF ON
GTSEC3 BITA <DRSTAT IN SECTOR N YET?
 BNE GTSEC3 LOOP IF NOT
 ANDCC #$FE RESET CARRY BIT
 PULS A,X,PC RESTORE REGISTERS & EXIT
GTSEC4 TFR B,CC RESTORE INTERRUPT STATE
 LEAX ,-X DECREMENT DELAY COUNTER
 BNE GTSEC2 LOOP IF NO TIMEOUT
 PULS A,X RESTORE REGISTERS
 LBSR DOWNDR MARK DRIVE DOWN
 ORCC #$01 SET CARRY BIT
 RTS
 SPC 1
**************************************************
* GET BUFFER ADDRESS IN U                        *
**************************************************
GETBUF LDU DCBBUF,X GET BUFFER START ADDRESS
 CMPU #$FFFF IS IT VOID?
 BNE GTBUFX EXIT IF NOT
 LDU DCBADD,X USE HEADER ADDRESS
GTBUFX RTS
 SPC 1
**************************************************
* CALCULATE CRC FOR SECTOR:                      *
**************************************************
CALCRC BSR GETBUF POINT U TO DATA BUFFER
 CLRA INITIALIZE CRC ON STACK
 PSHS A
 LDB DCBCNT,X DATA LENGTH TO B
 BSR CRCRTN GET CRC BYTE
 LEAU DCBCUR,X POINT TO SECTOR HEADER
 LDB #DCBCRC-DCBCUR HEADER SIZE TO 10
 BSR CRCRTN GET CRC BYTE
 PULS B,PC GET RESULT & EXIT
 SPC 1
CRCRTN:
 ;JNS+ Debug code for makeing crc calc in mpx9dsk.py crc match the rom version in mpx9
 ; pshs a,b
 ; tfr u,d
 ; JSR DspDBy       ; JNS data address
 ; lda ,U 
 ; JSR DspSBy       ; JNS data byte
 ; lda ,s
 ; JSR DspSBy       ; JNS crc part
 ; lda 2+2,S
 ; JSR DspSBy       ; JNS other crc part
 ; JSR CRLF
 ; puls b,a
 ;JNS-
 EORA ,U+ CRC CALCULATION ROUTINE
 ASLA
 ROL 2,S
 BCC CRCRT1
 INCA
CRCRT1 DECB DECREMENT LOOP COUNT
 BNE CRCRTN LOOP UNTIL 0
 RTS
 SPC 1
**************************************************
* WRITE A SECTOR TO DISK                         *
**************************************************
WTSEC LSRB VERIFY REQUESTED?
 INCA
 BCC WTSEC1 GO IF NOT
 LEAU VERIFY,PCR SET UP AUTO CALL
 PSHS U
WTSEC1 LBSR SELECT SELECT THE DRIVE
 BNE WTSECX GO IF ERROR
 LDB #9 PRESET ERROR CODE
 STB DCBERR,X (9)
 BITA #WTPROT IS DISK WRITE PROTECTED? JNS was #$02
 BEQ WTSECX GO IF YES
 BSR CALCRC CALCULATE CRC FOR SECTOR
 STD DCBCRC,X
 LDA #$FF SET FILL WORD
 STA <FILL
 LBSR GETSEC GET THE SECTOR
 BNE WTSECX GO IF ERROR
 PSHS B SAVE CC STATE (IN B)
 CLR <WRITON TURN ON WRITE GATE
 LDB #16 SET LEADER COUNT
 CLRA USE NULL FOR LEADER
WTSEC2 BSR WTBYTE WRITE A LEADER BYTE
 DECB DECREMENT COUNTER
 BNE WTSEC2 LOOP UNTIL ZERO
 LDA #$FB WRITE SYNC BYTE
 BSR WTBYTE
 LEAU DCBCUR,X POINT U AT HEADER
 LDB #DCBCRC-DCBCUR LENGTH TO B
 BSR OUTPUT OUTPUT HEADER
 LBSR GETBUF POINT U AT BUFFER
 LDB DCBCNT,X LENGTH OF DATA TO B
 BSR OUTPUT OUTPUT THE DATA
 LEAU DCBCRC,X POINT U AT CRC
 LDB #2 LENGTH TO B
 BSR OUTPUT OUTPUT THE CRC
 PULS CC RESTORE CC STATE
WTSEC3 LBSR RETRIG RETRIGGER MOTOR, GET STATUS
 BITA #WTGATE WRITE GATE STILL ON?
 BEQ WTSEC3 LOOP IF YES
 CLR DCBERR,X RESET ERROR CODE (0)
WTSECX TST DCBERR,X SET/RESET 'Z' FLAG
 RTS
 SPC 1
**************************************************
* OUTPUT A BLOCK OF DATA TO DISK                 *
**************************************************
OUTPUT LDA ,U+ GET A BYTE
 BSR WTBYTE WRITE IT TO DISK
 DECB DECREMENT COUNT
 BNE OUTPUT LOOP UNTIL 0
 RTS
 SPC 1
**************************************************
* WRITE A BYTE TO DISK                           *
**************************************************
WTBYTE TST <STATUS CHECK STATUS
 BPL WTBYTE LOOP IF NOT READY FOR OUTPUT
 STA <WTDATA WRITE THE BYTE
 RTS
 SPC 1
**************************************************
* VERIFY A SECTOR AFTER WRITE                    *
**************************************************
VERIFY BNE VERFYX EXIT IF WRITE ERROR
 LBSR GETSEC GET THE SECTOR
 BNE VERFYX GO IF ERROR
 PSHS B SAVE CC STATE (IN B)
 LBSR RDSYNC FIND & READ SYNC BYTE
 BNE VERFY1 GO IF ERROR
 LDA #8 PRESET ERROR CODE
 STA DCBERR,X (8)
 LEAU DCBCUR,X POINT TO HEADER
 LDB #DCBCRC-DCBCUR LENGTH TO B
 BSR COMPAR CHECK THE HEADER
 BNE VERFY1 GO IF ERROR
 LBSR GETBUF POINT U AT BUFFER
 LDB DCBCNT,X LENGTH TO B
 BSR COMPAR CHECK THE DATA
 BNE VERFY1 GO IF ERROR
 LEAU DCBCRC,X POINT U AT CRC
 LDB #2 LENGTH TO B
 BSR COMPAR CHECK CRC
 BNE VERFY1 GO IF ERROR
 CLR DCBERR,X RESET ERROR CODE (0)
VERFY1 PULS CC RESTORE CC STATE
VERFYX TST DCBERR,X SET/RESET 'Z' FLAG
 RTS
 SPC 1
**************************************************
* COMPARE DISK DATA WITH RAM DATA                *
**************************************************
COMPAR LBSR RDBYTE READ A BYTE FROM DISK
 CMPA ,U+ COMPARE WITH RAM
 BNE COMPR1 GO IF UNEQUAL
 DECB DECREMENT LOOP COUNT
 BNE COMPAR LOOP IF NOT ZERO
COMPR1 RTS
 SPC 1
**************************************************
* PROCESS DRIVER CONTROL FUNCTIONS               *
**************************************************
CTLFNC TSTA FUNCTION 0 - SCAN DRIVES FOR READY STATE
 BNE DRVSCX EXIT IF NOT LEGAL
 SPC 1
**************************************************
* SCAN DRIVES FOR READY CHECK                    *
*                                                *
* ENTRY REQUIREMENTS:  NONE                      *
*                                                *
* EXIT CONDITIONS:  DDT REFLECTS DRIVE STATUS    *
*                     FOR ALL DRIVES             *
*                                                *
**************************************************
DRVSCN LDA #$40 START WITH DRIVE 1
DRVSC1 STA <DRVSEL SELECT NEXT DRIVE
 LDD #$4000 SET TIMER FOR 1 REVOLUTION
 LBSR DRVTST TEST DRIVE STATE
 BNE DRVSC2 GO IF NOT READY
 LDA CURTRK,Y GET TRACK HISTORY
 INCA CHECK STATUS
 BNE DRVSC2 GO IF INITIALIZED
 LBSR RESTOR RESTORE THE DRIVE
DRVSC2 LEAY 8,Y ADVANCE DDT POINTER
 LDA <DRSTAT GET DRIVE STATUS
 ANDA #$C0 MASK OFF DRIVE SELECT
 ADDA #$40 BUMP TO NEXT DRIVE
 CMPA #$40 WRAP AROUND?
 BNE DRVSC1 LOOP IF NOT DONE
DRVSCX RTS
 SPC 1
**************************************************
* SYSTEM CALL DISPATCH ROUTINE                   *
*                                                *
* ENTRY REQUIREMENTS:  CALL VIA SWI3             *
*                      ROUTINE # FOLLOWS SWI3    *
*                                                *
* EXIT CONDITIONS:  VARY BY CALL                 *
*                                                *
**************************************************
SYCALL LDY #MONVEC POINT TO MONITOR VECTORS
 LDX 10,S GET RETURN ADDRESS
 LDA ,X+ GET PARAMETER
 STX 10,S UPDATE RETURN ADDRESS
 LDB -1,X GET PARAM IN B ALSO
 BNE SYCAL1 GO IF NOT 0
 JMP [,Y] EXIT TO MONITOR WITH STACK INTACT
SYCAL1 ASLB CONVERT PARAM TO OFFSET
 CMPA #7 MONITOR CALL?
 BHI SYCAL2 GO IF NOT
 NEGB MAKE NEGATIVE
 LEAY [B,Y] GET ROUTINE ADDRESS
 STY 8,S SAVE ON STACK
 PULS CC,A,B,DP,X,Y,PC MAKE THE CALL
SYCAL2 LDX PSYRAM POINT TO PSYMON RAM
 LDX FRERAM,X POINT TO MINIDOS/9 RAM
 JMP [SYSVEC,X] EXIT TO EXTERNAL HANDLER
 SPC 1
 END


*** *********************************************************************
*** *                  From Byte mag. May 1980 p. 48                    *
*** * https://ia902709.us.archive.org/35/items/byte-magazine-1980-05/1980_05_BYTE_05-05_Floppy_Disks.pdf
*** *********************************************************************
*** Hexadecimal         Function When Used as Input
*** Address
*** CC00    Read USRT status:
***         bit 0 = 1 means disk unit ready to send byte to computer
***             at address CC01 during read operation
***         bit 7 = 1 means disk unit ready to receive byte from computer
***             at address CC01 during write operation
*** CC01    Address used to transmit data from disk drive to computer during read
***         operation
*** CC02    During read operation, bits 0 thru 3 contain current sector number in
***         binary
*** CC03    Drive status byte: see table 2b.
*** CC04    Accessing this location with a load instruction (LDA) causes a read
***         operation to take place
*** *********************************************************************
*** Table 2b Drive status byte (Read)
*** Bit Value   Meaning
*** 0   1   Write protect notch in disk covered; disk is protected
*** 1   1   Head is at track 0
*** 2   0   Drive motor is on
*** 3   0   Drive circuit is ready to write to disk
*** 4   1   Sector pulse; drive detects sector hole
*** 5   1   Index pulse; drive detects special index hole
*** 6,7     Binary number of drive selected (01 thru 03)
*** *********************************************************************
*** table 2c
*** Hexadecimal Function When Used as Output
*** Address
*** CC00        Defines value that controller will recognize as the SYNC byte at the
***             beginning of a read operation; hexadecimal FB used in Percom format
*** CC01        Address used to transmit data from computer to disk unit during write
***             operation
*** CC02        Defines value that controller will recognize as the filler byte (written
***             after trailer until disk motor turns off); hexadecimal FF used in this
***             software
*** CC03        Data to select drive and head movement direction:
***             bit 4       direction of head movement: 1 = in, 0 = out
***             bit 5       step pulse bit; causes data�transfer head to
***                     jump to next track in direction given by bit 4
***             bits 6, 7   binary number of drive to be selected
*** CC04        Accessing this location with a store instruction (STA) causes a write
***             operation to take place
*** CC05        Accessing this location with either a load (LDA) or store instruction
***             causes a motor on pulse to be sent to the disk drive
*** CC06        Accessing this location with either a load or store instruction causes a
***             motor off pulse to be sent to the disk drive
*** *********************************************************************

