; **********************************************
; PSYMON VERSION 1.20
; A 6809 ROM MONITOR
;
; THE PERCOM SYSTEM MONITOR (PSYMON) WAS
; WRITTEN BY A TEAM OF PROGRAMMERS USING
; STRUCTURED TECHNIQUES. THE TEAM MEMBERS
; ARE AS FOLLOWS:
; HAROLD_A MAUCH - PRESIDENT, PERCOM DATA
; MIKE FOREMAN - 6809 PROJECT LEADER
; BYRON SEASTRUNK - DESIGN ENGINEER
; CLIFF RUSHING - PROGRAMMER
; JIM STUTSMAN - CHIEF PROGRAMMER
;
; COPYRIGHT (C) 1979 PERCOM DATA COMPANY, INC.
; USE OF THIS SOFTWARE IS GRANTED ROYALTY-FREE
; AS LONG AS THE USER CLEARLY ACKNOWLEDGES ITS
; ORIGIN.
;
; WHILE THIS MONITOR IS VERY SIMPLE, ITS TRUE
; POWER LIES IN ITS EXTENSIBILITY AND IN THE
; TOOLS THAT IT PROVIDES FOR OTHER SOFTWARE
; TO USE. THIS OPERATING SYSTEM IS DEDICATED
; TO HAROLD MAUCH AND HIS LEGENDARY 512 BYTE
; OPERATING SYSTEM.
;
; COMMANDS:
; M <ADDRESS> - MEMORY EXAMINE/CHANGE
; G <ADDRESS> - GO TO ADDRESS
; R <REGISTER> - REGISTER EXAMINE/CHANGE
; L - LOAD PROGRAM FROM TAPE
; S <START> <END> - SAVE PROGRAM TO TAPE
; B <ADDRESS> - SET/LIST BREAKPOINTS
; U <ADDRESS> - UNSET BREAKPOINTS
; Z - JUMP TO PROM AT ADDRESS Coo0 HEX
;
; CALLABLE SUBROUTINES:
; INCHR - INPUT CHARACTER FROM CONSOLE
; OUTCHR - OUTPUT CHARACTER TO CONSOLE
; REQIO - PERFORM I/O TO PERIPHERAL
; GETHEX - INPUT HEX NUMBER FROM CONSOLE
; INHEX - INPUT HEX DIGIT FROM CONSOLE
; DSPSBY - DISPLAY SINGLE BYTE & SPACE
; DSPDBY - DISPLAY DOUBLE BYTE & SPACE
; OUTHEX - DISPLAY 2 HEX DIGIST
; PSTRNG - DISPLAY STRING ON CONSOLE
; LOAD - LOAD HEX PROGRAM FROM CONSOLE I
; SAVE - SAVE HEX PROGRAM TO CONSOLE
; CRLF - BEGIN NEW LINE ON CONSOLE
; OUTS - OUTPUT SPACE TO CONSOLE
;
; ALL I/O WITHIN PSYMON IS DONE THROUGH THE
; USE OF DEVICE CONTROL BLOCKS. THIS ALLOsS
; EASY MODIFICATION BY THE USER. PSYMON HAS
; FOUR DCB POINTERS INITIALIZED TO POINT TO THE
; CONSOLE (ACIA) DCB. THEY ARE USED AS
; FOLLOWS:
;
; CIDCB -	POINTS TO DCB USED FOR CONSOLE
;			INPUT (CHARACTER I/O).
; CEDCB -	POINTS TO DCB USED FOR ECHO OF
;			CHARACTERS RECEIVED USING CIDCB.
;			ECHO MAY BE SUPPRESSED BY SETTING
;			THIS POINTER TO ZERO.
; CODCB -	POINTS TO DCB USED FOR CONSOLE
;			OUTPUT (CHARACTER I/O).
; TPDCB -	POINTS TO DCB USED FOR PSYMON
;			TAPE LOAD & SAVE COMMANDS.
;
; THE PSYMON COMMAND TABLE MAY BE EXTENDED
; OR CHANGED BY SETTING THE POINTER 'USRTBL'
; TO THE ADDRESS OF A USER COMMAND TABLE. IT
; IS INITIALIZED TO ZERO, INDICATING NO USER
; TABLE EXISTS.
;
; ADDITIONAL INFORMATION REGARDING THE USE OF
; PSYMON' MAY BE OBTAINED FROM:
;	PERCOM DATA COMPANY, INC.
;	211 NORTH KIRBY
;	GARLAND, TEXAS 75042
;
; REVISION A - 11/23/79
;	ADDITION OF A VECTOR FOR SCRATCHPAD RAM
;
; REVISION B - 02/08/80
;	ADDITION OF A VECTOR FOR FREE RAM
;
; **********************************************

; SYSTEM ADDRESS CONSTANTS

ROM1	EQU $FC00 ; BASE ADDRESS OF PSYMON ROM
ROM2	EQU $F800 ; BASE ADDRESS OF EXTENSION ROM
RAM	EQU $F380 ; BASE ADDRESS OF SCRATCHPAD RAM;
FREE	EQU $F000 ; ADDRESS OF FREE RAM

TERMNL	EQU $F7FE ; SYSTEM TERMINAL ACIA
;TERMNL	EQU $C000 ; SYSTEM TERMINAL ACIA -- for simulator

; ASCII CHARACTER CONSTANTS

CR EQU $D	; CARRIAGE RETURN
LF EQU $A	; LINE FEED
SP EQU $20	; SPACE

; ACIA CONTROL CONFIGURATIONS

RESET	EQU $03	; RESET ACIA
CONFIG	EQU $51	; SET FOR 8 DATA, 2 STOP, NO PARITY,
RDRON	EQU CONFIG-$40	; READER ON (RTS ON)
RDROFF	EQU CONFIG	; READER OFF (RTS OFF)


; DCB offsets
DCBLnk	equ 0	; pointer to next dcb in chain
DCBDId	equ 2	; ascii 2 char device Id
DCBDrv	equ 4	; device driver addr
DCBIOA	equ 6	; device i/o addr
DCBErr	equ 8	; error status code
DCBExt	equ 9	; number of extension bytes in dcb
DCBApp	equ 10	; dcb extension for driver
; DCB function Codes
ReadFn	equ 1	; Read function code
WritFn	equ 2	; Write function code
StatFn	equ 4	; Status function code
CntlFn	equ 8	; Device Control function code


; PSYMON RAM DEFINITIONS
	ORG RAM
; PSYMON INTERNAL STACK & REGISTER SPACE
; OFFSETS TO RAM BASE IN PARENTHESES

	RMB 55 ; STACK SPACE r1
STACK	EQU * ; (55) TOP OF STACK

REGC	RMB 1 ; (55) CONDITION CODE REGISTER

REGA	RMB 1 ; (56) A REGISTER
REGB	RMB 1 ; (57) B REGISTER
REGD	RMB 1 ; (58) DIRECT PAGE REGISTER
REGX	RMB 2 ; (59) X REGISTER
REGY	RMB 2 ; (61) Y REGISTER
REGU	RMB 2 ; (63) U STACK POINTER
REGP	RMB 2 ; (65) PROGRAM COUNTER

; PSYMON BREAKPOINT TABLE
BpTabl	RMB 15 ; (67) SPACE FOR 5 BREAKPOINTS
BpTEnd	EQU * ; (82) END OF BREAKPOINT TABLE

; PSYMON WORK AREAS
MemPtr	RMB 2 ; (82) MEMORY POINTER FOR ‘M’ COMMAN1
UsrTbl	RMB 2 ; (84) ADDRESS OF USER COMMAND TABLE)
Comand	RMB 1 ; (86) COMMAND CHARACTER STORAGE
CkSum	RMB 1 ; (87) CHECKSUM FOR LOAD AND SAVE
BegAdd	RMB 2 ; (88) BEGIN ADDRESS FOR SAVE
EndAdd	RMB 2 ; (90) END ADDRESS FOR SAVE
StkPtr	RMB 2 ; (92) CONTENTS OF STACK POINTER

; THE PSYMON CONSOLE DCB
ConDCB	RMB 10 ; (94) STANDARD DCB

; PSYMON DCB POINTERS
DCBCHN	RMB 2 ; (104) BASE OF DCB CHAIN
CIDCB	RMB 2 ; (106) CONSOLE INPUT DCB
CEDCB	RMB 2 ; (108) CONSOLE ECHO DCB
CODCB	RMB 2 ; (110) CONSOLE OUTPUT DCB
TPDCB	RMB 2 ; (112) CASSETTE TAPE DCB

; PSYMON VECTORS
SWI3v	RMB 2 ; (114) SOFTWARE INTERRUPT 3
SWI2v	RMB 2 ; (116) SOFTWARE INTERRUPT 2
FIRQv	RMB 2 ; (118) FAST INTERRUPT REQUEST
IRQv	RMB 2 ; (120) INTERRUPT REQUEST
SWIv	RMB 2 ; (122) SOFTWARE INTERRUPT
NMIv	RMB 2 ; (124) NON-MASKABLE INTERRUPT
FRERAM	RMB 2 ; (126) ADDRESS OF FREE RAM


; PSYMON ROM CODE
	ORG ROM1
begcod  equ *

*** ***********************
*** * Psymon Initialization
*** ***********************
Init:
	LDS   #STACK	; SET UP STACK POINTER
	TFR   S,X	; POINT X AT STACK
INIT1:
	CLR   ,X+	; CLEAR A BYTE
	CMPX  #ConDCB+2	; ALL FIELDS CLEAR?
	BNE   INIT1	; LOOP IF NOT
	LDY   #RamInt	; POINT TO RAM DATA
INIT2:
	LDD   ,Y++	; MOVE 2 BYTES
	STD   ,X++
	CMPX  #FRERAM+2	; END OF RAM?
	BNE   INIT2	; LOOP IF NOT
	LDX   #ConDCB	; POINT TO DCB
	LDD   #RESET*256+CntlFn ; A=RESET, B=CNTLFN
	JSR   ReqIO	; RESET ACIA
	LDA   #CONFIG	; CONFIGURE ACIA
	JSR   ReqIO
	LDA   ROM2	; CHECK FOR SECOND ROM
	CMPA  #$7E	; IS THERE A JUMP THERE?
	BNE   MonEnt	; GO IF NOT
	JSR   ROM2	; CALL SECOND ROM

*** ***********************
*** * Psymon User Entry
*** ***********************
MonEnt:
	STS   StkPtr	; SAVE STACK POINTER

*** ***********************
*** * Get Command
*** ***********************
GETCMD:
	LDX   #PROMPT	; DISPLAY PROMPT
	JSR   PString
	JSR   InChr	; INPUT COMMAND CHARACTER
	BSR   LOOKUP	; LOOK IT UP
	BNE   GETCMD	; LOOP IF NOT FOUND
	JSR   OutSp	; OUTPUT A SPACE
	JSR   [,X]	; CALL COMMAND ROUTINE
	BRA   GETCMD	; GO BACK FOR MORE

PROMPT:	
	FCB	CR,LF
	FCC 'Cmd'
	FCB '?+$80	; END OF STRING


*** ***********************
*** * Lookup Comman In Table
*** ***********************
LOOKUP:
  LDY   #Comand		; POINT Y TO COMMAND
  STA   ,Y		; SAVE COMMAND CHARACTER
  LDX   UsrTbl		; GET USER TABLE ADDRESS
  BEQ   Look1		; GO IF NONE
  BSR   Search		; SEARCH USER TABLE
  BEQ   SerchX		; GO IF FOUND
Look1:
  LDX   #CmdTbl		; SEARCH INTERNAL TABLE

*** ***********************
*** * General Table Search
*** *
*** * Entry: X -> Table
*** *        Y -> Item
*** *        First byte of table must contain the item length, last byte must be 0xFF
*** * Exit: C - Z set if found else clear
*** *       X -> addr of routine
*** *       A,B - changed
*** ***********************
Search:
  LDB   ,X+	; GET ITEM LENGTH
Serch1:
  BSR   ComPar	; COMPARE CURRENT ITEM
  ABX   	; ADVANCE To NEXT ITEM
  BEQ   SerchX	; EXIT IF MATCH
  LEAX  2,X	; STEP OVER ADDRESS
  TST   ,X	; END OF TABLE?
  BPL   Serch1	; LOOP IF NOT
SerchX:
  RTS   


*** ***********************
*** * General String Compare
*** *
*** * Entry: X -> addr str1
*** *        Y -> addr str2
*** *        B = Length of strings
*** * Exit: C - set per compare
*** *       A - changed
*** ***********************
ComPar:
  PSHS  Y,X,B	; SAVE REGISTERS
Comp1:
  LDA   ,X+	; GET NEXT CHARACTER
  CMPA  ,Y+	; COMPARE IT
  BNE   CompX	; EXIT IF UNMATCHED
  DECB  	; EXIT IF UNMATCHED
  BNE   Comp1
CompX:
  PULS  B,X,Y,PC ; RESTORE REGISTERS & EXIT

*** ***********************
*** * Load program from Tape
*** ***********************
TLoad:
  LDD   CIDCB 	; SAVE CONSOLE DCBS
  LDX   CEDCB
  PSHS  X,B,A
  LDX   TPDCB 	; POINT TO TAPE DCB
  CLRA  
  CLRB  
  STX   CIDCB 	; SET TAPE IN, NO ECHO
  STD   CEDCB
  LDD   #RDRON*256+CntlFn ; RAISE READER CONTROL
  JSR   ReqIO
  BSR   Load	; LOAD THE TAPE
  LDD   #RDROFF*256+CntlFn ; DROP READ CONTROL
  LDX   TPDCB
  JSR   ReqIO
  PULS  A,B,X	;  RESTORE CONSOLE DCBS
  STD   CIDCB
  STX   CEDCB
  TST   CkSum	; ANY ERRORS?
  BEQ   loadx	; GO IF NOT

*** ***********************
*** * Display error inicator '?'
*** ***********************
Error:
  LDA   #$3F	; DISPLAY ERROR INDICATOR
  JMP   OutChr


*** ***********************
*** * Load program in hex format.
*** * 
*** * ENTRY REQUIREMENTS: NONE
*** * 
*** * EXIT CONDITIONS: ALL REGISTERS CHANGED
*** * 
*** ***********************
Load:
  TFR   S,Y	; MARK STACK FOR ERROR RECOVERY
Load1:
  JSR   InChr	; GET A CHARACTER
load2:
  CMPA  #'S	; START OF RECORD?
  BNE   Load1	; LOOP IF NOT
  JSR   InChr	; GET ANOTHER CHARACTER
  CMPA  #'9	; END OF LOAD?
  BEQ   loadx	; GO IF YES
  CMPA  #'1	; START OF RECORD?
  BNE   load2	; LOOP IF NOT
  CLR   CkSum	; INIT CHECKSUM
  BSR   InByte	; READ LENGTH
  SUBA  #2	; ADJUST IT
  TFR   A,B	; SAVE IN B
  BSR   InByte	; GET ADDRESS HI
  STA   ,--S	; SAVE ON STACK
  BSR   InByte	; GET ADDRESS LO
  STA   1,S	; PUT ON STACK
  PULS  X	; ADDRESS Now IN x
load3:
  BSR   InByte	; READ A BYTE
  DECB  	; DECREMENT COUNT
  BEQ   load4	; Go IF DONE
  STA   ,X	; STORE BYTE
  CMPA  ,X+	; VERIFY GOOD STORE
  BNE   load5	; GO IF ERROR
  BRA   load3
load4:
  INC   CkSum	; CHECK CHECKSUM
  BEQ   Load1	; LOOP IF GOOD
load5:
  LDA   #$FF	; SET ERROR FLAG
  STA   CkSum
  TFR   Y,S	; RESTORE STACK
loadx:
  RTS   


*** ***********************
*** * Input Byte (2 hex digits).
*** ***********************
InByte:
  BSR   InHex	; GET HEX DIGIT
  BEQ   load4	; GO IF ERROR
  ASLA  	; SHIFT TO MS HALF (upper nibble)
  ASLA  
  ASLA  
  ASLA  
  PSHS  A	; SAVE DIGIT
  BSR   InHex	; GET ANOTHER DIGIT
  BEQ   load4	; GO IF ERROR
  ADDA  ,S	; COMBINE HALVES
  STA   ,S	; SAVE ON STACK
  ADDA  CkSum	; ADD TO CHECKSUM
  STA   CkSum
  PULS  A,PC 	; GET RESULT & RETURN


*** ***********************
*** * Input Hex number (4 hex digits).
*** * 
*** * Entry: None
*** * 
*** * Exit: A - last char input
*** *       B - hex digit count
*** *       X - Hex number
*** *       C - set according to B
*** * 
*** ***********************
GetHex:
  CLRB  	; INITIALIZE DIGIT COUNT, RESULT
  LDX   #$0000
GetHx1:
  BSR   InHex	; GET A DIGIT
  BEQ   GetHx2	; GO IF NOT HEX
  EXG   D,X	; OLD RESULT TO A,B
  ASLB  	; SHIFT LEFT 1 DIGIT
  ROLA  
  ASLB  
  ROLA  
  ASLB  
  ROLA  
  ASLB  
  ROLA  
  EXG   D,X	; REPLACE RESULT
  LEAX  A,X	; ADD IN NEW DIGIT
  INCB  	; ADD TO DIGIT COUNT
  BRA   GetHx1	; LOOP FOR MORE

GetHx2:
  TSTB  	; SET/RESET Z FLAG
  RTS   


*** ***********************
*** * Input Hex Digit (1 hex digits).
*** * 
*** * Entry: None
*** * 
*** * Exit: A - hex digit or non-hex
*** *       C - Z set if non-Hex
*** *		 ALL other regs perserved
*** * 
*** ***********************
InHex:
  BSR   InChr	; SET/RESET z FLAG
  PSHS  A	; SAVE IT
  SUBA  #$30	; CONVERT TO BINARY
  BMI   InHex2	; GO IF NOT NUMERIC
  CMPA  #$09	; GREATER THAN 9?
  BLS   InHex1	; GO IF NOT
  SUBA  #$07	; CONVERT LETTER
  CMPA  #$0A	; LEGAL VALUE?
  BCS   InHex2	; GO IF NOT
InHex1:
  CMPA  #$0F	; GREATER THAN 15?
  BLS   InHex3	; GO IF NOT
InHex2:
  LDA   ,S	; GET ORIGINAL CHAR BACK
InHex3:
  CMPA  ,S+	; SET/RESET Z FLAG
  RTS   


*** ***********************
*** * Console Input
*** * 
*** * Entry: None
*** * 
*** * Exit: A - char with Parity removed
*** *		 ALL other regs perserved, except C
*** * 
*** ***********************
InChr:
  PSHS  X,B	; SAVE REGISTERS
  LDX   CIDCB	; POINT TO INPUT DCB
  LDB   #ReadFn	; SET UP FOR READ
  BSR   ReqIO	; READ A CHARACTER
  ANDA  #$7F	; REMOVE PARITY
  LDX   CEDCB	; POINT TO ECHO DCB
  PSHS  A	; SAVE CHARACTER
  BNE   OutCh1	; GO IF ECHO
  PULS  A,B,X,PC ; RESTORE & RETURN


*** ***********************
*** * Console Output
*** * 
*** * Entry: A - Char to be output
*** * 
*** * Exit: ALL regs perserved, except C
*** * 
*** ***********************
OutChr:
	PSHS  X,B,A	; SAVE REGISTERS
	LDX   CODCB	; POINT TO OUTPUT DCB
OutCh1:
	LDB   #WritFn	; SET FUNCTION
	BSR   ReqIO	; OUTPUT THE CHARACTER
	PULS  A,B,X,PC	; RESTORE REGISTERS & RETURN


*** ***********************
*** * Preform I/O requests
*** * 
*** * Entry: A - Driver Parameter
*** *        B - Function Code
*** *        X -> DBC address
*** * 
*** * Exit: A - Driver Result
*** *       ALL regs perserved, except C
*** * 
*** ***********************
ReqIO:
	PSHS  U,Y,X,DP,B	; SAVE REGISTERS
	JSR   [DCBDrv,X]	; CALL DRIVER
	PULS  B,DP,X,Y,U,PC	; RESTORE REGISTERS & EXIT


*** ***********************
*** * Output a Double Byte (4 hex digits) and a space to the console.
*** * 
*** * Entry: A,B - double Byte to be printed
*** * 
*** * Exit: ALL regs perserved, except C
*** * 
*** ***********************
DspDBy:
	BSR   OUTHEX	; DISPLAY A AS 2 HEX DIGITS
	EXG   A,B	; LS BYTE TO A
	BSR   DspSBy	; DISPLAY AS 2 DIGITS, SPACE
	EXG   A,B	; RESTORE A & B
	RTS   


*** ***********************
*** * Output a Byte (2hex digits) and a space to the console.
*** * 
*** * Entry: A - Byte to be printed
*** * 
*** * Exit: ALL regs perserved
*** * 
*** ***********************
DspSBy:
  BSR   OUTHEX	; DISPLAY BYTE IN A

*** ***********************
*** * Output a space to the console
*** * 
*** * Entry: None
*** * 
*** * Exit: ALL regs perserved, except C
*** * 
*** ***********************
OutSp:
  PSHS  A	; SAVE A REGISTER
  LDA   #SP	; OUTPUT A SPACE
OutChX:
  BSR   OutChr	; DISPLAY CHARACTER
  PULS  A,PC	; RESTORE & EXIT

OUTHEX:
  PSHS  A	; RESTORE & EXIT
  LSRA  	; GET MS DIGIT
  LSRA  
  LSRA  
  LSRA  
  BSR   OUTDIG	; DISPLAY IT
  LDA   ,S	; GET LS DIGIT
  BSR   OUTDIG	; DISPLAY IT
  PULS  A,PC	; RESTORE AND RETURN

OUTDIG:
  ANDA  #$0F	; MASK OFF DIGIT
  ADDA  #'0	; CONVERT TO ASCII
  CMPA  #'9	; BIGGER THAN 9?
  BLS   OutChr	; GO IF NOT
  ADDA  #$07	; CONVERT TO LETTER
  BRA   OutChr	; PRINT AND EXIT

*** ***********************
*** * Output a String to the console
*** * 
*** * Entry: X -> POINTS TO STRING
*** *         LAST BYTE HAS BIT 7 ON
*** * 
*** * Exit: X -> POINTS 1 BYTE PAST END
*** *       A,C - CHANGED
*** * 
*** ***********************
PString:
  LDA   ,X	; GET A CHARACTER
  ANDA  #$7F	; MASK OFF
  BSR   OutChr	; DISPLAY IT
  TST   ,X+	; WAS IT LAST?
  BPL   PString	; LOOP IF NOT
  RTS   

*** ***********************
*** * Output a FRLF to the console
*** * 
*** * Entry: None
*** * 
*** * Exit: ALL REGISTERS PRESERVED EXCEPT C
*** * 
*** ***********************
CRLF:
  PSHS  A	; SAVE A REGISTER
  LDA   #CR	; OUTPUT CR
  BSR   OutChr
  LDA   #LF	; OUTPUT LF
  BRA   OutChX

*** ***********************
*** * Save Program on Tape.
*** ***********************
TSave:
  BSR   GETHX	; GET START ADDRESS
  BEQ   TSave2	; GO IF NONE
  STX   BegAdd	; SAVE START
  BSR   GETHX	; GET END ADDRESS
  BNE   TSave1	; GO IF ENTERED
  LDX   BegAdd	; DUPLICATE ADDRESS
  INCB  	; SET ADDRESS INDICATOR
TSave1:
  STX   EndAdd	; SAVE END
TSave2:
  LDX   CODCB	; SAVE CONSOLE DCB
  PSHS  X,A	; SAVE TERMINATOR TOO
  LDX   TPDCB	; SET UP FOR TAPE
  STX   CODCB
  TSTB  	; ANY ADDRESS ENTERED?
  BEQ   TSave3	; GO IF NOT
  BSR   SAVE	; SAVE THE PROGRAM
TSave3:
  PULS  A	; GET TERMINATOR
  CMPA  #$0D	; WAS IT RETURN?
  BNE   TSave4	; GO IF NOT
  LDB   #'9	; S9 RECORD
  BSR   OUTSN
TSave4:
  PULS  X	; RESTORE DCB POINTER
  STX   CODCB	; 
  RTS   

*** ***********************
*** * GET HEX NUMBER IN X.
*** ***********************
GETHX:
  JMP   GetHex	; RELATIVE BRANCH BOOSTER

*** ***********************
*** * SAVE A PROGRAM IN HEX
*** * 
*** * ENTRY: SAVE ADDRESSES ARE IN
*** *        BEGADD & ENDADD
*** * 
*** * EXIT: ALL REGISTERS CHANGED 
*** * 
*** ***********************

SAVE:
  LDX   BegAdd	; POINT AT FIRST BYTE
SAVE1:
  LDB   #'1	; BEGIN NEW S1 RECORD
  BSR   OUTSN
  CLR   CkSum	; INIT CHECKSUM
  LDD   EndAdd	; CALCULATE BYTES TO SAVE
  PSHS  X
  SUBD  ,S++
  TSTA  	; GREATER THAN 255?
  BNE   SAVE2	; GO IF YES
  CMPB  #16	; LESS THAN FULL RECORD?
  BCS   SAVE3	; GO IF YES
SAVE2:
  LDB   #15	; SET FULL RECORD SIZE
SAVE3:
  INCB  	; CORRECT RECORD SIZE
  TFR   B,A	; OUTPUT RECORD SIZE
  ADDA  #$03	; ADJUST FOR ADDRESS,COUNT
  BSR   OUTBYT
  PSHS  X	; ADDRESS TO STACK
  PULS  A	; OUTPUT ADDRESS HI
  BSR   OUTBYT
  PULS  A	; OUTPUT ADDRESS LO
  BSR   OUTBYT
SAVE4:
  LDA   ,X+	; SAVE A DATA BYTE
  BSR   OUTBYT
  DECB
  BNE   SAVE4	; LOOP UNTIL 0
  LDA   CkSum	; GET CHECKSUM
  COMA  	; COMPLIMENT IT
  BSR   OUTBYT	; OUTPUT IT
  LEAY  -1,X	; CHECK FOR END
  CMPY  EndAdd
  BNE   SAVE1	; LOOP IF NOT
  RTS   

*** ***********************
*** * OUTPUT BYTE AS HEX AND ADD TO CHECKSUM
*** ***********************
OUTBYT:
  JSR   OUTHEX	; OUTPUT BYTE AS HEX
  ADDA  CkSum	; ADD to Checksum
  STA   CkSum
  RTS   

*** ***********************
*** * OUTPUT 'S' TAPE RECORD HEADERS
*** ***********************
OUTSN:
  JSR   CRLF	; BEGIN NEW LINE
  LDA   #'S	; OUTPUT ‘S’ HEADER
  BSR   OUTC
  TFR   B,A	; RECORD TYPE TO A

*** ***********************
*** * OUTPUT CHARACTER TO CONSOLE
*** ***********************
OUTC:
  JMP   OutChr	;  RELATIVE BRANCH BOOSTER

*** ***********************
*** * MEMORY EXAMINE AND CHANGE
*** ***********************
MemEc:
  BSR   GETHX	; GET ADDRESS
  BNE   MEMEC1	; GO IF GOOD
  LDX   MemPtr	; USE PREVIOUS
MEMEC1:
  STX   MemPtr	; UPDATE RAM POINTER
  JSR   CRLF	; BGIN NEWLIN
  TFR   X,D	; DISPLAY ADDRESS
  JSR   DspDBy
  LDA   ,X+	; GET CONTENTS
  JSR   DspSBy	; DISPLAY THEM
  TFR   X,Y	; SAVE ADDRESS IN Y
  BSR   GETHX	; GET CHANGE DATA
  EXG   D,X	; SAVE DELIM, GET NEW
  BEQ   MEMEC2	; GO IF NO CHANGE
  STB   -1,Y	; UPDATE MEMORY
  CMPB  -1,Y	; VERIFY GOOD STORE
  BEQ   MEMEC2	; GO IF GOOD STORE
  JSR   Error	; DISPLAY ERROR
MEMEC2:
  TFR   X,D	; GET DELIMITER IN A
  TFR   Y,X	; NEXT ADDRESS IN X
  CMPA  #CR	; END OF UPDATE?
  BEQ   MEMEC3	; GO IF YES
  CMPA  #$5E	; BACKING UP?
  BNE   MEMEC1	; LOOP IF NOT
  LEAX  ,--X	; BACK UP 2
  BRA   MEMEC1	; CONTINUE

MEMEC3:
  RTS   

*** ***********************
*** * GO TO ADDRESS
*** ***********************
Go:
  LDS   StkPtr	; SET UP STACK
  JSR   GetHex	; GET TARGET ADDRESS
  BEQ   GO1	; GO IF NONE
  STX   10,S	; STORE IN PC ON STACK
GO1:
  LDA   ,S	; SET 'E' FLAG IN CC
  ORA   #$80
  STA   ,S
IntRet:
  RTI   	; LOAD REGISTERS AND GO

*** ***********************
*** * BREAKPOINT (SOFTWARE INTERRUPT) TRAP
*** ***********************
BrkPnt:
  LDX   10,S	; GET PROGRAM COUNTER
  LEAX  -1,X	; DECREMENT BY 1
  STX   10,S	; REPLACE ON STACK
  LDB   #$FF	; FLAG FOR SINGLE REMOVAL
  JSR   REMBK	; REMOVE BREAKPOINT

*** ***********************
*** * INTERRUPT (HARDWARE/SOFTWARE) TRAP
*** ***********************
Trap:
  STS   StkPtr	; SAVE STACK POINTER
  JSR   CRLF	; BEGIN NEW LINE
  BSR   RegDmp	; DUMP REGISTERS
  JMP   GETCMD	; GET NEXT COMMAND

*** ***********************
*** * REGISTER EXAMINE AND CHANGE
*** ***********************
RegEc:
  JSR   InChr	; GET REGISTER TO EXAMINE
  JSR   CRLF	; BEGIN NEW LINE
  CLRB  	; CLEAR OFFSET COUNT
  LDX   #RegIDs	; POINT TO REGISTER ID STRING
REGEC1:
  CMPA  B,X	; CHECK REGISTER NAME
  BEQ   REGEC2	; GO IF FOUND
  INCB  	; ADVANCE COUNTER
  CMPB  #11	; END OF LIST?
  BLS   REGEC1	; LOOP IF NOT
  BRA   RegDmp	; BAD ID - DUMP ALL

REGEC2:
  PSHS  B	; SAVE OFFSET
  BSR   RDUMP	; DISPLAY THE REG & CONTENTS
  JSR   GetHex	; GET NEW VALUE
  PULS  B	; RESTORE OFFSET
  BEQ   REGECX	; GO IF NO CHANGE
  LEAY  B,Y	; POINT TO REG ON STACK
  CMPB  #$03	; SINGLE BYTE REG?
  TFR   X,D	; GET NEW DATA IN A,B
  BLS   REGEC3	; GO IF SINGLE
  STA   ,Y+	; STORE MS BYTE
REGEC3:
  STB   ,Y	; STORE LS BYTE
REGECX:
  RTS   

RegIDs:
	FCC 'CABDXXYYUUPP'


*** *************************
*** * Complete Register Dump
*** *************************
RegDmp:
  LDX   #RegIDs	; POINT TO ID STRING
  CLRB  	; CLEAR OFFSET COUNTER
RGDMP1:
  LDA   B,X	; GET REG NAME
  BSR   RDUMP	; DISPLAY IT
  INCB  	; BUMP TO NEXT REG
  CMPB  #11	; ALL PRINTED?
  BLS   RGDMP1	; LOOP IF NOT
  LDA   #'S	; DISPLAY STACK ID
  BSR   DSPID
  LDY   #StkPtr-12	; Y+B=>StkPtr
  BRA   RDUMP1


*** *************************
*** * Display Register Contents
*** *************************
RDUMP:
  BSR   DSPID	; DISPLAY STACK ID
  LDY   StkPtr	; POINT Y AT STACK
  CMPB  #$03	; SINGLE BYTE REG?
  BLS   RDUMP2	; GO IF YES
RDUMP1:
  LDA   B,Y	; DISPLAY MS BYTE
  JSR   OUTHEX
  INCB  	; ADVANCE OFFSET
RDUMP2:
  LDA   B,Y	; DISPLAY A BYTE
  JMP   DspSBy


*** *************************
*** * Display Register Id
*** *************************
DSPID:
  BSR   OutCh	; DISPLAY REG NAME
  LDA   #'=	; DISPLAY '='
OutCh:
  JMP   OutChr	; RELATIVE BRANCH BOOSTER


*** *************************
*** * Set a Breakpoint
*** *************************
SetBk:
  JSR   GetHex	; GET ADDRESS
  BEQ   DSPBK	; GO IF NONE ENTERED
  BSR   INITBP	; POINT Y AT BP TABLE
SetBk1:
  LDD   ,Y	; EMPTY SLOT?
  BEQ   SetBk2	; GO IF YES
  BSR   NEXTBP	; ADVANCE TO NEXT SLOT
  BNE   SetBk1	; LOOP IF NOT END
  BRA   DSPBK	; EXIT

SetBk2:
  STX   ,Y	; SAVE ADDRESS
  BEQ   DSPBK	; GO IF ADDRESS = 0
  LDA   ,X	; GET CONTENTS
  STA   2,Y	; SAVE IN TABLE
  LDA   #$3F	; SWI OP CODE
  STA   ,X	; SET BREAK

*** *************************
*** * Display all Breakpoints
*** *************************
DSPBK:
  JSR   CRLF	; BEGIN NEW LINE
  BSR   INITBP	; POINT Y AT BP TABLE
DSPBK1:
  LDD   ,Y	; GET ADDRESS OF BP
  BEQ   DSPBK2	; GO IF INACTIVE
  JSR   DspDBy	; DISPLAY ADDRESS
DSPBK2:
  BSR   NEXTBP	; ADVANCE POINTER
  BNE   DSPBK1	; LOOP IF NOT END
  RTS   


*** *************************
*** * Initialize Breakpoint Table Ptr.
*** *************************
INITBP:
  LDY   #BpTabl	; POINT Y AT BP TABLE
  RTS   


*** *************************
*** * Advance Breakpoint Table Ptr.
*** *************************
NEXTBP:
  LEAY  3,Y	; ADVANCE TO NEXT ENTRY
  CMPY  #MemPtr	; CHECK FOR END OF TABLE
  RTS   


*** *************************
*** * Unset a Breakpoins
*** *************************
ClrBk:
  JSR   GetHex	; GET ADDRESS

*** *************************
*** * remove one or more Breakpoins
*** *************************
REMBK:
  BSR   INITBP	; POINT Y AT BP TABLE
REMBK1:
  TSTB  	; REMOVE ALL?
  BEQ   REMBK2	; GO IF YES
  CmpX   ,Y	; FIND ADDRESS?
  BEQ   UNSET	; GO IF YES
  BRA   REMBK3	; LOOP IF NO
REMBK2:
  BSR   UNSET	; UNSET IT
REMBK3:
  BSR   NEXTBP	; ADVANCE POINTER
  BNE   REMBK1	; LOOP IF NOT END
  RTS   


*** *************************
*** * Remove a Breakpoins
*** *************************
UNSET:
  LDX   ,Y	; GET ADDRESS OF BP
  BEQ   UNSETX	; GO IF INACTIVE
  LDA   2,Y	; GET CONTENTS
  STA   ,X	; REPLACE BP
  CLR   0,Y	; MARK BP INACTIVE
  CLR   1,Y
UNSETX:
  RTS   


*** *************************
*** * Terminal Driver (ACIA)
*** *************************
TermDr:
  CLR   DCBErr,X	; NO ERRORS POSSIBLE
  LDX   DCBIOA,X	; GET I/O ADDRESS
  LSRB  		; READ FUNCTION?
  BCS   TermRd		; GO IF YES
  LSRB  		; WRITE FUNCTION?
  BCS   TermWr		; GO IF YES
  LSRB  		; STATUS FUNCTION?
  BCS   TermSt		; GO IF YES
  LSRB  		; CONTROL FUNCTION?
  BCC   TermX		; GO IF NOT
  STA   ,X		; STORE CONTROL CODE
TermX:
  RTS   

TermRd:
  LDB   ,X		; GET STATUS
  LSRB  		; BIT TO C
  BCC   TermRd		; LOOP IF NO INPUT
  LDA   1,X		; GET CHARACTER
  RTS   

TermWr:
  LDB   ,X		; GET STATUS
  BITB  #$02		; READY FOR OUTPUT?
  BEQ   TermWr		; LOOP IF NOT
  STA   1,X		; OUTPUT CHARACTER A
  RTS   

TermSt:
  LDA   ,X		; GET STATUS
  ANDA  #$03		; MASK OFF READY BITS
  RTS   


*** *************************
*** Interrupt Handlers
*** *************************
SWI3:	JMP   [SWI3v]	; SOFTWARE INTERRUPT 3
SWI2:	JMP   [SWI2v]	; SOFTWARE INTERRUPT 2
FIRQ:	JMP   [FIRQv]	; FAST INTERRUPT REQUEST
IRQ:	JMP   [IRQv]	; INTERRUPT REQUEST
SWI:	JMP   [SWIv]	; SOFTWARE INTERRUPT
NMI:	JMP   [NMIv]	; NON-MASKABLE INTERRUPT

*** *************************
*** * PSYMON CMD TABLE
*** *************************
CmdTbl:

	fcb 1 
	fcb 'M		; MEMORY EXAMINE/CHANGE
	fdb	MemEc
	fcb 'G		; GOTO ADDRESS
	fdb	Go
	fcb 'L		; PROGRAM LOAD
	fdb	TLoad
	fcb 'S		; PROGRAM SAVE
	fdb	TSave
	fcb 'R		; REGISTER EXAMINE/CHANGE
	fdb	RegEc
	fcb 'B		; SET/PRINT BREAKPOINTS
	fdb	SetBk
	fcb 'U		; UNSET BREAKPOINTS
	fdb	ClrBk
	fcb $ff		; END SENTINEL
	
*** *************************
*** * Ram Initialization Data
*** *************************
RamInt:
	FCC 'CN'	; CONSOLE DCB ID
	FDB TermDr	; CONSOLE DRIVER
	FDB TERMNL	; CONSOLE I/O ADDRESS
	FDB 0		; STATUS, EXT
	FDB ConDCB	; DCB CHAIN POINTER
	FDB ConDCB	; DCB POINTERS
	FDB ConDCB
	FDB ConDCB
	FDB ConDCB
	FDB Trap	; INTERRUPT VECTORS
	FDB Trap
	FDB IntRet
	FDB Trap
	FDB BrkPnt
	FDB Trap
	FDB FREE

	FCB $ff,$ff,$ff,$ff ; Reserved Space

*** *************************
*** * Software Vectors
*** *************************
	FDB RAM		; BASE OF PSYMON RAM
	FDB DspSBy	; DISPLAY SINGLE BYTE ON CONSOLE
	FDB DspDBy	; DISPLAY DOUBLE BYTE ON CONSOLE
	FDB GetHex	; GET HEX NUMBER FROM CONSOLE
	FDB PString	; PRINT STRING TO CONSOLE
	FDB InChr	; INPUT CHARACTER FROM CONSOLE
	FDB OutChr	; OUTPUT CHARACTER TO CONSOLE
	FDB ReqIO	; PERFORM I/O REQUEST
	FDB MonEnt	; MONITOR RE-ENTRY
	
*** *************************
*** * Hardware Vectors
*** *************************

	FDB Init	; RESERVED BY MOTOROLA
	FDB SWI3	; SOFTWARE INTERRUPT 3
	FDB SWI2	; SOFTWARE INTERRUPT 2
	FDB FIRQ	; FAST INTERRUPT REQUEST
	FDB IRQ		; INTERRUPT REQUEST
	FDB SWI		; SOFTWARE INTERRUPT
	FDB NMI		; NON-MASKABLE INTERRUPT
	FDB Init	; RESTART
	
endcod  equ *
sizcod  equ endcod-begcod
frecod  equ $400-sizcod
        if sizcod&~$3ff
            warning Image must fit in 1k ROM
        endc

 END