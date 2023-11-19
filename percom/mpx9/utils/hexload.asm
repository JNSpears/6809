 pragma 6809
 pragma cescapes

 NAM HEXLOAD
 TTL HEXLOAD Utility
 
****************************************
*
* HEXLOAD - LOAD A HEXADECIMAL (S1-S9) FORMATTED FILE
* AN MPX-9 UTILITY PROGRAM
* 
* This Program will allow YOU to load files Produced by
* assemblers into memory. This Particular Prosram will allow
* you to relocate the code elsewhere in memory.  It is
* called by:
* 
* MPX? HEXLOAD <filename> [offset]
* 
* The filename must be specified. The optional offset should
* be specified in hexadecimal form. This offset will be ADDED
* to the location at which the file would normallY load.  Most
* 6809 Programs will be assembled to load at location 0, so
* the offset will be the actual load address for these Prosrams.
* 
* After the hex loader has loaded the file. it will prompt for
* a dditional input files.  You may continue entering filenames
* and offsets until vou are finished.  YOU MUST SPECIFY THE
* OFFSET FOR EACH FILE, it does not carry over.  When You have
* finished. Just enter a return at the prompt, it will exit the
* hex loader via an RTS.
* 
* All error conditions are reported. and then You are prompted
* for a new file and offset.  Be aware that if an error occurs
* the file has onlY been PartiallY loaded into memory.
*  
****************************************
*
* WRITTEN BY James Spears Nov 2023
* based on the user interface of the 
* MPX-9 implemetation by TIM MCKEE ON 13 JAN 1981
*
* VERSION 1.00
*
****************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i

        INCLUDE ascii.i

**************************************************
* Program (Position independant)
**************************************************
        ORG     $1000

init:
      ; sts   >stack,pcr	; save the stack for error recovery

load_more:

      clr 	offset,pcr
      clr 	offset+1,pcr

	MPX9  SKPSPC	; x -> filename in command line
	lbeq  synerr

	leay  >infcb,pcr	; point to the input fcb
	MPX9  INTFCB	; initialize the fcb
	lbne  dskerr	; report disk error

 IFDEF DEBUG_COPY
	leay  >infcb,pcr	; point to the output fcb
	LBSR	DspFCB
 ENDC

 	MPX9  HEXNUM    	; GET offset ADDRESS
 	std 	offset,pcr

LoadFile:
	
;
; open input files.
;

 IFDEF DEBUG_COPY
	lda	#'O
	MPX9	OUTCHR
	jsr	CRLF
 ENDC
	; open the input file
	leay 	>infcb,pcr	; point to the input fcb
	leax 	>buffer,pcr	; point to file buffer
	lda  	#ReadFn	; setup for read
	MPX9 	OPNFIL	; open the file
	lbne 	dskerr	; report disk error
 IFDEF DEBUG_COPY
	leay	>infcb,pcr
	LBSR	DspFCB
 ENDC

;
; read data from infile
;
	bsr 	load

      jsr	CRLF
	leax 	>msg,pcr
	MPX9  PSTRNG
	lda 	>ChkSum,pcr
	MPX9  DSPSBY
      jsr	CRLF



;
; we are done, close files.
;
 IFDEF DEBUG_COPY
	lda	#'c
	MPX9	OUTCHR
 ENDC
	; Close the input file
	leay 	>infcb,pcr	; point to the input fcb
      MPX9	CLSFIL	; close the input file
      bne	dskerr	; report disk error
 
 IFDEF DEBUG_COPY
      jsr	CRLF
 ENDC


	leax 	>MoreMsg,pcr
	MPX9  PSTRNG

* SYSTEM CALL 9 (>ChkSum,pcr) - GET INPUT LINE        *
*                                                *
* ENTRY REQUIREMENTS:  X POINTS AT LINE BUFFER   *
*                      B CONTAINS BUFFER LENGTH  *
*                                                *
* EXIT CONDITIONS:  A CONTAINS FIRST CHARACTER,  *
*                     NUL => LINE CANCELED       *
*                     Z FLAG IN CC SET PER A     *
*                   OTHERS UNCHANGED             *
	ldb 	#80
	leax 	>buffer,pcr
	MPX9 	GETLIN
      jsr	CRLF

	cmpa 	#CR
	bne 	load_more


;
; exiting
;

LoadX:        
	clrb 		; no error
	bra	return

;
; error handling
;
synerr ldb 	#ERR_SN
dskerr nop
return NOP ; lds  >stack,pcr
	tstb
      rts 

*** ***********************
*** * Load program in hex format.
*** * 
*** * ENTRY REQUIREMENTS: NONE
*** * 
*** * EXIT CONDITIONS: ALL REGISTERS CHANGED
*** * 
*** ***********************
load:
	; TFR   S,Y		; MARK STACK FOR ERROR RECOVERY
      sts   >stack,pcr	; save the stack for error recovery
load1:
	leay	>infcb,pcr 	; point to fcb
      MPX9	RDFIL		; get a character of input
      bne 	loadx
load2:
	CMPA  #'S		; START OF RECORD?
	BNE   load1		; LOOP IF NOT
	MPX9  RDFIL		; get a character of input
	MPX9 	OUTCHR
	CMPA  #'9		; END OF LOAD?
	BEQ   loadx		; GO IF YES
	CMPA  #'1		; START OF RECORD?
	BNE   load2		; LOOP IF NOT
	CLR   >ChkSum,pcr	; INIT CHEChkSum
	BSR   InByte	; READ LENGTH
	SUBA  #2		; ADJUST IT
	TFR   A,B		; SAVE IN B
	pshs  B
	BSR   InByte	; GET ADDRESS HI
	STA   ,--S		; SAVE ON STACK
	BSR   InByte	; GET ADDRESS LO
	STA   1,S		; PUT ON STACK
	ldx   ,s++
	ldd   >offset,pcr	; get offset
	leax  D,X		; X = line address + offset
	puls  B 		; B = byte length of line
load3:
	BSR   InByte	; READ A BYTE
	DECB  		; DECREMENT COUNT
	BEQ   load4		; GO IF DONE

	STA   ,X		; STORE BYTE
	CMPA  ,X+		; VERIFY GOOD STORE
	BNE   load5		; GO IF ERROR

	BRA   load3
load4:
	INC   >ChkSum,pcr	; CHECK CHEChkSum
	BEQ   load1		; LOOP IF GOOD
load5:
	LDA   #$FF		; SET ERROR FLAG
	STA   >ChkSum,pcr
	; TFR   Y,S	; RESTORE STACK
	lds  >stack,pcr
loadx:
	RTS   


*** ***********************
*** * Input Byte (2 hex digits).
*** ***********************
InByte:
	BSR   InHex		; GET HEX DIGIT
	BEQ   InByteERR		; GO IF ERROR
	ASLA  		; SHIFT TO MS HALF (upper nibble)
	ASLA  
	ASLA  
	ASLA  
	PSHS  A		; SAVE DIGIT
	BSR   InHex		; GET ANOTHER DIGIT
	BEQ   InByteERR		; GO IF ERROR
	ADDA  ,S		; COMBINE HALVES
	STA   ,S		; SAVE ON STACK
	ADDA  >ChkSum,pcr	; ADD TO CHEChkSum
	STA   >ChkSum,pcr
	PULS  A,PC 		; GET RESULT & RETURN
InByteERR
	BRA   load4		; GO IF ERROR


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
	MPX9 	RDFIL		; get a character of input
		CMPA  #$0D
		beq 	InHex
	PSHS  A		; SAVE IT
	SUBA  #$30		; CONVERT TO BINARY
	BMI   InHex2	; GO IF NOT NUMERIC
	CMPA  #$09		; GREATER THAN 9?
	BLS   InHex1	; GO IF NOT
	SUBA  #$07		; CONVERT LETTER
	CMPA  #$0A		; LEGAL VALUE?
	BCS   InHex2	; GO IF NOT
InHex1:
	CMPA  #$0F		; GREATER THAN 15?
 	BLS   InHex3	; GO IF NOT
InHex2:
	LDA   ,S		; GET ORIGINAL CHAR BACK
InHex3:
	CMPA  ,S+		; SET/RESET Z FLAG
	RTS   


 IFDEF DEBUG_COPY

DspFCB:		; Y -> FCB
	pshs   x
	leax	FCB0,PCR
	MPX9   PSTRNG
	ldd	#32
	tfr	y,x
	jsr	[DumpMem2v]
       jsr	CRLF
	leax	FCB1,PCR
	MPX9   PSTRNG
       jsr	CRLF
	puls   x,pc

DspDCB:		; Y -> DCB
	pshs   x
       MPX9   OUTCHR
	leax	DCB0,PCR
	MPX9   PSTRNG
	ldd	#32
	tfr	y,x
	jsr	[DumpMem2v]
       jsr	CRLF
	leax	DCB1,PCR
	MPX9   PSTRNG
       jsr	CRLF
	puls   x,pc


DCB0:	FCS "      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr"
DCB1: 	FCS "       sc prev_ next_ ct data@  ty crc"

FCB0: 	FCS "       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B"
FCB1: 	FCS "       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St"

 ENDC

*
** Constants.
*

msg   	fcs	"\r\nChkSum: "

MoreMsg   	fcs	"\r\nMore? (or CR) enter: filename [offset]\r\n? "
*
** Working Variables
*


infcb		rmb	32

ChkSum	rmb	1 	; cheChkSum for each line.

stack		rmb	2	; save the stack pointer, for error recovery.
offset	rmb	2	; offset for relocation

buffer	rmb	256	; buffer

endpgm  equ     *-1

 end

load 'utils/hexload.sym
BR "load
BR "loadx
BR "InByteERR
GO
Z
HEXLOAD TEST_MAI.S1 2000


