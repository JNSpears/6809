 pragma 6809
 NAM COPY
 TTL Copy Utility
 
****************************************
*
* COPY UTILITY
* 
* This MPX/9(tm) utility may be used to copy files from one disk to
* another disk. Files of any type may be copied. All disk writes are
* verified .
* 
* Procedure: The COPY utility is loaded from the MPX/9(tm) system disk.
* There are three command line formats. Blanks are used as delimiters
* between the entries on a command line. Errors are reported in MPX/9
* (tm) codes.
* 
* FORMAT #l: COPY <SOURCE> <DESTINATION >          (Utility in drive #l)
* The first file name is the source file and the second file name is the
* destination. (File names may include drive numbers as discussed in
* Section III.) The copying is performed from source to destination.
* 
* Examples: COPY SRCENAME DESTNAME 
* 
* 4/COPY SRCENAME DESTNAME                         (Utility in drive #4)
* 
* FORMAT #2: COPY <FNAME> <N> 
* The named file is copied to a file of the same name on the disk in
* drive N.
* 
* Examples: COPY NAMETEST 2
* COPY COPY.CM 3
* 
* FORMAT #3 : COPY <Nl> <N2> 
* Where N1, N2 = 1, 2, 3 or 4 All files on the disk in drive Nl are
* copied to disk in drive N2.
* 
* Example: COPY 3 2
* 
* After a copy operation is performed, the system will prompt:
* 'COPY' and wait for another command. To return to MPX/9(tm), strike
*  the carriage return. The COPY utility will also recognize:
* 
* ? N where N = 1, 2, 3 or 4
* 
* to display the files on a drive.
*  
****************************************
*
* WRITTEN BY James Spears Oct 2023
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

mycopy:
        sts     >stack,pcr	; save the stack for error recovery

        clr	copymode,pcr

	; ldd     #0  
       ; std     >opt1,pcr     ; initialize Options
	
       MPX9    SKPSPC	; x -> filename in command line
       lbeq    synerr

	leay    >infcb,pcr	; point to the input fcb
       MPX9    INTFCB	; initialize the fcb
       lbne    dskerr	; report disk error

	lda	FCBNAM,Y
	cmpa	#'1
	lblt	NOT_D2D_COPY
	cmpa	#'4
	lbgt	NOT_D2D_COPY
	suba	#'0		; convert ascii drive letter to drive number
	sta	FCBDRN,Y

	inc	copymode,pcr

NOT_D2D_COPY:

 IFDEF DEBUG_COPY
	leay    >infcb,pcr	; point to the output fcb
	LBSR	DspFCB
 ENDC
	leay    >outfcb,pcr	; point to the output fcb
       MPX9    INTFCB	; initialize the fcb
       lbne    dskerr	; report disk error

; FCBDRN RMB 1 ;FILE DRIVE
; FCBDBA RMB 2 ;DISK BUFFER ADDRESS
; FCBNAM RMB 8 ;FILE NAME
; FCBSUF RMB 2 ;FILE SUFFIX
       ; if FCBNAM[0] 1..4 && FCBNAM[1]==0 && FCBSUF[0]==0

	lda	FCBNAM,Y
	cmpa	#'1
	lblt	NOT_F2D_COPY
	cmpa	#'4
	lbgt	NOT_F2D_COPY

	suba	#'0		; convert ascii drive letter to drive number
	sta	FCBDRN,Y

	tst	FCBNAM+1,Y
	lbne	NOT_F2D_COPY
	tst	FCBSUF,Y
	lbne	NOT_F2D_COPY
        ; then copy to same name new drive
	pshs	x
	leax    >infcb,pcr	; point to the input fcb
	leax	FCBNAM,X	; X point to the input filename
	leay	FCBNAM,Y	; Y point to the output filename
	ldd	#10		; 10 Characters in filename + suffix
	MPX9	BLKMOV
	puls	x

	inc	copymode,pcr

NOT_F2D_COPY:
 IFDEF DEBUG_COPY
	leay	>outfcb,pcr	; point to the output fcb
	LBSR	DspFCB

	lda	copymode,pcr
	MPX9	DSPSBY
       jsr	CRLF
 ENDC
	lda	copymode,pcr
	cmpa	#2
	lbeq	CopyDr2Dr

	; Lbra	mycopyX		; we are done.

; option  MPX9    SKPSPC  ; point to the next word

        ; lda     ,x+
        ; cmpa    #'/     ; look for option flags
        ; bne     copy1file
        ; lda     ,x+     ; get option chanr and bump pointer
        ; cmpa    #'C     ; is a option 'C'?
        ; bne     opti00  ; skip if not
        ; com     >opt1,pcr ; toggle option 'C'
        ; bra     option  ; get next option

; opti00  cmpa    #'F     ; is a option 'F'?
        ; bne     opti01  ; skip if not
        ; com     >ffflg,pcr ; toggle option 'F'
        ; bra     option  ; get next option

; opti01  cmpa    #'N     ; is a option 'N'?
        ; lbne    opti02  ; syntax error if not
        ; com     >numflg,pcr ; toggle option 'N'
        ; bra     option  ; get next option

; opti02  cmpa    #'P     ; is a option 'P'?
        ; lbne    synerr  ; syntax error if not
        ; com     >polflg,pcr ; toggle option 'p'
        ; bra     option  ; get next option

copy1file:
	
;
; open both input and output files.
;

 IFDEF DEBUG_COPY
       lda	#'O
       MPX9    OUTCHR
       jsr	CRLF
 ENDC
	; open the input file
	leay    >infcb,pcr	; point to the input fcb
	leax    >buffer,pcr	; point to file buffer
	lda     #ReadFn	; setup for read
	MPX9    OPNFIL	; open the file
	lbne	dskerr	; report disk error
 IFDEF DEBUG_COPY
	leay	infcb,pcr
	LBSR	DspFCB
 ENDC

	; set correct size for new file.
	; y.FCBEND - y.FCBSTR + 1 --> x.FCBSIZ
	
	leay    >infcb,pcr	; point to the input fcb
	ldd	FCBEND,y
	subd	FCBSTR,y
	addd	#1
	leax    >outfcb,pcr	; point to the output fcb
	std	FCBSIZ,x

 IFDEF DEBUG_COPY
       lda	#'C
       MPX9    OUTCHR
       jsr	CRLF
 ENDC
	; open the output file
	leay    >outfcb,pcr	; point to the output fcb
	leax    >buffer,pcr	; point to file buffer
       lda     #WritFn	; setup for read

       MPX9    OPNFIL	; open/create the file
       Lbne    dskerr	; report disk error

;
; read data from infile, write data to out file.
;
CopyLoop:	
	; ; setup for writing this sector.
	
	; ; write a sector to output
	;	**************************************************
	;	* SYSTEM CALL 24 (WTBLK) - WRITE A BLOCK TO A    *
	;	*                          FILE                  *
	;	*                                                *
	;	* ENTRY REQUIREMENTS:  Y POINTS TO FCB FOR FILE  *
	;	*                                                *
	;	* EXIT CONDITIONS:  B CONTAINS ERROR CODE        *
	;	*                   Z FLAG IN CC SET PER B       *
	;	*                   OTHERS UNCHANGED             *
	;	**************************************************
 IFDEF DEBUG_COPY
       lda	#'W
       MPX9    OUTCHR
       jsr	CRLF
 ENDC
	leax    >infcb,pcr	; point to the input fcb
	leay    >outfcb,pcr	; point to the output fcb

       ldd	FCBCNT,x	; CURRENT BYTE COUNT
       std     FCBCNT,y

       ldd	FCBADD,x	; ADDRESS VECTOR (where it goes in memory when loaded to def location)
       std     FCBADD,y

       ldd	FCBTYP,x	; BLOCK TYPE CODE (USER DEFINED)
       std     FCBTYP,y

       ldd	FCBCUR,x	; CURRENT RELATIVE BLOCK #
       std     FCBCUR,y

       ldd	FCBPRV,x	; PREVIOUS RELATIVE BLOCK #
       std     FCBPRV,y

       ldd	FCBNXT,x	; NEXT RELATIVE BLOCK # 
       std     FCBNXT,y

       ldd	FCBEXT,x	; DIRECTORY EXTENSION BYTES 
       std     FCBEXT,y

 IFDEF DEBUG_COPY
	leay	>outfcb,pcr
	LBSR	DspFCB
 ENDC

	leay	>outfcb,pcr
       MPX9    WTBLK		; Write the sector
       lbne     dskerr	; report disk error

	; setup for reading the next sector.
	leay    >infcb,pcr	; point to the input fcb
	ldd	FCBNXT,Y
	std	FCBCUR,Y
	beq	done

	; read a sector from input
		**************************************************
		* SYSTEM CALL 23 (RDBLK) - READ RANDOM FILE BLOCK*
		*                                                *
		* ENTRY REQUIREMENTS:  Y POINTS TO FCB FOR READ  *
		*                                                *
		* EXIT CONDITIONS:  B CONTAINS ERROR CODE        *
		*                   Z FLAG IN CC SET PER B       *
		*                   OTHERS UNCHANGED             *
		**************************************************
 IFDEF DEBUG_COPY
       lda	#'R
       MPX9    OUTCHR
       jsr	CRLF
 ENDC

       MPX9    RDBLK		; Read the sector
	lbne	dskerr

 IFDEF DEBUG_COPY
	leay	infcb,pcr
	LBSR	DspFCB
 ENDC

	bra	CopyLoop
	
;
; we are done, close files.
;
done:

	leax    >infcb,pcr	; point to the input fcb
       ldd	FCBEXT,x	; DIRECTORY EXTENSION BYTES 
	leax    >outfcb,pcr	; point to the output fcb
       std     FCBEXT,x

	; Close the input file
	leay    >infcb,pcr	; point to the input fcb

 IFDEF DEBUG_COPY
       lda	#'c
       MPX9    OUTCHR
 ENDC

       MPX9    CLSFIL	; close the input file
       lbne     dskerr	; report disk error
 IFDEF DEBUG_COPY
       jsr	CRLF
 ENDC

	leay    >outfcb,pcr	; point to the output fcb
	; lda	FCBSTS,Y	; get the Status Bits
	; ora	#WTBIT		; clear the Write Pending bit.
	; sta	FCBSTS,Y	; save the Status Bits

	ldd	FCBCUR,Y	; workaround for issue where clsfil reads prev sector and
	std	FCBPRV,Y	; re-writes with next-link as zero (eof)

 IFDEF DEBUG_COPY
       lda	#'c
       MPX9    OUTCHR
 ENDC

       MPX9    CLSFIL	; close the output file, this will update the dir-ext value in the directory.
       Lbne     dskerr	; report disk error
       ; jsr	CRLF

	lbra	mycopyX		; we are done.




********************************************************************************************************************************
********************************************************************************************************************************
********************************************************************************************************************************
********************************************************************************************************************************
********************************************************************************************************************************
********************************************************************************************************************************
********************************************************************************************************************************
********************************************************************************************************************************


******************************************************
* DrvGetNumSec
* 
* entry: A - Drive# (0-3)
*        X - point to minidos ddt
* exit: D - number of blocks on drive.
* 
******************************************************
DrvGetNumSec:
	pshs 	X
	CMPA 	#4 IS IT > 4?
	BHI 	DrvGetNumSecXErr EXIT IF YES
	LDB 	#8 DDT ENTRIES 8 BYTES EACH
	MUL 	GET OFFSET INTO DDT
	LEAX 	D,X POINT X AT DDT ENTRY
	LDA 	NUMSEC,X
	LDB 	NUMTRK,X
	mul
	bra 	DrvGetNumSecX
DrvGetNumSecXErr:
	clra 
	clrb
DrvGetNumSecX:
	puls X
	rts

CopyDr2Dr:
	; copy drive FCBDRN,infcb to FCBDRN,outfcb

	; using FCBSTR and FCBEND in infcb for the current and limit block numbers.

	; calc # blocks in src and dst drives.
	; error if not equal

	ldx	#$f000 Minidos ram DDT
	leay	>infcb,pcr
	clr	FCBSTR,Y	; clear FCBSTR used as current block index
	clr	FCBSTR+1,Y
	lda	FCBDRN,Y
	bsr	DrvGetNumSec
	std	FCBEND,Y	; save number of blocks in FCBSTR

	; ldx	#$f000 Minidos ram DDT - must reload DrvGetNumSec changes X
	leay	>outfcb,pcr
	lda	FCBDRN,Y
	bsr	DrvGetNumSec
	leay	>infcb,pcr
	cmpd	FCBEND,Y
	bne	synerr

	; for blk in 0 to #blks

nextblock: 
	;	read block + meta

	leay	>infcb,pcr
	lda	FCBDRN,Y GET DRIVE #
	MPX9	GETDCB

	lda	FCBDRN,Y GET DRIVE #
	sta	DCBDRV,X

	ldd    FCBSTR,Y
	STD	DCBBLK,X

	leay	>buffer,pcr	; point to file buffer
	sty	DCBBUF,X

	ldb	#ReadFn READ THE BLOCK
	MPX9	REQIO

 IFDEF DEBUG_COPY
	;JNS
	pshs	D,Y,X
	jsr	CRLF
	LDA	#'r
	TFR	X,Y 
	LBSR	DspDCB
 	ldd	#$100
	leax	>buffer,pcr	; point to file buffer
	jsr	[DumpMem2v]
	jsr	CRLF
	puls	X,Y,D
 ENDC

	;	write block + meta

	leay	>outfcb,pcr
	lda	FCBDRN,Y GET DRIVE #
	MPX9	GETDCB

	lda	FCBDRN,Y GET DRIVE #
	sta	DCBDRV,X

	ldb	#WritFn READ THE BLOCK
	MPX9	REQIO

 IFDEF DEBUG_COPY
	;JNS
	pshs	D,Y,X
	jsr	CRLF
	LDA	#'w
	TFR	X,Y 
	LBSR	DspDCB
 	ldd	#$100
	; leax	>buffer,pcr	; point to file buffer
	; jsr	[DumpMem2v]
	; jsr	CRLF
	puls	X,Y,D
 ENDC
 
	; 	next block.
	leay	>infcb,pcr
	ldd    FCBSTR,Y
	addd	#1
	std	FCBSTR,Y
	cmpd	FCBEND,Y end of drive?
	blo	nextblock

	bra	mycopyX


;
; exiting
;

mycopyX:        
       clrb
	bra	return

;
; error handling
;
synerr ldb     #ERR_SN
dskerr nop
return lds     >stack,pcr
	tstb
       rts 

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
** Working Variables
*


infcb	rmb	32
outfcb	rmb	32

stack	rmb	2	; save the stack pointer, for error recovery.
opt1	rmb	2	; sample option
copymode rmb	1	; copy mode 0-file2file, 1-file2disk, 2-disk2disk

nbuff	equ	1
buffer	rmb	256*nbuff	; buffers (10)

endpgm  equ     *-1

 end

; COPY UTILITY
; This MPX/9(tm) utility may be used to copy files from one disk
; to another disk. Files of any type may be copied. All disk writes are verified .

; Procedure:
; The COPY utility is loaded from the MPX/9(tm) system disk. There are three command
; line formats. Blanks are used as delimiters between the entries on a command line.
; Errors are reported in MPX/9(tm) codes.

; FORMAT #l:
; COPY <SOURCE> <DESTINATION >
; The first file name is the source file and the second file name is the destination.
; (File names may include drive numbers as discussed in Section III.)
; The copying is performed from source to destination.

; Examples:
; COPY SRCENAME DESTNAME
; 4/COPY SRCENAME DESTNAME
; (Utility in drive #l)
; (Utility in drive #4)

; FORMAT #2:
; COPY <FNAME> <N>
; The named file is copied to a file of the same name on the disk in drive N.

; Examples:
; COPY NAMETEST 2
; COPY COPY 3

; FORMAT #3 :
; COPY <Nl> <N2>
; where N1, N2 = 1, 2, 3 or 4
; All files on the disk in drive Nl are copied to disk in drive N2.

; Example:
; COPY 3 2

; After a copy operation is performed, the system will prompt:
; 'COPY' and wait for another command. To return to MPX/9(tm),
; strike the carriage return.
; The COPY utility will also recognize:

; ? N where N = 1, 2, 3 or 4

; to display the files on a drive.
  
 -----------------------------------------------

load 'my-utils/mycopy.sym
br "dskerr
br 0c37a # rom code in verify
br 0c3a7 # break at verify failure.
br 0f02f write 02 # REPORT EVENT BUT DOES NOT BREAK, AND VALUE NOT IMPLEMENTED.
go
Z


load 'boot-rom/BootRom.sym
br "COMPR1 # break at verify failure.
br 0f02f write 02 # REPORT EVENT BUT DOES NOT BREAK, AND VALUE NOT IMPLEMENTED.
go
Z

MYCOPY 1/LIST.CM 3/LIST.CM

HEXDUMP /F 1/LIST.CM
HEXDUMP /F 3/LIST.CM

3/LIST LIST.AS

MYCOPY 1/LIST.CM 3



========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================

james@james-Satellite-L55-B:~/workspace/6809/percom/mpx9$ make testdv
../../tools/percom/percomd -v -v -1 system.dsk -2 tForth/tforth.dsk -3 blank.dsk -- ../psymon/psymon.ihex ../psymon/psymon-ext.ihex boot-rom/BootRom.ihex

loading file 1,: ../psymon/psymon.ihex 
loading file 2,: ../psymon/psymon-ext.ihex 
loading file 3,: boot-rom/BootRom.ihex 
mount(system.dsk)
mount(tForth/tforth.dsk)
mount(blank.dsk)
mount((null))
Drive:1 file: system.dsk                               dirty:0
Drive:2 file: tForth/tforth.dsk                        dirty:0
Drive:3 file: blank.dsk                                dirty:0
Drive:4 file: (null)                                   dirty:0

Done loading/mounting files.
>> g

Cmd?z
Cmd?Z Z5023::write(@=03,v=40) cDr:1 Step:0 StepDir:-1 Trk:0 
.....



MPX/9 VERSION 1.20 (+NEWSYSDCB) (+CURDRIVE)
COPYRIGHT (c) 1980 BY PERCOM DATA CO. INC.
Loaded @ $AD00 $B100 - $BD70 

MPX?MYCOPY 1 3
Z5023::write(@=03,v=6a) cDr:1 Step:1 StepDir:-1 Trk:0 
Z5023::write(@=03,v=4a) cDr:1 Step:0 StepDir:-1 Trk:0 
Z5023::write(@=00,v=fb) ????????????
Z5023::read(4) read sector current_drive: 1
Z5023::read(4) read sector FILENAME: system.dsk
MPX9_DskImg::ReadSector(t:0, s:2, ...)
block: 1
offset: 268
Z5023::read(1) (ix:-1) --> fb
Z5023::read(1) (ix:0) --> 00
Z5023::read(1) (ix:1) --> 02
Z5023::read(1) (ix:2) --> 00
Z5023::read(1) (ix:3) --> 00
Z5023::read(1) (ix:4) --> 00
Z5023::read(1) (ix:5) --> 02
Z5023::read(1) (ix:6) --> 00
Z5023::read(1) (ix:7) --> 00
Z5023::read(1) (ix:8) --> 00
Z5023::read(1) (ix:9) --> 00
Z5023::read(1) (ix:10) --> 28
Z5023::read(1) (ix:11) --> 53
Z5023::read(1) (ix:12) --> 59
Z5023::read(1) (ix:13) --> 53
Z5023::read(1) (ix:261) --> 6d
Z5023::read(1) (ix:262) --> 00
Z5023::read(1) (ix:263) --> 6d
Z5023::read(1) (ix:264) --> 10
Z5023::read(1) (ix:265) --> 00
Z5023::read(1) (ix:266) --> 52
Z5023::read(1) (ix:267) --> 31
Z5023::write(@=00,v=fb) ????????????
Z5023::read(4) read sector current_drive: 1
Z5023::read(4) read sector FILENAME: system.dsk
MPX9_DskImg::ReadSector(t:0, s:4, ...)
block: 2
offset: 536
Z5023::read(1) (ix:-1) --> fb
Z5023::read(1) (ix:0) --> 00
Z5023::read(1) (ix:1) --> 04
Z5023::read(1) (ix:2) --> 00
Z5023::read(1) (ix:3) --> 01
Z5023::read(1) (ix:4) --> 00
Z5023::read(1) (ix:5) --> 00
Z5023::read(1) (ix:6) --> 00
Z5023::read(1) (ix:7) --> 00
Z5023::read(1) (ix:8) --> 00
Z5023::read(1) (ix:9) --> 00
Z5023::read(1) (ix:10) --> 53
Z5023::read(1) (ix:11) --> 45
Z5023::read(1) (ix:12) --> 54
Z5023::read(1) (ix:13) --> 00
Z5023::read(1) (ix:261) --> 00
Z5023::read(1) (ix:262) --> 00
Z5023::read(1) (ix:263) --> 00
Z5023::read(1) (ix:264) --> 00
Z5023::read(1) (ix:265) --> 00
Z5023::read(1) (ix:266) --> 0c
Z5023::read(1) (ix:267) --> 00
Z5023::write(@=03,v=78) cDr:1 Step:1 StepDir:1 Trk:1 
Z5023::write(@=03,v=58) cDr:1 Step:0 StepDir:1 Trk:1 
Z5023::write(@=03,v=7a) cDr:1 Step:1 StepDir:1 Trk:2 
Z5023::write(@=03,v=5a) cDr:1 Step:0 StepDir:1 Trk:2 
Z5023::write(@=03,v=7a) cDr:1 Step:1 StepDir:1 Trk:3 
Z5023::write(@=03,v=5a) cDr:1 Step:0 StepDir:1 Trk:3 
Z5023::write(@=03,v=7a) cDr:1 Step:1 StepDir:1 Trk:4 
Z5023::write(@=03,v=5a) cDr:1 Step:0 StepDir:1 Trk:4 
Z5023::write(@=03,v=7a) cDr:1 Step:1 StepDir:1 Trk:5 
Z5023::write(@=03,v=5a) cDr:1 Step:0 StepDir:1 Trk:5 
Z5023::write(@=03,v=7a) cDr:1 Step:1 StepDir:1 Trk:6 
Z5023::write(@=03,v=5a) cDr:1 Step:0 StepDir:1 Trk:6 
Z5023::write(@=03,v=7a) cDr:1 Step:1 StepDir:1 Trk:7 
Z5023::write(@=03,v=5a) cDr:1 Step:0 StepDir:1 Trk:7 
Z5023::write(@=03,v=7a) cDr:1 Step:1 StepDir:1 Trk:8 
Z5023::write(@=03,v=5a) cDr:1 Step:0 StepDir:1 Trk:8 
Z5023::write(@=03,v=7a) cDr:1 Step:1 StepDir:1 Trk:9 
Z5023::write(@=03,v=5a) cDr:1 Step:0 StepDir:1 Trk:9 
Z5023::write(@=03,v=7a) cDr:1 Step:1 StepDir:1 Trk:10 
Z5023::write(@=03,v=5a) cDr:1 Step:0 StepDir:1 Trk:10 
Z5023::write(@=00,v=fb) ????????????
Z5023::read(4) read sector current_drive: 1
Z5023::read(4) read sector FILENAME: system.dsk
MPX9_DskImg::ReadSector(t:10, s:1, ...)
block: 105
offset: 28140
Z5023::read(1) (ix:-1) --> fb
Z5023::read(1) (ix:0) --> 0a
Z5023::read(1) (ix:1) --> 01
Z5023::read(1) (ix:2) --> 00
Z5023::read(1) (ix:3) --> 00
Z5023::read(1) (ix:4) --> 00
Z5023::read(1) (ix:5) --> 6a
Z5023::read(1) (ix:6) --> 00
Z5023::read(1) (ix:7) --> 10
Z5023::read(1) (ix:8) --> 00
Z5023::read(1) (ix:9) --> 00
Z5023::read(1) (ix:10) --> 10
Z5023::read(1) (ix:11) --> ef
Z5023::read(1) (ix:12) --> 8d
Z5023::read(1) (ix:13) --> 03
Z5023::read(1) (ix:261) --> 2c
Z5023::read(1) (ix:262) --> ec
Z5023::read(1) (ix:263) --> 0e
Z5023::read(1) (ix:264) --> ed
Z5023::read(1) (ix:265) --> 2e
Z5023::read(1) (ix:266) --> 38
Z5023::read(1) (ix:267) --> 2b
Z5023::write(@=00,v=fb) ????????????
Z5023::read(4) read sector current_drive: 1
Z5023::read(4) read sector FILENAME: system.dsk
MPX9_DskImg::ReadSector(t:10, s:3, ...)
block: 106
offset: 28408
Z5023::read(1) (ix:-1) --> fb
Z5023::read(1) (ix:0) --> 0a
Z5023::read(1) (ix:1) --> 03
Z5023::read(1) (ix:2) --> 00
Z5023::read(1) (ix:3) --> 69
Z5023::read(1) (ix:4) --> 00
Z5023::read(1) (ix:5) --> 6b
Z5023::read(1) (ix:6) --> 00
Z5023::read(1) (ix:7) --> 11
Z5023::read(1) (ix:8) --> 00
Z5023::read(1) (ix:9) --> 00
Z5023::read(1) (ix:10) --> ec
Z5023::read(1) (ix:11) --> 88
Z5023::read(1) (ix:12) --> 10
Z5023::read(1) (ix:13) --> ed
Z5023::read(1) (ix:261) --> 3f
Z5023::read(1) (ix:262) --> 01
Z5023::read(1) (ix:263) --> 34
Z5023::read(1) (ix:264) --> 36
Z5023::read(1) (ix:265) --> bd
Z5023::read(1) (ix:266) --> 1a
Z5023::read(1) (ix:267) --> 77
Z5023::write(@=00,v=fb) ????????????
Z5023::read(4) read sector current_drive: 1
Z5023::read(4) read sector FILENAME: system.dsk
MPX9_DskImg::ReadSector(t:10, s:5, ...)
block: 107
offset: 28676
Z5023::read(1) (ix:-1) --> fb
Z5023::read(1) (ix:0) --> 0a
Z5023::read(1) (ix:1) --> 05
Z5023::read(1) (ix:2) --> 00
Z5023::read(1) (ix:3) --> 6a
Z5023::read(1) (ix:4) --> 00
Z5023::read(1) (ix:5) --> 6c
Z5023::read(1) (ix:6) --> 00
Z5023::read(1) (ix:7) --> 12
Z5023::read(1) (ix:8) --> 00
Z5023::read(1) (ix:9) --> 00
Z5023::read(1) (ix:10) --> fd
Z5023::read(1) (ix:11) --> a2
Z5023::read(1) (ix:12) --> 86
Z5023::read(1) (ix:13) --> 77
Z5023::read(1) (ix:261) --> 43
Z5023::read(1) (ix:262) --> 75
Z5023::read(1) (ix:263) --> 72
Z5023::read(1) (ix:264) --> 2b
Z5023::read(1) (ix:265) --> 42
Z5023::read(1) (ix:266) --> 52
Z5023::read(1) (ix:267) --> c5
Z5023::write(@=00,v=fb) ????????????
Z5023::read(4) read sector current_drive: 1
Z5023::read(4) read sector FILENAME: system.dsk
MPX9_DskImg::ReadSector(t:10, s:7, ...)
block: 108
offset: 28944
Z5023::read(1) (ix:-1) --> fb
Z5023::read(1) (ix:0) --> 0a
Z5023::read(1) (ix:1) --> 07
Z5023::read(1) (ix:2) --> 00
Z5023::read(1) (ix:3) --> 6b
Z5023::read(1) (ix:4) --> 00
Z5023::read(1) (ix:5) --> 00
Z5023::read(1) (ix:6) --> 3d
Z5023::read(1) (ix:7) --> 13
Z5023::read(1) (ix:8) --> 00
Z5023::read(1) (ix:9) --> 00
Z5023::read(1) (ix:10) --> 20
Z5023::read(1) (ix:11) --> 50
Z5023::read(1) (ix:12) --> 72
Z5023::read(1) (ix:13) --> 76
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
133D : 00 01 00 00 31 00 00 00  00 00 00 00 00 00 00 08  ....1...........
134D : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
135D : 00 03 00 00 31 00 00 00  00 00 00 00 00 00 00 08  ....1...........
136D : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St
02 
Z5023::write(@=03,v=6a) cDr:1 Step:1 StepDir:-1 Trk:9 
Z5023::write(@=03,v=4a) cDr:1 Step:0 StepDir:-1 Trk:9 
Z5023::write(@=03,v=6a) cDr:1 Step:1 StepDir:-1 Trk:8 
Z5023::write(@=03,v=4a) cDr:1 Step:0 StepDir:-1 Trk:8 
Z5023::write(@=03,v=6a) cDr:1 Step:1 StepDir:-1 Trk:7 
Z5023::write(@=03,v=4a) cDr:1 Step:0 StepDir:-1 Trk:7 
Z5023::write(@=03,v=6a) cDr:1 Step:1 StepDir:-1 Trk:6 
Z5023::write(@=03,v=4a) cDr:1 Step:0 StepDir:-1 Trk:6 
Z5023::write(@=03,v=6a) cDr:1 Step:1 StepDir:-1 Trk:5 
Z5023::write(@=03,v=4a) cDr:1 Step:0 StepDir:-1 Trk:5 
Z5023::write(@=03,v=6a) cDr:1 Step:1 StepDir:-1 Trk:4 
Z5023::write(@=03,v=4a) cDr:1 Step:0 StepDir:-1 Trk:4 
Z5023::write(@=03,v=6a) cDr:1 Step:1 StepDir:-1 Trk:3 
Z5023::write(@=03,v=4a) cDr:1 Step:0 StepDir:-1 Trk:3 
Z5023::write(@=03,v=6a) cDr:1 Step:1 StepDir:-1 Trk:2 
Z5023::write(@=03,v=4a) cDr:1 Step:0 StepDir:-1 Trk:2 
Z5023::write(@=03,v=6a) cDr:1 Step:1 StepDir:-1 Trk:1 
Z5023::write(@=03,v=4a) cDr:1 Step:0 StepDir:-1 Trk:1 
Z5023::write(@=03,v=6a) cDr:1 Step:1 StepDir:-1 Trk:0 
Z5023::write(@=03,v=4a) cDr:1 Step:0 StepDir:-1 Trk:0 
Z5023::write(@=00,v=fb) ????????????




Z5023::read(4) read sector current_drive: 1
Z5023::read(4) read sector FILENAME: system.dsk
MPX9_DskImg::ReadSector(t:0, s:0, ...)
block: 0
offset: 0
Z5023::read(1) (ix:-1) --> fb
Z5023::read(1) (ix:0) --> 00
Z5023::read(1) (ix:1) --> 00
Z5023::read(1) (ix:2) --> 00
Z5023::read(1) (ix:3) --> 00
Z5023::read(1) (ix:4) --> 00
Z5023::read(1) (ix:5) --> 00
Z5023::read(1) (ix:6) --> 7d
Z5023::read(1) (ix:7) --> 00
Z5023::read(1) (ix:8) --> 00
Z5023::read(1) (ix:9) --> 00
Z5023::read(1) (ix:10) --> 34
Z5023::read(1) (ix:11) --> 70
Z5023::read(1) (ix:12) --> 8e
Z5023::read(1) (ix:13) --> c0

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 00 13 82 00  ..DK............
F030 : 00 00 00 00 00 7D 00 00  00 C9 48 00 00 00 00 00  .....}....H.....
       sc prev_ next_ ct data@  ty crc

1382 : 34 70 8E C0 00 CC 55 AA  30 89 F0 00 A7 84 A1 84  4p....U.0.......
1392 : 26 F6 E7 84 E1 84 26 F0  30 89 FC 00 CC 00 03 ED  &.....&.0.......
13A2 : 89 02 98 10 AF 89 02 92  EE 60 ED 4B 86 01 A7 4A  .........`.K...J
13B2 : EF 89 02 90 30 89 05 00  AF 4D 1E 31 17 00 1E 30  ....0....M.1...0
13C2 : C9 FB 00 86 08 A7 89 02  9A 31 89 02 9B C6 20 E7  .........1.... .
13D2 : A0 8A 80 A7 A0 86 1B A7  89 02 9F 6E C4 34 12 C6  ...........n.4..
13E2 : 01 11 3F 01 E6 08 26 12  4F E6 88 15 26 01 4C E3  ..?...&.O...&.L.
13F2 : 0D ED 0D EC 88 13 ED 0B  26 E5 5D 35 92 00 00 00  ........&.]5....
1402 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1412 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1422 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1432 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1442 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1452 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1462 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
Z5023::write(@=03,v=c0) cDr:3 Step:0 StepDir:-1 Trk:0 0  ................

Z5023::write(@=03,v=f8) cDr:3 Step:1 StepDir:1 Trk:1 
Z5023::write(@=03,v=d8) cDr:3 Step:0 StepDir:1 Trk:1 
Z5023::write(@=03,v=fa) cDr:3 Step:1 StepDir:1 Trk:2 
Z5023::write(@=03,v=da) cDr:3 Step:0 StepDir:1 Trk:2 
Z5023::write(@=03,v=fa) cDr:3 Step:1 StepDir:1 Trk:3 
Z5023::write(@=03,v=da) cDr:3 Step:0 StepDir:1 Trk:3 
Z5023::write(@=03,v=ea) cDr:3 Step:1 StepDir:-1 Trk:2 
Z5023::write(@=03,v=ca) cDr:3 Step:0 StepDir:-1 Trk:2 
Z5023::write(@=03,v=ea) cDr:3 Step:1 StepDir:-1 Trk:1 
Z5023::write(@=03,v=ca) cDr:3 Step:0 StepDir:-1 Trk:1 
Z5023::write(@=03,v=ea) cDr:3 Step:1 StepDir:-1 Trk:0 
Z5023::write(@=03,v=ca) cDr:3 Step:0 StepDir:-1 Trk:0 
Z5023::write(@=02,v=ff) ????????????
Z5023::read(4) read sector current_drive: 3
Z5023::read(4) read sector FILENAME: blank.dsk
MPX9_DskImg::ReadSector(t:0, s:0, ...)
block: 0
offset: 0
Z5023::write(@=04,v=00) Start WriteSector
                                         Z5023::write(1) (ix:-17) <-- 00
Z5023::write(1) (ix:-16) <-- 00
Z5023::write(1) (ix:-15) <-- 00
Z5023::write(1) (ix:-14) <-- 00
Z5023::write(1) (ix:-13) <-- 00
Z5023::write(1) (ix:-12) <-- 00
Z5023::write(1) (ix:-11) <-- 00
Z5023::write(1) (ix:-10) <-- 00
Z5023::write(1) (ix:-9) <-- 00
Z5023::write(1) (ix:-8) <-- 00
Z5023::write(1) (ix:-7) <-- 00
Z5023::write(1) (ix:-6) <-- 00
Z5023::write(1) (ix:-5) <-- 00
Z5023::write(1) (ix:-4) <-- 00
Z5023::write(1) (ix:-3) <-- 00
Z5023::write(1) (ix:-2) <-- 00
Z5023::write(1) (ix:-1) <-- fb
Z5023::write(1) (ix:0) <-- 00
Z5023::write(1) (ix:1) <-- 00
Z5023::write(1) (ix:2) <-- 00
Z5023::write(1) (ix:3) <-- 00
Z5023::write(1) (ix:4) <-- 00
Z5023::write(1) (ix:5) <-- 00
Z5023::write(1) (ix:6) <-- 7d
Z5023::write(1) (ix:7) <-- 00
Z5023::write(1) (ix:8) <-- 00
Z5023::write(1) (ix:9) <-- 00
Z5023::write(1) (ix:10) <-- 34
Z5023::write(1) (ix:11) <-- 70
Z5023::write(1) (ix:12) <-- 8e
Z5023::write(1) (ix:13) <-- c0
MPX9_DskImg::WriteSector(t:0, s:0, ...)
block: 0
offset: 0

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 00 13 82 00  ..DK............
F030 : 00 00 00 00 00 7D 00 00  00 C9 48 00 00 00 00 00  .....}....H.....
Z5023::write(@=03,v=40) cDr:1 Step:0 StepDir:-1 Trk:0 

Z5023::write(@=00,v=fb) ????????????
Z5023::read(4) read sector current_drive: 1
Z5023::read(4) read sector FILENAME: system.dsk
MPX9_DskImg::ReadSector(t:0, s:2, ...)
block: 1
offset: 268
Z5023::read(1) (ix:-1) --> fb
Z5023::read(1) (ix:0) --> 00
Z5023::read(1) (ix:1) --> 02
Z5023::read(1) (ix:2) --> 00
Z5023::read(1) (ix:3) --> 00
Z5023::read(1) (ix:4) --> 00
Z5023::read(1) (ix:5) --> 02
Z5023::read(1) (ix:6) --> 00
Z5023::read(1) (ix:7) --> 00
Z5023::read(1) (ix:8) --> 00
Z5023::read(1) (ix:9) --> 00
Z5023::read(1) (ix:10) --> 28
Z5023::read(1) (ix:11) --> 53
Z5023::read(1) (ix:12) --> 59
Z5023::read(1) (ix:13) --> 53
Z5023::read(1) (ix:261) --> 6d
Z5023::read(1) (ix:262) --> 00
Z5023::read(1) (ix:263) --> 6d
Z5023::read(1) (ix:264) --> 10
Z5023::read(1) (ix:265) --> 00
Z5023::read(1) (ix:266) --> 52
Z5023::read(1) (ix:267) --> 31

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 01 13 82 00  ..DK............
F030 : 02 00 00 00 02 00 00 00  00 52 31 00 00 00 00 00  .........R1.....
       sc prev_ next_ ct data@  ty crc

1382 : 28 53 59 53 44 49 52 29  53 59 00 00 00 02 00 00  (SYSDIR)SY......
1392 : 4D 50 58 39 00 00 00 00  53 59 00 03 00 0F 00 00  MPX9....SY......
13A2 : 43 52 45 41 54 45 00 00  43 4D 00 10 00 11 10 00  CREATE..CM......
13B2 : 44 53 4B 45 44 54 00 00  43 4D 00 12 00 17 10 00  DSKEDT..CM......
13C2 : 45 58 45 43 00 00 00 00  43 4D 00 18 00 19 10 00  EXEC....CM......
13D2 : 4C 49 53 54 00 00 00 00  43 4D 00 1A 00 1B 10 00  LIST....CM......
13E2 : 4D 45 4D 54 53 54 00 00  43 4D 00 1C 00 1C 10 00  MEMTST..CM......
13F2 : 4F 55 54 56 45 43 54 52  43 4D 00 1D 00 1D 10 00  OUTVECTRCM......
1402 : 4C 49 53 54 00 00 00 00  41 53 00 1E 00 61 10 00  LIST....AS...a..
1412 : 44 4F 00 00 00 00 00 00  43 4D 00 62 00 62 10 00  DO......CM.b.b..
1422 : 45 43 48 4F 00 00 00 00  43 4D 00 63 00 63 10 00  ECHO....CM.c.c..
1432 : 48 45 4C 4C 4F 57 4F 52  43 4D 00 64 00 64 10 00  HELLOWORCM.d.d..
1442 : 48 45 58 44 55 4D 50 00  43 4D 00 65 00 66 10 00  HEXDUMP.CM.e.f..
1452 : 4C 49 53 54 44 43 42 00  43 4D 00 67 00 68 10 00  LISTDCB.CM.g.h..
1462 : 4D 59 43 4F 50 59 00 00  43 4D 00 69 00 6C 10 00  MYCOPY..CM.i.l..
Z5023::write(@=03,v=c0) cDr:3 Step:0 StepDir:-1 Trk:0 0  NAME....CM.m.m..

Z5023::write(@=02,v=ff) ????????????
Z5023::read(4) read sector current_drive: 3
Z5023::read(4) read sector FILENAME: blank.dsk
MPX9_DskImg::ReadSector(t:0, s:2, ...)
block: 1
offset: 268
UnMount(blank.dsk) is dirty
Z5023::write(@=04,v=00) Start WriteSector
                                         Z5023::write(1) (ix:-17) <-- 00
Z5023::write(1) (ix:-16) <-- 00
Z5023::write(1) (ix:-15) <-- 00
Z5023::write(1) (ix:-14) <-- 00
Z5023::write(1) (ix:-13) <-- 00
Z5023::write(1) (ix:-12) <-- 00
Z5023::write(1) (ix:-11) <-- 00
Z5023::write(1) (ix:-10) <-- 00
Z5023::write(1) (ix:-9) <-- 00
Z5023::write(1) (ix:-8) <-- 00
Z5023::write(1) (ix:-7) <-- 00
Z5023::write(1) (ix:-6) <-- 00
Z5023::write(1) (ix:-5) <-- 00
Z5023::write(1) (ix:-4) <-- 00
Z5023::write(1) (ix:-3) <-- 00
Z5023::write(1) (ix:-2) <-- 00
Z5023::write(1) (ix:-1) <-- fb
Z5023::write(1) (ix:0) <-- 00
Z5023::write(1) (ix:1) <-- 02
Z5023::write(1) (ix:2) <-- 00
Z5023::write(1) (ix:3) <-- 00
Z5023::write(1) (ix:4) <-- 00
Z5023::write(1) (ix:5) <-- 02
Z5023::write(1) (ix:6) <-- 00
Z5023::write(1) (ix:7) <-- 00
Z5023::write(1) (ix:8) <-- 00
Z5023::write(1) (ix:9) <-- 00
Z5023::write(1) (ix:10) <-- 28
Z5023::write(1) (ix:11) <-- 53
Z5023::write(1) (ix:12) <-- 59
Z5023::write(1) (ix:13) <-- 53
Z5023::write(1) (ix:261) <-- 6d
Z5023::write(1) (ix:262) <-- 00
Z5023::write(1) (ix:263) <-- 6d
Z5023::write(1) (ix:264) <-- 10
Z5023::write(1) (ix:265) <-- 00
Z5023::write(1) (ix:266) <-- 52
Z5023::write(1) (ix:267) <-- 31
MPX9_DskImg::WriteSector(t:0, s:2, ...)
block: 1
offset: 268

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 01 13 82 00  ..DK............
F030 : 02 00 00 00 02 00 00 00  00 52 31 00 00 00 00 00  .........R1.....
Z5023::write(@=03,v=40) cDr:1 Step:0 StepDir:-1 Trk:0 

Z5023::write(@=00,v=fb) ????????????
Z5023::read(4) read sector current_drive: 1
Z5023::read(4) read sector FILENAME: system.dsk
MPX9_DskImg::ReadSector(t:0, s:4, ...)
block: 2
offset: 536
Z5023::read(1) (ix:-1) --> fb
Z5023::read(1) (ix:0) --> 00
Z5023::read(1) (ix:1) --> 04
Z5023::read(1) (ix:2) --> 00
Z5023::read(1) (ix:3) --> 01
Z5023::read(1) (ix:4) --> 00
Z5023::read(1) (ix:5) --> 00
Z5023::read(1) (ix:6) --> 00
Z5023::read(1) (ix:7) --> 00
Z5023::read(1) (ix:8) --> 00
Z5023::read(1) (ix:9) --> 00
Z5023::read(1) (ix:10) --> 53
Z5023::read(1) (ix:11) --> 45
Z5023::read(1) (ix:12) --> 54
Z5023::read(1) (ix:13) --> 00
Z5023::read(1) (ix:261) --> 00
Z5023::read(1) (ix:262) --> 00
Z5023::read(1) (ix:263) --> 00
Z5023::read(1) (ix:264) --> 00
Z5023::read(1) (ix:265) --> 00
Z5023::read(1) (ix:266) --> 0c
Z5023::read(1) (ix:267) --> 00

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 02 13 82 00  ..DK............
F030 : 04 00 01 00 00 00 00 00  00 0C 00 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

1382 : 53 45 54 00 00 00 00 00  43 4D 00 6E 00 6E 10 00  SET.....CM.n.n..
1392 : 52 41 4D 44 49 53 4B 4C  43 4D 00 6F 00 70 10 00  RAMDISKLCM.o.p..
13A2 : 4C 4F 41 44 48 49 00 00  43 4D 00 71 00 72 10 00  LOADHI..CM.q.r..
13B2 : 4D 45 4D 45 44 49 54 00  43 4D 00 73 00 76 10 00  MEMEDIT.CM.s.v..
13C2 : 4D 50 58 39 2B 00 00 00  43 4D 00 77 00 85 10 00  MPX9+...CM.w....
13D2 : 00 00 00 00 00 00 00 00  00 00 00 86 00 00 00 00  ................
13E2 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
13F2 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1402 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1412 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1422 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1432 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1442 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1452 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
1462 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
Z5023::write(@=03,v=c0) cDr:3 Step:0 StepDir:-1 Trk:0 0  ................

Z5023::write(@=02,v=ff) ????????????
Z5023::read(4) read sector current_drive: 3
Z5023::read(4) read sector FILENAME: (null)
MPX9_DskImg::ReadSector(t:0, s:4, ...)
block: 2
offset: 536
UnMount((null)) is dirty
Z5023::write(@=04,v=00) Start WriteSector
                                         Z5023::write(1) (ix:-17) <-- 00
Z5023::write(1) (ix:-16) <-- 00
Z5023::write(1) (ix:-15) <-- 00
Z5023::write(1) (ix:-14) <-- 00
Z5023::write(1) (ix:-13) <-- 00
Z5023::write(1) (ix:-12) <-- 00
Z5023::write(1) (ix:-11) <-- 00
Z5023::write(1) (ix:-10) <-- 00
Z5023::write(1) (ix:-9) <-- 00
Z5023::write(1) (ix:-8) <-- 00
Z5023::write(1) (ix:-7) <-- 00
Z5023::write(1) (ix:-6) <-- 00
Z5023::write(1) (ix:-5) <-- 00
Z5023::write(1) (ix:-4) <-- 00
Z5023::write(1) (ix:-3) <-- 00
Z5023::write(1) (ix:-2) <-- 00
Z5023::write(1) (ix:-1) <-- fb
Z5023::write(1) (ix:0) <-- 00
Z5023::write(1) (ix:1) <-- 04
Z5023::write(1) (ix:2) <-- 00
Z5023::write(1) (ix:3) <-- 01
Z5023::write(1) (ix:4) <-- 00
Z5023::write(1) (ix:5) <-- 00
Z5023::write(1) (ix:6) <-- 00
Z5023::write(1) (ix:7) <-- 00
Z5023::write(1) (ix:8) <-- 00
Z5023::write(1) (ix:9) <-- 00
Z5023::write(1) (ix:10) <-- 53
Z5023::write(1) (ix:11) <-- 45
Z5023::write(1) (ix:12) <-- 54
Z5023::write(1) (ix:13) <-- 00
Z5023::write(1) (ix:261) <-- 00
Z5023::write(1) (ix:262) <-- 00
Z5023::write(1) (ix:263) <-- 00
Z5023::write(1) (ix:264) <-- 00
Z5023::write(1) (ix:265) <-- 00
Z5023::write(1) (ix:266) <-- 0c
Z5023::write(1) (ix:267) <-- 00
MPX9_DskImg::WriteSector(t:0, s:4, ...)
block: 2
offset: 536

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 02 13 82 00  ..DK............
F030 : 04 00 01 00 00 00 00 00  00 0C 00 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

MPX?


############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################


I 3
F 3
CREATE 3/FOO.TX /#
F 3
HEXDUMP /F 3/FOO.TX
