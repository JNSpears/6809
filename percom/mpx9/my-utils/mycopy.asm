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
        sts     >stack,pcr     	; save the stack for error recovery

        clr	copymode,pcr

	; ldd     #0  
        ; std     >opt1,pcr     ; initialize Options
	
        MPX9    SKPSPC  	; x -> filename in command line
        lbeq    synerr

	leay    >infcb,pcr 	; point to the input fcb
        MPX9    INTFCB  	; initialize the fcb
        lbne    dskerr  	; report disk error

	lda	FCBNAM,Y
	cmpa	#'1
	lblt	NOT_D2D_COPY
	cmpa	#'4
	lbgt	NOT_D2D_COPY
	suba	#'0		; convert ascii drive letter to drive number
	sta	FCBDRN,Y

	inc	copymode,pcr

NOT_D2D_COPY:

 IFDEF DEBUG
	leay    >infcb,pcr 	; point to the output fcb
	LBSR	DspFCB
 ENDC
	leay    >outfcb,pcr 	; point to the output fcb
        MPX9    INTFCB  	; initialize the fcb
        lbne    dskerr  	; report disk error

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
	pshs 	x
	leax    >infcb,pcr	; point to the input fcb
	leax	FCBNAM,X	; X point to the input filename
	leay	FCBNAM,Y	; Y point to the output filename
	ldd	#10		; 10 Characters in filename + suffix
	MPX9	BLKMOV
	puls	x

	inc	copymode,pcr

NOT_F2D_COPY:
 IFDEF DEBUG
	leay	>outfcb,pcr 	; point to the output fcb
	LBSR	DspFCB

	lda	copymode,pcr
	MPX9	DSPSBY
        jsr	CRLF
 ENDC
	lda	copymode,pcr
	cmpa 	#2
	lbeq 	CopyDr2Dr

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

 IFDEF DEBUG
        lda	#'O
        MPX9    OUTCHR
        jsr	CRLF
 ENDC
	; open the input file
	leay    >infcb,pcr	; point to the input fcb
	leax    >buffer,pcr	; point to file buffer
        lda     #ReadFn 	; setup for read
        MPX9    OPNFIL	; open the file
        lbne 	dskerr	; report disk error
 IFDEF DEBUG
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

 IFDEF DEBUG
        lda	#'C
        MPX9    OUTCHR
        jsr	CRLF
 ENDC
	; open the output file
	leay    >outfcb,pcr	; point to the output fcb
	leax    >buffer,pcr	; point to file buffer
        lda     #WritFn 	; setup for read

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
 IFDEF DEBUG
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

 IFDEF DEBUG
	leay	>outfcb,pcr
	LBSR	DspFCB
 ENDC

	leay	>outfcb,pcr
        MPX9    WTBLK 		; Write the sector
        bne     dskerr 	; report disk error

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
 IFDEF DEBUG
        lda	#'R
        MPX9    OUTCHR
        jsr	CRLF
 ENDC

        MPX9    RDBLK 		; Read the sector
	bne	dskerr

 IFDEF DEBUG
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

 IFDEF DEBUG
        lda	#'c
        MPX9    OUTCHR
 ENDC

        MPX9    CLSFIL 	; close the input file
        bne     dskerr 	; report disk error
 IFDEF DEBUG
        jsr	CRLF
 ENDC

	leay    >outfcb,pcr	; point to the output fcb
	; lda	FCBSTS,Y	; get the Status Bits
	; ora	#WTBIT		; clear the Write Pending bit.
	; sta	FCBSTS,Y	; save the Status Bits

	ldd	FCBCUR,Y	; workaround for issue where clsfil reads prev sector and
	std	FCBPRV,Y	; re-writes with next-link as zero (eof)

 IFDEF DEBUG
        lda	#'c
        MPX9    OUTCHR
 ENDC

        MPX9    CLSFIL 	; close the output file, this will update the dir-ext value in the directory.
        bne     dskerr 	; report disk error
        ; jsr	CRLF

	bra	mycopyX		; we are done.

CopyDr2Dr:
	; copy drive FCBDRN,infcb to FCBDRN,outfcb

	; calc # blocks in src and dst drives.
	; error if not equal

	; for blk in 0 to #blks
	; 	read block + meta
	; 	write block + meta



; ******************************************************
; * MINIDOS/9 RAM DEFINITIONS (128 BYTES MAX) @ $f000  *
; ******************************************************
;  ORG M9RAM
;  SPC 1
; * DRIVE DESCRIPTION TABLE
; DDT RMB 32
;  SPC 1
; * ONE ENTRY PER DRIVE DEFINED AS FOLLOWS:
; CURTRK EQU 0 CURRENT TRACK # (HEAD POSITION)
; NUMTRK EQU 1 NUMBER OF TRACKS FOR THIS DRIVE
; NUMSEC EQU 2 NUMBER OF SECTORS PER TRACK
; STEPTM EQU 3 STEP TIME (IN MSEC) FOR THIS DRIVE
; SETLTM EQU 4 SETTLE TIME (IN MSEC) FOR THIS DRIVE
; SILTBL EQU 5 ADDRESS IN SECTOR INTERLACE TABLE
; * ONE BYTE RESERVED FOR EXPANSION



;
; exiting
;

mycopyX:        
        clrb
	bra	return

;
; error handling
;
synerr  ldb     #ERR_SN
dskerr  nop
return  lds     >stack,pcr
	tstb
        rts 

 IFDEF DEBUG

DspFCB:		; Y -> FCB
	pshs    x
	leax	FCB0,PCR
	MPX9    PSTRNG
	ldd	#32
	tfr 	y,x
	jsr	[DumpMem2v]
        jsr	CRLF
	leax	FCB1,PCR
	MPX9    PSTRNG
        jsr	CRLF
	puls    x,pc

DspDCB:		; Y -> DCB
	pshs    x
	leax	DCB0,PCR
	MPX9    PSTRNG
	ldd	#32
	tfr 	y,x
	jsr	[DumpMem2v]
        jsr	CRLF
	leax	DCB0,PCR
	MPX9    PSTRNG
        jsr	CRLF
	puls    x,pc



DCB0: FCS "       Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr"
DCB1: FCS "       sc prev_ next_ ct data@  ty crc"

FCB0: FCS "       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B"
FCB1: FCS "       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St"

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

../../tools/percom/percomd -1 system.dsk -2 tForth/tforth.dsk -3 blank.dsk -- ../psymon/psymon.ihex ../psymon/psymon-ext.ihex boot-rom/BootRom.ihex

loading file 1,: ../psymon/psymon.ihex 
loading file 2,: ../psymon/psymon-ext.ihex 
loading file 3,: boot-rom/BootRom.ihex 
loading DskFile 1: system.dsk 
loading DskFile 2: tForth/tforth.dsk 
loading DskFile 3: blank.dsk 
loading DskFile 4: (null) 
Done loading files
>> G

Cmd?Z 
MPX/9 VERSION 1.20 (+NEWSYSDCB) (+CURDRIVE)
COPYRIGHT (c) 1980 BY PERCOM DATA CO. INC.
Loaded @ $AD00 $B100 - $BE95 

MPX?MYCOPY 1/LIST.CM 3/LIST.CM
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1235 : 00 01 00 00 4C 49 53 54  00 00 00 00 43 4D 00 08  ....LIST....CM..
1245 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1255 : 00 03 00 00 4C 49 53 54  00 00 00 00 43 4D 00 08  ....LIST....CM..
1265 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St
O

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 1B 12 79 00  ..DK..........y.
F030 : 04 00 01 00 00 00 B0 00  00 8C 05 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1235 : 01 01 12 79 00 10 00 00  00 1B 00 1C 00 00 00 00  ...y............
1245 : 00 01 10 00 12 79 00 00  00 00 00 00 00 00 00 81  .....y..........
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St
C
W
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1255 : 02 03 12 79 00 10 00 00  00 11 00 12 00 00 00 00  ...y............
1265 : 00 01 10 00 12 79 00 00  00 00 00 00 00 00 00 80  .....y..........
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 11 12 79 00  ..DK..........y.
F030 : 04 00 00 00 12 00 10 00  00 00 0D 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc
R

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 1C 12 79 01  ..DK..........y.
F030 : 05 00 00 00 12 00 10 00  00 CE 9E 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1235 : 01 01 12 79 EE 11 00 00  00 1B 00 1C 00 01 00 00  ...y............
1245 : 00 00 10 00 12 79 00 00  00 00 00 00 00 00 00 81  .....y..........
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St
W
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1255 : 02 03 12 79 EE 11 00 00  00 11 00 12 00 01 00 00  ...y............
1265 : 00 00 10 00 12 79 00 00  00 00 00 00 00 00 00 80  .....y..........
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 12 12 79 02  ..DK..........y.
F030 : 07 00 00 00 00 EE 11 00  00 6B 69 00 00 00 00 00  .........ki.....
       sc prev_ next_ ct data@  ty crc
c
c
r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 11 12 79 00  ..DK..........y.
F030 : 04 00 01 00 00 00 B0 00  00 8C 05 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 11 12 79 01  ..DK..........y.
F030 : 05 00 00 00 00 00 10 00  00 CE 9E 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc


MPX?

MPX?

MPX?HEXDUMP /F 1/LIST.CM

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 1B 11 3D 00  ..DK..........=.
F030 : 04 00 01 00 00 00 B0 00  00 8C 05 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 1B 11 3D 02  ..DK..........=.
F030 : 05 00 00 00 1C 00 10 00  00 0E 93 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

0000 : 10 EF 8D 01 F5 CC 00 00  ED 8D 01 F0 ED 8D 01 E8  ................
0010 : 86 00 A7 8D 01 DD 86 00  A7 8D 01 D6 86 00 A7 8D  ................
0020 : 01 D2 86 FF A7 8D 01 CD  86 00 A7 8D 01 C8 6F 8D  ..............o.
0030 : 01 C5 10 BE FF DE 31 A8  6A 10 AF 8D 01 B0 11 3F  ......1.j......?
0040 : 0A 10 27 01 9E 81 23 26  22 30 01 EC 81 81 41 10  ..'...#&"0....A.
0050 : 25 01 90 C1 30 10 25 01  8A 1F 12 11 3F 21 AF 8D  %...0.%.....?!..
0060 : 01 9A 1F 21 10 27 01 77  11 3F 0A 31 8D 01 8F 11  ...!.'.w.?.1....
0070 : 3F 12 10 26 01 6F 6D 2C  26 05 CC 54 58 ED 2C 34  ?..&.om,&..TX.,4
0080 : 10 30 8D 01 99 86 01 11  3F 13 35 10 10 26 01 55  .0......?.5..&.U
0090 : 11 3F 0A 27 47 A6 80 81  2F 26 41 A6 80 81 43 26  .?.'G.../&A...C&
00A0 : 06 63 8D 01 4D 20 E9 81  46 26 06 63 8D 01 44 20  .c..M ..F&.c..D 
00B0 : DF 81 4E 26 06 63 8D 01  3B 20 D5 81 50 26 06 63  ..N&.c..; ..P&.c
00C0 : 8D 01 32 20 CB 81 52 10  26 01 18 A6 80 81 3A 10  ..2 ..R.&.....:.
00D0 : 26 01 10 11 3F 1D E7 8D  01 1C 20 B4 6F 8D 01 10  &...?..... .o...
00E0 : EC 8D 01 14 CB 01 1E 89  19 1E 89 89 00 19 ED 8D  ................
00F0 : 01 06 6D 8D 00 FE 27 1B  6F 8D 00 F5 34 02 17 00  ..m...'.o...4...
r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 1C 11 3D 02  ..DK..........=.
F030 : 05 00 00 00 1C 00 10 00  00 0E 93 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

0100 : 8A 35 02 17 00 89 1F 98  17 00 80 1F 98 17 00 7F  .5..............
0110 : 17 00 86 31 8D 00 E7 11  3F 15 C1 10 10 27 00 3C  ...1....?....'.<
0120 : 5D 10 26 00 C0 84 7F 81  7F 27 E8 81 04 10 27 00  ].&......'....'.
0130 : 2B 81 0A 26 02 20 DC 81  0D 27 10 8D 66 A6 8D 00  +..&. ...'..f...
0140 : B5 27 D0 A1 8D 00 B0 25  02 20 C8 A6 8D 00 A7 26  .'.....%. .....&
0150 : C2 8D 23 81 36 25 89 8D  03 16 FF 80 AE 8D 00 9C  ..#.6%..........
0160 : 27 12 6D 8D 00 8D 27 06  86 0C 8D 37 20 06 8D 06  '.m...'....7 ...
0170 : 81 42 26 FA 5F 39 86 0D  8D 29 86 0A 8D 25 6F 8D  .B&._9...)...%o.
0180 : 00 75 6C 8D 00 6A A6 8D  00 66 39 47 47 47 47 84  .ul..j...f9GGGG.
0190 : 0F 26 0A 6D 8D 00 5A 26  04 86 20 20 06 8A 30 A7  .&.m..Z&..  ..0.
01A0 : 8D 00 4E 34 04 6C 8D 00  4E 6D 8D 00 48 27 1A 34  ..N4.l..Nm..H'.4
01B0 : 02 AE 9D 00 39 C6 04 11  3F 01 85 01 27 09 C6 01  ....9...?...'...
01C0 : 11 3F 01 81 03 27 21 35  02 AE 8D 00 2F 27 0B C6  .?...'!5..../'..
01D0 : 02 11 3F 01 6D 8D 00 1A  27 03 11 3F 02 35 84 C6  ..?.m...'..?.5..
01E0 : 0A 20 02 C6 11 11 3F 0D  10 EE 8D 00 0D 39 ED 8D  . ....?......9..
01F0 : 01 06 6D 8D 00 FE 27 1B  6F 8D 00 F5 34 02 17 00  ..m...'.o...4...
MPX?HEXDUMP /F 3/LIST.CM

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 11 11 3D 00  ..DK..........=.
F030 : 04 00 01 00 00 00 B0 00  00 00 0D 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 11 11 3D 01  ..DK..........=.
F030 : 05 00 00 00 00 00 10 00  00 8E 9C 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

0000 : 10 EF 8D 01 F5 CC 00 00  ED 8D 01 F0 ED 8D 01 E8  ................
0010 : 86 00 A7 8D 01 DD 86 00  A7 8D 01 D6 86 00 A7 8D  ................
0020 : 01 D2 86 FF A7 8D 01 CD  86 00 A7 8D 01 C8 6F 8D  ..............o.
0030 : 01 C5 10 BE FF DE 31 A8  6A 10 AF 8D 01 B0 11 3F  ......1.j......?
0040 : 0A 10 27 01 9E 81 23 26  22 30 01 EC 81 81 41 10  ..'...#&"0....A.
0050 : 25 01 90 C1 30 10 25 01  8A 1F 12 11 3F 21 AF 8D  %...0.%.....?!..
0060 : 01 9A 1F 21 10 27 01 77  11 3F 0A 31 8D 01 8F 11  ...!.'.w.?.1....
0070 : 3F 12 10 26 01 6F 6D 2C  26 05 CC 54 58 ED 2C 34  ?..&.om,&..TX.,4
0080 : 10 30 8D 01 99 86 01 11  3F 13 35 10 10 26 01 55  .0......?.5..&.U
0090 : 11 3F 0A 27 47 A6 80 81  2F 26 41 A6 80 81 43 26  .?.'G.../&A...C&
00A0 : 06 63 8D 01 4D 20 E9 81  46 26 06 63 8D 01 44 20  .c..M ..F&.c..D 
00B0 : DF 81 4E 26 06 63 8D 01  3B 20 D5 81 50 26 06 63  ..N&.c..; ..P&.c
00C0 : 8D 01 32 20 CB 81 52 10  26 01 18 A6 80 81 3A 10  ..2 ..R.&.....:.
00D0 : 26 01 10 11 3F 1D E7 8D  01 1C 20 B4 6F 8D 01 10  &...?..... .o...
00E0 : EC 8D 01 14 CB 01 1E 89  19 1E 89 89 00 19 ED 8D  ................
00F0 : 01 06 6D 8D 00 FE 27 1B  6F 8D 00 F5 34 02 17 00  ..m...'.o...4...
MPX?

9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999

Cmd?Z 
MPX/9 VERSION 1.20 (+NEWSYSDCB) (+CURDRIVE)
COPYRIGHT (c) 1980 BY PERCOM DATA CO. INC.
Loaded @ $AD00 $B100 - $BE95 

MPX?MYCOPY 1/LIST.CM 3/LIST.CM
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1235 : 00 01 00 00 4C 49 53 54  00 00 00 00 43 4D 00 08  ....LIST....CM..
1245 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1255 : 00 03 00 00 4C 49 53 54  00 00 00 00 43 4D 00 08  ....LIST....CM..
1265 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St
O

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 1B 12 79 00  ..DK..........y.
F030 : 04 00 01 00 00 00 00 00  00 0C 00 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1235 : 01 01 12 79 00 10 00 00  00 1B 00 1C 00 00 00 00  ...y............
1245 : 00 01 10 00 12 79 00 00  00 00 00 00 00 00 00 81  .....y..........
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St
C
W
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1255 : 02 03 12 79 00 10 00 00  00 11 00 12 00 00 00 00  ...y............
1265 : 00 01 10 00 12 79 00 00  00 00 00 00 00 00 00 80  .....y..........
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 11 12 79 00  ..DK..........y.
F030 : 04 00 00 00 12 00 10 00  00 00 0D 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc
R

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 1C 12 79 01  ..DK..........y.
F030 : 05 00 00 00 12 00 10 00  00 CE 9E 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1235 : 01 01 12 79 EE 11 00 00  00 1B 00 1C 00 01 00 00  ...y............
1245 : 00 00 10 00 12 79 00 00  00 00 00 00 00 00 00 81  .....y..........
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St
W
       RW D# Buff@ C# @Vect TY  S-Blk E-Blk Cur+B Prv+B
1255 : 02 03 12 79 EE 11 00 00  00 11 00 12 00 01 00 00  ...y............
1265 : 00 00 10 00 12 79 00 00  00 00 00 00 00 00 00 80  .....y..........
       Nxt+B D-ext DataP xx xx  xx xx xx xx xx xx xx St

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 12 12 79 02  ..DK..........y.
F030 : 07 00 00 00 00 EE 11 00  00 6B 69 00 00 00 00 00  .........ki.....
       sc prev_ next_ ct data@  ty crc
c
c
r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 11 12 79 00  ..DK..........y.
F030 : 04 00 01 00 00 00 B0 00  00 8C 05 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 11 12 79 01  ..DK..........y.
F030 : 05 00 00 00 00 00 10 00  00 CE 9E 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc


MPX?

MPX?

MPX?

MPX?

MPX?CREATE 3/FOO.TX,8 /#

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 13 11 81 00  ..DK............
F030 : 04 00 00 00 14 00 00 81  00 00 0D 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 14 11 81 01  ..DK............
F030 : 09 00 00 00 15 00 00 81  00 84 16 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 15 11 81 02  ..DK............
F030 : 00 00 14 00 16 00 00 81  00 A4 08 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 16 11 81 02  ..DK............
F030 : 02 00 15 00 17 00 00 81  00 C4 06 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 17 11 81 02  ..DK............
F030 : 04 00 16 00 18 00 00 81  00 64 0A 00 00 00 00 00  .........d......
       sc prev_ next_ ct data@  ty crc

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 18 11 81 02  ..DK............
F030 : 06 00 17 00 19 00 00 81  00 04 0E 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 19 11 81 02  ..DK............
F030 : 08 00 18 00 1A 00 00 81  00 A4 12 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

w      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 1A 11 81 02  ..DK............
F030 : 01 00 19 00 00 FF 00 81  00 44 07 00 00 00 00 00  .........D......
       sc prev_ next_ ct data@  ty crc

MPX?                

MPX?

MPX?F 3

FILE NAME      START   END    EXT
(SYSDIR).SY        0     2    0000 
MPX9.SY            3    16    0000 
LIST.CM           17    18    1000 
FOO.TX            19    26    0000 
                  27     0    0000 

MPX?HEXDUMP 3/FOO.TX

*** MPX/9 ERROR SN (   17 ) ***

MPX?HEXDUMP 3/FOO.TX

*** MPX/9 ERROR SN (   17 ) ***

MPX?

MPX?

MPX?

MPX?F 1

FILE NAME      START   END    EXT
(SYSDIR).SY        0     2    0000 
MPX9.SY            3    16    0000 
CREATE.CM         17    18    1000 
DSKEDT.CM         19    24    1000 
EXEC.CM           25    26    1000 
LIST.CM           27    28    1000 
MEMTST.CM         29    29    1000 
OUTVECTR.CM       30    30    1000 
LIST.AS           31    98    1000 
DO.CM             99    99    1000 
ECHO.CM          100   100    1000 
HELLOWOR.CM      101   101    1000 
HEXDUMP.CM       102   103    1000 
LISTDCB.CM       104   105    1000 
MYCOPY.CM        106   108    1000 
NAME.CM          109   109    1000 
SET.CM           110   110    1000 
RAMDISKL.CM      111   112    1000 
LOADHI.CM        113   114    1000 
MEMEDIT.CM       115   118    1000 
MPX9+.CM         119   133    1000 
                 134     0    0000 

MPX?HEXDUMP /F 3/FOO.TX

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 13 11 3D 00  ..DK..........=. READ REQ
F030 : 04 00 01 00 00 00 B0 00  00 00 0D 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 13 11 3D 01  ..DK..........=. READ RESPONCE TR..CRC UPDATED..... <<<<<<<<<
F030 : 09 00 00 00 14 00 00 81  00 84 16 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

0000 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0010 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0020 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0030 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0040 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0050 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0060 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0070 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0080 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0090 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
00A0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
00B0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
00C0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
00D0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
00E0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
00F0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 14 11 3D 01  ..DK..........=.
F030 : 09 00 00 00 14 00 00 81  00 84 16 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

0100 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0110 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0120 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0130 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0140 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0150 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0160 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0170 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0180 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0190 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
01A0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
01B0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
01C0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
01D0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
01E0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
01F0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 15 11 3D 02  ..DK..........=.
F030 : 00 00 00 00 15 00 00 81  00 A4 08 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

0200 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0210 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0220 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0230 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0240 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0250 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0260 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0270 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0280 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0290 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
02A0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
02B0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
02C0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
02D0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
02E0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
02F0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 16 11 3D 02  ..DK..........=.
F030 : 02 00 14 00 16 00 00 81  00 C4 06 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

0300 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0310 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0320 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0330 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0340 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0350 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0360 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0370 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0380 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0390 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
03A0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
03B0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
03C0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
03D0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
03E0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
03F0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 17 11 3D 02  ..DK..........=.
F030 : 04 00 15 00 17 00 00 81  00 64 0A 00 00 00 00 00  .........d......
       sc prev_ next_ ct data@  ty crc

0400 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0410 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0420 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0430 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0440 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0450 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0460 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0470 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0480 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0490 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
04A0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
04B0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
04C0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
04D0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
04E0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
04F0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 18 11 3D 02  ..DK..........=.
F030 : 06 00 16 00 18 00 00 81  00 04 0E 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

0500 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0510 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0520 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0530 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0540 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0550 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0560 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0570 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0580 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0590 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
05A0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
05B0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
05C0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
05D0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
05E0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
05F0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 19 11 3D 02  ..DK..........=.
F030 : 08 00 17 00 19 00 00 81  00 A4 12 00 00 00 00 00  ................
       sc prev_ next_ ct data@  ty crc

0600 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0610 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0620 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0630 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0640 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0650 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0660 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0670 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0680 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0690 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
06A0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
06B0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
06C0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
06D0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
06E0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
06F0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
r      Nxt__ ID___ Drv@_ I/O@_  Er ex Dr blk__ buff@ tr
F020 : F2 00 44 4B C0 BC F0 00  00 16 03 00 1A 11 3D 02  ..DK..........=.
F030 : 01 00 18 00 1A 00 00 81  00 44 07 00 00 00 00 00  .........D......
       sc prev_ next_ ct data@  ty crc

0700 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0710 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0720 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0730 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0740 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0750 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0760 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0770 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0780 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
0790 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
07A0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
07B0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
07C0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
07D0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
07E0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
07F0 : 04 04 04 04 04 04 04 04  04 04 04 04 04 04 04 04  ................
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
