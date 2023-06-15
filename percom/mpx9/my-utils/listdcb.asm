 nam ListDBC
*********************************************************
* ListDBC.CM                            JNS 4/23/2014   *
*                                                       *
*                                                       *
*********************************************************

	INCLUDE mpx9.i
	INCLUDE psymon-ext.i

**************************************************
* Program (Position independant)
**************************************************
        ORG     $1000

ListDCBs:
	clr     >verbose,pcr    ; initialize variables
    clr     >sysdcbs,pcr
    
option:
	swi3
	fcb     SKPSPC          ; point to the next word
	beq     DoListDCB       ; No arguments

	lda     ,x+
	cmpa    #'/             ; look for option flags
	bne     DoListDCB       ; Not a switch

option_V:
	lda     ,x+             ; get option char and bump pointer

	cmpa    #'V             ; is a option 'V'?
	bne     Option_S        ; bad switch

	com     >verbose,pcr    ; toggle option 'V'
	bra     option          ; get next option

Option_S:
	cmpa    #'S             ; is a option 'S'?
	bne     synerr          ; bad switch

	com     >sysdcbs,pcr    ; toggle option 'S'
	bra     option          ; get next option

synerr:
	ldb     #ERR_SN         ; Error Syntax
	rts
	
DoListDCB:

    tst     >sysdcbs,pcr
    beq     Nosysdcbs
    leax    >sysdcbprefix,pcr
xxxx:
	jsr	    [PStringv]  ; display entry name

	ldy	    [,X++]
    ldd     DCBDId,y
    
    jsr	    [OutChrv]
    tfr     b,a
    jsr	    [OutChrv]

    jsr     OutSp

    tst     ,X
    bne     xxxx

	; LDD		#"DK
	; SWI3
	; FCB		LOCDCB				; Locate DCB
	; beq		Nosysdcbs
	; 	; X -> mpx9 DKDCB @ 20 in mpx9 ram

	; 	tfr		X,D
	; 	jsr		[DspDByv]

	; 	ldd     DCBDId,X
	; 	jsr	    [OutChrv]
	; 	tfr     b,a
	; 	jsr	    [OutChrv]

	; 	****
	; 	TFR		S,X
    ;     LDD		#$100		; dump specified # of bytes of data
    ;     JSR     [DumpMem2v]	; dump mpx9 data.
    ; 	jsr		CRLF
	; 	****
	; 	LEAY	11,S		; Y -> back into stack
	; 	LDY		,Y			; get return address from first call in command processor loop
	; 						; (os/mpx9.asm):00468          LBSR PROCM PROCESS CURRENT COMMAND
	; 	LEAY	-$611,Y		; move back to get base address of mpx9
    ;     LDD		#$400		; dump specified # of bytes of data
    ;     TFR		Y,X
    ;     JSR     [DumpMem2v]	; dump mpx9 data.
    ; 	jsr		CRLF
	; 	****


**************************************************

; 0290                  (         mpx9.asm):00273         SYSDCB RMB 2 POINTER TO SYSTEM DISK DCB
; 0292                  (         mpx9.asm):00274         MDSBAS RMB 2 MINIDOS/9 RAM BASE ADDRESS
; 0294                  (         mpx9.asm):00275         SCLVEC RMB 2 EXTENDED SWI3 CALL VECTOR
; 0296                  (         mpx9.asm):00276         DEVLST RMB 2 SYSTEM DEVICE LIST POINTER
; 0298                  (         mpx9.asm):00277         MPXLOC RMB 2 MPX/9 DISK LOCATION
;                       (         mpx9.asm):00278          SPC 1
;                       (         mpx9.asm):00279         * CONFIGURATION PARAMETERS
; 029A                  (         mpx9.asm):00280         SYSBS RMB 1 SYSTEM BACKSPACE CODE
; 029B                  (         mpx9.asm):00281         SYSBSE RMB 4 SYSTEM BACKSPACE ECHO STRING
; 029F                  (         mpx9.asm):00282         SYSCAN RMB 1 SYSTEM CANCEL CODE
;                       (         mpx9.asm):00283         
;                       (         mpx9.asm):00284          IFDEF NEWSYSDCB
; 02A0                  (         mpx9.asm):00285         SYSDCBn RMB 2 ;NUMBER OF SYSDCB'S (DRIVES) IN SYSTEM
; 02A0                  (         mpx9.asm):00286         SYSDCBv RMB 2*MaxDrv ; POINTER TO DCB FOR EACH DRIVE
;                       (         mpx9.asm):00287          ENDC



	    leax    >SYSDSKPREFIX,pcr
		jsr	    [PStringv]  ; display entry name

		SWI3
		FCB		GETBAS


        IFDEF NEWSYSDCB
			tfr		X,D
			jsr		[DspDByv]
			leax 	$01a0,X 		; Y --> SYSDCBn

				tfr		x,D
				jsr		[DspDByv]

			ldb 	,x+		d=dcb count ( b lsb ignore a )
diskloop:
			ldy     ,x++
			lda     DCBDId,y
	        jsr	    [OutChrv]
		    lda 	DCBDId+1,y
		    jsr	    [OutChrv]

	   		jsr     OutSp
	   		decb
	   		bne 	diskloop
		ELSE
			leay 	$0190,X 		; Y --> SYSDCB
				; tfr		Y,D
				; jsr		[DspDByv]

			ldy 	,Y

			ldd     DCBDId,y


	    
		    jsr	    [OutChrv]
		    tfr     b,a
		    jsr	    [OutChrv]

	   		jsr     OutSp
        ENDC

        jsr		CRLF		; display CRLF

Nosysdcbs:
	LEAX	DmyDcb,PCR			; X -> Dummy DCB
	clr		DCBLnk,X			; clear link to next DCB
	clr		DCBLnk+1,X
	LDD		DmyID,PCR			; set DCB Device ID 
	STD		DCBDId,X
	SWI3
	FCB		ADDDCB				; add to DCB list
	SWI3
	FCB		DELDCB				; remove from DCB list
	LDU		DmyDcb+DCBLnk,PCR	; Now we have a pointer to the MPX/9 Device list.

ListDCB_loop:

	jsr		CRLF


	; Get sanitize and print first char of DCB ID.
	lda	    DCBDId,U
	cmpa	#SP
	blt		bad1
	cmpa	#'~
	ble		ok1
bad1:
	lda		#'?
ok1:
	jsr	    [OutChrv]
	
	; Get sanitize and print second char of DCB ID.
	lda	    DCBDId+1,U
	cmpa	#SP
	blt		bad2
	cmpa	#'~
	ble		ok2
bad2:
	lda		#'?
ok2:
	jsr	    [OutChrv]

	leax	prefix,pcr
	jsr	    [PStringv]
	
	tfr	    U,D
	jsr	[DspDByv]	; display DCB address

	jsr	[PStringv]
	lda	DCBErr,U
	jsr	[DspSByv]	; display DCB Err Status

	jsr	[PStringv]
	ldD	DCBDvr,U
	jsr	[DspDByv]	; display DCB Dvr address

	jsr	[PStringv]
	ldd	DCBIOA,U
	jsr	[DspDByv]	; display DCB I/O address
	
	tst	verbose,pcr	; check flag
	beq	ListDCBX
	
        clra
        ldb		DCBExt,U	; Get number of extension bytes
        addb	#10			; add minimum # number of bytes in DCB
        tfr		U,X			; setup for hex dump
        JSR     [DumpMem2v]	; dump DCB data.

        jsr		CRLF		; display CRLF
ListDCBX


	ldu	DCBLnk,U        ; follow link
	bne	ListDCB_loop
    
    jsr		CRLF

	clrb
	rts					; Return to OS

prefix:
	fcc	/ @/
	fcb	'=+$80
	fcc	/Stat/
	fcb	'=+$80
	fcc	/Drv@/
	fcb	'=+$80
	fcc	/IO@/
	fcb	'=+$80

DmyID:	FCC	/ZZ/	
        
sysdcbprefix:
	fcc	/CIdcb/
	fcb	'=+$80
    fdb CIDCB   
	fcc	/CEdcb/
	fcb	'=+$80
    fdb CEDCB   
	fcc	/COdcb/
	fcb	'=+$80
    fdb CODCB   
	fcc	/TPdcb/
	fcb	'=+$80
    fdb TPDCB   
    
    FCB 0
SYSDSKPREFIX:
	fcc	/DSK/
	fcb	'=+$80
     
endcod  equ *-1

*
** Working Variables
*

DmyDcb	rmb 	10
verbose	rmb	1
sysdcbs	rmb	1

endpgm  equ     *-1


 END

Cmd?Z 
MPX/9 VERSION 1.20
COPYRIGHT (c) 1980 BY PERCOM DATA CO. INC.
Loaded @ $AD00 B100 - $BC54 

MPX?LISTDCB.CM /V /S
CIDCB=CN CEDCB=CN CODCB=CN TPDCB=CN F020 DK
F000 : 02 28 0A 19 0A C0 16 00  FF 28 0A 19 0A C0 16 00  .(.......(......
F010 : FF 28 0A 19 0A C0 16 00  FF 28 0A 19 0A C0 16 00  .(.......(......
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 00 FF FF 02  ..DK............
F030 : 03 00 19 00 00 32 01 00  00 85 11 00 00 00 00 00  .....2..........


DK @=F020 Stat=00 Drv@=C0BC IO@=F000 
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 00 FF FF 02  ..DK............
F030 : 03 00 19 00 00 32 01 00  00 85 11 00 00 00 00 00  .....2..........

NL @=F200 Stat=00 Drv@=F83E IO@=0000 
F200 : F3 DE 4E 4C F8 3E 00 00  00 00 00 00 00 00 00 00  ..NL.>..........

CN @=F3DE Stat=00 Drv@=FF62 IO@=F7FE 
F3DE : 00 00 43 4E FF 62 F7 FE  00 00 F2 00 F3 DE F3 DE  ..CN.b..........


MPX?M

Cmd?D F000 F080
F000 : 02 28 0A 19 0A C0 16 00  FF 28 0A 19 0A C0 16 00  .(.......(......
F010 : FF 28 0A 19 0A C0 16 00  FF 28 0A 19 0A C0 16 00  .(.......(......
F020 : F2 00 44 4B C0 BC F0 00  00 16 01 00 00 FF FF 02  ..DK............
F030 : 03 00 19 00 00 32 01 00  00 85 11 00 00 00 00 00  .....2..........
F040 : B1 B1 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
F050 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
F060 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
F070 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
Cmd?D F290
F290 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
Cmd?D AD00 B100
AD00 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AD10 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AD20 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AD30 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AD40 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AD50 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AD60 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AD70 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AD80 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AD90 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
ADA0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
ADB0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
ADC0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 C2  ................
ADD0 : AF C2 F3 E6 C2 FD 68 FD  68 02 00 F3 DE F3 D6 00  ......h.h....h..
ADE0 : D6 FD 61 20 0D AD E6 FD  7B 7B F8 90 AD E0 B1 00  .......aa.......
ADF0 : FC 48 F0 0A 6A 00 AE C1  F3 DE 00 00 BC 52 B2 11  .H..j........R..
AE00 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AE10 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AE20 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AE30 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AE40 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AE50 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AE60 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AE70 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AE80 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AE90 : F0 20 F0 00 B1 C7 F0 20  00 03 08 20 88 00 00 1B  . ..... ... ....  <<-- F020 @ +290 is sysdcb pointer.
AEA0 : 00 01 00 00 4C 49 53 54  44 43 42 00 43 4D 00 08  ....LISTDCB.CM..
AEB0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AEC0 : 4D 0D 53 54 44 43 42 2E  43 4D 20 2F 56 20 2F 53  M.STDCB.CM /V /S
AED0 : 0D 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AEE0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AEF0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
AF00 : 28 53 59 53 44 49 52 29  53 59 00 00 00 02 00 00  (SYSDIR)SY......
AF10 : 4D 50 58 39 00 00 00 00  53 59 00 03 00 0E 00 00  MPX9....SY......
AF20 : 4F 55 54 56 45 43 54 52  43 4D 00 0F 00 0F 00 00  OUTVECTRCM......
AF30 : 4C 49 53 54 00 00 00 00  43 4D 00 10 00 11 00 00  LIST....CM......
AF40 : 4D 45 4D 54 53 54 00 00  43 4D 00 12 00 12 00 00  MEMTST..CM......
AF50 : 43 52 45 41 54 45 00 00  43 4D 00 13 00 14 00 00  CREATE..CM......
AF60 : 44 4F 00 00 00 00 00 00  43 4D 00 15 00 15 00 00  DO......CM......
AF70 : 4E 41 4D 45 00 00 00 00  43 4D 00 16 00 16 00 00  NAME....CM......
AF80 : 48 45 58 44 55 4D 50 00  43 4D 00 17 00 17 00 00  HEXDUMP.CM......
AF90 : 48 45 4C 4C 4F 57 4F 52  43 4D 00 18 00 18 00 00  HELLOWORCM......
AFA0 : 4C 49 53 54 44 43 42 00  43 4D 00 19 00 1A 00 00  LISTDCB.CM......
AFB0 : 52 41 4D 44 49 53 4B 4C  43 4D 00 1B 00 1C 00 00  RAMDISKLCM......
AFC0 : 4D 59 43 4F 50 59 00 00  43 4D 00 1D 00 1D 00 00  MYCOPY..CM......
AFD0 : 53 45 54 00 00 00 00 00  43 4D 00 1E 00 1E 00 00  SET.....CM......
AFE0 : 45 43 48 4F 00 00 00 00  43 4D 00 1F 00 1F 00 00  ECHO....CM......
AFF0 : 00 00 00 00 00 00 00 00  00 00 00 20 00 00 00 00  ........... ....
B000 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B010 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B020 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B030 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B040 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B050 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B060 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B070 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B080 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B090 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B0A0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B0B0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B0C0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B0D0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B0E0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
B0F0 : 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
Cmd?

        
