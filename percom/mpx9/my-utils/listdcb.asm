 nam ListDBC
*********************************************************
* ListDBC.CM                            JNS 4/23/2014   *
*                                                       *
*                                                       *
*********************************************************

	INCLUDE mpx9.i
	INCLUDE psymon-ext.i
    INCLUDE ascii.i

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
sysdcbloop:
	jsr	    [PStringv]  ; display entry name

	ldy	    [,X++]
    ldd     DCBDId,y
    
    jsr	    [OutChrv]
    tfr     b,a
    jsr	    [OutChrv]

    jsr     OutSp

    tst     ,X 			; check for end of list
    bne     sysdcbloop

	
    leax    1,X
	jsr	    [PStringv]  ; display entry name

	SWI3
	FCB		GETBAS

    IFDEF NEWSYSDCB
		ldx #$f042+1
		; tfr		X,D
		; jsr		[DspDByv]
		; leax 	$01a0,X 		; Y --> SYSDCBn

		; tfr		x,D
		; jsr		[DspDByv]

		ldb 	,x+		d=dcb count ( b lsb ignore a )
diskloop:
		ldy     ,x++
		lda     DCBDId,y
        jsr	    [OutChrv]
	    lda 	DCBDId+1,y
	    jsr	    [OutChrv]

   		decb
   		beq  	SYSDCBSDONE

		lda 	#',
		jsr	    [OutChrv]
		BRA 	diskloop
SYSDCBSDONE:
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
bad1:	lda		#'?
ok1:	jsr	    [OutChrv]
		; Get sanitize and print second char of DCB ID.
		lda	    DCBDId+1,U
		cmpa	#SP
		blt		bad2
		cmpa	#'~
		ble		ok2
bad2:	lda		#'?
ok2:	jsr	    [OutChrv]

		leax	prefix,pcr

		jsr	    [PStringv]
		tfr	    U,D
		jsr		[DspDByv]	; display DCB address

		jsr		[PStringv]
		lda		DCBErr,U
		jsr		[DspSByv]	; display DCB Err Status

		jsr		[PStringv]
		ldD		DCBDvr,U
		jsr		[DspDByv]	; display DCB Dvr address

		jsr		[PStringv]
		ldd		DCBIOA,U
		jsr		[DspDByv]	; display DCB I/O address
		
		tst		verbose,pcr	; check flag
		beq		ListDCBX
	
        clra
        ldb		#10			; start with the minimum # number of bytes in DCB
        addb	DCBExt,U	; add the number of extension bytes
        tfr		U,X			; setup for hex dump
        JSR     [DumpMem2v]	; dump DCB data.

        jsr		CRLF		; display CRLF

ListDCBX:
		ldu		DCBLnk,U        ; follow link to the next DCB in the list.
		bne		ListDCB_loop
	    
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
	fcc	/DCB'S: CI/
	fcb	'=+$80
    fdb CIDCB   
	fcc	/CE/
	fcb	'=+$80
    fdb CEDCB   
	fcc	/CO/
	fcb	'=+$80
    fdb CODCB   
	fcc	/TP/
	fcb	'=+$80
    fdb TPDCB   
    
    FCB 0

	fcc	/DISK/
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
