 nam ListDBC
*********************************************************
* ListDBC.CM                            JNS 4/23/2014   *
*                                                       *
*                                                       *
*********************************************************

	INCLUDE psymon.i
	INCLUDE mpx9.i
	; INCLUDE psymon-ext.i
    ; INCLUDE jns.i
    INCLUDE ascii.i

**************************************************
* Program (Position independant)
**************************************************
        ORG     $1000

ListDCBs:
	clr     >verbose,pcr    ; initialize variables
    clr     >sysdcbs,pcr
    
option:
	MPX9    SKPSPC          ; point to the next word
	beq     DoListDCB       ; No arguments
	lda     ,x+
	cmpa    #'/  ''           ; look for option flags
	bne     DoListDCB       ; Not a switch

option_V:
	lda     ,x+             ; get option char and bump pointer
	cmpa    #'V'             ; is a option 'V'?
	bne     Option_S        ; bad switch
	com     >verbose,pcr    ; toggle option 'V'
	bra     option          ; get next option

Option_S:
	cmpa    #'S'             ; is a option 'S'?
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

	lbsr 	CookPrt    
    tfr     b,a
	lbsr 	CookPrt    

    jsr     OutSp

    tst     ,X 			; check for end of list
    bne     sysdcbloop

	
    leax    1,X
	jsr	    [PStringv]  ; display entry name

	MPX9	GETBAS

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
		bsr 	CookPrt 

		lda 	DCBDId+1,y
		bsr 	CookPrt    
		
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
		bsr 	CookPrt    
	    tfr     b,a
		bsr 	CookPrt    

   		jsr     OutSp
    ENDC
        jsr		CRLF		; display CRLF

Nosysdcbs:

		LEAX	DmyDcb,PCR			; X -> Dummy DCB
		clr		DCBLnk,X			; clear link to next DCB
		clr		DCBLnk+1,X
		LDD		DmyID,PCR			; set DCB Device ID 
		STD		DCBDId,X
		MPX9	ADDDCB				; add to DCB list
		MPX9	DELDCB				; remove from DCB list
		LDU		DmyDcb+DCBLnk,PCR	; Now we have a pointer to the MPX/9 Device list.

ListDCB_loop:

		jsr		CRLF

		; Get sanitize and print first char of DCB ID.
		lda	    DCBDId,U
		bsr 	CookPrt    
		; Get sanitize and print second char of DCB ID.
		lda	    DCBDId+1,U
		bsr 	CookPrt    

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

		; Get sanitize and print a char in the DCB ID.
CookPrt:
		cmpa	#SP
		blt		bad1
		cmpa	#'~'
		ble		ok1
bad1:	lda		#'?'
ok1:	jmp	    [OutChrv]


prefix:
	fcs	/ @=/
	fcs	/Stat=/
	fcs	/Drv@=/
	fcs	/IO@=/

DmyID:	FCC	/ZZ/	
        
sysdcbprefix:
	fcs	/DCB'S: CI=/
    fdb CIDCB   
	fcs	/CE=/
    fdb CEDCB   
	fcs	/CO=/
    fdb CODCB   
	fcs	/TP=/
    fdb TPDCB   
    
    FCB 0

	fcS	/DISK=/
     
endcod  equ *-1

*
** Working Variables
*

DmyDcb	rmb 	10
verbose	rmb	1
sysdcbs	rmb	1

endpgm  equ     *-1

 END
