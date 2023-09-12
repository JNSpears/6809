 PRAGMA cescapes
 nam Kalloc
 ttl Kernal alloc for MPX9+
*********************************************************
* CmdLine                               JNS 8/28/2023   *
*                                                       *
*                                                       *
*********************************************************

        INCLUDE psymon.i
        INCLUDE mpx9.i
        INCLUDE jns.i
        INCLUDE ascii.i
        INCLUDE mpx9+.i

        section .text

**************************************************
* Program (Position independent)
**************************************************


**************************************************
* Initialize Kernel memory ALLOCATE              *
*                                                *
* ENTRY REQUIREMENTS:  X -> lowest memory address*
*                        used by MPX9+           *
*                                                *
* EXIT CONDITIONS:  B zero                       *
*                   OTHER registers unchanged    *
**************************************************
KernalAllocInit    EXPORT
KernalAllocInit:
        stx     <KAMemPtr     everything below this address is free memory.

        clrb
        rts

**************************************************
* SYSTEM CALL $42 (KAlloc) - ALLOCATE KERNAL     *
*                            MEMORY BELOW KERNAL *
*                                                *
* ENTRY REQUIREMENTS:  D CONTAINS SIZE IN BYTES  *
*                        REQUESTED               *
*                                                *
* EXIT CONDITIONS:  X MEMORY REQUESTED OR ZERO ON*
*                     FAILURE           *
; *                   ROUTINE SHOULD RETURN ERROR  *
; *                     CODE IN B, 0 IF NO ERROR   *
**************************************************
KernalAlloc        EXPORT
KernalAlloc:

	tst 	<verbose
	beq 	@nodebug
        MPX9  	DBGFMT
        fcs   	/KAlloc_ D:$%Dx /
@nodebug
        pshs    D
        LDD     <KAMemPtr
        subd    ,S++
        tfr     D,X        
        stx     <KAMemPtr

	tst 	<verbose
	beq 	@nodebug
        MPX9  	DBGFMT
        fcs   	/--> X:$%Xx\r\n/
@nodebug
	NOP
        
	clrb 		; no error
	rts		; Return to OS

**************************************************
**************************************************
**************************************************
**************************************************


**************************************************
* DLnkLstInit - initialize a Double Linked List  *
*                                                *
* ENTRY REQUIREMENTS:  X - points to DLinkedList *
*                        object                  *
*                                                *
* EXIT CONDITIONS:                               *
**************************************************
DLnkLstInit:
        ; clr     DLinkedList.head,x
        ; clr     DLinkedList.head+1,x
        ; clr     DLinkedList.tail,x
        ; clr     DLinkedList.tail+1,x
        rts

; DLnkLstAddHead:
;         rts

; DLnkLstAddHead:
;         rts

; DLnkLstRmvHead:
;         rts

; DLnkLstRmvHead:
;         rts

**************************************************

; KAMemPtr EXPORT


        endsection      ; section .text

**************************************************
** Constants.
**************************************************

        ; section .data

        ; endsection      ; section .data

**************************************************
** Uninitialized Working Variables.
**************************************************

        ; section .bss

        ; endsection      ; section .bss

**************************************************
** Uninitialized Direct Page Working Variables.
**************************************************

        section .dp

KAMemPtr rmw    1

        endsection      ; section .dp


 END
