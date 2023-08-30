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

        section .text

**************************************************
* Program (Position independant)
**************************************************


**************************************************
* Initialiaze Kernel memory ALLOCATE             *
*                                                *
* ENTRY REQUIREMENTS:  X -> lowest memory address*
*                        used by MPX9+           *
*                                                *
* EXIT CONDITIONS:  B zero                       *
*                   OTHER registers unchanged    *
**************************************************
KAllocInit    EXPORT
KAllocInit:
        stx     KAMemPtr,pcr     everything below this address is free memory.

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
KAlloc        EXPORT
KAlloc:
        pshs    D
        LDD     KAMemPtr,PCR
        subd    ,S++
        tfr     D,X        
        stx     KAMemPtr,PCR

	clrb 		; no error
	rts		; Return to OS

**************************************************
**************************************************
**************************************************
**************************************************


**************************************************
* DLnkLstInit - initialiaze a Double Linked List *
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
** Uninitialiazed Working Variables.
**************************************************

        ; section .bss

        ; endsection      ; section .bss

**************************************************
** Uninitialiazed Direct Page Working Variables.
**************************************************

        section .dp

KAMemPtr rmw    1

        endsection      ; section .dp


 END
