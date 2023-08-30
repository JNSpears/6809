* mpx9+.i

        INCLUDE mpx9.i

        IFNDEF MPX9P__I


MPX9LOADED EQU $40
DBGFMT EQU $41
KALLOC EQU $42

; _verbose        equ     0
verbose         extern

DLinkedList     STRUCT
head            rmw 1
tail            rmw 1
                ENDSTRUCT

DLinkedListNode STRUCT
next            rmw 1
prev            rmw 1
                ENDSTRUCT



MPX9P__I SET 1

        ENDC
