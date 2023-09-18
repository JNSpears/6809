* mpx9+.i

        INCLUDE mpx9.i

        IFNDEF MPX9P__I


MPX9LOADED      EQU $40
DBGFMT          EQU $41
KALLOC          EQU $42
ADDSYSCALL      EQU $43
ADDRPTERR       EQU $44
ADDRESCMD       EQU $45
ADDMOD          EQU $46

ERR_RN          EQU 19  - ERR_RN (resource not available)
ERR_NM          EQU 20  - ERR_NM (not module)

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

SimpleList      STRUCT
pHead           rmw 1
pNext           rmw 1
pEnd            rmw 1
                ENDSTRUCT


MPX9P__I SET 1

        ENDC
