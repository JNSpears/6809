entry _Start

section .text load 0
section .data
section *,bss

section .dp load $BF00

define lensympat l_%s
define basesympat s_%s