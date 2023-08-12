entry _Start

section code load 0
section text
section start
section initgl_start
section initgl
section initgl_end
section constructors_start
section constructors
section constructors_end
section destructors_start
section destructors
section destructors_end
section rodata
section end_romdata
section rwdata
section data
section cend
section *,bss

define lensympat l_%s
define basesympat s_%s