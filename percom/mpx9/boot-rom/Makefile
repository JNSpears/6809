.SUFFIXES: .asm .s19 .ihex
TOOLS=../../tools/
ASM=lwasm
#DEFINES=-DRAMTGT -DS19LOADER
#DEFINES=-DMPX9
DEFINES=-DNULL_DCB
ASMFLAGS=-l$*.lst $(DEFINES)

%.hex : %.asm
	$(ASM) $(ASMFLAGS) -fhex -o$@ $<

%.ihex : %.asm
	$(ASM) $(ASMFLAGS) -fihex -o$@ $<

%.s19 : %.asm
	$(ASM) $(ASMFLAGS) -fsrec -o$@ $<

%.bin : %.asm
	$(ASM) $(ASMFLAGS) -fraw -o$@ $<

all: BootMpx9.hex

BootMpx9.hex: BootMpx9.asm

ref BootMpx9.ref: $(eval SHELL:=/bin/bash) Boot\ rom\ from\ EPROM/BootMpx9.bin
	@echo "here shell is $$0"
	srec_cat \
		"Boot rom from EPROM/BootMpx9.bin" -binary -offset 0xc000 \
		-o -Ascii_Hex -Data_Only -obs=16 | tail -n +2  | tr ' ' ',' >BootMpx9.tmp
	(seq 0 16 1023 | while read n; do printf "C%03X\n" $$n ; done) | paste -d ':' - BootMpx9.tmp >BootMpx9.ref
#	tail -n +2 mpx9bootrom.ref | tr ' ' ','
		
		
		
#
#mpx9bootrom.hex:
#	srec_cat \
#		"Boot rom from EPROM/BootMpx9.bin" -binary -offset 0xc000 \
#		-o mpx9bootrom.hex -intel -obs=16 -address-length=2
#	srec_info mpx9bootrom.hex


clean:
	rm *.s19 *.ihex *.lst *.log



