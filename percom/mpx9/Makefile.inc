.SUFFIXES: .asm .bin .s19 .ihex .cm

MPX9DSK=../mpx9dsk/mpx9dsk.py
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

%.cm : %.asm
	$(ASM) $(ASMFLAGS) -fraw -o$@ $<

.PHONY: clean
CLEAN=$(RM) -f *.s19 *.ihex *.ihex *.lst *.log *.bin *.cm