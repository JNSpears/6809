.SUFFIXES: .asm .bin .s19 .ihex .cm

MPX9DSK=../mpx9dsk/mpx9dsk.py
TOOLS=../../tools/
ASMINCLUDE=-I ..
ASM=lwasm
#DEFINES=-DRAMTGT -DS19LOADER
#DEFINES=-DMPX9
DEFINES=-DNULL_DCB -DBS_BUG -DCURDRIVE -DNEWSYSDCB 
ASMFLAGS=-l$*.lst $(ASMINCLUDE) $(DEFINES)

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


# AEA1