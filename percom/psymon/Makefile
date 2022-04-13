.SUFFIXES: .asm .s19 .ihex
TOOLS=../../tools/
ASM=lwasm
#DEFINES=-DRAMTGT -DS19LOADER
#DEFINES=-DMPX9
DEFINES=-DNULL_DCB
ASMFLAGS=-l$*.lst $(DEFINES)

%.ihex : %.asm
	$(ASM) $(ASMFLAGS) -fihex -o$@ $<

%.s19 : %.asm
	$(ASM) $(ASMFLAGS) -fsrec -o$@ $<

%.bin : %.asm
	$(ASM) $(ASMFLAGS) -fraw -o$@ $<

all: psymon.ihex psymon-ext.ihex

clean:
	rm *.s19 *.ihex *.lst *.log
