.SUFFIXES: .asm .bin .s19 .ihex .obj .lib .cm .cm1
.SUFFIXES: .asm .bin .s19 .ihex .cm 

MPX9DSK=../mpx9dsk/mpx9dsk.py
TOOLS=../../tools/
ASMINCLUDE=-I ..
ASM=lwasm
#DEFINES=-DRAMTGT -DS19LOADER
#DEFINES=-DMPX9
DEFINES=-DNULL_DCB -DBS_BUG -DCURDRIVE -DNEWSYSDCB -DDEBUG
ASMFLAGS=-l$*.lst --symbol-dump=$*.sym $(ASMINCLUDE) $(DEFINES)
LDFLAGS=-m$*.map -s$*.ld

%.hex : %.asm
	$(ASM) $(ASMFLAGS) -fhex -o$@ $<

%.ihex : %.asm
	$(ASM) $(ASMFLAGS) -fihex -o$@ $<

%.s19 : %.asm
	$(ASM) $(ASMFLAGS) -fsrec -o$@ $<

%.bin %.sy : %.asm
	$(ASM) $(ASMFLAGS) -fraw -o$@ $<

%.cm : %.asm
	$(ASM) $(ASMFLAGS) -fraw -o$@ $<

%.md : %.asm
	$(ASM) $(ASMFLAGS) -fraw -o$@ $<

.PHONY: clean
CLEAN=$(RM) -f *.s19 *.ihex *.ihex *.lst *.log *.bin *.cm *.cm1 *.md *.lib *.map *.obj *.bin *.sy *.sym *.sublime-workspace

# RMA/RLINK
ASM_		= lwasm --format=obj $(ASMFLAGS)
LINKER_		= lwlink --format=raw
LWAR_		= lwar -c

# Commands

# Rules
%.lib : %.obj
#	@echo LWAR_LWAR_LWAR_
	$(LWAR_) $@ $?

%.obj : %.asm
#	@echo ASM_ASM_ASM_
	$(ASM_) $(AFLAGS) -o$@ $< 

%.cm1 : %.obj
#	@echo LINKER_LINKER_LINKER_
	$(LINKER_) $(LDFLAGS) -o$@ $^
