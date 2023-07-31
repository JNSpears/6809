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

# RMA/RLINK
ASM_		= lwasm --format=obj $(ASMFLAGS)
LINKER_		= lwlink --format=raw
LWAR_		= lwar -l -a

# Commands

# Rules
%.lib: %.obj
#	@echo LWAR_LWAR_LWAR_
	$(LWAR_) $@ $?

%.obj: %.asm
#	@echo ASM_ASM_ASM_
	$(ASM_) $(AFLAGS) -o$@ $< 

%.cm1: %.obj %.lib
#	@echo LINKER_LINKER_LINKER_
	$(LINKER_) $(AFLAGS) -m$*.map -slink.ld -o$@ $< 




#.c.a:
 #       $(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $*.o
  #      $(AR) r $@ $*.o
#        $(RM) $*.o
