include Makefile.inc

EPROMS=../psymon/psymon.ihex ../psymon/psymon-ext.ihex boot-rom/BootRom.ihex
TGTDSK=system.dsk

UTILS=create list memtst OutVectr
UTIL_SRCS=$(foreach file,$(UTILS),utils/$(file).asm)
UTIL_OBJS=$(UTIL_SRCS:.asm=.cm)
# $(info UTIL_SRCS $(UTIL_SRCS))
# $(info UTIL_OBJS $(UTIL_OBJS))

XUTIL_SRCS=list.asm
XUTIL_FILES=$(foreach file,$(XUTIL_SRCS),utils/$(file))
$(info XUTIL_SRCS $(XUTIL_SRCS))
$(info XUTIL_FILES $(XUTIL_FILES))
# 	cp list.asm list.s
# 	../$(MPX9DSK) $(TGTDSK) -f -w -n list.s
# 	$(RM) list.s


MYUTILS=do echo HelloWorld HexDump listdcb mycopy name \
	set RamDiskL nCmdLine loadhi MemEdit
MYUTIL_SRCS=$(foreach file,$(MYUTILS),my-utils/$(file).asm)
MYUTIL_OBJS=$(MYUTIL_SRCS:.asm=.cm)
# $(info MYUTIL_SRCS $(MYUTIL_SRCS))
# $(info MYUTIL_OBJS $(MYUTIL_OBJS))

XMYUTIL_SRCS=
XMYUTIL_FILES=$(foreach file,$(XMYUTIL_SRCS),my-utils/$(file))

all: $(EPROMS) $(TGTDSK)

../psymon/psymon.ihex: ../psymon/psymon.asm
../psymon/psymon-ext.ihex: ../psymon/psymon.asm
boot-rom/BootRom.ihex: boot-rom/BootRom.asm 
boot-sector/BootSector.bin: boot-sector/BootSector.asm
os/mpx9.bin: os/mpx9.asm
os/MPX9.SY: os/mpx9.bin
	mv os/mpx9.bin os/MPX9.SY

# sub-makes:
# 	$(MAKE) -C ../psymon all
# 	$(MAKE) -C boot-rom all
# 	$(MAKE) -C boot-sector all
# 	$(MAKE) -C os all
# 	$(MAKE) -C utils all
# 	$(MAKE) -C my-utils all
# # 	$(MAKE) -C FigForth
# 	$(MAKE) -C tForth

init: boot-sector/BootSector.bin os/MPX9.SY
	$(MPX9DSK) $(TGTDSK) --init boot-sector/BootSector.bin os/MPX9.SY -f 

$(TGTDSK): init                $(UTIL_OBJS) $(XUTIL_FILES) $(MYUTIL_OBJS) $(XMYUTIL_FILES) my-utils/abc.cm1 # sub-makes
	$(MPX9DSK) $(TGTDSK) -n -w $(UTIL_OBJS) $(XUTIL_FILES) $(MYUTIL_OBJS) $(XMYUTIL_FILES) my-utils/abc.cm1 -f 

test: $(EPROMS) $(TGTDSK) 
	../../tools/usim/percom/percom -1 $(TGTDSK) -2 tForth/tforth.dsk -- $(EPROMS)

testd: $(EPROMS) $(TGTDSK) 
	../../tools/usim/percom/percomd -d -1 $(TGTDSK) -2 tForth/tforth.dsk -- $(EPROMS)

clean:
	$(MAKE) -C ../psymon clean
	$(MAKE) -C boot-rom clean
	$(MAKE) -C boot-sector clean
	$(MAKE) -C os clean
	$(MAKE) -C utils clean
	$(MAKE) -C my-utils clean
# 	$(MAKE) -C FigForth clean
	$(MAKE) -C tForth clean
	-$(CLEAN)

my-utils/abc1.obj: my-utils/abc1.asm 
my-utils/abc2.obj: my-utils/abc2.asm 
my-utils/abc.lib: my-utils/abc1.obj my-utils/abc2.obj
my-utils/abc.cm1: my-utils/abc.obj my-utils/abc.lib
	$(LINKER_) $(LDLAGS) -slink.ld -mmy-utils/abc.map -o$@ $? 
abc: my-utils/abc.cm1


# abc.lib: abc1.obj abc2.obj
# my-utils/abc.cm1: abc.obj abc.lib
# 	$(LINKER_) $(LDLAGS) -slink.ld -mabc.map -o$@ $? 
# abc: my-utils/abc.cm1