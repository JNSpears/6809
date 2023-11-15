include Makefile.inc

EPROMS=../psymon/psymon.ihex ../psymon/psymon-ext.ihex boot-rom/BootRom.ihex
TGTDSK=system.dsk
BLKDSK=blank.dsk

UTILS=create DskEdt exec list memtst OutVectr copy
UTIL_SRCS=$(foreach file,$(UTILS),utils/$(file).asm)
UTIL_OBJS=$(UTIL_SRCS:.asm=.cm)
# $(info UTIL_SRCS $(UTIL_SRCS))
# $(info UTIL_OBJS $(UTIL_OBJS))

XUTIL_SRCS=list.asm
XUTIL_FILES=$(foreach file,$(XUTIL_SRCS),utils/$(file))
# $(info XUTIL_SRCS $(XUTIL_SRCS))
# $(info XUTIL_FILES $(XUTIL_FILES))
# 	cp list.asm list.s
# 	../$(MPX9DSK) $(TGTDSK) -f -w -n list.s
# 	$(RM) list.s


MYUTILS=do echo HelloWorld HexDump listdcb name \
	set RamDiskL loadhi MemEdit
MYUTIL_SRCS=$(foreach file,$(MYUTILS),my-utils/$(file).asm)
MYUTIL_OBJS=$(MYUTIL_SRCS:.asm=.cm)
# $(info MYUTIL_SRCS $(MYUTIL_SRCS))
# $(info MYUTIL_OBJS $(MYUTIL_OBJS))

XMYUTIL_SRCS=
XMYUTIL_FILES=$(foreach file,$(XMYUTIL_SRCS),my-utils/$(file))

LDFLAGS=-m$*.map -s$*.ld

KERNAL=start dbgfmt CmdLine CmdShell Kalloc RptErr Mod
KERNAL_SRCS=$(foreach file,$(KERNAL),mpx9+/$(file).asm)
KERNAL_OBJS=$(KERNAL_SRCS:.asm=.obj)
# $(info KERNAL_SRCS $(KERNAL_SRCS))
# $(info KERNAL_OBJS $(KERNAL_OBJS))

MODULES=#abc1 abc4
MODULE_SRCS=$(foreach file,$(MODULES),mpx9+/$(file).asm)
MODULE_OBJS=$(MODULE_SRCS:.asm=.md)
# $(info MODULE_SRCS $(MODULE_SRCS))
# $(info MODULE_OBJS $(MODULE_OBJS))


all: $(EPROMS) $(TGTDSK)

.PHONY:
percom:
	make -C ../../tools/usim
	make -C ../../tools/usimdbg
	make -C ../../tools/percom

../psymon/psymon.ihex: ../psymon/psymon.asm
../psymon/psymon-ext.ihex: ../psymon/psymon.asm
boot-rom/BootRom.ihex: boot-rom/BootRom.asm 
boot-sector/BootSector.bin: boot-sector/BootSector.asm
# os/mpx9.sy: os/mpx9.asm
# os/MPX9.SY: os/mpx9.bin
# 	cp os/mpx9.bin os/MPX9.SY

sub-makes:
	$(MAKE) -C ../psymon all
	$(MAKE) -C boot-rom all
	$(MAKE) -C boot-sector all
	$(MAKE) -C os all
	$(MAKE) -C utils all
	$(MAKE) -C my-utils all
# 	$(MAKE) -C FigForth
	$(MAKE) -C tForth

# .PHONY:
# $(TGTDSK) : boot-sector/BootSector.bin os/MPX9.SY
# 	$(MPX9DSK) $(TGTDSK) --init boot-sector/BootSector.bin os/MPX9.SY -f 

$(TGTDSK): boot-sector/BootSector.bin os/mpx9.sy                                               $(UTIL_OBJS) $(XUTIL_FILES) $(MYUTIL_OBJS) $(XMYUTIL_FILES) mpx9+/mpx9+.cm1 $(MODULE_OBJS) # sub-makes
	$(MPX9DSK) $(TGTDSK) -n --init boot-sector/BootSector.bin os/mpx9.sy -f 
	$(MPX9DSK) $(TGTDSK) -n -w --entry 0x1000 --buffer 0x1000 $(UTIL_OBJS) $(XUTIL_FILES) $(MYUTIL_OBJS) $(XMYUTIL_FILES) mpx9+/mpx9+.cm1 $(MODULE_OBJS) -f 

$(BLKDSK): boot-sector/BootSector.bin os/mpx9.sy
	$(MPX9DSK) $(BLKDSK) -n --init boot-sector/BootSector.bin os/mpx9.sy -f 
	$(MPX9DSK) $(BLKDSK) -n -w --entry 0x1000 --buffer 0x1000 utils/list.cm -f 


test: $(EPROMS) $(TGTDSK) 
	../../tools/percom/percom -1 $(TGTDSK) -2 tForth/tforth.dsk -3 $(BLKDSK) -- $(EPROMS)

testd: $(EPROMS) $(TGTDSK) 
	../../tools/percom/percomd -1 $(TGTDSK) -2 tForth/tforth.dsk -3 $(BLKDSK) -- $(EPROMS)

testdv: $(EPROMS) $(TGTDSK) 
	../../tools/percom/percomd -v -v -1 $(TGTDSK) -2 tForth/tforth.dsk -3 $(BLKDSK) -- $(EPROMS)

clean:
	$(MAKE) -C ../psymon clean
	$(MAKE) -C boot-rom clean
	$(MAKE) -C boot-sector clean
	$(MAKE) -C os clean
	$(MAKE) -C utils clean
	$(MAKE) -C my-utils clean
# 	$(MAKE) -C FigForth clean
	$(MAKE) -C tForth clean
	$(MAKE) -C mpx9+ clean
	-$(CLEAN)

mpx9+/mpx9+.cm1 : $(KERNAL_OBJS)
	$(LINKER_) $(LDFLAGS) -o$@ $^

################################################################################

.ONESHELL:

tests: testsetup test-all report

.PHONY:
testsetup:
	rm -f tests/summary

test-all: test1 t

.PHONY:
report:
	@echo '--------------------------------------------------------------------'
	@cat tests/summary | awk '{ gsub("differ", "\033[1m\033[1;31m&\033[0m"); gsub("identical", "\033[1;36m&\033[0m"); print }'
	@echo '--------------------------------------------------------------------'
	@echo  Resuts: `grep identical tests/summary | wc -l` of `cat tests/summary  | wc -l` passed
	@echo '--------------------------------------------------------------------'

test1: $(TGTDSK) tForth/tforth.dsk $(EPROMS)
	../../tools/percom/percomd -1 $(TGTDSK) -2 tForth/tforth.dsk -- $(EPROMS) >tests/$(@).log 2>&1 <<EOF
	GO
	






	Z






	MPX9+
	EOF
	diff $(DIFFFLAGS) tests/$(@).log tests/$(@).std | tee -a tests/summary

