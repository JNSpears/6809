include Makefile.inc

EPROMS=../psymon/psymon.ihex ../psymon/psymon-ext.ihex boot-rom/BootRom.ihex
TGTDSK=foo1.dsk

all: test

../psymon/psymon.ihex: ../psymon/psymon.asm
../psymon/psymon-ext.ihex: ../psymon/psymon.asm
boot-rom/BootRom.ihex: boot-rom/BootRom.asm 
boot-sector/BootSector.bin: boot-sector/BootSector.asm
os/mpx9.bin: os/mpx9.asm
os/MPX9.SY: os/mpx9.bin
	mv os/mpx9.bin os/MPX9.SY

sub-makes:
	$(MAKE) -C ../psymon all
	$(MAKE) -C boot-rom all
	$(MAKE) -C boot-sector all
	$(MAKE) -C os all
	$(MAKE) -C utils all
	$(MAKE) -C my-utils all
# 	$(MAKE) -C FigForth
	$(MAKE) -C tForth

init: boot-sector/BootSector.bin os/MPX9.SY
	$(MPX9DSK) $(TGTDSK) --init boot-sector/BootSector.bin os/MPX9.SY -f 

foo1.dsk: init sub-makes
# 	$(MPX9DSK) $(TGTDSK) -n -w $(wildcard utils/*.cm) -f 
# 	$(MPX9DSK) $(TGTDSK) -n -w $(wildcard my-utils/*.cm) -f 

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

clean:
	-$(CLEAN)
