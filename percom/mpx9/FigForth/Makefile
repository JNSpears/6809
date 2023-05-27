include ../Makefile.inc
DEFINES += -DPSYMON
TGTDSK=fforth.dsk

all: forth9.cm $(TGTDSK)


$(TGTDSK): forth9.cm
	../$(MPX9DSK) $(TGTDSK) --init ../boot-sector/BootSector.bin ../os/MPX9.SY -f 
	../$(MPX9DSK) $(TGTDSK) -n -w forth9.cm --entry 2000 --buffer 2000 -f
	../$(MPX9DSK) $(TGTDSK) -n -w *.4th -f

clean:
	-$(CLEAN)
