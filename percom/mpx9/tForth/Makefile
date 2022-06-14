include ../Makefile.inc
DEFINES += -DPSYMON
TGTDSK=tforth.dsk

all: tforth09.cm tforth09.s19 $(TGTDSK)

$(TGTDSK): tforth09.cm
	../$(MPX9DSK) $(TGTDSK) --init ../boot-sector/BootSector.bin ../os/MPX9.SY -f 
	../$(MPX9DSK) $(TGTDSK) -n -w tforth09.cm --entry 0 --buffer 0 -f 
	../$(MPX9DSK) $(TGTDSK) -n -w *.4th -f

clean:
	-$(CLEAN)

