include ../Makefile.inc

SRCS=do.asm echo.asm HelloWorld.asm HexDump.asm listdcb.asm mycopy.asm name.asm \
	set.asm RamDiskL.asm nCmdLine.asm loadhi.asm
OBJS=$(SRCS:asm=cm)
TGTDSK=../system.dsk

all: $(OBJS)
	../$(MPX9DSK) $(TGTDSK) -w -n $(OBJS) -f

jns:
	$(MPX9DSK) $(TGTDSK) -v -f 

clean:
	-$(CLEAN)

# ----------------------------------------------------
	
# fix bootloader to add an new pointer (in freetram) and update it so that others can allocate from that pointer also
# add pointer into freeram that points to mpx/9 load location ($b500)

#DONE Put 4 sysdcb's in place of the single one, use one for each drive
#DONE replace code that gets the sysdcb with code to index into array by drive number
#DONE define maxdrives equ 4 and change hardcodeed to use that
#DONE init sysdcb array with default dcb	

# write Mount [#device [drive#]]  no args, display current, no drive# set all
# write unmount ???
#WIP make loadable ramdisk driver "Ramdisk #Rn addrl addrh

# case insensitivity???

# dumpfilein hex
# make forth work under mpx9

#DONE add memtst to the psymon-exts
#DONE JumpSubr, Read/WriteSector to user code; to the psymon exts
	
# make ramdisk set approiate size info (track/sector) in Psymon ram ($F000)
# implement somthing similar to df to show used/free/total space on a disk
# use exec address in dir to specify load relocatable below mpx9 ($ffnn) nn specifies the number of 256 byte areas to allocate in addition to the size of the program (endblock-startblock+1)*256. i.e. load at MPX9_begin@ - (endblock-startblock+1+EXECADDR & $FF)*256 entry point must be at zero relative to the start of code.
