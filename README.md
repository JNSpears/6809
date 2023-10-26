# Percom MPX/9 for the mc6809
Misc mc6809 code for Percom based SS-50 systems.

### Back story
Just out of high school when I determined that there was no  hope of every getting my wire wrapped 8080 system built from a Popular Electronics artical to function. I bought a Percom SBC/9 along with an Electric Window video card and a LFD-400 disk controller, and a Boaz 64k dram card.

### Fast forward 
Some time in the 20-teens I pulled it all out and with the help of a new memory card from [Corsham](www.corshamtech.com) I was able to get it running again minus the video card. The whole system has since become unstable and I haven't spent the energy to get it fixed. But along the way I got the system up and running on Usim from Ray Bellis, this repo contains the core of Percom's MPX-9 running on the upgraded Usim with my own LFD-400 emulation (see [github](https://github.com/JNSpears/usim)  and [github](https://github.com/JNSpears/usimdbg))

### Included here is:

* The Percom monitor Psymon, plus my personal PsyMon extensions 
* Mpx9 Boot eprom, Mpx9 Boot Sector and OS
    * Some of the Mpx9 utilities (I'm still recovering the remainder)
    * My personal additions to the utilities

### TBD:
    * implement Electric Window support
    * recover Percom super basic
    * fig forth for Percom hardware

## Psymon & MPX-9 OS implementation status 

| Status | Item             | Note                                                                                     |
|--------|------------------|------------------------------------------------------------------------------------------|
| Done   | Psymon           | Low level system monitor. (PSYMON - Percom 6809 System Monitor (Percom Data Company).pdf)|
| Done   | Psymon-ext       | My personal extensions to PsyMon.                                                        |
| Done(4)| LFD-400 Boot ROM | Boot rom contains bootloader and LFD-400 device driver.                                  |
| Done(4)| Boot Sector      | Code to determine where to load MPX-9 and load it.                                       |
| Done(4)| Boot Image       | MPX-9 os image.                                                                          |


## Mpx9 Utilities

| Status | Utility | Note                                                                                                   |
|--------|---------|--------------------------------------------------------------------------------------------------------|
| Done   | Create  | Create an ASCII text file an MPX-9 utility.                                                            |
| Done   | Exec    | File executor an MPX-9 utility program. (brain dead Batch or sh)                                       |
| Done   | List    | List the contents of a file an MPX-9 utility.                                                          |
| Done   | Outvectr| Alternate output vector an MPX-9 utility program.                                                      |
| Done   | Dskedt  | Disk Sector Editor an MPX-9 utility program.                                                           |
|        | Copy    | This Mpx/9(tm) utility may be used to copy files from one disk to another disk.                        |
|        | Remap   | This utility will remap a 6800 minidos/mpx diskette for compatibility with an mpx/9 operating system.  |
|        | Certify | Certify the usability of a diskette an MPX-9 utility program.                                          |
|        | Hexload | Load a hexadecimal s1-s9) formatted file an MPX-9 utility program.                                     |
|        | Lpseria | Driver for serial line printer an MPX-9 device driver.                                                 |
| Done   | Memtest | A 6809 memory test an MPX-9 utility program.                                                           |
|        | Verify  | Verify the readability of a diskette an MPX-9 utility.                                                 | 
|        | Pdir    | Print disk directory an MPX-9 utility program.                                                         |

## Mpx9 My added Utilities

| Status | Utility    | Note                                                       |
|--------|------------|------------------------------------------------------------|
| Done   | do         | Do several ';' seperated commands on one line.             |
| Done   | HelloWorld | Obgiltory Hello World.                                     |
| Done   | loadhi     | Load executables at different address than file calls for. |
| Done   | name       | Display or change the name of a disk (ms-dos lable).       |
| Done   | set        | Display/change some variables in Psymon/MPX-9.             |
| Done   | HexDump    | Hex display memory or file.                                |
| Done   | MemEdit    | Very basic memory editor.                                  |
| Done   | echo       | Simple echo utility.                                       |
| Done   | listdcb    | List DCB's.                                                |
| Done   | mycopy     | A start at re-implementing copy.                           |
| Done   | RamDiskL   | Not working ramdisk.                                       |

## MPX9+

My additions to MPX-9

| Status |         | Note                                                                               |
|--------|---------|-----                                                                               |
| Done   | CmdShell| Command dipatcher for resident commands + syscalls to define new resident commands.|
| Done   | DbgFmt  | Quick and dirty printf but register centric.                                       |
| Done   | Start   | Self relocation code, Initilization, syscal extensions.                            |
| Done   | Kalloc  | Brain dead memory allocator for other MPX9+ componets                              |
| Done   | Mod     | Load relocatable modules + cli support for modules                                 |
| Done   | RptErr  | Extend the built in error messages.                                                |
| Done   | CmdLine | Comandline editor w/ history recall.                                               |

## Random Notes:
	
| Status |    | Note                                                                                                                |
|--------|----|---------------------------------------------------------------------------------------------------------------------|
| DONE(3)|    | Fix bootloader to add an new pointer (in freetram) and update it so that others can allocate from that pointer also |
| DONE(3)|    | Add pointer into freeram that points to mpx/9 load location ($b500)                                                 |
| DONE   |    | Put 4 sysdcb's in place of the single one, use one for each drive                                                   |
| DONE   |    | Replace code that gets the sysdcb with code to index into array by drive number                                     |
| DONE   |    | Define maxdrives equ 4 and change hardcodeed to use that                                                            |
| DONE   |    | Init sysdcb array with default dcb                                                                                  |
|        |    | Write Mount [#device [drive#]]  no args, display current, no drive# set all                                         |
|        |    | Write unmount ???                                                                                                   |
| WIP    |    | Make loadable ramdisk driver "Ramdisk #Rn addrl addrh                                                               |
|        |    | Case insensitivity???                                                                                               |
| DONE   |    | Dumpfile in hex                                                                                                     |
| WIP    |    | Make forth work under mpx9                                                                                          |
| DONE   |    | Add memtst to the psymon-exts                                                                                       |
| DONE   |    | JumpSubr, Read/WriteSector to user code; to the psymon exts                                                         |
|        |    | Make ramdisk set approiate size info (track/sector) in Psymon ram ($F000)                                           |
|        |    | Implement somthing similar to df to show used/free/total space on a disk                                            |
| DONE(3)|    | use exec address in dir to specify load relocatable below mpx9 ($ffnn) nn specifies the number of 256 byte areas to allocate in addition to the size of the program (endblock-startblock+1)*256. i.e. load at MPX9_begin@ - (endblock-startblock+1+EXECADDR & $FF)*256 entry point must be at zero relative to the start of code.|


(1) Recovered from scanned and OCR processed printed listings.

(2) Enhanced beond original MPX/9 implemntation.

(3) Implementation is different than specified in the Note.

(4) Recovered from user group disks (source or binaries).
