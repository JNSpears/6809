# Percom MPX/9 for the mc6809
Misc mc6809 code for Percom based SS-50 systems.

### Back story
just out of high school when I determined that there was no  hope of every getting my wirewrapped 8080 system built from a Popular Electronics artical, to function. I bought a Percom SBC/9, Electric Window video card and a LFD-400 disk controller, along with a Boaz 64k dram card.

### Fast forward 
Some time in the 20-teens I pulled it all out and with the help of a new memory card from [Corsham](www.corshamtech.com) I was able to get it running again minus the video card. The whole system has since become unstable and I haven't spent the energy to get it fixed. but along the way I got the system up and running on Usim from Ray Bellis, this repo contains the core of Percom's Mpx9 running on the upgraded Usim with my own LFD-400 emulation (see [github](https://github.com/JNSpears/usim) )

### Included here is:

* The Percom monitor Psymon, my personal PsyMon extensions 
* Mpx9 Boot eprom, Mpx9 Boot Sector and OS
    * Some of the Mpx9 utilities (I'm still recovering the remainder)
    * My personal additions to the utilities

### TBD:
    * implement Electric Window support
    * recover Percom super basic
    * fig forth for percom hardware
