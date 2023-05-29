;	INCLUDE psymon.i

	IFNDEF PSYMON_EXT__I


*****************************
* PsyMon-Ext Software entry points
*****************************
	ifdef	RAMTGT
DumpMem2v        EQU     $f053 ; for in @ $F050 RAM ($f000-f3ff)
	ELSE
DumpMem2v        EQU     $F803 ; for in @ $F800 ROM ($f800-ffff)
	ENDC

PSYMON_EXT__I SET 1

	ENDC
