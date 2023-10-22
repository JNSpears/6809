/*
 * Z5023.h
 *
 *  Created on: Apr 11, 2022
 *      Author: james
 *      (C) J. Spears 2017, 2022
 */

#ifndef PERCOM_Z5023_H_
#define PERCOM_Z5023_H_

#pragma once

#include "device.h"
#include "wiring.h"

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

enum	{BYTES_PER_SECTOR = 256};
enum	{SECTORS_PER_TRACK = 10};
enum	{TRACKS_PER_DISK = 40};

enum	{PRE_SECTOR_DATA = 10}; // Sector Linkage
enum	{POST_SECTOR_DATA = 2}; // CRC

enum	{BLOCK_SIZE = (BYTES_PER_SECTOR+PRE_SECTOR_DATA+POST_SECTOR_DATA)};
enum	{DiskSize = BLOCK_SIZE*SECTORS_PER_TRACK*TRACKS_PER_DISK};

enum	{DRIVES_PER_SYSTEM = 4};

typedef struct {

     //000F             (                        mpx9.asm):00184 DCBCUR EQU 15 CURRENT TRACK/SECTOR
     //0011             (                        mpx9.asm):00185 DCBPRV EQU 17 PREVIOUS BLOCK #
     //0013             (                        mpx9.asm):00186 DCBNXT EQU 19 NEXT BLOCK #
     //0015             (                        mpx9.asm):00187 DCBCNT EQU 21 BYTE COUNT IN THIS BLOCK
     //0016             (                        mpx9.asm):00188 DCBADD EQU 22 DATA ADDRESS FOR THIS BLOCK
     //0018             (                        mpx9.asm):00189 DCBTYP EQU 24 BLOCK TYPE CODE

	//unsigned short CurrTS;
	//unsigned short PrevBlock;
	//unsigned short NextBlock;
	//unsigned char DataBytes;
	//unsigned short DataAddr;
	//unsigned char BlockType;
	unsigned short X[5];

	unsigned char Data[BYTES_PER_SECTOR];

     //0019             (                        mpx9.asm):00190 DCBCRC EQU 25 DATA CRC

	unsigned short CRC;

} MPX9_SECTOR;

class MPX9_DskImg {

	char	DskImage[DiskSize];

public:

// Public constructor and destructor

	MPX9_DskImg();
	virtual			~MPX9_DskImg();

	const char *filename;

	void	Mount(const char* filename);
	void UnMount(void);
	int	TrackSector2Block(int track, int sector);
	void	ReadSector(int track, int sector, Byte* pData);
	void	WriteSector(int track, int sector, Byte* pData);
	int	debug;
	bool dirty;
};

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////

class Z5023 : virtual public ActiveMappedDevice {

// Internal registers

public:

	MPX9_DskImg mountedDisks[DRIVES_PER_SYSTEM];
	MPX9_SECTOR currSectorImage;

//	Byte			td, rd, cr, sr;

// Access to real IO device

	long		cycles;		// cycles since last ...

// Initialization functions

protected:
	virtual void		tick(uint8_t);
	virtual void		reset();

// Read and write functions
public:

	virtual Byte		read(Word offset);
	virtual void		write(Word offset, Byte val);

// Other exposed interfaces
public:
//	OutputPinReg		IRQ;

// Public constructor and destructor

	Z5023();
	virtual			~Z5023();

	int		debug;

protected:
	Byte	current_drive;
	bool	motor_on;
	bool	SectorBit;
	bool	IndexBit;
	int	Sector;
	int 	Track;
	int	ReadIndex;
	bool	freeze_sector;
	int 	sector_sample_count;
	Byte WriteGate;
	int  WriteGateCount;

};

#endif /* PERCOM_Z5023_H_ */
