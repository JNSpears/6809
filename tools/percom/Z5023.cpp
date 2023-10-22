/*
 * Z5023.cpp
 *
 *  Created on: Apr 11, 2022
 *      Author: james
 */

#include "Z5023.h"
#include "mc6809dbg.h"

// #ifdef MC6809DBG
#include "hexadump.h"
// #endif
#include <cstring>

//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
Z5023::Z5023() {
	// TODO Auto-generated constructor stub
//	fprintf(stderr, "Z5023::Z5023()\n\r");
	cycles = 0L;
	current_drive = 0;
	motor_on = false;
	SectorBit = false;
	IndexBit = false;
	Sector = 0;
	Track = 0;
	ReadIndex = 0;
	freeze_sector = false;
	sector_sample_count = 0;
	debug = 0;
	WriteGate = 0;

	mountedDisks[0].Mount("foo1.dsk");
	mountedDisks[1].Mount("foo2.dsk");
	mountedDisks[2].Mount("foo3.dsk");
	mountedDisks[3].Mount("foo4.dsk");
}

//////////////////////////////////////////////////////////////////////////////////////////////////
Z5023::~Z5023() {
	// TODO Auto-generated destructor stub
	mountedDisks[0].UnMount();
	mountedDisks[1].UnMount();
	mountedDisks[2].UnMount();
	mountedDisks[3].UnMount();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void Z5023::reset()
{
	cycles = 0L;
	freeze_sector = false;
	sector_sample_count = 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void Z5023::tick(uint8_t ticks)
{
//	fprintf(stderr, "Z5023::tick()\n\r");
	cycles += ticks;
//	if (cycles < 1000)
//		return;
//
//	cycles = 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
Byte Z5023::read(Word offset)
{
	Byte retval = 0;
	int current_phy_drive = 0;

	switch (offset)
	{
	case 0:
		//	*** CC00 	Read USRT status:
		//	*** 		bit 0 = 1 means disk unit ready to send byte to computer
		//	*** 			at address CC01 during read operation
		//	*** 		bit 7 = 1 means disk unit ready to receive byte from computer
		//	*** 			at address CC01 during write operation
		retval = 0x01 /* read ready */ | WriteGate;
		// if (debug >= 2)
		// 	fprintf(stderr, "Z5023::read(0) --> %02x\r\n", retval);
		break;

	case 1:
		//	*** CC01 	Address used to transmit data from disk drive to computer during read
		//	*** 		operation
		switch (ReadIndex)
		{
		case -1:
			retval = 0xFB;
			break;
		default:
			retval = ((Byte*)(&currSectorImage))[ReadIndex];
			if (ReadIndex == 266)
			{
				freeze_sector = false;
			}
			break;
		};
		// if ((debug >= 2) && ((ReadIndex < 14) || (ReadIndex > 260)))
		// 	fprintf(stderr, "Z5023::read(1) (n:%d) --> %02x\r\n", ReadIndex, retval);
		ReadIndex++;
		break;

	case 2:
		//	*** CC02 	During read operation, bits 0 thru 3 contain current sector number in
		//	*** 		binary
		// if (debug >= 2)
		// 	fprintf(stderr, "Z5023::read(2) Sector#:%d\r\n", Sector);
		retval = Sector;
		break;

	case 3:
		//	*** CC03 	Drive status byte: see table 2b.
		//	*** Bit	Value	Meaning
		//	*** 0	1	Write protect notch in disk covered; disk is protected
		//	*** 1	1 	Head is at track 0
		//	*** 2	0	Drive motor is on
		//	*** 3	0	Drive circuit is ready to write to disk
		//	*** 4	1	Sector pulse; drive detects sector hole
		//	*** 5	1	Index pulse; drive detects special index hole
		//	*** 6,7		Binary number of drive selected (01 thru 03)
		//	*** *********************************************************************

		IndexBit = false;

		switch ((sector_sample_count++) % 300)
		{
		case 0:
			SectorBit = true;
			Sector = (Sector + 1) % 10;
			break;
		case 5:
			SectorBit = false;
			break;
		case 20:
			if (Sector == 0)
				IndexBit = true;
			break;
		case 25:
			IndexBit = false;
			break;
		}

		if (WriteGate && (WriteGateCount++ > 3000))
		{
			WriteGate = 0;
		
			current_phy_drive = (current_drive - 1) % DRIVES_PER_SYSTEM;
			if (current_phy_drive == -1)
				current_phy_drive = 3;

			if (debug >= 1)
				fprintf(stderr, "Z5023::read(3) write sector current_phy_drive: %d\r\n", current_phy_drive);
			mountedDisks[current_phy_drive].WriteSector(((Byte*)&currSectorImage)[0], ((Byte*)&currSectorImage)[1], (Byte*)&currSectorImage);
		}

		retval 	= (current_drive<<6)
				| ((Track == 0)?0:0x2)
				| (motor_on?0:0x4) // 0 active
				| (WriteGate?0:0x8) // O active
				| (SectorBit?0x10:0)
//				| (IndexBit?0x20:0)
			;
		// if (debug >= 2)
		// 	fprintf(stderr, "Z5023::read(3) IndexBit:%d SectorBit:%d motor_on:%d Sector:%d TrackZ:%d Trk:%d (cycles=%ld) --> 0x%02x\r\n",
		// 		(int)IndexBit, (int)SectorBit, motor_on, Sector, (Track == 0), Track, cycles, retval);
		break;

	case 4:
		//	*** CC04 	Accessing this location with a load instruction (LDA) causes a read
		//	*** 		operation to take place
		ReadIndex = -1;
		Sector = (Sector + 1) % 10;
		freeze_sector = true;

		current_phy_drive = (current_drive - 1) % DRIVES_PER_SYSTEM;
		if (current_phy_drive == -1)
			current_phy_drive = 3;

		if (debug >= 1)
		{
			fprintf(stderr, "Z5023::read(4) read sector current_drive: %d\r\n", current_drive);
			fprintf(stderr, "Z5023::read(4) read sector current_phy_drive: %d\r\n", current_phy_drive);
			fprintf(stderr, "Z5023::read(4) read sector FILENAME: %s\r\n", mountedDisks[current_phy_drive].filename);			
		}

		mountedDisks[current_phy_drive].ReadSector(Track, Sector, (Byte*)&currSectorImage);

		retval = 0xff;
		break;

	case 5:
		//	*** CC05		Accessing this location with either a load (LDA) or store instruction
		//	*** 			causes a motor on pulse to be sent to the disk drive
//		fprintf(stderr, "Z5023::read(5) -- MOTOR on (cycles=%ld)\n", cycles);
		motor_on = true;
		retval = 0xff;
		break;

	case 6:
		//	*** CC06		Accessing this location with either a load or store instruction causes a
		//	*** 			motor off pulse to be sent to the disk drive
//		fprintf(stderr, "Z5023::read(6) -- MOTOR OFF\n");
		motor_on = false;
		retval = 0xff;
		break;

	default:
		// if (debug >= 2)
		// 	fprintf(stderr, "Z5023::read(@=%02x)-->%02x\n", offset, retval);
		break;

	}
	return retval;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void Z5023::write(Word offset, Byte val)
{

	bool direction;
	bool step_pulse;
	int step_offset;

	switch (offset)
	{
	case 1:
			// *** CC01        Address used to transmit data from computer to disk unit during write
			// ***             operation

			if (ReadIndex >= 0)
				((Byte*)(&currSectorImage))[ReadIndex] = val;
			
			// if ((debug >= 2)
			//  // && ((ReadIndex < 14) || (ReadIndex > 260))
			//  )
			// 	fprintf(stderr, "Z5023::write(1) (n:%d) <-- %02x\r\n", ReadIndex, val);

			if (ReadIndex == 266)
			{
				freeze_sector = false;
			}

			ReadIndex++;
			break;

	case 3:
		//	*** CC03		Data to select drive and head movement direction:
		//	*** 			bit 4 		direction of head movement: 1 = in, 0 = out
		//	*** 			bit 5 		step pulse bit; causes data transfer head to
		//	*** 					jump to next track in direction given by bit 4
		//	*** 			bits 6, 7	binary number of drive to be selected
		direction = ((val>>4) & 1);
		step_pulse = ((val>>5) & 1);
		current_drive = ((val>>6) & 3);
		step_offset = (direction ? +1 : -1);
		if (step_pulse)
		{
			Track += step_offset;
		}
		if (debug >= 2)
			fprintf(stderr, "Z5023::write(@=%02x,v=%02x) cDr:%d Step:%d StepDir:%d Trk:%d \r\n",
				offset, val, current_drive, step_pulse, step_offset, Track);
		break;

	case 4:
		// *** CC04        Accessing this location with a store instruction (STA) causes a write
		// ***             operation to take place
		ReadIndex = -17;
		WriteGate = 0x80;
		WriteGateCount = 0;
		Sector = (Sector + 1) % 10;
		freeze_sector = true;

		if (debug >= 2)
			fprintf(stderr, "Z5023::write(@=%02x,v=%02x) Start WriteSector\n",
				offset, val);

		break;

	case 5:
		//	*** CC05		Accessing this location with either a load (LDA) or store instruction
		//	*** 			causes a motor on pulse to be sent to the disk drive
		motor_on = true;
		// fprintf(stderr, "Z5023::write(@=%02x,v=%02x) Motor **ON**\n", offset, val); // Note: if writing a sector we are now done.
		break;

	case 6:
		//	*** CC06		Accessing this location with either a load or store instruction causes a
		//	*** 			motor off pulse to be sent to the disk drive
		motor_on = false;
		break;

	default:
		if (debug >= 2)
			fprintf(stderr, "Z5023::write(@=%02x,v=%02x) ????????????\r\n", offset, val);
		break;
	}
}


//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
MPX9_DskImg::MPX9_DskImg()
{
	filename = NULL;
	memset(DskImage, 0, sizeof(DskImage));
	debug = 0;
	dirty = false;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
MPX9_DskImg::~MPX9_DskImg()
{
	if (dirty)
		UnMount();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void	MPX9_DskImg::Mount(const char* filename)
{
		this->filename = filename;

		if (debug >= 1)
			fprintf(stderr, "mount(%s)\r\n", this->filename);
		FILE *dskFile = fopen(this->filename, "rb");
		if (dskFile)
		{
			int status = fread(DskImage, sizeof(DskImage[0]), sizeof(DskImage), dskFile);
			(void)status;
//			fprintf(stderr, "status: %d\r\n", status);
			fclose(dskFile);
// #ifdef MC6809DBG
// 			if (debug >= 1)
// 				hexdump(DskImage, 4*sizeof(MPX9_SECTOR), 0, "MPX9_DskImg::Mount()");
// #endif
			dirty = false;
		}
//		fprintf(stderr, "EXIT mount()\r\n");
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void MPX9_DskImg::UnMount(void)
{
		// fprintf(stderr, "UnMount(%s)\r\n", this->filename);
		if (dirty)
		{
				if (debug >= 1)
					fprintf(stderr, "UnMount(%s) is dirty\r\n", this->filename);
				FILE *dskFile = fopen(this->filename, "wb");
				if (dskFile)
				{
					int status = fwrite(DskImage, sizeof(DskImage[0]), sizeof(DskImage), dskFile);
					if (status)
						fprintf(stderr, "UnMount(%s) error status: %d\r\n", this->filename, status);
					fclose(dskFile);
					// fprintf(stderr, "UnMount(%s) done\r\n", this->filename);
					dirty = false;
				}
		}
}

//////////////////////////////////////////////////////////////////////////////////////////////////
int		MPX9_DskImg::TrackSector2Block(int track, int sector)
{
//	C016                  (     BootMpx9.asm):00291         SectorInterleave:
//	C016 0002040608010305 (     BootMpx9.asm):00292          			 fcb 00,02,04,06,08,01,03,05,07,09
//					    			   0, 1, 2, 3, 4, 5, 6, 7, 8, 9	index:	 00,01,02,03,04,05,06,07,08,09
	const int SectorDeInterleave[] = { 0, 5, 1, 6, 2, 7, 3, 8, 4, 9};

	int block = (track * SECTORS_PER_TRACK) + SectorDeInterleave[sector];
	return block;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void	MPX9_DskImg::ReadSector(int track, int sector, Byte* pData)
{
	if (debug >= 1)
		fprintf(stderr, "MPX9_DskImg::ReadSector(t:%d, s:%d, ...)\r\n", track, sector);
	int block = TrackSector2Block(track, sector);
	if (debug >= 1)
		fprintf(stderr, "block: %d\r\n", block);

	long int offset = block * sizeof(MPX9_SECTOR);
	if (debug >= 1)
		fprintf(stderr, "offset: %ld\r\n", offset);

	MPX9_SECTOR *pSector = (MPX9_SECTOR*)(&DskImage[offset]);

	memcpy(pData, pSector, sizeof(MPX9_SECTOR));

	// if (debug >= 1)
	// 	hexdump((char*)pSector, sizeof(MPX9_SECTOR), 0, "MPX9_SECTOR - Read");

	// if dirty and read sector 1 then write image back to disk
	if (dirty)
		UnMount();
}

//////////////////////////////////////////////////////////////////////////////////////////////////
void	MPX9_DskImg::WriteSector(int track, int sector, Byte* pData)
{
	if (debug >= 1)
		fprintf(stderr, "MPX9_DskImg::WriteSector(t:%d, s:%d, ...)\r\n", track, sector);
	int block = TrackSector2Block(track, sector);
	if (debug >= 1)
		fprintf(stderr, "block: %d\r\n", block);

	long int offset = block * sizeof(MPX9_SECTOR);
	if (debug >= 1)
		fprintf(stderr, "offset: %ld\r\n", offset);

	MPX9_SECTOR *pSector = (MPX9_SECTOR*)(&DskImage[offset]);

	memcpy(pSector, pData, sizeof(MPX9_SECTOR));

	// if (debug >= 1)
	// 	hexdump((char*)pSector, sizeof(MPX9_SECTOR), 0, "MPX9_SECTOR - Write");

	dirty = true;
}

