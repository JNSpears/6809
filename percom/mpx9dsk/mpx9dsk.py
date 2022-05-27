#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
#  mpx9dsk.py
#  
#  Copyright 2017 James <james@james-HP-Spectre-x360-Convertible-13>
#  
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.
#  
#  

import sys, string, optparse, pprint
# from matplotlib.testing.jpl_units import sec
# from parted import disk
import os
# from importlib.metadata import files
# from Xlib.X import Above
import hexdump

import struct


BYTES_PER_SECTOR = 256
SECTORS_PER_TRACK = 10
TRACKS_PER_DISK = 40

SECTORS_PER_DISK = TRACKS_PER_DISK * SECTORS_PER_TRACK

PRE_SECTOR_DATA = 10
POST_SECTOR_DATA = 2

# print('SECTORS_PER_DISK:', SECTORS_PER_DISK)
DSK_SIZE = (BYTES_PER_SECTOR+PRE_SECTOR_DATA+POST_SECTOR_DATA)*SECTORS_PER_TRACK*TRACKS_PER_DISK
# print('DSK_SIZE:', DSK_SIZE)

# STANDARD SECTOR INTERLACE TABLE
STDSIL = (0,2,4,6,8,1,3,5,7,9)

def getBlockAsTrkSec(block):
	(trk, sec) = divmod(block, SECTORS_PER_TRACK)
	return (trk << 8) | STDSIL[sec]


# 
# ; Header = Cur(2) + Bak(2) + Nxt(2) + CNT(1) + Addr(2) + TYP(1)
# 
# DCBTRK EQU 15 ;TRACK
# DCBSEC EQU 16 ;SECTOR
# DCBCUR EQU 15 ;CURRENT TRACK/SECTOR
# DCBPRV EQU 17 ;PREVIOUS BLOCK
# DCBNXT EQU 19 ;NEXT BLOCK
# DCBCNT EQU 21 ;BYTE COUNT IN THIS BLOCK
# DCBADD EQU 22 ;DATA ADDRESS FOR THIS BLOCK
# DCBTYP EQU 24 ;BLOCK TYPE CODE
# DCBCRC EQU 25 ;DATA CRC
# 

class Sector(object):


	SECTOR_FMT  = ">HHHBHB256sH"
	SECTOR_FMT1 = ">6xB3x256x2x"
	SECTOR_FMT2 = ">HHHBHB%dsH%dx"

	SECTOR_LENGTH = struct.calcsize(SECTOR_FMT)
	# print('SECTOR_LENGTH:', SECTOR_LENGTH)


	def __init__(self, raw_sector):
		SECTOR_FMT1_LENGTH = struct.calcsize(Sector.SECTOR_FMT1)
		if raw_sector is None:
			sector_tuple = (0,0,0,0,0,0,b'\0'*BYTES_PER_SECTOR,0)
		else:
			(count,) = struct.unpack(Sector.SECTOR_FMT1, raw_sector)
			# print('count:', count)
			if count == 0:
				count = 256
			sector_tuple = struct.unpack(Sector.SECTOR_FMT2 % (count, 256-count), raw_sector)
		# print('sector_tuple:', sector_tuple)
		self.Curr, self.Back, self.Next, self.Count, self.Address, self.Type, self.Data, self.CRC = sector_tuple
		# if self.Curr != 0 or self.Back != 0 or self.Next != 0:
		# 	print('yyyyyyy', self)


	def getTuple(self):
		return self.Curr, self.Back, self.Next, self.Count, self.Address, self.Type, self.Data, self.CRC

	def getPackedString(self):
		count =  self.Count
		if count == 256:
			count = 0
		# if self.Curr != 0 or self.Back != 0 or self.Next != 0:
		# 	print('xxxxx', self)
		ps = struct.pack(Sector.SECTOR_FMT, self.Curr, self.Back, self.Next, count, self.Address, self.Type, self.Data, self.CRC)
		if count == 0:
			return ps

		# print('ps:')
		# hexdump.hexdump(ps)
		
		ps1 = ps[0:10+self.Count]
		ps2 = ps[-2:]
		ps3 = ps[10+self.Count:-2]
		# if self.Next < 20:
		# 	print('ps1:', ps1)
		# 	print('ps2:', ps2)
		# 	print('ps3:', ps3)
		psnew = b''.join([ps1,ps2,ps3])
		# print('psnew:')
		# hexdump.hexdump(psnew)
		
		return psnew
		
	def __repr__(self):
		return "<Sector(ts:%04x, <%d, >%d, #%d, @%04x, t%d, ... %04x)>" % (self.Curr, self.Back, self.Next, self.Count, self.Address, self.Type, self.CRC)

	def fixCRC(self):
		(A, B) = calcrc(self)
		# print('fixCRC: Curr:%04X crc:%02X:%02X' % (self.Curr, A, B))
		crc = (A << 8) | B
		self.CRC = crc
	
	def fixCurr(self, block):
		self.Curr = getBlockAsTrkSec(block)
	
	
	
DIR_ENT_FMT = ">8s2sHHH"
DIR_ENT_LENGTH = struct.calcsize(DIR_ENT_FMT)
ENT_PER_DIR = 32

class DirEntry(object):

	def __init__(self, dir_tuple):
		self.name, self.ext, self.first, self.last, self.entry = dir_tuple
		
	def getFilename(self):
		# print('getFilename() ::', self.isUsed(), self.name, self.ext, self.first, self.last, self.entry)
		if self.isUsed():
			ext = self.ext.strip(b'\0')
			if ext:
				return (self.name.strip(b'\0') + b'.' + self.ext.strip(b'\0')).decode()
			return (self.name.strip(b'\0')).decode()
		else:
			return '<empty>'
		
	def setFilename(self, filename):
		path, filename = os.path.split(filename)
		name, ext = os.path.splitext(filename)
		# print('name, ext:', (name, ext))
		self.name = bytes(name[:8], 'utf-8') # truncate to 8 chars
		self.ext = bytes(ext[1:3], 'utf-8') # get rid of the leading . and truncate to 2 chars

	def isUsed(self):
		# print('isUsed() self.name[0]:', repr(self.name[0]), type(self.name[0]), (self.name[0]))
		return (self.name[0])

	@classmethod
	def printDirHeader(cls, verbose):
		if verbose: print("idx    ", end=' ')
		print("filename        first   last  entry", end=' ')
		if verbose: print("#sec      K", end=' ')
		print()
		if verbose: print("----   ", end=' ')
		print("-----------     -----  -----  ----", end=' ')
		if verbose: print(" ----  -----", end=' ')
		print()

	def getFomattedDirEntry(self):
		return "% -12s\t% 5d  % 5d  %04x" % (self.getFilename(), self.first, self.last, self.entry)
	
	def printDirEntry(self):
		print(self.getFomattedDirEntry)
		
	def getTuple(self):
		return self.name, self.ext, self.first, self.last, self.entry

	def __repr__(self):
		return "<DirEntry(%s, %5d, %5d, %04x)>" % (self.getFilename(), self.first, self.last, self.entry)

	def getPackedString(self):
		return struct.pack(DIR_ENT_FMT, self.name, self.ext, self.first, self.last, self.entry)

class Dir(object):

	def __init__(self, rawdir):
		self.dir = []
		for i in range(0, len(rawdir), DIR_ENT_LENGTH):
			dirent = rawdir[i:i+DIR_ENT_LENGTH]
			self.dir.append(DirEntry(struct.unpack(DIR_ENT_FMT, dirent)))

	def printDir(self, verbose):    
		prev = None
		DirEntry.printDirHeader(verbose)
		for i, entry in enumerate(self.dir):
			
			if verbose: print("(%d)\t" % i, end=' ')
			if verbose or entry.isUsed():
				print(entry.getFomattedDirEntry(), end=' ')
			if verbose and (entry.last or entry.first):
				print("%5d" % ((entry.last if entry.last else SECTORS_PER_DISK)-entry.first), end=' ')
				print("%5.2fK" % ((((entry.last if entry.last else SECTORS_PER_DISK)-entry.first)*BYTES_PER_SECTOR)/1024), end=' ')
			
			if prev and ((prev.last+1 != entry.first and entry.first != 0) or (entry.first != 0 and entry.last == 0)):
				if verbose or entry.isUsed(): print()
				# print('gap', (prev.last+1 != entry.first and entry.first != 0), (entry.first != 0 and entry.last == 0))
				# print(prev, entry, entry.first-prev.last+1)
				if prev.last+1 != entry.first and entry.first != 0:
					print('\tgap space:', entry.first-(prev.last+1))
				if entry.first and not entry.last:
					print('\tremaining space:', SECTORS_PER_DISK-entry.first+1)
			else:
				if verbose or entry.isUsed(): print()

			prev = entry

	def getRawdir(self):
		newrawdir =  []
		for i, (filename, ext, first, last, enter) in [ (i, direntry.getTuple()) for i, direntry in enumerate(self.dir)]:
			newrawdir.append(struct.pack(DIR_ENT_FMT, filename, ext, first, last, enter))
		foo = ''.join(newrawdir)
		# print('len(newrawdir):', len(newrawdir))
		return newrawdir

	def findFile(self, filename, debug=False):
		if debug:
			print('FindFile(',repr(filename),')')
		for i, entry in enumerate(self.dir):
			if debug:
				print(i, entry, filename, entry.getFilename(), filename == entry.getFilename())
			if entry.isUsed():
				if filename == entry.getFilename():
					return i, entry

	def deleteFile(self, filename):
		# print("deleteFile(%s)" % filename)
		info = self.findFile(filename)
		if info:
			i, entry = info
			if i < len(self.dir)-1 and self.dir[i+1].isUsed():
				# entry is in the middle of the directory 
				# self.printDir(True)
				del self.dir[i]
				# self.printDir(True)
				self.dir.append(DirEntry((b'\0',b'\0', 0,0,0)))
				# self.printDir(True)
			elif i < len(self.dir)-1 and not self.dir[i+1].isUsed():
				# entry is the last used entry in the directory and there us freespace entries after it
				entry.name = entry.ext = '\0'
				entry.last = self.dir[i+1].last
			elif i == len(self.dir)-1:
				# entry is the last entry in the directory
				entry.name = entry.ext = '\0'
		else:
			print('NotFound:', filename)

	def getPackedString(self):
		# print("dir.getPackedString()")
		newrawdir = [ent.getPackedString() for i, ent in enumerate(self.dir)]
		newrawdir = b''.join(newrawdir)
		return newrawdir


 # def getFreeSpaceBlocks(self):
 # 	freespaces = [(i, d, (d.last if d.last else 399)-d.first) for (i, d) in enumerate(self.dir) if (not d.isUsed()) and d.first]
 # 	print('freeSpaces:', freespaces)
 # 	return freespaces

	def findSpace(self, size):
		size_in_blocks = int(( size+BYTES_PER_SECTOR-1) / BYTES_PER_SECTOR)
		# print('size_in_blocks:', size_in_blocks)
		# freespaceblocklist = self.getFreeSpaceBlocks()
		# canadateblocks = [(i, d, s) for (i, d, s) in freespaceblocklist if s >= size_in_blocks]
		# print('canadateblocks:', canadateblocks)
		# if canadateblocks:
		# 	i, d, s = canadateblocks[0]
		# 	if s == size_in_blocks: # use this dir entry as is.
		# 		return i, d
		# 	elif s > size_in_blocks: # use this dir entry, but split it in two.
		# 		self.dir.insert(i,DirEntry((b'\0',b'\0', d.first, d.first+size_in_blocks, 0)))
		# 		self.dir[i+1].first = d.first+size_in_blocks+1
		# 		self.dir.pop()
		# 		return i, self.dir[1]
		# else:
		# 	return None, None

		prev = None
		for i, ent in enumerate(self.dir):
			# print(i, ent)
			if i > 0 and not ent.isUsed(): 
				# print('CANIDATE ENTRY -->', i, ent, prev, ent.first-prev.last-1)
				if ent.first-prev.last-1 > size_in_blocks: # is the space between this file and Prev file big enough?
					# use this one!
					# print('found a gap')
					self.dir.insert(i,DirEntry((b'\0',b'\0', prev.last+1, ent.first-1, 0)))
					self.dir.pop()
					# self.printDir(True)
					return i, self.dir[i]
				if ent.first and not ent.last: # remainder of disk...
					# print('found space on end, ent:', ent)
					self.dir[i] = DirEntry((b'\0',b'\0', ent.first, ent.first+size_in_blocks-1, 0))
					self.dir[i+1] = DirEntry((b'\0',b'\0', ent.first+size_in_blocks, 0, 0))
					# self.printDir(True)
					return i, self.dir[i]
			prev = ent

class Mpx9DskImg(object):
	
	def __init__(self, filename, init=False):
		# print('__init__(', filename, init, ')')
		self.sectors = []
		self.dir = None
		self.filename = filename
		if not init:
			self.loadImage()
		self.dirty = False

	def __del__(self):
		# print("__del__()")
		# self.dir.getFreeSpaceBlocks()
		if self.dirty:
			self.saveImage()

	def initImage(self, bootloader, osimage, debug=False):
		if debug:
			print("initImage(%s, %s)" % (bootloader, osimage))
		self.sectors = []
		for i in range(SECTORS_PER_DISK):
			self.sectors.append(Sector(None))
		self.loadDir()
		# if debug:
		# 	self.displayDir(True)
		bootloader_size = os.path.getsize(bootloader)
		if debug:
			print('bootloader: %s (size:%d)' % (bootloader, bootloader_size))
		with open(bootloader, 'rb') as src:
			data = src.read(BYTES_PER_SECTOR)
			if debug:
				print('read:', data[:45], '...')
			self.sectors[0].Data = data
			self.sectors[0].Count = (len(data) & 0xff)
			self.sectors[0].fixCRC()

		self.dir.dir[0] = DirEntry( (b'(SYSDIR)', b'SY', 0, 2, 0) )
		self.dir.dir[1] = DirEntry( (b'\0', b'\0', 3, 0, 0) )
		if debug:
			self.displayDir(True)

		self.writeFile(osimage, 0, debug=debug)

		self.dirty = True
		if debug:
			self.displayDir(True)


	def loadImage(self):
		# print("loadImage()")
		with open(self.filename, 'rb') as f:
			dskimg = f.read(DSK_SIZE)
			# print('len(dskimg):',  len(dskimg))
			self.sectors = []
			for i in range(0, DSK_SIZE, Sector.SECTOR_LENGTH):
				raw_sector = dskimg[i:i+Sector.SECTOR_LENGTH]
				if len(raw_sector) < Sector.SECTOR_LENGTH:
					break
				self.sectors.append(Sector(raw_sector))
			self.loadDir()
		
	def saveImage(self):
		# print("saveImage()")
		self.saveDir()

		# rawsectors = []
		# for sector in self.sectors:
		#     rawsectors.append(sector.getPackedString())
		
		rawsectors = [sector.getPackedString() for sector in self.sectors]

		dskimg = b''.join(rawsectors)

		with open(self.filename, 'wb') as f:
			f.write(dskimg)
		
	def loadDir(self):
		# print("loadDir()")
		sec1 = self.sectors[1].Data
		sec2 = self.sectors[2].Data
		rawdir =  sec1+sec2
		self.dir = Dir(rawdir)
				
	def saveDir(self):
		# print("saveDir()")
		sec1 = self.sectors[1]
		sec2 = self.sectors[2]
		directory = self.dir.getPackedString()
		self.sectors[1].Data = directory[0:256]
		self.sectors[2].Data = directory[256:512]
		sec1.fixCurr(1)
		sec2.fixCurr(2)
		sec1.fixCRC()
		sec2.fixCRC()
#        self.displayDir()
		self.dirty = True
				
	def displayDir(self, verbose):
		self.dir.printDir(verbose)
		
	def readFile(self, filename):
		info = self.dir.findFile(filename)
		# print(info)
		if info:
			(i, dirent) = info # (fn, ext, first, last, entry)
			(fn, ext, first, last, entry) = dirent.getTuple()
			with open(filename, 'wb') as dst:
				for ii in range(dirent.first, dirent.last+1):
					# print(self.sectors[ii])
					data = self.sectors[ii].Data
					# if self.sectors[ii].Count:
					#     # some number of bytes less that 256
					#     data = self.sectors[ii].Data[0:self.sectors[ii].Count]
					# print(data)
					dst.write(data)

	def writeFile(self, filename, entry_address=0, buffer_address=0, normalcase=False, debug=False):
		file_size = os.path.getsize(filename)

		mpxfilename = filename
		if normalcase:
			mpxfilename = filename.upper()

		if debug:
			print('writeFile() filename: %s (size:%d entry:%04x buffer:%04x)' % (filename, file_size, entry_address, buffer_address))
		info = self.dir.findFile(mpxfilename)
		if debug:
			print('info:', info)
		
		if info:
			if debug:
				print('### file exists')
			i, ent = info
			#todo: check to see if new file will fit in space allocated for old file.
		
		else:
			if debug:
				print('### file does not exist finding space')
			i, ent = self.dir.findSpace(file_size)
			if debug:
				print('write file for free space', i, ent, ent.getTuple())
			ent.setFilename(mpxfilename)

		ent.entry = entry_address
		fn, ext, first, last, entry = ent.getTuple()

		prev = next = 0
		with open(filename, 'rb') as src:
			for ii in range(first, last+1):
				next = ii + 1
				if next == last+1:
					next = 0
				data = src.read(BYTES_PER_SECTOR)
				count = (len(data) & 0xff)
				if debug:
					print('writeFile() II:', ii, 'count:', hex(count), 'data:', data[:45], '...')
				self.sectors[ii].fixCurr(ii)
				self.sectors[ii].Back = prev
				self.sectors[ii].Next = next
				self.sectors[ii].Data = data
				self.sectors[ii].Count = count
				self.sectors[ii].Address = buffer_address
				buffer_address += BYTES_PER_SECTOR
				self.sectors[ii].fixCRC()

				prev = ii
				next += 1

		self.sectors[prev].Next = 0
		self.sectors[prev].fixCRC()
		self.dirty = True

# create    
	
	def deleteFile(self,filename):
		self.dir.deleteFile(filename)
		self.dirty = True

	def displayBlockInfo(self, blockrange, filename, verbose):
		print("displayBlockInfo(%s, %s)" % (blockrange, filename))
		if blockrange:
			print( blockrange )
			bstart, bend = blockrange
			if bend <= bstart:
				bend = bstart + 1
		elif filename:
			info = self.dir.findFile(filename)
			print(info)
			if info:
				(i, dirent) = info # (fn, ext, first, last, entry)
				(fn, ext, first, last, entry) = dirent.getTuple()
				bstart, bend = first, last
		# print('ccccc', bstart, bend)
		print(self.sectors[bstart:bend+1])
		for i, sector in enumerate(self.sectors[bstart:bend+1]):
			print(bstart+i, sector)
			if verbose:
				print(sector.Data)
	   
	def quickFix(self):
		# print("quickFix()")
		for block, sector in enumerate(self.sectors):
			# print("Block:%03d" % block, end="\n")
			sector.fixCurr(block)
			# (A, B) = calcrc(sector)
			# # print('calcrc:',"%02X:%02X" % (A, B), "*** Expected: 0A:E5")
			# crc = (A << 8) | B
			# sector.CRC = crc
			sector.fixCRC()
		self.dirty = True


# **************************************************
# * CALCULATE CRC FOR SECTOR:                      *
# **************************************************
# CALCRC BSR GETBUF POINT U TO DATA BUFFER
#  CLRA INITIALIZE CRC ON STACK
#  PSHS A
#  LDB DCBCNT,X DATA LENGTH TO B
#  BSR CRCRTN GET CRC BYTE
#  LEAU DCBCUR,X POINT TO SECTOR HEADER
#  LDB #DCBCRC-DCBCUR HEADER SIZE TO B
#  BSR CRCRTN GET CRC BYTE
#  PULS B,PC GET RESULT & EXIT
def calcrc(sector):                
	ps = sector.getPackedString()
	# SECTOR_FMT = ">HHHBHB256sH"

	A, P = 0, 0
	count = sector.Count
	if count == 0:
		count = 256
	# print('calcrc() count:', count)
	for i, c in enumerate(ps[10:10+count]):
		A, P = crc1(i, c, A, P)
	# print('calcrc() 0:10')
	for i, c in enumerate(ps[0:10]):
		A, P = crc1(i, c, A, P)
	return A, P

# CRCRTN EORA ,U+ CRC CALCULATION ROUTINE
#  ASLA
#  ROL 2,S
#  BCC CRCRT1
#  INCA
# CRCRT1 DECB DECREMENT LOOP COUNT
#  BNE CRCRTN LOOP UNTIL 0
#  RTS
def crc1(i, c,  A, P):
	# print('i:', i, 'c:', "%02x" % c)
	A = (A ^ c) & 0x00FF
	A = A << 1
	P = P << 1
	if A & 0x0100: # A has carry?
		P = P | 1
	if P & 0x0100:
		A = A + 1
	# print('crc1:',"%02x    %02x:%02x" % (c, P & 0xff, A & 0xff))
	return A & 0xff, P & 0xff

################################################################################
def ParseOptions():
	opt = optparse.OptionParser(version=('%prog'))
	
	#debug hooks...
	opt.add_option("-v", "--verbose",
						action="count",
						dest="verbose",
						help="Print more(and more) debug messages."
						)
	opt.add_option("-r", "--read",
						dest="read",
						type='string',
						help="Read file.",
						metavar="FILENAME",
						default='',
						)
	opt.add_option("-w", "--write",
						action="store_true",
						dest="write",
						help="write files.",
						metavar="FILENAMES",
						default='',
						)
	opt.add_option("-d", "--delete",
						dest="delete",
						type='string',
						help="Read file.",
						metavar="FILENAME",
						default='',
						)
	# opt.add_option("-n", "--newlines",
	#                     action="store_true",
	#                     dest="newlines",
	#                     help="include 'newlines'.",
	#                     metavar="STRING",
	#                     default=False,
	#                     )
	opt.add_option("-f", "--files",
						action="store_true",
						dest="files",
						help="list files in dsk image.",
#                         metavar="STRING",
						default=False,
						)
	
	
	opt.add_option("--file",
						dest="file",
						type='string',
						help="set block numbers for --info command from the FILENAME",
						metavar="FILENAME",
						)
	opt.add_option("--block",
						dest="block",
						type='int',
						help="set block number or range for --info command",
						nargs=2,
						metavar="BLOCKNUMBER",
						)
	opt.add_option("-i", "--info",
						action="store_true",
						dest="info",
						help="view BLOCKSPEC info.",
						metavar="BLOCKSPEC",
						)

	opt.add_option("-q", "--quickfix",
						action="store_true",
						dest="quickfix",
						help="fix disk (re-calc trk:sec & crc values.)",
						)

	opt.add_option("--init",
						dest="init",
						type='string',
						help="Initialize disk with bootsector and os",
						nargs=2,
						metavar="BOOTSECTOR OS",
						)

	opt.add_option("-e", "--entry",
	                    dest="entry",
	                    help="specify entry point address (hex) for file",
	                    default='',
						metavar="ADDRESS",
	                    )
	opt.add_option("--buffer",
	                    dest="buffer",
	                    help="specify buffer address (hex) for file to load at",
	                    default='',
						metavar="ADDRESS",
	                    )

	opt.add_option("-n", "--normalcase",
	                    action="store_true",
	                    dest="normalcase",
	                    help="normalize case of filename in .dsk to be upper on writes.",
	                    default=False,
	                    )

	# opt.add_option("-z", "--zero",
	#                     action="store_true",
	#                     dest="zero",
	#                     help="use zero....",
	#                     metavar="STRING",
	#                     default=False,
	#                     )
	# opt.add_option("-p", "--pad",
	#                     action="store_true",
	#                     dest="pad",
	#                     help="use pad....",
	#                     metavar="STRING",
	#                     default=False,
	#                     )
# TODO:
#     -init init disk (install bootloader and os)
#     -Xtract get all files
#         -o ouput dir for Above
#         -w wildcard patter for Above
   
	Options, Args = opt.parse_args()
	
	# debug display
	if Options.verbose:
		print(pprint.PrettyPrinter(indent=4).pformat( Options.__dict__ ), file=sys.stderr)
		print(pprint.PrettyPrinter(indent=4).pformat( Args ), file=sys.stderr)
	
	return Options, Args

################################################################################
def main(args):
	Options, Args = ParseOptions()

	dskimg = Mpx9DskImg(Args[0], Options.init)
		
	if Options.entry:
		entry_address = int(Options.entry, 16)
	else:
		entry_address = 0

	if Options.buffer:
		buffer_address = int(Options.buffer, 16)
	else:
		buffer_address = 0


	if Options.info:
		dskimg.displayBlockInfo(Options.block, Options.file, Options.verbose)
	elif Options.quickfix:
		dskimg.quickFix()
	elif Options.delete:
		dskimg.deleteFile(Options.delete)
	elif Options.init is not None:
		dskimg.initImage(*Options.init)

	if Options.read:
		dskimg.readFile(Options.read)
	
	if Options.write:
		for filename in Args[1:]:
			# print('filename:', filename)
			dskimg.writeFile(filename, entry_address, buffer_address, normalcase=Options.normalcase)

	if Options.files:
		dskimg.displayDir(Options.verbose)

if __name__ == '__main__':
	import sys
	sys.exit(main(sys.argv))
