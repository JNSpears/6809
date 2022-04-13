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
                        dest="write",
                        type='string',
                        help="write file.",
                        metavar="FILENAME",
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
    # opt.add_option("-s", "--spaces",
    #                     action="store_true",
    #                     dest="spaces",
    #                     help="include 'spaces'.",
    #                     metavar="STRING",
    #                     default=False,
    #                     )
    # opt.add_option("-e", "--end",
    #                     dest="end",
    #                     help="file end line",
    #                     metavar="STRING",
    #                     default='',
    #                     )
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
    
    Options, Args = opt.parse_args()
    
    # debug display
    if Options.verbose:
        print(pprint.PrettyPrinter(indent=4).pformat( Options.__dict__ ), file=sys.stderr)
        print(pprint.PrettyPrinter(indent=4).pformat( Args ), file=sys.stderr)
    
    return Options, Args

SECTOR_FMT = ">HHHBHB256sH"

import struct
SECTOR_LENGTH = struct.calcsize(SECTOR_FMT)
print('SECTOR_LENGTH:', SECTOR_LENGTH)


BYTES_PER_SECTOR = 256
SECTORS_PER_TRACK = 10
TRACKS_PER_DISK = 40

SECTORS_PER_DISK = TRACKS_PER_DISK * SECTORS_PER_TRACK

PRE_SECTOR_DATA = 10
POST_SECTOR_DATA = 2

print('SECTORS_PER_DISK:', SECTORS_PER_DISK)
DSK_SIZE = (BYTES_PER_SECTOR+PRE_SECTOR_DATA+POST_SECTOR_DATA)*SECTORS_PER_TRACK*TRACKS_PER_DISK
print('DSK_SIZE:', DSK_SIZE)

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

    def __init__(self, sector_tuple):
        self.Curr, self.Back, self.Next, self.Count, self.Address, self.Type, self.Data, self.CRC = sector_tuple

    def getTuple(self):
        return self.Curr, self.Back, self.Next, self.Count, self.Address, self.Type, self.Data, self.CRC

    def getPackedString(self):
        return struct.pack(SECTOR_FMT, self.Curr, self.Back, self.Next, self.Count, self.Address, self.Type, self.Data, self.CRC)

DIR_ENT_FMT = ">8s2sHHH"
DIR_ENT_LENGTH = struct.calcsize(DIR_ENT_FMT)

class DirEntry(object):

    def __init__(self, dir_tuple):
        self.name, self.ext, self.first, self.last, self.entry = dir_tuple
        
    def getFilename(self):
        if self.isUsed():
            return (self.name.strip(b'\0') + b'.' + self.ext.strip(b'\0')).decode()
        else:
            return '<empty>'
        
    def isUsed(self):
        return self.name[0]

    @classmethod
    def printDirHeader(cls, verbose):
        if verbose: print("idx    ", end=' ')
        print("filename        first   last  entry")
        if verbose: print("----   ", end=' ')
        print("-----------     -----  -----  ----")

    def printDirEntry(self):
        print("% -12s\t% 5d  % 5d  %04x" % (self.getFilename(), self.first, self.last, self.entry))
        
    def getTuple(self):
        return self.name, self.ext, self.first, self.last, self.entry

    def __repr__(self):
        return "<DirEntry(%s %5d %5d %04x)>" % (self.getFilename(), self.first, self.last, self.entry)

    def getPackedString(self):
        return struct.pack(DIR_ENT_FMT, self.name, self.ext, self.first, self.last, self.entry)

class Dir(object):


    def __init__(self, rawdir):
        self.dir = []
        for i in range(0, len(rawdir), DIR_ENT_LENGTH):
            dirent = rawdir[i:i+DIR_ENT_LENGTH]
            self.dir.append(DirEntry(struct.unpack(DIR_ENT_FMT, dirent)))

    def printDir(self, verbose):    
        DirEntry.printDirHeader(verbose)
        for i, entry in enumerate(self.dir):
            if verbose: print("(%d)\t" % i, end=' ')
            if verbose or entry.isUsed():
                entry.printDirEntry()
            else:
                if verbose: print()

    def getRawdir(self):
        newrawdir =  []
        for i, (filename, ext, first, last, enter) in [ (i, direntry.getTuple()) for i, direntry in enumerate(self.dir)]:
            newrawdir.append(struct.pack(DIR_ENT_FMT, filename, ext, first, last, enter))
        foo = ''.join(newrawdir)
        print('len(newrawdir):', len(newrawdir))
        return newrawdir

    def findFile(self, filename):
        print('FindFile(',repr(filename),')')
        for i, entry in enumerate(self.dir):
#             print name, ext
            if entry.isUsed():
                if filename == entry.getFilename():
                    return i, entry

    def deleteFile(self, filename):
        print("deleteFile(%s)" % filename)
        info = self.findFile(filename)
        if info:
            i, entry = info
            if i < len(self.dir)-1 and self.dir[i+1].isUsed():
                print("mid")
                self.printDir(True)
                del self.dir[i]
                self.printDir(True)
                self.dir.append(DirEntry(('\0','\0', 0,0,0)))
                self.printDir(True)
            elif i < len(self.dir)-1 and not self.dir[i+1].isUsed():
                print("last not full")
                entry.name = entry.ext = '\0'
                entry.last = self.dir[i+1].last
            elif i == len(self.dir)-1:
                print("last full")
                entry.name = entry.ext = '\0'
        else:
            print('NotFound:', filename)

    def getPackedString(self):
        print("dir.getPackedString()")
        newrawdir =  []
        for i, ent in enumerate(self.dir):
            newrawdir.append(ent.getPackedString())
        newrawdir = ''.join(newrawdir)
        return newrawdir


    def findSpace(self, size):
        size_in_blocks = ( size+BYTES_PER_SECTOR-1) / BYTES_PER_SECTOR
        print('size_in_blocks:', size_in_blocks)
        prev = None
        self.printDir(True)
        for i, ent in enumerate(self.dir):
            print(i, ent)
            if i > 2: 
              print(i, ent, prev, ent.first-prev.last-1)
              if ent.first-prev.last-1 > size_in_blocks:
                # use this one!
                self.dir.insert(i,DirEntry(('\0','\0', prev.last+1, ent.first-1, 0)))
                self.dir.pop()
                self.printDir(True)
                return i, self.dir[i]
            prev = ent

class Mpx9DskImg(object):
    
    def __init__(self, filename):
        self.sectors = []
        self.dir = []
        self.filename = filename
        self.loadImage()
        self.dirty = False

    def __del__(self):
        if self.dirty:
            self.saveImage()
        print("__del__()")

    def loadImage(self):
        print("loadImage()")
        f = open(self.filename, 'rb')
        dskimg = f.read(DSK_SIZE)
        print('len(dskimg):',  len(dskimg))
        self.sectors = []
        for i in range(0, DSK_SIZE, SECTOR_LENGTH):
            raw_sector = dskimg[i:i+SECTOR_LENGTH]
            if len(raw_sector) < SECTOR_LENGTH:
                break
            self.sectors.append(Sector(struct.unpack(SECTOR_FMT, raw_sector)))
        self.loadDir()
        
    def saveImage(self):
        print("saveImage()")
        rawsectors = []
        self.saveDir()
        for sector in self.sectors:
            rawsectors.append(sector.getPackedString())
        dskimg = ''.join(rawsectors)
        f = open(self.filename, 'wb')
        f.write(dskimg)
        
    def loadDir(self):
        print("loadDir()")
        sec1 = self.sectors[1].Data
        sec2 = self.sectors[2].Data
        rawdir =  sec1+sec2
        self.dir = Dir(rawdir)
                
    def saveDir(self):
        print("saveDir()")
        sec1 = self.sectors[1]
        sec2 = self.sectors[2]
        directory = self.dir.getPackedString()
        self.sectors[1].Data = directory[0:256]
        self.sectors[2].Data = directory[256:512]
#TODO: re-calc CRC
#        self.displayDir()
        self.dirty = True
                
    def displayDir(self, verbose):
        self.dir.printDir(verbose)
        
    def readFile(self, filename):
        info = self.dir.findFile(filename)
        print(info)
        if info:
            i, (fn, ext, first, last, entry) = info
            for ii in range(first, last+1):
                print(self.sectors[ii].Data)
            
    def writeFile(self, filename):
        info = self.dir.findFile(filename)
        print('info:', info)
        if info:
            # file exists
            i, (fn, ext, first, last, entry) = info
            src = open(filename, 'rb')
            for ii in range(first, last+1):
                data = src.read(BYTES_PER_SECTOR)
                print('read:', data[:45], '...')
                self.sectors[ii].Data = data
                self.sectors[ii].Count = len(Data)
#TODO: re-calc CRC
                
            self.dirty = True    
        else:
            i, ent = self.dir.findSpace(1)
            print(i, ent)
            
# create    
    
    def deleteFile(self,filename):
        self.dir.deleteFile(filename)
        self.dirty = True


def main(args):
    Options, Args = ParseOptions()

    dskimg = Mpx9DskImg(Args[0])
            
    if Options.files:
        dskimg.displayDir(Options.verbose)
        
    if Options.read:
        dskimg.readFile(Options.read)
        
    if Options.write:
        dskimg.writeFile(Options.write)
        
    if Options.delete:
        dskimg.deleteFile(Options.delete)
        

if __name__ == '__main__':
    import sys
    sys.exit(main(sys.argv))
