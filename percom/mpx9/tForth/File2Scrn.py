import sys, string, optparse, pprint

opt = optparse.OptionParser(version=('%prog'))

#debug hooks...
opt.add_option("-v", "--verbose",
                    action="count",
                    dest="verbose",
                    help="Print more(and more) debug messages."
                    )

opt.add_option("-p", "--pad",
                    type=int,
                    action="store",
                    dest="pad",
                    help="Print more(and more) debug messages."
                    )

opt.add_option("-D", "--directory",
                    action="store_true",
                    dest="directory",
                    help="put a directory in screens 0-2."
                    )


Options, Args = opt.parse_args()

# debug display
if Options.verbose:
    print >>sys.stderr, pprint.PrettyPrinter(indent=4).pformat( Options.__dict__ )
    print >>sys.stderr, pprint.PrettyPrinter(indent=4).pformat( Args )


linenumber = 0
h = "             +----+----1----+----2----+--- scr#%02d ---4----+----5----+----6----+"

directory = []

for filename in Args:
    f = open(filename)
    #print "File:%s Scr#%d" % (filename, linenumber/16)
    directory.append( (filename, linenumber/16) )

    for line in f.readlines():
        try:
            assert(len(string.rstrip(line)) <= 64)
            line = (string.rstrip(line) + ' '*64)[:64]
            if linenumber%16 == 0 and Options.verbose:
                print h % (linenumber/16)
            if Options.verbose:
                print "% 5d % 3d.%02d |%s|" % (linenumber+1, linenumber/16, linenumber%16, line[:64])
            else:
                sys.stdout.write(line)
            linenumber += 1
        except AssertionError:
            print filename, linenumber, line
    if linenumber%16 > 0:
        for i in range(16-linenumber%16):
            line = ' '*64
            if Options.verbose:
                print "% 5d % 3d.%02d |%s|" % (linenumber, linenumber/16, linenumber%16, line[:64])
            else:
                sys.stdout.write(line)
            linenumber += 1
if Options.pad:
    directory.append( ('Padding...', linenumber/16) )
    for i in range(Options.pad):
        line = (('( SCREEN #%02d )' % (linenumber/16)) + ' '*64)[:64]
        for j in range(16-linenumber%16):
            if linenumber%16 == 0 and Options.verbose:
                print h % (linenumber/16)
            if Options.verbose:
                print "% 5d % 3d.%02d |%s|" % (linenumber, linenumber/16, linenumber%16, line[:64])
            else:
                sys.stdout.write(line)
            line = ' '*64
            linenumber += 1

if Options.directory:
    for f,s in directory:
        print >>sys.stderr, "Screen# % 3d file: %s" % (s,f)
    print >>sys.stderr, "Screen# % 3d last screen." % ((linenumber/16)-1)