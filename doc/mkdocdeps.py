from sys import argv
conffile = argv[1]
builddir = argv[2]
outfile = argv[3]

execfile(conffile)

roff_files = []
out=open(outfile,'w')
for page in man_pages:
    roff_files = roff_files + ["{0:s}/man/{1:s}.{2:d}".format(builddir,page[1],page[4])]

out.write ('MAN_ROFF_FILES := ' + ' \\\n\t'.join(roff_files)+'\n')
