from sys import argv
conffile = argv[1]
builddir = argv[2]
outfile = argv[3]

execfile(conffile)

roff_files = []
rst_files = []
out=open(outfile,'w')
for page in man_pages:
    rst_files = rst_files + ["doc/{0:s}.rst".format(page[0])]
    roff_files = roff_files + ["{0:s}/man/{1:s}.{2:d}".format(builddir,page[1],page[4])]

out.write ('MAN_ROFF_FILES := ' + ' \\\n\t'.join(roff_files)+'\n')
out.write ('MAN_RST_FILES := ' + ' \\\n\t'.join(rst_files)+'\n')
