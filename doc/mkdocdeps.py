from sys import argv
srcdir = argv[1]
builddir = argv[2]
outfile = argv[3]

execfile(srcdir + '/conf.py')


roff_files = []
rst_files = []
out=open(outfile,'w')
for page in man_pages:
    rst_files = rst_files + ["{0:s}/{1:s}.rst".format(srcdir,page[0])]
    roff_files = roff_files + ["{0:s}/man/{1:s}.{2:d}".format(builddir,page[0],page[4])]

out.write ('MAN_ROFF_FILES := ' + ' \\\n\t'.join(roff_files)+'\n')
out.write ('MAN_RST_FILES := ' + ' \\\n\t'.join(rst_files)+'\n')
