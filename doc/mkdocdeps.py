import sys

srcdir = sys.argv[1]
builddir = sys.argv[2]
outfile = sys.argv[3]

sys.path.insert(0, srcdir)
import conf

roff_files = []
rst_files = []
out=open(outfile,'w')
for page in conf.man_pages:
    rst_files = rst_files + ["{0:s}/{1:s}.rst".format(srcdir,page[0])]
    roff_files = roff_files + ["{0:s}/man/{1:s}.{2:d}".format(builddir,page[0],page[4])]

out.write ('MAN_ROFF_FILES := ' + ' \\\n\t'.join(roff_files)+'\n')
out.write ('MAN_RST_FILES := ' + ' \\\n\t'.join(rst_files)+'\n')
