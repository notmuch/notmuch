from sys import argv
from datetime import date
from os.path import dirname, isdir
from os import makedirs, system
import re

sourcedir = argv[1]
outdir = argv[2]

if not isdir(outdir):
    makedirs(outdir, 0o755)

execfile(sourcedir + "/conf.py")


def header(file, startdocname, command, description, authors, section):
    file.write("""
{0:s}
{1:s}
{2:s}

:Date:   {3:s}
:Version: {4:s}
:Manual section: {5:d}
:Manual group: {6:s}

""".format(
'-' * len(description),
description,
'-' * len(description),
date.today().isoformat(), release, section, project))

blankre = re.compile("^\s*$")
for page in man_pages:
    outdirname = outdir + '/' + dirname(page[0])
    if not isdir(outdirname):
        makedirs(outdirname, 0o755)
    filename = outdir + '/' + page[0] + '.rst'
    outfile = open(filename, 'w')
    infile = open(sourcedir + '/' + page[0] + '.rst', 'r')

    # this is a crude hack. We look for the first blank line, and
    # insert the rst2man header there.
    #
    # XXX consider really parsing input

    count = 0
    lines = infile.readlines()
    for line in lines:
        outfile.write(line)
        if (blankre.match(line)):
            break
        count = count + 1

    del lines[0:count + 1]

    header(outfile, *page)

    outfile.write("".join(lines))
    outfile.close()

    system('set -x; rst2man {0} {1}/{2}.{3}'
           .format(filename, outdir, page[0], page[4]))
