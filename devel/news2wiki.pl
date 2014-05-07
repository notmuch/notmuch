#!/usr/bin/perl
#
# Author: Tomi Ollila
# License: same as notmuch

# This program is used to split NEWS file to separate (mdwn) files
# for notmuch wiki. Example run:
#
# $ ./devel/news2wiki.pl NEWS ../notmuch-wiki/news
#
# In case taken into more generic use, modify these comments and examples.

use strict;
use warnings;

unless (@ARGV == 2) {
    warn "\n$0 <source-file> <destination-directory>\n\n";
    warn "Example: ./devel/news2wiki.pl NEWS ../notmuch-wiki/news\n\n";
    exit 1;
}

die "'$ARGV[0]': no such file\n" unless -f $ARGV[0];
die "'$ARGV[1]': no such directory\n" unless -d $ARGV[1];

open I, '<', $ARGV[0] or die "Cannot open '$ARGV[0]': $!\n";

open O, '>', '/dev/null' or die $!;
my @emptylines = ();
my $cln;
print "\nWriting to $ARGV[1]:\n";
while (<I>)
{
    warn "$ARGV[0]:$.: tab(s) in line!\n" if /\t/;
    warn "$ARGV[0]:$.: trailing whitespace\n" if /\s\s$/;
    if (/^Notmuch\s+(\S+)\s+\((\d\d\d\d-\d\d-\d\d|UNRELEASED)\)\s*$/) {
	# open O... autocloses previously opened file.
	open O, '>', "$ARGV[1]/release-$1.mdwn" or die $!;
	print "+ release-$1.mdwn...\n";
	print O "[[!meta date=\"$2\"]]\n\n";
	@emptylines = ();
    }

    last if /^<!--\s*$/; # Local variables block at the end (as of now).

    # Buffer "trailing" empty lines -- dropped at end of file.
    push(@emptylines, $_), next if s/^\s*$/\n/;
    if (@emptylines) {
	print O @emptylines;
	@emptylines = ();
    }

    # Convert '*' to '`*`' and "*" to "`*`" so that * is not considered
    # as starting emphasis character there. We're a bit opportunistic
    # there -- some single * does not cause problems and, on the other
    # hand, this would not regognize already 'secured' *:s.
    s/'[*]'/'`*`'/g; s/"[*]"/"`*`"/g;

    # Convert nonindented lines that aren't already headers or
    # don't contain periods (.) or '!'s to level 4 header.
    if ( /^[^\s-]/ ) {
	my $tbc = ! /[.!]\s/;
	chomp;
	my @l = $_;
	$cln = $.;
	while (<I>) {
	    last if /^\s*$/;
	    #$cln = 0 if /^---/ or /^===/; # used for debugging.
	    $tbc = 0 if /[.!]\s/ or /^---/ or /^===/;
	    chomp; s/^\s+//;
	    push @l, $_;
	}
	if ($tbc) {
	    print O "### ", (join ' ', @l), "\n";
	}
	else {
	    #print "$ARGV[0]:$cln: skip level 4 header conversion\n" if $cln;
	    print O (join "\n", @l), "\n";
	}
	@emptylines = ( "\n" );
	next;
    }

    # Markdown doc specifies that list item may have paragraphs if those
    # are indented by 4 spaces (or a tab) from current list item marker
    # indentation (paragraph meaning there is empty line in between).
    # If there is empty line and next line is not indented 4 chars then
    # that should end the above list. This doesn't happen in all markdown
    # implementations.
    # In our NEWS case this problem exists in release 0.6 documentation.
    # It can be avoided by removing 2 leading spaces in lines that are not
    # list items and requiring all that indents are 0, 2, and 4+ (to make
    # regexp below work).
    # Nested lists are supported but one needs to be more careful with
    # markup there (as the hack below works only on first level).

    s/^[ ][ ]// unless /^[ ][ ](?:[\s*+-]|\d+\.)\s/;

    print O $_;
}
print "\ndone.\n";
close O;
