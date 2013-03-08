#!/usr/bin/perl
#
# Author: Tomi Ollila
# License: same as notmuch
#
# This program is used to generate mdwn-formatted notmuch manual pages
# for notmuch wiki. Example run:
#
# $ ./devel/man-to-mdwn.pl man ../notmuch-wiki
#
# In case taken into more generic use, modify these comments and examples.

use 5.8.1;
use strict;
use warnings;

unless (@ARGV == 2) {
    warn "\n$0 <source-directory> <destination-directory>\n\n";
    # Remove/edit this comment if this script is taken into generic use.
    warn "Example: ./devel/man-to-mdwn.pl man ../notmuch-wiki\n\n";
    exit 1;
}

die "'$ARGV[0]': no such source directory\n" unless -d $ARGV[0];
die "'$ARGV[1]': no such destination directory\n" unless -d $ARGV[1];

#die "'manpages' exists\n" if -e 'manpages';
#die "'manpages.mdwn' exists\n" if -e 'manpages.mdwn';

die "Expecting '$ARGV[1]/manpages' to exist.\n" .
  "Please create it first or adjust <destination-directory>.\n"
  unless -d $ARGV[1] . '/manpages';

my $ev = 0;
my %fhash;

open P, '-|', 'find', $ARGV[0], qw/-name *.[0-9] -print/;
while (<P>)
{
    chomp;
    next unless -f $_; # follows symlink.
    $ev = 1, warn "'$_': no such file\n" unless -f $_;
    my ($in, $on) = ($_, $_);
    $on =~ s|.*/||; $on =~ tr/./-/;
    my $f = $fhash{$on};
    $ev = 1, warn "'$in' collides with '$f' ($on.mdwn)\n" if defined $f;
    $fhash{$on} = $in;
}
close P;

#undef $ENV{'GROFF_NO_SGR'};
#delete $ENV{'GROFF_NO_SGR'};
$ENV{'GROFF_NO_SGR'} = '1';
$ENV{'TERM'} = 'vt100'; # does this matter ?

my %htmlqh = qw/& &amp;   < &lt;   > &gt;   ' &apos;   " &quot;/;
# do html quotation to $_[0] (which is an alias to the given arg)
sub htmlquote($)
{
    $_[0] =~ s/([&<>'"])/$htmlqh{$1}/ge;
}

sub maymakelink($);
sub mayconvert($$);

#warn keys %fhash, "\n";

while (my ($k, $v) = each %fhash)
{
    #next if -l $v; # skip symlinks here. -- not... references there may be.

    my @lines;
    #open I, '-|', qw/groff -man -T utf8/, $v;
    open I, '-|', qw/groff -man -T latin1/, $v; # this and GROFF_NO_SGR='1'

    my ($emptyline, $pre, $hl) = (0, 0, 'h1');
    while (<I>) {
	if (/^\s*$/) {
	    $emptyline = 1;
	    next;
	}
	s/(?<=\S)\s{8,}.*//; # $hl = 'h1' if s/(?<=\S)\s{8,}.*//;
	htmlquote $_;
	s/[_&]\010&/&/g;
	s/((?:_\010[^_])+)/<u>$1<\/u>/g;
	s/_\010(.)/$1/g;
	s/((?:.\010.)+)/<b>$1<\/b>/g;
	s/.\010(.)/$1/g;

	if (/^\S/) {
	    $pre = 0, push @lines, "</pre>\n" if $pre;
	    s/<\/?b>//g;
	    chomp;
	    $_ = "\n<$hl>$_</$hl>\n";
	    $hl = 'h2';
	    $emptyline = 0;
	}
	elsif (/^\s\s\s\S/) {
	    $pre = 0, push @lines, "</pre>\n" if $pre;
	    s/(?:^\s+)?<\/?b>//g;
	    chomp;
	    $_ = "\n<h3> &nbsp; $_</h3>\n";
	    $emptyline = 0;
	}
	else {
	    $pre = 1, push @lines, "<pre>\n" unless $pre;
	    $emptyline = 0, push @lines, "\n" if $emptyline;
	}
	push @lines, $_;
    }
    $lines[0] =~ s/^\n//;
    $k = "$ARGV[1]/manpages/$k.mdwn";
    open O, '>', $k or die;
    print STDOUT 'Writing ', "'$k'\n";
    select O;
    my $pe = '';
    foreach (@lines) {
	if ($pe) {
	    if (s/^(\s+)<b>([^<]+)<\/b>\((\d+)\)//) {
		my $link = maymakelink "$pe-$2-$3";
		$link = maymakelink "$pe$2-$3" unless $link;
		if ($link) {
		    print "<a href='$link'>$pe-</a>\n";
		    print "$1<a href='$link'>$2</a>($3)";
		}
		else {
		    print "<b>$pe-</b>\n";
		    print "$1<b>$2</b>($3)";
		}
	    } else {
		print "<b>$pe-</b>\n";
	    }
	    $pe = '';
	}
	s/<b>([^<]+)<\/b>\((\d+)\)/mayconvert($1, $2)/ge;
	$pe = $1 if s/<b>([^<]+)-<\/b>\s*$//;
	print $_;
    }
}

sub maymakelink($)
{
#    warn "$_[0]\n";
    return "../$_[0]/" if exists $fhash{$_[0]};
    return '';
}

sub mayconvert($$)
{
    my $f = "$_[0]-$_[1]";
#    warn "$f\n";
    return "<a href='../$f/'>$_[0]</a>($_[1])" if exists $fhash{$f};
    return "<b>$_[0]</b>($_[1])";
}

# Finally, make manpages.mdwn

open O, '>', $ARGV[1] . '/manpages.mdwn' or die $!;
print STDOUT "Writing '$ARGV[1]/manpages.mdwn'\n";
select O;
print "Manual page index\n";
print "=================\n\n";

sub srt { my ($x, $y) = ($a, $b); $x =~ tr/./-/; $y =~ tr/./-/; $x cmp $y; }

foreach (sort srt values %fhash)
{
    my $in = $_;
    open I, '<', $in or die $!;
    my $s;
    while (<I>) {
	if (/^\s*[.]TH\s+\S+\s+(\S+)/) {
	    $s = $1;
	    last;
	}
    }
    while (<I>) {
	last if /^\s*[.]SH NAME/
    }
    my $line = '';
    while (<I>) {
	tr/\\//d;
	if (/\s*(\S+)\s+(.*)/) {
	    my $e = $2;
	    # Ignoring the NAME in file, get from file name instead.
	    #my $on = (-l $in)? readlink $in: $in;
	    my $on = $in;
	    $on =~ tr/./-/; $on =~ s|.*/||;
	    my $n = $in; $n =~ s|.*/||; $n =~ tr/./-/; $n =~ s/-[^-]+$//;
	    $line = "<a href='$on/'>$n</a>($s) $e\n";
	    last;
	}
    }
    die "No NAME in '$in'\n" unless $line;
    print "* $line";
    #warn $line;
}
