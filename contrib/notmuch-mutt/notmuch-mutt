#!/usr/bin/perl -w
#
# notmuch-mutt - notmuch (of a) helper for Mutt
#
# Copyright: © 2011-2015 Stefano Zacchiroli <zack@upsilon.cc>
# License: GNU General Public License (GPL), version 3 or above
#
# See the bottom of this file for more documentation.
# A manpage can be obtained by running "pod2man notmuch-mutt > notmuch-mutt.1"

use strict;
use warnings;

use File::Path;
use Getopt::Long qw(:config no_getopt_compat);
use Mail::Header;
use Mail::Box::Maildir;
use Pod::Usage;
use String::ShellQuote;
use Term::ReadLine;
use Digest::SHA;


my $xdg_cache_dir = "$ENV{HOME}/.cache";
$xdg_cache_dir = $ENV{XDG_CACHE_HOME} if $ENV{XDG_CACHE_HOME};
my $cache_dir = "$xdg_cache_dir/notmuch/mutt";


# create an empty maildir (if missing) or empty an existing maildir"
sub empty_maildir($) {
    my ($maildir) = (@_);
    rmtree($maildir) if (-d $maildir);
    my $folder = new Mail::Box::Maildir(folder => $maildir,
					create => 1);
    $folder->close();
}

# search($maildir, $remove_dups, $query)
# search mails according to $query with notmuch; store results in $maildir
sub search($$$) {
    my ($maildir, $remove_dups, $query) = @_;
    my $dup_option = "";

    $query = shell_quote($query);

    if ($remove_dups) {
      $dup_option = "--duplicate=1";
    }

    empty_maildir($maildir);
    system("notmuch search --output=files $dup_option $query"
	   . " | sed -e 's: :\\\\ :g'"
	   . " | xargs -r -I searchoutput ln -s searchoutput $maildir/cur/");
}

sub prompt($$) {
    my ($text, $default) = @_;
    my $query = "";
    my $term = Term::ReadLine->new( "notmuch-mutt" );
    my $histfile = "$cache_dir/history";

    $term->ornaments( 0 );
    $term->unbind_key( ord( "\t" ) );
    $term->MinLine( 3 );
    $histfile = $ENV{MUTT_NOTMUCH_HISTFILE} if $ENV{MUTT_NOTMUCH_HISTFILE};
    $term->ReadHistory($histfile) if (-r $histfile);
    while (1) {
	chomp($query = $term->readline($text, $default));
	if ($query eq "?") {
	    system("man", "notmuch-search-terms");
	} else {
	    $term->WriteHistory($histfile);
	    return $query;
	}
    }
}

sub get_message_id() {
    my $mid = undef;
    my @headers = ();

    while (<STDIN>) {  # collect header lines in @headers
	push(@headers, $_);
	last if $_ =~ /^$/;
    }
    my $head = Mail::Header->new(\@headers);
    $mid = $head->get("message-id") or undef;

    if ($mid) {  # Message-ID header found
	$mid =~ /^<(.*)>$/;  # extract message id
	$mid = $1;
    } else {  # Message-ID header not found, synthesize a message id
	      # based on SHA1, as notmuch would do.  See:
	      # http://git.notmuchmail.org/git/notmuch/blob/HEAD:/lib/sha1.c
	my $sha = Digest::SHA->new(1);
	$sha->add($_) foreach(@headers);
	$sha->addfile(\*STDIN);
	$mid = 'notmuch-sha1-' . $sha->hexdigest;
    }

    return $mid;
}

sub search_action($$$@) {
    my ($interactive, $results_dir, $remove_dups, @params) = @_;

    if (! $interactive) {
	search($results_dir, $remove_dups, join(' ', @params));
    } else {
	my $query = prompt("search ('?' for man): ", join(' ', @params));
	if ($query ne "") {
	    search($results_dir, $remove_dups, $query);
	}
    }
}

sub thread_action($$@) {
    my ($results_dir, $remove_dups, @params) = @_;

    my $mid = get_message_id();
    if (! defined $mid) {
	empty_maildir($results_dir);
	die "notmuch-mutt: cannot find Message-Id, abort.\n";
    }
    my $search_cmd = 'notmuch search --output=threads ' . shell_quote("id:$mid");
    my $tid = `$search_cmd`;	# get thread id
    chomp($tid);

    search($results_dir, $remove_dups, $tid);
}

sub tag_action(@) {
    my $mid = get_message_id();
    defined $mid or die "notmuch-mutt: cannot find Message-Id, abort.\n";

    system("notmuch", "tag", @_, "--", "id:$mid");
}

sub die_usage() {
    my %podflags = ( "verbose" => 1,
		    "exitval" => 2 );
    pod2usage(%podflags);
}

sub main() {
    mkpath($cache_dir) unless (-d $cache_dir);

    my $results_dir = "$cache_dir/results";
    my $interactive = 0;
    my $help_needed = 0;
    my $remove_dups = 0;

    my $getopt = GetOptions(
	"h|help" => \$help_needed,
	"o|output-dir=s" => \$results_dir,
	"p|prompt" => \$interactive,
	"r|remove-dups" => \$remove_dups);
    if (! $getopt || $#ARGV < 0) { die_usage() };
    my ($action, @params) = ($ARGV[0], @ARGV[1..$#ARGV]);

    foreach my $param (@params) {
      $param =~ s/folder:=/folder:/g;
    }

    if ($help_needed) {
	die_usage();
    } elsif ($action eq "search" && $#ARGV == 0 && ! $interactive) {
	print STDERR "Error: no search term provided\n\n";
	die_usage();
    } elsif ($action eq "search") {
	search_action($interactive, $results_dir, $remove_dups, @params);
    } elsif ($action eq "thread") {
	thread_action($results_dir, $remove_dups, @params);
    } elsif ($action eq "tag") {
	tag_action(@params);
    } else {
	die_usage();
    }
}

main();

__END__

=head1 NAME

notmuch-mutt - notmuch (of a) helper for Mutt

=head1 SYNOPSIS

=over

=item B<notmuch-mutt> [I<OPTION>]... search [I<SEARCH-TERM>]...

=item B<notmuch-mutt> [I<OPTION>]... thread < I<MAIL>

=item B<notmuch-mutt> [I<OPTION>]... tag [I<TAGS>]... < I<MAIL>

=back

=head1 DESCRIPTION

notmuch-mutt is a frontend to the notmuch mail indexer capable of populating
a maildir with search results.

=head1 OPTIONS

=over 4

=item -o DIR

=item --output-dir DIR

Store search results as (symlink) messages under maildir DIR. Beware: DIR will
be overwritten. (Default: F<~/.cache/notmuch/mutt/results/>)

=item -p

=item --prompt

Instead of using command line search terms, prompt the user for them (only for
"search").

=item -r

=item --remove-dups

Remove emails with duplicate message-ids from search results.  (Passes
--duplicate=1 to notmuch search command.)  Note this can hide search
results if an email accidentally or maliciously uses the same message-id
as a different email.

=item -h

=item --help

Show usage information and exit.

=back

=head1 INTEGRATION WITH MUTT

notmuch-mutt can be used to integrate notmuch with the Mutt mail user agent
(unsurprisingly, given the name). To that end, you should define macros like
the following in your Mutt configuration (usually one of: F<~/.muttrc>,
F</etc/Muttrc>, or a configuration snippet under F</etc/Muttrc.d/>):

    macro index <F8> \
    "<enter-command>set my_old_pipe_decode=\$pipe_decode my_old_wait_key=\$wait_key nopipe_decode nowait_key<enter>\
    <shell-escape>notmuch-mutt -r --prompt search<enter>\
    <change-folder-readonly>`echo ${XDG_CACHE_HOME:-$HOME/.cache}/notmuch/mutt/results`<enter>\
    <enter-command>set pipe_decode=\$my_old_pipe_decode wait_key=\$my_old_wait_key<enter>" \
          "notmuch: search mail"

    macro index <F9> \
    "<enter-command>set my_old_pipe_decode=\$pipe_decode my_old_wait_key=\$wait_key nopipe_decode nowait_key<enter>\
    <pipe-message>notmuch-mutt -r thread<enter>\
    <change-folder-readonly>`echo ${XDG_CACHE_HOME:-$HOME/.cache}/notmuch/mutt/results`<enter>\
    <enter-command>set pipe_decode=\$my_old_pipe_decode wait_key=\$my_old_wait_key<enter>" \
          "notmuch: reconstruct thread"

    macro index <F6> \
    "<enter-command>set my_old_pipe_decode=\$pipe_decode my_old_wait_key=\$wait_key nopipe_decode nowait_key<enter>\
    <pipe-message>notmuch-mutt tag -- -inbox<enter>\
    <enter-command>set pipe_decode=\$my_old_pipe_decode wait_key=\$my_old_wait_key<enter>" \
          "notmuch: remove message from inbox"

The first macro (activated by <F8>) prompts the user for notmuch search terms
and then jump to a temporary maildir showing search results. The second macro
(activated by <F9>) reconstructs the thread corresponding to the current mail
and show it as search results. The third macro (activated by <F6>) removes the
tag C<inbox> from the current message; by changing C<-inbox> this macro may be
customised to add or remove tags appropriate to the users notmuch work-flow.

To keep notmuch index current you should then periodically run C<notmuch
new>. Depending on your local mail setup, you might want to do that via cron,
as a hook triggered by mail retrieval, etc.

=head1 SEE ALSO

mutt(1), notmuch(1)

=head1 AUTHOR

Copyright: (C) 2011-2012 Stefano Zacchiroli <zack@upsilon.cc>

License: GNU General Public License (GPL), version 3 or higher

=cut
