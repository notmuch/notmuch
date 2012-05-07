#!/usr/bin/env ruby
# coding: utf-8
# Copyright 2010, 2011, 2012 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v3

require 'mkmf'

NOTDIR = File.expand_path(File.join(File.dirname(__FILE__), '..', '..', 'lib'))
NOTHDR = File.join(NOTDIR, 'notmuch.h')
NOTLIB = File.join(NOTDIR, 'libnotmuch.a')

unless File.exists? NOTHDR
  $stderr.puts "notmuch.h is missing under #{NOTDIR}"
  exit 1
end

unless File.exists? NOTLIB
  $stderr.puts "libnotmuch.a is missing under #{NOTDIR}"
  exit 1
end

# Small hack to build with in-tree version not the installed one.
# find_header() and friends use standard include/library paths first.
$stderr.puts "Added -I#{NOTDIR} to $INCFLAGS"
$INCFLAGS = "-I#{NOTDIR}".quote + " " + $INCFLAGS
find_header('notmuch.h', NOTDIR)

$LOCAL_LIBS += NOTLIB

# Create Makefile
dir_config('notmuch')
create_makefile('notmuch')
