#!/usr/bin/env ruby
# coding: utf-8
# Copyright 2010, 2011 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU General Public License v3

require 'mkmf'

# Notmuch Library
find_header('notmuch.h', '../../lib')
find_library('notmuch', 'notmuch_database_create', '../../lib')

# Create Makefile
dir_config('notmuch')
create_makefile('notmuch')
