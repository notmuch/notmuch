#!/usr/bin/python3

import argparse
from collections import namedtuple
from contextlib import ExitStack, closing
import email
import email.policy as email_policy
import logging
import mimetypes
import os
import re
import shutil
import subprocess
import sys
from tempfile import NamedTemporaryFile, TemporaryDirectory

try:
    import libarchive
except ImportError:
    libarchive = None

try:
    import magic
except ImportError:
    magic = None

MAX_RECURSE_DEFAULT = 8

# archives, extracted with libarchive
mimetypes_archive = {
    'application/zip',
    'application/x-zip',
    'application/x-zip-compressed',
    'application/rar',
    'application/vnd.rar',
    'application/x-rar',
    'application/x-rar-compressed',
    'application/x-7z-compressed',
    'application/x-tar',
    'application/x-tar-gz',
    'application/x-compressed-tar',
    'application/x-compressed',
    'application/x-bzip-compressed-tar',
    'application/x-gtar',
    'application/x-gtar-compressed',
    'application/x-tbz',
    'application/vnd.ms-cab-compressed',
    'application/x-ace-compressed',
}

# these may be compressed individual files rather than archives (handled
# via libarchive's 'raw' format), but sometimes they are mistyped compressed
# tar archives
mimetypes_compressed = {
    'application/gzip',
    'application/x-gzip',
    'application/x-gunzip',
    'application/x-xz',
    'application/x-bzip',
    'application/x-bzip2',
    'application/bzip2',
    'application/x-bzip',
}

# types that can be indexed directly as text
mimetypes_passthrough = {
    'application/x-patch',
    'application/x-diff',
    'application/x-sh',
    'application/x-shellscript',
    'application/x-csh',
    'application/x-ruby',
    'application/x-tex',
    'application/x-perl',
    'application/x-httpd-php',
    'application/javascript',
    'application/x-javascript',
    'application/emacs-lisp',
    'application/json',
    'application/x-subrip',
    'application/x-config',
    'application/xml',
    'application/ics',
    'application/x-po',
    'application/x-info',
    'application/x-texinfo',
    'application/vnd.lotus-organizer',
}

# generic or unreliable mime types;
# guess the real type from file contents or filename extension
mimetypes_guess = {
    'application/octet-stream',
    'application/octetstream',
    'octet/stream',
    'application/mac-binary',
    'application/macbinary',
    'application/text',
    'application/text-plain',
    'application/x-download',
    'application/force-download',
}

class InputData:
    mime_type = None

    logger    = None

    # original filename associated with the data
    # NOT what is returned by get_filename()
    _orig_filename = None

    _bytes      = None
    _stream     = None
    _tempfile   = None

    def __init__(self, log_parent,
                 bytes = None, stream = None,
                 filename = None, mime_type = None):
        if int(bytes is not None) + int(stream is not None) != 1:
            raise ValueError('Exactly one of bytes/stream must be provided')

        if bytes is not None:
            self._bytes = bytes
        elif stream is not None:
            self._stream = stream

        self._orig_filename = filename

        if mime_type:
            mime_type = mime_type.lower()

        if mime_type is None or mime_type in mimetypes_guess:
            mime_type = self._guess_mimetype(filename)

        self.mime_type = mime_type

        self.logger = log_parent.getChild('|' + (filename or self.mime_type) + '|')

    def _guess_mimetype(self, filename = None):
        mime_type = None

        if magic:
            t = magic.from_buffer(self.bytes(), mime = True)

            if t != 'application/octet-stream':
                mime_type = t

        if not mime_type and filename:
            t = mimetypes.guess_file_type(filename, strict = False)
            if t:
                mime_type = t[0]

        return mime_type or 'application/octet-stream'

    def get_filename(self):
        if not self._tempfile:
            ext = os.path.splitext(self._orig_filename)[1] if self._orig_filename else None

            self._tempfile = NamedTemporaryFile(suffix = ext)
            self._tempfile.write(self.bytes())
            self._tempfile.flush()
            os.fsync(self._tempfile.fileno())

        return self._tempfile.name

    def bytes(self):
        if self._stream:
            b = self._stream.read()
            self._stream = None
            self._bytes = (self._bytes or b'') + b
        return self._bytes

    def close(self):
        if self._tempfile:
            self._tempfile.close()
            self._tempfile = None


Handler = namedtuple('Handler', ('cmdline', 'need_tempdir', 'need_file_in'),
                     defaults = (False, False))
handler_map = {
    'application/(x-)?pdf' : (
        Handler(cmdline = ('pdftotext', '-', '-')),
    ),
    '(application|text)/html' : (
        Handler(cmdline = ('elinks', '-force-html', '-dump')),
        Handler(cmdline = ('w3m', '-T', 'text/html', '-dump')),
    ),
    r'application/(vnd\.openxmlformats-officedocument\..*|'
                 r'vnd\.oasis\.opendocument\..*|'
                 r'vnd\.ms-powerpoint|vnd\.ms-excel|'
                  '(ms)?word|docx?|rtf)': (
        Handler(cmdline = ('soffice', '-env:UserInstallation=file://{tempdir}', '--cat', '{file_in}'),
                           need_tempdir = True, need_file_in = True),
    ),
}

def get_handler(mime_type):
    for pat, handlers in handler_map.items():
        if not re.fullmatch(pat, mime_type):
            continue

        for handler in handlers:
            if shutil.which(handler.cmdline[0]):
                return handler

def handle_default(data, outfile):
    handler = get_handler(data.mime_type)

    if not handler:
        if (data.mime_type.startswith('text/') or
            data.mime_type in mimetypes_passthrough):
            data.logger.debug('passthrough: %s', data.mime_type)
            outfile.write(data.bytes())
            outfile.flush()
        else:
            data.logger.debug('no handler, skipping: %s', data.mime_type)

        return

    cmdline = list(handler.cmdline)

    with ExitStack() as stack:
        tempdir = None
        if handler.need_tempdir:
            tempdir = stack.enter_context(TemporaryDirectory())

        args = { 'cwd' : tempdir }

        file_in = None
        if handler.need_file_in:
            file_in = data.get_filename()
        else:
            args['input'] = data.bytes()

        for i in range(len(cmdline)):
            cmdline[i] = cmdline[i].format(file_in = file_in, tempdir = tempdir)

        data.logger.debug('executing external handler: %s', ' '.join(cmdline))

        ret = subprocess.run(cmdline, stdout = outfile, **args)

def handle_archive(data, outfile, depth, max_depth):
    # mime types in mimetypes_compressed may be either an archive (a compressed
    # tarball) or a compressed invididual file;
    # the following heuristic first tries the former, falling back on the latter
    # on error
    format_name = 'all'
    if data.mime_type in mimetypes_compressed:
        try:
            with libarchive.memory_reader(data.bytes(), format_name = format_name):
                pass
        except libarchive.ArchiveError:
            format_name = 'raw'

    try:
        with libarchive.memory_reader(data.bytes(),
                                      format_name = format_name) as archive:
            for entry in archive:
                name = None
                if str(archive.format_name) == 'raw':
                    # for raw formats the filename is typically
                    # something like foo.pdf.gz
                    if filename:
                        name = os.path.splitext(filename)[0]
                elif entry.name:
                    name = entry.name
                    # this should not happen, but apparently sometimes does
                    if isinstance(name, bytes):
                        name = name.decode(errors = 'replace')

                if name:
                    outfile.write(name.encode(errors = 'replace') + b'\n')
                    outfile.flush()

                if not entry.isfile:
                    continue

                blocks = []
                for b in entry.get_blocks():
                    # force data copy, as the buffer underlying b may be reused
                    blocks.append(b[:])

                with closing(InputData(data.logger,
                                       bytes = b''.join(blocks),
                                       filename = name)) as data_e:
                    handle_data(data_e, outfile, depth + 1, max_depth)
                    outfile.write(b'\n')
                    outfile.flush()
    except libarchive.exception.ArchiveError as e:
        data.logger.error('Error reading archive: %s', str(e))

def handle_email(msg, outfile, depth, max_depth, logger):
    headers = ('From', 'To', 'Subject', 'Date')
    for header in headers:
        val = msg.get(header)
        if val:
            line = '%s: %s\n' % (header, val)
            outfile.write(line.encode(errors = 'replace'))
            outfile.flush()

    outfile.write(b'\n')
    outfile.flush()

    for part in msg.walk():
        if not part.is_multipart():
            payload = part.get_content()
            if isinstance(payload, str):
                payload = payload.encode(errors = 'replace')

            data = InputData(logger,
                             bytes = payload,
                             filename = part.get_filename(),
                             mime_type = part.get_content_type())

            handle_data(data, outfile, depth + 1, max_depth)

def handle_mbox(data, outfile, depth, max_depth):
    # unfortunately we cannot use python's mailbox handling, because it is
    # file-based; so instead we roll a very simple mbox parser below
    lines = data.bytes().decode(errors = 'replace').splitlines()

    def flush_msg(msg_lines):
        if not msg_lines:
            return

        msg = email.message_from_string('\r\n'.join(msg_lines),
                                        policy = email_policy.SMTP)
        handle_email(msg, outfile, depth, max_depth, data.logger)

    buffered_lines = []
    for line in lines:
        if line.startswith('From '):
            outfile.write(line.encode() + b'\n')
            outfile.flush()
            flush_msg(buffered_lines)
            continue

        # add current line to current-message buffer
        buffered_lines.append(line)

    flush_msg(buffered_lines)

def handle_data(data, outfile, depth, max_depth):
    if depth > max_depth:
        data.logger.error('Maximum recursion depth %d reached, discarding %s',
                          max_depth, data.mime_type)
        return False

    if data.mime_type in mimetypes_compressed | mimetypes_archive:
        if not libarchive:
            data.logger.error('libarchive not available, cannot handle %s',
                              data.mime_type)
            return False

        return handle_archive(data, outfile, depth, max_depth)
    elif data.mime_type == 'application/mbox':
        return handle_mbox(data, outfile, depth, max_depth)
    elif data.mime_type == 'message/rfc822':
        msg = email.message_from_bytes(data.bytes(), policy = email_policy.SMTP)
        return handle_email(msg, outfile, depth, max_depth, data.logger)

    return handle_default(data, outfile)

parser = argparse.ArgumentParser()

parser.add_argument('-v', '--verbose', action = 'count', default = 0)
parser.add_argument('-q', '--quiet',   action = 'count', default = 0)
parser.add_argument('-m', '--max-recurse', default = MAX_RECURSE_DEFAULT)

args = parser.parse_args()

msgid     = os.environ.get('NOTMUCH_FILTER_MESSAGE_ID', '<unknown>')
mime_type = os.environ.get('NOTMUCH_FILTER_MIME_TYPE')
filename  = os.environ.get('NOTMUCH_FILTER_FILENAME')

logging.basicConfig(level = max(2 - args.verbose + args.quiet, 0) * 10)

outfile = sys.stdout.buffer

with closing(InputData(logging.getLogger(msgid),
                       stream = sys.stdin.buffer,
                       filename = filename, mime_type = mime_type)) as data:
    handle_data(data, outfile, 0, args.max_recurse)
