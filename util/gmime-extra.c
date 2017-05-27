#include "gmime-extra.h"

GMimeStream *
g_mime_stream_stdout_new()
{
    GMimeStream *stream_stdout = NULL;
    GMimeStream *stream_buffered = NULL;

    stream_stdout = g_mime_stream_pipe_new (STDOUT_FILENO);
    if (!stream_stdout)
	return NULL;

    g_mime_stream_pipe_set_owner (GMIME_STREAM_PIPE (stream_stdout), FALSE);

    stream_buffered = g_mime_stream_buffer_new (stream_stdout, GMIME_STREAM_BUFFER_BLOCK_WRITE);

    g_object_unref (stream_stdout);

    return stream_buffered;
}
