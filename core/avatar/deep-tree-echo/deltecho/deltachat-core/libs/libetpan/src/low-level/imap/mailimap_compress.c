#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include <stddef.h>
#include "mailimap_compress.h"
#include "mailimap.h"
#include "mailimap_sender.h"
#include "mailstream_compress.h"
#include <stdio.h>
LIBETPAN_EXPORT
int mailimap_compress(mailimap * session)
{
struct mailimap_response * response;
int r;
int res;
int error_code;
mailstream_low * compressed_stream;
mailstream_low * low;
r = mailimap_send_current_tag(session);
if (r != MAILIMAP_NO_ERROR) {
res = r;
goto err;
}
r = mailimap_token_send(session->imap_stream, "COMPRESS DEFLATE");
if (r != MAILIMAP_NO_ERROR) {
res = r;
goto err;
}
r = mailimap_crlf_send(session->imap_stream);
if (r != MAILIMAP_NO_ERROR) {
res = r;
goto err;
}
if (mailstream_flush(session->imap_stream) == -1) {
res = MAILIMAP_ERROR_STREAM;
goto err;
}
if (mailimap_read_line(session) == NULL) {
res = MAILIMAP_ERROR_STREAM;
goto err;
}
r = mailimap_parse_response(session, &response);
if (r != MAILIMAP_NO_ERROR) {
res = r;
goto err;
}
error_code = response->rsp_resp_done->rsp_data.rsp_tagged->rsp_cond_state->rsp_type;
mailimap_response_free(response);
if (error_code != MAILIMAP_RESP_COND_STATE_OK) {
res = MAILIMAP_ERROR_EXTENSION;
goto err;
}
low = mailstream_get_low(session->imap_stream);
compressed_stream = mailstream_low_compress_open(low);
if (compressed_stream == NULL) {
res = MAILIMAP_ERROR_STREAM;
goto err;
}
mailstream_low_set_timeout(compressed_stream, session->imap_timeout);
mailstream_set_low(session->imap_stream, compressed_stream);
return MAILIMAP_NO_ERROR;
err:
return res;
}
LIBETPAN_EXPORT
int mailimap_has_compress_deflate(mailimap * session)
{
return mailimap_has_extension(session, "COMPRESS=DEFLATE");
}