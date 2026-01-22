#ifndef ISTREAM_RAW_MBOX_H
#define ISTREAM_RAW_MBOX_H
struct istream *i_stream_create_raw_mbox(struct istream *input);
uoff_t istream_raw_mbox_get_start_offset(struct istream *stream);
int istream_raw_mbox_get_header_offset(struct istream *stream,
uoff_t *hdr_offset_r);
int istream_raw_mbox_get_body_offset(struct istream *stream,
uoff_t *body_offset_r);
int istream_raw_mbox_get_body_size(struct istream *stream,
uoff_t expected_body_size,
uoff_t *body_size_r);
time_t istream_raw_mbox_get_received_time(struct istream *stream);
const char *istream_raw_mbox_get_sender(struct istream *stream);
bool istream_raw_mbox_has_crlf_ending(struct istream *stream);
int istream_raw_mbox_next(struct istream *stream, uoff_t expected_body_size);
int istream_raw_mbox_seek(struct istream *stream, uoff_t offset);
void istream_raw_mbox_set_next_offset(struct istream *stream, uoff_t offset);
bool istream_raw_mbox_is_eof(struct istream *stream);
bool istream_raw_mbox_is_corrupted(struct istream *stream);
void istream_raw_mbox_set_locked(struct istream *stream);
void istream_raw_mbox_set_unlocked(struct istream *stream);
#endif