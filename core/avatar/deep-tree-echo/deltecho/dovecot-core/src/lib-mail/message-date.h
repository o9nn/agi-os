#ifndef MESSAGE_DATE_H
#define MESSAGE_DATE_H
bool message_date_parse(const unsigned char *data, size_t size,
time_t *timestamp_r, int *timezone_offset_r);
const char *message_date_create(time_t timestamp);
#endif