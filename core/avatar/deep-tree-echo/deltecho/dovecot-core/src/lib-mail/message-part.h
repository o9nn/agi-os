#ifndef MESSAGE_PART_H
#define MESSAGE_PART_H
#include "message-size.h"
struct message_part_data;
enum message_part_flags {
MESSAGE_PART_FLAG_MULTIPART = 0x01,
MESSAGE_PART_FLAG_MULTIPART_DIGEST = 0x02,
MESSAGE_PART_FLAG_MESSAGE_RFC822 = 0x04,
MESSAGE_PART_FLAG_TEXT = 0x08,
MESSAGE_PART_FLAG_UNUSED = 0x10,
MESSAGE_PART_FLAG_HAS_NULS = 0x20,
MESSAGE_PART_FLAG_IS_MIME = 0x40,
MESSAGE_PART_FLAG_OVERFLOW = 0x80,
};
struct message_part {
struct message_part *parent;
struct message_part *next;
struct message_part *children;
uoff_t physical_pos;
struct message_size header_size;
struct message_size body_size;
struct message_part_data *data;
unsigned int children_count;
enum message_part_flags flags;
void *context;
};
unsigned int message_part_to_idx(const struct message_part *part);
struct message_part *
message_part_by_idx(struct message_part *parts, unsigned int idx);
typedef bool message_part_comparator_t(
const struct message_part *p1, const struct message_part *p2);
bool message_part_is_equal(const struct message_part *p1,
const struct message_part *p2) ATTR_NULL(1, 2);
bool message_part_is_equal_ex(const struct message_part *p1,
const struct message_part *p2,
message_part_comparator_t *equals_ex) ATTR_NULL(1, 2, 3);
#endif