#ifndef MESSAGE_BINARY_PART_H
#define MESSAGE_BINARY_PART_H
struct message_binary_part {
struct message_binary_part *next;
uoff_t physical_pos;
uoff_t binary_hdr_size;
uoff_t binary_body_size;
unsigned int binary_body_lines_count;
};
void message_binary_part_serialize(const struct message_binary_part *parts,
buffer_t *dest);
int message_binary_part_deserialize(pool_t pool, const void *data, size_t size,
struct message_binary_part **parts_r);
#endif