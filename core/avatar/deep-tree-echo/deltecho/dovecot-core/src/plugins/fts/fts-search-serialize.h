#ifndef FTS_SEARCH_SERIALIZE_H
#define FTS_SEARCH_SERIALIZE_H
void fts_search_serialize(buffer_t *buf, const struct mail_search_arg *args);
void fts_search_deserialize(struct mail_search_arg *args,
const buffer_t *buf);
void fts_search_deserialize_add_matches(struct mail_search_arg *args,
const buffer_t *buf);
void fts_search_deserialize_add_nonmatches(struct mail_search_arg *args,
const buffer_t *buf);
#endif