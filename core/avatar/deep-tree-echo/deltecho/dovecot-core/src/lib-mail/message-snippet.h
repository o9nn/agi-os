#ifndef MESSAGE_SNIPPET_H
#define MESSAGE_SNIPPET_H
int message_snippet_generate(struct istream *input,
unsigned int max_snippet_chars,
string_t *snippet);
#endif