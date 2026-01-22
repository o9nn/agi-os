#ifndef INPUT_H
#define INPUT_H
#include <errno.h>
struct input;
typedef struct input *input_t;
error_t input_create (input_t *r_input, const char *encoding);
void input_destroy (input_t input);
ssize_t input_enqueue (input_t input, int nonblock, const char *data,
size_t datalen);
ssize_t input_dequeue (input_t input, int nonblock, char *data,
size_t datalen);
void input_flush (input_t input);
#endif