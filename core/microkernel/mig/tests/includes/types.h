#ifndef TEST_TYPES_H
#define TEST_TYPES_H
typedef int another_int;
typedef struct char_struct {
char c1;
char c2;
char c3;
char c4;
} char_struct_t;
typedef char string_t[256];
typedef const char* const_string_t;
typedef struct simple_struct {
char a;
} simple_struct_t;
typedef struct complex_struct_x {
simple_struct_t a;
simple_struct_t b;
int c;
} complex_struct_x_t;
typedef struct complex_struct_y {
complex_struct_x_t a;
char b;
} complex_struct_y_t;
typedef struct complex_struct_z {
complex_struct_y_t a;
int d;
} complex_struct_z_t;
static inline int8_t int_to_int8(int n) {
return (int8_t) n;
}
static inline int int8_to_int(int8_t n) {
return (int) n;
}
#endif