#ifndef iutil2_INCLUDED
# define iutil2_INCLUDED
#define MAX_PASSWORD 64
typedef struct password_s {
uint size;
byte data[MAX_PASSWORD];
} password;
# define NULL_PASSWORD {0, {0}}
int param_read_password(gs_param_list *, const char *, password *);
int param_write_password(gs_param_list *, const char *, const password *);
int param_check_password(gs_param_list *, const password *);
int dict_read_password(password *, const ref *, const char *);
int dict_write_password(const password *, ref *, const char *, bool);
#endif