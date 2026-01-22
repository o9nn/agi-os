#ifndef _DICT_FILE_UTILITIES_H_
#define _DICT_FILE_UTILITIES_H_
#include <stdbool.h>
#include <stdio.h>
char * join_path(const char * prefix, const char * suffix);
FILE * dictopen(const char *filename, const char *how);
void * object_open(const char *filename,
void * (*opencb)(const char *, const void *),
const void * user_data);
bool check_db(const char *lang);
bool check_atomspace(const char *lang);
bool file_exists(const char * dict_name);
char * get_file_contents(const char *filename);
void free_file_contents(char *);
char * find_last_dir_separator(char *path);
#endif