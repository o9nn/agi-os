#ifndef _EMOTION_JSON_H_
#define _EMOTION_JSON_H_
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
typedef struct {
double joy;
double sadness;
double anger;
double fear;
double surprise;
double disgust;
double vorticity;
char timestamp[32];
} Emotion;
typedef struct {
Emotion emotion;
int n_avatars;
double coherence;
double circulation;
struct {
char name[64];
double vorticity;
} *avatars;
int n_avatar_entries;
} CollectiveEmotion;
int emotion_from_json(const char* json, Emotion* emotion);
char* emotion_to_json(const Emotion* emotion);
char* collective_emotion_to_json(const CollectiveEmotion* collective);
void emotion_json_free(char* json);
bool emotion_validate(const Emotion* emotion);
void emotion_get_timestamp(char* buf, size_t size);
#endif