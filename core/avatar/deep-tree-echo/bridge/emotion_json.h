/**
 * Emotion JSON Parsing and Serialization
 * 
 * Provides JSON support for emotional state representation in the
 * Deep Tree Echo 9P file server.
 */

#ifndef _EMOTION_JSON_H_
#define _EMOTION_JSON_H_

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* Emotion structure */
typedef struct {
    double joy;
    double sadness;
    double anger;
    double fear;
    double surprise;
    double disgust;
    double vorticity;        /* computed */
    char timestamp[32];      /* ISO8601 */
} Emotion;

/* Collective emotion structure */
typedef struct {
    Emotion emotion;         /* average emotion */
    int n_avatars;
    double coherence;
    double circulation;
    
    /* Individual avatars */
    struct {
        char name[64];
        double vorticity;
    } *avatars;
    int n_avatar_entries;
} CollectiveEmotion;

/**
 * Parse emotion from JSON string
 * 
 * json: JSON string
 * emotion: Output emotion structure
 * 
 * Returns: 0 on success, -1 on error
 */
int emotion_from_json(const char* json, Emotion* emotion);

/**
 * Serialize emotion to JSON string
 * 
 * emotion: Input emotion structure
 * 
 * Returns: JSON string (caller must free), or NULL on error
 */
char* emotion_to_json(const Emotion* emotion);

/**
 * Serialize collective emotion to JSON string
 * 
 * collective: Input collective emotion structure
 * 
 * Returns: JSON string (caller must free), or NULL on error
 */
char* collective_emotion_to_json(const CollectiveEmotion* collective);

/**
 * Free JSON string
 */
void emotion_json_free(char* json);

/**
 * Validate emotion values (all in [0.0, 1.0])
 * 
 * Returns: true if valid, false otherwise
 */
bool emotion_validate(const Emotion* emotion);

/**
 * Get current ISO8601 timestamp
 */
void emotion_get_timestamp(char* buf, size_t size);

#endif /* _EMOTION_JSON_H_ */
