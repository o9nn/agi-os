/**
 * Emotion JSON Parsing and Serialization Implementation
 */

#include "emotion_json.h"
#include "cJSON.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <stdio.h>

/* Parse emotion from JSON */
int emotion_from_json(const char* json, Emotion* emotion) {
    if (!json || !emotion) return -1;
    
    cJSON* root = cJSON_Parse(json);
    if (!root) {
        fprintf(stderr, "JSON parse error: %s\n", cJSON_GetErrorPtr());
        return -1;
    }
    
    /* Parse required fields */
    cJSON* joy = cJSON_GetObjectItem(root, "joy");
    cJSON* sadness = cJSON_GetObjectItem(root, "sadness");
    cJSON* anger = cJSON_GetObjectItem(root, "anger");
    cJSON* fear = cJSON_GetObjectItem(root, "fear");
    cJSON* surprise = cJSON_GetObjectItem(root, "surprise");
    cJSON* disgust = cJSON_GetObjectItem(root, "disgust");
    
    if (!joy || !sadness || !anger || !fear || !surprise || !disgust) {
        fprintf(stderr, "Missing required emotion fields\n");
        cJSON_Delete(root);
        return -1;
    }
    
    if (!cJSON_IsNumber(joy) || !cJSON_IsNumber(sadness) || 
        !cJSON_IsNumber(anger) || !cJSON_IsNumber(fear) ||
        !cJSON_IsNumber(surprise) || !cJSON_IsNumber(disgust)) {
        fprintf(stderr, "Emotion fields must be numbers\n");
        cJSON_Delete(root);
        return -1;
    }
    
    emotion->joy = joy->valuedouble;
    emotion->sadness = sadness->valuedouble;
    emotion->anger = anger->valuedouble;
    emotion->fear = fear->valuedouble;
    emotion->surprise = surprise->valuedouble;
    emotion->disgust = disgust->valuedouble;
    
    /* Optional fields */
    cJSON* timestamp = cJSON_GetObjectItem(root, "timestamp");
    if (timestamp && cJSON_IsString(timestamp)) {
        strncpy(emotion->timestamp, timestamp->valuestring, 
                sizeof(emotion->timestamp) - 1);
        emotion->timestamp[sizeof(emotion->timestamp) - 1] = '\0';
    } else {
        emotion_get_timestamp(emotion->timestamp, sizeof(emotion->timestamp));
    }
    
    /* Vorticity will be computed by caller */
    emotion->vorticity = 0.0;
    
    cJSON_Delete(root);
    
    /* Validate */
    if (!emotion_validate(emotion)) {
        fprintf(stderr, "Emotion validation failed\n");
        return -1;
    }
    
    return 0;
}

/* Serialize emotion to JSON */
char* emotion_to_json(const Emotion* emotion) {
    if (!emotion) return NULL;
    
    cJSON* root = cJSON_CreateObject();
    if (!root) return NULL;
    
    cJSON_AddNumberToObject(root, "joy", emotion->joy);
    cJSON_AddNumberToObject(root, "sadness", emotion->sadness);
    cJSON_AddNumberToObject(root, "anger", emotion->anger);
    cJSON_AddNumberToObject(root, "fear", emotion->fear);
    cJSON_AddNumberToObject(root, "surprise", emotion->surprise);
    cJSON_AddNumberToObject(root, "disgust", emotion->disgust);
    cJSON_AddNumberToObject(root, "vorticity", emotion->vorticity);
    cJSON_AddStringToObject(root, "timestamp", emotion->timestamp);
    
    char* json = cJSON_PrintUnformatted(root);
    cJSON_Delete(root);
    
    return json;
}

/* Serialize collective emotion to JSON */
char* collective_emotion_to_json(const CollectiveEmotion* collective) {
    if (!collective) return NULL;
    
    cJSON* root = cJSON_CreateObject();
    if (!root) return NULL;
    
    /* Average emotion */
    cJSON_AddNumberToObject(root, "joy", collective->emotion.joy);
    cJSON_AddNumberToObject(root, "sadness", collective->emotion.sadness);
    cJSON_AddNumberToObject(root, "anger", collective->emotion.anger);
    cJSON_AddNumberToObject(root, "fear", collective->emotion.fear);
    cJSON_AddNumberToObject(root, "surprise", collective->emotion.surprise);
    cJSON_AddNumberToObject(root, "disgust", collective->emotion.disgust);
    cJSON_AddNumberToObject(root, "vorticity", collective->emotion.vorticity);
    cJSON_AddStringToObject(root, "timestamp", collective->emotion.timestamp);
    
    /* Collective metrics */
    cJSON_AddNumberToObject(root, "n_avatars", collective->n_avatars);
    cJSON_AddNumberToObject(root, "coherence", collective->coherence);
    cJSON_AddNumberToObject(root, "circulation", collective->circulation);
    
    /* Individual avatars */
    cJSON* avatars = cJSON_CreateArray();
    for (int i = 0; i < collective->n_avatar_entries; i++) {
        cJSON* avatar = cJSON_CreateObject();
        cJSON_AddStringToObject(avatar, "name", collective->avatars[i].name);
        cJSON_AddNumberToObject(avatar, "vorticity", collective->avatars[i].vorticity);
        cJSON_AddItemToArray(avatars, avatar);
    }
    cJSON_AddItemToObject(root, "avatars", avatars);
    
    char* json = cJSON_PrintUnformatted(root);
    cJSON_Delete(root);
    
    return json;
}

/* Free JSON string */
void emotion_json_free(char* json) {
    if (json) {
        cJSON_free(json);
    }
}

/* Validate emotion */
bool emotion_validate(const Emotion* emotion) {
    if (!emotion) return false;
    
    /* Check all values in [0.0, 1.0] */
    if (emotion->joy < 0.0 || emotion->joy > 1.0) {
        fprintf(stderr, "Invalid joy: %.2f\n", emotion->joy);
        return false;
    }
    if (emotion->sadness < 0.0 || emotion->sadness > 1.0) {
        fprintf(stderr, "Invalid sadness: %.2f\n", emotion->sadness);
        return false;
    }
    if (emotion->anger < 0.0 || emotion->anger > 1.0) {
        fprintf(stderr, "Invalid anger: %.2f\n", emotion->anger);
        return false;
    }
    if (emotion->fear < 0.0 || emotion->fear > 1.0) {
        fprintf(stderr, "Invalid fear: %.2f\n", emotion->fear);
        return false;
    }
    if (emotion->surprise < 0.0 || emotion->surprise > 1.0) {
        fprintf(stderr, "Invalid surprise: %.2f\n", emotion->surprise);
        return false;
    }
    if (emotion->disgust < 0.0 || emotion->disgust > 1.0) {
        fprintf(stderr, "Invalid disgust: %.2f\n", emotion->disgust);
        return false;
    }
    
    /* Check for NaN */
    if (isnan(emotion->joy) || isnan(emotion->sadness) || 
        isnan(emotion->anger) || isnan(emotion->fear) ||
        isnan(emotion->surprise) || isnan(emotion->disgust)) {
        fprintf(stderr, "Emotion contains NaN values\n");
        return false;
    }
    
    return true;
}

/* Get current timestamp */
void emotion_get_timestamp(char* buf, size_t size) {
    if (!buf || size == 0) return;
    
    time_t now = time(NULL);
    struct tm* tm = gmtime(&now);
    
    if (tm) {
        strftime(buf, size, "%Y-%m-%dT%H:%M:%SZ", tm);
    } else {
        snprintf(buf, size, "1970-01-01T00:00:00Z");
    }
}
