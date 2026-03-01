/*
 * AI Angel Platform Layer
 *
 * The aiangel-platform ⊕ layer of the echo-angel composition.
 * Provides the additive platform features: real-time chat,
 * streaming output, and fan engagement hooks.
 *
 * This layer is ⊕ (additive) — it can be swapped or removed
 * without affecting the cognitive core.
 *
 * Copyright (C) 2026 OpenCog Community
 * Licensed under AGPL-3.0
 */

#include <stdio.h>
#include <string.h>
#include "../include/echo_angel.h"

/* ================================================================
 * Chat Message Queue
 * ================================================================ */

#define CHAT_QUEUE_SIZE  64
#define CHAT_MSG_MAXLEN  512

typedef struct ChatMessage {
    char    sender[64];
    char    content[CHAT_MSG_MAXLEN];
    int     processed;
    uint64_t timestamp;
} ChatMessage;

typedef struct ChatQueue {
    ChatMessage messages[CHAT_QUEUE_SIZE];
    int         write_pos;
    int         read_pos;
    int         count;
} ChatQueue;

static ChatQueue g_chat_queue = {.write_pos = 0, .read_pos = 0, .count = 0};

int
platform_chat_enqueue(const char *sender, const char *content)
{
    if (g_chat_queue.count >= CHAT_QUEUE_SIZE)
        return -1; /* Queue full */

    ChatMessage *msg = &g_chat_queue.messages[g_chat_queue.write_pos];
    strncpy(msg->sender, sender, sizeof(msg->sender) - 1);
    strncpy(msg->content, content, sizeof(msg->content) - 1);
    msg->processed = 0;
    msg->timestamp = 0; /* Would use real timestamp */

    g_chat_queue.write_pos = (g_chat_queue.write_pos + 1) % CHAT_QUEUE_SIZE;
    g_chat_queue.count++;
    return 0;
}

int
platform_chat_dequeue(char *sender, size_t sender_max,
                      char *content, size_t content_max)
{
    if (g_chat_queue.count <= 0)
        return -1; /* Queue empty */

    ChatMessage *msg = &g_chat_queue.messages[g_chat_queue.read_pos];
    if (sender) strncpy(sender, msg->sender, sender_max - 1);
    if (content) strncpy(content, msg->content, content_max - 1);
    msg->processed = 1;

    g_chat_queue.read_pos = (g_chat_queue.read_pos + 1) % CHAT_QUEUE_SIZE;
    g_chat_queue.count--;
    return 0;
}

/* ================================================================
 * Streaming Output
 * ================================================================ */

typedef struct StreamFrame {
    float morph_targets[68];  /* MetaHuman CTRL_ values */
    float audio_amplitude;
    int   frame_number;
} StreamFrame;

static StreamFrame g_current_frame = {.frame_number = 0};

int
platform_stream_update(const EchoFACSState *facs)
{
    if (facs == NULL) return -1;

    /* Map FACS to stream frame morph targets */
    for (int i = 0; i < ECHO_FACS_AU_COUNT && i < 46; i++)
        g_current_frame.morph_targets[i] = facs->final_au[i];

    g_current_frame.frame_number++;
    return 0;
}

int
platform_stream_get_frame(char *buf, size_t maxlen)
{
    if (buf == NULL) return -1;

    int n = 0;
    n += snprintf(buf + n, maxlen - n, "frame=%d\n", g_current_frame.frame_number);
    for (int i = 0; i < 46 && (size_t)n < maxlen - 30; i++) {
        if (g_current_frame.morph_targets[i] > 0.001f) {
            n += snprintf(buf + n, maxlen - n, "m%d=%.4f\n",
                          i, g_current_frame.morph_targets[i]);
        }
    }
    return n;
}

/* ================================================================
 * Fan Engagement Hooks
 * ================================================================ */

typedef struct EngagementEvent {
    char type[32];      /* "donation", "subscription", "reaction", "follow" */
    char user[64];
    float amount;
    char message[256];
} EngagementEvent;

#define ENGAGEMENT_QUEUE_SIZE 32

static EngagementEvent g_engagement_queue[ENGAGEMENT_QUEUE_SIZE];
static int g_engagement_write = 0;
static int g_engagement_count = 0;

int
platform_engagement_push(const char *type, const char *user,
                         float amount, const char *message)
{
    if (g_engagement_count >= ENGAGEMENT_QUEUE_SIZE)
        return -1;

    EngagementEvent *ev = &g_engagement_queue[g_engagement_write];
    strncpy(ev->type, type, sizeof(ev->type) - 1);
    strncpy(ev->user, user, sizeof(ev->user) - 1);
    ev->amount = amount;
    if (message)
        strncpy(ev->message, message, sizeof(ev->message) - 1);
    else
        ev->message[0] = '\0';

    g_engagement_write = (g_engagement_write + 1) % ENGAGEMENT_QUEUE_SIZE;
    g_engagement_count++;
    return 0;
}

/*
 * Convert engagement event to endocrine stimulus.
 * This is the ⊕ bridge: platform events additively contribute
 * to the cognitive core's endocrine state.
 */
int
platform_engagement_to_endocrine(EchoAngel *angel)
{
    if (angel == NULL || g_engagement_count <= 0)
        return -1;

    /* Process all pending events */
    while (g_engagement_count > 0) {
        int idx = (g_engagement_write - g_engagement_count + ENGAGEMENT_QUEUE_SIZE) % ENGAGEMENT_QUEUE_SIZE;
        EngagementEvent *ev = &g_engagement_queue[idx];

        if (strcmp(ev->type, "donation") == 0) {
            /* Donations boost oxytocin and dopamine */
            echo_angel_stimulate_gland(angel, GLAND_OXYTOCINERGIC, 0.3f);
            echo_angel_stimulate_gland(angel, GLAND_DOPAMINERGIC, 0.2f);
        } else if (strcmp(ev->type, "subscription") == 0) {
            /* Subscriptions boost serotonin */
            echo_angel_stimulate_gland(angel, GLAND_SEROTONERGIC, 0.2f);
        } else if (strcmp(ev->type, "reaction") == 0) {
            /* Reactions boost norepinephrine (arousal) */
            echo_angel_stimulate_gland(angel, GLAND_NORADRENERGIC, 0.1f);
        } else if (strcmp(ev->type, "follow") == 0) {
            /* Follows boost oxytocin */
            echo_angel_stimulate_gland(angel, GLAND_OXYTOCINERGIC, 0.1f);
        }

        g_engagement_count--;
    }

    return 0;
}

/* ================================================================
 * Platform 9P Interface
 * ================================================================ */

int
platform_9p_read(const char *path, char *buf, size_t maxlen)
{
    if (path == NULL || buf == NULL) return -1;

    if (strcmp(path, "chat/pending") == 0) {
        return snprintf(buf, maxlen, "%d\n", g_chat_queue.count);
    }

    if (strcmp(path, "stream/frame") == 0) {
        return platform_stream_get_frame(buf, maxlen);
    }

    if (strcmp(path, "engagement/pending") == 0) {
        return snprintf(buf, maxlen, "%d\n", g_engagement_count);
    }

    return snprintf(buf, maxlen, "error: unknown platform path '%s'\n", path);
}

int
platform_9p_write(EchoAngel *angel, const char *path, const char *data, size_t len)
{
    if (path == NULL || data == NULL) return -1;

    /* Write to chat queue */
    if (strcmp(path, "chat/send") == 0) {
        /* Format: "sender:message" */
        const char *sep = strchr(data, ':');
        if (sep == NULL) return -1;
        char sender[64] = {0};
        int slen = (int)(sep - data);
        if (slen > 63) slen = 63;
        memcpy(sender, data, slen);
        return platform_chat_enqueue(sender, sep + 1);
    }

    /* Process engagement events */
    if (strcmp(path, "engagement/process") == 0) {
        return platform_engagement_to_endocrine(angel);
    }

    return -1;
}
