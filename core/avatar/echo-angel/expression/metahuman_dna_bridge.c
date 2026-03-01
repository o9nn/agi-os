/*
 * MetaHuman DNA Bridge — Expression Pipeline
 *
 * The meta-echo-dna ⊗ layer of the echo-angel composition.
 * Maps FACS action units to MetaHuman DNA calibration morph targets,
 * applying chaotic dynamics and aesthetic biasing.
 *
 * Pipeline: Endocrine → FACS AUs → Chaos → Aesthetics → CTRL_ Morphs
 *
 * Copyright (C) 2026 OpenCog Community
 * Licensed under AGPL-3.0
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "../include/echo_angel.h"

/* ================================================================
 * MetaHuman CTRL_ Morph Target Definitions
 * ================================================================ */

#define MH_MORPH_COUNT  68

typedef struct MetaHumanMorphTarget {
    const char *name;
    int         source_au;    /* Primary FACS AU source */
    float       scale;        /* Scaling factor */
    float       offset;       /* Baseline offset */
} MetaHumanMorphTarget;

static const MetaHumanMorphTarget g_morph_targets[MH_MORPH_COUNT] = {
    /* Brow region */
    {"CTRL_brow_inner_L_up",      1,  0.8f, 0.0f},
    {"CTRL_brow_inner_R_up",      1,  0.8f, 0.0f},
    {"CTRL_brow_outer_L_up",      2,  0.7f, 0.0f},
    {"CTRL_brow_outer_R_up",      2,  0.7f, 0.0f},
    {"CTRL_brow_L_down",          4,  0.9f, 0.0f},
    {"CTRL_brow_R_down",          4,  0.9f, 0.0f},

    /* Eye region */
    {"CTRL_eye_L_wide",           5,  0.6f, 0.0f},
    {"CTRL_eye_R_wide",           5,  0.6f, 0.0f},
    {"CTRL_eye_L_squint",         7,  0.7f, 0.0f},
    {"CTRL_eye_R_squint",         7,  0.7f, 0.0f},
    {"CTRL_eye_L_blink",         43,  1.0f, 0.0f},
    {"CTRL_eye_R_blink",         43,  1.0f, 0.0f},

    /* Cheek region */
    {"CTRL_cheek_L_squint",       6,  0.8f, 0.0f},
    {"CTRL_cheek_R_squint",       6,  0.8f, 0.0f},
    {"CTRL_cheek_L_puff",        13,  0.5f, 0.0f},
    {"CTRL_cheek_R_puff",        13,  0.5f, 0.0f},

    /* Nose region */
    {"CTRL_nose_L_wrinkle",       9,  0.7f, 0.0f},
    {"CTRL_nose_R_wrinkle",       9,  0.7f, 0.0f},
    {"CTRL_nose_L_flare",        38,  0.4f, 0.0f},
    {"CTRL_nose_R_flare",        38,  0.4f, 0.0f},

    /* Mouth region — upper lip */
    {"CTRL_mouth_L_upperLipUp",  10,  0.8f, 0.0f},
    {"CTRL_mouth_R_upperLipUp",  10,  0.8f, 0.0f},

    /* Mouth region — smile/frown */
    {"CTRL_mouth_L_smile",       12,  1.0f, 0.0f},
    {"CTRL_mouth_R_smile",       12,  1.0f, 0.0f},
    {"CTRL_mouth_L_frown",       15,  0.9f, 0.0f},
    {"CTRL_mouth_R_frown",       15,  0.9f, 0.0f},

    /* Mouth region — other */
    {"CTRL_mouth_L_dimple",      14,  0.5f, 0.0f},
    {"CTRL_mouth_R_dimple",      14,  0.5f, 0.0f},
    {"CTRL_mouth_L_stretch",     20,  0.7f, 0.0f},
    {"CTRL_mouth_R_stretch",     20,  0.7f, 0.0f},
    {"CTRL_mouth_L_tighten",     23,  0.6f, 0.0f},
    {"CTRL_mouth_R_tighten",     23,  0.6f, 0.0f},
    {"CTRL_mouth_L_press",       24,  0.5f, 0.0f},
    {"CTRL_mouth_R_press",       24,  0.5f, 0.0f},
    {"CTRL_mouth_L_lipPart",     25,  0.7f, 0.0f},
    {"CTRL_mouth_R_lipPart",     25,  0.7f, 0.0f},

    /* Jaw */
    {"CTRL_jaw_open",            26,  0.8f, 0.0f},
    {"CTRL_jaw_L",               30,  0.4f, 0.0f},
    {"CTRL_jaw_R",               30,  0.4f, 0.0f},
    {"CTRL_jaw_fwd",             29,  0.3f, 0.0f},

    /* Chin */
    {"CTRL_chin_raise",          17,  0.6f, 0.0f},

    /* Lip roll */
    {"CTRL_mouth_lipRoll_upper", 28,  0.5f, 0.0f},
    {"CTRL_mouth_lipRoll_lower", 28,  0.5f, 0.0f},

    /* Tongue */
    {"CTRL_tongue_out",          19,  0.4f, 0.0f},

    /* Neck */
    {"CTRL_neck_L_tighten",      21,  0.3f, 0.0f},
    {"CTRL_neck_R_tighten",      21,  0.3f, 0.0f},

    /* Sentinel */
    {NULL, -1, 0.0f, 0.0f}
};

/* ================================================================
 * SuperHotGirl Aesthetic Parameters
 * ================================================================ */

typedef struct AestheticParams {
    float confidence_posture;   /* Subtle chin-up, relaxed brows */
    float charisma;             /* Enhanced smile warmth */
    float eye_sparkle;          /* Subtle eye widening + squint combo */
    float poise;                /* Reduced fidgeting/noise */
    float expressiveness;       /* Overall expression amplitude */
} AestheticParams;

static AestheticParams g_aesthetics = {
    .confidence_posture = 0.6f,
    .charisma = 0.7f,
    .eye_sparkle = 0.5f,
    .poise = 0.8f,
    .expressiveness = 0.9f
};

/* ================================================================
 * Pipeline Functions
 * ================================================================ */

/*
 * Apply aesthetic biasing to FACS action units.
 * This is the "SuperHotGirl" aesthetic layer that gives the avatar
 * its characteristic presence and style.
 */
void
metahuman_apply_aesthetics(EchoFACSState *facs, const AestheticParams *aes)
{
    /* Confidence: subtle chin raise, relaxed brows */
    facs->aesthetic_bias[17] = aes->confidence_posture * 0.15f; /* Chin raise */
    facs->aesthetic_bias[1] -= aes->confidence_posture * 0.05f; /* Relax inner brow */

    /* Charisma: warm smile bias */
    facs->aesthetic_bias[6] = aes->charisma * 0.1f;   /* Cheek raiser */
    facs->aesthetic_bias[12] = aes->charisma * 0.08f;  /* Smile */

    /* Eye sparkle: subtle widening + micro-squint */
    facs->aesthetic_bias[5] = aes->eye_sparkle * 0.1f;  /* Upper lid raise */
    facs->aesthetic_bias[7] = aes->eye_sparkle * 0.05f;  /* Slight squint */

    /* Poise: reduce chaos noise amplitude */
    float poise_damping = 1.0f - aes->poise * 0.5f;
    for (int i = 0; i < ECHO_FACS_AU_COUNT; i++)
        facs->chaos_noise[i] *= poise_damping;

    /* Expressiveness: scale all action units */
    for (int i = 0; i < ECHO_FACS_AU_COUNT; i++) {
        facs->final_au[i] = (facs->action_units[i] * aes->expressiveness +
                              facs->chaos_noise[i] +
                              facs->aesthetic_bias[i]);
        if (facs->final_au[i] < 0.0f) facs->final_au[i] = 0.0f;
        if (facs->final_au[i] > 1.0f) facs->final_au[i] = 1.0f;
    }
}

/*
 * Convert FACS action units to MetaHuman CTRL_ morph targets.
 * Returns the number of active morph targets written to the buffer.
 */
int
metahuman_facs_to_morphs(const EchoFACSState *facs, char *buf, size_t maxlen)
{
    int n = 0;
    int active = 0;

    for (int i = 0; g_morph_targets[i].name != NULL && (size_t)n < maxlen - 80; i++) {
        int au = g_morph_targets[i].source_au;
        if (au >= 0 && au < ECHO_FACS_AU_COUNT) {
            float value = facs->final_au[au] * g_morph_targets[i].scale +
                          g_morph_targets[i].offset;
            if (value < 0.0f) value = 0.0f;
            if (value > 1.0f) value = 1.0f;

            if (value > 0.001f) {
                n += snprintf(buf + n, maxlen - n, "%s=%.4f\n",
                              g_morph_targets[i].name, value);
                active++;
            }
        }
    }

    if (active == 0)
        n = snprintf(buf, maxlen, "# neutral expression\n");

    return n;
}

/*
 * Full expression pipeline: endocrine → FACS → aesthetics → morphs
 */
int
metahuman_full_pipeline(EchoAngel *angel, char *morph_buf, size_t maxlen)
{
    if (angel == NULL || morph_buf == NULL)
        return -1;

    /* Apply aesthetics to the FACS state */
    metahuman_apply_aesthetics(&angel->facs, &g_aesthetics);

    /* Convert to MetaHuman morph targets */
    return metahuman_facs_to_morphs(&angel->facs, morph_buf, maxlen);
}
