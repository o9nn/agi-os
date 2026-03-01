/*
 * Autognosis Introspection Engine
 *
 * The echo-introspect ⊗ layer of the echo-angel composition.
 * Implements hierarchical self-image building, endocrine history
 * analysis, CogMorph glyph visualization, and moral perception.
 *
 * Copyright (C) 2026 OpenCog Community
 * Licensed under AGPL-3.0
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "../include/echo_angel.h"

/* ================================================================
 * Endocrine History Ring Buffer
 * ================================================================ */

#define ENDO_HISTORY_SIZE  256

typedef struct EndocrineHistory {
    EchoEndocrineState snapshots[ENDO_HISTORY_SIZE];
    int                write_pos;
    int                count;
} EndocrineHistory;

static EndocrineHistory g_endo_history = {.write_pos = 0, .count = 0};

void
autognosis_record_endocrine(const EchoEndocrineState *state)
{
    if (state == NULL) return;
    memcpy(&g_endo_history.snapshots[g_endo_history.write_pos],
           state, sizeof(EchoEndocrineState));
    g_endo_history.write_pos = (g_endo_history.write_pos + 1) % ENDO_HISTORY_SIZE;
    if (g_endo_history.count < ENDO_HISTORY_SIZE)
        g_endo_history.count++;
}

/* ================================================================
 * Endocrine Pattern Analysis
 * ================================================================ */

typedef struct EndocrinePattern {
    float mean_valence;
    float valence_variance;
    float mean_arousal;
    float arousal_variance;
    float dominant_hormone;
    int   dominant_hormone_id;
    float emotional_stability;
    float mood_trend;  /* Positive = improving, negative = declining */
} EndocrinePattern;

static EndocrinePattern
autognosis_analyze_endocrine(void)
{
    EndocrinePattern pat;
    memset(&pat, 0, sizeof(pat));

    if (g_endo_history.count == 0)
        return pat;

    /* Compute mean valence and arousal */
    float sum_v = 0, sum_a = 0;
    for (int i = 0; i < g_endo_history.count; i++) {
        sum_v += g_endo_history.snapshots[i].valence;
        sum_a += g_endo_history.snapshots[i].arousal;
    }
    pat.mean_valence = sum_v / g_endo_history.count;
    pat.mean_arousal = sum_a / g_endo_history.count;

    /* Compute variance */
    float var_v = 0, var_a = 0;
    for (int i = 0; i < g_endo_history.count; i++) {
        float dv = g_endo_history.snapshots[i].valence - pat.mean_valence;
        float da = g_endo_history.snapshots[i].arousal - pat.mean_arousal;
        var_v += dv * dv;
        var_a += da * da;
    }
    pat.valence_variance = var_v / g_endo_history.count;
    pat.arousal_variance = var_a / g_endo_history.count;

    /* Find dominant hormone */
    float max_h = 0;
    int max_id = 0;
    int latest = (g_endo_history.write_pos - 1 + ENDO_HISTORY_SIZE) % ENDO_HISTORY_SIZE;
    for (int i = 0; i < ECHO_HORMONE_COUNT; i++) {
        if (g_endo_history.snapshots[latest].hormones[i] > max_h) {
            max_h = g_endo_history.snapshots[latest].hormones[i];
            max_id = i;
        }
    }
    pat.dominant_hormone = max_h;
    pat.dominant_hormone_id = max_id;

    /* Emotional stability = inverse of valence variance */
    pat.emotional_stability = 1.0f / (1.0f + pat.valence_variance * 10.0f);

    /* Mood trend: compare recent vs older valence */
    if (g_endo_history.count >= 10) {
        float recent = 0, older = 0;
        int half = g_endo_history.count / 2;
        for (int i = 0; i < half; i++) {
            int idx = (g_endo_history.write_pos - 1 - i + ENDO_HISTORY_SIZE) % ENDO_HISTORY_SIZE;
            recent += g_endo_history.snapshots[idx].valence;
        }
        for (int i = half; i < g_endo_history.count; i++) {
            int idx = (g_endo_history.write_pos - 1 - i + ENDO_HISTORY_SIZE) % ENDO_HISTORY_SIZE;
            older += g_endo_history.snapshots[idx].valence;
        }
        recent /= half;
        older /= (g_endo_history.count - half);
        pat.mood_trend = recent - older;
    }

    return pat;
}

/* ================================================================
 * Moral Perception Engine
 * ================================================================ */

typedef struct MoralIntuition {
    float beneficence;     /* Tendency toward helping */
    float non_maleficence; /* Tendency to avoid harm */
    float autonomy;        /* Respect for others' agency */
    float justice;         /* Fairness and equity */
    float overall;         /* Composite moral sense */
} MoralIntuition;

static MoralIntuition
autognosis_moral_perception(const EchoAngel *angel)
{
    MoralIntuition moral;
    memset(&moral, 0, sizeof(moral));

    if (angel == NULL) return moral;

    /* Moral intuitions emerge from endocrine state and experience */
    moral.beneficence = angel->endocrine.hormones[HORMONE_OXYTOCIN] * 0.6f +
                        angel->self_image.emotional_awareness * 0.4f;

    moral.non_maleficence = (1.0f - angel->endocrine.hormones[HORMONE_CORTISOL]) * 0.5f +
                            angel->self_image.moral_intuition * 0.5f;

    moral.autonomy = angel->cognition_4e.enacted * 0.4f +
                     angel->self_image.identity_strength * 0.6f;

    moral.justice = angel->self_image.cognitive_clarity * 0.5f +
                    angel->self_image.wisdom_index * 0.5f;

    moral.overall = (moral.beneficence + moral.non_maleficence +
                     moral.autonomy + moral.justice) / 4.0f;

    return moral;
}

/* ================================================================
 * CogMorph Glyph Visualization
 * ================================================================ */

/*
 * Generate a text-based glyph representation of the cognitive state.
 * This is a simplified version of the CogMorph visualization.
 */
static int
autognosis_cogmorph_glyph(const EchoAngel *angel, char *buf, size_t maxlen)
{
    if (angel == NULL || buf == NULL) return -1;

    /* Map cognitive state to a glyph pattern */
    char valence_char = angel->endocrine.valence > 0.3f ? '+' :
                        angel->endocrine.valence < -0.3f ? '-' : '~';
    char arousal_char = angel->endocrine.arousal > 0.6f ? '!' :
                        angel->endocrine.arousal > 0.3f ? '*' : '.';
    char wisdom_char = angel->self_image.wisdom_index > 0.6f ? 'W' :
                       angel->self_image.wisdom_index > 0.3f ? 'w' : '_';

    const char *stage_glyphs[] = {"[....]", "[*...]", "[**. ]", "[***.]", "[****]"};
    int stage = angel->evolution.stage;
    if (stage < 0) stage = 0;
    if (stage > 4) stage = 4;

    return snprintf(buf, maxlen,
        "┌─────────────────────┐\n"
        "│  Echo Angel Glyph   │\n"
        "├─────────────────────┤\n"
        "│ Valence: %c  Arousal: %c │\n"
        "│ Wisdom:  %c  Stage: %s│\n"
        "│ Cycle: %8lu      │\n"
        "│ Step: %d/9           │\n"
        "└─────────────────────┘\n",
        valence_char, arousal_char,
        wisdom_char, stage_glyphs[stage],
        (unsigned long)angel->cycle_count,
        angel->current_step);
}

/* Forward declaration */
static void self_image_update_from_introspection(EchoAngel *angel,
                                     const EndocrinePattern *pat,
                                     const MoralIntuition *moral);

/* ================================================================
 * Full Introspection Session
 * ================================================================ */

int
autognosis_full_introspection(EchoAngel *angel, char *report, size_t maxlen)
{
    if (angel == NULL || report == NULL) return -1;

    int n = 0;

    /* Record current endocrine state */
    autognosis_record_endocrine(&angel->endocrine);

    /* Analyze endocrine patterns */
    EndocrinePattern pat = autognosis_analyze_endocrine();

    /* Moral perception */
    MoralIntuition moral = autognosis_moral_perception(angel);

    /* Generate report */
    n += snprintf(report + n, maxlen - n,
        "=== Autognosis Introspection Report ===\n"
        "Angel: %s (cycle %lu)\n\n",
        angel->name, (unsigned long)angel->cycle_count);

    n += snprintf(report + n, maxlen - n,
        "--- Endocrine Analysis ---\n"
        "Mean valence: %.4f (var=%.4f)\n"
        "Mean arousal: %.4f (var=%.4f)\n"
        "Emotional stability: %.4f\n"
        "Mood trend: %s (%.4f)\n"
        "Dominant hormone: #%d (level=%.4f)\n\n",
        pat.mean_valence, pat.valence_variance,
        pat.mean_arousal, pat.arousal_variance,
        pat.emotional_stability,
        pat.mood_trend > 0.01f ? "improving" :
        pat.mood_trend < -0.01f ? "declining" : "stable",
        pat.mood_trend,
        pat.dominant_hormone_id, pat.dominant_hormone);

    n += snprintf(report + n, maxlen - n,
        "--- Moral Perception ---\n"
        "Beneficence: %.4f\n"
        "Non-maleficence: %.4f\n"
        "Autonomy: %.4f\n"
        "Justice: %.4f\n"
        "Overall moral sense: %.4f\n\n",
        moral.beneficence, moral.non_maleficence,
        moral.autonomy, moral.justice, moral.overall);

    n += snprintf(report + n, maxlen - n,
        "--- Self-Image ---\n"
        "Identity: %.4f\n"
        "Clarity: %.4f\n"
        "Emotional awareness: %.4f\n"
        "Moral intuition: %.4f\n"
        "Wisdom: %.4f (level %d)\n\n",
        angel->self_image.identity_strength,
        angel->self_image.cognitive_clarity,
        angel->self_image.emotional_awareness,
        angel->self_image.moral_intuition,
        angel->self_image.wisdom_index,
        angel->self_image.level);

    /* CogMorph glyph */
    n += snprintf(report + n, maxlen - n, "--- CogMorph Glyph ---\n");
    n += autognosis_cogmorph_glyph(angel, report + n, maxlen - n);

    /* Update self-image based on introspection */
    self_image_update_from_introspection(angel, &pat, &moral);

    return n;
}

/*
 * Update self-image based on introspection findings.
 * This is the feedback loop: introspection -> self-model -> behavior.
 */
static void
self_image_update_from_introspection(EchoAngel *angel,
                                     const EndocrinePattern *pat,
                                     const MoralIntuition *moral)
{
    if (angel == NULL) return;

    /* Emotional stability improves emotional awareness */
    angel->self_image.emotional_awareness +=
        (pat->emotional_stability - angel->self_image.emotional_awareness) * 0.02f;

    /* Moral perception improves moral intuition */
    angel->self_image.moral_intuition +=
        (moral->overall - angel->self_image.moral_intuition) * 0.01f;

    /* Positive mood trend improves identity strength */
    if (pat->mood_trend > 0.0f)
        angel->self_image.identity_strength += pat->mood_trend * 0.01f;

    /* Increment insights counter */
    angel->evolution.insights_generated++;
}
