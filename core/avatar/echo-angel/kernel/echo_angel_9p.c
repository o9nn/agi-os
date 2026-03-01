/*
 * Echo Angel 9P Filesystem Interface
 *
 * Exposes the Echo Angel cognitive avatar as a 9P filesystem:
 *
 *   /angel/
 *   ├── ctl           # Control: write commands (init, step, cycle, introspect)
 *   ├── status        # Read current status
 *   ├── input         # Write perception input
 *   ├── response      # Read generated response
 *   ├── endocrine/
 *   │   ├── hormones  # Read all 16 hormone levels
 *   │   ├── valence   # Read valence (-1 to +1)
 *   │   ├── arousal   # Read arousal (0 to 1)
 *   │   └── stimulate # Write "gland_id amount" to stimulate
 *   ├── expression/
 *   │   ├── facs      # Read all 46 FACS action units
 *   │   └── morph     # Read MetaHuman morph targets
 *   ├── cognition/
 *   │   ├── 4e        # Read 4E cognition metrics
 *   │   ├── reservoir # Read ESN reservoir state summary
 *   │   └── step      # Read current Echobeats step
 *   ├── self/
 *   │   ├── image     # Read Autognosis self-image
 *   │   ├── wisdom    # Read wisdom index
 *   │   └── stage     # Read evolution stage
 *   └── stats         # Read comprehensive statistics
 *
 * Copyright (C) 2026 OpenCog Community
 * Licensed under AGPL-3.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/echo_angel.h"

/* ================================================================
 * 9P Path Routing
 * ================================================================ */

int
echo_angel_9p_init(EchoAngel *angel)
{
    if (angel == NULL)
        return -1;
    /* 9P filesystem is implicitly available once angel is initialized */
    return angel->initialized ? 0 : -1;
}

int
echo_angel_9p_read(EchoAngel *angel, const char *path, char *buf, size_t maxlen)
{
    if (angel == NULL || path == NULL || buf == NULL || !angel->initialized)
        return -1;

    /* /angel/status */
    if (strcmp(path, "status") == 0 || strcmp(path, "/angel/status") == 0) {
        return echo_angel_stats(angel, buf, maxlen);
    }

    /* /angel/response */
    if (strcmp(path, "response") == 0 || strcmp(path, "/angel/response") == 0) {
        return echo_angel_get_response(angel, buf, maxlen);
    }

    /* /angel/stats */
    if (strcmp(path, "stats") == 0 || strcmp(path, "/angel/stats") == 0) {
        return echo_angel_stats(angel, buf, maxlen);
    }

    /* /angel/endocrine/hormones */
    if (strcmp(path, "endocrine/hormones") == 0 ||
        strcmp(path, "/angel/endocrine/hormones") == 0) {
        int n = 0;
        const char *names[] = {
            "cortisol", "dopamine", "serotonin", "norepinephrine",
            "oxytocin", "thyroxine", "melatonin", "insulin",
            "cytokine", "anandamide", "adrenaline", "gaba",
            "glutamate", "endorphin", "testosterone", "estrogen"
        };
        for (int i = 0; i < ECHO_HORMONE_COUNT && (size_t)n < maxlen - 40; i++) {
            n += snprintf(buf + n, maxlen - n, "%s=%.4f\n",
                          names[i], angel->endocrine.hormones[i]);
        }
        return n;
    }

    /* /angel/endocrine/valence */
    if (strcmp(path, "endocrine/valence") == 0 ||
        strcmp(path, "/angel/endocrine/valence") == 0) {
        return snprintf(buf, maxlen, "%.4f\n", angel->endocrine.valence);
    }

    /* /angel/endocrine/arousal */
    if (strcmp(path, "endocrine/arousal") == 0 ||
        strcmp(path, "/angel/endocrine/arousal") == 0) {
        return snprintf(buf, maxlen, "%.4f\n", angel->endocrine.arousal);
    }

    /* /angel/expression/facs */
    if (strcmp(path, "expression/facs") == 0 ||
        strcmp(path, "/angel/expression/facs") == 0) {
        int n = 0;
        for (int i = 0; i < ECHO_FACS_AU_COUNT && (size_t)n < maxlen - 30; i++) {
            if (angel->facs.final_au[i] > 0.001f) {
                n += snprintf(buf + n, maxlen - n, "AU%d=%.4f\n",
                              i, angel->facs.final_au[i]);
            }
        }
        if (n == 0)
            n = snprintf(buf, maxlen, "neutral\n");
        return n;
    }

    /* /angel/expression/morph */
    if (strcmp(path, "expression/morph") == 0 ||
        strcmp(path, "/angel/expression/morph") == 0) {
        int n = 0;
        /* Map FACS AUs to MetaHuman CTRL_ morph targets */
        const struct { int au; const char *morph; } au_to_morph[] = {
            {1, "CTRL_brow_inner_up"},
            {2, "CTRL_brow_outer_up"},
            {4, "CTRL_brow_down"},
            {5, "CTRL_eye_wide"},
            {6, "CTRL_cheek_squint"},
            {7, "CTRL_eye_squint"},
            {9, "CTRL_nose_wrinkle"},
            {10, "CTRL_upper_lip_raise"},
            {12, "CTRL_smile"},
            {15, "CTRL_frown"},
            {17, "CTRL_chin_raise"},
            {20, "CTRL_lip_stretch"},
            {23, "CTRL_lip_tighten"},
            {25, "CTRL_lip_part"},
            {26, "CTRL_jaw_drop"},
            {-1, NULL}
        };
        for (int i = 0; au_to_morph[i].morph != NULL && (size_t)n < maxlen - 60; i++) {
            int au = au_to_morph[i].au;
            if (au >= 0 && au < ECHO_FACS_AU_COUNT && angel->facs.final_au[au] > 0.001f) {
                n += snprintf(buf + n, maxlen - n, "%s=%.4f\n",
                              au_to_morph[i].morph, angel->facs.final_au[au]);
            }
        }
        if (n == 0)
            n = snprintf(buf, maxlen, "neutral\n");
        return n;
    }

    /* /angel/cognition/4e */
    if (strcmp(path, "cognition/4e") == 0 ||
        strcmp(path, "/angel/cognition/4e") == 0) {
        return snprintf(buf, maxlen,
            "embodied=%.4f\nembedded=%.4f\nenacted=%.4f\nextended=%.4f\n",
            angel->cognition_4e.embodied, angel->cognition_4e.embedded,
            angel->cognition_4e.enacted, angel->cognition_4e.extended);
    }

    /* /angel/cognition/step */
    if (strcmp(path, "cognition/step") == 0 ||
        strcmp(path, "/angel/cognition/step") == 0) {
        const char *step_names[] = {
            "sense", "attend", "remember", "predict", "compare",
            "learn", "decide", "act", "reflect"
        };
        return snprintf(buf, maxlen, "%s (%d/%d)\n",
            step_names[angel->current_step], angel->current_step, ECHO_STEP_COUNT);
    }

    /* /angel/cognition/reservoir */
    if (strcmp(path, "cognition/reservoir") == 0 ||
        strcmp(path, "/angel/cognition/reservoir") == 0) {
        /* Compute reservoir energy (L2 norm of state) */
        float energy = 0.0f;
        for (int i = 0; i < ECHO_RESERVOIR_SIZE; i++)
            energy += angel->reservoir.state[i] * angel->reservoir.state[i];
        float mean = 0.0f;
        for (int i = 0; i < ECHO_RESERVOIR_SIZE; i++)
            mean += angel->reservoir.state[i];
        mean /= ECHO_RESERVOIR_SIZE;
        return snprintf(buf, maxlen,
            "size=%d\nspectral_radius=%.3f\nleak_rate=%.3f\n"
            "energy=%.4f\nmean_activation=%.6f\n",
            ECHO_RESERVOIR_SIZE, angel->reservoir.spectral_radius,
            angel->reservoir.leak_rate, energy, mean);
    }

    /* /angel/self/image */
    if (strcmp(path, "self/image") == 0 ||
        strcmp(path, "/angel/self/image") == 0) {
        return snprintf(buf, maxlen,
            "identity=%.4f\nclarity=%.4f\nawareness=%.4f\n"
            "moral=%.4f\nwisdom=%.4f\nlevel=%d\n",
            angel->self_image.identity_strength,
            angel->self_image.cognitive_clarity,
            angel->self_image.emotional_awareness,
            angel->self_image.moral_intuition,
            angel->self_image.wisdom_index,
            angel->self_image.level);
    }

    /* /angel/self/wisdom */
    if (strcmp(path, "self/wisdom") == 0 ||
        strcmp(path, "/angel/self/wisdom") == 0) {
        return snprintf(buf, maxlen, "%.6f\n", angel->self_image.wisdom_index);
    }

    /* /angel/self/stage */
    if (strcmp(path, "self/stage") == 0 ||
        strcmp(path, "/angel/self/stage") == 0) {
        const char *stage_names[] = {"nascent", "learning", "adapting", "maturing", "wise"};
        return snprintf(buf, maxlen, "%s\nmaturity=%.4f\nxp=%.0f\ninsights=%d\n",
            stage_names[angel->evolution.stage],
            angel->evolution.maturity,
            angel->evolution.experience_points,
            angel->evolution.insights_generated);
    }

    return snprintf(buf, maxlen, "error: unknown path '%s'\n", path);
}

int
echo_angel_9p_write(EchoAngel *angel, const char *path, const char *data, size_t len)
{
    if (angel == NULL || path == NULL || data == NULL)
        return -1;

    /* /angel/ctl */
    if (strcmp(path, "ctl") == 0 || strcmp(path, "/angel/ctl") == 0) {
        if (strncmp(data, "step", 4) == 0)
            return echo_angel_step(angel);
        if (strncmp(data, "cycle", 5) == 0)
            return echo_angel_full_cycle(angel);
        if (strncmp(data, "introspect", 10) == 0)
            return echo_angel_introspect(angel);
        if (strncmp(data, "evolve", 6) == 0)
            return echo_angel_evolve(angel);
        if (strncmp(data, "init ", 5) == 0)
            return echo_angel_init(angel, data + 5);
        return -1; /* Unknown command */
    }

    /* /angel/input */
    if (strcmp(path, "input") == 0 || strcmp(path, "/angel/input") == 0) {
        return echo_angel_perceive(angel, data, len);
    }

    /* /angel/endocrine/stimulate */
    if (strcmp(path, "endocrine/stimulate") == 0 ||
        strcmp(path, "/angel/endocrine/stimulate") == 0) {
        int gland_id;
        float amount;
        if (sscanf(data, "%d %f", &gland_id, &amount) == 2) {
            return echo_angel_stimulate_gland(angel, (EchoGland)gland_id, amount);
        }
        return -1;
    }

    return -1; /* Unknown write path */
}
