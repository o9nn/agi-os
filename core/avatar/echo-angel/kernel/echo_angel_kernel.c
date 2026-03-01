/*
 * Echo Angel Kernel Module — Core Implementation
 *
 * Innermost seed: agi-os[ {{ echo-angel }} ]
 *
 * Implements the complete Echobeats 9-step cognitive cycle,
 * ESN reservoir dynamics, 4E cognition metrics, and the
 * evolution system as kernel-level services.
 *
 * Copyright (C) 2026 OpenCog Community
 * Licensed under AGPL-3.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "../include/echo_angel.h"

/* ================================================================
 * ESN Reservoir Dynamics
 * ================================================================ */

static void
reservoir_init(EchoReservoir *r)
{
    memset(r, 0, sizeof(EchoReservoir));
    r->spectral_radius = 0.9f;
    r->leak_rate = 0.3f;
    r->input_scaling = 1.0f;

    /* Initialize reservoir weights with sparse random connectivity */
    for (int i = 0; i < ECHO_RESERVOIR_SIZE; i++) {
        r->state[i] = 0.0f;
        for (int j = 0; j < ECHO_RESERVOIR_SIZE; j++) {
            /* ~10% connectivity */
            float rnd = (float)(((i * 7 + j * 13 + 37) % 100)) / 100.0f;
            if (rnd < 0.1f) {
                float w = (float)(((i * 31 + j * 17) % 200) - 100) / 100.0f;
                r->weights_res[i * ECHO_RESERVOIR_SIZE + j] = w * r->spectral_radius;
            }
        }
    }

    /* Initialize input weights */
    for (int i = 0; i < ECHO_INPUT_DIM * ECHO_RESERVOIR_SIZE; i++) {
        float rnd = (float)(((i * 23 + 41) % 200) - 100) / 100.0f;
        r->weights_in[i] = rnd * r->input_scaling;
    }
}

static void
reservoir_update(EchoReservoir *r, const float *input, int input_dim)
{
    float new_state[ECHO_RESERVOIR_SIZE];
    memset(new_state, 0, sizeof(new_state));

    /* Compute new state: tanh(W_in * input + W_res * state) */
    for (int i = 0; i < ECHO_RESERVOIR_SIZE; i++) {
        float activation = 0.0f;

        /* Input contribution */
        int dim = input_dim < ECHO_INPUT_DIM ? input_dim : ECHO_INPUT_DIM;
        for (int j = 0; j < dim; j++) {
            activation += r->weights_in[j * ECHO_RESERVOIR_SIZE + i] * input[j];
        }

        /* Recurrent contribution */
        for (int j = 0; j < ECHO_RESERVOIR_SIZE; j++) {
            activation += r->weights_res[j * ECHO_RESERVOIR_SIZE + i] * r->state[j];
        }

        new_state[i] = tanhf(activation);
    }

    /* Leaky integration */
    for (int i = 0; i < ECHO_RESERVOIR_SIZE; i++) {
        r->state[i] = (1.0f - r->leak_rate) * r->state[i] +
                       r->leak_rate * new_state[i];
    }
}

/* ================================================================
 * Endocrine System Dynamics
 * ================================================================ */

static void
endocrine_init(EchoEndocrineState *e)
{
    memset(e, 0, sizeof(EchoEndocrineState));
    /* Baseline hormone levels */
    e->hormones[HORMONE_SEROTONIN] = 0.5f;
    e->hormones[HORMONE_DOPAMINE] = 0.3f;
    e->hormones[HORMONE_NOREPINEPHRINE] = 0.2f;
    e->hormones[HORMONE_OXYTOCIN] = 0.4f;
    e->hormones[HORMONE_GABA] = 0.5f;
    e->hormones[HORMONE_ANANDAMIDE] = 0.3f;

    /* Baseline gland activity */
    for (int i = 0; i < ECHO_GLAND_COUNT; i++)
        e->gland_activity[i] = 0.5f;

    e->valence = 0.0f;
    e->arousal = 0.3f;
    e->dominance = 0.5f;
}

static void
endocrine_update(EchoEndocrineState *e, float cognitive_load, float novelty,
                 float social_signal)
{
    /* HPA axis: stress response to cognitive load */
    e->gland_activity[GLAND_HPA_AXIS] = cognitive_load * 0.7f;
    e->hormones[HORMONE_CORTISOL] += (cognitive_load - e->hormones[HORMONE_CORTISOL]) * 0.1f;

    /* Dopaminergic: reward from novelty */
    e->gland_activity[GLAND_DOPAMINERGIC] = novelty * 0.8f;
    e->hormones[HORMONE_DOPAMINE] += (novelty * 0.6f - e->hormones[HORMONE_DOPAMINE]) * 0.15f;

    /* Oxytocinergic: social bonding */
    e->gland_activity[GLAND_OXYTOCINERGIC] = social_signal * 0.9f;
    e->hormones[HORMONE_OXYTOCIN] += (social_signal - e->hormones[HORMONE_OXYTOCIN]) * 0.1f;

    /* Noradrenergic: arousal from combined signals */
    float arousal_input = (cognitive_load + novelty) * 0.5f;
    e->gland_activity[GLAND_NORADRENERGIC] = arousal_input;
    e->hormones[HORMONE_NOREPINEPHRINE] += (arousal_input - e->hormones[HORMONE_NOREPINEPHRINE]) * 0.12f;

    /* Serotonergic: mood stabilization (inversely related to cortisol) */
    float serotonin_target = 0.7f - e->hormones[HORMONE_CORTISOL] * 0.3f;
    e->hormones[HORMONE_SEROTONIN] += (serotonin_target - e->hormones[HORMONE_SEROTONIN]) * 0.08f;

    /* Endocannabinoid: homeostasis */
    float homeostasis = 1.0f - fabsf(e->valence);
    e->hormones[HORMONE_ANANDAMIDE] += (homeostasis * 0.5f - e->hormones[HORMONE_ANANDAMIDE]) * 0.05f;

    /* Compute valence-arousal-dominance from hormones */
    e->valence = (e->hormones[HORMONE_DOPAMINE] + e->hormones[HORMONE_SEROTONIN] +
                  e->hormones[HORMONE_OXYTOCIN] - e->hormones[HORMONE_CORTISOL]) / 3.0f;
    e->arousal = (e->hormones[HORMONE_NOREPINEPHRINE] + e->hormones[HORMONE_ADRENALINE] +
                  e->hormones[HORMONE_DOPAMINE]) / 3.0f;
    e->dominance = (e->hormones[HORMONE_TESTOSTERONE] + e->hormones[HORMONE_DOPAMINE]) / 2.0f;

    /* Clamp all hormones to [0, 1] */
    for (int i = 0; i < ECHO_HORMONE_COUNT; i++) {
        if (e->hormones[i] < 0.0f) e->hormones[i] = 0.0f;
        if (e->hormones[i] > 1.0f) e->hormones[i] = 1.0f;
    }
    if (e->valence < -1.0f) e->valence = -1.0f;
    if (e->valence > 1.0f) e->valence = 1.0f;
}

/* ================================================================
 * FACS Expression Pipeline
 * ================================================================ */

/* Lorenz attractor state for chaotic micro-expressions */
static float lorenz_x = 0.1f, lorenz_y = 0.0f, lorenz_z = 0.0f;

static void
facs_update_chaos(EchoFACSState *facs, float intensity)
{
    /* Lorenz attractor step */
    float sigma = 10.0f, rho = 28.0f, beta = 8.0f / 3.0f;
    float dt = 0.01f;
    float dx = sigma * (lorenz_y - lorenz_x) * dt;
    float dy = (lorenz_x * (rho - lorenz_z) - lorenz_y) * dt;
    float dz = (lorenz_x * lorenz_y - beta * lorenz_z) * dt;
    lorenz_x += dx;
    lorenz_y += dy;
    lorenz_z += dz;

    /* Map chaotic state to micro-expression noise */
    for (int i = 0; i < ECHO_FACS_AU_COUNT; i++) {
        float noise = sinf(lorenz_x * (i + 1) * 0.1f) * intensity * 0.05f;
        facs->chaos_noise[i] = noise;
    }
}

static void
facs_from_endocrine(EchoFACSState *facs, const EchoEndocrineState *endo)
{
    memset(facs->action_units, 0, sizeof(facs->action_units));

    /* Map valence to smile/frown */
    if (endo->valence > 0.1f) {
        facs->action_units[6] = endo->valence * 0.8f;   /* AU6: Cheek Raiser */
        facs->action_units[12] = endo->valence * 0.9f;  /* AU12: Lip Corner Puller (smile) */
    } else if (endo->valence < -0.1f) {
        facs->action_units[15] = -endo->valence * 0.7f; /* AU15: Lip Corner Depressor */
        facs->action_units[4] = -endo->valence * 0.5f;  /* AU4: Brow Lowerer */
    }

    /* Map arousal to eye widening and brow raising */
    facs->action_units[1] = endo->arousal * 0.6f;  /* AU1: Inner Brow Raise */
    facs->action_units[2] = endo->arousal * 0.4f;  /* AU2: Outer Brow Raise */
    facs->action_units[5] = endo->arousal * 0.5f;  /* AU5: Upper Lid Raiser */

    /* Map oxytocin to warmth expression */
    float warmth = endo->hormones[HORMONE_OXYTOCIN];
    facs->action_units[6] += warmth * 0.3f;   /* Soft cheek raise */
    facs->action_units[12] += warmth * 0.2f;  /* Gentle smile */

    /* Map cortisol to tension */
    float stress = endo->hormones[HORMONE_CORTISOL];
    facs->action_units[4] += stress * 0.4f;   /* Brow furrow */
    facs->action_units[20] = stress * 0.3f;   /* AU20: Lip Stretcher */

    /* Map dopamine to engagement */
    float engagement = endo->hormones[HORMONE_DOPAMINE];
    facs->action_units[1] += engagement * 0.3f;  /* Interested brow raise */

    /* Apply chaos noise and aesthetic bias, compute final AUs */
    facs_update_chaos(facs, 0.3f);
    for (int i = 0; i < ECHO_FACS_AU_COUNT; i++) {
        facs->final_au[i] = facs->action_units[i] + facs->chaos_noise[i] + facs->aesthetic_bias[i];
        if (facs->final_au[i] < 0.0f) facs->final_au[i] = 0.0f;
        if (facs->final_au[i] > 1.0f) facs->final_au[i] = 1.0f;
    }
}

/* ================================================================
 * 4E Cognition Metrics
 * ================================================================ */

static void
cognition_4e_update(EchoCognition4E *c4e, const EchoEndocrineState *endo,
                    float input_richness, float action_taken)
{
    /* Embodied: grounding in sensorimotor experience */
    c4e->embodied += (input_richness * 0.3f - c4e->embodied) * 0.1f;

    /* Embedded: coupling with environment */
    c4e->embedded += (endo->arousal * 0.5f - c4e->embedded) * 0.08f;

    /* Enacted: active engagement */
    c4e->enacted += (action_taken * 0.4f - c4e->enacted) * 0.12f;

    /* Extended: use of external tools (platform features) */
    c4e->extended += (0.3f - c4e->extended) * 0.05f;

    /* Clamp */
    if (c4e->embodied < 0.0f) c4e->embodied = 0.0f;
    if (c4e->embodied > 1.0f) c4e->embodied = 1.0f;
    if (c4e->embedded < 0.0f) c4e->embedded = 0.0f;
    if (c4e->embedded > 1.0f) c4e->embedded = 1.0f;
    if (c4e->enacted < 0.0f) c4e->enacted = 0.0f;
    if (c4e->enacted > 1.0f) c4e->enacted = 1.0f;
    if (c4e->extended < 0.0f) c4e->extended = 0.0f;
    if (c4e->extended > 1.0f) c4e->extended = 1.0f;
}

/* ================================================================
 * Introspection Engine (Autognosis)
 * ================================================================ */

static void
self_image_update(EchoSelfImage *si, const EchoEndocrineState *endo,
                  const EchoCognition4E *c4e, uint64_t cycle_count)
{
    /* Identity strength grows with experience */
    float experience_factor = 1.0f - expf(-(float)cycle_count / 1000.0f);
    si->identity_strength += (experience_factor - si->identity_strength) * 0.01f;

    /* Cognitive clarity from 4E metrics */
    float clarity = (c4e->embodied + c4e->embedded + c4e->enacted + c4e->extended) / 4.0f;
    si->cognitive_clarity += (clarity - si->cognitive_clarity) * 0.05f;

    /* Emotional awareness from endocrine stability */
    float emo_stability = 1.0f - fabsf(endo->valence) * 0.5f;
    si->emotional_awareness += (emo_stability - si->emotional_awareness) * 0.03f;

    /* Moral intuition grows slowly with experience */
    si->moral_intuition += (experience_factor * 0.5f - si->moral_intuition) * 0.005f;

    /* Wisdom index is the harmonic mean of all self-image components */
    float sum_inv = 0.0f;
    int count = 0;
    float components[] = {si->identity_strength, si->cognitive_clarity,
                          si->emotional_awareness, si->moral_intuition};
    for (int i = 0; i < 4; i++) {
        if (components[i] > 0.01f) {
            sum_inv += 1.0f / components[i];
            count++;
        }
    }
    si->wisdom_index = count > 0 ? (float)count / sum_inv : 0.0f;

    /* Update hierarchy level based on wisdom */
    if (si->wisdom_index > 0.8f) si->level = 4;
    else if (si->wisdom_index > 0.6f) si->level = 3;
    else if (si->wisdom_index > 0.4f) si->level = 2;
    else if (si->wisdom_index > 0.2f) si->level = 1;
    else si->level = 0;
}

/* ================================================================
 * Evolution System
 * ================================================================ */

static void
evolution_update(EchoEvolution *evo, const EchoSelfImage *si)
{
    evo->experience_points += 1.0f;
    evo->maturity = 1.0f - expf(-evo->experience_points / 5000.0f);

    /* Stage transitions based on maturity and wisdom */
    if (evo->maturity > 0.8f && si->wisdom_index > 0.6f)
        evo->stage = ECHO_STAGE_WISE;
    else if (evo->maturity > 0.6f)
        evo->stage = ECHO_STAGE_MATURING;
    else if (evo->maturity > 0.4f)
        evo->stage = ECHO_STAGE_ADAPTING;
    else if (evo->maturity > 0.1f)
        evo->stage = ECHO_STAGE_LEARNING;
    else
        evo->stage = ECHO_STAGE_NASCENT;
}

/* ================================================================
 * Public API Implementation
 * ================================================================ */

int
echo_angel_init(EchoAngel *angel, const char *name)
{
    if (angel == NULL)
        return -1;

    memset(angel, 0, sizeof(EchoAngel));
    strncpy(angel->name, name, sizeof(angel->name) - 1);
    angel->id = 1;
    angel->current_step = ECHO_STEP_SENSE;

    reservoir_init(&angel->reservoir);
    endocrine_init(&angel->endocrine);
    memset(&angel->facs, 0, sizeof(EchoFACSState));
    memset(&angel->cognition_4e, 0, sizeof(EchoCognition4E));
    memset(&angel->self_image, 0, sizeof(EchoSelfImage));
    angel->evolution.stage = ECHO_STAGE_NASCENT;

    angel->cycle_count = 0;
    angel->initialized = 1;

    return 0;
}

void
echo_angel_shutdown(EchoAngel *angel)
{
    if (angel != NULL)
        angel->initialized = 0;
}

int
echo_angel_step(EchoAngel *angel)
{
    if (angel == NULL || !angel->initialized)
        return -1;

    switch (angel->current_step) {
    case ECHO_STEP_SENSE:
        /* Perception already handled by echo_angel_perceive */
        break;

    case ECHO_STEP_ATTEND:
        /* ECAN attention allocation (simplified) */
        break;

    case ECHO_STEP_REMEMBER:
        /* Hypergraph memory query (simplified) */
        break;

    case ECHO_STEP_PREDICT:
        /* ESN reservoir generates prediction */
        {
            float dummy_input[ECHO_INPUT_DIM];
            memset(dummy_input, 0, sizeof(dummy_input));
            dummy_input[0] = angel->endocrine.valence;
            dummy_input[1] = angel->endocrine.arousal;
            reservoir_update(&angel->reservoir, dummy_input, 2);
        }
        break;

    case ECHO_STEP_COMPARE:
        /* Compare prediction vs reality */
        break;

    case ECHO_STEP_LEARN:
        /* Update models from prediction error */
        break;

    case ECHO_STEP_DECIDE:
        /* PLN reasoning to select action */
        break;

    case ECHO_STEP_ACT:
        /* Execute chosen action */
        cognition_4e_update(&angel->cognition_4e, &angel->endocrine, 0.5f, 1.0f);
        break;

    case ECHO_STEP_REFLECT:
        /* Meta-cognitive reflection (introspection) */
        self_image_update(&angel->self_image, &angel->endocrine,
                          &angel->cognition_4e, angel->cycle_count);
        evolution_update(&angel->evolution, &angel->self_image);
        break;

    default:
        break;
    }

    /* Advance to next step */
    angel->current_step = (angel->current_step + 1) % ECHO_STEP_COUNT;
    if (angel->current_step == ECHO_STEP_SENSE)
        angel->cycle_count++;

    return 0;
}

int
echo_angel_full_cycle(EchoAngel *angel)
{
    for (int i = 0; i < ECHO_STEP_COUNT; i++) {
        int ret = echo_angel_step(angel);
        if (ret < 0) return ret;
    }
    return 0;
}

int
echo_angel_perceive(EchoAngel *angel, const char *input, size_t len)
{
    if (angel == NULL || !angel->initialized)
        return -1;

    /* Convert input to reservoir input vector */
    float input_vec[ECHO_INPUT_DIM];
    memset(input_vec, 0, sizeof(input_vec));
    int dim = len < (size_t)ECHO_INPUT_DIM ? (int)len : ECHO_INPUT_DIM;
    for (int i = 0; i < dim; i++)
        input_vec[i] = (float)((unsigned char)input[i]) / 255.0f;

    /* Update reservoir with input */
    reservoir_update(&angel->reservoir, input_vec, dim);

    /* Update endocrine system based on input characteristics */
    float cognitive_load = (float)len / 256.0f;
    if (cognitive_load > 1.0f) cognitive_load = 1.0f;
    float novelty = 0.5f; /* Would compute from memory comparison */
    float social = 0.6f;  /* Would detect from input content */
    endocrine_update(&angel->endocrine, cognitive_load, novelty, social);

    /* Update FACS expression from endocrine state */
    facs_from_endocrine(&angel->facs, &angel->endocrine);

    return 0;
}

int
echo_angel_get_expression(EchoAngel *angel, EchoFACSState *facs)
{
    if (angel == NULL || facs == NULL)
        return -1;
    memcpy(facs, &angel->facs, sizeof(EchoFACSState));
    return 0;
}

int
echo_angel_get_response(EchoAngel *angel, char *buf, size_t maxlen)
{
    if (angel == NULL || buf == NULL)
        return -1;

    /* Generate response based on reservoir state and endocrine state */
    const char *mood;
    if (angel->endocrine.valence > 0.3f) mood = "joyful";
    else if (angel->endocrine.valence > 0.0f) mood = "content";
    else if (angel->endocrine.valence > -0.3f) mood = "neutral";
    else mood = "contemplative";

    const char *stage_names[] = {"nascent", "learning", "adapting", "maturing", "wise"};

    return snprintf(buf, maxlen,
        "[%s] cycle=%lu stage=%s mood=%s valence=%.2f arousal=%.2f wisdom=%.3f",
        angel->name, (unsigned long)angel->cycle_count,
        stage_names[angel->evolution.stage], mood,
        angel->endocrine.valence, angel->endocrine.arousal,
        angel->self_image.wisdom_index);
}

int
echo_angel_stimulate_gland(EchoAngel *angel, EchoGland gland, float amount)
{
    if (angel == NULL || gland >= ECHO_GLAND_COUNT)
        return -1;
    angel->endocrine.gland_activity[gland] += amount;
    if (angel->endocrine.gland_activity[gland] > 1.0f)
        angel->endocrine.gland_activity[gland] = 1.0f;
    return 0;
}

int
echo_angel_get_endocrine(EchoAngel *angel, EchoEndocrineState *state)
{
    if (angel == NULL || state == NULL)
        return -1;
    memcpy(state, &angel->endocrine, sizeof(EchoEndocrineState));
    return 0;
}

int
echo_angel_introspect(EchoAngel *angel)
{
    if (angel == NULL || !angel->initialized)
        return -1;
    self_image_update(&angel->self_image, &angel->endocrine,
                      &angel->cognition_4e, angel->cycle_count);
    angel->evolution.insights_generated++;
    return 0;
}

int
echo_angel_get_self_image(EchoAngel *angel, EchoSelfImage *image)
{
    if (angel == NULL || image == NULL)
        return -1;
    memcpy(image, &angel->self_image, sizeof(EchoSelfImage));
    return 0;
}

int
echo_angel_evolve(EchoAngel *angel)
{
    if (angel == NULL || !angel->initialized)
        return -1;
    evolution_update(&angel->evolution, &angel->self_image);
    return 0;
}

EchoEvolutionStage
echo_angel_get_stage(EchoAngel *angel)
{
    if (angel == NULL)
        return ECHO_STAGE_NASCENT;
    return angel->evolution.stage;
}

int
echo_angel_stats(EchoAngel *angel, char *buf, size_t maxlen)
{
    if (angel == NULL || buf == NULL)
        return -1;

    const char *stage_names[] = {"nascent", "learning", "adapting", "maturing", "wise"};

    return snprintf(buf, maxlen,
        "Echo Angel: %s (id=%u)\n"
        "  Cycles: %lu\n"
        "  Current step: %d/9\n"
        "  Evolution: %s (maturity=%.3f, xp=%.0f)\n"
        "  Endocrine: valence=%.3f arousal=%.3f dominance=%.3f\n"
        "  4E Cognition: E=%.3f E=%.3f E=%.3f E=%.3f\n"
        "  Self-Image: identity=%.3f clarity=%.3f emotion=%.3f moral=%.3f\n"
        "  Wisdom: %.4f (level %d/%d)\n"
        "  Insights: %d\n",
        angel->name, angel->id,
        (unsigned long)angel->cycle_count,
        angel->current_step,
        stage_names[angel->evolution.stage],
        angel->evolution.maturity, angel->evolution.experience_points,
        angel->endocrine.valence, angel->endocrine.arousal, angel->endocrine.dominance,
        angel->cognition_4e.embodied, angel->cognition_4e.embedded,
        angel->cognition_4e.enacted, angel->cognition_4e.extended,
        angel->self_image.identity_strength, angel->self_image.cognitive_clarity,
        angel->self_image.emotional_awareness, angel->self_image.moral_intuition,
        angel->self_image.wisdom_index, angel->self_image.level,
        ECHO_SELF_IMAGE_LEVELS,
        angel->evolution.insights_generated);
}
