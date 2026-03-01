/*
 * Echo Angel Bridge for Cognitive Grip
 *
 * Middle agi-os layer: integrates the Echo Angel cognitive avatar
 * into the unified cognitive-grip abstraction layer, connecting it
 * to all other agi-os subsystems (CogNUMach, HurdCog, OpenCog,
 * Inferno Kernel, CogBolt, KoboldCpp).
 *
 * This bridge implements the ⊗ (multiplicative) integration where
 * the Echo Angel's state is entangled with all other subsystems.
 *
 * Copyright (C) 2026 OpenCog Community
 * Licensed under AGPL-3.0
 */

#include <cstdio>
#include <cstring>
#include <cmath>
#include <string>
#include <vector>
#include <mutex>

/* C linkage for echo_angel kernel module */
extern "C" {
#include "../../avatar/echo-angel/include/echo_angel.h"
}

namespace cognitive_grip {

/* ================================================================
 * Echo Angel Bridge
 * ================================================================ */

class EchoAngelBridge {
public:
    EchoAngelBridge() : initialized_(false) {}

    ~EchoAngelBridge() {
        if (initialized_) {
            echo_angel_shutdown(&angel_);
        }
    }

    /* Initialize the Echo Angel with a given name */
    bool init(const std::string& name) {
        std::lock_guard<std::mutex> lock(mutex_);
        if (echo_angel_init(&angel_, name.c_str()) == 0) {
            initialized_ = true;
            return true;
        }
        return false;
    }

    /* ============================================================
     * AtomSpace Integration (⊗ with OpenCog Layer 3)
     * ============================================================
     *
     * The Echo Angel's cognitive state is represented as atoms
     * in the shared AtomSpace, enabling PLN reasoning about
     * the avatar's emotional and cognitive states.
     */

    std::string export_to_atomese() const {
        std::lock_guard<std::mutex> lock(mutex_);
        if (!initialized_) return "";

        std::string atomese;

        /* Export endocrine state as EvaluationLinks */
        atomese += "(EvaluationLink (stv "
                + std::to_string(angel_.endocrine.valence * 0.5 + 0.5) + " 0.9)\n"
                + "  (PredicateNode \"echo-angel-valence\")\n"
                + "  (ConceptNode \"" + angel_.name + "\"))\n\n";

        atomese += "(EvaluationLink (stv "
                + std::to_string(angel_.endocrine.arousal) + " 0.9)\n"
                + "  (PredicateNode \"echo-angel-arousal\")\n"
                + "  (ConceptNode \"" + angel_.name + "\"))\n\n";

        /* Export evolution stage */
        const char* stages[] = {"nascent", "learning", "adapting", "maturing", "wise"};
        atomese += "(InheritanceLink (stv "
                + std::to_string(angel_.evolution.maturity) + " 0.95)\n"
                + "  (ConceptNode \"" + angel_.name + "\")\n"
                + "  (ConceptNode \"echo-stage-"
                + stages[angel_.evolution.stage] + "\"))\n\n";

        /* Export wisdom index */
        atomese += "(EvaluationLink (stv "
                + std::to_string(angel_.self_image.wisdom_index) + " 0.85)\n"
                + "  (PredicateNode \"wisdom-index\")\n"
                + "  (ConceptNode \"" + angel_.name + "\"))\n\n";

        /* Export 4E cognition metrics */
        atomese += "(EvaluationLink (stv "
                + std::to_string(angel_.cognition_4e.embodied) + " 0.8)\n"
                + "  (PredicateNode \"4e-embodied\")\n"
                + "  (ConceptNode \"" + angel_.name + "\"))\n";

        return atomese;
    }

    /* ============================================================
     * Inferno Kernel Integration (⊗ with Layer 0)
     * ============================================================
     *
     * The Echo Angel is exposed as a 9P filesystem within the
     * Inferno kernel's cognitive namespace at /angel/<name>/
     */

    int read_9p(const std::string& path, char* buf, size_t maxlen) {
        std::lock_guard<std::mutex> lock(mutex_);
        if (!initialized_) return -1;
        return echo_angel_9p_read(&angel_, path.c_str(), buf, maxlen);
    }

    int write_9p(const std::string& path, const char* data, size_t len) {
        std::lock_guard<std::mutex> lock(mutex_);
        if (!initialized_) return -1;
        return echo_angel_9p_write(&angel_, path.c_str(), data, len);
    }

    /* ============================================================
     * HurdCog Integration (⊗ with Layer 2)
     * ============================================================
     *
     * The Echo Angel's endocrine state can be used by HurdCog's
     * cognitive kernel to influence OS-level attention allocation
     * and resource scheduling.
     */

    struct HurdCogMetrics {
        float attention_priority;
        float resource_affinity;
        float scheduling_urgency;
    };

    HurdCogMetrics get_hurdcog_metrics() const {
        std::lock_guard<std::mutex> lock(mutex_);
        HurdCogMetrics m = {0.0f, 0.0f, 0.0f};
        if (!initialized_) return m;

        /* Map arousal to attention priority */
        m.attention_priority = angel_.endocrine.arousal;

        /* Map engagement (dopamine) to resource affinity */
        m.resource_affinity = angel_.endocrine.hormones[HORMONE_DOPAMINE];

        /* Map stress (cortisol) to scheduling urgency */
        m.scheduling_urgency = angel_.endocrine.hormones[HORMONE_CORTISOL];

        return m;
    }

    /* ============================================================
     * CogNUMach Integration (⊗ with Layer 1)
     * ============================================================
     *
     * The Echo Angel can register IPC ports for inter-process
     * communication with other cognitive services.
     */

    struct IPCPort {
        int port_id;
        std::string service_name;
    };

    std::vector<IPCPort> get_ipc_ports() const {
        std::vector<IPCPort> ports;
        if (!initialized_) return ports;

        ports.push_back({1001, "echo-angel-perceive"});
        ports.push_back({1002, "echo-angel-express"});
        ports.push_back({1003, "echo-angel-introspect"});
        ports.push_back({1004, "echo-angel-endocrine"});
        ports.push_back({1005, "echo-angel-evolve"});
        return ports;
    }

    /* ============================================================
     * KoboldCpp-Cog Integration (⊗ with Layer 3.7)
     * ============================================================
     *
     * The Echo Angel can use LLM inference to generate natural
     * language responses, with the endocrine state influencing
     * the generation parameters (temperature, top-p, etc.)
     */

    struct LLMParams {
        float temperature;
        float top_p;
        float presence_penalty;
        float frequency_penalty;
        std::string system_prompt;
    };

    LLMParams get_llm_params() const {
        std::lock_guard<std::mutex> lock(mutex_);
        LLMParams p;
        if (!initialized_) {
            p.temperature = 0.7f;
            p.top_p = 0.9f;
            p.presence_penalty = 0.0f;
            p.frequency_penalty = 0.0f;
            p.system_prompt = "You are a helpful assistant.";
            return p;
        }

        /* Map endocrine state to LLM parameters */
        /* High arousal → higher temperature (more creative) */
        p.temperature = 0.5f + angel_.endocrine.arousal * 0.5f;

        /* High valence → higher top_p (more diverse) */
        p.top_p = 0.8f + (angel_.endocrine.valence * 0.5f + 0.5f) * 0.15f;

        /* Stress → presence penalty (avoid repetition under stress) */
        p.presence_penalty = angel_.endocrine.hormones[HORMONE_CORTISOL] * 0.5f;

        /* Low serotonin → frequency penalty */
        p.frequency_penalty = (1.0f - angel_.endocrine.hormones[HORMONE_SEROTONIN]) * 0.3f;

        /* Generate system prompt based on evolution stage */
        const char* stage_prompts[] = {
            "You are a curious, newly awakened AI companion exploring the world.",
            "You are an eager AI companion actively learning about everything.",
            "You are an adaptable AI companion finding your unique voice.",
            "You are a mature AI companion with stable personality and deep knowledge.",
            "You are a wise AI companion who cultivates insight and compassion."
        };
        p.system_prompt = stage_prompts[angel_.evolution.stage];

        return p;
    }

    /* ============================================================
     * CogBolt Integration (⊗ with Layer 4)
     * ============================================================
     *
     * The Echo Angel can be visualized and debugged through
     * the CogBolt IDE, providing real-time cognitive state
     * inspection and parameter tuning.
     */

    std::string get_cogbolt_debug_json() const {
        std::lock_guard<std::mutex> lock(mutex_);
        if (!initialized_) return "{}";

        char buf[4096];
        snprintf(buf, sizeof(buf),
            "{"
            "\"name\":\"%s\","
            "\"cycle\":%lu,"
            "\"step\":%d,"
            "\"stage\":%d,"
            "\"valence\":%.4f,"
            "\"arousal\":%.4f,"
            "\"dominance\":%.4f,"
            "\"wisdom\":%.4f,"
            "\"maturity\":%.4f,"
            "\"4e\":{\"embodied\":%.4f,\"embedded\":%.4f,\"enacted\":%.4f,\"extended\":%.4f},"
            "\"self_image\":{\"identity\":%.4f,\"clarity\":%.4f,\"awareness\":%.4f,\"moral\":%.4f}"
            "}",
            angel_.name,
            (unsigned long)angel_.cycle_count,
            angel_.current_step,
            angel_.evolution.stage,
            angel_.endocrine.valence,
            angel_.endocrine.arousal,
            angel_.endocrine.dominance,
            angel_.self_image.wisdom_index,
            angel_.evolution.maturity,
            angel_.cognition_4e.embodied,
            angel_.cognition_4e.embedded,
            angel_.cognition_4e.enacted,
            angel_.cognition_4e.extended,
            angel_.self_image.identity_strength,
            angel_.self_image.cognitive_clarity,
            angel_.self_image.emotional_awareness,
            angel_.self_image.moral_intuition);

        return std::string(buf);
    }

    /* ============================================================
     * Deep Tree Echo Integration (⊗ with existing avatar)
     * ============================================================
     *
     * The Echo Angel bridges to the existing deep-tree-echo
     * TypeScript implementation, providing C-level performance
     * for the cognitive core while maintaining the JS API.
     */

    struct DeepTreeEchoState {
        float reservoir_energy;
        float reservoir_mean;
        float prediction_error;
        int   current_step;
        int   cycle_count;
    };

    DeepTreeEchoState get_deep_tree_state() const {
        std::lock_guard<std::mutex> lock(mutex_);
        DeepTreeEchoState s = {0};
        if (!initialized_) return s;

        /* Compute reservoir energy */
        for (int i = 0; i < ECHO_RESERVOIR_SIZE; i++) {
            s.reservoir_energy += angel_.reservoir.state[i] * angel_.reservoir.state[i];
            s.reservoir_mean += angel_.reservoir.state[i];
        }
        s.reservoir_mean /= ECHO_RESERVOIR_SIZE;
        s.current_step = angel_.current_step;
        s.cycle_count = (int)angel_.cycle_count;

        return s;
    }

    /* ============================================================
     * Unified Cognitive Cycle
     * ============================================================
     *
     * Run a full cognitive cycle with all integrations active.
     */

    bool run_cycle() {
        std::lock_guard<std::mutex> lock(mutex_);
        if (!initialized_) return false;
        return echo_angel_full_cycle(&angel_) == 0;
    }

    bool perceive(const std::string& input) {
        std::lock_guard<std::mutex> lock(mutex_);
        if (!initialized_) return false;
        return echo_angel_perceive(&angel_, input.c_str(), input.size()) == 0;
    }

    bool introspect() {
        std::lock_guard<std::mutex> lock(mutex_);
        if (!initialized_) return false;
        return echo_angel_introspect(&angel_) == 0;
    }

    std::string get_stats() const {
        std::lock_guard<std::mutex> lock(mutex_);
        if (!initialized_) return "not initialized";
        char buf[2048];
        echo_angel_stats(const_cast<EchoAngel*>(&angel_), buf, sizeof(buf));
        return std::string(buf);
    }

    bool is_initialized() const { return initialized_; }

private:
    EchoAngel angel_;
    bool initialized_;
    mutable std::mutex mutex_;
};

/* Global singleton bridge instance */
static EchoAngelBridge g_echo_angel_bridge;

/* ================================================================
 * C API for cognitive_grip integration
 * ================================================================ */

extern "C" {

int cognitive_grip_echo_angel_init(const char* name) {
    return g_echo_angel_bridge.init(name) ? 0 : -1;
}

int cognitive_grip_echo_angel_cycle() {
    return g_echo_angel_bridge.run_cycle() ? 0 : -1;
}

int cognitive_grip_echo_angel_perceive(const char* input, int len) {
    return g_echo_angel_bridge.perceive(std::string(input, len)) ? 0 : -1;
}

int cognitive_grip_echo_angel_introspect() {
    return g_echo_angel_bridge.introspect() ? 0 : -1;
}

int cognitive_grip_echo_angel_9p_read(const char* path, char* buf, int maxlen) {
    return g_echo_angel_bridge.read_9p(path, buf, maxlen);
}

int cognitive_grip_echo_angel_9p_write(const char* path, const char* data, int len) {
    return g_echo_angel_bridge.write_9p(path, data, len);
}

int cognitive_grip_echo_angel_stats(char* buf, int maxlen) {
    std::string s = g_echo_angel_bridge.get_stats();
    strncpy(buf, s.c_str(), maxlen - 1);
    buf[maxlen - 1] = '\0';
    return (int)s.size();
}

int cognitive_grip_echo_angel_debug_json(char* buf, int maxlen) {
    std::string s = g_echo_angel_bridge.get_cogbolt_debug_json();
    strncpy(buf, s.c_str(), maxlen - 1);
    buf[maxlen - 1] = '\0';
    return (int)s.size();
}

int cognitive_grip_echo_angel_atomese(char* buf, int maxlen) {
    std::string s = g_echo_angel_bridge.export_to_atomese();
    strncpy(buf, s.c_str(), maxlen - 1);
    buf[maxlen - 1] = '\0';
    return (int)s.size();
}

} /* extern "C" */

} /* namespace cognitive_grip */
