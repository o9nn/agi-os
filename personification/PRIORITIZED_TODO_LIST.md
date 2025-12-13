# Prioritized TODO/FIXME List for moeru-ai

**Date:** December 12, 2025
**Author:** Manus AI

---

## 1. Introduction

This document presents a prioritized list of the 5 most critical `TODO` and `FIXME` markers identified in the `moeru-ai` codebase. These items have been selected based on their significant impact on core functionality, architectural integrity, and the overall progress of the project. Addressing these markers is essential for bridging the gap between the project's advanced theoretical framework and its practical implementation.

---

## 2. Prioritized List of Critical Markers

### Priority 1: Implement Kernel Fitness Evaluation

| | |
| :--- | :--- |
| **File & Line** | `packages/character-neuro/src/character.ts:179` |
| **TODO Marker** | `// TODO: Implement kernel fitness evaluation and self-optimization` |

#### Impact Analysis
This is the **most critical `TODO`** in the entire repository as it lies at the heart of the AGI's ability to learn and evolve. The `character-neuro` package is intended to be the most advanced cognitive agent, but without a mechanism for self-optimization, it remains a static system. Implementing this feature is the first step toward achieving genuine autonomous intelligence and fulfilling the project's core vision of a self-improving AGI.

#### Actionable Recommendation
1.  **Define Fitness Metrics:** Collaborate with the project lead to define a set of quantifiable metrics for "kernel fitness." These could include task completion efficiency, relevance realization accuracy, and adherence to personality constraints.
2.  **Implement a Feedback Loop:** Create a new class, `KernelOptimizer`, that periodically evaluates the agent's performance against the defined fitness metrics.
3.  **Connect to Personality Evolution:** Use the output of the `KernelOptimizer` to make small, incremental adjustments to the `NeuroPersonality` traits within the bounds defined in `PERSONALITY_EVOLUTION_BOUNDS`.
4.  **Add Logging and Monitoring:** Implement logging to track how the agent's personality and performance change over time, providing a clear audit trail of its self-optimization process.

---

### Priority 2: Implement Automatic Tool Discovery

| | |
| :--- | :--- |
| **File & Line** | `packages/stage-ui/src/stores/llm.ts:55` |
| **TODO Marker** | `// TODO: we need Automatic tools discovery` |

#### Impact Analysis
This `TODO` is critical for the **extensibility and scalability** of the agent's capabilities. Currently, the tools available to the LLM are hard-coded, which means that every new tool or MCP integration requires a manual code change. This creates a significant bottleneck for development and limits the agent's ability to adapt to new tasks and environments dynamically. Implementing automatic tool discovery will make the system far more modular and powerful.

#### Actionable Recommendation
1.  **Create a Tool Registry:** Design a centralized `ToolRegistry` class that can be populated at runtime.
2.  **Develop a Tool Manifest Standard:** Define a standard JSON or YAML format for tool manifests. Each manifest should describe the tool's name, description, input parameters, and output schema.
3.  **Implement a Discovery Mechanism:** On application startup, the `ToolRegistry` should scan a designated `tools` directory for manifests and register them.
4.  **Integrate with the LLM Store:** Modify the `llm.ts` store to dynamically fetch the list of available tools from the `ToolRegistry` and provide them to the LLM during chat sessions.

---

### Priority 3: Implement VAD-Driven Silence Detection

| | |
| :--- | :--- |
| **File & Line** | `packages/stage-ui/src/stores/modules/hearing.ts:102` |
| **TODO Marker** | `// TODO: integrate VAD-driven silence detection to stop and restart realtime sessions based on silence thresholds.` |

#### Impact Analysis
This `TODO` is crucial for creating a **natural and efficient user experience** in voice-based interactions. Without Voice Activity Detection (VAD), the system cannot distinguish between speech and silence, leading to inefficient use of resources (continuous streaming to the STT service) and an unnatural conversational flow. Implementing VAD will enable the agent to listen and respond in a more human-like manner.

#### Actionable Recommendation
1.  **Select a VAD Library:** Choose a suitable web-based VAD library, such as `@ricky0123/vad-web`.
2.  **Integrate VAD into the `hearing` Store:** In the `hearing.ts` store, wrap the audio input stream with the VAD library.
3.  **Implement State Management:** Use the VAD's `onSpeechStart` and `onSpeechEnd` events to control the streaming of audio to the speech-to-text service. Only send audio when the user is actively speaking.
4.  **Add Configuration Options:** Expose VAD settings (e.g., silence threshold, padding) in the application's settings UI to allow users to fine-tune the detection for their environment.

---

### Priority 4: Implement Confidence Calibration

| | |
| :--- | :--- |
| **File & Line** | `packages/cognitive-core/src/relevance-coordinator.ts:95` |
| **TODO Marker** | `// TODO: Implement confidence calibration` |

#### Impact Analysis
This `TODO` is vital for the **reliability and trustworthiness** of the agent's cognitive processes. The `RelevanceCoordinator` is responsible for deciding what the agent pays attention to, but it currently assigns a static confidence score of `0.8`. A robust AGI needs to be aware of its own uncertainty. Implementing confidence calibration will allow the agent to know when it is operating in a domain of high uncertainty, which can be used to trigger information-seeking behaviors or to express its uncertainty to the user.

#### Actionable Recommendation
1.  **Develop a Calibration Model:** Create a `ConfidenceCalibrator` class that takes the components of the relevance score as input.
2.  **Implement a Heuristic-Based Model:** Initially, this can be a simple heuristic-based model. For example, confidence could be a function of the variance between the different relevance components (novelty, emotional, pragmatic, etc.). High variance might indicate low confidence.
3.  **Integrate with the `RelevanceCoordinator`:** Replace the hard-coded `0.8` value with a call to the `ConfidenceCalibrator`.
4.  **Plan for a Learning-Based Model:** For a more advanced implementation, plan to collect data on the outcomes of the agent's decisions and use this data to train a machine learning model that can more accurately predict the probability of success.

---

### Priority 5: Integrate the Emotion Mapper

| | |
| :--- | :--- |
| **File & Line** | `packages/stage-ui/src/composables/live2d/animation.ts:28` |
| **TODO Marker** | `// TODO: After emotion mapper, stage editor, eye related parameters should be take cared to be dynamical instead of hardcoding` |

#### Impact Analysis
This `TODO` is critical for creating an **expressive and engaging embodied agent**. While a foundational `EmotionMapper` already exists in the `live2d-core` package, it is not being used in the `stage-ui`. The animation logic currently relies on hard-coded parameter values, which limits the agent's emotional expressiveness. Integrating the existing `EmotionMapper` will bring the Live2D model to life and create a more believable and engaging user experience.

#### Actionable Recommendation
1.  **Import and Instantiate the `EmotionMapper`:** In the `animation.ts` composable, import the `EmotionMapper` from `@proj-airi/live2d-core` and create an instance of it.
2.  **Connect to the Agent's Emotional State:** Access the agent's current emotional state from the relevant Pinia store (e.g., the `llm` store or a dedicated `character` store).
3.  **Dynamically Apply Parameters:** In the `update` function, use the `EmotionMapper` to get the target parameter values based on the current emotion. Replace the hard-coded `ParamEyeBallX` and `ParamEyeBallY` updates with a loop that dynamically applies all parameters from the mapper.
4.  **Expose Emotion Controls:** In the application's UI, add controls to allow users to manually set the agent's emotion to test the full range of expressions provided by the `EmotionMapper`.

---

## 3. Conclusion

Addressing these five critical `TODO` markers will have a transformative impact on the `moeru-ai` project. By focusing on these high-leverage areas, the development team can quickly move from a state of foundational but fragmented potential to a cohesive, functional, and evolving proto-AGI system. The successful implementation of these features will not only stabilize the project but also unlock its true capacity for autonomous intelligence and engaging user engagement.
