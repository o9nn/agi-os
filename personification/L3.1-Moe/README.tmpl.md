---
license: llama3.1
library_name: transformers
tags:
  - moe
  - frankenmoe
  - merge
  - mergekit
base_model:
  - Joseph717171/Llama-3.1-SuperNova-8B-Lite_TIES_with_Base
  - ArliAI/Llama-3.1-8B-ArliAI-RPMax-v1.2
  - rombodawg/rombos_Replete-Coder-Instruct-8b-Merged
  - 3rd-Degree-Burn/Llama-3.1-8B-Squareroot-v0
---

# L3.1-Moe-4x8B-v0.2

This model is a Mixture of Experts (MoE) made with mergekit-moe. It uses the following base models:

- [Joseph717171/Llama-3.1-SuperNova-8B-Lite_TIES_with_Base](https://huggingface.co/Joseph717171/Llama-3.1-SuperNova-8B-Lite_TIES_with_Base)
- [ArliAI/Llama-3.1-8B-ArliAI-RPMax-v1.2](https://huggingface.co/ArliAI/Llama-3.1-8B-ArliAI-RPMax-v1.2)
- [rombodawg/rombos_Replete-Coder-Instruct-8b-Merged](https://huggingface.co/rombodawg/rombos_Replete-Coder-Instruct-8b-Merged)
- [3rd-Degree-Burn/Llama-3.1-8B-Squareroot-v0](https://huggingface.co/3rd-Degree-Burn/Llama-3.1-8B-Squareroot-v0)

Heavily inspired by [mlabonne/Beyonder-4x7B-v3](https://huggingface.co/mlabonne/Beyonder-4x7B-v3).

## Quantized models

> TODO

## Mergekit config

```yaml
base_model: Joseph717171/Llama-3.1-SuperNova-8B-Lite_TIES_with_Base
gate_mode: hidden
dtype: bfloat16
experts:
  - source_model: Joseph717171/Llama-3.1-SuperNova-8B-Lite_TIES_with_Base
    positive_prompts: &chat_prompts
      - "chat"
      - "assistant"
      - "tell me"
      - "explain"
      - "I want"
    negative_prompts: &rp_prompts
      - "storywriting"
      - "write"
      - "scene"
      - "story"
      - "character"
  - source_model: ArliAI/Llama-3.1-8B-ArliAI-RPMax-v1.2
    positive_prompts: *rp_prompts
    negative_prompts: *chat_prompts
  - source_model: rombodawg/rombos_Replete-Coder-Instruct-8b-Merged
    positive_prompts:
      - "code"
      - "python"
      - "javascript"
      - "programming"
      - "algorithm"
  - source_model: 3rd-Degree-Burn/Llama-3.1-8B-Squareroot-v0
    positive_prompts:
      - "reason"
      - "math"
      - "mathematics"
      - "solve"
      - "count"
```
