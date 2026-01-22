#!/usr/bin/env bash
set -e
cd "$(dirname "$0")/.." || exit
MODEL="${MODEL:-./models/llama-2-13b-chat.ggmlv3.q5_K_M.bin}"
PROMPT_TEMPLATE=${PROMPT_TEMPLATE:-./prompts/chat-system.txt}
N_THREAD="${N_THREAD:-12}"
GEN_OPTIONS="${GEN_OPTIONS:---ctx_size 4096 --batch-size 1024}"
./llama-server $GEN_OPTIONS \
  --model "$MODEL" \
  --threads "$N_THREAD" \
  --rope-freq-scale 1.0 \
  "$@"