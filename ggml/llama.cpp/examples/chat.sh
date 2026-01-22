#!/usr/bin/env bash
cd `dirname $0`
cd ..
./llama-cli -m ./models/llama-7b/ggml-model-q4_0.gguf -c 512 -b 1024 -n 256 --keep 48 \
    --repeat_penalty 1.0 --color -i \
    -r "User:" -f prompts/chat-with-bob.txt