#!/usr/bin/env bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR
set -eux
mkdir -p $SCRIPT_DIR/output
PROJ_ROOT="$SCRIPT_DIR/../.."
cd $PROJ_ROOT
RUN_BIG_TESTS=false
if [ "${1:-}" = "big" ]; then
    RUN_BIG_TESTS=true
    echo "Include BIG models..."
fi
RUN_HUGE_TESTS=false
if [ "${1:-}" = "huge" ]; then
    RUN_HUGE_TESTS=true
    RUN_BIG_TESTS=true
    echo "Include BIG and HUGE models..."
fi
arr_prefix=()
arr_hf=()
arr_tmpl=()
arr_file=()
add_test_vision() {
    local hf=$1
    local tmpl=${2:-""}
    arr_prefix+=("[vision]")
    arr_hf+=("$hf")
    arr_tmpl+=("$tmpl")
    arr_file+=("test-1.jpeg")
}
add_test_audio() {
    local hf=$1
    arr_prefix+=("[audio] ")
    arr_hf+=("$hf")
    arr_tmpl+=("")
    arr_file+=("test-2.mp3")
}
add_test_vision "ggml-org/SmolVLM-500M-Instruct-GGUF:Q8_0"
add_test_vision "ggml-org/SmolVLM2-2.2B-Instruct-GGUF:Q4_K_M"
add_test_vision "ggml-org/SmolVLM2-500M-Video-Instruct-GGUF:Q8_0"
add_test_vision "ggml-org/gemma-3-4b-it-GGUF:Q4_K_M"
add_test_vision "THUDM/glm-edge-v-5b-gguf:Q4_K_M"
add_test_vision "second-state/Llava-v1.5-7B-GGUF:Q2_K"            "vicuna"
add_test_vision "cjpais/llava-1.6-mistral-7b-gguf:Q3_K_M"         "vicuna"
add_test_vision "ibm-research/granite-vision-3.2-2b-GGUF:Q4_K_M"
add_test_vision "second-state/MiniCPM-Llama3-V-2_5-GGUF:Q2_K"
add_test_vision "openbmb/MiniCPM-V-2_6-gguf:Q2_K"
add_test_vision "openbmb/MiniCPM-o-2_6-gguf:Q4_0"
add_test_vision "bartowski/Qwen2-VL-2B-Instruct-GGUF:Q4_K_M"
add_test_vision "ggml-org/Qwen2.5-VL-3B-Instruct-GGUF:Q4_K_M"
add_test_vision "ggml-org/InternVL2_5-1B-GGUF:Q8_0"
add_test_vision "ggml-org/InternVL3-1B-Instruct-GGUF:Q8_0"
add_test_vision "ggml-org/Qwen2.5-Omni-3B-GGUF:Q4_K_M"
add_test_audio  "ggml-org/ultravox-v0_5-llama-3_2-1b-GGUF:Q8_0"
add_test_audio  "ggml-org/Qwen2.5-Omni-3B-GGUF:Q4_K_M"
add_test_audio  "ggml-org/Voxtral-Mini-3B-2507-GGUF:Q4_K_M"
if [ "$RUN_BIG_TESTS" = true ]; then
    add_test_vision "ggml-org/pixtral-12b-GGUF:Q4_K_M"
    add_test_vision "ggml-org/Mistral-Small-3.1-24B-Instruct-2503-GGUF" "mistral-v7"
    add_test_vision "ggml-org/Qwen2-VL-2B-Instruct-GGUF:Q4_K_M"
    add_test_vision "ggml-org/Qwen2-VL-7B-Instruct-GGUF:Q4_K_M"
    add_test_vision "ggml-org/Qwen2.5-VL-3B-Instruct-GGUF:Q4_K_M"
    add_test_vision "ggml-org/Qwen2.5-VL-7B-Instruct-GGUF:Q4_K_M"
    add_test_vision "ggml-org/InternVL3-8B-Instruct-GGUF:Q4_K_M"
    add_test_vision "ggml-org/InternVL3-14B-Instruct-GGUF:Q4_K_M"
    add_test_vision "ggml-org/Qwen2.5-Omni-7B-GGUF:Q4_K_M"
    add_test_audio  "ggml-org/ultravox-v0_5-llama-3_1-8b-GGUF:Q4_K_M"
    add_test_audio  "ggml-org/Qwen2.5-Omni-7B-GGUF:Q4_K_M"
fi
if [ "$RUN_HUGE_TESTS" = true ]; then
    add_test_vision "ggml-org/Qwen2.5-VL-72B-Instruct-GGUF:Q4_K_M"
    add_test_vision "ggml-org/Llama-4-Scout-17B-16E-Instruct-GGUF:IQ1_S"
fi
cmake --build build -j --target llama-mtmd-cli
arr_res=()
for i in "${!arr_hf[@]}"; do
    bin="llama-mtmd-cli"
    prefix="${arr_prefix[$i]}"
    hf="${arr_hf[$i]}"
    tmpl="${arr_tmpl[$i]}"
    inp_file="${arr_file[$i]}"
    echo "Running test with binary: $bin and HF model: $hf"
    echo ""
    echo ""
    output=$(\
        "$PROJ_ROOT/build/bin/$bin" \
        -hf "$hf" \
        --image $SCRIPT_DIR/$inp_file \
        -p "what is the publisher name of the newspaper?" \
        --temp 0 -n 128 \
        ${tmpl:+--chat-template "$tmpl"} \
        2>&1 | tee /dev/tty)
    echo "$output" > $SCRIPT_DIR/output/$bin-$(echo "$hf" | tr '/' '-').log
    if echo "$output" | grep -iq "new york"; then
        result="$prefix \033[32mOK\033[0m:   $bin $hf"
    else
        result="$prefix \033[31mFAIL\033[0m: $bin $hf"
    fi
    echo -e "$result"
    arr_res+=("$result")
    echo ""
    echo ""
    echo ""
    echo "
    echo "
    echo ""
    echo ""
done
set +x
for i in "${!arr_res[@]}"; do
    echo -e "${arr_res[$i]}"
done
echo ""
echo "Output logs are saved in $SCRIPT_DIR/output"