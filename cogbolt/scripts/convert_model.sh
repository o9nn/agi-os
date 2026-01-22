#!/bin/bash
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
echo "========================================"
echo "RWKV Model Conversion Tool"
echo "========================================"
echo ""
if [ $
echo "Usage: $0 <input_model> [output_model] [vocab_path]"
echo ""
echo "Supported input formats:"
echo "  - .pth (PyTorch checkpoint)"
echo "  - .gguf (old GGML format - will be converted to new GGUF)"
echo ""
echo "Examples:"
echo "  $0 model.pth"
echo "  $0 model.pth model-converted.gguf"
echo "  $0 model.pth model.gguf vocab.txt"
echo ""
exit 1
fi
INPUT_MODEL="$1"
OUTPUT_MODEL="${2:-}"
VOCAB_PATH="${3:-}"
if [ ! -f "$INPUT_MODEL" ]; then
echo "Error: Input model file not found: $INPUT_MODEL"
exit 1
fi
if [ -z "$OUTPUT_MODEL" ]; then
OUTPUT_MODEL="${INPUT_MODEL%.pth}.gguf"
OUTPUT_MODEL="${OUTPUT_MODEL%.bin}.gguf"
fi
echo "Input model:  $INPUT_MODEL"
echo "Output model: $OUTPUT_MODEL"
if [ -n "$VOCAB_PATH" ]; then
echo "Vocabulary:   $VOCAB_PATH"
fi
echo ""
FILE_MAGIC=$(head -c 4 "$INPUT_MODEL" | od -A n -t x1 | tr -d ' ')
echo "Detecting format..."
echo "Magic number: $FILE_MAGIC"
echo ""
if [[ "$INPUT_MODEL" == *.pth ]]; then
echo "Format: PyTorch checkpoint (.pth)"
echo "Converting .pth to GGUF..."
echo ""
CONVERT_SCRIPT="$SCRIPT_DIR/convert_rwkv_pth_to_gguf.py"
if [ ! -f "$CONVERT_SCRIPT" ]; then
echo "Error: Conversion script not found: $CONVERT_SCRIPT"
echo "Please run: cd scripts && wget https://raw.githubusercontent.com/MollySophia/rwkv-mobile/master/converter/convert_rwkv_pth_to_gguf.py"
exit 1
fi
CMD="python3 '$CONVERT_SCRIPT' '$INPUT_MODEL' --outfile '$OUTPUT_MODEL'"
if [ -n "$VOCAB_PATH" ]; then
CMD="$CMD --vocab '$VOCAB_PATH'"
fi
echo "Running: $CMD"
echo ""
eval $CMD
if [ $? -eq 0 ]; then
echo ""
echo "✓ Conversion successful!"
echo "Output: $OUTPUT_MODEL"
ls -lh "$OUTPUT_MODEL"
else
echo ""
echo "✗ Conversion failed"
exit 1
fi
elif [ "$FILE_MAGIC" = "666d6767" ]; then
echo "Format: Old GGML format"
echo "Note: This format is not directly supported by the current implementation"
echo ""
echo "Options:"
echo "  1. Find the original .pth file and convert that instead"
echo "  2. Add GGML format support to the loader"
echo "  3. Use a different model in GGUF format"
echo ""
exit 1
elif [ "$FILE_MAGIC" = "47475546" ]; then
echo "Format: GGUF (already in correct format)"
echo "No conversion needed!"
echo ""
if [ "$INPUT_MODEL" != "$OUTPUT_MODEL" ]; then
echo "Copying to output location..."
cp "$INPUT_MODEL" "$OUTPUT_MODEL"
echo "✓ Copied to: $OUTPUT_MODEL"
fi
else
echo "Format: Unknown"
echo "This file format is not recognized"
echo ""
echo "Supported formats:"
echo "  - .pth (PyTorch) - Magic: 504b0304 or others"
echo "  - .gguf (GGUF)   - Magic: 47475546 (GGUF)"
echo "  - .ggml (old)    - Magic: 666d6767 (ggmf)"
echo ""
exit 1
fi
echo ""
echo "========================================"
echo "Conversion Complete"
echo "========================================"
echo ""
echo "Next steps:"
echo "  1. Move model to test directory:"
echo "     mv '$OUTPUT_MODEL' '$PROJECT_ROOT/test/data/models/'"
echo ""
echo "  2. Run model loading test:"
echo "     cd '$PROJECT_ROOT/build'"
echo "     export LD_LIBRARY_PATH=.:./ ggml/ggml.cpp/src:\$LD_LIBRARY_PATH"
echo "     ./test_model_loading"
echo ""