#!/bin/bash
src="https://huggingface.co/ggerganov/ggml"
pfx="resolve/main/ggml-model-gpt-2"
ggml_path=$(dirname $(realpath $0))
models=( "117M" "345M" "774M" "1558M" )
function list_models {
    printf "\n"
    printf "  Available models:"
    for model in "${models[@]}"; do
        printf " $model"
    done
    printf "\n\n"
}
if [ "$
    printf "Usage: $0 <model>\n"
    list_models
    exit 1
fi
model=$1
if [[ ! " ${models[@]} " =~ " ${model} " ]]; then
    printf "Invalid model: $model\n"
    list_models
    exit 1
fi
printf "Downloading ggml model $model ...\n"
mkdir -p models/gpt-2-$model
if [ -x "$(command -v wget)" ]; then
    wget --quiet --show-progress -O models/gpt-2-$model/ggml-model.bin $src/$pfx-$model.bin
elif [ -x "$(command -v curl)" ]; then
    curl -L --output models/gpt-2-$model/ggml-model.bin $src/$pfx-$model.bin
else
    printf "Either wget or curl is required to download models.\n"
    exit 1
fi
if [ $? -ne 0 ]; then
    printf "Failed to download ggml model $model \n"
    printf "Please try again later or download the original GPT-2 model files and convert them yourself.\n"
    exit 1
fi
printf "Done! Model '$model' saved in 'models/gpt-2-$model/ggml-model.bin'\n"
printf "You can now use it like this:\n\n"
printf "  $ ./bin/gpt-2 -m models/gpt-2-$model/ggml-model.bin -p \"This is an example\"\n"
printf "\n"