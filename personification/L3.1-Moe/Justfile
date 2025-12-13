# show available recipes.
list:
  @just --list

merge variant='4x8b' version='0.2' device='cuda':
  # rm -r result/{{variant}}-v{{version}}
  mkdir -p result/{{variant}}-v{{version}}
  mergekit-moe ./cfg/{{variant}}-v{{version}}.yml ./result/{{variant}}-v{{version}} --device {{device}}

upload variant='4x8b' version='0.2':
  huggingface-cli upload moeru-ai/L3.1-Moe-{{replace(variant, 'b', '')}}B-v{{version}} ./result/{{variant}}-v{{version}} .
