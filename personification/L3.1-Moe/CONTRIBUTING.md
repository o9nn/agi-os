# Contributing

## Clone project

```bash
git clone https://github.com/moeru-ai/L3.1-Moe.git
cd L3.1-Moe
```

## Install mergekit

```bash
git clone https://github.com/arcee-ai/mergekit.git
pip install -e mergekit
```

## Run

```bash
rm -r output # if output exists
mkdir output
mergekit-moe ./config.yml ./output --device cuda
```
