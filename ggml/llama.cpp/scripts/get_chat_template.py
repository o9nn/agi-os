import json
import re
import sys
def get_chat_template(model_id, variant=None):
    try:
        from huggingface_hub import hf_hub_download
        with open(hf_hub_download(repo_id=model_id, filename='tokenizer_config.json'), encoding='utf-8') as f:
            config_str = f.read()
    except ImportError:
        import requests
        assert re.match('^[\\w.-]+/[\\w.-]+$', model_id), f'Invalid model ID: {model_id}'
        response = requests.get(f'https://huggingface.co/{model_id}/resolve/main/tokenizer_config.json')
        if response.status_code == 401:
            raise Exception('Access to this model is gated, please request access, authenticate with `huggingface-cli login` and make sure to run `pip install huggingface_hub`')
        response.raise_for_status()
        config_str = response.text
    try:
        config = json.loads(config_str)
    except json.JSONDecodeError:
        config = json.loads(re.sub('\\}([\\n\\s]*\\}[\\n\\s]*\\],[\\n\\s]*"clean_up_tokenization_spaces")', '\\1', config_str))
    chat_template = config['chat_template']
    if isinstance(chat_template, str):
        return chat_template
    else:
        variants = {ct['name']: ct['template'] for ct in chat_template}
        def format_variants():
            return ', '.join((f'"{v}"' for v in variants.keys()))
        if variant is None:
            if 'default' not in variants:
                raise Exception(f'Please specify a chat template variant (one of {format_variants()})')
            variant = 'default'
            sys.stderr.write(f'Note: picked "default" chat template variant (out of {format_variants()})\n')
        elif variant not in variants:
            raise Exception(f'Variant {variant} not found in chat template (found {format_variants()})')
        return variants[variant]
def main(args):
    if len(args) < 1:
        raise ValueError('Please provide a model ID and an optional variant name')
    model_id = args[0]
    variant = None if len(args) < 2 else args[1]
    template = get_chat_template(model_id, variant)
    sys.stdout.write(template)
if __name__ == '__main__':
    main(sys.argv[1:])