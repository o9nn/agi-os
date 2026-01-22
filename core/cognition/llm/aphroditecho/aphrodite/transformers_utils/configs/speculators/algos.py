SUPPORTED_SPECULATORS_TYPES = {}
def register_speculator(name):
    def decorator(fn):
        SUPPORTED_SPECULATORS_TYPES[name] = fn
        return fn
    return decorator
@register_speculator('eagle3')
def update_eagle3(config_dict: dict, aphrodite_config: dict) -> None:
    aphrodite_config['draft_vocab_size'] = config_dict.get('draft_vocab_size')
    if config_dict.get('target_hidden_size') is not None:
        aphrodite_config['target_hidden_size'] = config_dict['target_hidden_size']
    aphrodite_config['norm_before_residual'] = config_dict.get('norm_before_residual', True)
    aphrodite_config['architectures'] = ['Eagle3LlamaForCausalLM']