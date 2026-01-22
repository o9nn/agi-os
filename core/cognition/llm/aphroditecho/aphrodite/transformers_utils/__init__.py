from aphrodite.common import envs
if envs.APHRODITE_USE_MODELSCOPE:
    try:
        import modelscope
        from packaging import version
        if version.parse(modelscope.__version__) <= version.parse('1.18.0'):
            raise ImportError('Using vLLM with ModelScope needs modelscope>=1.18.1, please install by `pip install modelscope -U`')
        from modelscope.utils.hf_util import patch_hub
        patch_hub()
    except ImportError as err:
        raise ImportError('Please install modelscope>=1.18.1 via `pip install modelscope>=1.18.1` to use ModelScope.') from err