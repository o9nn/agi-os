from typing import List, Optional, Tuple, Type
from aphrodite.common.utils import is_cpu
if not is_cpu():
    import pytest
    from transformers import AutoModelForSeq2SeqLM
    from aphrodite.common.sequence import SampleLogprobs
    from ....conftest import AphroditeRunner, DecoderPromptType, ExplicitEncoderDecoderPrompt, HfRunner
    from ....utils import multi_gpu_test
    from ...utils import check_logprobs_close
    MODELS = ['facebook/bart-base', 'facebook/bart-large-cnn']
    def aphrodite_to_hf_output(aphrodite_output: Tuple[List[int], str, Optional[SampleLogprobs]], decoder_prompt_type: DecoderPromptType):
        output_ids, output_str, out_logprobs = aphrodite_output
        hf_output_str = output_str + '</s>'
        if decoder_prompt_type == DecoderPromptType.NONE:
            hf_output_str = '<s>' + hf_output_str
        return (output_ids, hf_output_str, out_logprobs)
    def run_test(hf_runner: Type[HfRunner], aphrodite_runner: Type[AphroditeRunner], prompts: List[ExplicitEncoderDecoderPrompt[str, str]], decoder_prompt_type: DecoderPromptType, model: str, *, dtype: str, max_tokens: int, num_logprobs: int, tensor_parallel_size: int, distributed_executor_backend: Optional[str]=None) -> None:
        with aphrodite_runner(model, dtype=dtype, tensor_parallel_size=tensor_parallel_size, distributed_executor_backend=distributed_executor_backend, enforce_eager=True) as aphrodite_model:
            aphrodite_outputs = aphrodite_model.generate_encoder_decoder_greedy_logprobs(prompts, max_tokens, num_logprobs)
        hf_kwargs = {'top_k': None, 'num_beams': 1, 'repetition_penalty': 1.0, 'top_p': 1.0, 'length_penalty': 1.0, 'early_stopping': False, 'no_repeat_ngram_size': None, 'min_length': 0}
        with hf_runner(model, dtype=dtype, auto_cls=AutoModelForSeq2SeqLM) as hf_model:
            hf_outputs = hf_model.generate_encoder_decoder_greedy_logprobs_limit(prompts, max_tokens, num_logprobs, **hf_kwargs)
        hf_skip_tokens = 1 if decoder_prompt_type == DecoderPromptType.NONE else 0
        check_logprobs_close(outputs_0_lst=hf_outputs, outputs_1_lst=[aphrodite_to_hf_output(aphrodite_output, decoder_prompt_type) for aphrodite_output in aphrodite_outputs], name_0='hf', name_1='aphrodite', num_outputs_0_skip_tokens=hf_skip_tokens)
    @pytest.mark.parametrize('model', MODELS)
    @pytest.mark.parametrize('dtype', ['float', 'bfloat16'])
    @pytest.mark.parametrize('max_tokens', [64])
    @pytest.mark.parametrize('num_logprobs', [5])
    @pytest.mark.parametrize('decoder_prompt_type', list(DecoderPromptType))
    def test_models(hf_runner, aphrodite_runner, example_encoder_decoder_prompts, model, dtype, max_tokens, num_logprobs, decoder_prompt_type) -> None:
        run_test(hf_runner, aphrodite_runner, example_encoder_decoder_prompts[decoder_prompt_type], decoder_prompt_type, model, dtype=dtype, max_tokens=max_tokens, num_logprobs=num_logprobs, tensor_parallel_size=1)
    @multi_gpu_test(num_gpus=2)
    @pytest.mark.parametrize('distributed_executor_backend', ['ray', 'mp'])
    @pytest.mark.parametrize('model', ['facebook/bart-large-cnn'])
    @pytest.mark.parametrize('dtype', ['float'])
    @pytest.mark.parametrize('max_tokens', [64])
    @pytest.mark.parametrize('num_logprobs', [5])
    @pytest.mark.parametrize('decoder_prompt_type', [DecoderPromptType.CUSTOM])
    def test_models_distributed(hf_runner, aphrodite_runner, example_encoder_decoder_prompts, distributed_executor_backend, model, dtype, max_tokens, num_logprobs, decoder_prompt_type) -> None:
        run_test(hf_runner, aphrodite_runner, example_encoder_decoder_prompts[decoder_prompt_type], decoder_prompt_type, model, dtype=dtype, max_tokens=max_tokens, num_logprobs=num_logprobs, tensor_parallel_size=2, distributed_executor_backend=distributed_executor_backend)