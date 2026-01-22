import base64
import io
import json
import logging
import random
from abc import ABC, abstractmethod
from collections.abc import Mapping
from dataclasses import dataclass
from functools import cache
from io import BytesIO
from typing import Any, Callable, Optional, Union
import numpy as np
from PIL import Image
from transformers import PreTrainedTokenizerBase
from aphrodite.utils import PlaceholderModule
from aphrodite.lora.request import LoRARequest
from aphrodite.lora.utils import get_adapter_absolute_path
from aphrodite.multimodal import MultiModalDataDict
from aphrodite.multimodal.image import convert_image_mode
from aphrodite.transformers_utils.tokenizer import AnyTokenizer, get_lora_tokenizer
try:
    from datasets import load_dataset
except ImportError:
    datasets = PlaceholderModule('datasets')
    load_dataset = datasets.placeholder_attr('load_dataset')
try:
    import pandas as pd
except ImportError:
    pd = PlaceholderModule('pandas')
try:
    import librosa
except ImportError:
    librosa = PlaceholderModule('librosa')
try:
    from aphrodite.utils import FlexibleArgumentParser
except ImportError:
    from argparse import ArgumentParser as FlexibleArgumentParser
logger = logging.getLogger(__name__)
@dataclass
class SampleRequest:
    prompt: Union[str, Any]
    prompt_len: int
    expected_output_len: int
    multi_modal_data: Optional[Union[MultiModalDataDict, dict]] = None
    lora_request: Optional[LoRARequest] = None
class BenchmarkDataset(ABC):
    DEFAULT_SEED = 0
    IS_MULTIMODAL = False
    def __init__(self, dataset_path: Optional[str]=None, random_seed: int=DEFAULT_SEED) -> None:
        self.dataset_path = dataset_path
        self.random_seed = random_seed if random_seed is not None else self.DEFAULT_SEED
        self.data = None
    def apply_multimodal_chat_transformation(self, prompt: str, mm_content: Optional[MultiModalDataDict]=None) -> list[dict]:
        content = [{'text': prompt, 'type': 'text'}]
        if mm_content is not None:
            content.append(mm_content)
        return [{'role': 'user', 'content': content}]
    def load_data(self) -> None:
        raise NotImplementedError('load_data must be implemented in subclasses.')
    def get_random_lora_request(self, tokenizer: PreTrainedTokenizerBase, max_loras: Optional[int]=None, lora_path: Optional[str]=None) -> tuple[Optional[LoRARequest], AnyTokenizer]:
        if max_loras is None or lora_path is None:
            return (None, tokenizer)
        lora_id = random.randint(1, max_loras)
        lora_request = LoRARequest(lora_name=str(lora_id), lora_int_id=lora_id, lora_path=lora_path_on_disk(lora_path))
        if lora_id not in lora_tokenizer_cache:
            lora_tokenizer_cache[lora_id] = get_lora_tokenizer(lora_request)
        return (lora_request, lora_tokenizer_cache[lora_id] or tokenizer)
    @abstractmethod
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int) -> list[SampleRequest]:
        raise NotImplementedError('sample must be implemented in subclasses.')
    def maybe_oversample_requests(self, requests: list[SampleRequest], num_requests: int) -> None:
        if len(requests) < num_requests:
            random.seed(self.random_seed)
            additional = random.choices(requests, k=num_requests - len(requests))
            requests.extend(additional)
            logger.info('Oversampled requests to reach {} total samples.', num_requests)
def is_valid_sequence(prompt_len: int, output_len: int, min_len: int=4, max_prompt_len: int=1024, max_total_len: int=2048, skip_min_output_len_check: bool=False) -> bool:
    prompt_too_short = prompt_len < min_len
    output_too_short = not skip_min_output_len_check and output_len < min_len
    prompt_too_long = prompt_len > max_prompt_len
    combined_too_long = prompt_len + output_len > max_total_len
    return not (prompt_too_short or output_too_short or prompt_too_long or combined_too_long)
@cache
def lora_path_on_disk(lora_path: str) -> str:
    return get_adapter_absolute_path(lora_path)
lora_tokenizer_cache: dict[int, AnyTokenizer] = {}
def process_image(image: Any) -> Mapping[str, Any]:
    if isinstance(image, dict) and 'bytes' in image:
        image = Image.open(BytesIO(image['bytes']))
    if isinstance(image, Image.Image):
        image = convert_image_mode(image, 'RGB')
        with io.BytesIO() as image_data:
            image.save(image_data, format='JPEG')
            image_base64 = base64.b64encode(image_data.getvalue()).decode('utf-8')
        return {'type': 'image_url', 'image_url': {'url': f'data:image/jpeg;base64,{image_base64}'}}
    if isinstance(image, str):
        image_url = image if image.startswith(('http://', 'file://')) else f'file://{image}'
        return {'type': 'image_url', 'image_url': {'url': image_url}}
    raise ValueError(f'Invalid image input {image}. Must be a PIL.Image.Image or str or dictionary with raw image bytes.')
class RandomDataset(BenchmarkDataset):
    DEFAULT_PREFIX_LEN = 0
    DEFAULT_RANGE_RATIO = 0.0
    DEFAULT_INPUT_LEN = 1024
    DEFAULT_OUTPUT_LEN = 128
    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        random.seed(self.random_seed)
        np.random.seed(self.random_seed)
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, prefix_len: int=DEFAULT_PREFIX_LEN, range_ratio: float=DEFAULT_RANGE_RATIO, input_len: int=DEFAULT_INPUT_LEN, output_len: int=DEFAULT_OUTPUT_LEN, **kwargs) -> list[SampleRequest]:
        assert range_ratio < 1.0, 'random_range_ratio must be < 1.0 to ensure a valid sampling range'
        vocab_size = tokenizer.vocab_size
        num_special_tokens = tokenizer.num_special_tokens_to_add()
        real_input_len = input_len - num_special_tokens
        prefix_token_ids = np.random.randint(0, vocab_size, size=prefix_len).tolist() if prefix_len > 0 else []
        input_low = int(real_input_len * (1 - range_ratio))
        input_high = int(real_input_len * (1 + range_ratio))
        output_low = int(output_len * (1 - range_ratio))
        output_high = int(output_len * (1 + range_ratio))
        logger.info('Sampling input_len from [{}, {}] and output_len from [{}, {}]', input_low, input_high, output_low, output_high)
        input_lens = np.random.randint(input_low, input_high + 1, size=num_requests)
        output_lens = np.random.randint(output_low, output_high + 1, size=num_requests)
        offsets = np.random.randint(0, vocab_size, size=num_requests)
        requests = []
        for i in range(num_requests):
            inner_seq = ((offsets[i] + i + np.arange(input_lens[i])) % vocab_size).tolist()
            token_sequence = prefix_token_ids + inner_seq
            prompt = tokenizer.decode(token_sequence)
            total_input_len = prefix_len + int(input_lens[i])
            re_encoded_sequence = tokenizer.encode(prompt, add_special_tokens=False)[:total_input_len]
            prompt = tokenizer.decode(re_encoded_sequence)
            total_input_len = len(re_encoded_sequence)
            requests.append(SampleRequest(prompt=prompt, prompt_len=total_input_len, expected_output_len=int(output_lens[i])))
        return requests
class ShareGPTDataset(BenchmarkDataset):
    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        self.load_data()
    def load_data(self) -> None:
        if self.dataset_path is None:
            raise ValueError('dataset_path must be provided for loading data.')
        with open(self.dataset_path, encoding='utf-8') as f:
            self.data = json.load(f)
        self.data = [entry for entry in self.data if 'conversations' in entry and len(entry['conversations']) >= 2]
        random.seed(self.random_seed)
        random.shuffle(self.data)
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, lora_path: Optional[str]=None, max_loras: Optional[int]=None, output_len: Optional[int]=None, enable_multimodal_chat: bool=False, **kwargs) -> list:
        samples: list = []
        for entry in self.data:
            if len(samples) >= num_requests:
                break
            prompt, completion = (entry['conversations'][0]['value'], entry['conversations'][1]['value'])
            lora_request, tokenizer = self.get_random_lora_request(tokenizer=tokenizer, max_loras=max_loras, lora_path=lora_path)
            prompt_ids = tokenizer(prompt).input_ids
            completion_ids = tokenizer(completion).input_ids
            prompt_len = len(prompt_ids)
            new_output_len = len(completion_ids) if output_len is None else output_len
            if not is_valid_sequence(prompt_len, new_output_len, skip_min_output_len_check=output_len is not None):
                continue
            if enable_multimodal_chat:
                prompt = self.apply_multimodal_chat_transformation(prompt, None)
            samples.append(SampleRequest(prompt=prompt, prompt_len=prompt_len, expected_output_len=new_output_len, lora_request=lora_request))
        self.maybe_oversample_requests(samples, num_requests)
        return samples
def add_dataset_parser(parser: FlexibleArgumentParser):
    parser.add_argument('--seed', type=int, default=0)
    parser.add_argument('--num-prompts', type=int, default=1000, help='Number of prompts to process.')
    parser.add_argument('--dataset-name', type=str, default='random', choices=['sharegpt', 'burstgpt', 'sonnet', 'random', 'hf', 'custom'], help='Name of the dataset to benchmark on.')
    parser.add_argument('--no-stream', action='store_true', help='Do not load the dataset in streaming mode.')
    parser.add_argument('--dataset-path', type=str, default=None, help='Path to the sharegpt/sonnet dataset. Or the huggingface dataset ID if using HF dataset.')
    custom_group = parser.add_argument_group('custom dataset options')
    custom_group.add_argument('--custom-output-len', type=int, default=256, help='Number of output tokens per request, used only for custom dataset.')
    custom_group.add_argument('--custom-skip-chat-template', action='store_true', help='Skip applying chat template to prompt, used only for custom dataset.')
    sonnet_group = parser.add_argument_group('sonnet dataset options')
    sonnet_group.add_argument('--sonnet-input-len', type=int, default=550, help='Number of input tokens per request, used only for sonnet dataset.')
    sonnet_group.add_argument('--sonnet-output-len', type=int, default=150, help='Number of output tokens per request, used only for sonnet dataset.')
    sonnet_group.add_argument('--sonnet-prefix-len', type=int, default=200, help='Number of prefix tokens per request, used only for sonnet dataset.')
    sharegpt_group = parser.add_argument_group('sharegpt dataset options')
    sharegpt_group.add_argument('--sharegpt-output-len', type=int, default=None, help='Output length for each request. Overrides the output length from the ShareGPT dataset.')
    random_group = parser.add_argument_group('random dataset options')
    random_group.add_argument('--random-input-len', type=int, default=1024, help='Number of input tokens per request, used only for random sampling.')
    random_group.add_argument('--random-output-len', type=int, default=128, help='Number of output tokens per request, used only for random sampling.')
    random_group.add_argument('--random-range-ratio', type=float, default=0.0, help='Range ratio for sampling input/output length, used only for random sampling. Must be in the range [0, 1) to define a symmetric sampling range[length * (1 - range_ratio), length * (1 + range_ratio)].')
    random_group.add_argument('--random-prefix-len', type=int, default=0, help='Number of fixed prefix tokens before the random context in a request. The total input length is the sum of `random-prefix-len` and a random context length sampled from [input_len * (1 - range_ratio), input_len * (1 + range_ratio)].')
    hf_group = parser.add_argument_group('hf dataset options')
    hf_group.add_argument('--hf-subset', type=str, default=None, help='Subset of the HF dataset.')
    hf_group.add_argument('--hf-split', type=str, default=None, help='Split of the HF dataset.')
    hf_group.add_argument('--hf-output-len', type=int, default=None, help='Output length for each request. Overrides the output lengths from the sampled HF dataset.')
def get_samples(args, tokenizer) -> list[SampleRequest]:
    if args.dataset_name == 'custom':
        dataset = CustomDataset(dataset_path=args.dataset_path)
        input_requests = dataset.sample(num_requests=args.num_prompts, tokenizer=tokenizer, output_len=args.custom_output_len, skip_chat_template=args.custom_skip_chat_template)
    elif args.dataset_name == 'sonnet':
        dataset = SonnetDataset(dataset_path=args.dataset_path)
        if args.endpoint_type == 'openai-chat':
            input_requests = dataset.sample(num_requests=args.num_prompts, input_len=args.sonnet_input_len, output_len=args.sonnet_output_len, prefix_len=args.sonnet_prefix_len, tokenizer=tokenizer, return_prompt_formatted=False)
        else:
            assert tokenizer.chat_template or tokenizer.default_chat_template, 'Tokenizer/model must have chat template for sonnet dataset.'
            input_requests = dataset.sample(num_requests=args.num_prompts, input_len=args.sonnet_input_len, output_len=args.sonnet_output_len, prefix_len=args.sonnet_prefix_len, tokenizer=tokenizer, return_prompt_formatted=True)
    elif args.dataset_name == 'hf':
        if args.dataset_path in VisionArenaDataset.SUPPORTED_DATASET_PATHS:
            dataset_class = VisionArenaDataset
            args.hf_split = 'train'
            args.hf_subset = None
        elif args.dataset_path in InstructCoderDataset.SUPPORTED_DATASET_PATHS:
            dataset_class = InstructCoderDataset
            args.hf_split = 'train'
        elif args.dataset_path in MTBenchDataset.SUPPORTED_DATASET_PATHS:
            dataset_class = MTBenchDataset
            args.hf_split = 'train'
        elif args.dataset_path in ConversationDataset.SUPPORTED_DATASET_PATHS:
            dataset_class = ConversationDataset
        elif args.dataset_path in AIMODataset.SUPPORTED_DATASET_PATHS:
            dataset_class = AIMODataset
            args.hf_split = 'train'
        elif args.dataset_path in NextEditPredictionDataset.SUPPORTED_DATASET_PATHS:
            dataset_class = NextEditPredictionDataset
            args.hf_split = 'train'
        elif args.dataset_path in ASRDataset.SUPPORTED_DATASET_PATHS:
            dataset_class = ASRDataset
            args.hf_split = 'train'
        elif args.dataset_path in MLPerfDataset.SUPPORTED_DATASET_PATHS:
            dataset_class = MLPerfDataset
            args.hf_split = 'train'
        else:
            supported_datasets = set([dataset_name for cls in HuggingFaceDataset.__subclasses__() for dataset_name in cls.SUPPORTED_DATASET_PATHS])
            raise ValueError(f'Unsupported dataset path: {args.dataset_path}. Huggingface dataset only supports dataset_path from one of following: {supported_datasets}. Please consider contributing if you would like to add support for additional dataset formats.')
        if dataset_class.IS_MULTIMODAL and args.endpoint_type not in ['openai-chat', 'openai-audio']:
            raise ValueError("Multi-modal content is only supported on 'openai-chat' and 'openai-audio' backend.")
        input_requests = dataset_class(dataset_path=args.dataset_path, dataset_subset=args.hf_subset, dataset_split=args.hf_split, random_seed=args.seed, no_stream=args.no_stream).sample(num_requests=args.num_prompts, tokenizer=tokenizer, output_len=args.hf_output_len)
    else:
        dataset_mapping = {'sharegpt': lambda: ShareGPTDataset(random_seed=args.seed, dataset_path=args.dataset_path).sample(tokenizer=tokenizer, num_requests=args.num_prompts, output_len=args.sharegpt_output_len), 'burstgpt': lambda: BurstGPTDataset(random_seed=args.seed, dataset_path=args.dataset_path).sample(tokenizer=tokenizer, num_requests=args.num_prompts), 'random': lambda: RandomDataset(random_seed=args.seed, dataset_path=args.dataset_path).sample(tokenizer=tokenizer, num_requests=args.num_prompts, prefix_len=args.random_prefix_len, input_len=args.random_input_len, output_len=args.random_output_len, range_ratio=args.random_range_ratio)}
        try:
            input_requests = dataset_mapping[args.dataset_name]()
        except KeyError as err:
            raise ValueError(f'Unknown dataset: {args.dataset_name}') from err
    return input_requests
class CustomDataset(BenchmarkDataset):
    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        self.load_data()
    def load_data(self) -> None:
        if self.dataset_path is None:
            raise ValueError('dataset_path must be provided for loading data.')
        self.data = []
        if self.dataset_path.endswith('.jsonl'):
            jsonl_data = pd.read_json(path_or_buf=self.dataset_path, lines=True)
            if 'prompt' not in jsonl_data.columns:
                raise ValueError("JSONL file must contain a 'prompt' column.")
            for _, row in jsonl_data.iterrows():
                self.data.append(row.to_dict())
        else:
            raise NotImplementedError('Only JSONL format is supported for CustomDataset.')
        random.seed(self.random_seed)
        random.shuffle(self.data)
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, lora_path: Optional[str]=None, max_loras: Optional[int]=None, output_len: Optional[int]=None, enable_multimodal_chat: bool=False, skip_chat_template: bool=False, **kwargs) -> list:
        sampled_requests = []
        for item in self.data:
            if len(sampled_requests) >= num_requests:
                break
            prompt = item['prompt']
            if not skip_chat_template:
                prompt = tokenizer.apply_chat_template([{'role': 'user', 'content': prompt}], add_generation_prompt=True, tokenize=False)
            prompt_len = len(tokenizer(prompt).input_ids)
            sampled_requests.append(SampleRequest(prompt=prompt, prompt_len=prompt_len, expected_output_len=output_len))
        self.maybe_oversample_requests(sampled_requests, num_requests)
        return sampled_requests
class SonnetDataset(BenchmarkDataset):
    DEFAULT_PREFIX_LEN = 200
    DEFAULT_INPUT_LEN = 550
    DEFAULT_OUTPUT_LEN = 150
    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        self.load_data()
    def load_data(self) -> None:
        if not self.dataset_path:
            raise ValueError('dataset_path must be provided.')
        with open(self.dataset_path, encoding='utf-8') as f:
            self.data = f.readlines()
    def sample(self, tokenizer, num_requests: int, prefix_len: int=DEFAULT_PREFIX_LEN, input_len: int=DEFAULT_INPUT_LEN, output_len: int=DEFAULT_OUTPUT_LEN, return_prompt_formatted: bool=False, **kwargs) -> list:
        tokenized_lines = [tokenizer(line).input_ids for line in self.data]
        avg_len = sum((len(tokens) for tokens in tokenized_lines)) / len(tokenized_lines)
        base_prompt = 'Pick as many lines as you can from these poem lines:\n'
        base_msg = [{'role': 'user', 'content': base_prompt}]
        base_fmt = tokenizer.apply_chat_template(base_msg, add_generation_prompt=True, tokenize=False)
        base_offset = len(tokenizer(base_fmt).input_ids)
        if input_len <= base_offset:
            raise ValueError(f"'input_len' must be higher than the base prompt length ({base_offset}).")
        num_input_lines = round((input_len - base_offset) / avg_len)
        num_prefix_lines = max(round((prefix_len - base_offset) / avg_len), 0)
        prefix_lines = self.data[:num_prefix_lines]
        samples = []
        while len(samples) < num_requests:
            extra_lines = random.choices(self.data, k=num_input_lines - num_prefix_lines)
            prompt = f"{base_prompt}{''.join(prefix_lines + extra_lines)}"
            msg = [{'role': 'user', 'content': prompt}]
            prompt_formatted = tokenizer.apply_chat_template(msg, add_generation_prompt=True, tokenize=False)
            prompt_len = len(tokenizer(prompt_formatted).input_ids)
            if prompt_len <= input_len:
                samples.append(SampleRequest(prompt=prompt_formatted if return_prompt_formatted else prompt, prompt_len=prompt_len, expected_output_len=output_len))
        return samples
class BurstGPTDataset(BenchmarkDataset):
    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        self.load_data()
    def load_data(self):
        if self.dataset_path is None:
            raise ValueError('dataset_path must be provided for loading data.')
        df = pd.read_csv(self.dataset_path)
        gpt4_df = df[df['Model'] == 'GPT-4']
        gpt4_df = gpt4_df[gpt4_df['Response tokens'] > 0]
        self.data = gpt4_df
    def _sample_loaded_data(self, num_requests: int) -> list:
        if num_requests <= len(self.data):
            data = self.data.sample(n=num_requests, random_state=self.random_seed)
        else:
            data = self.data.sample(n=num_requests, random_state=self.random_seed, replace=True)
        return data.values.tolist()
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, max_loras: Optional[int]=None, lora_path: Optional[str]=None, **kwargs) -> list[SampleRequest]:
        samples = []
        data = self._sample_loaded_data(num_requests=num_requests)
        for i in range(num_requests):
            input_len = int(data[i][2])
            output_len = int(data[i][3])
            lora_req, tokenizer = self.get_random_lora_request(tokenizer=tokenizer, max_loras=max_loras, lora_path=lora_path)
            vocab_size = tokenizer.vocab_size
            token_ids = [(i + j) % vocab_size for j in range(input_len)]
            prompt = tokenizer.decode(token_ids)
            samples.append(SampleRequest(prompt=prompt, prompt_len=input_len, expected_output_len=output_len, lora_request=lora_req))
        return samples
class HuggingFaceDataset(BenchmarkDataset):
    SUPPORTED_DATASET_PATHS: Union[set[str], dict[str, Callable]] = set()
    def __init__(self, dataset_path: str, dataset_split: str, no_stream: bool=False, dataset_subset: Optional[str]=None, **kwargs) -> None:
        super().__init__(dataset_path=dataset_path, **kwargs)
        self.dataset_split = dataset_split
        self.dataset_subset = dataset_subset
        self.load_stream = not no_stream
        self.load_data()
    def load_data(self) -> None:
        self.data = load_dataset(self.dataset_path, name=self.dataset_subset, split=self.dataset_split, streaming=self.load_stream)
        self.data = self.data.shuffle(seed=self.random_seed)
class ConversationDataset(HuggingFaceDataset):
    SUPPORTED_DATASET_PATHS = {'lmms-lab/LLaVA-OneVision-Data', 'Aeala/ShareGPT_Vicuna_unfiltered'}
    IS_MULTIMODAL = True
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, output_len: Optional[int]=None, enable_multimodal_chat: bool=False, **kwargs) -> list:
        filtered_data = self.data.filter(lambda x: len(x['conversations']) >= 2)
        sampled_requests = []
        dynamic_output = output_len is None
        for item in filtered_data:
            if len(sampled_requests) >= num_requests:
                break
            conv = item['conversations']
            prompt, completion = (conv[0]['value'], conv[1]['value'])
            prompt_ids = tokenizer(prompt).input_ids
            completion_ids = tokenizer(completion).input_ids
            prompt_len = len(prompt_ids)
            completion_len = len(completion_ids)
            output_len = completion_len if dynamic_output else output_len
            assert isinstance(output_len, int) and output_len > 0
            if dynamic_output and (not is_valid_sequence(prompt_len, completion_len)):
                continue
            mm_content = process_image(item['image']) if 'image' in item else None
            if enable_multimodal_chat:
                prompt = self.apply_multimodal_chat_transformation(prompt, mm_content)
            sampled_requests.append(SampleRequest(prompt=prompt, prompt_len=prompt_len, expected_output_len=output_len, multi_modal_data=mm_content))
        self.maybe_oversample_requests(sampled_requests, num_requests)
        return sampled_requests
class VisionArenaDataset(HuggingFaceDataset):
    DEFAULT_OUTPUT_LEN = 128
    SUPPORTED_DATASET_PATHS = {'lmarena-ai/VisionArena-Chat': lambda x: x['conversation'][0][0]['content'], 'lmarena-ai/vision-arena-bench-v0.1': lambda x: x['turns'][0][0]['content']}
    IS_MULTIMODAL = True
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, output_len: Optional[int]=None, enable_multimodal_chat: bool=False, **kwargs) -> list:
        output_len = output_len if output_len is not None else self.DEFAULT_OUTPUT_LEN
        sampled_requests = []
        for item in self.data:
            if len(sampled_requests) >= num_requests:
                break
            parser_fn = self.SUPPORTED_DATASET_PATHS.get(self.dataset_path)
            if parser_fn is None:
                raise ValueError(f'Unsupported dataset path: {self.dataset_path}')
            prompt = parser_fn(item)
            mm_content = process_image(item['images'][0])
            prompt_len = len(tokenizer(prompt).input_ids)
            if enable_multimodal_chat:
                prompt = self.apply_multimodal_chat_transformation(prompt, mm_content)
            sampled_requests.append(SampleRequest(prompt=prompt, prompt_len=prompt_len, expected_output_len=output_len, multi_modal_data=mm_content))
        self.maybe_oversample_requests(sampled_requests, num_requests)
        return sampled_requests
class InstructCoderDataset(HuggingFaceDataset):
    DEFAULT_OUTPUT_LEN = 200
    SUPPORTED_DATASET_PATHS = {'likaixin/InstructCoder'}
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, output_len: Optional[int]=None, enable_multimodal_chat: bool=False, **kwargs) -> list:
        output_len = output_len if output_len is not None else self.DEFAULT_OUTPUT_LEN
        sampled_requests = []
        for item in self.data:
            if len(sampled_requests) >= num_requests:
                break
            prompt = f"{item['input']}\n\n{item['instruction']} Just output             the code, do not include any explanation."
            prompt = tokenizer.apply_chat_template([{'role': 'user', 'content': prompt}], add_generation_prompt=True, tokenize=False)
            prompt_len = len(tokenizer(prompt).input_ids)
            sampled_requests.append(SampleRequest(prompt=prompt, prompt_len=prompt_len, expected_output_len=output_len))
        self.maybe_oversample_requests(sampled_requests, num_requests)
        return sampled_requests
class MTBenchDataset(HuggingFaceDataset):
    DEFAULT_OUTPUT_LEN = 256
    SUPPORTED_DATASET_PATHS = {'philschmid/mt-bench'}
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, output_len: Optional[int]=None, enable_multimodal_chat: bool=False, **kwargs) -> list:
        output_len = output_len if output_len is not None else self.DEFAULT_OUTPUT_LEN
        sampled_requests = []
        for item in self.data:
            if len(sampled_requests) >= num_requests:
                break
            prompt = item['turns'][0]
            prompt = tokenizer.apply_chat_template([{'role': 'user', 'content': prompt}], add_generation_prompt=True, tokenize=False)
            prompt_len = len(tokenizer(prompt).input_ids)
            sampled_requests.append(SampleRequest(prompt=prompt, prompt_len=prompt_len, expected_output_len=output_len))
        self.maybe_oversample_requests(sampled_requests, num_requests)
        return sampled_requests
class AIMODataset(HuggingFaceDataset):
    SUPPORTED_DATASET_PATHS = {'AI-MO/aimo-validation-aime', 'AI-MO/NuminaMath-1.5', 'AI-MO/NuminaMath-CoT'}
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, output_len: Optional[int]=None, **kwargs) -> list:
        sampled_requests = []
        dynamic_output = output_len is None
        for item in self.data:
            if len(sampled_requests) >= num_requests:
                break
            prompt, completion = (item['problem'], item['solution'])
            prompt_ids = tokenizer(prompt).input_ids
            completion_ids = tokenizer(completion).input_ids
            prompt_len = len(prompt_ids)
            completion_len = len(completion_ids)
            output_len = completion_len if dynamic_output else output_len
            assert isinstance(output_len, int) and output_len > 0
            if dynamic_output and (not is_valid_sequence(prompt_len, completion_len, max_prompt_len=2048, max_total_len=32000)):
                continue
            sampled_requests.append(SampleRequest(prompt=prompt, prompt_len=prompt_len, expected_output_len=output_len, multi_modal_data=None))
        self.maybe_oversample_requests(sampled_requests, num_requests)
        return sampled_requests
zeta_prompt = '### Instruction:\nYou are a code completion assistant and your task is to analyze user edits and then rewrite an excerpt that the user provides, suggesting the appropriate edits within the excerpt, taking into account the cursor location.\n\n### User Edits:\n\n{}\n\n### User Excerpt:\n\n{}\n\n### Response:\n\n'
def _format_zeta_prompt(sample: dict, original_start_marker: str='<|editable_region_start|>') -> dict:
    events = sample['events']
    input = sample['input']
    output = sample['output']
    prompt = zeta_prompt.format(events, input)
    output_start_index = output.find(original_start_marker)
    output_focused_region = output[output_start_index:]
    expected_output = output_focused_region
    return {'prompt': prompt, 'expected_output': expected_output}
class NextEditPredictionDataset(HuggingFaceDataset):
    SUPPORTED_DATASET_PATHS = {'zed-industries/zeta'}
    MAPPING_PROMPT_FUNCS = {'zed-industries/zeta': _format_zeta_prompt}
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, **kwargs):
        formatting_prompt_func = self.MAPPING_PROMPT_FUNCS.get(self.dataset_path)
        if formatting_prompt_func is None:
            raise ValueError(f'Unsupported dataset path: {self.dataset_path}')
        samples = []
        for sample in self.data:
            sample = formatting_prompt_func(sample)
            samples.append(SampleRequest(prompt=sample['prompt'], prompt_len=len(tokenizer(sample['prompt']).input_ids), expected_output_len=len(tokenizer(sample['expected_output']).input_ids)))
            if len(samples) >= num_requests:
                break
        self.maybe_oversample_requests(samples, num_requests)
        return samples
class ASRDataset(HuggingFaceDataset):
    SUPPORTED_DATASET_PATHS = {'openslr/librispeech_asr', 'facebook/voxpopuli', 'LIUM/tedlium', 'edinburghcstr/ami', 'speechcolab/gigaspeech', 'kensho/spgispeech'}
    DEFAULT_OUTPUT_LEN = 128
    IS_MULTIMODAL = True
    TRANSCRIPTION_PREAMBLE = '<|startoftranscript|><|en|><|transcribe|><|notimestamps|>'
    skip_long_audios: bool = True
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, output_len: Optional[int]=None, **kwargs) -> list:
        output_len = output_len if output_len is not None else self.DEFAULT_OUTPUT_LEN
        prompt = ASRDataset.TRANSCRIPTION_PREAMBLE
        prompt_len = len(tokenizer(prompt).input_ids)
        sampled_requests = []
        skipped = 0
        for item in self.data:
            if len(sampled_requests) >= num_requests:
                break
            audio = item['audio']
            y, sr = (audio['array'], audio['sampling_rate'])
            duration_s = librosa.get_duration(y=y, sr=sr)
            if self.skip_long_audios and duration_s > 30:
                skipped += 1
                continue
            mm_content = {'audio': (y, sr)}
            sampled_requests.append(SampleRequest(prompt=prompt, prompt_len=prompt_len, expected_output_len=output_len, multi_modal_data=mm_content))
        if skipped:
            logger.warning('{} samples discarded from dataset due to their length being greater than what Whisper supports.', skipped)
        self.maybe_oversample_requests(sampled_requests, num_requests)
        return sampled_requests
class MLPerfDataset(HuggingFaceDataset):
    SUPPORTED_DATASET_PATHS = {'mgoin/mlperf-inference-llama2-data', 'mgoin/mlperf-inference-llama3.1-data'}
    def sample(self, tokenizer: PreTrainedTokenizerBase, num_requests: int, output_len: Optional[int]=None, **kwargs) -> list[SampleRequest]:
        dynamic_output = output_len is None
        sampled_requests: list[SampleRequest] = []
        for item in self.data:
            if len(sampled_requests) >= num_requests:
                break
            system_prompt = item['system_prompt']
            question = item['question']
            reference_answer = item['output']
            messages = [{'role': 'system', 'content': system_prompt}, {'role': 'user', 'content': question}]
            prompt_formatted = tokenizer.apply_chat_template(messages, add_generation_prompt=True, tokenize=False)
            prompt_len = len(tokenizer(prompt_formatted).input_ids)
            ref_out_len = len(tokenizer(reference_answer, add_special_tokens=False).input_ids)
            expected_output_len = ref_out_len if dynamic_output else output_len
            if not is_valid_sequence(prompt_len, expected_output_len):
                continue
            sampled_requests.append(SampleRequest(prompt=prompt_formatted, prompt_len=prompt_len, expected_output_len=expected_output_len))
        self.maybe_oversample_requests(sampled_requests, num_requests)
        return sampled_requests