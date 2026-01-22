from typing import Any, Dict, List
import numpy as np
import ray
from packaging.version import Version
from ray.util.scheduling_strategies import PlacementGroupSchedulingStrategy
from aphrodite import LLM, SamplingParams
assert Version(ray.__version__) >= Version('2.22.0'), 'Ray version must be at least 2.22.0'
sampling_params = SamplingParams(temperature=0.8, top_p=0.95)
tensor_parallel_size = 1
num_instances = 1
class LLMPredictor:
    def __init__(self):
        self.llm = LLM(model='NousResearch/Meta-Llama-3.1-8B-Instruct', tensor_parallel_size=tensor_parallel_size)
    def __call__(self, batch: Dict[str, np.ndarray]) -> Dict[str, list]:
        outputs = self.llm.generate(batch['text'], sampling_params)
        prompt: List[str] = []
        generated_text: List[str] = []
        for output in outputs:
            prompt.append(output.prompt)
            generated_text.append(' '.join([o.text for o in output.outputs]))
        return {'prompt': prompt, 'generated_text': generated_text}
ds = ray.data.read_text('s3://anonymous@air-example-data/prompts.txt')
def scheduling_strategy_fn():
    pg = ray.util.placement_group([{'GPU': 1, 'CPU': 1}] * tensor_parallel_size, strategy='STRICT_PACK')
    return dict(scheduling_strategy=PlacementGroupSchedulingStrategy(pg, placement_group_capture_child_tasks=True))
resources_kwarg: Dict[str, Any] = {}
if tensor_parallel_size == 1:
    resources_kwarg['num_gpus'] = 1
else:
    resources_kwarg['num_gpus'] = 0
    resources_kwarg['ray_remote_args_fn'] = scheduling_strategy_fn
ds = ds.map_batches(LLMPredictor, concurrency=num_instances, batch_size=32, **resources_kwarg)
outputs = ds.take(limit=10)
for output in outputs:
    prompt = output['prompt']
    generated_text = output['generated_text']
    print(f'Prompt: {prompt!r}, Generated text: {generated_text!r}')