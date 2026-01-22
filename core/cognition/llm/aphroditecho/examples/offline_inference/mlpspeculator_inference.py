import gc
import time
from typing import List
from aphrodite import LLM, SamplingParams
def time_generation(llm: LLM, prompts: List[str], sampling_params: SamplingParams):
    llm.generate(prompts, sampling_params)
    llm.generate(prompts, sampling_params)
    start = time.time()
    outputs = llm.generate(prompts, sampling_params)
    end = time.time()
    print((end - start) / sum([len(o.outputs[0].token_ids) for o in outputs]))
    for output in outputs:
        generated_text = output.outputs[0].text
        print(f'text: {generated_text!r}')
if __name__ == '__main__':
    template = 'Below is an instruction that describes a task. Write a response that appropriately completes the request.\n\n### Instruction:\n{}\n\n### Response:\n'
    prompts = ['Write about the president of the United States.']
    prompts = [template.format(prompt) for prompt in prompts]
    sampling_params = SamplingParams(temperature=0.0, max_tokens=200)
    llm = LLM(model='NousResearch/Meta-Llama-3.1-8B-Instruct', max_model_len=8192)
    print('Without speculation')
    time_generation(llm, prompts, sampling_params)
    del llm
    gc.collect()
    llm = LLM(model='NousResearch/Meta-Llama-3.1-8B-Instruct', speculative_model='ibm-fms/llama3-8b-accelerator', enforce_eager=True, max_model_len=8192)
    print('With speculation')
    time_generation(llm, prompts, sampling_params)