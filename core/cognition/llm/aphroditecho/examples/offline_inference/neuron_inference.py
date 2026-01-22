import os
from aphrodite import LLM, SamplingParams
os.environ['NEURON_CONTEXT_LENGTH_BUCKETS'] = '128,512,1024,2048'
os.environ['NEURON_TOKEN_GEN_BUCKETS'] = '128,512,1024,2048'
prompts = ['Once upon a time,', 'In a galaxy far, far away,', 'The quick brown fox jumps over the lazy dog.', 'The meaning of life is']
sampling_params = SamplingParams(temperature=0.8, top_p=0.95)
llm = LLM(model='TinyLlama/TinyLlama-1.1B-Chat-v1.0', max_num_seqs=8, max_model_len=2048, block_size=2048, device='neuron', tensor_parallel_size=2)
outputs = llm.generate(prompts, sampling_params)
for output in outputs:
    prompt = output.prompt
    generated_text = output.outputs[0].text
    print(f'Prompt: {prompt!r}, Generated text: {generated_text!r}')