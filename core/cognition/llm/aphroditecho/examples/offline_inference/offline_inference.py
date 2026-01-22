from aphrodite import LLM, SamplingParams
prompts = ['Once upon a time,', 'In a galaxy far, far away,', 'The quick brown fox jumps over the lazy dog.', 'The meaning of life is']
sampling_params = SamplingParams(temperature=1.15, min_p=0.06)
llm = LLM(model='NousResearch/Meta-Llama-3.1-8B-Instruct', max_model_len=8192)
outputs = llm.generate(prompts, sampling_params)
for output in outputs:
    prompt = output.prompt
    generated_text = output.outputs[0].text
    print(f'Prompt: {prompt!r}, Generated text: {generated_text!r}')