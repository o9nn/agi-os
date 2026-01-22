from aphrodite import LLM, SamplingParams
llm = LLM(model='NousResearch/Meta-Llama-3.1-8B-Instruct')
sampling_params = SamplingParams(temperature=0.5)
def print_outputs(outputs):
    for output in outputs:
        prompt = output.prompt
        generated_text = output.outputs[0].text
        print(f'Prompt: {prompt!r}, Generated text: {generated_text!r}')
    print('-' * 80)
print('=' * 80)
conversation = [{'role': 'system', 'content': 'You are a helpful assistant'}, {'role': 'user', 'content': 'Hello'}, {'role': 'assistant', 'content': 'Hello! How can I assist you today?'}, {'role': 'user', 'content': 'Write an essay about the importance of higher education.'}]
outputs = llm.chat(conversation, sampling_params=sampling_params, use_tqdm=False)
print_outputs(outputs)
conversation = [{'role': 'system', 'content': 'You are a helpful assistant'}, {'role': 'user', 'content': 'Hello'}, {'role': 'assistant', 'content': 'Hello! How can I assist you today?'}, {'role': 'user', 'content': 'Write an essay about the importance of higher education.'}]
conversations = [conversation for _ in range(10)]
outputs = llm.chat(messages=conversations, sampling_params=sampling_params, use_tqdm=True)
print_outputs(outputs)