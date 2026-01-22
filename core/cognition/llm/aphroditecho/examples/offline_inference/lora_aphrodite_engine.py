from typing import List, Optional, Tuple
from huggingface_hub import snapshot_download
from aphrodite import AphroditeEngine, EngineArgs, RequestOutput, SamplingParams
from aphrodite.lora.request import LoRARequest
def create_test_prompts(lora_path: str) -> List[Tuple[str, SamplingParams]]:
    return [('A robot may not injure a human being', SamplingParams(temperature=0.0, prompt_logprobs=1, max_tokens=128), None), ('To be or not to be,', SamplingParams(temperature=0.8, top_k=5, presence_penalty=0.2, max_tokens=128), None), ('[user] Write a SQL query to answer the question based on the\n            table schema.\n\n context: CREATE TABLE table_name_74\n            (icao VARCHAR, airport VARCHAR)\n\n\n            question: Name the ICAO for lilongwe\n            international airport [/user] [assistant]', SamplingParams(temperature=0.0, prompt_logprobs=1, max_tokens=128, stop_token_ids=[32003]), LoRARequest('l2-lora-test', 1, lora_path)), ('[user] Write a SQL query to answer the question based on the table\n         schema.\n\n context: CREATE TABLE table_name_11 (nationality VARCHAR,\n         elector VARCHAR)\n\n question: When Anchero Pantaleone was the elector\n         what is under nationality? [/user] [assistant]', SamplingParams(n=3, best_of=3, temperature=0.8, max_tokens=128, stop_token_ids=[32003]), LoRARequest('l2-lora-test', 1, lora_path)), ('[user] Write a SQL query to answer the question based on the\n            table schema.\n\n context: CREATE TABLE table_name_74 (icao\n            VARCHAR, airport VARCHAR)\n\n question: Name the ICAO for lilongwe\n            international airport [/user] [assistant]', SamplingParams(temperature=0.0, prompt_logprobs=1, max_tokens=128, stop_token_ids=[32003]), LoRARequest('l2-lora-test2', 2, lora_path)), ('[user] Write a SQL query to answer the question based on the table\n         schema.\n\n context: CREATE TABLE table_name_11 (nationality VARCHAR,\n         elector VARCHAR)\n\n question: When Anchero Pantaleone was the elector\n         what is under nationality? [/user] [assistant]', SamplingParams(n=3, best_of=3, temperature=0.9, max_tokens=128, stop_token_ids=[32003]), LoRARequest('l2-lora-test', 1, lora_path))]
def process_requests(engine: AphroditeEngine, test_prompts: List[Tuple[str, SamplingParams, Optional[LoRARequest]]]):
    request_id = 0
    while test_prompts or engine.has_unfinished_requests():
        if test_prompts:
            prompt, sampling_params, lora_request = test_prompts.pop(0)
            engine.add_request(str(request_id), prompt, sampling_params, lora_request=lora_request)
            request_id += 1
        request_outputs: List[RequestOutput] = engine.step()
        for request_output in request_outputs:
            if request_output.finished:
                print(request_output)
def initialize_engine() -> AphroditeEngine:
    engine_args = EngineArgs(model='NousResearch/Llama-2-7b-hf', enable_lora=True, max_loras=1, max_lora_rank=8, max_cpu_loras=2, max_num_seqs=256)
    return AphroditeEngine.from_engine_args(engine_args)
def main():
    engine = initialize_engine()
    lora_path = snapshot_download(repo_id='alpindale/l2-lora-test')
    test_prompts = create_test_prompts(lora_path)
    process_requests(engine, test_prompts)
if __name__ == '__main__':
    main()