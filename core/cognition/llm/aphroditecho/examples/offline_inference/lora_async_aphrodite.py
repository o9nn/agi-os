import asyncio
from typing import List, Optional, Tuple
from aphrodite import AsyncAphrodite, AsyncEngineArgs, SamplingParams
from aphrodite.lora.request import LoRARequest
def create_test_prompts(lora_path: str) -> List[Tuple[str, SamplingParams, Optional[LoRARequest]]]:
    return [('A robot may not injure a human being', SamplingParams(temperature=0.0, prompt_logprobs=1, max_tokens=128), None), ('To be or not to be,', SamplingParams(temperature=0.8, top_k=5, presence_penalty=0.2, max_tokens=128), None), ('[user] Write a SQL query to answer the question based on the\n            table schema.\n\n context: CREATE TABLE table_name_74\n            (icao VARCHAR, airport VARCHAR)\n\n\n            question: Name the ICAO for lilongwe\n            international airport [/user] [assistant]', SamplingParams(temperature=0.0, prompt_logprobs=1, max_tokens=128, stop_token_ids=[32003]), LoRARequest(lora_name='l2-lora-test', lora_int_id=1, lora_path=lora_path)), ('[user] Write a SQL query to answer the question based on the table\n         schema.\n\n context: CREATE TABLE table_name_11 (nationality VARCHAR,\n         elector VARCHAR)\n\n question: When Anchero Pantaleone was the elector\n         what is under nationality? [/user] [assistant]', SamplingParams(n=3, best_of=3, temperature=0.8, max_tokens=128, stop_token_ids=[32003]), LoRARequest(lora_name='l2-lora-test', lora_int_id=1, lora_path=lora_path)), ('[user] Write a SQL query to answer the question based on the\n            table schema.\n\n context: CREATE TABLE table_name_74 (icao\n            VARCHAR, airport VARCHAR)\n\n question: Name the ICAO for lilongwe\n            international airport [/user] [assistant]', SamplingParams(temperature=0.0, prompt_logprobs=1, max_tokens=128, stop_token_ids=[32003]), LoRARequest(lora_name='l2-lora-test2', lora_int_id=2, lora_path=lora_path)), ('[user] Write a SQL query to answer the question based on the table\n         schema.\n\n context: CREATE TABLE table_name_11 (nationality VARCHAR,\n         elector VARCHAR)\n\n question: When Anchero Pantaleone was the elector\n         what is under nationality? [/user] [assistant]', SamplingParams(n=3, best_of=3, temperature=0.9, max_tokens=128, stop_token_ids=[32003]), LoRARequest(lora_name='l2-lora-test', lora_int_id=1, lora_path=lora_path))]
async def process_requests(engine: AsyncAphrodite, test_prompts: List[Tuple[str, SamplingParams, Optional[LoRARequest]]]):
    request_id = 0
    active_requests = []
    for prompt, sampling_params, lora_request in test_prompts:
        request_generator = engine.generate(prompt, sampling_params, str(request_id), lora_request=lora_request)
        active_requests.append(request_generator)
        request_id += 1
    for request_generator in active_requests:
        async for request_output in request_generator:
            if request_output.finished:
                print(request_output)
def initialize_engine() -> AsyncAphrodite:
    engine_args = AsyncEngineArgs(model='NousResearch/Llama-2-7b-hf', enable_lora=True, max_loras=1, max_lora_rank=8, max_cpu_loras=2, max_num_seqs=256)
    return AsyncAphrodite.from_engine_args(engine_args)
async def main():
    engine = initialize_engine()
    test_prompts = create_test_prompts('alpindale/l2-lora-test')
    await process_requests(engine, test_prompts)
if __name__ == '__main__':
    asyncio.run(main())