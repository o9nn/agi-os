import argparse
import sampling
from rwkv_cpp import rwkv_cpp_shared_library, rwkv_cpp_model
from tokenizer_util import add_tokenizer_argument, get_tokenizer
from typing import List
parser = argparse.ArgumentParser(description='Generate some text with an RWKV model')
parser.add_argument('model_path', help='Path to RWKV model in ggml format')
parser.add_argument('-ngl', '--num_gpu_layers', type=int, default=99, help='Number of layers to run on GPU')
add_tokenizer_argument(parser)
args = parser.parse_args()
library = rwkv_cpp_shared_library.load_rwkv_shared_library()
model = rwkv_cpp_model.RWKVModel(library, args.model_path, gpu_layer_count=args.num_gpu_layers)
tokenizer_decode, tokenizer_encode = get_tokenizer(args.tokenizer, model.n_vocab)
prompt: str = 'One upon a time,'
prompt_tokens: List[int] = tokenizer_encode(prompt)
logits, state = model.eval_sequence_in_chunks(prompt_tokens, None, None, None, use_numpy=True)
print(prompt, end='')
for i in range(32):
    token: int = sampling.sample_logits(logits, temperature=0.8, top_p=0.5)
    print(tokenizer_decode([token]), end='', flush=True)
    logits, state = model.eval(token, state, state, logits, use_numpy=True)
model.free()