import sys
import struct
import json
import torch
import numpy as np
import re
from transformers import AutoModelForCausalLM
def bytes_to_unicode():
    bs = list(range(ord('!'), ord('~') + 1)) + list(range(ord('¡'), ord('¬') + 1)) + list(range(ord('®'), ord('ÿ') + 1))
    cs = bs[:]
    n = 0
    for b in range(2 ** 8):
        if b not in bs:
            bs.append(b)
            cs.append(2 ** 8 + n)
            n += 1
    cs = [chr(n) for n in cs]
    return dict(zip(bs, cs))
if len(sys.argv) < 2:
    print('Usage: convert-cerebras-to-ggml.py dir-model [use-f32]\n')
    sys.exit(1)
dir_model = sys.argv[1]
fname_out = sys.argv[1] + '/ggml-model-f16.bin'
with open(dir_model + '/vocab.json', 'r', encoding='utf-8') as f:
    encoder = json.load(f)
with open(dir_model + '/config.json', 'r', encoding='utf-8') as f:
    hparams = json.load(f)
use_f16 = True
if len(sys.argv) > 2:
    use_f16 = False
    fname_out = sys.argv[1] + '/ggml-model-f32.bin'
model = AutoModelForCausalLM.from_pretrained(dir_model, low_cpu_mem_usage=True)
list_vars = model.state_dict()
print(hparams)
fout = open(fname_out, 'wb')
fout.write(struct.pack('i', 1734831468))
fout.write(struct.pack('i', hparams['vocab_size']))
fout.write(struct.pack('i', hparams['n_positions']))
fout.write(struct.pack('i', hparams['n_embd']))
fout.write(struct.pack('i', hparams['n_head']))
fout.write(struct.pack('i', hparams['n_layer']))
fout.write(struct.pack('i', use_f16))
byte_encoder = bytes_to_unicode()
byte_decoder = {v: k for k, v in byte_encoder.items()}
fout.write(struct.pack('i', len(encoder)))
for key in encoder:
    text = bytearray([byte_decoder[c] for c in key])
    fout.write(struct.pack('i', len(text)))
    fout.write(text)
for name in list_vars.keys():
    data = list_vars[name].squeeze().numpy()
    print('Processing variable: ' + name + ' with shape: ', data.shape)
    if name == 'transformer.ln_f.weight':
        name = 'model/ln_f/g'
    elif name == 'transformer.ln_f.bias':
        name = 'model/ln_f/b'
    elif name == 'transformer.wte.weight':
        name = 'model/wte'
    elif name == 'transformer.wpe.weight':
        name = 'model/wpe'
    elif name == 'lm_head.weight':
        name = 'model/lm_head'
    elif re.match('transformer.h\\.\\d+\\.ln_1\\.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/ln_1/g'
    elif re.match('transformer.h\\.\\d+\\.ln_1\\.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/ln_1/b'
    elif re.match('transformer.h\\.\\d+\\.attn\\.c_attn\\.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/attn/c_attn/w'
    elif re.match('transformer.h\\.\\d+\\.attn\\.c_attn\\.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/attn/c_attn/b'
    elif re.match('transformer.h\\.\\d+\\.attn\\.c_proj\\.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/attn/c_proj/w'
    elif re.match('transformer.h.\\d+.attn.c_proj.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/attn/c_proj/b'
    elif re.match('transformer.h.\\d+.ln_2.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/ln_2/g'
    elif re.match('transformer.h.\\d+.ln_2.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/ln_2/b'
    elif re.match('transformer.h.\\d+.mlp.c_fc.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/mlp/c_fc/w'
    elif re.match('transformer.h.\\d+.mlp.c_fc.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/mlp/c_fc/b'
    elif re.match('transformer.h.\\d+.mlp.c_proj.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/mlp/c_proj/w'
    elif re.match('transformer.h.\\d+.mlp.c_proj.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/mlp/c_proj/b'
    else:
        print('Unrecognized variable name. %s', name)
    if name.endswith('attn.masked_bias') or name.endswith('.attn.bias'):
        print('  Skipping variable: ' + name)
        continue
    n_dims = len(data.shape)
    ftype = 0
    if use_f16:
        if (name == 'model/wte' or name == 'model/lm_head' or name[-2:] == '/g' or (name[-2:] == '/w')) and n_dims == 2:
            print('  Converting to float16')
            data = data.astype(np.float16)
            ftype = 1
        else:
            print('  Converting to float32')
            data = data.astype(np.float32)
            ftype = 0
    if name[-14:] == '/attn/c_attn/w' or name[-14:] == '/attn/c_proj/w' or name[-11:] == '/mlp/c_fc/w' or (name[-13:] == '/mlp/c_proj/w'):
        print('  Transposing')
        data = data.transpose()
    str = name.encode('utf-8')
    fout.write(struct.pack('iii', n_dims, len(str), ftype))
    for i in range(n_dims):
        fout.write(struct.pack('i', data.shape[n_dims - 1 - i]))
    fout.write(str)
    data.tofile(fout)
fout.close()
print('Done. Output file: ' + fname_out)
print('')