import sys
import struct
import json
import torch
import numpy as np
from transformers import GPTJForCausalLM
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
if len(sys.argv) < 3:
    print('Usage: convert-h5-to-ggml.py dir-model [use-f32]\n')
    print('  ftype == 0 -> float32')
    print('  ftype == 1 -> float16')
    sys.exit(1)
dir_model = sys.argv[1]
fname_out = sys.argv[1] + '/ggml-model.bin'
with open(dir_model + '/vocab.json', 'r', encoding='utf-8') as f:
    encoder = json.load(f)
with open(dir_model + '/added_tokens.json', 'r', encoding='utf-8') as f:
    encoder_added = json.load(f)
with open(dir_model + '/config.json', 'r', encoding='utf-8') as f:
    hparams = json.load(f)
ftype_str = ['f32', 'f16']
ftype = 1
if len(sys.argv) > 2:
    ftype = int(sys.argv[2])
    if ftype < 0 or ftype > 1:
        print('Invalid ftype: ' + str(ftype))
        sys.exit(1)
    fname_out = sys.argv[1] + '/ggml-model-' + ftype_str[ftype] + '.bin'
model = GPTJForCausalLM.from_pretrained(dir_model, low_cpu_mem_usage=True)
list_vars = model.state_dict()
fout = open(fname_out, 'wb')
fout.write(struct.pack('i', 1734831468))
fout.write(struct.pack('i', hparams['vocab_size']))
fout.write(struct.pack('i', hparams['n_positions']))
fout.write(struct.pack('i', hparams['n_embd']))
fout.write(struct.pack('i', hparams['n_head']))
fout.write(struct.pack('i', hparams['n_layer']))
fout.write(struct.pack('i', hparams['rotary_dim']))
fout.write(struct.pack('i', ftype))
byte_encoder = bytes_to_unicode()
byte_decoder = {v: k for k, v in byte_encoder.items()}
fout.write(struct.pack('i', len(encoder) + len(encoder_added)))
for key in encoder:
    text = bytearray([byte_decoder[c] for c in key])
    fout.write(struct.pack('i', len(text)))
    fout.write(text)
for key in encoder_added:
    text = bytearray([byte_decoder[c] for c in key])
    fout.write(struct.pack('i', len(text)))
    fout.write(text)
for name in list_vars.keys():
    data = list_vars[name].squeeze().numpy()
    print('Processing variable: ' + name + ' with shape: ', data.shape)
    if name.endswith('attn.masked_bias') or name.endswith('.attn.bias'):
        print('  Skipping variable: ' + name)
        continue
    n_dims = len(data.shape)
    ftype_cur = 0
    if ftype != 0:
        if name[-7:] == '.weight' and n_dims == 2:
            print('  Converting to float16')
            data = data.astype(np.float16)
            ftype_cur = 1
        else:
            print('  Converting to float32')
            data = data.astype(np.float32)
            ftype_cur = 0
    elif data.dtype != np.float32:
        print('  Converting to float32')
        data = data.astype(np.float32)
        ftype_cur = 0
    str = name.encode('utf-8')
    fout.write(struct.pack('iii', n_dims, len(str), ftype_cur))
    for i in range(n_dims):
        fout.write(struct.pack('i', data.shape[n_dims - 1 - i]))
    fout.write(str)
    data.tofile(fout)
fout.close()
print('Done. Output file: ' + fname_out)
print('')