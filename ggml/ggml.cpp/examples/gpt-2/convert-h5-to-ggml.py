import sys
import struct
import json
import numpy as np
import re
from transformers import GPT2Model
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
    print('Usage: convert-h5-to-ggml.py dir-model [use-f32]\n')
    sys.exit(1)
dir_model = sys.argv[1]
fname_out = sys.argv[1] + '/ggml-model.bin'
with open(dir_model + '/vocab.json', 'r', encoding='utf-8') as f:
    encoder = json.load(f)
with open(dir_model + '/added_tokens.json', 'r', encoding='utf-8') as f:
    encoder_added = json.load(f)
with open(dir_model + '/config.json', 'r', encoding='utf-8') as f:
    hparams = json.load(f)
use_f16 = True
if len(sys.argv) > 2:
    use_f16 = False
    fname_out = sys.argv[1] + '/ggml-model-f32.bin'
model = GPT2Model.from_pretrained(dir_model, low_cpu_mem_usage=True)
list_vars = model.state_dict()
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
    ftype = 0
    if use_f16:
        if name[-7:] == '.weight' and n_dims == 2:
            print('  Converting to float16')
            data = data.astype(np.float16)
            ftype = 1
        else:
            print('  Converting to float32')
            data = data.astype(np.float32)
            ftype = 0
    if name.endswith('.mlp.c_proj.weight'):
        print('  Transposing')
        data = data.transpose()
    if name == 'ln_f.weight':
        name = 'model/ln_f/g'
    elif name == 'ln_f.bias':
        name = 'model/ln_f/b'
    elif name == 'wte.weight':
        name = 'model/wte'
    elif name == 'wpe.weight':
        name = 'model/wpe'
    elif re.match('h\\.\\d+\\.ln_1\\.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/ln_1/g'
    elif re.match('h\\.\\d+\\.ln_1\\.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/ln_1/b'
    elif re.match('h\\.\\d+\\.attn\\.c_attn\\.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/attn/c_attn/w'
    elif re.match('h\\.\\d+\\.attn\\.c_attn\\.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/attn/c_attn/b'
    elif re.match('h\\.\\d+\\.attn\\.c_proj\\.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/attn/c_proj/w'
    elif re.match('h.\\d+.attn.c_proj.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/attn/c_proj/b'
    elif re.match('h.\\d+.ln_2.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/ln_2/g'
    elif re.match('h.\\d+.ln_2.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/ln_2/b'
    elif re.match('h.\\d+.mlp.c_fc.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/mlp/c_fc/w'
    elif re.match('h.\\d+.mlp.c_fc.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/mlp/c_fc/b'
    elif re.match('h.\\d+.mlp.c_proj.weight', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/mlp/c_proj/w'
    elif re.match('h.\\d+.mlp.c_proj.bias', name):
        i = re.findall('\\d+', name)[0]
        name = f'model/h{i}/mlp/c_proj/b'
    else:
        print('Unrecognized variable name. %s', name)
    str = name.encode('utf-8')
    fout.write(struct.pack('iii', n_dims, len(str), ftype))
    for i in range(n_dims):
        fout.write(struct.pack('i', data.shape[n_dims - 1 - i]))
    fout.write(str)
    data.tofile(fout)
fout.close()
print('Done. Output file: ' + fname_out)
print('')