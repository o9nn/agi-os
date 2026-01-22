import sys
import struct
import json
import numpy as np
from transformers import AutoModelForCausalLM, AutoTokenizer
import sentencepiece.sentencepiece_model_pb2 as model
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
tokenizer = AutoTokenizer.from_pretrained(dir_model, trust_remote_code=True)
model = AutoModelForCausalLM.from_pretrained(dir_model, low_cpu_mem_usage=True, trust_remote_code=True)
list_vars = model.state_dict()
for name in list_vars.keys():
    print(name, list_vars[name].shape, list_vars[name].dtype)
fout = open(fname_out, 'wb')
print(hparams)
fout.write(struct.pack('i', 1734831468))
fout.write(struct.pack('i', hparams['d_model']))
fout.write(struct.pack('i', hparams['max_seq_len']))
fout.write(struct.pack('i', hparams['n_heads']))
fout.write(struct.pack('i', hparams['n_layers']))
fout.write(struct.pack('i', hparams['vocab_size']))
fout.write(struct.pack('f', hparams['attn_config']['alibi_bias_max']))
fout.write(struct.pack('f', hparams['attn_config']['clip_qkv'] or 0.0))
fout.write(struct.pack('i', ftype))
vocab_size = hparams['vocab_size']
encoder = tokenizer.vocab
encoder.update(tokenizer.get_added_vocab())
byte_encoder = bytes_to_unicode()
byte_decoder = {v: k for k, v in byte_encoder.items()}
counter = 0
for key in sorted(encoder, key=encoder.get):
    text = ''
    for c in key:
        if c not in byte_decoder:
            text += c
        else:
            text += chr(byte_decoder[c])
    text = bytearray(text, encoding='utf-8')
    fout.write(struct.pack('i', len(text)))
    fout.write(text)
    counter += 1
while counter < vocab_size:
    fout.write(struct.pack('i', len(text)))
    fout.write(text)
    counter += 1
for name in list_vars.keys():
    data = list_vars[name].squeeze().numpy()
    print('Processing variable: ' + name + ' with shape: ', data.shape)
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