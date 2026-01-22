import sys
import json
import struct
import numpy as np
import tensorflow as tf
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
def convert_to_ftype(data, ftype):
    if ftype == 1:
        return data.astype(np.float16)
    assert False, 'Invalid ftype: ' + str(ftype)
if len(sys.argv) < 3:
    print('Usage: convert-ckpt-to-ggml.py dir-model ftype\n')
    print('  ftype == 0 -> float32')
    print('  ftype == 1 -> float16')
    sys.exit(1)
dir_model = sys.argv[1]
fname_out = sys.argv[1] + '/ggml-model.bin'
with open(dir_model + '/encoder.json', 'r', encoding='utf-8') as f:
    encoder = json.load(f)
with open(dir_model + '/hparams.json', 'r', encoding='utf-8') as f:
    hparams = json.load(f)
ftype_str = ['f32', 'f16']
ftype = 1
if len(sys.argv) > 2:
    ftype = int(sys.argv[2])
    if ftype < 0 or ftype > 1:
        print('Invalid ftype: ' + str(ftype))
        sys.exit(1)
    fname_out = sys.argv[1] + '/ggml-model-' + ftype_str[ftype] + '.bin'
list_vars = tf.train.list_variables(dir_model)
fout = open(fname_out, 'wb')
fout.write(struct.pack('i', 1734831468))
fout.write(struct.pack('i', hparams['n_vocab']))
fout.write(struct.pack('i', hparams['n_ctx']))
fout.write(struct.pack('i', hparams['n_embd']))
fout.write(struct.pack('i', hparams['n_head']))
fout.write(struct.pack('i', hparams['n_layer']))
fout.write(struct.pack('i', ftype))
byte_encoder = bytes_to_unicode()
byte_decoder = {v: k for k, v in byte_encoder.items()}
fout.write(struct.pack('i', len(encoder)))
for key in encoder:
    text = bytearray([byte_decoder[c] for c in key])
    fout.write(struct.pack('i', len(text)))
    fout.write(text)
for name, shape in list_vars:
    print('Processing variable: ' + name + ' with shape: ', shape)
    data = tf.train.load_variable(dir_model, name).squeeze()
    n_dims = len(data.shape)
    if name[-14:] == '/attn/c_attn/w' or name[-14:] == '/attn/c_proj/w' or name[-11:] == '/mlp/c_fc/w' or (name[-13:] == '/mlp/c_proj/w'):
        print('  Transposing')
        data = data.transpose()
    dshape = data.shape
    ftype_cur = 0
    if ftype != 0:
        if name == 'model/wte' or name[-2:] == '/w':
            print('  Converting to ' + ftype_str[ftype])
            data = convert_to_ftype(data, ftype)
            ftype_cur = ftype
        else:
            print('  Converting to float32')
            data = data.astype(np.float32)
            ftype_cur = 0
    str = name.encode('utf-8')
    fout.write(struct.pack('iii', n_dims, len(str), ftype_cur))
    for i in range(n_dims):
        fout.write(struct.pack('i', dshape[n_dims - 1 - i]))
    fout.write(str)
    data.tofile(fout)
fout.close()
print('Done. Output file: ' + fname_out)
print('')