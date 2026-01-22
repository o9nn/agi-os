import sys
import torch
import struct
import numpy as np
if len(sys.argv) < 3:
    print('Usage: convert-pth-to-ggml.py file-model dir-output [ftype]\n')
    print('  ftype == 0 -> float32')
    print('  ftype == 1 -> float16')
    sys.exit(1)
fname_model = sys.argv[1]
dir_out = sys.argv[2]
fname_out = dir_out + '/ggml-model.bin'
ftype_str = ['f32', 'f16']
ftype = 1
if len(sys.argv) > 3:
    ftype = int(sys.argv[3])
if ftype < 0 or ftype > 1:
    print('Invalid ftype: ' + str(ftype))
    sys.exit(1)
fname_out = fname_out.replace('.bin', '-' + ftype_str[ftype] + '.bin')
n_enc_state = 768
n_enc_layers = 12
n_enc_heads = 12
n_enc_out_chans = 256
n_pt_embd = 4
model = torch.load(fname_model, map_location='cpu')
for k, v in model.items():
    print(k, v.shape)
    if k == 'image_encoder.blocks.0.norm1.weight':
        n_enc_state = v.shape[0]
if n_enc_state == 1024:
    n_enc_layers = 24
    n_enc_heads = 16
elif n_enc_state == 1280:
    n_enc_layers = 32
    n_enc_heads = 16
hparams = {'n_enc_state': n_enc_state, 'n_enc_layers': n_enc_layers, 'n_enc_heads': n_enc_heads, 'n_enc_out_chans': n_enc_out_chans, 'n_pt_embd': n_pt_embd}
print(hparams)
for k, v in model.items():
    print(k, v.shape)
fout = open(fname_out, 'wb')
fout.write(struct.pack('i', 1734831468))
fout.write(struct.pack('i', hparams['n_enc_state']))
fout.write(struct.pack('i', hparams['n_enc_layers']))
fout.write(struct.pack('i', hparams['n_enc_heads']))
fout.write(struct.pack('i', hparams['n_enc_out_chans']))
fout.write(struct.pack('i', hparams['n_pt_embd']))
fout.write(struct.pack('i', ftype))
for k, v in model.items():
    name = k
    shape = v.shape
    if name[:19] == 'prompt_encoder.mask':
        continue
    print('Processing variable: ' + name + ' with shape: ', shape, ' and type: ', v.dtype)
    data = v.numpy()
    n_dims = len(data.shape)
    dshape = data.shape
    ftype_cur = 1
    if ftype == 0 or n_dims == 1 or name == 'image_encoder.pos_embed' or name.startswith('prompt_encoder') or name.startswith('mask_decoder.iou_token') or name.startswith('mask_decoder.mask_tokens'):
        print('  Converting to float32')
        data = data.astype(np.float32)
        ftype_cur = 0
    else:
        print('  Converting to float16')
        data = data.astype(np.float16)
    if name == 'image_encoder.patch_embed.proj.bias':
        data = data.reshape(1, data.shape[0], 1, 1)
        n_dims = len(data.shape)
        dshape = data.shape
    print('  New shape: ', dshape)
    str = name.encode('utf-8')
    fout.write(struct.pack('iii', n_dims, len(str), ftype_cur))
    for i in range(n_dims):
        fout.write(struct.pack('i', dshape[n_dims - 1 - i]))
    fout.write(str)
    data.tofile(fout)
fout.close()
print('Done. Output file: ' + fname_out)
print('')