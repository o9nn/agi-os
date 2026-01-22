import json, os
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
byte_encoder = bytes_to_unicode()
byte_decoder = {v: k for k, v in byte_encoder.items()}
sortedbd = sorted(byte_decoder.items(), key=lambda kv: kv[1])
tr = '{'
for i in sortedbd:
    tr += '"' + i[0] + '",'
tr += '}'
print(tr)
with open(os.path.dirname(os.path.realpath(__file__)) + '/' + 'rwkv_orig_vocab.json', 'r', encoding='utf-8') as f:
    encoder = json.load(f)
    s = ''
    with open('rwkv_vocab.embd', 'w', encoding='utf-8') as f2:
        for key in encoder:
            s += key + '\n'
        f2.write(s)
print('OK')