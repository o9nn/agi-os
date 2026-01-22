from __future__ import annotations
import uuid
import hashlib
import logging
import argparse
import os
import sys
from pathlib import Path
from tqdm import tqdm
if 'NO_LOCAL_GGUF' not in os.environ and (Path(__file__).parent.parent.parent.parent / 'gguf-py').exists():
    sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from gguf import GGUFReader
logger = logging.getLogger('gguf-hash')
UUID_NAMESPACE_LLAMA_CPP = uuid.UUID('ef001206-dadc-5f6d-a15f-3359e577d4e5')
def gguf_hash(reader: GGUFReader, filename: str, disable_progress_bar: bool, no_layer: bool) -> None:
    sha1 = hashlib.sha1()
    sha256 = hashlib.sha256()
    uuidv5_sha1 = hashlib.sha1()
    uuidv5_sha1.update(UUID_NAMESPACE_LLAMA_CPP.bytes)
    total_weights = 0
    for n, tensor in enumerate(reader.tensors, 1):
        if tensor.name.endswith(('.attention.masked_bias', '.attention.bias', '.rotary_emb.inv_freq')):
            continue
        sum_weights_in_tensor = 1
        for dim in tensor.shape:
            sum_weights_in_tensor *= dim
        total_weights += sum_weights_in_tensor
    bar = tqdm(desc='Hashing', total=total_weights, unit='weights', unit_scale=True, disable=disable_progress_bar)
    for tensor in reader.tensors:
        if tensor.name.endswith(('.attention.masked_bias', '.attention.bias', '.rotary_emb.inv_freq')):
            continue
        sum_weights_in_tensor = 1
        for dim in tensor.shape:
            sum_weights_in_tensor *= dim
        bar.update(sum_weights_in_tensor)
        if not no_layer:
            sha1_layer = hashlib.sha1()
            sha1_layer.update(tensor.data.data)
            print('sha1      {0}  {1}:{2}'.format(sha1_layer.hexdigest(), filename, tensor.name))
            sha256_layer = hashlib.sha256()
            sha256_layer.update(tensor.data.data)
            print('sha256    {0}  {1}:{2}'.format(sha256_layer.hexdigest(), filename, tensor.name))
        sha1.update(tensor.data.data)
        sha256.update(tensor.data.data)
        uuidv5_sha1.update(tensor.data.data)
    bar.close()
    print('sha1      {0}  {1}'.format(sha1.hexdigest(), filename))
    print('sha256    {0}  {1}'.format(sha256.hexdigest(), filename))
    print('uuid      {0}  {1}'.format(uuid.UUID(bytes=uuidv5_sha1.digest()[:16], version=5), filename))
def main() -> None:
    parser = argparse.ArgumentParser(description='Dump GGUF file metadata')
    parser.add_argument('model', type=str, help='GGUF format model filename')
    parser.add_argument('--no-layer', action='store_true', help='exclude per layer hash')
    parser.add_argument('--verbose', action='store_true', help='increase output verbosity')
    parser.add_argument('--progressbar', action='store_true', help='enable progressbar')
    args = parser.parse_args(None if len(sys.argv) > 1 else ['--help'])
    logging.basicConfig(level=logging.DEBUG if args.verbose else logging.INFO)
    reader = GGUFReader(args.model, 'r')
    gguf_hash(reader, args.model, not args.progressbar, args.no_layer)
if __name__ == '__main__':
    main()