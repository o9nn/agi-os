import argparse
import dataclasses
import json
import uuid
import aphrodite.common.envs as envs
from aphrodite import LLM
from aphrodite.engine.args_tools import EngineArgs
from aphrodite.modeling.model_loader.tensorizer import TensorizerArgs, TensorizerConfig, tensorize_aphrodite_model
'\ntensorize_aphrodite_model.py is a script that can be used to serialize and \ndeserialize Aphrodite models. These models can be loaded using tensorizer \nto the GPU extremely quickly over an HTTP/HTTPS endpoint, an S3 endpoint,\nor locally. Tensor encryption and decryption is also supported, although \nlibsodium must be installed to use it. Install aphrodite with tensorizer\nsupport  using `pip install aphrodite[tensorizer]`. To learn more about\ntensorizer, visit https://github.com/coreweave/tensorizer\n\nTo serialize a model, install Aphrodite from source, then run something \nlike this from the root level of this repository:\n\npython -m examples.tensorize_aphrodite_model    --model facebook/opt-125m    serialize    --serialized-directory s3://my-bucket    --suffix v1\n   \nWhich downloads the model from HuggingFace, loads it into Aphrodite, serializes\nit, and saves it to your S3 bucket. A local directory can also be used. This\nassumes your S3 credentials are specified as environment variables\nin the form of `S3_ACCESS_KEY_ID`, `S3_SECRET_ACCESS_KEY`, and \n`S3_ENDPOINT_URL`. To provide S3 credentials directly, you can provide \n`--s3-access-key-id` and `--s3-secret-access-key`, as well as `--s3-endpoint` \nas CLI args to this script.\n\nYou can also encrypt the model weights with a randomly-generated key by \nproviding a `--keyfile` argument.\n\nTo deserialize a model, you can run something like this from the root \nlevel of this repository:\n\npython -m examples.tensorize_aphrodite_model    --model EleutherAI/gpt-j-6B    --dtype float16    deserialize    --path-to-tensors s3://my-bucket/aphrodite/EleutherAI/gpt-j-6B/v1/model.tensors\n\nWhich downloads the model tensors from your S3 bucket and deserializes them.\n\nYou can also provide a `--keyfile` argument to decrypt the model weights if \nthey were serialized with encryption.\n\nTo support distributed tensor-parallel models, each model shard will be\nserialized to a separate file. The tensorizer_uri is then specified as a string\ntemplate with a format specifier such as \'%03d\' that will be rendered with the\nshard\'s rank. Sharded models serialized with this script will be named as\nmodel-rank-%03d.tensors\n\nFor more information on the available arguments for serializing, run \n`python -m examples.tensorize_aphrodite_model serialize --help`.\n\nOr for deserializing:\n\n`python -m examples.tensorize_aphrodite_model deserialize --help`.\n\nOnce a model is serialized, tensorizer can be invoked with the `LLM` class \ndirectly to load models:\n\n    llm = LLM(model="facebook/opt-125m",\n              load_format="tensorizer",\n              model_loader_extra_config=TensorizerConfig(\n                    tensorizer_uri = path_to_tensors,\n                    num_readers=3,\n                    )\n              )\n            \nA serialized model can be used during model loading for the Aphrodite OpenAI\ninference server. `model_loader_extra_config` is exposed as the CLI arg\n`--model-loader-extra-config`, and accepts a JSON string literal of the\nTensorizerConfig arguments desired.\n\nIn order to see all of the available arguments usable to configure \nloading with tensorizer that are given to `TensorizerConfig`, run:\n\n`python -m examples.tensorize_aphrodite_model deserialize --help`\n\nunder the `tensorizer options` section. These can also be used for\ndeserialization in this example script, although `--tensorizer-uri` and\n`--path-to-tensors` are functionally the same in this case.\n'
def parse_args():
    parser = argparse.ArgumentParser(description='An example script that can be used to serialize and deserialize Aphrodite models. These models can be loaded using tensorizer directly to the GPU extremely quickly. Tensor encryption and decryption is also supported, although libsodium must be installed to use it.')
    parser = EngineArgs.add_cli_args(parser)
    subparsers = parser.add_subparsers(dest='command')
    serialize_parser = subparsers.add_parser('serialize', help='Serialize a model to `--serialized-directory`')
    serialize_parser.add_argument('--suffix', type=str, required=False, help='The suffix to append to the serialized model directory, which is used to construct the location of the serialized model tensors, e.g. if `--serialized-directory` is `s3://my-bucket/` and `--suffix` is `v1`, the serialized model tensors will be saved to `s3://my-bucket/aphrodite/EleutherAI/gpt-j-6B/v1/model.tensors`. If none is provided, a random UUID will be used.')
    serialize_parser.add_argument('--serialized-directory', type=str, required=True, help='The directory to serialize the model to. This can be a local directory or S3 URI. The path to where the tensors are saved is a combination of the supplied `dir` and model reference ID. For instance, if `dir` is the serialized directory, and the model HuggingFace ID is `EleutherAI/gpt-j-6B`, tensors will be saved to `dir/aphrodite/EleutherAI/gpt-j-6B/suffix/model.tensors`, where `suffix` is given by `--suffix` or a random UUID if not provided.')
    serialize_parser.add_argument('--keyfile', type=str, required=False, help='Encrypt the model weights with a randomly-generated binary key, and save the key at this path')
    deserialize_parser = subparsers.add_parser('deserialize', help='Deserialize a model from `--path-to-tensors` to verify it can be loaded and used.')
    deserialize_parser.add_argument('--path-to-tensors', type=str, required=True, help='The local path or S3 URI to the model tensors to deserialize. ')
    deserialize_parser.add_argument('--keyfile', type=str, required=False, help='Path to a binary key to use to decrypt the model weights, if the model was serialized with encryption')
    TensorizerArgs.add_cli_args(deserialize_parser)
    return parser.parse_args()
def deserialize():
    llm = LLM(model=args.model, load_format='tensorizer', tensor_parallel_size=args.tensor_parallel_size, model_loader_extra_config=tensorizer_config)
    return llm
if __name__ == '__main__':
    args = parse_args()
    s3_access_key_id = getattr(args, 's3_access_key_id', None) or envs.S3_ACCESS_KEY_ID
    s3_secret_access_key = getattr(args, 's3_secret_access_key', None) or envs.S3_SECRET_ACCESS_KEY
    s3_endpoint = getattr(args, 's3_endpoint', None) or envs.S3_ENDPOINT_URL
    credentials = {'s3_access_key_id': s3_access_key_id, 's3_secret_access_key': s3_secret_access_key, 's3_endpoint': s3_endpoint}
    model_ref = args.model
    model_name = model_ref.split('/')[1]
    keyfile = args.keyfile if args.keyfile else None
    if args.model_loader_extra_config:
        config = json.loads(args.model_loader_extra_config)
        tensorizer_args = TensorizerConfig(**config)._construct_tensorizer_args()
        tensorizer_args.tensorizer_uri = args.path_to_tensors
    else:
        tensorizer_args = None
    if args.command == 'serialize':
        eng_args_dict = {f.name: getattr(args, f.name) for f in dataclasses.fields(EngineArgs)}
        engine_args = EngineArgs.from_cli_args(argparse.Namespace(**eng_args_dict))
        input_dir = args.serialized_directory.rstrip('/')
        suffix = args.suffix if args.suffix else uuid.uuid4().hex
        base_path = f'{input_dir}/aphrodite/{model_ref}/{suffix}'
        if engine_args.tensor_parallel_size > 1:
            model_path = f'{base_path}/model-rank-%03d.tensors'
        else:
            model_path = f'{base_path}/model.tensors'
        tensorizer_config = TensorizerConfig(tensorizer_uri=model_path, encryption_keyfile=keyfile, **credentials)
        tensorize_aphrodite_model(engine_args, tensorizer_config)
    elif args.command == 'deserialize':
        if not tensorizer_args:
            tensorizer_config = TensorizerConfig(tensorizer_uri=args.path_to_tensors, encryption_keyfile=keyfile, **credentials)
        deserialize()
    else:
        raise ValueError('Either serialize or deserialize must be specified.')