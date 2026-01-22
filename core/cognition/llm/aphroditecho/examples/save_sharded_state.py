import argparse
import dataclasses
import os
import shutil
from pathlib import Path
from aphrodite import LLM, EngineArgs
parser = argparse.ArgumentParser()
EngineArgs.add_cli_args(parser)
parser.add_argument('--output', '-o', required=True, type=str, help='path to output checkpoint')
parser.add_argument('--file-pattern', type=str, help='string pattern of saved filenames')
parser.add_argument('--max-file-size', type=str, default=5 * 1024 ** 3, help='max size (in bytes) of each safetensors file')
def main(args):
    engine_args = EngineArgs.from_cli_args(args)
    if engine_args.enable_lora:
        raise ValueError('Saving with enable_lora=True is not supported!')
    model_path = engine_args.model
    if not Path(model_path).is_dir():
        raise ValueError('model path must be a local directory')
    llm = LLM(**dataclasses.asdict(engine_args))
    Path(args.output).mkdir(exist_ok=True)
    model_executor = llm.llm_engine.model_executor
    model_executor.save_sharded_state(path=args.output, pattern=args.file_pattern, max_size=args.max_file_size)
    for file in os.listdir(model_path):
        if os.path.splitext(file)[1] not in ('.bin', '.pt', '.safetensors'):
            if os.path.isdir(os.path.join(model_path, file)):
                shutil.copytree(os.path.join(model_path, file), os.path.join(args.output, file))
            else:
                shutil.copy(os.path.join(model_path, file), args.output)
if __name__ == '__main__':
    args = parser.parse_args()
    main(args)