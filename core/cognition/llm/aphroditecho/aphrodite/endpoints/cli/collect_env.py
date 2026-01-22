from __future__ import annotations
import argparse
import typing
from aphrodite.collect_env import main as collect_env_main
from aphrodite.endpoints.cli.types import CLISubcommand
if typing.TYPE_CHECKING:
    from aphrodite.utils import FlexibleArgumentParser
class CollectEnvSubcommand(CLISubcommand):
    name = 'collect-env'
    @staticmethod
    def cmd(args: argparse.Namespace) -> None:
        collect_env_main()
    def subparser_init(self, subparsers: argparse._SubParsersAction) -> FlexibleArgumentParser:
        return subparsers.add_parser('collect-env', help='Start collecting environment information.', description='Start collecting environment information.', usage='aphrodite collect-env')
def cmd_init() -> list[CLISubcommand]:
    return [CollectEnvSubcommand()]