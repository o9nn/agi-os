import argparse
from aphrodite.endpoints.cli.types import CLISubcommand
class BenchmarkSubcommandBase(CLISubcommand):
    help: str
    @classmethod
    def add_cli_args(cls, parser: argparse.ArgumentParser) -> None:
        raise NotImplementedError
    @staticmethod
    def cmd(args: argparse.Namespace) -> None:
        raise NotImplementedError