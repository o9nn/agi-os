import threading
from hyperon_das.commons.atoms import Atom, Link, Node
from hyperon_das.distributed_algorithm_node.bus_node import BusCommand
from hyperon_das.logger import log
from hyperon_das.service_clients.base import BaseProxy
class AtomDBProxy(BaseProxy):
    COMMAND_SIZE_LIMIT = 5000
    ADD_ATOMS = 'add_atoms'
    START_STREAM = 'start_stream'
    END_STREAM = 'end_stream'
    def __init__(self) -> None:
        super().__init__()
        self._lock = threading.Lock()
        self.command = BusCommand.ATOMDB
        log.debug('Created AtomDBProxy')
    def pack_command_line_args(self) -> None:
        self.tokenize(self.args)
    def tokenize(self, output: list[str]) -> None:
        return super().tokenize(output)
    def build_atoms_from_tokens(self, tokens: list[str]):
        with self._lock:
            atoms = []
            current = ''
            buffer = []
            def flush():
                if not current:
                    return
                if current == 'NODE':
                    atoms.append(Node(tokens=buffer))
                else:
                    atoms.append(Link(tokens=buffer))
                buffer.clear()
            for token in tokens:
                if token == 'NODE' or token == 'LINK':
                    if current:
                        flush()
                    current = token
                else:
                    buffer.append(token)
            if len(buffer) != 0 and current:
                flush()
            return atoms
    def add_atoms(self, atoms: list[Atom], use_streaming: bool=False) -> list[str]:
        args = []
        handles = []
        stream_info = [str(len(atoms))]
        if use_streaming:
            self.to_remote_peer(self.START_STREAM, stream_info)
        for atom in atoms:
            atom_type = 'NODE' if atom.arity() == 0 else 'LINK'
            args.append(atom_type)
            atom.tokenize(args)
            handles.append(atom.handle)
            if len(args) > self.COMMAND_SIZE_LIMIT or atom == atoms[-1]:
                self.to_remote_peer(self.ADD_ATOMS, args)
                args.clear()
        if use_streaming:
            self.to_remote_peer(self.END_STREAM, [])
        return handles