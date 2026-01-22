from abc import ABC, abstractmethod
class HandleDecoder(ABC):
    @abstractmethod
    def get_atom(self, handle: str):