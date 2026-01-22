class EngineGenerateError(Exception):
    pass
class EngineDeadError(Exception):
    def __init__(self, *args, suppress_context: bool=False, **kwargs):
        ENGINE_DEAD_MESSAGE = 'EngineCore encountered an issue. See stack trace (above) for the root cause.'
        super().__init__(ENGINE_DEAD_MESSAGE, *args, **kwargs)
        self.__suppress_context__ = suppress_context