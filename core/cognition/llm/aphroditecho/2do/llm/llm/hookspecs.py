from pluggy import HookimplMarker
from pluggy import HookspecMarker
hookspec = HookspecMarker('llm')
hookimpl = HookimplMarker('llm')
@hookspec
def register_commands(cli):
@hookspec
def register_models(register):
@hookspec
def register_embedding_models(register):
@hookspec
def register_template_loaders(register):
@hookspec
def register_fragment_loaders(register):
@hookspec
def register_tools(register):