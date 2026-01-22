__version__ = '0.1.0'
try:
    from .server import ModelConfig as ServerModelConfig
    from .server import app as server_app
    from .nctalk import ModelConfig as CLIModelConfig
    from .nctalk import NanoCogCLI
    from .introspection.atomspace_client import AtomSpaceClient
except ImportError:
    pass
def create_server(model_path, device='cuda', port=8080, host='0.0.0.0'):
    from .server import app, ModelConfig
    model_config = ModelConfig(model_path, device)
    app.state.model_config = model_config
    return (app, {'host': host, 'port': port})
def start_cli(model_path, device='cuda'):
    from .nctalk import ModelConfig, NanoCogCLI
    model_config = ModelConfig(model_path, device)
    model_config.load_model()
    cli = NanoCogCLI(model_config)
    cli.run()