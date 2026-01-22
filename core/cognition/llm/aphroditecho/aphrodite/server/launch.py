import asyncio
import signal
import socket
from http import HTTPStatus
from typing import Any, Optional
import uvicorn
from fastapi import FastAPI, Request, Response
from loguru import logger
from aphrodite.common import envs
from aphrodite.utils import find_process_using_port
from aphrodite.endpoints.ssl import SSLCertRefresher
from aphrodite.engine.async_aphrodite import AsyncEngineDeadError
from aphrodite.engine.multiprocessing import MQEngineDeadError
from aphrodite.engine.protocol import EngineClient
from aphrodite.v1.engine.exceptions import EngineDeadError, EngineGenerateError
async def serve_http(app: FastAPI, sock: Optional[socket.socket], enable_ssl_refresh: bool=False, **uvicorn_kwargs: Any):
    for route in app.routes:
        methods = getattr(route, 'methods', None)
        path = getattr(route, 'path', None)
        if methods is None or path is None:
            continue
    config = uvicorn.Config(app, **uvicorn_kwargs)
    config.load()
    server = uvicorn.Server(config)
    _add_shutdown_handlers(app, server)
    loop = asyncio.get_running_loop()
    watchdog_task = loop.create_task(watchdog_loop(server, app.state.engine_client))
    server_task = loop.create_task(server.serve(sockets=[sock] if sock else None))
    ssl_cert_refresher = None if not enable_ssl_refresh else SSLCertRefresher(ssl_context=config.ssl, key_path=config.ssl_keyfile, cert_path=config.ssl_certfile, ca_path=config.ssl_ca_certs)
    def signal_handler() -> None:
        server_task.cancel()
        watchdog_task.cancel()
        if ssl_cert_refresher:
            ssl_cert_refresher.stop()
    async def dummy_shutdown() -> None:
        pass
    loop.add_signal_handler(signal.SIGINT, signal_handler)
    loop.add_signal_handler(signal.SIGTERM, signal_handler)
    try:
        await server_task
        return dummy_shutdown()
    except asyncio.CancelledError:
        port = uvicorn_kwargs['port']
        process = find_process_using_port(port)
        if process is not None:
            logger.debug('port {} is used by process {} launched with command:\n{}', port, process, ' '.join(process.cmdline()))
        logger.info('Shutting down FastAPI HTTP server.')
        return server.shutdown()
    finally:
        watchdog_task.cancel()
async def watchdog_loop(server: uvicorn.Server, engine: EngineClient):
    APHRODITE_WATCHDOG_TIME_S = 5.0
    while True:
        await asyncio.sleep(APHRODITE_WATCHDOG_TIME_S)
        terminate_if_errored(server, engine)
def terminate_if_errored(server: uvicorn.Server, engine: EngineClient):
    engine_errored = engine.errored and (not engine.is_running)
    if not envs.APHRODITE_KEEP_ALIVE_ON_ENGINE_DEATH and engine_errored:
        server.should_exit = True
def _add_shutdown_handlers(app: FastAPI, server: uvicorn.Server) -> None:
    @app.exception_handler(RuntimeError)
    @app.exception_handler(AsyncEngineDeadError)
    @app.exception_handler(MQEngineDeadError)
    @app.exception_handler(EngineDeadError)
    @app.exception_handler(EngineGenerateError)
    async def runtime_exception_handler(request: Request, __):
        terminate_if_errored(server=server, engine=request.app.state.engine_client)
        return Response(status_code=HTTPStatus.INTERNAL_SERVER_ERROR)