import pytest
from utils import *
@pytest.fixture(autouse=True)
def stop_server_after_each_test():
    yield
    instances = set(server_instances)
    for server in instances:
        server.stop()