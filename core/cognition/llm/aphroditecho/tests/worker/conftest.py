import pytest
@pytest.fixture(scope='function', autouse=True)
def use_v0_only(monkeypatch):
    monkeypatch.setenv('APHRODITE_USE_V1', '0')