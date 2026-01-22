import pytest
@pytest.fixture()
def should_do_global_cleanup_after_test() -> bool:
    return False