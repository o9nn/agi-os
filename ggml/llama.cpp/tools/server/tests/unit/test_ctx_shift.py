import pytest
from utils import *
server = ServerPreset.tinyllama2()
LONG_TEXT = '\nLorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\nUt enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.\nExcepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\n'.strip()
@pytest.fixture(scope='module', autouse=True)
def create_server():
    global server
    server = ServerPreset.tinyllama2()
    server.n_ctx = 256
    server.n_slots = 2
def test_ctx_shift_enabled():
    global server
    server.start()
    res = server.make_request('POST', '/completion', data={'n_predict': 64, 'prompt': LONG_TEXT})
    assert res.status_code == 200
    assert res.body['timings']['prompt_n'] == 109
    assert res.body['timings']['predicted_n'] == 64
    assert res.body['truncated'] is True
@pytest.mark.parametrize('n_predict,n_token_output,truncated', [(64, 64, False), (-1, 120, True)])
def test_ctx_shift_disabled_short_prompt(n_predict: int, n_token_output: int, truncated: bool):
    global server
    server.disable_ctx_shift = True
    server.n_predict = -1
    server.start()
    res = server.make_request('POST', '/completion', data={'n_predict': n_predict, 'prompt': 'Hi how are you'})
    assert res.status_code == 200
    assert res.body['timings']['predicted_n'] == n_token_output
    assert res.body['truncated'] == truncated
def test_ctx_shift_disabled_long_prompt():
    global server
    server.disable_ctx_shift = True
    server.start()
    res = server.make_request('POST', '/completion', data={'n_predict': 64, 'prompt': LONG_TEXT})
    assert res.status_code != 200
    assert 'error' in res.body
    assert 'exceeds the available context size' in res.body['error']['message']
def test_ctx_shift_disabled_stream():
    global server
    server.disable_ctx_shift = True
    server.start()
    res = server.make_stream_request('POST', '/v1/completions', data={'n_predict': 256, 'prompt': 'Once', 'stream': True})
    content = ''
    for data in res:
        choice = data['choices'][0]
        if choice['finish_reason'] == 'length':
            assert len(content) > 0
        else:
            assert choice['finish_reason'] is None
            content += choice['text']