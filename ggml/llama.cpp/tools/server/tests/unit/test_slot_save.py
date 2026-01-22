import pytest
from utils import *
server = ServerPreset.tinyllama2()
@pytest.fixture(scope='module', autouse=True)
def create_server():
    global server
    server = ServerPreset.tinyllama2()
    server.slot_save_path = './tmp'
    server.temperature = 0.0
def test_slot_save_restore():
    global server
    server.start()
    res = server.make_request('POST', '/completion', data={'prompt': 'What is the capital of France?', 'id_slot': 1, 'cache_prompt': True})
    assert res.status_code == 200
    assert match_regex('(Whiskers|Flana)+', res.body['content'])
    assert res.body['timings']['prompt_n'] == 21
    res = server.make_request('POST', '/slots/1?action=save', data={'filename': 'slot1.bin'})
    assert res.status_code == 200
    assert res.body['n_saved'] == 84
    res = server.make_request('POST', '/completion', data={'prompt': 'What is the capital of Germany?', 'id_slot': 1, 'cache_prompt': True})
    assert res.status_code == 200
    assert match_regex('(Jack|said)+', res.body['content'])
    assert res.body['timings']['prompt_n'] == 6
    res = server.make_request('POST', '/slots/0?action=restore', data={'filename': 'slot1.bin'})
    assert res.status_code == 200
    assert res.body['n_restored'] == 84
    res = server.make_request('POST', '/completion', data={'prompt': 'What is the capital of Germany?', 'id_slot': 0, 'cache_prompt': True})
    assert res.status_code == 200
    assert match_regex('(Jack|said)+', res.body['content'])
    assert res.body['timings']['prompt_n'] == 6
    res = server.make_request('POST', '/completion', data={'prompt': 'What is the capital of Germany?', 'id_slot': 1, 'cache_prompt': True})
    assert res.status_code == 200
    assert match_regex('(Jack|said)+', res.body['content'])
    assert res.body['timings']['prompt_n'] == 1
def test_slot_erase():
    global server
    server.start()
    res = server.make_request('POST', '/completion', data={'prompt': 'What is the capital of France?', 'id_slot': 1, 'cache_prompt': True})
    assert res.status_code == 200
    assert match_regex('(Whiskers|Flana)+', res.body['content'])
    assert res.body['timings']['prompt_n'] == 21
    res = server.make_request('POST', '/slots/1?action=erase')
    assert res.status_code == 200
    res = server.make_request('POST', '/completion', data={'prompt': 'What is the capital of France?', 'id_slot': 1, 'cache_prompt': True})
    assert res.status_code == 200
    assert match_regex('(Whiskers|Flana)+', res.body['content'])
    assert res.body['timings']['prompt_n'] == 21