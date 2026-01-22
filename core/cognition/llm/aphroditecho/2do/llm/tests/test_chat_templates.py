from click.testing import CliRunner
import sys
import llm.cli
import pytest
@pytest.mark.xfail(sys.platform == 'win32', reason='Expected to fail on Windows')
def test_chat_template_system_only_no_duplicate_prompt(mock_model, logs_db, templates_path):
    (templates_path / 'wild-french.yaml').write_text('system: Speak in French\n', 'utf-8')
    runner = CliRunner()
    mock_model.enqueue(['Bonjour !'])
    result = runner.invoke(llm.cli.cli, ['chat', '-m', 'mock', '-t', 'wild-french'], input='hi\nquit\n', catch_exceptions=False)
    assert result.exit_code == 0
    rows = list(logs_db['responses'].rows)
    assert len(rows) == 1
    assert rows[0]['prompt'] == 'hi'
    assert rows[0]['system'] == 'Speak in French'
@pytest.mark.xfail(sys.platform == 'win32', reason='Expected to fail on Windows')
def test_chat_system_fragments_only_first_turn(tmpdir, mock_model, logs_db):
    sys_frag_path = str(tmpdir / 'sys.txt')
    with open(sys_frag_path, 'w', encoding='utf-8') as fp:
        fp.write('System fragment content')
    runner = CliRunner()
    mock_model.enqueue(['first'])
    mock_model.enqueue(['second'])
    result = runner.invoke(llm.cli.cli, ['chat', '-m', 'mock', '--system-fragment', sys_frag_path], input='Hi\nHi two\nquit\n', catch_exceptions=False)
    assert result.exit_code == 0
    responses = list(logs_db['responses'].rows)
    assert len(responses) == 2
    first_id = responses[0]['id']
    second_id = responses[1]['id']
    sys_frags = list(logs_db['system_fragments'].rows)
    assert len(sys_frags) == 1
    assert sys_frags[0]['response_id'] == first_id
    assert sys_frags[0]['response_id'] != second_id
@pytest.mark.xfail(sys.platform == 'win32', reason='Expected to fail on Windows')
def test_chat_template_loads_tools_into_logs(logs_db, templates_path):
    (templates_path / 'mytools.yaml').write_text('model: echo\ntools:\n- llm_version\n- llm_time\n', 'utf-8')
    runner = CliRunner()
    result = runner.invoke(llm.cli.cli, ['chat', '-t', 'mytools'], input='hi\nquit\n', catch_exceptions=False)
    assert result.exit_code == 0
    responses = list(logs_db['responses'].rows)
    assert len(responses) == 1
    assert responses[0]['prompt'] == 'hi'
    response_id = responses[0]['id']
    rows = list(logs_db.query('\n            select tools.name from tools\n            join tool_responses tr on tr.tool_id = tools.id\n            where tr.response_id = ?\n            order by tools.name\n            ', [response_id]))
    assert [r['name'] for r in rows] == ['llm_time', 'llm_version']