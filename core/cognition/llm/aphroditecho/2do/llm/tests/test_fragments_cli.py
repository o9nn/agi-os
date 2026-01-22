from click.testing import CliRunner
from llm.cli import cli
import yaml
import sqlite_utils
import textwrap
def test_fragments_set_show_remove(user_path):
    runner = CliRunner()
    with runner.isolated_filesystem():
        open('fragment1.txt', 'w').write('Hello fragment 1')
        assert runner.invoke(cli, ['fragments', 'list', '--aliases']).output == ''
        assert runner.invoke(cli, ['fragments', 'set', 'f1', 'fragment1.txt']).exit_code == 0
        result1 = runner.invoke(cli, ['fragments', 'show', 'f1'])
        assert result1.exit_code == 0
        assert result1.output == 'Hello fragment 1\n'
        def get_list():
            result2 = runner.invoke(cli, ['fragments', 'list'])
            assert result2.exit_code == 0
            return yaml.safe_load(result2.output)
        assert 'f1' in runner.invoke(cli, ['fragments', 'list', '--aliases']).output
        loaded1 = get_list()
        assert set(loaded1[0].keys()) == {'aliases', 'content', 'datetime_utc', 'source', 'hash'}
        assert loaded1[0]['content'] == 'Hello fragment 1'
        assert loaded1[0]['aliases'] == ['f1']
        for key in ('f1', loaded1[0]['hash']):
            result3 = runner.invoke(cli, ['fragments', 'show', key])
            assert result3.exit_code == 0
            assert result3.output == 'Hello fragment 1\n'
        result4 = runner.invoke(cli, ['fragments', 'show', 'badalias'])
        assert result4.exit_code == 1
        assert "Fragment 'badalias' not found" in result4.output
        result5 = runner.invoke(cli, ['fragments', 'remove', 'f1'])
        assert result5.exit_code == 0
        loaded2 = get_list()
        assert loaded2[0]['aliases'] == []
        assert loaded2[0]['content'] == 'Hello fragment 1'
        assert runner.invoke(cli, ['fragments', 'list', '--aliases']).output == ''
def test_fragments_list(user_path):
    runner = CliRunner()
    with runner.isolated_filesystem():
        open('fragment1.txt', 'w').write('1')
        assert runner.invoke(cli, ['fragments', 'set', 'f1', 'fragment1.txt']).exit_code == 0
        db = sqlite_utils.Database(str(user_path / 'logs.db'))
        db['fragments'].delete_where()
        db['fragments'].insert({'content': '1', 'datetime_utc': '2023-10-01T00:00:00Z', 'source': 'file1.txt', 'hash': 'hash1'})
        db['fragments'].insert({'content': '2', 'datetime_utc': '2022-10-01T00:00:00Z', 'source': 'file2.txt', 'hash': 'hash2'})
        db['fragments'].insert({'content': '3', 'datetime_utc': '2024-10-01T00:00:00Z', 'source': 'file3.txt', 'hash': 'hash3'})
        result = runner.invoke(cli, ['fragments', 'list'])
        assert result.exit_code == 0
        assert result.output.strip() == textwrap.dedent("\n                - hash: hash2\n                  aliases: []\n                  datetime_utc: '2022-10-01T00:00:00Z'\n                  source: file2.txt\n                  content: '2'\n                - hash: hash1\n                  aliases:\n                  - f1\n                  datetime_utc: '2023-10-01T00:00:00Z'\n                  source: file1.txt\n                  content: '1'\n                - hash: hash3\n                  aliases: []\n                  datetime_utc: '2024-10-01T00:00:00Z'\n                  source: file3.txt\n                  content: '3'\n                ").strip()