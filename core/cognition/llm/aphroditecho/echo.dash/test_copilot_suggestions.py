import os
import unittest
from unittest.mock import patch, mock_open, MagicMock
import copilot_suggestions
class TestCopilotSuggestions(unittest.TestCase):
    def setUp(self):
        for var in ['AZURE_OPENAI_ENDPOINT', 'AZURE_OPENAI_KEY', 'AZURE_OPENAI_DEPLOYMENT']:
            if var in os.environ:
                del os.environ[var]
    def test_missing_environment_variables(self):
        result = copilot_suggestions.fetch_suggestions_from_azure_openai({})
        self.assertIsNone(result)
        os.environ['AZURE_OPENAI_ENDPOINT'] = 'https://test.openai.azure.com/'
        result = copilot_suggestions.fetch_suggestions_from_azure_openai({})
        self.assertIsNone(result)
        os.environ['AZURE_OPENAI_KEY'] = 'test-key'
        result = copilot_suggestions.fetch_suggestions_from_azure_openai({})
        self.assertIsNone(result)
    def test_api_url_construction(self):
        os.environ['AZURE_OPENAI_ENDPOINT'] = 'https://test.openai.azure.com'
        os.environ['AZURE_OPENAI_KEY'] = 'test-key'
        os.environ['AZURE_OPENAI_DEPLOYMENT'] = 'gpt-4'
        with patch('copilot_suggestions.requests.post') as mock_post:
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = {'choices': [{'message': {'content': '{"suggestions": ["test"], "next_focus": "test"}'}}]}
            mock_post.return_value = mock_response
            copilot_suggestions.fetch_suggestions_from_azure_openai({'test': 'data'})
            expected_url = 'https://test.openai.azure.com/openai/deployments/gpt-4/chat/completions?api-version=2024-02-15-preview'
            mock_post.assert_called_once()
            actual_url = mock_post.call_args[0][0]
            self.assertEqual(actual_url, expected_url)
            headers = mock_post.call_args[1]['headers']
            self.assertEqual(headers['api-key'], 'test-key')
            self.assertEqual(headers['Content-Type'], 'application/json')
    def test_endpoint_formatting(self):
        os.environ['AZURE_OPENAI_ENDPOINT'] = 'https://test.openai.azure.com'
        os.environ['AZURE_OPENAI_KEY'] = 'test-key'
        os.environ['AZURE_OPENAI_DEPLOYMENT'] = 'gpt-4'
        with patch('copilot_suggestions.requests.post') as mock_post:
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = {'choices': [{'message': {'content': '{"suggestions": ["test"]}'}}]}
            mock_post.return_value = mock_response
            copilot_suggestions.fetch_suggestions_from_azure_openai({'test': 'data'})
            expected_url = 'https://test.openai.azure.com/openai/deployments/gpt-4/chat/completions?api-version=2024-02-15-preview'
            actual_url = mock_post.call_args[0][0]
            self.assertEqual(actual_url, expected_url)
    def test_note_file_handling(self):
        with patch('copilot_suggestions.fetch_suggestions_from_azure_openai') as mock_fetch:
            mock_fetch.return_value = {'suggestions': ['test suggestion']}
            with patch('builtins.open', side_effect=[FileNotFoundError(), mock_open().return_value]):
                copilot_suggestions.main()
                expected_empty_note = {'timestamp': None, 'improvement': {}, 'assessment': ''}
                mock_fetch.assert_called_once_with(expected_empty_note)
    def test_json_response_parsing(self):
        os.environ['AZURE_OPENAI_ENDPOINT'] = 'https://test.openai.azure.com/'
        os.environ['AZURE_OPENAI_KEY'] = 'test-key'
        os.environ['AZURE_OPENAI_DEPLOYMENT'] = 'gpt-4'
        with patch('copilot_suggestions.requests.post') as mock_post:
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = {'choices': [{'message': {'content': '{"suggestions": ["test"], "next_focus": "focus"}'}}]}
            mock_post.return_value = mock_response
            result = copilot_suggestions.fetch_suggestions_from_azure_openai({'test': 'data'})
            expected = {'suggestions': ['test'], 'next_focus': 'focus'}
            self.assertEqual(result, expected)
        with patch('copilot_suggestions.requests.post') as mock_post:
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = {'choices': [{'message': {'content': 'This is not JSON'}}]}
            mock_post.return_value = mock_response
            result = copilot_suggestions.fetch_suggestions_from_azure_openai({'test': 'data'})
            self.assertIn('suggestions', result)
            self.assertEqual(result['suggestions'], ['This is not JSON'])
            self.assertIn('next_focus', result)
            self.assertIn('source', result)
            self.assertEqual(result['source'], 'azure_openai')
if __name__ == '__main__':
    unittest.main()