import pytest
from fastapi.testclient import TestClient
from unittest.mock import MagicMock
from aphrodite.endpoints.deep_tree_echo.app_factory import create_app
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
class TestServerSideTemplates:
    def setup_method(self):
        self.config = DTESNConfig(max_membrane_depth=4, esn_reservoir_size=512, bseries_max_order=8)
        self.mock_engine = MagicMock()
        self.app = create_app(engine=self.mock_engine, config=self.config)
        self.client = TestClient(self.app)
    def test_templates_directory_exists(self):
        from aphrodite.endpoints.deep_tree_echo.app_factory import TEMPLATES_DIR
        assert TEMPLATES_DIR.exists(), f'Templates directory should exist at {TEMPLATES_DIR}'
        assert TEMPLATES_DIR.is_dir(), 'Templates path should be a directory'
    def test_base_template_exists(self):
        from aphrodite.endpoints.deep_tree_echo.app_factory import TEMPLATES_DIR
        base_template = TEMPLATES_DIR / 'base.html'
        assert base_template.exists(), 'Base template should exist'
    def test_app_has_templates(self):
        assert hasattr(self.app.state, 'templates'), 'App state should have templates'
        assert self.app.state.templates is not None, 'Templates should be initialized'
    def test_health_check_includes_templates_status(self):
        response = self.client.get('/health')
        assert response.status_code == 200
        data = response.json()
        assert 'templates_available' in data
        assert data['templates_available'] is True
    def test_json_response_default(self):
        response = self.client.get('/deep_tree_echo/')
        assert response.status_code == 200
        assert response.headers['content-type'] == 'application/json'
        data = response.json()
        assert data['service'] == 'Deep Tree Echo API'
        assert data['server_rendered'] is True
    def test_html_response_with_accept_header(self):
        headers = {'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'}
        response = self.client.get('/deep_tree_echo/', headers=headers)
        assert response.status_code == 200
        assert 'text/html' in response.headers['content-type']
        assert b'<!DOCTYPE html>' in response.content
        assert b'Deep Tree Echo' in response.content
    def test_status_endpoint_json(self):
        response = self.client.get('/deep_tree_echo/status')
        assert response.status_code == 200
        assert response.headers['content-type'] == 'application/json'
        data = response.json()
        assert data['dtesn_system'] == 'operational'
        assert data['server_side'] is True
    def test_status_endpoint_html(self):
        headers = {'Accept': 'text/html'}
        response = self.client.get('/deep_tree_echo/status', headers=headers)
        assert response.status_code == 200
        assert 'text/html' in response.headers['content-type']
        assert b'System Status' in response.content
        assert b'DTESN System' in response.content
    def test_membrane_info_endpoint_html(self):
        headers = {'Accept': 'text/html'}
        response = self.client.get('/deep_tree_echo/membrane_info', headers=headers)
        assert response.status_code == 200
        assert 'text/html' in response.headers['content-type']
        assert b'Membrane Information' in response.content
        assert b'P-System' in response.content
        assert b'A000081' in response.content
    def test_esn_state_endpoint_html(self):
        headers = {'Accept': 'text/html'}
        response = self.client.get('/deep_tree_echo/esn_state', headers=headers)
        assert response.status_code == 200
        assert 'text/html' in response.headers['content-type']
        assert b'Echo State Network' in response.content
        assert b'Reservoir Configuration' in response.content
    def test_template_inheritance(self):
        headers = {'Accept': 'text/html'}
        response = self.client.get('/deep_tree_echo/', headers=headers)
        content = response.content.decode()
        assert '<!DOCTYPE html>' in content
        assert '<title>Deep Tree Echo - Home</title>' in content
        assert 'Deep Tree Echo' in content
        assert 'Server-side rendered with Jinja2' in content
    def test_server_side_data_binding(self):
        headers = {'Accept': 'text/html'}
        response = self.client.get('/deep_tree_echo/status', headers=headers)
        content = response.content.decode()
        assert str(self.config.max_membrane_depth) in content
        assert str(self.config.esn_reservoir_size) in content
        assert str(self.config.bseries_max_order) in content
    def test_content_negotiation_consistency(self):
        endpoints = ['/deep_tree_echo/', '/deep_tree_echo/status', '/deep_tree_echo/membrane_info']
        for endpoint in endpoints:
            json_response = self.client.get(endpoint)
            assert json_response.status_code == 200
            assert 'application/json' in json_response.headers['content-type']
            html_response = self.client.get(endpoint, headers={'Accept': 'text/html'})
            assert html_response.status_code == 200
            assert 'text/html' in html_response.headers['content-type']
            assert b'<!DOCTYPE html>' in html_response.content
            xml_response = self.client.get(endpoint, headers={'Accept': 'application/xml'})
            assert xml_response.status_code == 200
            assert 'application/xml' in xml_response.headers['content-type']
            assert b'<?xml version=' in xml_response.content
    def test_xml_response_format(self):
        headers = {'Accept': 'application/xml'}
        response = self.client.get('/deep_tree_echo/', headers=headers)
        assert response.status_code == 200
        assert 'application/xml' in response.headers['content-type']
        content = response.content.decode('utf-8')
        assert '<?xml version="1.0" encoding="UTF-8"?>' in content
        assert '<deep_tree_echo_api>' in content
        assert '</deep_tree_echo_api>' in content
        assert '<service>Deep Tree Echo API</service>' in content
        assert '<version>1.0.0</version>' in content
        assert '<server_rendered>True</server_rendered>' in content
    def test_xml_response_status_endpoint(self):
        headers = {'Accept': 'application/xml'}
        response = self.client.get('/deep_tree_echo/status', headers=headers)
        assert response.status_code == 200
        assert 'application/xml' in response.headers['content-type']
        content = response.content.decode('utf-8')
        assert '<dtesn_status>' in content
        assert '</dtesn_status>' in content
        assert '<dtesn_system>operational</dtesn_system>' in content
        assert '<server_side>True</server_side>' in content
    def test_content_type_quality_negotiation(self):
        headers = {'Accept': 'application/xml;q=0.9,application/json;q=0.8'}
        response = self.client.get('/deep_tree_echo/', headers=headers)
        assert 'application/xml' in response.headers['content-type']
        headers = {'Accept': 'application/json;q=0.9,application/xml;q=0.8'}
        response = self.client.get('/deep_tree_echo/', headers=headers)
        assert 'application/json' in response.headers['content-type']
        headers = {'Accept': 'text/html;q=1.0,application/xml;q=0.9,application/json;q=0.8'}
        response = self.client.get('/deep_tree_echo/', headers=headers)
        assert 'text/html' in response.headers['content-type']
    def test_fallback_to_default_json(self):
        headers = {'Accept': 'application/pdf,image/jpeg'}
        response = self.client.get('/deep_tree_echo/', headers=headers)
        assert response.status_code == 200
        assert 'application/json' in response.headers['content-type']
    def test_wildcard_accept_headers(self):
        headers = {'Accept': '*/*'}
        response = self.client.get('/deep_tree_echo/', headers=headers)
        assert 'application/json' in response.headers['content-type']
        headers = {'Accept': 'application/*'}
        response = self.client.get('/deep_tree_echo/', headers=headers)
        assert 'application/json' in response.headers['content-type']
if __name__ == '__main__':
    pytest.main([__file__, '-v'])