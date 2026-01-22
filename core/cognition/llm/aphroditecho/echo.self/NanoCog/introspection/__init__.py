__version__ = '0.1.0'
from .atomspace_client import AtomSpaceClient
def create_mock_data():
    client = AtomSpaceClient('http://localhost:8080/api/v1')
    return client.mock_get_cognitive_state()
def analyze_atomspace(endpoint, include_bottlenecks=True, include_recommendations=True):
    client = AtomSpaceClient(endpoint)
    if not client.test_connection():
        return None
    return client.generate_introspection_report(include_bottlenecks=include_bottlenecks, include_recommendations=include_recommendations)