__author__ = 'Cosmo Harrigan'
from nose.tools import *
import json
from opencog.atomspace import *
try:
    from opencog.web.api.apimain import RESTAPI
    from opencog.web.api.utilities import count_to_confidence
except ImportError:
    import unittest
    raise unittest.SkipTest('ImportError exception: make sure the required dependencies are installed.')
class TestRESTApi:
    def setUp(self):
        self.uri = '/api/v1.1/'
        self.headers = {'content-type': 'application/json'}
        self.atomspace = AtomSpace()
        self.animal = self.atomspace.add_node(types.ConceptNode, 'animal', TruthValue(0.1, 0.0011860914528369904))
        self.bird = self.atomspace.add_node(types.ConceptNode, 'bird', TruthValue(0.01, 0.0011237357975915074))
        self.swan = self.atomspace.add_node(types.ConceptNode, 'swan', TruthValue(0.001, 0.0011237357975915074))
        self.frog = self.atomspace.add_node(types.ConceptNode, 'frog', TruthValue(0.001, 0.7142857313156128))
        self.swan_bird = self.atomspace.add_link(types.InheritanceLink, [self.swan, self.bird], TruthValue(1, 0.0011237357975915074))
        self.bird_animal = self.atomspace.add_link(types.InheritanceLink, [self.bird, self.animal], TruthValue(1, 0.0011237357975915074))
        self.bird.sti = 9
        self.swan.sti = 9
        self.api = RESTAPI(self.atomspace)
        self.client = self.api.test()
    def tearDown(self):
        del self.api
        del self.client
    def mkatom(self, json_atom):
        post_response = self.client.post(self.uri + 'atoms', data=json.dumps(json_atom), headers=self.headers)
        post_result = json.loads(post_response.data)['atoms']
        return post_result
    def mkswan(self):
        return self.mkatom({'type': 'ConceptNode', 'name': 'swan', 'truthvalue': {'type': 'simple', 'details': {'strength': 0.001, 'count': 0.9}}})
    def mkbird(self):
        return self.mkatom({'type': 'ConceptNode', 'name': 'bird', 'truthvalue': {'type': 'simple', 'details': {'strength': 0.01, 'count': 0.9}}})
    def mkanimal(self):
        return self.mkatom({'type': 'ConceptNode', 'name': 'animal', 'truthvalue': {'type': 'simple', 'details': {'strength': 0.1, 'count': 0.95}}})
    def mkbird_animal(self):
        jbird = self.mkbird()
        janimal = self.mkanimal()
        return self.mkatom({'type': 'InheritanceLink', 'truthvalue': {'type': 'simple', 'details': {'strength': 1.0, 'count': 0.9}}, 'outgoing': [jbird['handle'], janimal['handle']]})
    def get_atom(self, handle):
        get_response_handle = self.client.get(self.uri + 'atoms/' + str(handle))
        get_result_handle = json.loads(get_response_handle.data)['result']['atoms'][0]
        return get_result_handle
    def test_a_post_and_get_node(self):
        truthvalue = {'type': 'simple', 'details': {'strength': 0.08, 'count': 0.2}}
        jatom = {'type': 'ConceptNode', 'name': 'giant_frog', 'truthvalue': truthvalue}
        post_response = self.client.post(self.uri + 'atoms', data=json.dumps(jatom), headers=self.headers)
        post_result = json.loads(post_response.data)['atoms']
        assert post_result['type'] == jatom['type']
        assert post_result['name'] == jatom['name']
        assert post_result['truthvalue']['type'] == truthvalue['type']
        assert_almost_equals(float(post_result['truthvalue']['details']['strength']), truthvalue['details']['strength'], places=5)
        assert_almost_equals(float(post_result['truthvalue']['details']['count']), truthvalue['details']['count'], places=5)
        frog = self.atomspace.add_node(types.ConceptNode, 'giant_frog', TruthValue(0.08, count_to_confidence(0.2)))
        atomspace_result = frog
        assert post_result['name'] == atomspace_result.name
        assert types.__dict__.get(post_result['type']) == atomspace_result.type
        assert TruthValue(float(post_result['truthvalue']['details']['strength']), count_to_confidence(float(post_result['truthvalue']['details']['count']))) == atomspace_result.tv
        handle = post_result['handle']
        get_response_handle = self.client.get(self.uri + 'atoms/' + str(handle))
        get_result_handle = json.loads(get_response_handle.data)['result']['atoms'][0]
        assert post_result == get_result_handle
        name = post_result['name']
        get_response_name = self.client.get(self.uri + 'atoms?name=' + name)
        get_result_name = json.loads(get_response_name.data)['result']['atoms'][0]
        assert post_result == get_result_name
        type = post_result['type']
        get_response_name_type = self.client.get(self.uri + 'atoms?name=' + name + '&type=' + type)
        get_result_name_type = json.loads(get_response_name_type.data)['result']['atoms'][0]
        assert post_result == get_result_name_type
    def test_b_post_and_get_link(self):
        jswan = self.mkswan()
        janimal = self.mkanimal()
        truthvalue = {'type': 'simple', 'details': {'strength': 0.5, 'count': 0.4}}
        atom = {'type': 'InheritanceLink', 'truthvalue': truthvalue, 'outgoing': [jswan['handle'], janimal['handle']]}
        post_response = self.client.post(self.uri + 'atoms', data=json.dumps(atom), headers=self.headers)
        post_result = json.loads(post_response.data)['atoms']
        assert post_result['type'] == atom['type']
        assert post_result['truthvalue']['type'] == truthvalue['type']
        assert_almost_equals(float(post_result['truthvalue']['details']['strength']), truthvalue['details']['strength'], places=5)
        assert_almost_equals(float(post_result['truthvalue']['details']['count']), truthvalue['details']['count'], places=5)
        assert jswan['handle'] in post_result['outgoing']
        assert janimal['handle'] in post_result['outgoing']
        swan_animal = self.atomspace.add_link(types.InheritanceLink, [self.swan, self.animal], TruthValue(0.5, count_to_confidence(0.4)))
        atomspace_result = swan_animal
        assert types.__dict__.get(post_result['type']) == atomspace_result.type
        assert TruthValue(float(post_result['truthvalue']['details']['strength']), count_to_confidence(float(post_result['truthvalue']['details']['count']))) == atomspace_result.tv
        handle = post_result['handle']
        get_response_handle = self.client.get(self.uri + 'atoms/' + str(handle))
        get_result_handle = json.loads(get_response_handle.data)['result']['atoms'][0]
        assert post_result == get_result_handle
        jswan = self.get_atom(jswan['handle'])
        janimal = self.get_atom(janimal['handle'])
        for h in post_result['outgoing']:
            assert post_result['handle'] in jswan['incoming']
            assert post_result['handle'] in janimal['incoming']
    def test_c_put_and_get_tv_av_node(self):
        jswan = self.mkswan()
        truthvalue = {'type': 'simple', 'details': {'strength': 0.005, 'count': 0.8}}
        attentionvalue = {'sti': 9, 'lti': 2, 'vlti': True}
        atom_update = {'truthvalue': truthvalue, 'attentionvalue': attentionvalue}
        put_response = self.client.put(self.uri + 'atoms/' + str(jswan['handle']), data=json.dumps(atom_update), headers=self.headers)
        put_result = json.loads(put_response.data)['atoms']
        assert put_result['handle'] == jswan['handle']
        assert_almost_equals(float(put_result['truthvalue']['details']['strength']), truthvalue['details']['strength'], places=5)
        assert_almost_equals(float(put_result['truthvalue']['details']['count']), truthvalue['details']['count'], places=5)
        assert put_result['attentionvalue']['sti'] == attentionvalue['sti']
        assert put_result['attentionvalue']['lti'] == attentionvalue['lti']
        assert put_result['attentionvalue']['vlti'] == attentionvalue['vlti']
        atomspace_result = self.swan
        assert types.__dict__.get(put_result['type']) == atomspace_result.type
        assert TruthValue(float(put_result['truthvalue']['details']['strength']), count_to_confidence(float(put_result['truthvalue']['details']['count']))) == atomspace_result.tv
        assert put_result['attentionvalue'] == atomspace_result.av
        get_response = self.client.get(self.uri + 'atoms/' + str(jswan['handle']))
        get_result = json.loads(get_response.data)['result']['atoms'][0]
        assert put_result == get_result
    def test_d_put_and_get_tv_av_link(self):
        jatom = self.mkbird_animal()
        truthvalue = {'type': 'simple', 'details': {'strength': 0.9, 'count': 0.95}}
        attentionvalue = {'sti': 6, 'lti': 3, 'vlti': True}
        atom_update = {'truthvalue': truthvalue, 'attentionvalue': attentionvalue}
        put_response = self.client.put(self.uri + 'atoms/' + str(jatom['handle']), data=json.dumps(atom_update), headers=self.headers)
        put_result = json.loads(put_response.data)['atoms']
        assert put_result['handle'] == jatom['handle']
        assert_almost_equals(float(put_result['truthvalue']['details']['strength']), truthvalue['details']['strength'], places=5)
        assert_almost_equals(float(put_result['truthvalue']['details']['count']), truthvalue['details']['count'], places=5)
        assert put_result['attentionvalue']['sti'] == attentionvalue['sti']
        assert put_result['attentionvalue']['lti'] == attentionvalue['lti']
        assert put_result['attentionvalue']['vlti'] == attentionvalue['vlti']
        atomspace_result = self.bird_animal
        assert types.__dict__.get(put_result['type']) == atomspace_result.type
        assert TruthValue(float(put_result['truthvalue']['details']['strength']), count_to_confidence(float(put_result['truthvalue']['details']['count']))) == atomspace_result.tv
        assert put_result['attentionvalue'] == atomspace_result.av
        get_response = self.client.get(self.uri + 'atoms/' + str(jatom['handle']))
        get_result = json.loads(get_response.data)['result']['atoms'][0]
        assert put_result == get_result
    def test_e_post_revise_existing_node(self):
        existing_atom = self.bird
        truthvalue = {'type': 'simple', 'details': {'strength': 0.1, 'count': 0.95}}
        atom = {'type': 'ConceptNode', 'name': 'bird', 'truthvalue': truthvalue}
        post_response = self.client.post(self.uri + 'atoms', data=json.dumps(atom), headers=self.headers)
        post_result = json.loads(post_response.data)['atoms']
        assert_almost_equals(float(post_result['truthvalue']['details']['strength']), truthvalue['details']['strength'], places=5)
        assert_almost_equals(float(post_result['truthvalue']['details']['count']), truthvalue['details']['count'], places=5)
        assert TruthValue(float(post_result['truthvalue']['details']['strength']), count_to_confidence(float(post_result['truthvalue']['details']['count']))) == existing_atom.tv
    def test_f_post_revise_existing_link(self):
        existing_atom = self.bird_animal
        jbird_animal = self.mkbird_animal()
        truthvalue = {'type': 'simple', 'details': {'strength': 0.1, 'count': 0.95}}
        outgoing = jbird_animal['outgoing']
        atom = {'type': 'InheritanceLink', 'truthvalue': truthvalue, 'outgoing': outgoing}
        post_response = self.client.post(self.uri + 'atoms', data=json.dumps(atom), headers=self.headers)
        post_result = json.loads(post_response.data)['atoms']
        assert post_result['outgoing'] == outgoing
        assert_almost_equals(float(post_result['truthvalue']['details']['strength']), truthvalue['details']['strength'], places=5)
        assert_almost_equals(float(post_result['truthvalue']['details']['count']), truthvalue['details']['count'], places=5)
        assert TruthValue(float(post_result['truthvalue']['details']['strength']), count_to_confidence(float(post_result['truthvalue']['details']['count']))) == existing_atom.tv
    def test_g_delete_node(self):
        jswan = self.mkswan()
        handle = jswan['handle']
        get_response = self.client.get(self.uri + 'atoms/' + str(handle))
        get_result = json.loads(get_response.data)['result']['atoms'][0]
        old_size = self.atomspace.size()
        delete_response = self.client.delete(self.uri + 'atoms/' + str(handle))
        delete_result = json.loads(delete_response.data)['result']
        assert delete_result['success']
        assert delete_result['handle'] == get_result['handle']
        new_size = self.atomspace.size()
        assert new_size + 2 == old_size
    def test_h_delete_link(self):
        jatom = self.mkbird_animal()
        handle = jatom['handle']
        get_response = self.client.get(self.uri + 'atoms/' + str(handle))
        get_result = json.loads(get_response.data)['result']['atoms'][0]
        old_size = self.atomspace.size()
        delete_response = self.client.delete(self.uri + 'atoms/' + str(handle))
        delete_result = json.loads(delete_response.data)['result']
        assert delete_result['success']
        assert delete_result['handle'] == get_result['handle']
        new_size = self.atomspace.size()
        assert new_size + 1 == old_size
    def test_j_get_types(self):
        get_response = self.client.get(self.uri + 'types')
        get_result = json.loads(get_response.data)['types']
        assert len(get_result) > 0
        assert get_result.__contains__('ConceptNode')
    def test_k_tv_filter(self):
        get_response = self.client.get(self.uri + 'atoms?tvStrengthMin=0.1')
        get_result = json.loads(get_response.data)['result']['atoms']
        assert len(get_result) == 3
        get_response = self.client.get(self.uri + 'atoms?tvConfidenceMin=0.7')
        get_result = json.loads(get_response.data)['result']['atoms']
        assert len(get_result) == 1
        get_response = self.client.get(self.uri + 'atoms?tvCountMin=2000')
        get_result = json.loads(get_response.data)['result']['atoms']
        assert len(get_result) == 1
    def test_l_include_incoming_outgoing(self):
        get_response = self.client.get(self.uri + 'atoms?filterby=stirange&stimin=1&includeIncoming=false')
        get_result = json.loads(get_response.data)['result']['atoms']
        assert len(get_result) == 2
        get_response = self.client.get(self.uri + 'atoms?filterby=stirange&stimin=1&includeIncoming=true')
        get_result = json.loads(get_response.data)['result']['atoms']
        assert len(get_result) == 4
        get_response = self.client.get(self.uri + 'atoms?filterby=stirange&stimin=1&includeIncoming=true&includeOutgoing=true')
        get_result = json.loads(get_response.data)['result']['atoms']
        assert len(get_result) == 5
    def test_m_scheme_command(self):
        pass
    def test_n_dot_export(self):
        try:
            from graph_description import dot
            get_response = self.client.get(self.uri + 'atoms?filterby=attentionalfocus&dot=True')
            get_result = json.loads(get_response.data)['result']
            assert get_result.startswith('// OpenCog Graph')
            assert 'digraph' in get_result
            assert 'swan' in get_result
            assert 'bird' in get_result
            assert get_result.count('label') == 2
        except ImportError:
            pass