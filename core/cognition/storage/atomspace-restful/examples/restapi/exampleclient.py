__author__ = 'Cosmo Harrigan'
from requests import *
import json
IP_ADDRESS = '127.0.0.1'
PORT = '5000'
uri = 'http://' + IP_ADDRESS + ':' + PORT + '/api/v1.1/'
headers = {'content-type': 'application/json'}
def pprint(call, contents):
    print('\n' + call.request.method + ' ' + call.request.path_url + ':')
    print(json.dumps(contents, indent=2))
truthvalue = {'type': 'simple', 'details': {'strength': 0.08, 'count': 0.2}}
atom = {'type': 'ConceptNode', 'name': 'giant_frog', 'truthvalue': truthvalue}
post_response = post(uri + 'atoms', data=json.dumps(atom), headers=headers)
post_result = post_response.json()
pprint(post_response, post_result)
'\nPOST /api/v1.1/atoms:\n{\n  "outgoing": [],\n  "incoming": [],\n  "name": "giant_frog",\n  "truthvalue": {\n    "type": "simple",\n    "details": {\n      "count": "0.20000000298023224",\n      "confidence": "0.0002499375259503722",\n      "strength": "0.07999999821186066"\n    }\n  },\n  "attentionvalue": {\n    "lti": 0,\n    "sti": 0,\n    "vlti": false\n  },\n  "handle": 57,\n  "type": "ConceptNode"\n}\n'
handle_node_1 = post_result['atoms']['handle']
get_response = get(uri + 'atoms/' + str(handle_node_1))
get_result = get_response.json()['result']['atoms']
pprint(get_response, get_result)
'\nGET /api/v1.1/atoms/57:\n{\n  "outgoing": [],\n  "incoming": [],\n  "name": "giant_frog",\n  "truthvalue": {\n    "type": "simple",\n    "details": {\n      "count": "0.20000000298023224",\n      "confidence": "0.0002499375259503722",\n      "strength": "0.07999999821186066"\n    }\n  },\n  "attentionvalue": {\n    "lti": 0,\n    "sti": 0,\n    "vlti": false\n  },\n  "handle": 57,\n  "type": "ConceptNode"\n}\n'
name = post_result['atoms']['name']
get_response = get(uri + 'atoms', params={'name': name})
get_result = get_response.json()['result']['atoms'][0]
pprint(get_response, get_result)
'\nGET /api/v1.1/atoms?name=giant_frog:\n{\n  "outgoing": [],\n  "incoming": [],\n  "name": "giant_frog",\n  "truthvalue": {\n    "type": "simple",\n    "details": {\n      "count": "0.20000000298023224",\n      "confidence": "0.0002499375259503722",\n      "strength": "0.07999999821186066"\n    }\n  },\n  "attentionvalue": {\n    "lti": 0,\n    "sti": 0,\n    "vlti": false\n  },\n  "handle": 57,\n  "type": "ConceptNode"\n}\n'
type = post_result['atoms']['type']
get_response = get(uri + 'atoms', params={'name': name, 'type': type})
get_result = get_response.json()['result']['atoms'][0]
pprint(get_response, get_result)
'\nGET /api/v1.1/atoms?name=giant_frog&type=ConceptNode:\n{\n  "outgoing": [],\n  "incoming": [],\n  "name": "giant_frog",\n  "truthvalue": {\n    "type": "simple",\n    "details": {\n      "count": "0.20000000298023224",\n      "confidence": "0.0002499375259503722",\n      "strength": "0.07999999821186066"\n    }\n  },\n  "attentionvalue": {\n    "lti": 0,\n    "sti": 0,\n    "vlti": false\n  },\n  "handle": 57,\n  "type": "ConceptNode"\n}\n'
truthvalue = {'type': 'simple', 'details': {'strength': 0.2, 'count': 0.5}}
atom = {'type': 'ConceptNode', 'name': 'animal', 'truthvalue': truthvalue}
post_response = post(uri + 'atoms', data=json.dumps(atom), headers=headers)
post_result = post_response.json()['atoms']
handle_node_2 = post_result['handle']
pprint(post_response, post_result)
'\nPOST /api/v1.1/atoms:\n{\n  "outgoing": [],\n  "incoming": [],\n  "name": "animal",\n  "truthvalue": {\n    "type": "simple",\n    "details": {\n      "count": "0.5",\n      "confidence": "0.0006246096454560757",\n      "strength": "0.20000000298023224"\n    }\n  },\n  "attentionvalue": {\n    "lti": 0,\n    "sti": 0,\n    "vlti": false\n  },\n  "handle": 58,\n  "type": "ConceptNode"\n}\n'
truthvalue = {'type': 'simple', 'details': {'strength': 0.5, 'count': 0.4}}
atom = {'type': 'InheritanceLink', 'truthvalue': truthvalue, 'outgoing': [handle_node_1, handle_node_2]}
post_response = post(uri + 'atoms', data=json.dumps(atom), headers=headers)
post_result = post_response.json()['atoms']
pprint(post_response, post_result)
'\nPOST /api/v1.1/atoms:\n{\n  "outgoing": [\n    57,\n    58\n  ],\n  "incoming": [],\n  "name": "",\n  "truthvalue": {\n    "type": "simple",\n    "details": {\n      "count": "0.4000000059604645",\n      "confidence": "0.0004997501382604241",\n      "strength": "0.5"\n    }\n  },\n  "attentionvalue": {\n    "lti": 0,\n    "sti": 0,\n    "vlti": false\n  },\n  "handle": 59,\n  "type": "InheritanceLink"\n}\n'
truthvalue = {'type': 'simple', 'details': {'strength': 0.006, 'count': 0.8}}
attentionvalue = {'sti': 5, 'lti': 3, 'vlti': True}
atom_update = {'truthvalue': truthvalue, 'attentionvalue': attentionvalue}
put_response = put(uri + 'atoms/' + str(handle_node_1), data=json.dumps(atom_update), headers=headers)
put_result = put_response.json()['atoms']
pprint(put_response, put_result)
'\nPUT /api/v1.1/atoms/76:\n{\n  "outgoing": [],\n  "incoming": [\n    77\n  ],\n  "name": "giant_frog",\n  "truthvalue": {\n    "type": "simple",\n    "details": {\n      "count": "0.800000011920929",\n      "confidence": "0.0009990009712055326",\n      "strength": "0.006000000052154064"\n    }\n  },\n  "attentionvalue": {\n    "lti": 3,\n    "sti": 5,\n    "vlti": true\n  },\n  "handle": 76,\n  "type": "ConceptNode"\n}\n'
delete_response = delete(uri + 'atoms/' + str(handle_node_1))
delete_result = delete_response.json()['result']
pprint(delete_response, delete_result)
'\nDELETE /api/v1.1/atoms/76:\n{\n  "handle": 76,\n  "success": true\n}\n'