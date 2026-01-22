__author__ = 'Cosmo Harrigan'
from flask import Flask, request
from flask_restful import Api
from flask_cors import CORS
from opencog.web.api.apiatomcollection import *
from opencog.web.api.apitypes import *
from opencog.web.api.apishell import *
from opencog.web.api.apischeme import *
from opencog.web.api.apighost import *
from flask_restful_swagger import swagger
class RESTAPI(object):
    def __init__(self, atomspace):
        self.atomspace = atomspace
        self.app = Flask(__name__, static_url_path='')
        self.api = swagger.docs(Api(self.app), apiVersion='1.1', api_spec_url='/api/v1.1/spec')
        self.cors = CORS(self.app, resources={'/api/*': {'origins': '*'}})
        atom_collection_api = AtomCollectionAPI.new(self.atomspace)
        atom_types_api = TypesAPI
        shell_api = ShellAPI
        scheme_api = SchemeAPI.new(self.atomspace)
        ghost_api = GhostApi.new(self.atomspace)
        self.api.decorators = [cors.crossdomain(origin='*', automatic_options=False)]
        self.api.add_resource(atom_collection_api, '/api/v1.1/atoms', '/api/v1.1/atoms/<int:id>', endpoint='atoms')
        self.api.add_resource(atom_types_api, '/api/v1.1/types', endpoint='types')
        self.api.add_resource(shell_api, '/api/v1.1/shell', endpoint='shell')
        self.api.add_resource(scheme_api, '/api/v1.1/scheme', endpoint='scheme')
        self.api.add_resource(ghost_api, '/api/v1.1/ghost', endpoint='ghost')
    def run(self, host='127.0.0.1', port=5000):
        self.app.run(debug=False, host=host, port=port)
    def test(self):
        return self.app.test_client()