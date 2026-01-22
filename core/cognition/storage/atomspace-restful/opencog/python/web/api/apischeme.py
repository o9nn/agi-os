__author__ = 'Cosmo Harrigan'
from flask import abort, jsonify
from flask_restful import Resource, reqparse
from opencog.scheme_wrapper import scheme_eval, __init__
from flask_restful_swagger import swagger
COGSERVER_PORT = 17001
class SchemeAPI(Resource):
    @classmethod
    def new(cls, atomspace):
        cls.atomspace = atomspace
        return cls
    def __init__(self):
        self.reqparse = reqparse.RequestParser()
        self.reqparse.add_argument('command', type=str, location='args')
        super(SchemeAPI, self).__init__()
    @swagger.operation(notes='\nInclude a JSON object with the POST request containing the command\nin a field named "command"\n\n<p>Example command:\n\n<pre>\n{\'command\': \'(cog-set-af-boundary! 100)\'}\n</pre>\n\n<p>Returns:\n\n<p>A JSON object containing the Scheme-formatted result of the command in\na field named "response".\n\n<p>Example response:\n\n<pre>\n{\'response\': \'100\n\'}\n</pre>\n\n<p>Note that in this API, the request is processed synchronously. It\nblocks until the request has finished.\n\n<p>This functionality is implemented as a POST method because it can\ncause side-effects.', responseClass='response', nickname='post', parameters=[{'name': 'command', 'description': 'Scheme command', 'required': True, 'allowMultiple': False, 'dataType': 'string', 'paramType': 'body'}], responseMessages=[{'code': 200, 'message': 'Scheme command executed successfully'}, {'code': 400, 'message': 'Invalid request: Required parameter command missing'}])
    def post(self):
        data = reqparse.request.get_json()
        if 'command' in data:
            response = scheme_eval(self.atomspace, data['command'])
        else:
            abort(400, 'Invalid request: required parameter command is missing')
        return jsonify({'response': response})