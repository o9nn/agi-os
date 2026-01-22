__author__ = 'Cosmo Harrigan'
from flask import json, current_app
from flask_restful import Resource, reqparse
from opencog.web.api.mappers import *
from flask_restful.utils import cors
from flask_restful_swagger import swagger
class TypesAPI(Resource):
    def __init__(self):
        self.reqparse = reqparse.RequestParser()
        self.reqparse.add_argument('callback', type=str, location='args')
        super(TypesAPI, self).__init__()
    @cors.crossdomain(origin='*')
    @swagger.operation(notes='\nReturns a JSON representation of a list of valid atom types\n\n<p>Example:\n\n<pre>\n{"types": ["TrueLink", "NumberNode", "OrLink",\n  "PrepositionalRelationshipNode"]}\n</pre>\n', responseClass='response', nickname='get', parameters=[], responseMessages=[{'code': 200, 'message': 'Returned list of valid atom types'}])
    def get(self):
        json_data = {'types': filter(lambda x: not x.startswith('__') and (not x.endswith('__')) and (not x == 'NO_TYPE'), types.__dict__.keys())}
        args = self.reqparse.parse_args()
        callback = args.get('callback')
        if callback is not None:
            response = str(callback) + '(' + json.dumps(json_data) + ');'
            return current_app.response_class(response, mimetype='application/javascript')
        else:
            return current_app.response_class(json.dumps(json_data), mimetype='application/json')