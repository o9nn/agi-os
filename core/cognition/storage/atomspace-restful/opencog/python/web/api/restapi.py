__author__ = 'Cosmo Harrigan'
import socket
import opencog.cogserver
from opencog.web.api.apimain import RESTAPI
from threading import Thread
IP_ADDRESS = '0.0.0.0'
PORT = 5000
class Start(opencog.cogserver.Request):
    summary = 'Start the OpenCog REST API'
    description = 'Usage: restapi.Start\n\nStarts the OpenCog REST API. This will provide a REST interface to the Atomspace,\nallowing you to create, read, update and delete atoms across the network using\nHTTP requests/responses with JSON-formatted data.\n\nDefault endpoint: http://127.0.0.1:5000/api/v1.1/\nExample request: http://127.0.0.1:5000/api/v1.1/atoms?type=ConceptNode'
    def __init__(self):
        self.atomspace = None
    def run(self, args, atomspace):
        self.atomspace = atomspace
        '\n        make a daemon thread so that it can be interrupted\n        '
        thread = Thread(target=self.invoke)
        thread.setDaemon(True)
        thread.start()
        print('REST API is now running in a separate daemon thread.')
    def invoke(self):
        self.api = RESTAPI(self.atomspace)
        try_again = True
        while try_again:
            try_again = False
            try:
                self.api.run(host=IP_ADDRESS, port=PORT)
            except socket.error as e:
                try_again = True