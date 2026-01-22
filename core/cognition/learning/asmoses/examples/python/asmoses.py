__author__ = 'Cosmo Harrigan'
from opencog.pyasmoses import *
moses = moses()
input_data = [[0, 0, 0], [1, 1, 0], [1, 0, 1], [0, 1, 1]]
output = moses.run(input=input_data, python=True)
print('\nTraining data:\n\n{0}'.format(input_data))
print('\nThe following program was learned:\n-------------------\n\n{0}'.format(output[0].program.decode('utf-8')))
model = output[0].eval
print('-------------------\nTesting model on data:\n')
print('[0, 1]: {0}'.format(model([0, 1])))
print('[1, 1]: {0}'.format(model([1, 1])))
print('\n-------------------\nEquivalent Combo program:\n')
print(moses.run(input=input_data, python=False)[0].program.decode('utf-8'))