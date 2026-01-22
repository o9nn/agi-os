from opencog.atomspace import *
from opencog.type_constructors import *
from opencog.vision import *
asp = AtomSpace()
set_default_atomspace(asp)
img_node = Image('example_image.png')
img_collection = Concept('Image Collection')
key_1 = Concept('image#1')
img_blur = ImageBlur(img_node, NumberNode('10'))
result = img_blur.execute()
img_collection.set_value(key_1, result)
img_write = ImageWriteLink(ValueOfLink(img_collection, key_1), ConceptNode('example_output_py.png'))
img_write.execute()
print('Image processed successfully. Output saved to example_output_py.png')