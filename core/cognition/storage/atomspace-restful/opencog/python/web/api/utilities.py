from opencog.atomspace import types
def count_to_confidence(count):
    default_k = 800.0
    return count / (count + default_k)
def confidence_to_count(conf):
    default_k = 800.0
    return default_k * conf / (1.0 - conf)
def get_atoms_by_name(z_type, name, atomspace):
    return filter(lambda x: x.name == name, atomspace.get_atoms_by_type(z_type))