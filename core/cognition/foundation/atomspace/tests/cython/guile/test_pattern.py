from unittest import TestCase
from opencog.atomspace import AtomSpace, Atom
from opencog.type_constructors import TruthValue
from opencog.atomspace import types, is_a, get_type, get_type_name
from opencog.scheme import scheme_eval, scheme_eval_h
import os
shared_space = AtomSpace()
class SchemeTest(TestCase):
    def setUp(self):
        global shared_space
        self.space = shared_space
        scheme_eval(self.space, '(add-to-load-path "' + os.environ['PROJECT_SOURCE_DIR'] + '")')
        scheme_eval(self.space, '(add-to-load-path "' + os.environ['PROJECT_SOURCE_DIR'] + '/opencog/scm")')
    def tearDown(self):
        pass
    def test_a_load_core_types(self):
        scheme_eval(self.space, '(use-modules (opencog))')
    def test_b_load_file(self):
        print('Enter load-file test\n')
        status = scheme_eval(self.space, '(load-from-path "tests/cython/guile/basic_unify.scm")')
        self.assertTrue(status)
        print('Loaded file\n')
        a1 = self.space.add_node(types.ConceptNode, 'hello')
        self.assertTrue(a1)
        print('Added atom\n')
        expected = TruthValue(0.5, 0.5)
        self.assertEqual(a1.tv, expected)
        print(f'Got={str(a1.tv)} expected={str(expected)}')
    def test_c_gc(self):
        print('Enter garbage-collection-test\n')
        status = scheme_eval(self.space, '(define n 0)')
        self.assertTrue(status)
        status = scheme_eval(self.space, '\n            (for-each\n                (lambda (y)\n                    (let* ((bigstr (list->string (map\n                                (lambda (x)\n                                    (integer->char (+ 48 (modulo (+ x y) 79))))\n                                (iota 900))))\n                           (biglst (string->list bigstr))\n                           (revstr (reverse-list->string biglst)))\n                        (set! n (+ 1 n))))\n                    (iota 2000))')
        self.assertTrue(status)
        status = scheme_eval(self.space, '(gc-stats)')
        self.assertTrue(status)
        print('Finish garbage-collection-test\n')
    def test_d_eval(self):
        basic = scheme_eval_h(self.space, '(ConceptNode "whatever" (stv 0.5 0.5))')
        a1 = self.space.add_node(types.ConceptNode, 'whatever')
        self.assertTrue(a1)
        expected = TruthValue(0.5, 0.5)
        self.assertEqual(a1.tv, expected)
        self.assertEqual(a1, basic)
        again = scheme_eval_h(self.space, 'wobbly')
        a2 = self.space.add_node(types.ConceptNode, 'wobbly')
        self.assertTrue(a2)
        self.assertEqual(a2, again)
    def test_unifier(self):
        scheme_eval(self.space, '(use-modules (opencog exec))')
        question = scheme_eval_h(self.space, 'find-animals')
        self.assertTrue(question)
        print(('\nThe question is:', question))
        answer = scheme_eval_h(self.space, '(cog-execute! find-animals)')
        self.assertTrue(answer)
        print(('\nThe answer is:', answer))
        self.assertEqual(answer.type, types.SetLink)
        self.assertEqual(answer.arity, 3)