(define-module (opencog eva-behavior))
(use-modules (opencog) (opencog nlp))
(use-modules (opencog eva-model))
(load "eva-behavior/cfg-tools.scm")
(load "eva-behavior/cfg-eva.scm")
(load "eva-behavior/express.scm")
(load "eva-behavior/primitives.scm")
(load "eva-behavior/movement-api.scm")
(load "eva-behavior/behavior.scm")
(load "eva-behavior/face-priority.scm")
(load "eva-behavior/orchestrate.scm")
(define-public (load-eva-config)
"
load-eva-config
Load behavior paramters appropriage for the Eva blender model.
"
(load "eva-behavior/cfg-eva.scm"))
(define-public (load-sophia-config)
"
load-sophia-config
Load behavior paramters appropriage for the Sophia blender model.
"
(load "eva-behavior/cfg-sophia.scm"))