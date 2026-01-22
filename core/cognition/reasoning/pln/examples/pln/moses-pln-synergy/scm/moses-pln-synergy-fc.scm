(load "moses-model.scm")
(load "background-knowledge.scm")
(load "pln-fc-config.scm")
(pln-fc (SetLink if-X-takes-Y-and-Y-contains-Z-then-X-takes-Z
                 take-treatment-1-X-is-equivalent-to-take-X-treatment-1
                 take-compound-A-X-is-equivalent-to-take-X-compound-A
                 being-well-hydrated-tends-to-speed-up-injury-recovery))