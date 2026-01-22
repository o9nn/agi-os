(use-modules (opencog ure))
(ure-logger-set-level! "debug")
(load "kb.scm")
(load "pln-config.scm")
(pln-fc (SetLink people-telling-the-truth-are-honest
                 friends-tend-to-be-honest
                 human-acquainted-tend-to-become-friends
                 people-telling-jokes-are-funny
                 funny-is-loosely-equivalent-to-amusing))