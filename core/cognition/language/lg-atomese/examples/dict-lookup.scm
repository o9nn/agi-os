(use-modules (opencog) (opencog exec))
(use-modules (opencog nlp) (opencog nlp lg-dict))
(lg-dict-entry (Word "yikes"))
(cog-execute! (LgDictEntry (Word "yikes") (LgDictNode "en")))
(cog-incoming-by-type (Word "yikes") 'LgDisjunct)
(cog-execute! (LgDictEntry (Word "доктор") (LgDictNode "ru")))
(cog-incoming-by-type (Word "доктор") 'LgDisjunct)
(cog-evaluate! (LgHaveDictEntry (Word "доктор") (LgDictNode "ru")))
(cog-evaluate! (LgHaveDictEntry (Word "дasdf") (LgDictNode "ru")))
,d lg-conn-linkable?
(lg-conn-linkable?
(LgConnector (LgConn "Ss") (LgConnDir "+"))
(LgConnector (LgConn "S") (LgConnDir "-")))
(lg-conn-type-match?
(LgConnector (LgConn "Ss") (LgConnDir "+"))
(LgConnector (LgConn "S") (LgConnDir "+")))