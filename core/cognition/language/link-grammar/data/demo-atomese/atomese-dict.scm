(Section
(Word "###LEFT-WALL###")
(ConnectorSeq
(Connector (Word "level") (ConnectorDir "+"))))
(Section
(Word "level")
(ConnectorSeq
(Connector (Word "###LEFT-WALL###") (ConnectorDir "-"))
(Connector (Word "playing") (ConnectorDir "+"))))
(Section
(Word "playing")
(ConnectorSeq
(Connector (Word "level") (ConnectorDir "-"))
(Connector (Word "field") (ConnectorDir "+"))))
(Section
(Word "field")
(ConnectorSeq
(Connector (Word "playing") (ConnectorDir "-"))))
(Section
(Word "###LEFT-WALL###")
(ConnectorSeq
(Connector (WordClass "person") (ConnectorDir "+"))))
(Member (Word "Mary") (WordClass "person"))
(Member (Word "John") (WordClass "person"))
(Member (Word "Olga") (WordClass "person"))
(Member (Word "Sasha") (WordClass "person"))
(Section
(WordClass "person")
(ConnectorSeq
(Connector (Word "###LEFT-WALL###") (ConnectorDir "-"))
(Connector (WordClass "verb") (ConnectorDir "+"))))
(Member (Word "saw") (WordClass "verb"))
(Member (Word "heard") (WordClass "verb"))
(Section
(WordClass "verb")
(ConnectorSeq
(Connector (WordClass "person") (ConnectorDir "-"))
(Connector (WordClass "animal") (ConnectorDir "+"))))
(Member (Word "bird") (WordClass "animal"))
(Member (Word "cat") (WordClass "animal"))
(Member (Word "dog") (WordClass "animal"))
(Section
(WordClass "animal")
(ConnectorSeq
(Connector (WordClass "verb") (ConnectorDir "-"))
(Connector (WordClass "determiner") (ConnectorDir "-"))))
(Member (Word "the") (WordClass "determiner"))
(Member (Word "a") (WordClass "determiner"))
(Member (Word "this") (WordClass "determiner"))
(Member (Word "that") (WordClass "determiner"))
(Section
(WordClass "determiner")
(ConnectorSeq
(Connector (WordClass "animal") (ConnectorDir "+"))))
(cog-set-value!
(Section
(WordClass "determiner")
(ConnectorSeq
(Connector (WordClass "animal") (ConnectorDir "+"))))
(Predicate "*-Mutual Info Key cover-section")
(FloatValue 0 3.1))
(cog-set-value!
(Section
(WordClass "verb")
(ConnectorSeq
(Connector (WordClass "person") (ConnectorDir "-"))
(Connector (WordClass "animal") (ConnectorDir "+"))))
(Predicate "*-Mutual Info Key cover-section")
(FloatValue 555 2.4))
(Section
(Word "###LEFT-WALL###")
(ConnectorSeq
(Connector (Word "fountain") (ConnectorDir "+"))))
(Section
(Word "fountain")
(ConnectorSeq
(Connector (Word "###LEFT-WALL###") (ConnectorDir "-"))
(Connector (Word "1") (ConnectorDir "-"))
(Connector (Word "2") (ConnectorDir "-"))
(Connector (Word "3") (ConnectorDir "-"))
(Connector (Word "4") (ConnectorDir "+"))
(Connector (Word "5") (ConnectorDir "+"))
(Connector (Word "6") (ConnectorDir "+"))))
(Section (Word "1")
(ConnectorSeq (Connector (Word "fountain") (ConnectorDir "+"))))
(Section (Word "2")
(ConnectorSeq (Connector (Word "fountain") (ConnectorDir "+"))))
(Section (Word "3")
(ConnectorSeq (Connector (Word "fountain") (ConnectorDir "+"))))
(Section (Word "4")
(ConnectorSeq (Connector (Word "fountain") (ConnectorDir "-"))))
(Section (Word "5")
(ConnectorSeq (Connector (Word "fountain") (ConnectorDir "-"))))
(Section (Word "6")
(ConnectorSeq (Connector (Word "fountain") (ConnectorDir "-"))))
(Evaluation (Predicate "wrdpr") (List (Word "###LEFT-WALL###") (Word "jumped")))
(Evaluation (Predicate "wrdpr") (List (Word "###LEFT-WALL###") (Word "fish")))
(Evaluation (Predicate "wrdpr") (List (Word "the") (Word "fish")))
(Evaluation (Predicate "wrdpr") (List (Word "fish") (Word "jumped")))
(Evaluation (Predicate "wrdpr") (List (Word "jumped") (Word "out")))
(Evaluation (Predicate "wrdpr") (List (Word "out") (Word "water")))
(Evaluation (Predicate "wrdpr") (List (Word "of") (Word "water")))
(Evaluation (Predicate "wrdpr") (List (Word "the") (Word "water")))
(Evaluation (Predicate "wrdpr") (List (Word "out") (Word "of")))
(cog-set-value!
(Evaluation (Predicate "wrdpr") (List (Word "the") (Word "fish")))
(Predicate "*-Mutual Info Key-*")
(FloatValue 0 3.1))
(cog-set-value!
(Evaluation (Predicate "wrdpr") (List (Word "jumped") (Word "out")))
(Predicate "*-Mutual Info Key-*")
(FloatValue 0 4.2))