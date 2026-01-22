(use-modules (opencog))
(use-modules (opencog exec))
(define bad-ester
(BindLink
(VariableList
(TypedVariable (Variable "$carboxyH1") (Type 'H))
(TypedVariable (Variable "$carboxyO1") (Type 'O))
(TypedVariable (Variable "$carboxyC1") (Type 'C))
(TypedVariable (Variable "$carboxyO2") (Type 'O))
(TypedVariable (Variable "$hydroxH1") (Type 'H))
(TypedVariable (Variable "$hydroxO1") (Type 'O))
(Variable "carboxy moiety")
(Variable "hydroxy moiety")
(Glob "rest of carboxy")
(Glob "rest of hydroxy")
)
(AndLink
(Molecule
(DB (Variable "$carboxyC1") (Variable "$carboxyO2"))
(SB (Variable "$carboxyC1") (Variable "$carboxyO1"))
(SB (Variable "$carboxyO1") (Variable "$carboxyH1"))
(SB (Variable "$carboxyC1") (Variable "carboxy moiety"))
(Glob "rest of carboxy")
)
(Not (Identical (Variable "$carboxyO1") (Variable "carboxy moiety")))
(Molecule
(SB (Variable "$hydroxO1") (Variable "$hydroxH1"))
(SB (Variable "$hydroxO1") (Variable "hydroxy moiety"))
(Glob "rest of hydroxy")
)
)
(AndLink
(Molecule
(DB (Variable "$carboxyC1") (Variable "$carboxyO2"))
(SB (Variable "$carboxyC1") (Variable "$carboxyO1"))
(SB (Variable "$carboxyC1") (Variable "carboxy moiety"))
(Glob "rest of carboxy")
(SB (Variable "$carboxyO1") (Variable "hydroxy moiety"))
(Glob "rest of hydroxy")
)
(Molecule
(SB (Variable "$hydroxO1") (Variable "$carboxyH1"))
(SB (Variable "$hydroxO1") (Variable "$hydroxH1"))
)
)
)
)
(define esterification
(BindLink
(VariableList
(TypedVariable (Variable "$carboxyH1") (Type 'H))
(TypedVariable (Variable "$carboxyO1") (Type 'O))
(TypedVariable (Variable "$carboxyC1") (Type 'C))
(TypedVariable (Variable "$carboxyO2") (Type 'O))
(TypedVariable (Variable "$hydroxH1") (Type 'H))
(TypedVariable (Variable "$hydroxO1") (Type 'O))
(Variable "carboxy moiety")
(Variable "hydroxy moiety")
(Glob "rest of carboxy")
(Glob "rest of hydroxy")
)
(AndLink
(Molecule
(DB (Variable "$carboxyC1") (Variable "$carboxyO2"))
(SB (Variable "$carboxyC1") (Variable "$carboxyO1"))
(SB (Variable "$carboxyO1") (Variable "$carboxyH1"))
(SB (Variable "$carboxyC1") (Variable "carboxy moiety"))
(Glob "rest of carboxy")
)
(Not (Identical (Variable "$carboxyO1") (Variable "carboxy moiety")))
(Molecule
(SB (Variable "$hydroxO1") (Variable "$hydroxH1"))
(SB (Variable "$hydroxO1") (Variable "hydroxy moiety"))
(Glob "rest of hydroxy")
)
(Not (Identical (Variable "$hydroxH1") (Variable "hydroxy moiety")))
(Not (Identical
(SB (Variable "$hydroxO1") (Variable "$hydroxH1"))
(SB (Variable "$carboxyO1") (Variable "$carboxyH1"))))
)
(AndLink
(Molecule
(DB (Variable "$carboxyC1") (Variable "$carboxyO2"))
(SB (Variable "$carboxyC1") (Variable "$carboxyO1"))
(SB (Variable "$carboxyC1") (Variable "carboxy moiety"))
(Glob "rest of carboxy")
(SB (Variable "$carboxyO1") (Variable "hydroxy moiety"))
(Glob "rest of hydroxy")
)
(Molecule
(SB (Variable "$hydroxO1") (Variable "$carboxyH1"))
(SB (Variable "$hydroxO1") (Variable "$hydroxH1"))
)
)
)
)
(Molecule
(DB (C "the carboxyl carb") (O "oxy two"))
(SB (C "the carboxyl carb") (O "oxy one"))
(SB (O "oxy one") (H "carboxyl proton"))
(SB (C "the carboxyl carb") (Fe "carbox R"))
(SB (Fe "carbox R") (Ni "more carbox junk"))
)
(Molecule
(SB (O "hydroxyl oxy") (H "hydroxyl proton"))
(SB (C "hydroxyl carbon") (O "hydroxyl oxy"))
(SB (C "hydroxyl carbon") (Zn "hydrox R"))
(SB (Zn "hydrox R") (Cu "junk hydrox moiety"))
)