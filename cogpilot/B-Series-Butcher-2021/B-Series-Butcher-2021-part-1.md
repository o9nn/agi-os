# B-Series: Algebraic Analysis of Numerical Methods - Part 1

**Author:** John C. Butcher

**Series:** Springer Series in Computational Mathematics, Volume 55

---

Springer Series in Computational Mathematics 55




John C. Butcher


B-Series
Algebraic Analysis of Numerical Methods
Springer Series in Computational Mathematics

Volume 55


Series Editors
Randolph E. Bank, Department of Mathematics, University of California,
San Diego, La Jolla, CA, USA
Wolfgang Hackbusch, Max-Planck-Institut für Mathematik in den
Naturwissenschaften, Leipzig, Germany
Josef Stoer, Institut für Mathematik, University of Würzburg, Würzburg, Germany
Richard S. Varga, Kent State University, Kent, OH, USA
Harry Yserentant, Institut für Mathematik, Technische Universität Berlin, Berlin,
Germany
This is basically a numerical analysis series in which high-level monographs are
published. We develop this series aiming at having more publications in it which
are closer to applications. There are several volumes in the series which are linked
to some mathematical software. This is a list of all titles published in this series.


More information about this series is available at http://www.springer.com/series/
797
John C. Butcher




B-Series
Algebraic Analysis of Numerical Methods




123
John C. Butcher
Department of Mathematics
University of Auckland
Auckland, New Zealand




ISSN 0179-3632                      ISSN 2198-3712 (electronic)
Springer Series in Computational Mathematics
ISBN 978-3-030-70955-6              ISBN 978-3-030-70956-3 (eBook)
https://doi.org/10.1007/978-3-030-70956-3

Mathematics Subject Classiﬁcation: 65L05, 65L06, 65L20

© Springer Nature Switzerland AG 2021
This work is subject to copyright. All rights are reserved by the Publisher, whether the whole or part
of the material is concerned, speciﬁcally the rights of translation, reprinting, reuse of illustrations,
recitation, broadcasting, reproduction on microﬁlms or in any other physical way, and transmission
or information storage and retrieval, electronic adaptation, computer software, or by similar or dissimilar
methodology now known or hereafter developed.
The use of general descriptive names, registered names, trademarks, service marks, etc. in this
publication does not imply, even in the absence of a speciﬁc statement, that such names are exempt from
the relevant protective laws and regulations and therefore free for general use.
The publisher, the authors and the editors are safe to assume that the advice and information in this
book are believed to be true and accurate at the date of publication. Neither the publisher nor the
authors or the editors give a warranty, expressed or implied, with respect to the material contained
herein or for any errors or omissions that may have been made. The publisher remains neutral with regard
to jurisdictional claims in published maps and institutional afﬁliations.

This Springer imprint is published by the registered company Springer Nature Switzerland AG
The registered company address is: Gewerbestrasse 11, 6330 Cham, Switzerland
Foreword




In Spring 1965 I took part in a scientiﬁc meeting in Vienna, where I gave a talk
on Lie derivatives and Lie series, research done together with Hans Knapp under
the supervision of Wolfgang Gröbner at the University of Innsbruck. Right after,
in the same session, was a talk by Dietmar Sommer from Aachen on a sensation:
Implicit Runge–Kutta methods of any order, together with impressive numerical
results and trajectory plots. Back in Innsbruck, the paper in question was quickly
found in volume 18 of Mathematics of Computation from 1964, but not at all quickly
understood. Deﬁnition, Deﬁnition, so-called “trees” were expressions composed of
brackets and τ’s, Lemma, Proof, Lemma, Proof, Theorem, Proof, etc.
   Apparently, another paper of John Butcher’s was required to stand a chance to
understand it, but that paper was in the Journal of the Australian Mathematical
Society, a journal almost completely unknown in Austria, at a time long before the
internet when even access to a Xerox copying machine required permission from
the Rektorat. John told me later that this paper had previously been refused for
publication in Numerische Mathematik for its lack of practical interest, no Algol
code, no impressive numerical results with thousands of digits, and no rigorous error
bounds.
   These two papers of Butcher brought elegance and order into the theory of Runge–
Kutta methods. Earlier, these methods were very popular for practical applications;
one of the ﬁrst computers, ENIAC (Electronic Numerical Integrator and Computer),
containing only a few “registers”, had mainly been built for solving differential equa-
tions using a Runge–Kutta method. The very ﬁrst program which G. Dahlquist wrote
for the ﬁrst Swedish computer was a Runge–Kutta code. But not much theoretical
progress had been achieved in these methods after Kutta’s paper from 1901. For
example, the question whether a ﬁfth order method can be found with only ﬁve
stages was open for half a century, until Butcher had proved that such methods were
impossible.
   The next sensation came when, “at the Dundee Conference in 1969, a paper by
J. Butcher was read which contained a surprising result” 1 . The idea of combining

1 H. J. Stetter 1971



                                                                                     v
vi                                                                            Foreword

different fourth order methods with ﬁve stages in such a way, that their composition
leads to a ﬁfth order numerical result, culminated in Butcher’s algebraic theory
of integration methods, ﬁrst accessible as a preprint with beautiful Māori design,
then published in an accessible journal, but which was “admirantur plures quam
intelligant” 2 .
   Fortunately, in the academic year 1968/69 I had the chance to deliver a ﬁrst year
Analysis course in Innsbruck, where a couple of very brilliant students continued to
participate in a subsequent seminar on Numerical Analysis, above all Ernst Hairer, in
whose hand this Māori-design preprint eventually arrived. Many months later, Ernst
suddenly came to me and said: “Iatz hab i’s verstandn”3 .
   But to push all these Runge–Kutta and generalized Runge–Kutta spaces into a
brain that has worked for years on Lie series and Taylor series, was another adventure.
The best procedure was ﬁnally to bring the algebraic structures directly into the series
themselves. So we arrived at the composition of B-series.
                                                                      Gerhard Wanner




2 more admired than understood, (A. Taquet 1672)
3 now I have understood it
Preface




The term “B-series”, also known as “Butcher series”, was introduced by Ernst Hairer
and Gerhard Wanner [52] (1974).
In 1970, I was invited to visit the University of Innsbruck to give a series of lectures
to a very talented audience, which included Ernst and Gerhard. At that time, my 1972
paper [14] had not been published, but a preprint was available. A few years later the
important Hairer–Wanner paper [52] appeared .
   “B-series” refers to a special type of Taylor series associated with initial-value
problems                                 
                          y (x) = f y(x) ,       y(x0 ) = y0 ,
and the need to approximate y(x0 + h), with h a speciﬁed “stepsize”, using Runge–
Kutta and other numerical methods.
   The formal Taylor series of the solution about the initial point x0 is a sum of terms
containing two factors:
 (i) a factor related to a speciﬁc initial value problem; and
(ii) a coefﬁcient factor which is the same for each initial value problem.
If, instead of the exact solution to an initial value problem, it is required to ﬁnd
the Taylor series for an approximate solution, calculated by a speciﬁc Runge–Kutta
method, the terms in (i) are unchanged, but the coefﬁcients in (ii) are replaced by a
different sequence of coefﬁcients, which are characteristic of particular Runge–Kutta
methods.
    This factorization effectively divides mathematical questions, about initial value
problems and approximate solutions, into two components: questions about f analyt-
ical in nature, and essentially algebraic questions concerning coefﬁcient sequences.
An important point to note is that, in the various Taylor series, the terms in the
sequences are best not thought of in terms of indices 0, 1, 2, 3, 4, . . . , but in terms of
graph-theoretic indices: ∅, , , , , . . . The sequence of graphs which appears here
consists of the empty tree, followed by the sequence of all rooted trees.
    The signiﬁcance of trees in mathematics was pointed out by Arthur Cayley [28]
(1857), and the name “tree”, referring to these objects, is usually atributed to him.

                                                                                         vii
viii                                                                             Preface

The use of trees in the analysis of Runge–Kutta methods seems to have been due
to S. Gill [45] (1951), and then by R. H. Merson [72] (1957). The present author
has also developed these ideas [7, 14] (1963, 1972), leading to the use of group and
other algebraic structures in the analysis of B-series coefﬁcients. The Butcher group,
referred to in this volume as the “B-group”, is central to this theory, and is related to
algebraic structures with applications in Physics and Geometry – see [4], (Brouder,
2000).
   Chapter 1 is a broad and elementary introduction to differential equation systems
and numerical methods for their solution. It also contains an introduction to some of
the topics included in later chapters. Chapter 2 is concerned with trees and related
graphical structures.
   B-series, with further properties, especially those associated with compositions of
series, are introduced in Chapter 3
   Properties of the B-group are explored in Chapter 4. This chapter is also concerned
with “integration methods” in the sense of [14]. Integration methods were intended
as a unifying theory that includes Runge–Kutta methods, with a ﬁnite number of
stages, and continuous stage Runge–Kutta methods, such as in the kernel of the
Picard–Lindelöf theorem – see for example [30] (Coddington and Levinson, 1957).
   Chapter 5 deals with Runge–Kutta methods with an emphasis on the B-series
analysis of these methods. Multivalue methods are the subject of Chapter 6. This
includes linear multistep methods and so-called “general linear methods”. In these
general linear methods, multistage and multivalue aspects of numerical methods ﬁt
together in a balanced way. In the ﬁnal Chapter 7, the B-series approach is applied to
limited aspects of the burgeoning subject of Geometric Integration.
   In addition to exercises scattered throughout the book, especially in the early
chapters, a number of substantial “projects” are added at the end of each chapter.
Unlike the exercises, the projects are open-ended and of a more challenging nature
and no answers are given to these.
   Throughout the volume, a number of algorithms have been included. As far as I
am aware, the ﬁrst B-series algorithm was composed by Jim Verner and myself on
1 January 1970. A shared interest in related algorithms has been maintained between
us to this day.
   Amongst the many people who have taken an interest in this work, I would like
to mention four people who have read all or part of the text and given me detailed
advice. I express my gratitude for this valuable help to Adrian Hill, Yuto Miyatake,
Helmut Podhaisky and Shun Sato. Special thanks to Tommaso Buvoli, Valentin
Dallerit, Anita Kean and Helmut Podhaisky, who are kindly working with me on the
support page.
Support page
A support page for this book is being developed at
                        jcbutcher.com/B-series-book
Amongst other information, the Algorithms in the book will be re-presented as
procedures or functions in one or more standard languages. The support page will
also contain some informal essays on some of the broad topics of the book.
Contents




1   Differential equations, numerical methods and algebraic analysis . . . .                                           1
    1.1 Introduction . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .     1
    1.2 Differential equations . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .           4
    1.3 Examples of differential equations . . . . . . . . . . . . . . . . . . . . . . . . . . . .                     8
    1.4 The Euler method . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .          14
    1.5 Runge–Kutta methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .             19
    1.6 Multivalue methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .          28
    1.7 B-series analysis of numerical methods . . . . . . . . . . . . . . . . . . . . . . . .                        33

2   Trees and forests . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .   39
    2.1 Introduction to trees, graphs and forests . . . . . . . . . . . . . . . . . . . . . . . .                     39
    2.2 Rooted trees and unrooted (free) trees . . . . . . . . . . . . . . . . . . . . . . . . .                      44
    2.3 Forests and trees . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .       50
    2.4 Tree and forest spaces . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .          53
    2.5 Functions of trees . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .        58
    2.6 Trees, partitions and evolutions . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .                65
    2.7 Trees and stumps . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .        76
    2.8 Subtrees, supertrees and prunings . . . . . . . . . . . . . . . . . . . . . . . . . . . . .                   80
    2.9 Antipodes of trees and forests . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .                88

3   B-series and algebraic analysis . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 99
    3.1 Introduction . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 99
    3.2 Autonomous formulation and mappings . . . . . . . . . . . . . . . . . . . . . . . . 101
    3.3 Fréchet derivatives and Taylor series . . . . . . . . . . . . . . . . . . . . . . . . . . . 106
    3.4 Elementary differentials and B-series . . . . . . . . . . . . . . . . . . . . . . . . . . 110
    3.5 B-series for ﬂow h and implicit h . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 117
    3.6 Elementary weights and the order of Runge–Kutta methods . . . . . . . 124
    3.7 Elementary differentials based on Kronecker products . . . . . . . . . . . . 127
    3.8 Attainable values of elementary weights and differentials . . . . . . . . . 129
    3.9 Composition of B-series . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 133

                                                                                                                      ix
x                                                                                                                      Contents

4      Algebraic analysis and integration methods . . . . . . . . . . . . . . . . . . . . . . . 151
       4.1 Introduction . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 151
       4.2 Integration methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 152
       4.3 Equivalence and reducibility of Runge–Kutta methods . . . . . . . . . . . 155
       4.4 Equivalence and reducibility of integration methods . . . . . . . . . . . . . . 158
       4.5 Compositions of Runge–Kutta methods . . . . . . . . . . . . . . . . . . . . . . . . 160
       4.6 Compositions of integration methods . . . . . . . . . . . . . . . . . . . . . . . . . . 163
       4.7 The B-group and subgroups . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 165
       4.8 Linear operators on B∗ and B0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 174

5      B-series and Runge–Kutta methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 177
       5.1 Introduction . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 177
       5.2 Order analysis for scalar problems . . . . . . . . . . . . . . . . . . . . . . . . . . . . 178
       5.3 Stability of Runge–Kutta methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 187
       5.4 Explicit Runge–Kutta methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 189
       5.5 Attainable order of explicit methods . . . . . . . . . . . . . . . . . . . . . . . . . . . 199
       5.6 Implicit Runge–Kutta methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 201
       5.7 Effective order methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 205

6      B-series and multivalue methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 211
       6.1 Introduction . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 211
       6.2 Survey of linear multistep methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . 213
       6.3 Motivations for general linear methods . . . . . . . . . . . . . . . . . . . . . . . . . 217
       6.4 Formulation of general linear methods . . . . . . . . . . . . . . . . . . . . . . . . . 220
       6.5 Order of general linear methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 231
       6.6 An algorithm for determining order . . . . . . . . . . . . . . . . . . . . . . . . . . . 239

7      B-series and geometric integration . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 247
       7.1 Introduction . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 247
       7.2 Hamiltonian and related problems . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 248
       7.3 Canonical and symplectic Runge–Kutta methods . . . . . . . . . . . . . . . . 252
       7.4 G-symplectic methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 262
       7.5 Derivation of a fourth order method . . . . . . . . . . . . . . . . . . . . . . . . . . . 266
       7.6 Construction of a sixth order method . . . . . . . . . . . . . . . . . . . . . . . . . . 270
       7.7 Implementation . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 276
       7.8 Numerical simulations . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 277
       7.9 Energy preserving methods . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 281

Answers to the exercises . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 291

References . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 303

Index . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 307
