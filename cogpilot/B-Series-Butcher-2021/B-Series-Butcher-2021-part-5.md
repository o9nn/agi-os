# B-Series: Algebraic Analysis of Numerical Methods - Part 5

**Author:** John C. Butcher

**Series:** Springer Series in Computational Mathematics, Volume 55

---

Chapter 4
Algebraic analysis and integration methods




4.1 Introduction

Integration methods [14] are a generalization of Runge–Kutta methods in which the
index set for the stages is no longer restricted to a ﬁnite set {1, 2, . . . , s}. If the index
set is a closed interval, or more generally, the union of a ﬁnite number of closed
intervals, they are referred to as “continuous Runge–Kutta methods”..
   A Runge–Kutta method can be written

                     Yi = y0 + h ∑ ai j f (Y j ),    i ∈ I = {1, 2, . . . , s},
                                  j∈I

                     y1 = y0 + h ∑ bi f (Yi ),
                                  i∈I

and, in contrast, a continuous Runge–Kutta method takes the form
                                                       
               Y (ξ ) = y0 + h          a(ξ , η) f Y (η) d η,      ξ ∈ I = [0, 1],
                                 ξ ∈I
                                                     
                  y 1 = y0 + h          b(ξ ) f Y (ξ ) d ξ .
                                 ξ ∈I

   The composition group of general linear methods is a subgroup of the group
of B-series, as is clear from Theorem 4.6B (p. 165). This provides an alternative
approach to Theorem 3.9C (p. 139). The subgroups of B are important because of
their applications to simplifying assumptions of Runge–Kutta.


Chapter outline

In Section 4.2 the deﬁnition and principal properties of integration methods and
continuous Runge–Kutta methods will be introduced. This is followed in Section
4.3 by a discussion of the reducibility of Runge–Kutta mehods and, in Section 4.4, a

© Springer Nature Switzerland AG 2021                                                      151
J. C. Butcher, B-Series, Springer Series in Computational Mathematics 55,
https://doi.org/10.1007/978-3-030-70956-3_4
152                                             4 Algebraic analysis and integration methods

similar analysis is given for general linear methods . In Section 4.5, compositions of
Runge–Kutta methods are studied, followed by a consideration of the general linear
case in Chapter 4.6. This section includes work on the composition of elementary
weights.
   The order-deﬁning subgroup 1 + O p+1 is introduced in Section 4.7, where O p+1
can be thought of as an algebraic counterpart to O(h p+1 ). Further subgroups are
also introduced. These include subgroups corresponding to simplifying assumptions,
such as the assumptions introduced in [9]. The linear spaces B∗ and B0 , and linear
operators on these spaces, are introduced in section 4.8.



4.2 Integration methods

“Integration methods” refers to a generalization of Runge–Kutta methods introduced
in [14] (Butcher, 1972). In place of the usual ﬁnite index set {1, 2, . . . , s}, the stages
can be indexed by an interval [0, 1] or some other set. We will assume this set is
Hausdorff and compact [81] (Rudin, 1976).
    For X a compact Hausdorff set, let B(X) denote the Banach algebra of continuous
real-valued functions on X. That is, B(X) is a vector space and at the same time has
a product deﬁned pointwise over X.

 Deﬁnition 4.2A An integration method is a triple (X, A, b), where A is a linear
 operator on B(X) and b is a linear functional on B(X).

In these deﬁnitions, the norm A is deﬁned as supξ =0 Aξ /ξ . The norm b
is deﬁned as supξ =0 |bξ |/ξ .
                                                                
    Given an N-dimensional initial value problem, y (x) = f y(x) , y(x0 ) = y0 , the
“solution” using (X, A, b) is found from

                                  Y = 1y0 + hA( f ◦Y ),                              (4.2 a)
                                  y1 = y0 + hb( f ◦Y ).                             (4.2 b)



 Theorem 4.2B The solution to (4.2 a), (4.2 b) exists and is unique, for h <
 (LA)−1 , where L is the Lipschitz constant for f .

Proof. The mapping Y → 1y0 + hA( f ◦Y ) is a contraction.


Continuous Runge–Kutta methods
Let X be a closed interval I, for example, I = [0, 1]. For these methods, it is convenient
to interpret A, b as integrals
4.2 Integration methods                                                                         153
                                        1
                                            A(ξ , η)Y (η) d η,
                                    0
                                        1
                                            b(η)Y (η) d η.
                                    0




Example methods

Picard–Lindelöf theorem
The initial value problem,                            
                                        y (x) = f y(x) ,
                                     y(x0 ) = y0 ,

and its solution y1 at x0 + h, can be recast as an integral equation and gives the
Picard–Lindelöf theorem [30] (Coddington, Levinson,1955), in the form
                                                ξ               
               y(x0 + hξ ) = y0 + h                 f y(x0 + hξ ) d ξ ,        ξ ∈ [0, 1],   (4.2 c)
                                            0
                                                1               
                          y 1 = y0 + h              f y(x0 + hξ ) d ξ .                      (4.2 d)
                                            0

This example of an integration method on [0, 1] will be referred to as “the Picard
method”. It can be written in the form
                                                            ξ
                          Y (ξ ) = y(x0 ) + h                   ( f ◦Y )(η) d η,
                                                        0
                                                            1
                              y1 = y(x0 ) + h                   ( f ◦Y )(η) d η.
                                                        0

   Just as for Runge–Kutta methods, speciﬁc integration methods can be character-
ized by their elementary weights. In particular, the method given by (4.2 c,4.2 d) has
elementary weights Φ(t) = 1/t!, as in the right-hand sides of Runge–Kutta order
conditions. This is
                   to be
                         expected because we can identify the value of y1 with the
ﬂow of y (x) = f y(x) through a stepsize h. That is, (4.2 d) can be regarded as a
limiting case of a Runge–Kutta method as the order tends to inﬁnity.


Average Vector Field method
The Average Vector Field method (AVF method) [80] (Quispel, McLaren, 2008), and
other related energy-preserving methods, ﬁts naturally into the family of integration
methods. The AVF method can be written in the form
                                                                ξ              
                   y(x0 + hξ ) = y(x0 ) + hξ                        f y(x0 + hη) d η,
                                                             0
                                                            1              
                              y1 = y(x0 ) + h                   f y(x0 + hη) d η
                                                        0
154                                                         4 Algebraic analysis and integration methods

or in the form
                                                        ξ
                        Y (ξ ) = y(x0 ) + h                 ξ ( f ◦Y )(η) d η,                       (4.2 e)
                                                    0
                                                        1
                            y1 = y(x0 ) + h                 ( f ◦Y )(η) d η.                          (4.2 f)
                                                    0



Similarity with Runge–Kutta methods
In the two equations in the Picard–Lindelöf formulation, (4.2 c) corresponds to the
computation of the stages in a Runge–Kutta method, and (4.2 d) corresponds to
the computation of the output to the step. That is, the integrals in these two lines
correspond to

       c   A                                                        c    A
                    and                                 in                     ,          respectively.
                                      b   T
                                                                         bT

Similar remarks apply to the AVF method, with (4.2 e) corresponding to the stage
calculation and (4.2 f) corresponding to the evaluation of the output.
   In discussing integration methods, we will often ﬁnd it convenient to distinguish
the stage computation only, from the full method. We will use the expression “inte-
gration system” for the stages-only aspect of an integration method.


Elementary weights of integration methods

Recall the formulae for elementary weights given by Deﬁnition 3.6A (p. 125):
                                               s
                              ϕi (τ) = ∑ ai j ,                      i = 1, 2, . . . s,
                                              j=1
                                               s
                              Φ(τ) = ∑ bi j ,
                                              j=1
                                               s        n
                  ϕi ([t 1 t2 · · · t n ]) = ∑ ai j ∏ ϕ j (t k ),
                                              j=1    k=1
                                               s     n
                  Φ([t 1 t2 · · · t n ]) = ∑ bi ∏ ϕi (t k ).
                                              i=1   k=1

  The generalization to integration methods on [0, 1] is given by re-interpreting this
deﬁnition:
                                       ϕ(τ) = A1,
                                      Φ(τ) = bT 1,
                         ϕ([t 1 t2 · · · t m ]) = A dotm
                                                       i=1 ϕ(t i ),
                            Φ([t 1 t2 · · · t m ]) = bT dotm
                                                           i=1 ϕ(t i ).
4.3 Equivalence and reducibility of Runge–Kutta methods                                155

Exercise 39 Find Φ(t) for |t| ≤ 4 for the Picard method.


Exercise 40 Find Φ(t) for |t| ≤ 4 for the average vector ﬁeld method.




4.3 Equivalence and reducibility of Runge–Kutta methods

Flavours of equivalence

It will be convenient to consider only the stages of a generic Runge–Kutta method

                                                 c    A ,

rather than a full tableau
                                                 c    A
                                                             .
                                                      bT


Equivalence
For a given initial value problem y = f (y), y(x0 ) = y0 , satisfying a Lipschitz condi-
tion, let Yi , i = 1, 2, . . . , s, be deﬁned by
                                             s
                             Yi = y0 + h ∑ ai j f (Y j ),            h ≤ h0 .
                                             j=1



 Deﬁnition 4.3A Two stages are equivalent, written i ≡ j, if for any such problem,
 Yi = Y j .



ϕ-equivalence
Recall the elementary weight vector of A given by Deﬁnition 3.6A (p. 125). In the
present context, this is given by
                                                                             T
                          ϕ=        ϕ1 (t)       ϕ2 (t)      ···     ϕs (t)        ,

where ϕi (t) is the appropriate elementary weight for the tableau

                                              c       A
                                                                 .
                                                     eTi A
156                                                    4 Algebraic analysis and integration methods



                                                                               ϕ
 Deﬁnition 4.3B Two stages are ϕ-equivalent, written i ≡ j, if for all t, ϕi (t) =
 ϕ j (t).




P-equivalence
Let P be a partition of the stages of A; that is
                                                                  
                                P=         P1   P2     ···      PS ,

where the components, P1 , P2 , . . . PS , are disjoint, with union {1, 2, . . . , s}.

 Deﬁnition 4.3C A is P-reducible if for all I, J = 1, 2, . . . , S,

                                ∑ aik = ∑ a jk ,             i, j ∈ PI .
                                k∈PJ        k∈PJ




                                                                           P
 Deﬁnition 4.3D Two stages are P-equivalent, written i ≡ j if there exists P such
 that A is P-reducible and i, j are in the same component.

The concept of P-equivalence is related to Hundsdorfer–Spijker reducibility [58]
(Hundsdorfer, Spijker, 1981).


Main result on stage equivalence


 Theorem 4.3E Let i, j be two stages. Then
                                                 ϕ                                  P
                  i≡ j    if and only if        i≡ j     if and only if            i ≡ j.

Proof.
                  ϕ
 if i ≡ j then i ≡ j
For given t, use Theorem 3.8D (p. 132), with n = |t|, and θk = 0 except for tk = t,
in which case θk = 1. From the B-series for xTYi and xTY j , we have ϕi (t)hn /σ (t) +
O(hn+1 ) = ϕ j (t)hn /σ (t) + O(hn+1 ), for h < h0 ; and it follows that ϕi (t) = ϕ j (t).
      ϕ           P
 if i ≡ j then i ≡ j
                                                                                        ϕ
Deﬁne P such that i, j are in the same component if and only if i ≡ j. We will show
4.3 Equivalence and reducibility of Runge–Kutta methods                                       157

that A is P-reducible. Let V ∈ Rs denote the vector space spanned by ϕ(t) for all
t. It can be seen that (a) 1 ∈ V , (b) if u, v ∈ V then dot2 uv ∈ V , (c) if u ∈ V then
Au ∈ V . Deﬁne V ∈ RS such that, for v ∈ V , v ∈ V is deﬁned by vi = vI , for i ∈ PI .
Because V satisﬁes the conditions of Theorem 3.8A (p. 129), it contains members
arbitrarily close to eI , for I = 1, 2, . . . , S so that V contains members arbitrarily close
to x deﬁned by xk = 1 for k ∈ Pi and xk = 0 otherwise. Hence (Ax) has constant
value for all  in any of the components of P.
     P
if i ≡ j then i ≡ j
                                                   [k]                             [0]
For  = 1, 2, . . . , s, deﬁne the sequence Y , k = 0, 1, 2, . . . , by Y = y0 and s

                          [k]
                                    s       [k−1] 
                        Y = y0 + h ∑ am f Ym       ,                k ≥ 1.
                                        m=1

                 [k]     [k]
By induction, Yi = Y j for all k. Hence, in the limit as k → ∞, Yi = Y j .



Reduced tableau


 Deﬁnition 4.3F If the stages of a method are partitioned as in Deﬁnition 4.3C,
 the pre-reduced method is the S-stage-tableau

                                       c1     a11        a12   ...    a1S
                                      c2      a21        a22   ...    a2S
                        c      A       .       ..         ..           ..
                                    = ..        .          .            .      ,         (4.3 a)
                               bT
                                      cS      aS1        aS2   ...    aSS
                                              b1         b2    ...    bS

 where
                                     cI = ci ,             i ∈ PI ,
                                    aIJ = ∑ ai j ,         i ∈ PI ,
                                            j∈PJ

                                     bI = ∑ bi .
                                            i∈PI


Essential stages of a pre-reduced method are deﬁned recursively:

 Deﬁnition 4.3G A stage J of (4.3 a) is essential if bJ = 0, or there exists an
 essential stage I such that aIJ = 0.
158                                                      4 Algebraic analysis and integration methods

For the method (4.3 a), a non-essential stage can be deleted. For example, the deletion
of stage K gives

               c1       a11     ...        a1,K−1              a1,K+1        ...    a1S
                ..       ..                     ..                  ..               ..
                 .        .                      .                   .                .
             cK−1     aK−1,1    ...       aK−1,K−1            aK−1,K+1       ...   aK−1,S
             cK+1     aK+1,1    ...       aK+1,K−1            aK+1,K+1       ...   aK+1,S ,
                ..       ..                     ..                  ..               ..
                 .        .                      .                   .                .
               cS       aS1     ...        aS,K−1              aS,K+1        ...    aSS
                        b1      ...        bK−1                bK+1          ...    bS


 Deﬁnition 4.3H A reduced method is a pre-reduced method from which all non-
 essential stages have been deleted.

Deﬁnition 4.3H is related to Dahlquist–Jeltsch reduction [39] (Dahlquist, Jeltsch,
2006).

Exercise 41 Find the reduced form of the tableau
                                      1
                                      2
                                            1
                                            6        1 −1       1
                                                                3
                                      2
                                      3
                                            1
                                            3        1 − 13     0

                                                          3 −3           .
                                      2              1    1  2
                                      3     1        3
                                      1
                                      2     1 − 12        2 −2
                                                          1  1


                                                     4 −4
                                            1        1  1       1
                                            2                   2




4.4 Equivalence and reducibility of integration methods

The aim of this section is to show how concepts associated with Runge–Kutta
methods extend in a natural way to integration methods.


Reducible integration systems


 Deﬁnition 4.4A The Banach algebra B(X, A) is the closed sub-algebra of B(X)
 containing the unit function 1, Av and uv whenever u and v are in the algebra.
4.4 Equivalence and reducibility of integration methods                              159


                                                                     A
 Deﬁnition 4.4B ξ1 , ξ2 ∈ X are A-equivalent, written ξ1 ≡ ξ2 , if for all u ∈ B(X, A),
 u(ξ1 ) = u(ξ2 ).


                                                                         ϕ
 Deﬁnition 4.4C ξ1 , ξ2 ∈ X are ϕ-equivalent, written ξ1 ≡ ξ2 , if for all t ∈ T,
 ϕξ1 (t) = ϕξ2 (t).



 Theorem 4.4D ξ1 , ξ2 ∈ X are ϕ-equivalent if and only if they are A-equivalent.

Proof. We will use the “height” of a tree introduced in Section 2.3 (p. 50). Deﬁne
B0 as the linear space spanned by A1 and for n = 1, 2, . . . , deﬁne Bn as the Banach
algebra containing uv whenever u, v ∈ Bn−1 and u and Au whenever u ∈ Bn−1 . By
induction, Bn contains ϕ(t) for height(t) ≤ n. Since every member of B(X, A) lies in
              A
some Bn , ξ1 ≡ ξ2 implies that ϕξ1 (t) = ϕξ2 (t) for each t.
On the basis of this result, we will use “equivalence” as a short-hand for either
“ϕ-equivalence” or “A-equivalence”, and use the notation ξ1 ≡ ξ2 .


Pre-reduced and reduced methods

If (X, A, b) is an integration method then X := X/ ≡ will denote the set of equivalence
classes into which X is partitioned. For xi ∈ X, 1ξ (ξ ) = 1, if ξ ∈ ξ , and 0 otherwise.
   We also deﬁne bξ = b1ξ and

                               Aξ ,ξ  = A(ξ ,·) 1ξ  ,     ξ ∈ ξ.



 Deﬁnition 4.4E Given an integration method (X, A, b), (X, A, b), is the corre-
 sponding pre-reduced method.



Equivalence of methods


 Deﬁnition 4.4F Two integration methods are “equivalent” if their elementary
 weight functions are related by

                                   Φ(t) = Φ(t),           t ∈ T.
160                                                          4 Algebraic analysis and integration methods

4.5 Compositions of Runge–Kutta methods

Given two Runge–Kutta methods

          c1       a11        a12      ···     a1s               c1          a11      a12      ···    a1s
          c2       a21        a22      ···     a2s               c2          a21      a22      ···    a2s
      M = ...       ..
                     .
                               ..
                                .
                                                .. ,
                                                 .           M = ...          ..
                                                                               .
                                                                                       ..
                                                                                        .
                                                                                                       .. ,
                                                                                                        .
          cs       as1        as2      ···     ass               cs          as1      as2      ···    ass
                   b1             b2   ···      bs                           b1        b2      ···     bs

a new tableau can be constructed with s + s stages:

                   c1                  a11    a12      ···   a1s     0        0       ···      0
                   c2                  a21    a22      ···   a2s     0        0       ···      0
                    ..                  ..     ..             ..     ..       ..               ..
                     .                   .      .              .      .        .                .
                   cs                  as1    as2      ···   ass     0        0       ···      0
            MM := cs+1                 b1     b2       ···   bs     a11      a12      ···     a1s ,
                  cs+2                 b1      b2      ···    bs    a21      a22      ···     a2s
                             ..         ..      ..             ..    ..       ..               ..
                              .          .       .              .     .        .                .
                         cs+s          b1      b2      ···    bs    as1      as2      ···     ass
                                       b1      b2      ···    bs     b1       b2      ···      bs

where
                                                 s
                                  cs+i = ci + ∑ b j ,          i = 1, 2, . . . , s.
                                                j=1

To see the signiﬁcance of this construction, consider the application of the two
Runge–Kutta methods in sequence so that y1 is the result of applying M to y0 and
y2 is the result of applying M to y1 . Let Y1 , Y2 , . . . , Ys denote the stages computed
in the ﬁrst of these steps and Y 1 , Y 2 , . . . , Y s the stages computed in the second step.
Throughout this discussion, Fi and F i will denote f (Yi ) and f (Y i ), respectively.
   We have
                                                       s
         Yi =                                y0 + h ∑ ai j Fj ,                       i = 1, 2, . . . , s,
                                                      j=1
                         s
         y1 = y0 + h ∑ bi Fi ,
                        i=1
                          s                            s             s
         Y i = y1 + h ∑ ai j F j = y0 + h ∑ bi Fi + h ∑ ai j F j .                    i = 1, 2, . . . , s,
                        j=1                           i=1           j=1
4.5 Compositions of Runge–Kutta methods                                                               161

                     s                               s                  s
        y2 = y1 + h ∑ bi F i = y0 + h ∑ bi Fi + h ∑ bi F i ,
                    i=1                         i=1                i=1

so that the tableau MM deﬁnes the composed Runge–Kutta method for computing y2
directly from y0 .


The inverting method
Given the tableau
                                c1             a11       a12     ···        a1s
                                c2             a21       a22     ···        a2s
                            M = ...             ..
                                                 .
                                                          ..
                                                           .
                                                                             ..
                                                                              .
                                cs             as1       as2     ···        ass
                                               b1        b2      ···        bs
and the stage and output formulae
                                           s
                          Yi = y0 + h ∑ ai j Fj ,              i = 1, 2, . . . , s,
                                          j=1
                                           s
                          y1 = y0 + h ∑ bi Fi ,
                                          i=1

we can solve for y0 in terms of y1 to obtain
                                    s
                    Yi = y1 + h ∑ (ai j − b j )Fj ,                 i = 1, 2, . . . , s,
                                    j=1
                                     s
                    y0 = y1 + h ∑ (−bi )Fi .
                                   i=1

This deﬁnes the inverting method

                    c1 − ∑si=1 bi         a11 − b1            a12 − b2           ···   a1s − bs
                    c2 − ∑si=1 bi         a21 − b1            a22 − b2           ···   a2s − bs
              −             ..                  ..                 ..                      ..
            M =              .                   .                  .                       .     ,

                    cs − ∑si=1 bi         as1 − b1            as2 − b2           ···   ass − bs
                                           −b1                 −b2               ···   −bs

which exactly undoes the work of M.
   The two composed methods MM − and M − M should, in some sense, be equiv-
alent to the identity method with zero stages and a zero-dimensional bT vector. To
investigate this question we ﬁrst reduce it to a single case by:
162                                                          4 Algebraic analysis and integration methods



 Theorem 4.5A Let M be a tableau and M − denote its inverting method. Then

                                                 (M − )− = M.

The following theorem also applies to M − M, because of Theorem 4.5A:

 Theorem 4.5B Given a tableau M, the Runge–Kutta method MM − applied to an
 initial value problem ( f , y0 ) with Lipschitz condition L, with hL sufﬁciently small,
 gives a result equal to y0 .

Proof. We will assume h is sufﬁciently small for a certain iteration scheme to be a
                                                        [k]
contraction. To describe this iteration scheme, write Yi for iteration number k, and
                                                           [k]
stage number i, in the step using M and Y i for the corresponding iteration in the
M − step. For iteration zero, deﬁne
                                 [0]       [0]
                               Yi      = Y i = y0 ,              i = 1, 2, . . . , s

and deﬁne subsequent iterations by

              [k]
                        s        [k−1] 
            Yi = y0 + h ∑ ai j f Y j      ,                      i = 1, 2, . . . , s, k = 1, 2, . . . ,
                               j=1
              [k]                [k−1] 
                                s             s                [k−1] 
            Y i = y0 + h ∑ b j f Y j      + h ∑ (ai j −b j ) f Y j      .
                               j=1                         j=1

It can be veriﬁed, using induction on k, that
                         [k]         [k]
                       Yi = Y i ,                i = 1, 2, . . . , s,     k = 1, 2, . . .

Hence, for the limiting values, Yi = Y i , i = 1, 2, . . . , s.
To look at this question in terms of reduced tableaux, we have the result:

 Theorem 4.5C Given a tableau M, the reduced tableau of MM − is the identity
 method.

Proof. The composite tableau is

                                            c      A             0
                                            c     θ1 A−θ1 ,
                                                   bT      −bT

where θ = bT 1. According to Deﬁnition 4.3C (p. 156), the tableau is P-reducible,
with P = [ {1, s + 1} {2, s + 2} · · · {s, 2s} ], and the pre-reduced tableau is
4.6 Compositions of integration methods                                                163

                                           c       A
                                                        ,
                                                  0T

with no essential stages and the reduced tableau of the identity Runge–Kutta method.




4.6 Compositions of integration methods

If we have two integration methods M1 = (X1 , A1 , bT1 ) and M2 = (X2 , A2 , bT2 ), which
are to be used sequentially,

                                 Y1 = 1y0 + hA1 ( f ◦Y1 ),
                                 y1 = y0 + hbT1 ( f ◦ 1),
                                 Y2 = 1y1 + hA2 ( f ◦Y2 ),
                                 y2 = y1 + hbT2 ( f ◦Y2 ).

Conventionally, assume X1 ∩ X2 3= 0/ and write
                                            3 X := X1 ∪ X2 , (X, A, b ) =: M = M1 M2T.
                                                                     T

                               3            3
For ϕ : X → R , write ϕ1 := ϕ X ϕ2 := ϕ X . The operator A and the functional b
              N
                                 1            2
are deﬁned by
                           '
                              (A1 ϕ1 )(ξ ),                 ξ ∈ X1 ,
                (Aϕ)(ξ ) =
                              1(b1 ϕ1 )(ξ ) + A2 (ϕ2 )(ξ ), ξ ∈ X2 ,
                                   T



                     bT (ϕ) = (bT1 ϕ1 ) + (bT2 ϕ2 ).



Elementary weights of composed methods



 Theorem 4.6A Let Ψ (t), Ω (t), and Φ(t) denote the elementary weights for M1 ,
 M2 , and M = M1 M2 , respectively. Then

                           Φ(t) = Ψ (t) + ∑ Ψ (t t )Ω (t ).
                                               t  ≤t


Proof. Let t = (V, E, r) and let V  be a connected subset of V such that V  = 0/ or
r ∈ V  . The possible choices of V  will be denoted by Vk , k = 0, 1, 2, . . . , n, with
      / For each such Vk , tk will denote the corresponding subtree of t (with special
V0 = 0.
case t0 = ∅). Note that V Vk deﬁnes t t k . For conciseness, child(i) will be written
C(i). For each i ∈ V , deﬁne ψi ∈ B(X1 ), ωi ∈ B(X2 ), by the recursion,
164                                                     4 Algebraic analysis and integration methods

                             ψi = A1 dot j∈C(i) ψ j ,
                             ωi = 1A1 dot j∈C(i) ψ j + A2 dot j∈C(i) ω j ,                  (4.6 a)
                         Φ(t) = b1 dotr∈C(i) ψ j + b2 dotr∈C(i) ω j .

For V  = Vk , let Φk indicate that the recursions for (4.6 a) are replaced by

                        ψi = A1 dot j∈C(i) ψ j ,                              i = r,
                              1b dot                        i ∈ Vk ,
                                  1     j∈C(i) ψ j ,                          i = r,        (4.6 b)
                        ωi =
                               A2 dot j∈C(i) ω j ,           i ∈ Vk ,        i = r,        (4.6 c)
                              b dot                         r ∈ Vk ,
                                1     j∈C(r) ψ j ,                                          (4.6 d)
                     Φk (t) =
                               b2 dot j∈C(r) ω j ,           r ∈ Vk ,                      (4.6 e)

so that Φ = ∑nk=0 Φk . The evaluation of Φ0 involves the iterations (4.6 b), (4.6 d),
so that Φ0 = Ψ (t). In (4.6 c), (4.6 e), write C(i) = D(i) ∪ E(i), where D(i) ⊂ V Vk ,
E(i) ⊂ Vk . For k > 0, rewrite (4.6 c), (4.6 e) in the form
                                                      
                  ωi = ∏ b1 ψ j A2 dot j∈E(i) ω j , i ∈ Vk , i = r,
                             j∈D(i)
                                                          
              Φk (t) =        ∏ b1 ψ j      b2 dot j∈E(r) ω j , r ∈ Vk ,
                             j∈D(r)

so that                                    s          
                               Φk (t) =     ∏ ∏ b1 ψ j
                                                         k (t)
                                                         Φ
                                            j=1 j∈D(i)
                                                  k (t),
                                      = Ψ (t t  )Φ

      k (t) is deﬁned by the recursion
where Φ

                              ωi = A2 dot j∈E(i) ω j ,      i ∈ Vk ,    i = r,
                         k (t) = b2 dot j∈E(r) ω j ,
                         Φ                                 r ∈ Vk ,

                    k (t) = Ω (t  ) and
and it follows that Φ
                                 n                                       n
                                       k (t) = Ψ (t) + ∑ Ψ (t t )Ω( t ).
           Φ(t) = Φ0 (t) + ∑ Ψ (t t  )Φ
                                k=1                                     k=1




Comments on Theorem 4.6A

For a speciﬁc example of t, the choices of V  are illustrated in the upper line of (4.6 f).
The 10 possible non-empty choices of V  are given, with vertices shown as . The
4.7 The B-group and subgroups                                                                165

remaining vertices of t are shown as . In addition to 1, 2, . . . , 10, the special case
headed 0 corresponds to t = ∅. On the lower line of (4.6 f), t  is shown together with
t t .

   0       1      2      3      4        5         6       7      8        9       10


                                                                                          (4.6 f)




Recall Theorem 3.9C (p. 139), which we restate

 Theorem 4.6B (Reprise of Theorem 3.9C)                 Let a ∈ B, b ∈ B∗ . Then


                  (ab)(∅) = b(∅),
                   (ab)(t) = b(∅)a(t) + ∑ b(t )a(t t ),             t ∈ T.            (4.6 g)
                                               t  ≤t


Proof. Let p be a positive integer. By Theorem 3.8B (p. 130), there exist Runge–Kutta
method tableaux M1 , M2 , with B-series coefﬁcients, Ψ , Ω , respectively, such that
Ψ = a + O p+1 , Ω = b + O p+1 . For M = M1 M2 , let Φ be the corresponding B-Series
so that Φ = Ψ Ω . Use Theorem 4.6A so that (4.6 g) holds to order p. Since p is
arbitrary, the result follows.


4.7 The B-group and subgroups

Reinterpreting Theorem 4.6B in the case that b ∈ B, we obtain a binary operation on
this set given by

                      (ab)(t) = a(t) + ∑ b(t )a(t t ),          t ∈ T,                 (4.7 a)
                                          t  ≤t

or, written another way, as

                       (ab)(t) =      ∑ b(t )a(t t ),         t ∈ T,                   (4.7 b)
                                    ∅≤t  ≤t




 Theorem 4.7A The set B equipped with the binary operation (a, b) → ab, given
 by (4.7 b), is a group.
166                                             4 Algebraic analysis and integration methods

Proof. We verify the three group axioms.
(i) B is associative because (a(bc))(t) and ((ab)c)(t) are each equal to

                               ∑    a(t t )b(t t )c(t  )
                                
                            ∅≤t ≤t ≤t

(ii) The identity element exists, given by

                           1(∅) = 1,         1(t) = 0,   t∈T

(iii) The inverse exists. For a ∈ B, a−1 = x is deﬁned recursively by

                        x(t) = −    ∑ x(t )a(t t ),     t ∈ T,
                                   ∅≤t <t

or
                        x(t) = −    ∑ a(t )x(t t ),     t ∈ T,
                                   ∅<t ≤t


The “B-group” of Theorem 4.7A was introduced in [14] (Butcher, 1972) and named
the “Butcher group” in [52] (Hairer, Wanner,1974).


Subgroups of B

If H is a subset of B then it is a “subgroup”, written H ≤ B, if xy ∈ H whenever
x, y ∈ H.


The order-deﬁning subgroup


 Deﬁnition 4.7B The subset O p+1 of B0 is deﬁned by a ∈ O p+1 , if a(t) = 0 for
 |t| ≤ p.


 Theorem 4.7C O p+1 is a linear subspace of B∗ .

Proof. If a, b ∈ O p+1 then a + b ∈ O p+1 , because (ab )(t) = 0 if |t| > p.
The subgroup of B, (1 + O p+1 ), is of particular interest.

 Theorem 4.7D (1 + O p+1 ) is a normal subgroup of B.

Proof. For convenience, write H = (1 + O p+1 ). This is a subgroup because if h1 , h2 ∈
H, then (h1 h2 )(t) = h1 (t) + h2 (t) + Q, where the quantity Q involves trees of order
less than |t|. If x ∈ B, h ∈ H, so that xh ∈ xH, we construct h recursively through
4.7 The B-group and subgroups                                                        167

trees of increasing orders so that hx ∈ xh. Up to order p it is seen that h and h agree
and therefore h ∈ H.

 Deﬁnition 4.7E The quotient group B/(1 + O p+1 ) is deﬁned to be B p .


 Theorem 4.7F If a ∈ bB p then

                              B(a)h y0 = B(b)h y0 + O(h p+1 ).

Proof. If |t| ≤ p, then a(t) = b(t).
Hence, we can identify B/(1 + O p+1 ) with the group formed by restricting the
mapping T → R which deﬁnes elements of B to trees with order not exceeding p.


Informal explanation of O p+1 and 1 + O p+1
Use the example case p = 4. Members of O5 and 1 + O5 are of the form below where
the trees which index the various components are also shown


                      ∅                                                 ···
                                                                             T
           O5 :       0   0   0    0   0   0    0    0   0   a9   a10   ···
                                                                             T
       1 + O5 :       1   0   0    0   0   0    0    0   0   a9   a10   ···

Linear combinations of members of O5 are also members of this subspace of B∗ ;
furthermore the product of two members of the group 1 + O5 is also in this group
because (ab)(t), for |t| ≤ 4, is always zero.
    If x and y are the coefﬁcients in the B-series expansions corresponding to x h and
y h respectively, then x = y + O5 means that the terms up to order 4 are identical in x
and y. This corresponds to x h = y h + O(h5 ).
    If x, y ∈ B, so that x h and y h are central mappings, then x = y + O5 can be written
in the form x = y(1 + O5 ).


Numerical signiﬁcance of special subgroups

Although algebraic analysis is widely used in the study of many types of numerical
methods, we will conﬁne these remarks to Runge–Kutta methods. If a subgroup H
has the property
                                   E ∈ H ≤ B,
then it might be simpler to search for approximations in H than in B directly. A
particular case is the group D1 which contains all solutions to the conditions for order
4 within explicit methods with 4 stages.
168                                                     4 Algebraic analysis and integration methods

Introduction to B p

In this discussion, we will use the standard numbering of trees introduced in Table 7
(p. 66). If a and b are two speciﬁc points with

                      ai := a(ti ),     bi := b(ti ),        i = 1, 2, . . . , 17,

then, as far as order 3 trees, we deﬁne the product ab by giving the values (ab)i :=
(ab)(ti ), as follows
                           (ab)1 = a1 + b1 ,
                             (ab)2 = a2 + a1 b1 + b2 ,
                                                                                            (4.7 c)
                             (ab)3 = a3 + a21 b1 + 2a1 b2 + b3 ,
                             (ab)4 = a4 + a2 b1 + a1 b2 + b4 .

For |t| ≥ 4, the formula for the product evaluated for this tree has the form

         (ab)(t) = a(t) + b(t)
                   + an expression involving a and b for lower order trees.
Hence we can illustrate some of the group-theoretic properties of B, introduced in
Section 3.4, using the restriction to just the ﬁrst four trees. We will temporarily write
B for the set R4 , with a binary operation deﬁned by (4.7 c). We will also write a−1
for the right inverse satisfying a · a−1 = 1, where 1 denotes the left or right identity
element 1(ti ) = 0, i = 1, 2, 3, 4. It is found that

                                (a−1 )1 = −a1 ,
                                (a−1 )2 = a21 − a2 ,
                                (a−1 )3 = −a31 + 2a1 a2 − a3 ,
                                (a−1 )4 = −a31 + 2a1 a2 − a4 .


 Theorem 4.7G B p is a group.

Proof. It is only necessary to prove that B p is associative. Introduce a third member
c ∈ B. We have
                    
                (ab)c 1 = a1 + b1 + c1
                                
                        = a(bc) 1 ,
                    
                (ab)c 2 = (a2 + a1 b1 + b2 ) + (a1 + b1 )c1 + c2
                         = a2 + a1 (b1 + c1 ) + (b2 + b1 c1 + c2 )
                                 
                         = a(bc) 2 ,
                     
                 (ab)c 3 = (a3 + a21 b1 + 2a1 b2 + b3 )
                                      + (a1 + b1 )2 c1 + 2(a1 + b1 )c2 + c3
4.7 The B-group and subgroups                                                                         169

                             = a3 + a21 (b1 + c1 ) + 2a1 (b2 + b1 c1 + c2 )
                                   + (b3 + b21 c1 + 2b1 c2 + c3 )
                                   
                             = a(bc) 3 ,
                      
                  (ab)c 4 = (a3 + a2 b1 + a1 b2 + b3 )
                                     + (a2 + a1 b1 + b2 )c1 + (a1 + b1 )c2 + c3
                             = a3 + a2 (b1 + c1 ) + a1 (b2 + b1 c1 + c2 )
                                   + (b3 + b2 c1 + b1 c2 + c3 )
                                   
                             = a(bc) 4 .



Subgroups of B p for low p

The group B p has some interesting sub-groups.
   The subgroup H1 with elements deﬁned by a2 = 12 a21 , a3 = 13 a31 , a4 = 16 a31 .
   The subgroup H2 with elements satisfying a1 = 0.
   The subgroup H3 with elements deﬁned by a2 = 12 a21 , a4 = 12 a3 .
   The subgroup H4 with elements deﬁned by a2 = 12 a21 .
These are all low order examples of subgroups of B but illustrate some important
features. The subgroup H1 represents, to order 3, the group generated by ﬂows
through a time a1 h. The subgroup H2 represents approximations to the identity map
to order 1. The subgroup H3 illustrates the use of the simplifying assumption C(2)
and H4 represents methods with time-reversal symmetry.
Examples and exercises
                                                                            
In these examples we will write an element of B as a = a1 a2 a3 a4 . The set
H1 is a subgroup of B because
                                                     
     a1 12 a21 13 a31 61 a41    b1 21 b21 13 b31 61 b41
                                                                                 
                                 = (a1 + b) 12 (a1 + b)2 13 (a1 + b)3 16 (a1 + b)4 ,

but it is not a normal subgroup because there does not exist an a1 such that
                                                                                          
            1 0 0 0           1    1
                                   2
                                       1
                                       3
                                           1
                                           6       =       a1   1 2
                                                                2 a1
                                                                       1 3
                                                                       3 a1
                                                                              1 4
                                                                              6 a1        1 0 0 0 ,

where it is noted that the ﬁrst component gives a1 + 1 = 2 whereas the third compo-
nent gives 12 a21 + 16 a31 = 2.
   The set H2 is a subgroup because (ab)1 =a1 +b1 =0; it is normal because, for x ∈ B
                                                                               
and a ∈ H2 , b ∈ H2 satisfying xa = bx is given by b = 0 a2 a3 + 2a2 x1 a4 .

Exercise 42 Show that H3 is a subgroup of B but is not a normal subgroup.


Exercise 43 Show that H4 is a normal subgroup of B.
170                                                   4 Algebraic analysis and integration methods

The Q subgroups


 Deﬁnition 4.7H Q p is the set of members of B such that, if a ∈ Q p , then

                            a([τ k−1 ]) = a(τ)k /k,             k ≤ p.

 Q is the set of members of B such that, if a ∈ Q , then

                         a([τ k−1 ]) = 1k a(τ)k ,           k = 1, 2, . . .


 Theorem 4.7I For any p, Q p ≤ B. Also Q ≤ B.

Proof. We have
                                                            
       (ab) [τ k−1 ] = a [τ k−1 ] +          ∑       a [τ k−1 ] x b( x )
                                        x≤[τ k−1 ]
                                               k−1               
                        1                            k−1
                      = a(τ)k + a(τ)k−1 b(τ) + ∑         a(τ)k−1−i b([τ i ])
                        k                      i=1
                                                      i
                                               k−1       1            
                        1                            k−1
                      = a(τ)k + a(τ)k−1 b(τ) + ∑               a(τ)k−i
                        k                      i=1
                                                      i    k−i
                        1            k
                      = k a(τ) + b(τ)
                                 k
                      = 1k (ab)(τ) .



Realization in Runge–Kutta methods
The elementary weights of a Runge–Kutta method (A, bT , c) lie in Q p , if

                            bT ck−1 = 1k ,           k = 1, 2, . . . , p.                   (4.7 d)



The Cq subgroups

Given a positive integer q, we will construct a reﬁnement of Qq such that it can be
realized by a Runge–Kutta method in which (4.7 d) applies, not only to the output
vector bT , but also to each row eTi A, in the form
                s
               ∑ ai j ck−1
                       j   = 1k cki ,        k = 1, 2, . . . , q,    i = 1, 2, . . . , s.
               j=1
4.7 The B-group and subgroups                                                                         171

The case k = 1 will be recognised as the deﬁnition of ci as the sum of the elements
in row i of A.


Deﬁnition of Sk
This aim is achieved by constructing sets of tree-tree pairs, Sk , k = 2, 3, . . . , q, and
deﬁning Cq in Deﬁnition 4.7J below. In constructing Sk , we recall that t ∗ (k−1 τ k )
has the value k−1 t(∗τ)k and that t ∗ (k−1 t  ) := k−1 t ∗ t  . The following deﬁnition is
used only in the current section

 Deﬁnition 4.7J The sets Sk , k = 1, 2, . . . are deﬁned by

 1. ([[τ k−1 ]], [τ k ]) ∈ Sk ,
 2. (t  ∗ t, t ∗ t ) ∈ Sk if (t, t  ) ∈ Sk , t  ∈ T.

Some examples in the case k = 2 are
                                                                                      
                             ,         ,                ,            ,            ,         ,
                                                                                      
                             ,          ,               ,             ,           ,          ,
                                                                                      
                             ,           ,              ,            ,            ,          .

The diagram (4.7 e) shows a generic example of trees t and t  such that (t, t  ) ∈ Sk .

                                     k−1

                                                                                       k

                t=                    v                     t =
                                                                                  v               (4.7 e)

The discs surrounding the subtrees [τ k−1 ] in t and [τ k ] in t are to identify speciﬁc
vertices which will be referred to in the proof of Theorem 4.7L.

 Deﬁnition 4.7K Cq is the set of members of B such that, for k = 1, 2, . . . , q,

                              a([τ k−1 ]) = k−1 a(τ)k ,                                          (4.7 f)
                                                  −1                     
                                     a(t) = k          a(t )       if (t, t ) ∈ Sk .             (4.7 g)


 Theorem 4.7L
                                       Cq ≤ B,               q = 1, 2, . . .
172                                                             4 Algebraic analysis and integration methods

Proof. Assume a, b ∈ Ck . To show that if a, b satisfy (4.7 f), then the same is true for
ab, use the argument in the proof of Theorem 4.7I. To show that (4.7 g) holds with
a replaced by ab, let t and t be as in diagram (4.7 e), and evaluate (ab)(t), (ab)(t )
using

                          (ab)(t) = a(t) + ∑ a(t t  )b(t  ),                                       (4.7 h)
                                                    t  ≤t
                         (ab)(t ) = a(t  ) + ∑ a(t t )b(t ).                                  (4.7 i)
                                                       t  ≤t 

Consider three cases:
(1) t and t  each contains the vertex v as well as the k descendants of v. Apart from
the circled vertices, t and t  are identical.
(2) t and t do not contain any of the k descendants of v and hence t = t  .
(3) t and t each contain  of the k descendants of v, where 1 ≤  ≤ k − 1. Apart
from the circled vertices, t and t  are identical.
In case (1), t and t have the same form as t and t in diagram (4.7 e), respec-
tively, and hence, b(t) = k−1 b(t ). Furthermore, t t = t  t . Hence, in this case,
a(t t )b(t ) = k−1 a(t t  )b(t ).
In case (2), t  = t  . Furthermore, t t and t  t  have the same forms as in the two
parts of (4.7 e). Hence, also in this case, a(t t )b(t ) = k−1 a(t t  )b(t ).
In case (3), the terms in (4.7 h) and (4.7 i) need to have additional factors n and n
respectively inserted, to allow for replications of choices of  − 1 from k − 1 ( from
k, respectively) vertices. That is,                    
                                   n = k−1
                                         −1 ,    n = k = k n.
We now have
     na(t t )b(t  ) = n k a(t t )−1 b(t ) = k−1 n a(t t  )b(t  ).

Realization in Runge–Kutta methods
A Runge–Kutta method has “stage-order q”, written as C(q), if Ack−1 = 1k ck , k =
1, 2, . . . , q and bck−1 = 1k . That is,
                                                                            4
               ∑sj=1 ai j ck−1
                           j   = 1k cki ,       i = 1, 2, . . . , s,
                                                                                k = 1, 2, . . . , q.
                ∑si=1 bi ck−1
                          i   = 1k ,

For C(q) methods, the corresponding B-series lie in Cq . All collocation methods,
such as Gauss methods, satisfy C(s). For q > 1, C(q) is impossible for explicit Runge–
Kutta methods but it is possible if some limited form of implicitness is allowed, such
as for the fourth order method
                                     0
                                            1      1        1
                                            2      4        4
                                                                        .
                                            1     0         1
                                                   1        2       1
                                                   6        3       6
4.7 The B-group and subgroups                                                                                    173

Explicit methods with order greater than 4 have stage order 2 or higher, except that
this does not apply to the second stage. It is necessary to compensate for this anomaly
by imposing additional conditions, such as b2 = ∑i bi ai2 = ∑i bi ci ai2 = ∑ bi ai j a j2 = 0.

The D subgroups

These subgroups have several important roles in numerical analysis. First, the sub-
group D1 has a historic connection with the work of Kutta [66]. Every fourth order
Runge–Kutta method with four stages is a member of D1 . Although this is not gener-
ally true for higher order explicit methods, it is standard practice, in searching for
such methods, to consider only methods belonging this subgroup. Thus it plays the
role of a “simplfying assumption”. Secondly, Ds contains the s-stage Gauss method.
Finally, D deﬁnes canonical Runge–Kutta, which play a central role in the solution
of Hamiltonian problems, as we will see in Chapter 7.

 Deﬁnition 4.7M D p ⊂ B, D pq ⊂ B and D ⊂ B are deﬁned by
   a ∈ D p if        a(t ∗ t ) + a(t ∗ t) = a(t)a(t  ),              t, t  ∈ T,       |t| ≤ p,
                                                                        
   a ∈ D pq if       a(t ∗ t ) + a(t ∗ t) = a(t)a(t ),                  t, t ∈ T,         |t| ≤ p, |t  | ≤ q,
                                                                        
   a∈D       if      a(t ∗ t ) + a(t ∗ t) = a(t)a(t ),                  t, t ∈ T.



 Theorem 4.7N Each of D pq , D p and D is a subgroup of B.

Proof. It will be sufﬁcient to prove the result in the case of D pq . Assume that a, b ∈ D pq ,
and evaluate (ab)(t ∗ t ):

  (ab)(t ∗ t )
            = a(t ∗ t ) +         ∑  a(t x)a(t x )b(x ∗ x ) + ∑ a(t x)a(t )b(x).
                             x≤t,x ≤t                                             x≤ t

Add the corresponding expression, with t and t interchanged, and we ﬁnd

 (ab)(t ∗ t ) + (ab)(t ∗ t) = a(t)a(t  ) +                   ∑  a(t x)a(t x )b(x)b(x )
                                                          x≤t,x ≤t
                                         + ∑ a(t x )a(t )b( x ) + ∑ a(t x  )a(t)b( x  )
                                              x≤t                              x ≤t
                                                                                       
                                   = a(t) + ∑ a(t x )b( x ) a(t ) + ∑ a(t x  )b( x  )
                                                    x≤t                                 x ≤t
                                                            
                                   = (ab)(t)(ab)(t ).

One of the aims of this chapter is to generalize these constructions to the full set of
trees on which B is deﬁned. A second aim is to interrelate B with the generalizations
174                                                 4 Algebraic analysis and integration methods

of Runge–Kutta methods introduced in [14] as “integration methods” in which the
usual A in (A, b, c) is replaced by a linear operator and b is replaced by a linear
functional in a space of functions on a possibly inﬁnite index set. For example, the
index set {1, 2, . . . , s} could be replaced by the interval [0, 1]. The integration methods
would thus include not only Runge–Kutta methods, but also the Picard construction
                                                    ξ                 
                    Y (x0 + hξ ) = y(x0 ) + h            f Y (x0 + hξ ) d ξ ,
                                                0
                                                    1                
                              y1 = y(x0 ) + h           f Y (x0 + hξ ) d ξ ,
                                                0

where we see that A and bT correspond respectively to the operations

                                        ϕ→          ϕ,
                                                0
                                                    1
                                        ϕ→              ϕ.
                                                0

Discrete gradient methods also fall into the deﬁnition of integration methods.



4.8 Linear operators on B∗ and B0
B and its subspaces recalled

In B-series analysis, mappings deﬁned in terms of the triple (y0 , f , h) are represented
by (Bh y0 )a for a ∈ B∗ . The afﬁne subspace B has a special role as the counterpart to
central mappings which are within O(h) of id h . The members of B act as multipliers
operating on the linear space B∗ and are typiﬁed by Runge–Kutta mappings. The
linear subspace B∗0 corresponds to the space spanned by slopeh ◦ C h , where C h is a
central mapping. This means that B∗0 is the span of BD.


An extended set of linear operators on B∗0
In Chapter 2, Section 2.7 (p. 79), the set S of uni-valent stumps was introduced. In
the case of τ1 ∈ S, power-series φ (τ1 ) = a0 1 + a1 τ1 + a2 τ12 + · · · were introduced.
We will consider B-series ramiﬁcations of these expressions.


Motivation
By introducing linear operators, such as h f  (y0 ), into the computation, a Runge–
Kutta method can be converted to a Rosenbrock method or some other generalization.
For example, the method
              Y1 = y0 ,                                               F1 = f (Y1 ),     (4.8 a)
              Y2 = y0 + a21 hF1 ,                                     F2 = f (Y1 ),     (4.8 b)
               L = h f  (y0 + g1 hF1 + g2 hF2 ),                                       (4.8 c)
4.8 Linear operators on B∗ and B0                                                       175

              y1 = y0 + b1 hF1 + d1 hLF1 + b2 hF2 + d2 hLF2 ,                       (4.8 d)
contains additional ﬂexibility compared with a Runge–Kutta method. If (4.8 d) is
replaced by
                 y1 = y0 + b1 hF1 + d1 hφ (L)F1 + b2 hF2 + d2 hφ (L)F2 ,
numerical properties can be enhanced. We aim to include the use of L within the
B-series formulation.


The operator J
Corresponding to h f  (y0 ), we introduce the linear function J : B0 → B0 , satisfying
Bh Jb = h f  (y0 )Bh b, where b(∅) = 0. By evaluating term by term, we see that
(Jb)([t]) = b(t) for t ∈ T, with (Jb)(t ) = 0 if t  cannot be written as   
                                                                           t = [t].We also
                                                                       
need to ﬁnd the B-series for operations of the form h f (y1 ) = h f (Bh a)y0 so that
we can handle expressions such as L in (4.8 c).

 Theorem 4.8A
                                                           
                     h f  (Bh a)y0 (Bh b)y0 = BBh (aJ)(a−1 b) y0 .                (4.8 e)

Proof. Let y1 = (Bh a)y0 , b = a b so that (4.8 e) becomes
                                                        
                          h f  y1 (Bhb)y1 = BBh (J)(  b) y1 .

which is (4.8 e) after the substitutions y0 → y1 , b → 
                                                       b.




Summary of Chapter 4 and the way forward

Integration methods were introduced as a generalization of Runge–Kutta methods in
which the index set I = {1, 2, . . . , s} is replaced by a more complicated alternative.
Equivalence and reducibility of methods, with an emphasis on the Runge–Kutta
case, were considered. Compositions of methods were introduced leading to the
composition theorem for integration methods. A number of subgroups of B were
introduced, many of which have a relationship with simplifying assumptions for
Runge–Kutta methods.


The way forward

Subgroups of B are used in the construction of the working numerical methods
considered in Chapters 5 and 6. Continuous Runge–Kutta methods have, as a natural
application, the energy-preserving methods of Chapter 7.
176                                                  4 Algebraic analysis and integration methods

Teaching and study notes
Possible supplementary reading includes
Butcher, J.C. An algebraic theory of integration methods (1972) [14]
Butcher, J.C. Numerical Methods for Ordinary Differential Equations (2016) [20]
Hairer, E., Nørsett, S.P. and Wanner, G. Solving Ordinary Differential Equations
I: Nonstiff Problems (1993) [50]
Hairer, E. and Wanner, G. Multistep-multistage-multiderivative methods for ordi-
nary differential equations (1973) [51]
Hairer, E. and Wanner, G. On the Butcher group and general multi-value method
(1974) [52]

Projects
Project 12 Develop the topic of reducibility in Section 4.4 further so that pre-reduced methods
become fully reduced by eliminating unnecessary stages.
Project 13   Investigate the conditions for order 4 for the method given by (4.8 a) – (4.8 d),
