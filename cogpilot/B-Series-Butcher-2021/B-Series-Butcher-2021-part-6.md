# B-Series: Algebraic Analysis of Numerical Methods - Part 6

**Author:** John C. Butcher

**Series:** Springer Series in Computational Mathematics, Volume 55

---

Chapter 5
B-series and Runge–Kutta methods




5.1 Introduction

The aim of this chapter is to continue a selected study of Runge–Kutta methods.
While the emphasis will necessarily be on the application of the B-series approach to
order questions, we will also attempt to carry out a traditional analysis following in
the footsteps of the pioneers of these methods. This will be based on the scalar test
equation y (x) = f (x, y).
    In modern computing there is little interest in numerical methods which are
applicable only to scalar problem and it comes as a cautionary tale that the derivation
of these methods does not automatically yield a method that works more widely. This
will be illustrated by deriving a family of methods for which order 5 is achieved for
scalar problems, whereas only the order 4 conditions hold for a vector problem.
    For the derivation of practical methods, we need to use the key result:

 Theorem 5.1A (Reprise of Theorem 3.6C (p. 127)) For an initial value problem
 of arbitrary dimension, a Runge–Kutta method (A, bT , c) has order p if and only if
                                                1
                                         Φ(t) = t! ,                           (5.1 a)

 for all trees such that |t| ≤ p.



Chapter outline
The theory of order for scalar problems is presented in Section 5.2. The stability of
Runge–Kutta methods is surveyed in Section 5.3, followed by the derivation of ex-
plicit methods in Section 5.4. Order barriers will be introduced in Section 5.5 through
the simplest case (that order p = s is impossible for explicit methods with p > 4). This
is followed in Section 5.6 by a consideration of implicit methods. The generalization
to effective (or conjugate) order is surveyed in Section 5.7.

© Springer Nature Switzerland AG 2021                                               177
J. C. Butcher, B-Series, Springer Series in Computational Mathematics 55,
https://doi.org/10.1007/978-3-030-70956-3_5
178                                                            5 B-series and Runge–Kutta methods

5.2 Order analysis for scalar problems

In contrast to the B-series approach to order conditions, it is also instructive to explore
order conditions in the same way as the early pioneers. Hence, we will review the
work in [82] (Runge, 1895), [56] (Heun, 1900), [66] (Kutta, 1901), who took as their
starting point the scalar initial value problem

                               y (x) = f (x, y),        y(x0 ) = y0 .                          (5.2 a)

In this derivation of the order conditions, ∂x f := ∂ f /∂ x, ∂y f := ∂ f /∂ y, with similar
notations for higher partial derivatives.
   We start with (5.2 a) and ﬁnd the second derivative of y by the chain rule

                                      y = ∂x f + ( ∂y f ) f .

Similarly, we ﬁnd the third derivative
                    2                                                                    2
      y(3) = ( ∂x f + ( ∂x ∂y f ) f ) + ∂y f ( ∂x f + ( ∂y f ) f ) + ( ∂x ∂y f ) f + ( ∂y f ) f 2
                2                          2
           = ∂x f + 2( ∂x ∂y f ) f + ( ∂y f ) f 2 + ( ∂x f ∂y f ) f + ( ∂y f )2 f

and carry on to ﬁnd fourth and higher derivatives. By evaluating the y(n) at x = x0 ,
we ﬁnd the Taylor expansions to use in (5.2 a). A more complicated calculation leads
us to the detailed series (5.2 d) in the case of any particular Runge–Kutta method
and hence to the determination of its order. Details of this line of enquiry will be
followed below.
    The greatest achievement in this line of work was given in [59] (Hut’a, 1956),
where sixth order methods involving 8 stages were derived. In all derivations of new
methods up to the publication of this tour de force, a tacit assumption is made. This
is that a method derived to have a speciﬁc order for a general scalar problem will
have this same order for a coupled system of scalar problems; that is, it will have this
order for a problem with N > 1. This unproven assumption is untrue and it becomes
necessary to carry out the order analysis in a multi-dimensional setting.



Systematic derivation of Taylor series


The evaluation of y(n) , n = 1, 2, . . . , 5, will now be carried out in a systematic manner.
                                        m 
Let
                                              m      m−i n+i
                            Dmn = ∑               ( ∂x ∂y f ) f i .                    (5.2 b)
                                      i=0      i

   We will also write D mn to denote Dmn evaluated at (x0 , y0 ).
5.2 Order analysis for scalar problems                                                    179

 Lemma 5.2A
                             d x Dmn = Dm+1,n + mDm−1,n+1 D10 .
                              d
                                                                                     (5.2 c)


Proof.
         
  d m m         m−i n+i
     ∑ i ( ∂x ∂y f ) f i
 d x i=0
     m                                m 
            m     m−i+1 n+i                  m        m−i n+i+1
 = ∑           ( ∂x      ∂y f ) f i + ∑           ( ∂x ∂y       f ) f i+1
       i=0  i                         i=0     i
                        m 
                            m               m−i n+i
                    +∑          i( ∂x f )( ∂x ∂y ∂y f ) f i−1
                       i=0   i
    m+1          
            m          m        m−i+1      n+i
 = ∑            +              ∂x      ( ∂y f ) f i
     i=0     i       i − 1
                        m 
                                 m!                  m−i n+i
                    +∑ i                   ( ∂x f )( ∂x ∂y ∂y f ) f i−1
                       i=0   i!(m −  i)!
    m+1                                     m−1 
           m+1       m−i+1 n+i                       m−1               m−i−1 n+1+i
 = ∑              ( ∂x      ∂y f ) f i + m ∑               ( ∂x f )( ∂x     ∂y     ∂y f ) f i
     i=0     i                               i=0       i
 = Dm+1,n + mDm−1,n+1 D10 .


Using Lemma 5.2A, we ﬁnd in turn,

      y = D00 ,
      y = D10 ,
   y = D20 + D01 D10 ,
  y(4) = D30 + 2D11 D10 + D11 D10 + D01 (D20 + D01 D10 ),
            = D30 + 3D11 D10 + D01 D20 + D201 D10 ,                                    (5.2 d)
      (5)
  y         = D40 + 3D21 D10 + 3(D21 + D02 D10 )D10 + 3D11 (D20 +D01 D10 ),
              +D11 D20+D01 (D30+2D11 D10 )+2D01 D11 D10+D201 (D20+D01 D10 ),
            = D40 + 6D21 D10 + 3D02 D10 D10 + 4D11 D20 + 7D11 D01 D10 ,
                + D01 D30 + D201 D20 + D301 D10 .

To ﬁnd the order conditions for a Runge–Kutta method, up to order 5, we need to
systematically ﬁnd the Taylor series for the stages, and ﬁnally for the output. In this
analysis, we will assume that ∑sj=1 ai j = ci for all stages. For the stages it will be
sufﬁcient to work only to order 4 so that the scaled stage derivatives will include h5
terms.
180                                                                5 B-series and Runge–Kutta methods

   As a step towards ﬁnding the Taylor expansions of the stages and the output, we
need to ﬁnd the Taylor series for h f (Y ), for a given series Y = y0 + · · · . The following
result does this for an arbitrary weighted series using the terms in (5.2 d).


 Lemma 5.2B If
                D00 + a2 h2 D 10 + a3 h3 21 D 20 + a4 h3 D 01 D 10
   Y = y0 + a1 hD
            + a5 h4 61 D 30 + a6 h4 D 11 D 10 + a7 h4 21 D 01 D 20 + a8 h4 D 201 D 10 + O(h5 ),

 then
              h f (x0 + ha1 ,Y ) = hT1 + h2 T2 + h3 T3 + h4 T4 + h5 T5 + O(h6 ),
 where
        T1 = D 00 ,
        T2 = a1 D10 ,
        T3 = 12 a21 D 20 + a2 D 01 D 10 ,
        T4 = 16 a31 D 30 + a1 a2 D 11 D 10 + 12 a3 D 01 D 20 + a4 D 201 D 10 ,
               1 4
                                                                              
        T5 = 24  a1 D 40 + 12 a21 a2 D 21 D 10 + a1 a3 D 11 D 20 + a1 a4 + a6 D 11 D 01 D 10
           + 12 a22 D 02 D 210 + 16 a5 D 30 D 01 + 12 a7 D 201 D 20 + a8 D 301 D 10 .


                                                                             k   m
Proof. Throughout this proof, an expression of the form ∂x ∂y f is assumed to have
been evaluated at (x0 , y0 ). Evaluate T1 , T2 , T3 , T4 :

           T1 = f (x0 , y0 ) = D00 ,
           T2 = a1 ∂x f + a1 ( ∂y f ) f = a1 D 10 ,
                         2                                    2
           T3 = 12 a21 ∂x f + a21 ( ∂x ∂y )D
                                           D00 + 12 a21 ( ∂y f )D
                                                                D200 + a2 ( ∂y f )D
                                                                                  D10
              = 12 a21 D 20 + a2 D 01 D 10 ,
                         3              2                              2                3
           T4 = 16 a31 ∂x f + 12 a31 ( ∂x ∂y f )D
                                                D10 + 12 a31 ( ∂x ∂y f )D
                                                                        D210 + 16 a31 ∂y f D 310
                                                                   2
                             + a1 a2 ( ∂x ∂y f )D
                                                D10 + a1 a2 ( ∂y f )D
                                                                    D10 D 01
                                  + a3 ( ∂y f )D
                                               D20 + a4 ( ∂y f )D
                                                                D01 D 10
              = 6 a1 D 30 + a1 a2 D 11 D 10 + a3 D 01 D 20 + a4 D 201 D 10 .
                1 3


The evaluation of the terms in T5 is similar and is omitted except, as examples, the
terms in a1 a4 and a6 , which can be found from the simpliﬁed expression

                 h f (x0 + a1 h, y0 + ha1 D 00 + h3 a4 D 01 D 10 + h5 a6 D 11 D 10 ).

The two example terms are
5.2 Order analysis for scalar problems                                                                  181


                               Table 16 Data for Theorem 5.2C

      p       σ               T                       t                         φ                 e

      1       1              D 00                                             ∑ bi                1
                                                                                                   1
      2       1              D 10                                            ∑ bi ci               2

                                                                                                   1
              2              D 20                                           ∑ bi c2i i             3
      3
                                                                                                   1
              1            D 01 D 10                                       ∑ bi ai j c j           6

                                                                                                   1
              6              D 30                                            ∑ bi c3i              4
                                                                                                   1
              1            D 11 D 10                                     ∑ bi ci ai j c j          8
      4
                                                                                                   1
              2            D 01 D 20                                      ∑ bi ai j c2j           12


              1            D 201 D 10                                   ∑ bi ai j a jk ck          1
                                                                                                  24

                                                                                                   1
              24             D 40                                            ∑ bi c4i              5
                                                                                                   1
              2            D 21 D 10                                     ∑ bi c2i ai j c j        10
                                                                                                   1
              2            D 11 D 20                                     ∑ bi ci ai j c2j         15

                                                                                                   7
              1          D 11 D 01 D 10                            ∑ bi (ci + c j )ai j a jk ck   120
      5
              2            D 02 D 210                                  ∑ bi ai j c j aik ck        1
                                                                                                  20
                                                                                                   1
              6            D 01 D 30                                      ∑ bi ai j c33           20


              2            D 201 D 20                                   ∑ bi ai j a jk c2k         1
                                                                                                  60


              1            D 301 D 10                                 ∑ bi ai j a jk ak c        1
                                                                                                  120




                                         2              
            a1 a4 h5 ∂x ∂y f D 01 D 10 + ∂y f f D 01 D 10 = h5 a1 a4 D 10 D 11 D 01 ,
and
                                          h5 a6 ( ∂y f D 11 D 10 ) = h5 a6 D 10 D 11 D 01 ,

which combine to give the single term

                                                    D10 D 11 D 01 .
                                    h5 (a1 a4 + a6 )D


For the stage values of a Runge–Kutta method, we have
182                                                                    5 B-series and Runge–Kutta methods
                                                  s
                           Yi = y0 + ∑ ai j h f (x0 + hc j ,Y j )
                                                  j=1

                                 = y0 + hci D 00 + O(h2 ),

and then, to one further order,
                             s
                 Yi = y0 + ∑ ai j h f (x0 + hc j , y0 + hc j D 00 ) + O(h3 )
                            j=1

                    = y0 + hci D 00 + h2 ∑ ai j c j D 10 + O(h3 ).
                                                        j

A similar expression can be written down for the output from a step

                    y1 = y0 + h ∑ bi D 00 + h2 ∑ bi ci D 10 + O(h3 ).
                                     i                         i


A comparison with the exact solution, y0 + hy (x0 ) + 12 h2 y (x0 ) + O(h3 ), evaluated
using (5.2 d) gives, as second order conditions,

                                             ∑ bi D 00 = D 00 ,
                                              i

                                     ∑ bi ci D 10 = 12 D 10 .
                                         i




 Theorem 5.2C In the statement of this result, the quantities p, T , σ , φ are given
 in Table 16
  1. The Taylor expansion for the exact solution to the initial value problem

                                 y (x) = f (x, y),                y(x0 ) = y0 ,                (5.2 e)

      to within O(h6 ), is y0 plus the sum of terms of the form

                                                      e h p σ −1 T .

  2. The Taylor expansion for the numerical solution y1 to (5.2 e), using a Runge–
      Kutta method (A, bT , c), to within O(h6 ), is y0 plus the sum of terms of the
      form
                                        φ h p σ −1 T .
  3. The conditions to order 5 for the solution of (5.2 e) using (A, bT , c) are the
      equations of the form
                                        φ = e.
5.2 Order analysis for scalar problems                                                 183

   This analysis can be taken further in a straightforward and systematic way and
is summarized, as far as order 5, in Theorem 5.2C. This theorem, for which the
detailed proof is omitted, has to be read together with Table 16 (p. 181). To obtain a
convenient comparison with the non-scalar case, the corresponding t, or more than a
single t, in Theorem 5.1A (p. 177), are also shown in this table.


Relation with isomeric trees
Isomeric trees, introduced in Section 2.7 (p. 77), involve quantities smn which
correspond to D mn in the present section. The isomers occur when the s factors are
formally allowed to commute. Commutation of the D factors in the order analysis
actually occurs because these are scalar quantities. However, if the same analysis
were carried out in the RN setting, commutation would not occur because the D
factors then become vectors, linear operators and multilinear operators. Hence, the
trees comprising the isomeric classes would yield independent order conditions. In
particular, for order 5, the only non-trivial class is

                    {D
                     D11 D 01 D 10 , D 01 D 11 D 10 } = {F(t12 ), F(t15 )}.         (5.2 f)
   These give separate order conditions in the vector case because D 11 and D 01
no longer commute. This phenomenon will be illustrated by the construction of a
method with ambiguous order.


Derivation of an ambiguous method

We will now construct a method which has order 5 for a scalar problem but only order
4 for a vector based problem. This means that all the conditions Φ(ti ) = 1/ti ! are
satisﬁed for i = 1, 2, . . . , 17 except for i = 12 and i = 15, for which the corresponding
order conditions are replaced by
                                                       1     1     7
                         Φ(t12 ) + Φ(t15 ) =               +     =    .            (5.2 g)
                                                      t12 ! t15 ! 120
For convenience, we will refer to the order conditions as (O1), (O2), . . . , (O17),
where (Oi) is the equation
                                 Φ(ti ) = 1/ti !.                               (Oi)
That is,
                                           bT 1 = 1,                                 (O1)
                                           b c = 12 ,
                                             T
                                                                                     (O2)
                                          bT c2 = 13 ,                               (O3)
                                                 ..    ..
                                                  .     .
                                         bT A3 c = 120
                                                    1
                                                       .                            (O17)
184                                                             5 B-series and Runge–Kutta methods

In construcing this method, it is convenient to introduce a vector d T deﬁned as

                                      d T = bT A + bTC − bT ,

where C = diag(c), which satisﬁes the property

                               d T cn−1 = 0,            n = 1, 2, 3, 4,                           (5.2 h)

because d T cn−1 = bT Acn−1 + bT cn − bT cn−1 = 1/n(n + 1) + 1/(n + 1) − 1/n = 0. In
the method to be constructed, some assumptions will be made. These are
                               i−1
                               ∑ ai j c j = 12 c2i ,          i = 2, 3,                           (5.2 i)
                               j=1
                                           c6 = 1,                                                (5.2 j)
                                b2 = b3 = 0.                                                      (5.2 k)

From (5.2 j), (5.2 k), (O1), (O2), (O3), (O5), (O9), it follows that
                                  6
                                 ∑ bi ci (ci − c4 )(ci − c5 )(1 − ci ) = 0,
                                 i=1

                                   120 (20c4 c5 − 10(c4 + c5 ) + 4) = 0
                                    1
                  implying
                  and hence                      ( 12 − c4 )(c5 − 12 ) = 20
                                                                          1
                                                                            .

Choose the convenient values c4 = 14 , c5 = 10
                                            7
                                               together with c2 = 12 and c3 = 1. The
value of b, from (O1), (O2), (O3), (O5), and d from (5.2 h) are

                          b=           1
                                      14     0    0     32
                                                        81
                                                               250
                                                               567
                                                                     5
                                                                     54         ,

                         d =t        1      7    9 − 27 − 27
                                                 7  112  125
                                                                     0     ,

where t is a parameter, assumed to be non-zero. The third row of A can be found
from
                         d2 (− 12 c22 ) + d3 (a32 c2 − 12 c23 ) = 0,      (5.2 l)
because, from (O3) – (O8),

       d T (Ac − 12 c2 ) = bT A2 c + bTCAc − bT Ac − 12 bT Ac2 − 12 bT c3 + 12 bT c2
                         1
                       = 24       + 18           − 16        − 24
                                                               1
                                                                         − 18       + 16   = 0.

 From (5.2 l), it is found that a32 = 13
                                      4 . The values of a42 , and a52 can be written in
terms of the other elements of rows 4 and 5 of A and row 6 can be found in terms
of the other rows. There are now four free parameters remaining: a43 , a53 , a54 and t,
and four conditions that are not automatically satisﬁed. These (O10), O16), (O17)
and (5.2 g). The solutions are given in the complete tableau, with t = −3/140,
5.2 Order analysis for scalar problems                                                       185

                       0
                        1       1
                        2       2

                       1     − 94        13
                                         4
                        1
                        4     64
                                9         5
                                         32     − 64
                                                  3
                                                                                          (5.2 m)
                       7       63        259      231      252
                       10      625       2500    2500      625

                       1    − 27
                              50 − 50
                                  139
                                                − 21
                                                  50
                                                           56
                                                           25
                                                                    5
                                                                    2
                               1                           32      250    5
                               14         0       0        81      567    54



Numerical tests on the ambiguous method


For these tests we use the test problem (1.3 c) (p. 11), written in two alternative
formulations, one scalar and one vector-valued, using matching initial and ﬁnal
values. Let
                                                                            
                                                                     x 0
    t0 = exp( 101
                  π), x0 = t0 sin ln(t0 ) , y0 = t0 cos ln(t0 ) , z0 =         ,
                                                                         y0
                                                                            
                                                                     x 1
    t1 = exp( 12 π), x1 = t1 sin ln(t1 ) , y1 = t1 cos ln(t1 ) , z1 =          .
                                                                         y1

The scalar formulation, as an initial value problem with a speciﬁed output value is
                       dy y−x
                         =    ,            y(x0 ) = y0 ,         y1 = y(x1 )
                       dx y+x
and the vector-valued formulation is
                                      
           d    z1          −1   z2 + z1
                      = z                ,            z(t0 ) = z0 ,      z1 = z(t1 ).
          dt z2                  z2 − z1

Numerical tests were made for each problem on the intervals [x0 , x1 ] and [t0 ,t1 ]
respectively, using a sequence of stepsizes based on a total of n = 5, 10, 20, . . . , 5 × 26
steps, in each case.


   Shown below are the absolute value, or the norm in the vector case, of the error at
the output point. These are denoted by errn . Also shown are errn/2 /errn . For fourth
order behaviour, these ratios should be approximately 16 and for ﬁfth order, they
should be approximately 32. The results are given in the display
186                                                         5 B-series and Runge–Kutta methods

           n             errn          errn/2 /errn            errn         errn/2 /errn
        5 × 20     4.3170 × 10−4                       9.4865 × 10−4
        5 × 21     1.0906 × 10−5         39.583        5.2577 × 10−5          18.043
        5 × 23     2.8486 × 10−7         38.286        3.4454 × 10−6          15.260
        5 × 24     8.3007 × 10−9         34.318        2.3100 × 10−7          14.915
        5 × 25    2.5422 × 10−10         32.651        1.5117 × 10−8          15.281
        5 × 26    7.8960 × 10−12         32.198        9.6908 × 10−10         15.599

As we see, the predictions are conﬁrmed by the computed error ratios.


The ﬁrst sixth order method

In [59, 60] (Hut’a, 1966, 1967), the detailed conditions for a method with 8 stages to
have order six were derived. The very intricate analysis in this work was combined
with stage order conditions and other simplifying assumptions to yield methods with
the required properties. In [8] (Butcher, 1963) it was shown that the simpliﬁcations
had the effect of forcing the 31 conditions assumed by Hut’a to actually hold for the
full set of 37 conditions required for applications to vector-valued problems. We will
review this result starting with Table 17, which generalizes the pairing of two trees
because they share an isomeric class. This generalization is taken to order 6 trees.
Using Table 17, we can write down the scalar order conditions in the case of these
isomeric classes.

                                Φ(t12 ) + Φ(t15 ) = t 1 ! + t 1 ! ,
                                                       12       15

                                Φ(t21 ) + Φ(t29 ) = t ! + t 1 ! ,
                                                       1
                                                     21    29

                                Φ(t25 ) + Φ(t31 ) = t 1 ! + t 1 ! ,
                                                       25       31

                                Φ(t28 ) + Φ(t33 ) = t ! + t 1 ! ,
                                                       1
                                                     28    33

                   Φ(t26 ) + Φ(t32 ) + Φ(t35 ) = t 1 ! + t 1 ! + t 1 ! .
                                                       26       32     35

The single-tree isomeric classes provide 26 order conditions which, together with the
5 listed above, constitute the 31 conditions to obtain order 6 for scalar problems, as
in [59] (Hut’a, 1966).
    Remarkably, Hut’a’s methods are actually order 6, even for high-dimensional
problems. To verify this, it is only necessary to show that

                      Φ(ti ) = t1! ,        i = 15, 29, 31, 32, 33, 35.                    (5.2 n)
                                   i

But each of these trees is of the form t = [t  ] so that, according to the D(1) simplifying
assumption, which holds for the Hut’a methods,
5.3 Stability of Runge–Kutta methods                                                        187


      Table 17 Trees arranged in isomeric classes, with corresponding order conditions


                        D 11 D 01 D 10                      Φ(t12 ) = t 1 !
                                                                       12

                        D 01 D 11 D 10                      Φ(t15 ) = t 1 !
                                                                       15

                        D 21 D 01 D 10                      Φ(t21 ) = t 1 !
                                                                       21

                        D 01 D 21 D 10                      Φ(t29 ) = t 1 !
                                                                       29

                        D 11 D 01 D 20                      Φ(t25 ) = t 1 !
                                                                       25

                        D 01 D 11 D 20                      Φ(t31 ) = t 1 !
                                                                       31

                      D 02 D 10 D 01 D 10                   Φ(t28 ) = t 1 !
                                                                       28

                      D 01 D 02 D 10 D 10                   Φ(t33 ) = t 1 !
                                                                       33


                      D 11 D 01 D 01 D 10                   Φ(t26 ) = t 1 !
                                                                       26


                      D 01 D 11 D 01 D 10                   Φ(t32 ) = t 1 !
                                                                       32


                      D 01 D 01 D 11 D 10                   Φ(t35 ) = t 1 !
                                                                       35




                     Φ(t) − Φ(t ) + Φ(t ∗ τ) = t!
                                                 1
                                                    − t1 ! + (t  ∗τ)!
                                                                   1
                                                                        .

Consequently, Φ(t) = 1/t! because Φ(t ) = 1/t  ! and Φ(t ∗ τ) = 1/(t  ∗ τ)!, in each
of these cases listed in (5.2 n).



5.3 Stability of Runge–Kutta methods

The stability function

Given a method (A, bT , c), consider the result computed for the linear problem, y = qy,
where q is a (possibly complex) constant. If z = hq, the output after a single step is
y1 = R(z)y0 , where R(z) is the “stability function”, deﬁned by

                                          Y = y0 1 + zAY,                                (5.3 a)
                                      R(z)y0 = y0 + zbTY.                                (5.3 b)

From (5.3 a), Y = y0 (I − zA)−1 and from (5.3 b),
188                                                           5 B-series and Runge–Kutta methods

                                 R(z) = 1 + zbT (I − zA)−1 1.                            (5.3 c)

For an explicit s-stage method, it further follows that
                                                s
                                 R(z) = 1 + ∑ Φ([n 1]n )zn .                             (5.3 d)
                                              n=1




Exercise 44 Verify (5.3 d) for an explicit s stage method.


If an explicit method has order p = s, it further follows that
                                                     p    n
                                      R(z) = 1 + ∑ zn! .                                 (5.3 e)
                                                    n=1




Exercise 45 Verify (5.3 e) for an explicit method with p = s .


Finally, we ﬁnd a convenient general formula for the stability function. See for
example [53] (Hairer, Wanner, 1996).


 Lemma 5.3A The stability function for a Runge–Kutta method (A, bT , c) is equal
 to                                               
                                det I + z(1bT − A)
                        R(z) =                       .                    (5.3 f)
                                     det(I − zA)


Proof. If a square non-singular matrix M is perturbed by a rank 1 matrix uvT , the
determinant is modiﬁed according to det(M + uvT ) = det(M) + vT adj(M)u. It follows
that det(M + uvT )/ det(M) = 1 + vT M −1 u. Substitute M = I − zA, uT = zbT , v = 1 and
the result follows from (5.3 c).




The stability region and the stability interval

The set of points in the complex plane for which |R(z)| ≤ 1 is the “stability region”.
The interval I = [−r, 0], with maximal r, such that I lies in the stability region, is the
stability “interval”. In the case of the explicit methods for which 1 ≤ p = s ≤ 4, the
boundaries of these ﬁnite regions are as shown in the diagram:
5.4 Explicit Runge–Kutta methods                                                  189


                                                               3i

                                          4
                                              3
                                                  2
                                                      1


                                  −3                          0




                                                              −3i‘




The stability intervals are
                                  p = 1 : I = [−2.000, 0]
                                  p = 2 : I = [−2.000, 0]
                                  p = 3 : I = [−2.513, 0]
                                  p = 4 : I = [−2.785, 0]

The stability interval is an important attribute of a numerical method because, for a
decaying exponential component of a problem, we want to avoid exponential growth
of the corresponding numerical approximation.

Exercise 46 Find the stability function for the implicit method

                                                  24 − 24
                                          1        7    1
                                          4
                                                  2       1
                                          1       3       3
                                                  2       1
                                                  3       3




5.4 Explicit Runge–Kutta methods


In this section we will review the derivation of the classical explicit methods in the
full generality of multi-dimensional autonomous problems. That is, we will deﬁne
the order of a method as given by Theorem 5.1A (p. 177)).
190                                                             5 B-series and Runge–Kutta methods

Low orders

Order 1
For a single stage there is only the Euler method but for s > 1 other options are
possible such as

                                          0
                                          1      1                                         (5.4 a)
                                                 7    1
                                                 8    8

This so-called “Runge–Kutta–Chebyshev” method [90] (van der Houwen, Sommeijer,
1980) is characterized by its extended real interval of stability; that is, a high value of
r for its stability interval I = [−r, 0]. For this method the stability function is R(z) =
1 + z + 18 z2 compared with RE (z) = 1 + z for the Euler method. The corresponding
stability regions are shown in the following diagrams, with Euler on the left and
(5.4 a) on the right.


                    i                                                                       i


   −2               0          −8                                                           0
                    −i                                                                      −i



The extended stability interval [−8, 0] is regarded as an advantage for the solution of
mildly stiff problems.


Order 2

From the order conditions
                                         b1 + b2 = 1,
                                              b2 c2 = 12 ,

the family of methods is found, where c2 = 0,

                                    0
                                    c2          c2              .
                                          1 − 2c12         1
                                                          2c2


This family, particularly the special cases c2 = 12 and c2 = 1, were made famous in
the pioneering paper by Runge [82].
5.4 Explicit Runge–Kutta methods                                                                             191

Exercise 47 Derive an explicit Runge–Kutta method with s = p = 2 and b2 = 1.



Order 3

These methods are associated with the paper by Heun [56]. For a three stage method,
with p = 3, we need to satisfy

                                     b1 + b2 + b3 = 1,
                                      b2 c2 + b3 c2 = 12 ,
                                      b2 c22 + b3 c22 = 13 ,
                                             b3 a32 c2 = 16 .

There are four cases to consider
   (i) c2 = c3 , c2 = 0 = c3 , c2 = 23 = c3 ,
  (ii) c3 = 23 , 0 = c2 = 23 ,
(iii) c2 = 23 , c3 = 0, b3 = 0,
 (iv) c2 = c3 = 23 .
These are

                             0
                             c2              c2
 (i)                                 (3c22 −3c2 +c3)c3                    c3 (c2 −c3 )                   ,
                             c3         (3c2 −2)c2                        (3c2 −2)c2
                                    6c2 c3 −3c2 −3c3 +2                 3c3 −2             2−3c2
                                            6c2 c3                    6c2 (c3 −c2 )      6c3 (c3 −c2 )

                             0
                             c2       c2
(ii)                         2      6c2 −2       2                ,
                             3       9c2        9c2
                                      1                      3
                                      4           0          4

                             0
                             2        2
                             3        3
(iii)                                                                 ,
                             0     − 4b3
                                      1            1
                                                  4b3

                                   4 − b3
                                   1               3
                                                   4         b3

                             0
                             2         2
                             3         3
(iv)                                                                           .
                                   3 − 4b3
                             2     2    1               1
                             3                         4b3

                                                   4 − b3
                                       1           3
                                       4                              b3
192                                                      5 B-series and Runge–Kutta methods

Examples from each case are

             0                                                 0
             1     1                                           1      1
             2     2                                           3      3
(i)                              ,              (ii)                                    ,
             1 −1      2                                       2
                                                               3      0       2
                                                                              3
                   1    2    1                                        1            3
                   6    3    6                                        4       0    4

             0                                                 0
             2     2                                           2      2
             3     3                                           3      3
(iii)                            ,              (iv)           2              2         .
             0 −1      1                                       3      0       3
                        3    1                                        1       3    3
                   0    4    4                                        4       8    8




Exercise 48 Derive an explicit Runge–Kutta method with s = p = 3, c2 = c3 , b2 = 0.




Order 4

To obtain an order 4 method, with s = 4 stages, the equations Φ(t) = 1/t!, |t| ≤ 4,
must be satisﬁed. Write these as ui = vi , i = 1, 2, . . . , 8, where the vectors u and v are
given by

             ⎡                                           ⎤                ⎡        ⎤
                            b1 + b2 + b3 + b4                                 1
             ⎢                                            ⎥             ⎢          ⎥
             ⎢                                            ⎥             ⎢      1   ⎥
             ⎢           b2 c2 + b3 c3 + b4 c4            ⎥             ⎢     2    ⎥
             ⎢                                            ⎥             ⎢          ⎥
             ⎢                                            ⎥             ⎢     1    ⎥
             ⎢                2       2
                         b2 c2 + b3 c3 + b4 c4 2
                                                          ⎥             ⎢     3    ⎥
             ⎢                                            ⎥             ⎢          ⎥
             ⎢                                            ⎥             ⎢     1    ⎥
             ⎢     b3 a32 c2 + b4 a42 c2 + b4 a43 c3      ⎥             ⎢     6    ⎥
             ⎢
        u := ⎢                                            ⎥,       v := ⎢          ⎥.       (5.4 b)
                                                          ⎥             ⎢     1    ⎥
             ⎢                3       3
                         b2 c2 + b3 c3 + b4 c4 3          ⎥             ⎢          ⎥
             ⎢                                            ⎥             ⎢     4
                                                                                   ⎥
             ⎢                                            ⎥             ⎢     1    ⎥
             ⎢ b3 c3 a32 c2 + b4 c4 a42 c2 + b4 c4 a43 c3 ⎥             ⎢          ⎥
             ⎢                                            ⎥             ⎢     8    ⎥
             ⎢                                            ⎥             ⎢     1    ⎥
             ⎢     b3 a32 c22 + b4 a42 c22 + b4 a43 c23   ⎥             ⎢          ⎥
             ⎣                                            ⎦             ⎣     12   ⎦
                                                                               1
                              b4 a43 a32 c2                                   24


The value of c4
If c2 , c3 , c4 are parameters we might attempt to solve the system in three steps: (i)
solve the linear system u2 = v2 , u3 = v3 , u5 = v5 , to obtain b2 , b3 , b3 , (ii) solve
u4 = v4 , u6 = v6 , u7 = v7 to obtain a32 , a42 , a43 , (iii) substitute the solutions to steps
(ii) and (iii) into u8 = v8 . This will give a condition on the c values. A short-circuit
to this analysis is given in the following:
5.4 Explicit Runge–Kutta methods                                                  193



 Lemma 5.4A For any explicit Runge–Kutta method, with p = s = 4, c4 = 1.

Proof. From the equations
                                 (u7 − c2 u4 ) =        b4 a43 · (c3 − c2 )c3 ,
                                 (u6 − c4 u4 ) = b3 (c3 − c4 ) · a32 c2 ,
                  u5 − (c2 + c4 )u3 + c2 c4 u2 = b3 (c3 − c4 ) · (c3 − c2 )c3 ,
                                             u8 =         b4 a43 · a32 c2 ,
it follows that
                                                                    
            (u7 − c2 u4 )(u6 − c4 u4 ) − u5 − (c2 + c4 )u3 + c2 c4 u2 u8 = 0.

Substitute ui = vi , with the result
                                                                     
             (v7 − c2 v4 )(v6 − c4 v4 ) − v5 − (c2 + c4 )v3 + c2 c4 v2 v8 = 0,
which simpliﬁes to
                                       c2 (c4 − 1) = 0.
If c2 = 0 we deduce the contradiction u8 = 0 = v8 . Hence, c4 = 1.


 Lemma 5.4B For any explict Runge–Kutta method, with p = s = 4, D(1) holds.
 That is, di = 0, where d j = ∑i> j bi ai j − b j (1 − c j ), j = 1, 2, 3, 4.

Proof. From Lemma 5.4A, d4 = 0; from ∑4j=1 d j (c j − c2 )c j = 0, d3 = 0; from
∑4j=1 d j (c j = 0,d2 = 0; and from ∑4j=1 d j = 0, d1 = 0.



Reduced tableaux for order 4

For Runge–Kutta methods in which D(1) holds, the ﬁrst column and last row of A
can be omitted from the analysis and restored later when the derivation of the method
is completed in other respects. Let 
                                    bT = bT A and consider the reduced tableau

                                        c2
                                        c3    a32         ,
                                               
                                               b2    
                                                     b3
satisfying the reduced order conditions

                                      
                                      b2 c2 + 
                                              b3 c3 = 16 ,
                                      
                                      b2 c22 +          1
                                               b3 c23 = 12 ,
194                                                                        5 B-series and Runge–Kutta methods

                                                                 1
                                                       ba32 c2 = 24 .

Assuming c2 , c3 ∈ {0, 1} and c2 = 12 (to avoid 
                                                b3 = 0), we ﬁnd that

                                                      2c3 − 1
                                               b2 =                 ,
                                                   12c2 (c3 − c2 )
                                                    (1 − 2c2 )
                                              b3 =                  ,
                                                   12c3 (c3 − c2 )
                                                    c3 (c3 − c2 )
                                             a32 =                ,
                                                   2c2 (1 − 2c2 )

leading to the full tableau:

   0
  c2            c2
         c3 (3c2 −4c22 −c3 )            c3 (c3 −c2 )
  c3        2c2 (1−2c2 )               2c2 (1−2c2 )                                                      , (5.4 c)
   1            a41                        a42                       a43
        6c2 c3 −2c2 −2c3 +1            2c3 −1                   (1−2c2 )           6c2 c3 −4c2 −4c3 +3
               12c2 c3 )        12c2 (1−c2 )(c3 −c2 )     12c3 (1−c3 )(c3 −c2 )     12(1−c3 )(1−c2 ))


where
                       12c22 c23 − 12c22 c3 − 12c2 c23 + 4c22 + 15c2 c3 + 4c23 − 6c2 − 5c3 + 2
               a41 =                                                                           ,
                                            2c3 c2 (6c2 c3 − 4c2 − 4c3 + 3)
                         (1 − c2 )(−4c23 + c2 + 5c3 − 2)
               a42 =                                         ,
                     2c2 (c3 − c2 )(6c2 c3 − 4c2 − 4c3 + 3)
                           (1 − 2c2 )(1 − c2 )(1 − c3 )
               a43 =                                       .
                     c3 (c3 − c2 )(6c2 c3 − 4c2 − 4c3 + 3)

 This “general” case takes a simpler form if c2 and c3 are symmetrically placed in
[0, 1]. Write c2 = 1 − c3 and (5.4 c) becomes

        0
      1 − c3              1 − c3
                         c3 (1−2c3 )                         c3
       c3                 2(1−c3 )                        2(1−c3 )
                  (1−2c3 )(4−9c3 +6c23 )               c3 (1−2c3 )                1−c−3
        1        2(1−c3 )(−1+6c3 −6c23 )          2(1−c3 )(−1+6c3 −6c23 )       1−6c3 +6c32

                       −1+6c3 −6c23                           1                       1         −1+6c3 −6c23
                        12c3 (1−c3 )                    12c3 (1−c3 )            12c3 (1−c3 )     12c3 (1−c3 )
                                                                                                           (5.4 d)

Exercise 49 Derive an explicit Runge–Kutta method with s = p = 4, c2 = 13 , c3 = 34 .
5.4 Explicit Runge–Kutta methods                                                                            195

Kutta’s classiﬁcation of fourth order methods

In the famous 1901 paper by Kutta [66] the classical theory of Runge–Kutta methods,
up to order 4, was effectively completed. Given the value c4 = 1, 5 families of
methods were formulated, in addition to the general case. These are as follows, where
the reduced tableau is shown in the ﬁrst column and the full tableau in the second. In
each case λ is a non-zero parameter.

                                             0
                                             λ       λ

                                                   2 − 8λ
                                             1     1    1   1
      λ                                      2             8λ                                            (5.4 e)
      1     1
      2    8λ                    ,           1   −1 + 2λ
                                                       1
                                                         − 2λ
                                                            1
                                                                       2                 ,
                         1                              1              2        1
            0            3                              6       0      3        6

                                             0
                                             1       1
                                             2       2
      1
                                                  2 − 12λ
                                             1    1    1          1
      2                                      2                   12λ                                     (5.4 f)
      1
      2
                1
               12λ                       ,   1       0         1 − 6λ           6λ                   ,

                                                               3 − 2λ
                                                     1         2                         1
           3 −λ              λ
           1                                                                    2λ
                                                     6                                   6

                                             0
                                             1      1
                                             1      3           1
      1                                      2      8           8                                        (5.4 g)
      1    1
      2    8                 ,               1    1 − 4λ
                                                       1
                                                             − 12λ
                                                                1           1
                                                                           3λ                    ,
                     1
                                                              6 −λ                   λ
           0                                        1         1            2
                     3                              6                      3

                                             0
                                             1         1
                                             2         2
      1
      2                              ,       0    − 12λ1        1
                                                               12λ                                       (5.4 h)
            1
      0    12λ                               1   − 12 − 6λ      3
                                                                2      6λ                    .
               1
               3         λ                         6 −λ
                                                   1            2
                                                                3          λ         1
                                                                                     6




The contributions of S. Gill

A four stage method usually requires a memory of size 4N +C to carry out a single
step. In [45] (Gill, 1951), a new fourth order method was derived in which the
196                                                                  5 B-series and Runge–Kutta methods

memory requirements were reduced to 3N +C. However, the work of S. Gill in this
paper has a wider signiﬁcance.
   An important feature of Gill’s analysis, and derivation of the new method, was
the use of elementary differentials written in tensor form

                        f i,   f ji f j ,          f f ,
                                                i j k
                                              f jk             f ji fkj f k ,   ...,

represented by the trees
                                     ,       ,      ,      ,     ....

The Gill Runge–Kutta method
To motivate this discussion, we need to ask how many vectors are generated in each
step of the calculation process, as stage values are evaluated and stage derivatives are
then calculated. The calculations, and a list of the vectors needed at the end of each
stage derivative calculation, are:
          Y1 = y0 ,
                                  
         hF1 = h f (Y1 ), Y1 hF1 ,
          Y2 = y0 + a21 hF1 ,
                                       
         hF2 = h f (Y2 ), Y2 hF1 hF2 ,
          Y3 = y0 + a31 hF1 + a32 hF2 ,
                                                                            
         hF3 = h f (Y3 ), Y3 y0 +a41 hF1 +a42 hF2 y0 +b1 hF1 +b2 hF2 hF3 ,
          Y4 = y0 + a41 hF1 + a42 hF2 + a43 hF3 ,
                                                                
         hF4 = h f (Y4 ), Y4 y0 + b1 hF1 + b2 hF2 + b3 hF3 hF4 .

Note that, at the time h f (Y3 ) is about to be computed, hF1 and hF2 are still needed
for the eventual calculation of Y4 and the output value y1 and these are shown, in the
list of required vectors, as partial formulae for these quantities, which can be updated
as soon as hF3 and hF4 become available.
    At the point in the process, immediately before hF3 is computed, we need to
have values of the three vectors, Y3 = y0 + a31 hF1 + a32 hF2 , y0 + a41 hF1 + a42 hF2
and y0 + b1 hF1 + b2 hF2 . This information can be stored as just two vectors if
                                   ⎛⎡              ⎤⎞
                                        1 a31 a32
                                   ⎜⎢              ⎥⎟
                               det ⎝⎣ 1 a41 a42 ⎦⎠ = 0.                            (5.4 i)
                                        1 b1 b2
Gill’s derivation based on the Kutta class (5.4 f), for which (5.4 i) holds gives
                                     72λ 2 − 24λ + 1 = 0,
leading to                                           1 √
                                            λ = 16 ± 12  2.
                            √
Gill recommends λ = 16 − 12
                         1
                             2 based on the magnitude of the error coefﬁcients.
5.4 Explicit Runge–Kutta methods                                                                   197

An alternative solution satisfying Gill’s criterion
If the assumption c2 = 1 − c3 is made, as in (5.4 d), Gill’s criterion (5.4 i) gives
                                        3c33 − 3c3 + 1 = 0,
with solutions                                      π
                                       c3 = − √2 cos 18  ,                                    (5.4 j)
                                                3
                                                     
                                       c3 = √2 cos 5π
                                                    18 ,                                     (5.4 k)
                                              3
                                                   
                                       c3 = √2 cos 7π
                                                    18 .                                      (5.4 l)
                                                3
The ﬁrst case (5.4 j) gives c2 > 1, c3 < 0 and this should be rejected. The second
case (5.4 k) has elements of rather large magnitude in A and seems, on this basis, less
desirable than the third case (5.4 l), for which the tableau is, to 10 decimal places,

   0
   0.6050691568       0.6050691568
   0.3949308432       0.0685790216          0.3263518216
   1                −0.5530334218           0.1581025768     1.3949308450                          .
                      0.1512672890          0.3487327110     0.3487327110      0.1512672890


Fifth order and higher order methods

The pattern which holds up to order 4, in which methods with order p exist with s = p
stages, does not hold above order 4. It will be shown in Theorem 5.5A (p. 200) that,
for p > 4, s > p is necessary. We will complete this survey of explicit Runge–Kutta
methods by presenting a single ﬁfth order method with 6 stages and referring to a
famous method with p = 10.

The role of simplifying assumptions
The D(1) condition was necessary in the case of s = p = 4 and, at the same time,
simpliﬁed the order requirements. The C(2) condition is not really possible because
it would imply 12 c22 = a21 c1 = 0. This would mean that c2 = c1 and the ﬁrst two
stages compute the same value and could be combined into a single stage. Taking
this argument further, we conclude that all stages are equivalent to a single stage and
only order 1 is possible.
   But for order at least 5, it becomes very difﬁcult to construct methods without
assuming something related in some way to C(2). We can see this by evaluating the
following expression on the assumption that the order 5 order conditions are satisﬁed.

       s    s−1               2       s    s−1                 s s−1                  s
    ∑ bi ∑ ai j c j − 12 c2i        = ∑ bi ∑ ai j c j aik ck − ∑ ∑ bi c2i ai j c j + 14 ∑ bi c4i
    i=1      j=1                      i=1    j=1               i=1 j=1                 i=1
                                      1
                                    = 20 − 10
                                           1    1
                                              + 20
                                    = 0.
198                                                                      5 B-series and Runge–Kutta methods

If for example the C(2) requirement were satisﬁed for each stage except the second
stage, it would be necessary that b2 = 0. It would also be necessary that

                   ∑ bi ai2 = 0, ∑ bi ci ai2 = 0, ∑ bi ai j a j2 = 0,                              (5.4 m)

otherwise it would be impossible for the following three pairs of conditions to hold
simultaneously.
                    ∑ bi ai j a jk ck = 241 ,    ∑ bi ai j c2j = 121 ,
                   ∑ bi ci ai j a jk ck = 301 ,                   ∑ bi ci ai j c2j = 151 ,
                                            1
                   ∑ bi ai j a jk ak c = 120  ,                ∑ bi ai j a jk c2k = 601 .
   If D(1) holds, a suitably modiﬁed form of C(2) does not require (5.4 m) but only
∑ bi (1 − ci )ai2 = 0, in addition to b2 = 0. These assumptions open a path to the
construction of ﬁfth order methods. Rewrite (5.4 b) with ui replaced by ui , where the
bi are replaced by
                            bi = ∑ b j a ji ,  i = 1, 2, 3, 4,
                                         j>i

and the vi are replaced by the elements of the vector
                                                                                        
                   vT =       1
                              2
                                     1
                                     6
                                               1
                                               12
                                                      1
                                                      24
                                                            1
                                                            20
                                                                     1
                                                                     40
                                                                              1
                                                                              60
                                                                                    1
                                                                                   120    .

To enable us to focus on the parts of the tableau that are most signiﬁcant, we use a
reduced tableau of the form

                                         c3
                                         c4         a43
                                                                          .
                                         c5         a53    a54
                                                    b3     b4       b5

We need to solve
                              b3 c3 + b4 c4 + b5 c5 = 16 ,
                              b3 c23 + b4 c24 + b5 c25 = 12
                                                         1
                                                            ,
                              b3 c33 + b4 c34 + b5 c35 = 20
                                                         1
                                                            ,
                                  b5 a54 c4 (c4 − c3 ) = 60
                                                         1
                                                            − 24
                                                              1
                                                                 c3 .

These conditions give no information about a43 and a53 . We also need to take into
account the relations based on the C(2) condition. These are

                                  a32 c2 = 12 c23 ,
                                  a42 c2 = 12 c24 − a43 c3 ,
                                  b5 a52 = −b3 a32 − b4 a42 ,
                                  a53 c3 = 12 c25 − a52 c2 − a54 c4 .
5.5 Attainable order of explicit methods                                                        199

We can solve these equations sequentially for a32 , a42 , a52 , and a53 , with a43 chosen
arbitrarily, to complete the reduced tableau. In the special case
                                                                                  T
                                c=        0   1
                                              4
                                                       1
                                                       4
                                                                 1
                                                                 2
                                                                       3
                                                                       4       1        ,

with the chosen value a43 = 1 we ﬁnd the reduced tableau to be

                                          1
                                          4       1
                                          1
                                          2       0         9
                                                            16             ,
                                                  4         1         4
                                                  15        15        45

leading to the full tableau

                           0
                            1        1
                            4        4
                            1        1       1
                            4        8       8
                            1
                            2        0    − 12         1                                    .
                            3       3                       9
                            4     16          0        0   16

                           1     − 37         2
                                              7         7 − 7
                                                       12  12              8
                                                                           7
                                     7                 16         2        16        7
                                     90       0        45        15        45       90




A tenth order method

Using a combination of simplifying assumptions and variants of these assumptions, a
17 stage method of order 10 [47] (Hairer, 1978) has been constructed. It is not known
if methods of this order exist with s < 17.




5.5 Attainable order of explicit methods


As we have seen, it is possible to obtain order p with s = p stages for p ≤ 4. However,
for p ≥ 5, methods only exist if s ≥ p + 1. Furthermore, for p ≥ 7, methods only
exist if s ≥ p + 2.

   Before presenting these results, we make some preliminary remarks.
200                                                        5 B-series and Runge–Kutta methods

Remarks

It can always be assumed that c2 = 0
If a method existed with c2 = 0, the second stage will give a result identical to the
ﬁrst stage so that the second stage can be effectively eliminated. That is, the tableau

                                 0
                                 0     0
                                c3    a21
                                c4    a31    a32               ,
                                 ..    ..     ..      ..
                                  .     .      .           .
                                      b1         b2   ···

can be replaced by
                                 0
                                c3         a21
                                c4    a31 + a32                ,
                                 ..       ..          ..
                                  .        .               .
                                       b1 + b2        ···
with one less stage but the same order.


Some low rank matrix products
In the proofs given in this section, products of matrices U and V occur in which
many terms cancel from UV because of zero elements in the ﬁnal columns of U
and the initial rows of V . Typically the rows of U have the form bT Am , with some of
the A factors replaced by some other strictly lower triangular matrices, and with the
columns of V of the form An c, again with some of the A factors replaced by strictly
lower triangular matrices. From the speciﬁc structure of U and V , an upper bound on
the rank of UV can be given.


Order bounds

In this section, C = diag(c), dI = 1 − ci , D = I −C.

 Theorem 5.5A No (explicit) Runge–Kutta s-stage method exists with order p =
 s > 4.

Proof. We will assume a method of this type exists and obtain a contradiction. Let
5.6 Implicit Runge–Kutta methods                                                                 201
                                       ⎡                          ⎤
                                                  bT A p−3
                                 U =⎣                             ⎦,
                                            bT A p−4 (D − d4 I)

                                  V=       Ac     (C − c2 I)c     .

Since each of these matrices has rank 1, their product is singular. However, the
product is given by
             ⎡                                                          ⎤
                     1                         2     c2
             ⎢       p!                       p! − (p−1)!               ⎥
     UV = ⎢  ⎣ p−3
                                                                        ⎥
                                                                        ⎦
                        c4      2(p−3)     2(p−3)c2      2c4      c2 c4
                 s! − (p−1)!       p! − (p−1)! − (p−1)! + (p−2)!
                                       ⎡                          ⎤
                                           1          1                             
                    1        0         ⎢     p!       (p−1)! ⎥ 1                  2
          =                            ⎣                     ⎦                               .
                  p−3      −d4               1          1        0              −c2
                                           (p−1)!     (p−2)!

Since the last two factors are non-singular, it follows that d4 = 0 so that c4 = 1.
Repeat this argument with V unchanged but
                               ⎡                       ⎤
                                         bT A p−3
                          U =⎣                         ⎦,
                                  bT A p−5 (D − d5 I)A

and it follows that c5 = 1. From c4 = c5 = 1, it follows that bT A p−5 (I − diag(c))A2 c
is zero. However, by the order conditions,
                                                                p−4
                           bT A p−5 (I − diag(c))A2 c = p! = 0.


For further results on attainable order, see [11] (Butcher 1965), [16] (Butcher 1985),
[20] (Butcher 2016).



5.6 Implicit Runge–Kutta methods

The classical methods in which A is strictly lower triangular can be implemented
explicitly. That is the stages can be evaluated in sequence, using only information
already available so that the stage values, Yi , and the corresponding B-series, which
will be denoted by ηi , are computed by

                  Yi = y0 + h ∑ ai j f (Y j ),                        i = 1, 2, . . . , s,
                                 j<i

                  ηi = 1 + ∑ ai j η j D,                              i = 1, 2, . . . , s.
                            j<i
202                                                    5 B-series and Runge–Kutta methods

In this section, we consider the consequences of allowing A to have non-zero elements
on or above the diagonal. Four examples are the methods with tableaux
                               √                √
                           2−6          4−6
                           1 1          1 1              1
                                3                3       4
                               √                             √
                           1 1
                           2+6 3
                                            1
                                            4
                                                     1 1
                                                     4+6         3 ,              (5.6 a)
                                            1            1
                                            2            2


                                      12 − 12
                                  1   5    1
                                  3

                                  1   3
                                      4
                                            1
                                            4    ,                                (5.6 b)
                                       3    1
                                       4    4
                                √             √
                          1 − 12 2     1 − 12 2         0
                                           √                √
                              1          1
                                         2 2         1 − 12 2 ,                   (5.6 c)
                                           √                √
                                         1
                                         2 2         1 − 12 2
                             √                √             √
                          3−2 2        4−4 2
                                       5    3
                                                     4−4 2
                                                     7    5
                                              √             √
                                                     4−4 2 ,
                                       1    1        3    1
                              1        4+4 2                                      (5.6 d)
                                              √             √
                                                     4−4 2
                                       1    1        3    1
                                       4+4 2

The fourth order method (5.6 a), due to Hammer and Hollingsworth [54] (1955),
is a member of the important Gauss family. These methods have maximal order
p = 2s amongst methods with s stages. They are A-stable and symplectic but they
are fully implicit with high implementation costs, which increase as s increases. The
method (5.6 b) is an example of the Radau IIA family. The order is only p = 2s − 1,
for the method with s stages, but it has the perceived advantage that bT = eTs A. The
effect of this restriction is a stronger stability property and improved behaviour for
differential-algebraic problems.
    In contrast to these fully-implicit methods, (5.6 c) has order two, but the imple-
mentation cost is only twice that of the implicit Euler method. Like (5.6 a), it is
A-stable. The method (5.6 d), while having a full A matrix, has comparable imple-
mentation cost to (5.6 d) because σ (A) contains only a single eigenvalue. That is, it
is a “singly-implicit” method.


Competing attributes of implicit methods

The order, the stability and the implementability — meaning the existence of a
low-cost implementation algorithm — are all attributes that make an implicit method
capable of yielding results for a stiff problem efﬁciently and accurately. In this survey,
some additional attributes are also considered.
5.6 Implicit Runge–Kutta methods                                                       203

Gauss methods
Let Pn , n = 0, 1, 2, . . . denote the shifted Legendre polynomials, orthogonal on [0, 1],
and normalized by Pn (1) = 1. They satisfy the recursion

                   P0 (x) = 1,
                   P1 (x) = 2x − 1,
                 nPn (x) = (2n − 1)(2x − 1)Pn−1 (x) − (n − 1)Pn−2 (x).

The zeros of Pn are real and distinct and lie in (0, 1). The ﬁrst few polynomials and
their zeros are equal to
                                                  
       P0 (x) = 1,                        zeros: ,
                                                   
       P1 (x) = 2x − 1,                   zeros: 12 ,
                                                          √            √ 
       P2 (x) = 6x2 − 6x + 1,             zeros: 12 − 16 3, 12 + 16 3 ,
                                                           √                  √ 
       P3 (x) = 20x3 − 30x2 + 12x − 1, zeros: 12 − 10     1
                                                              15, 12 , 12 + 10
                                                                            1
                                                                                15 .

To construct the Gauss method of order 2s, choose c, b and A as follows
1. Choose the components of c as the zeros of Ps .
2. Choose bT as the solution to the linear system bT ck−1 = 1/k, k = 1, 2, . . . , s.
3. Choose A as the solution to the linear system AT ck−1 = ck /k, k = 1, 2, . . . , s.

Exercise 50 Find the tableau for the Gauss method with s = 3.



Radau IIA methods
These methods are usually preferred to Gauss methods for the solution of stiff
problems and differential-algebraic equations. The components of c are the zeros
of Ps − Ps−1 . Also bT and A are the solutions of bT ck−1 = 1/k, k = 1, 2, . . . , s and
AT ck−1 = ck /k, k = 1, 2, . . . , s.

Exercise 51 Show that 1 is a component of c for a Radau IIA method.


Exercise 52 Find the tableau for the Radau IIA method with s = 3.




Implementability

We will discuss the valuation of the solution of the implicit equations in a single step,
using a simpliﬁed form of Newton’s method in which the Jacobian matrices f  (Yi ),
i = 1, 2, . . . , s, are approximated by a single matrix J. Each iteration takes the form
Y → Y − D, where
204                                                           5 B-series and Runge–Kutta methods
                         ⎡  ⎤                   ⎡             ⎤             ⎡
                                                                            ⎤
                         Y1                         f (Y1 )              D1
                       ⎢    ⎥             ⎢         ⎥                  ⎢    ⎥
                       ⎢ Y2 ⎥             ⎢ f (Y2 ) ⎥                  ⎢ D2 ⎥
                       ⎢    ⎥             ⎢         ⎥                  ⎢    ⎥
                   Y = ⎢ . ⎥,          F =⎢         ⎥,             D = ⎢ . ⎥,
                       ⎢ . ⎥              ⎢    ..   ⎥                  ⎢ . ⎥
                       ⎣ . ⎦              ⎣     .   ⎦                  ⎣ . ⎦
                         Ys                 f (Ys )                      Ds
               Φ(Y ) = Y − h(A ⊗ IN )F,
              Φ  (Y ) = IsN − h(A ⊗ J),
                   D = Φ  (Y )−1 Φ(Y ).

Write V = Φ(Y ), M = Φ  (Y ), and consider the solution of MD = V , where we see
that               ⎡                                              ⎤
                      IN − a11 J      −a12 J     ···    −a1s J
                   ⎢                                              ⎥
                   ⎢ −a21 J         IN − a22 J · · ·    −a2s J ⎥
                   ⎢                                              ⎥
               M=⎢           ..          ..                ..     ⎥.
                   ⎢                                              ⎥
                   ⎣          .           .                 .     ⎦
                         −as1 J       −as2 J     · · · IN − ass J
The cost of carrying out an LU factorization, in preparation for a sequence of
iterations in a single step, or in a run of steps for slowly varying values of f  (Yi ), is
const s3 N 3 , where const is a small constant. However, if A is lower triangular, with
constant diagonal λ , it is only necessary to prepare, by LU factorization, the single
N × N matrix IN − hλ J.


Transformations and practical implementable methods

Let T be a non-singular s × s matrix and deﬁne transformed vectors Y = TY , F = T F,
V = TV and D = T D. Also write A = TAT −1 , M = T MT −1 . The iterations can now
be carried out using the solution of MD = V . It was proposed [5] (Burrage, 1978) to
use “singly-implicit” methods, for which σ (A) = {λ }; with T chosen so that A is
lower triangular, the preparation costs reduce to that of IN − hλ J.
   To obtain singly-implicit methods with stage-order s we must satisfy the two
conditions

                             (A − λ I)s = 0,                                             (5.6 e)
                                Ack−1 = 1k ck ,      k = 1, 2, . . . , s.                (5.6 f)

From (5.6 f), Ak 1 = (1/k!)ck , and from (5.6 e) we then have

                                       s    
                                             s
                      (A − λ I)s 1 = ∑         (−λ )s−i Ai 1                             (5.6 g)
                                      i=0    i
                                                     s!
                                    = (−λ )s                  (−λ −1 c)i .               (5.6 h)
                                                (s − i)!(i!)2
5.7 Effective order methods                                                             205

The Laguerre polynomials are given by
                                              n
                                                      n!
                                Ln (x) = ∑                     (−x)i ,               (5.6 i)
                                             i=0 (n − i)!(i!)2


and it follows that                                ⎡         ⎤
                                                        ξ1
                                             ⎢    ⎥
                                             ⎢ ξ2 ⎥
                                             ⎢    ⎥
                                          c=λ⎢    ⎥
                                             ⎢ .. ⎥ ,                                (5.6 j)
                                             ⎢ . ⎥
                                             ⎣    ⎦
                                               ξs

where ξ1 , ξ2 , . . . , ξs are the zeros of Ls .



5.7 Effective order methods

In [13] it was pointed out that the ﬁfth-order barrier can be overcome by weakening
the order conditions slightly to obtain methods which are conjugate to a ﬁfth order
method but have only 5 stages. The methods can be used efﬁciently, at least with
constant stepsize, by carrying out pre- and post- processing steps at the beginning
and end of a sequence of integration steps.
   If the main steps correspond to a mapping A h and the perturbation step, used as
the pre-processor, corresponds to the mapping B h then “effective order p” means
that B h −1 ◦ A h ◦ B h has order p. If A h corresponds to Bh a and B h to Bh b then we
replace the usual order conditions by
                                       bab−1 = E + O p+1 ,
or, what is equivalent,
                                         ba = Eb + O p+1 .                           (5.7 a)
    The diagram on the left of (5.7 b) illustrates the relationship between A h , B h and
E h . The diagram on the right represents the corresponding relationships between
B-series.

                                                        x)
                                             (B h ◦ y)(                      a
                                Ah                     Bh
                                        Bh                                       b   (5.7 b)
            Bh         Bh                                            b
                               Eh
                                          y(x)
                                                                         E
In this section we will start by ﬁnding explicit conditions on the elementary weights
required to satisfy (5.7 a), in terms of the free parameters made available from the
elementary weights for b.
206                                                          5 B-series and Runge–Kutta methods

Complexity of effective order conditions

Using the notation introduced in Section 2.4, write an for the number of trees with
order n. This means that, for methods with classical order p, the number of order
conditions is
                                a1 + a2 + · · · + a p ,
but for effective order this is replaced by 1 + a p . This is shown in [26]. Although
there are a1 + a2 + · · · + a p parameters from b, these are not all free parameters. The
equations in (5.7 a) do not involve b(t) when |t| = p because these terms cancel
from the sides of (ba)(t) = (Eb)(t). Furthermore, there is no loss in generality in
assuming that b(τ) = 0 because, for any parameter θ , b can be replaced by E (θ ) b,
because, if (5.7 a) holds, then
                 (E(θ ) b)a = E(θ ) ba = E(θ ) Eb = E(θ +1) b = E(E(θ ) b).
We will focus on the case p = 5. For simplicity we will consider methods in the
intersection of C(2) and D(1).



Methods satisfying D(1) and C(2)

For the special methods in D(1), we can substitute τ ∗ t = τ t − t ∗ τ as a step towards
writing a(τ ∗ t) = a(τ)a(t) − a(t ∗ τ), for |t| ≤ 5. In the case of C(2), [τ] = 12 τ 2 and
for certain pairs (t, t  ), we have t  = 12 t. These assumptions are shown together in the
following tabulation

                   C(2)                     D(1)
             t2 = 12 t21 ,            t2 = 12 t,1
             t4 = 12 t3 ,             t4 = t1 t2 − t3 = 12 t31 − t3 ,
             t6 = 12 t5 ,
                                      t7 = t1 t3 − t5 ,
             t8 = 12 t7 ,             t8 = t1 t4 − t6 = 12 t41 − t1 t3 − t6 ,
            t10 = 12 t9 ,
            t12 = 12 t11 ,
            t13 = 12 t10 = 14 t9 ,
                                     t14 = t1 t5 − t9 ,
            t15 = 12 t14 ,           t15 = t1 t6 − t10 ,
                                     t16 = t1 t7 − t11 = t21 t3 − t1 t5 − t11 ,
            t17 = 12 t16 ,           t17 = t1 t8 − t12 = 12 t51 − t21 t3 − t1 t6 − t12 .
5.7 Effective order methods                                                                         207

By comparing the two lists, we see that t3 = 13 t31 and t4 = 16 t31 . From the combination
of these conditions, the set of trees necessary to analyse the effective order conditions
reduces to {t1 , t5 , t9 , t11 }.
   To facilitate evaluations of group products calculate the bi-product in Sweedler
notation
        Δ (t1 ) = t1 ⊗ ∅ + 1 ⊗ t1 ,
        Δ (t5 ) = t5 ⊗ ∅ + t31 ⊗ t1 + 3t21 ⊗ t2 + t1 ⊗ t31 + 1 ⊗ t5
               = t5 ⊗ ∅ + t31 ⊗ t1 + 23 t21 ⊗ t21 + t1 ⊗ t31 + 1 ⊗ t5 ,
        Δ (t9 ) = t9 ⊗ ∅ + t41 ⊗ t1 + 4t31 ⊗ t2 + 6t21 ⊗ t3 + 4t1 ⊗ t5 + 1 ⊗ t9
               = t9 ⊗ ∅ + t41 ⊗ t1 + 2t31 ⊗ t21 + 2t21 ⊗ t31 + 4t1 ⊗ t5 + 1 ⊗ t9 ,
       Δ (t11 ) = t11 ⊗ ∅ + t1 t3 ⊗ t1 + (t31 + t3 ) ⊗ t2 + t21 ⊗ t3
                              + 2t21 ⊗ t4 + 2t1 ⊗ t6 + t1 ⊗ t7 + 1 ⊗ t11
               = t11 ⊗ ∅ + 31 t41 ⊗ t1 + 23 t31 ⊗ t21 + 23 t21 ⊗ t31 + 14 t1 ⊗ t41 + 1 ⊗ t11 .
To ﬁnd the formula for (ba)(t), replace each term in (f ⊗ f ) in Δ (t) by b(f)a(f  ) and
similarly for E(f)b(f ). As has been remarked, we will, without loss of generality,
assume that b(t) = 0 for t = τ and whenever |t| = 5. The results are
            (ba)(t1 ) = a(t1 ),                      (Eb)(t1 ) = 1,
            (ba)(t5 ) = b(t5 ) + a(t5 ),             (Eb)(t3 ) = 14 + b(t5 ),
            (ba)(t9 ) = b(t9 ) + a(t9 ),             (Eb)(t9 ) = 15 + 4b(t5 ) + b(t9 ),
           (ba)(t11 ) = a(t11 ),                                 1
                                                    (Eb)(t11 ) = 15 .
These imply the usual order conditions
                                       for t = t1 , t5 , t11 , whereas the t9 equation can
be satisﬁed by choosing b(t5 ) = 14 a(t9 ) − 15 . With this proviso, we can construct a
method
                        0
                        c2          c2
                        c3      c3 − a32       a32                  ,
                        1 1 − a42 − a43 a42 a43
                                          b1                 0   b3     b4
satisfying order as well as the C(2) and D(1) conditions

                                   ∑ j ai j c j = 12 c2i ,            i = 3, 4,
                                    ∑i bi ai j = b j (1 − c j ),      j = 1, 2, 3, 4,
                       b1 + b3 + b4 + b5 = 1,                                                    (5.7 c)
                      3 + b4 c4 + b5 = k ,
                  b3 ck−1     k−1      1
                                                                      k = 2, 3, 4,
                                                   1
                                ∑ bi ci ai j c2j = 15 .
208                                                           5 B-series and Runge–Kutta methods

Exercise 53 Show that, for a solution of (5.7 c), c3 = 25 .

   An example solution of (5.7 c) is given by the tableau

                                   0
                                   2     2
                                   5     5
                                   2     1  1
                                   5     5  5
                                   1     3          5              .                     (5.7 d)
                                   2     16 0       16

                                         4 −4
                                         1  5
                                   1                0     2
                                         1                2    1
                                         6  0       0     3    6




Summary of Chapter 5 and the way forward
Summary

The early Runge–Kutta methods were built around the aim of obtaining successively
higher orders for a generic scalar problem. However, in modern computing there is
no interest in numerical methods which are applicable only to scalar problems and,
above order 4, an analysis based on B-series is more appropriate. Even for order 5
there exist scalar methods with reduced order when applied to non-scalar problems.
    Explicit methods to order 5 are derived. Order barriers are introduced through
the simplest case (that order p = s is impossible for explicit methods with p > 4).
It is shown that this barrier can be circumvented through the use of effective order
(conjugate order). Implicit methods, intended for the solution of stiff problems, are
analysed and derived. Effective order is introduced and a new method of effective
order 5 is derived.

The way forward

Not discussed in this chapter are Runge–Kutta methods with error estimators and
methods of Nystöm type. In Chapter 6, Runge–Kutta methods are used in the con-
struction of starting methods for general linear methods. In Chapter 7 symplectic
methods are introduced for the solution of Hamiltonian problems.

Teaching and study notes

The following items are suggested reading associated with Runge–Kutta methods.
Butcher, J.C. Coefﬁcients for the study of Runge–Kutta integration processes (1963)
[7]
Butcher, J.C. A stability property of implicit Runge–Kutta methods (1975) [15]
Butcher, J.C. The Numerical Analysis of Ordinary Differential Equations, Runge–
Kutta and General Linear Methods (1987) [17]
5.7 Effective order methods                                                                   209

Cooper, G.J. and Verner, J.H. Some explicit Runge-Kutta methods of high order
(1972) [33]
Gill, S. A process for the step-by-step integration of differential equations in an
automatic computing machine (1951) [45]
Hairer, E. A Runge–Kutta method of order 10 (1978) [47]
Hammer, P.C. and Hollingsworth, J.W. Trapezoidal methods of approximating
solutions of differential equations (1955) [54]
Merson, R.H. An operational method for the study of integration processes (1957)
[72]
Nyström, E.J. Über die numerische Integration von Differentialgleichungen (1925)
[77]
Prince, P.J. and Dormand, J.R. High order embedded Runge–Kutta formulae
(1981) [78]
Verner, J.H. Some Runge–Kutta formula pairs (1991) [91]



Projects
Project 14 The stability function of a Runge–Kuta method with s = 6, p = 5 has the form
1 + z + z2 /2 + z3 /6 + z4 /24 + z5 /120 +Cz6 , where C depends on free parameters in the method
tableau. Explore the dependence of the stability region on C.
Project 15 Show that a Runge–Kutta method with s = 6, and order 5 for scalar problems, also
has the same order for high-dimensional problems if D(1) holds.
Project 16   Explore the family of Runge–Kutta methods with s = 7, p = 6.
Project 17   Explore the family of Runge–Kutta methods with s = 11, p = 8.
Project 18 Study the theory of Order Stars and its application to the relationship between
A-stability and order of Runge–Kutta methods.
Project 19   Study the theory of Order Arrows.
Project 20   Learn about Runge–Kutta pairs such as those in [78] and [91].
Project 21   Learn about the Radau family of numerical codes developed by Ernst Hairer.
