# B-Series: Algebraic Analysis of Numerical Methods - Part 7

**Author:** John C. Butcher

**Series:** Springer Series in Computational Mathematics, Volume 55

---

Chapter 6
B-series and multivalue methods




6.1 Introduction

Multivalue and multistage methods

The history of multivalue methods parallels the history of Runge–Kutta methods.
Runge–Kutta methods achieved high accuracy through the multistage approach,
whereas multivalue methods obtained improvements by re-using computed informa-
tion in two or more steps.
   The ﬁrst notable publication on multistep methods was motivated by the need
for numerical results for a speciﬁc problem [3] (Bashforth and Adams,1883) and
the Adams–Bashforth method was introduced. When Adams–Moulton methods
[74] (Moulton, 1926) became available, predictor-corrector methods increased in
popularity and have become a dominant technique in practical computation.
   So-called “stiff” problems, such as those arising from the space-discretisation of
partial differential equations, brought with them special difﬁculties which rendered
Adams methods impractical and inefﬁcient for many problems. The backward dif-
ference methods introduced in [35] (Curtiss and Hirschfelder, 1952) were a timely
response to stiffness.
   Even though multistep and Runge–Kutta methods developed individually and
separately, they have always had a common core. That is, they are each built up
from two basic operations and nothing more: the evaluation of the function f and the
calculation of linear combinations of existing vectors.


Generalizations of traditional methods

Several innovations in the 1960s illustrated that methods could exist with aspects of
both classical families, although they belonged to neither of them. These new ideas
included the possible use of off-step predictors as a modiﬁcation to the standard
linear multistep algorithms [46] (Gragg and Stetter, 1964), [43] (Gear, 1965), [10]

© Springer Nature Switzerland AG 2021                                            211
J. C. Butcher, B-Series, Springer Series in Computational Mathematics 55,
https://doi.org/10.1007/978-3-030-70956-3_6
212                                                          6 B-series and multivalue methods

(Butcher, 1965). A second break from the strict linear multistep type of methods was
the introduction of cyclic composite methods [41] (Donelson and Hansen,1971).
   Modiﬁcations to Runge–Kutta are also possible, such as cyclic composite forms of
these methods and pseudo Runge–Kutta methods [27]. (Byrne and Lambert, 1966).
   Methods in which stage derivatives in one step are approximated by stage deriva-
tives already evaluated in a previous step will be referred to as “re-use” methods An
example of this is given in (6.3 h) (p. 219).


Schematic diagram of classes of numerical methods

As we have seen, multivalue and multistage methods are generalizations of the Euler
method. A diagram showing the various types of methods that can be built on these
ideas is given in (6.1 a):
                                          General linear



                       Linear multistep                    Runge–Kutta                 (6.1 a)


                                              Euler


In this diagram,    symbolizes the use of a multivalue method, rather than a one
value method, whereas      symbolizes a multistage rather than a one stage method.
General linear methods at the top of the diagram are both multivalue and multistage.


General linear methods

In [12] (Butcher, 1966), the methods, now called General Linear Methods, were
introduced with the intention that the multivalue and multistage aspects of the
method should be equally balanced. These methods, using the formulation based on
[6] (Burrage and Butcher, 1980), are the principal subjects of the present chapter.
   In traditional and closely related methods, the quantities being approximated have
a natural meaning. However, for a method written in terms of coefﬁcient matrices,
this will not always be the case. Hence, a completely fresh approach to the meaning
of order is required, which will be theoretically sound and, at the same time, is
practical.


Chapter outline

Section 6.2, a broad survey of linear multistep methods, is followed by Section 6.3
which attempts to motivate the need for the class of general linear methods. The
formulation of these methods is presented in Section 6.4. In Section 6.5, the meaning
6.2 Survey of linear multistep methods                                                 213

of order, based on the use of a starting method, to be used together with the main
method, is introducd.
   In Section 6.6, a general approach is discussed for determining the order of a
method in terms of the B-series of the “underlying one-step method”. See [87]
(Stoffer, 1993). Denote this B-series by (Bh y0 )a. To complete the process of ﬁnding
the order, an algorithm is constructed for ﬁnding the maximum p such that a ∼
E + O p+1 .



6.2 Survey of linear multistep methods

It is characteristic of multivalue methods that some preliminary work has to be
carried out before the method can be used in its own right. In the case of a linear
k-step method, k − 1 steps need to be performed by some other numerical method
before the information is available to allow the method to be used in subsequent steps
After these k − 1 steps have been evaluated, approximations to the solution and the
scaled derivatives at xi = x0 + hi, i = 0, 1, . . . , k − 1 are available. This will enable
step number k and subsequent steps, to be computed using the formula

                                    k              k
                             yn = ∑ ai yn−i + ∑ bi h f (yn−i ),                     (6.2 a)
                                   i=1            i=0

where ai , i = 1, 2, . . . , k and bi , i = 0, 1, . . . , k are real constants.
    Note that if b0 = 0, the method is implicit so that yn and h f (yn ) need to be
evaluated together using an iterative process. Following the practice introduced in
[36] (Dahlquist, 1956), a linear multistep method (6.2 a) is characterized, not directly
in terms of the ai and bi , but in terms of two polynomials:

                           ρ(w) = wk − a1 wk−1 − · · · − ak ,                      (6.2 b)
                           σ (w) = b0 w + b1 w
                                           k        k−1
                                                          + · · · + bk .            (6.2 c)

It is customary to refer to the method given by (6.2 a) as the method (ρ, σ ).
   Basic deﬁnitions are

 Deﬁnition 6.2A The method (ρ, σ ) is consistent if

                                         ρ(1) = 0,                                (6.2 d)
                                         ρ  (1) = σ (1).                         (6.2 e)
214                                                              6 B-series and multivalue methods



 Deﬁnition 6.2B The method (ρ, σ ) is stable if the difference equation

                                 k
                          un = ∑ ai un−i ,           n = k, k + 1, . . . ,
                                i=1

 has bounded solutions for all possible choices of the initial values u0 , u1 , . . . , uk−1 .


   The aim of these deﬁnitions is to characterize what it means for a method to
be convergent. That is, if the values of y1 , . . . , yk−1 are determined by appropriate
starting values, the value of yn computed by the method, using stepsize H/n, the
ﬁnal approximation should converge to y(x0 + H) as n → ∞. Informally, appropriate
starting values S i h , i = 1, 2, . . . , k − 1, means approximations to y(x0 + ih) as h →
0. The deﬁnition of convergence is given in [36] (Dahlquist, 1956) and in other
references, such as [55] (Henrici, 1962), [43] (Gear, 1965), [50] (Hairer, Nørsett and
Wanner, 1993), [20] (Butcher, 2016), as is the theorem relating this concept to the
consistency and stability properties given in Deﬁnitions 6.2A and 6.2B. The ﬁnal
outcome is that consistency and stability are together necessary and sufﬁcient for
convergence.


Comments on notation

In this presentation, (6.2 a, 6.2 b, 6.2 c) correspond respectively to

                                k               k
                               ∑ αi yn+i = ∑ βi h f (yn+i ),
                               i=0             i=0
                                                k
                                      ρ(ζ ) = ∑ αi ζ i ,
                                               i=0
                                                k
                                      σ (ζ ) = ∑ βi ζ i .
                                               i=0

in [36]. However, the correspondence is exact only when the coefﬁcients are scaled
so that αk+1 = 1. In Dahlquist’s classic work, there is no such restriction.


Order conditions


Consider a single step of the method (ρ, σ ), starting from the exact initial value

                                k                     k
                         yk = ∑ ai y(xk−i ) + h ∑ bi f (y(xk−i )).                         (6.2 f)
                               i=1                   i=0
6.2 Survey of linear multistep methods                                                215

The error committed in this single step is y(xk ) − yk , which becomes, written in
B-series,
                                k                      k
               (Bh y0 )(Ek ) − ∑ ai (Bh y0 )(Ek−i ) − ∑ bi (Bh y0 )(Ek−i D).
                               i=1                    i=0

For order p, this must give an expansion for which the coefﬁcients of F(t) is zero for
all trees satisfying |t| ≤ p. That is

                                 k           k
                          Ek − ∑ ai Ek−i − ∑ bi Ek−i D = O p+1
                               i=1          i=0

or, what is equivalent,

          1 = a1 E−1 + a2 E−2 + · · · + ak E−k
                                                                                   (6.2 g)
                 + b0 D + b1 E−1 D + b2 E−2 D + · · · + bk E−k D + O p+1 .




Stability regions


We will consider the behaviour of a linear multistep method (ρ, σ ), in attempting to
solve the linear problem y = qy, where q is a complex scalar constant. The analysis
is also applicable to y = Qy, where the N × N constant matrix Q is diagonalizable.
A sequence of k step values satisﬁes the relation

     (1 − hqb0 )yn = a1 yn−1 + · · · + ak yn−k + hq(b1 yn−1 + · · · + bk yn−k ),   (6.2 h)

so that the sequence satisﬁes the difference equation with characteristic polynomial
ρ(w) − zσ (w), where z = hq ∈ C. A complex number z is a “stable point” if solutions
to this difference equation are bounded; the set of all stable points is the “stability
region”.


A-stability

Following [37] (Dahlquist, 1963), we deﬁne


 Deﬁnition 6.2C A method (ρ, σ ) is A-stable if the open left-half complex plane
 C− is a subset of the stability region.
216                                                              6 B-series and multivalue methods

Signiﬁcance of A-stability

If Re q < 0 then the exact solution behaves like exp(qx). It makes sense to model
problems with decaying solutions by numerical approximations which are at least
bounded with increasing time. While it might be difﬁcult to guarantee this in general,
we can at least achieve it for the case of linear problems.


One-leg methods

In [38] (Dahlquist, 1976), the idea of “one-leg methods” was introduced. For these
methods, the terms in (6.2 f)

                                          k
                                        h ∑ bi f (y(xk−i ))
                                         i=0

are replaced by
                                k             
                                                     bi
                              h   ∑ bi f            k
                                                           y(xk−i )   .
                                  i=0              ∑i=0 bi
  The order conditions for the linear multistep method (6.2 g) become, for the
one-leg method,

         1 = a1 E−1 + a2 E−2 + · · · + ak E−k
                   k  b + b E−1 + b E−2 + · · · + b E−k
                             0       1         2                k
                + ∑ bi                                            D + O p+1 .
                   i=0               b 0 + b1 + b2 + · · · + bk



Exercise 54 Show that the stability regions of a linear multistep method and the corresponding
one-leg method are identical.




The Dahlquist barriers


The ﬁrst barrier

Traditional linear multistep methods and predictor-corrector methods are governed
by the Dahlquist barrier [36] (Dahlquist, 1956), quoted here without proof.

 Theorem 6.2D (Dahlquist barrier) The order of a stable k-step method cannot
 exceed k + 2 (k even) or k + 1 (k odd).
6.3 Motivations for general linear methods                                               217

The second Dahlquist barrier

The second barrier is concerned with the attainable order of A-stable k-step methods.
The result is proved using order-stars [53] (Hairer and Wanner, 1996), or alternatively
by order-arrows [20] (Butcher, 2016) and states

 Theorem 6.2E (Second Dahlquist barrier)               The order of an A-stable k-step
 method cannot exceed 2.




6.3 Motivations for general linear methods

There are several reasons why a wider formulation of numerical methods than offered
by either of the traditional linear multistep or one-step schemes is appropriate. First,
the barriers on what is achievable, if the constraints of the traditional methods can be
overcome, and we start by considering some of these.


Breaking the Dahlquist barrier

The consequences of the ﬁrst Dahlquist barrier on the order of linear k-step methods
can be avoided in various ways.


Breaking the barrier using off-step points

In a number of independent contributions [46] (Gragg and Stetter,1964), [43] (Gear,
1965), [10] (Butcher, 1965), a new approach to numerical integration was put forward
in several independent projects. These can be seen as attempts to overcome limitations
inherent in traditional methods by the use of off-step points.
    For example, a two-predictor 2-step method which is stable and has order 5 is
given by (6.3 a)–(6.3 c) below. The ﬁrst predictor (6.3 a) gives an approximation
yn−1/2 to y(xn−1/2 ) from which fn−1/2 := f (y(xn−1/2 )) ≈ y (xn−1/2 ) is found. The
second predictor (6.3 b) gives y(xn ) ≈ y(xn ), leading to fn := f (y(xn )) ≈ y (xn ).
Finally, the corrector formula (6.3 c) gives yn ≈ y(xn )

    yn−1/2 = yn−2 + 98 h fn−1 + 38 h fn−2 ,                                           (6.3 a)

              5 yn−1 − 5 yn−2 − 4h f n−1 − 15 h f n−2 + 15 h f n−1/2 ,
         yn = 28       23                  26           32
                                                                                      (6.3 b)

              31 yn−1 − 31 yn−2 + 31 h f n + 31 h f n−1 − 93 h f n−2 + 93 h f n−1/2
         yn = 32        1         5          4            1            64
                                                                                      (6.3 c)
218                                                                6 B-series and multivalue methods

Breaking the barrier using cyclic composite methods

Cyclic composite methods were introduced in [41] (Donelson and Hansen,1971).
Consider the family of ﬁfth order 3-step methods

         33yn + (24 + 57λ )yn−1 − (57 + 24λ )yn−2 − 33λ yn−3
                = (10 − λ )h f (yn ) + (57 + 24λ )h f (yn−1 )                                  (6.3 d)
                          + (24 + 57λ )h f (yn−2 ) − (1 − 10λ )h f (yn−3 ).

According to Theorem 6.2D, this method is unstable for any choice of λ . However,
it can be used in a stable manner by alternating the value of λ between steps. For
example λ = 0 could be used in even numbered steps and λ = − 361
                                                               240 in odd numbered
steps.
   The composite method can now be written,

      y2n = − 11
              8
                 y2n−1 + 19
                         11 y2n−2

                 33 h f (y2n ) + 11 h f (y2n−1 ) + 11 h f (y2n−2 ) − 33 h f (y2n−3 ),
               + 10              19                8                 1
                                                                                                (6.3 e)
           240 y2n + 30 y2n−1 − 240 y2n−2
   y2n+1 = 449       19         361


                 720 h f (y2n+1 ) + 30 h f (y2n ) − 240 h f (y2n−1 ) − 72 h f (y2n−2 ).
               + 251                19              449                35


The stability of each of the two methods can be characterized by the companion
matrices of their ρ polynomials; that is, the pair of matrices
                     ⎡                    ⎤            ⎡                           ⎤
                      −8         19
                                      0                    449
                                                                        30 − 240
                                                                        19   361
                     ⎢ 11        11     ⎥              ⎢   240                   ⎥
                M1 = ⎢
                     ⎣ 1         0    0 ⎥
                                        ⎦,        M2 = ⎢
                                                       ⎣       1        0      0 ⎥
                                                                                 ⎦.             (6.3 f)
                       0         1    0                        0        1      0

Neither M1 nor M2 is power-bounded, a criterion equivalent to Deﬁnition 6.2B.
However, for the cyclic method, stability is determined by the product
                                    ⎡                      ⎤
                                     −8           19
                                                       0
                                    ⎢ 11          11
                                                         ⎥
                            M2 M1 = ⎢
                                    ⎣− 11
                                       8          19
                                                       0 ⎥
                                                         ⎦ =: M,
                                                  11
                                              1    0   0

                                                                   T
                                                                       − 11 11 0 , for n = 3, 4, . . . .
                                                                         8 19
which is power bounded because M n = M 2 = 1 1 1


Exercise 55 Show that the composite cyclic method based on (6.3 e) is stable if λ = 0 in
even-numbered steps and λ = μ, where μ ∈ (− 241
                                            120 , −1) in odd-numbered steps.
6.3 Motivations for general linear methods                                              219

Breaking the Runge–Kutta order barriers


Although explicit Runge–Kutta methods require only p stages for order p = 1, 2, 3, 4,
it was shown in Theorem 5.5A (p. 200) that order p ≥ 5 requires at least p + 1 stages.
However, generalizations of Runge–Kutta methods are available to alleviate these
restrictions.


Breaking the barrier by re-use of stages

The following tableau, for a 6 stage ﬁfth order method, can be modiﬁed to become a
5 stage method in which stage number 2 is replaced by the value of stage number 4,
evaluated in the previous step.
                            0     0
                          − 12 − 12

                                 16 − 16
                             1    5   1
                             4

                                  4 −4
                             1    3   1
                             2                                     .                (6.3 g)

                             4 − 16
                             3   15    3       3    9
                                       8       4   16

                                  7 −1         0 − 12
                                 18                      8
                            1                      7     7
                                 7             16   2    16   7
                                 90     0      45   15   45   90


Rewrite the remaining stages in step number n, after the second stage is deleted, as
Yin , i = 1, 2, . . . , 5, with a similar notation for the stage derivatives, and the method
becomes

          Y1n = yn−1 ,
          Y2n = yn−1 + 16
                       5
                          hF1n − 16
                                 1
                                    hF3n−1 ,
          Y3n = yn−1 + 34 hF1n − 14 hF3n−1 ,
                                                                                    (6.3 h)
          Y4n = yn−1 − 15   n   3   n−1
                       16 hF1 + 8 hF3   + 34 hF2n + 16
                                                    9
                                                       hF3n ,

                       7 hF1 − hF3
          Y5n = yn−1 + 18            − 12
                                        7 hF3 + 7 hF4 ,
                           n     n−1        n   8   n


                                45 hF2 + + 15 hF3 + 45 hF4 + 90 hF5 .
                         hF1n + 16
                      7              n     2    n   16   n   7    n
          yn = yn−1 + 90

This method is reformulated as a general linear method in (6.4 n) (p. 225).


Breaking the barrier using effective order
Effective order, or conjugate order Section 5.7 (p. 205), is available as a means of
obtaining ﬁfth order accuracy with ﬁve stages, as long as the work expended to carry
out pre- and post-processing is added to the cost.
220                                                      6 B-series and multivalue methods

Breaking the barrier using cyclic composite methods

Even though methods with s = p = 5 do not exist, it is possible to construct methods
with s = 5 which satisfy all except one of the ﬁfth order conditions. The following
two tableaux are examples of this
       0                                         0
       5      5                                  7     7
       8      8                                  8     8
       1      1     1                             7    21     7
       4      5     20                           10    50    25
                                            ,                                       .
       10 − 1250 − 625                                392 − 196
       7    1127   259    252                    1    75    11     45
                          125                    4                 392

       1     737
             175
                    44
                    25   − 32
                           5
                                 10
                                 7               1 − 501
                                                     245 − 245
                                                           268      46
                                                                    49
                                                                          16
                                                                           5
              1            32    250   5               1           250    32   5
              14     0     81    567   54              14     0    567    81   54

If the 17 order conditions up to order 5 are tested, they are satisﬁed in each case,
                                                                                        1
except for the single tree t16 = [[[τ 2 ]]]. For this tree, the condition is bT A2 c2 = 60
but the values of the elementary weight for the two methods are 960 = 60 − 64
                                                                            1       1   1

and 960 = 60 + 64 , respectively. If the two methods are used cyclically, the ± 64 ,
      31     1    1                                                                     1

contributions to the error coefﬁcients cancel out and ﬁfth order is achieved after every
pair of steps.



A common basis for one-step and multistep methods


The most important motivation for introducing general linear methods is that it is
natural. For all step-by-step methods, some data is received at the beginning of each
step and updated for output and subsequent use by the following step. The updating
consists of the calculation of one or more approximations to the solution at points
in or near the step; from these approximations, stage derivatives as samples of the
vector ﬁeld are evaluated and made available for further calculations in the step or
else made available in the updating process.




6.4 Formulation of general linear methods


Following the formulation in [6] (Burrage and Butcher, 1980), we denote the data
input to step number n by y[n−1] and the data output at the completion of the step by
y[n] . Each of these is a vector in (RN )r and is decomposed into individual components
in the form
6.4 Formulation of general linear methods                                                           221
                                    ⎡                ⎤                     ⎡            ⎤
                                         [n−1]                                    [n]
                                        y1                                      y1
                               ⎢       ⎥                              ⎢     ⎥
                               ⎢ [n−1] ⎥                              ⎢ [n] ⎥
                               ⎢ y     ⎥                              ⎢ y ⎥
                               ⎢ 2     ⎥                              ⎢ 2 ⎥
                      y[n−1] = ⎢       ⎥,                      y[n] = ⎢     ⎥.                   (6.4 a)
                               ⎢   ..  ⎥                              ⎢ .. ⎥
                               ⎢    .  ⎥                              ⎢ . ⎥
                               ⎣       ⎦                              ⎣     ⎦
                                         [n−1]                                    [n]
                                        yr                                      yr
During the computation s stages are evaluated and for each of these stages the stage
derivative needs to be evaluated. These are written as vectors in (RN )s with the
notation             ⎡      ⎤            ⎡     ⎤ ⎡               ⎤
                        Y1                  F1           f (Y1 )
                     ⎢      ⎥            ⎢     ⎥ ⎢               ⎥
                     ⎢ Y2 ⎥              ⎢ F2 ⎥ ⎢        f (Y2 ) ⎥
                     ⎢      ⎥            ⎢     ⎥ ⎢               ⎥
                Y = ⎢ . ⎥,          F = ⎢ . ⎥ := ⎢               ⎥.          (6.4 b)
                     ⎢ . ⎥               ⎢ . ⎥ ⎢     ⎢
                                                            ..   ⎥
                                                                 ⎥
                     ⎣ . ⎦               ⎣ . ⎦ ⎣             .   ⎦
                            Ys                            Fs                     f (Ys )
To express the relation between these quantities, introduce a coefﬁcient matrix
partitioned as (s + r) × (s + r):
                       ⎡                                          ⎤
                          a11 a12 · · · a1s u11 u12 · · · u1r
                       ⎢                                          ⎥
                       ⎢ a21 a22 · · · a2s u21 u22 · · · u2r ⎥
                       ⎢                                          ⎥
                       ⎢ .                                    .. ⎥
                       ⎢ .        ..     ..   ..    ..            ⎥
                       ⎢ .         .      .    .     .         . ⎥
                 ⎢                                              ⎥
                       ⎢ a                                        ⎥
         A U           ⎢ s1 as2 · · · ass us1 us2 · · · usr ⎥
                   := ⎢                                           ⎥.     (6.4 c)
         B V           ⎢ b11 b12 · · · b1s v11 v12 · · · v1r ⎥
                       ⎢                                          ⎥
                       ⎢                                          ⎥
                       ⎢ b21 b22 · · · b2s v21 v22 · · · v2r ⎥
                       ⎢                                          ⎥
                       ⎢ .         ..     ..   ..    ..        .. ⎥
                       ⎢ ..                                     . ⎥
                       ⎣            .      .    .     .           ⎦
                          br1 br2 · · · brs vr1 vr2 · · · vrr

The evaluation of the result consists of evaluating the stages, together with the stage
derivatives,
                           s                 r
                                                         [n−1]
                    Yi = ∑ hai j Fj + ∑ ui j y j                 ,       i = 1, 2, . . . , s,    (6.4 d)
                           j=1               j=1

followed by the evaluation of the output values
                               s                 r
                     [n]                                 [n−1]
                    yi = ∑ hbi j Fj + ∑ vi j y j                     ,    i = 1, 2, . . . , r.   (6.4 e)
                           j=1               j=1

Written more compactly, (6.4 d) and (6.4 e) become

                                   Y = h(A ⊗ I)F + (U ⊗ I)y[n−1] ,                               (6.4 f)
                            y[n] = h(B ⊗ I)F + (V ⊗ I)y[n−1]                                     (6.4 g)
222                                                           6 B-series and multivalue methods

or                                                                    
                       Y                  A⊗I      U ⊗I             F
                                  =                                            .       (6.4 h)
                       y[n]               B⊗I      V ⊗I           y[n−1]
There is usually no confusion if ⊗I is omitted from each element in (6.4 h).


Consistency, stability and convergence

Consistency and pre-consistency
The ﬁrst of the consistency conditions for linear multistep methods, (6.2 d), some-
times known as “pre-consistency”, really means that there is a possibility of following
a constant solution correctly. The full condition including also (6.2 e) enables linear
growth to be modelled. In general linear methods we also need a condition like
covariance to get the consistent behaviour that we need.

 Deﬁnition 6.4A A general linear method (A,U, B,V ) is pre-consistent if there
 exists u ∈ Rr , known as the “pre-consistency vector”, such that

                                  Vu = u,
                              Uu = 1 := [ 1 1 · · · 1 ]T ∈ Rs .




 Deﬁnition 6.4B A general linear method (A,U, B,V ) is consistent if it is pre-
 consistent with pre-consistency vector u, and there exists v ∈ Rr such that

                                          Bu +V v = v + u.



Stability
Because there are many uses of the term “stability”, the concept considered here is
sometimes referred to as “zero-stability” or “stability in the sense of Dahlquist”.

 Deﬁnition 6.4C A general linear method (A,U, B,V ) is stable if there exists a
 constant C such that
                        V n  ≤ C,    n = 1, 2, . . .

For an unstable method, an error due to truncation or round-off, committed in one
step of a computation, can have an impact on the overall computation which grows
without bound. These informal remarks will be made more precise in the discussion
of convergence.
6.4 Formulation of general linear methods                                         223

   In the meantime we can give criteria for V having bounded powers. First we
remark that V satisﬁes Deﬁnition 6.4C if and only if the same is true for V deﬁned
as the Jordan canonical form of V and this is true if and only if each Jordan block J
satisﬁes J n  ≤ C for all n = 1, 2, . . . , for some C.


 Lemma 6.4D For given complex λ and positive integer m, let J be the m × m
 matrix                  ⎡                       ⎤
                           λ 0 0 ··· 0
                         ⎢                       ⎥
                         ⎢ μ λ 0 ··· 0 ⎥
                         ⎢                       ⎥
                         ⎢                       ⎥
                     J=⎢ ⎢  0  μ   λ   · · · 0   ⎥,
                                                 ⎥
                         ⎢ . . . .               ⎥
                         ⎢ .. .. ..      . . ... ⎥
                         ⎣                       ⎦
                            0 0 0 ··· λ
 where μ is arbitrary non-zero and does not appear in the matrix if m = 1. Then J
 has bounded powers if and only if (i) |λ | < 1 or (ii) |λ | = 1 and m = 1.


Proof. If |λ | < 1, choose μ = 1 − |λ | so that J∞ = 1, and hence J n  ≤ 1, in all
cases. The necessity of |λ | ≤ 1 follows from the fact that the (1, 1) element of J n
is λ n , and the necessity of m = 1, when |λ | = 1, follows from the the fact that, if
m ≥ 2, the (2, 1) element of J n is nμλ n−1 .

A consequence of this result is


 Theorem 6.4E A method (A,U, B,V ) is stable if and only if all zeros of the
 minimal polynomial of V lie in the closed unit disc and those on the boundary are
 simple.


Convergence

In the deﬁnition of convergence below, a Lipschitz continuous problem

                            y (x) = f (y(x)),   y(x0 ) = y0 ,                 (6.4 i)

is to be solved on the interval [x0 , x] using a starting method which satisﬁes y[0] =
uy0 + αn using n steps and stepsize h = (x − x0 )/n to give a ﬁnal result y[n] =
uy(x) + βn .


 Deﬁnition 6.4F A pre-consistent method (A,U, B,V ) is convergent if in the solu-
 tion of (6.4 i) with n steps with αn  → 0 as n → ∞, then βn  → 0 as n → ∞.
224                                                                     6 B-series and multivalue methods

Examples of traditional methods

Example of a Runge–Kutta method
The classical Runge–Kutta method

                                   0
                                   1    1
                                   2    2
                                   1
                                   2    0       1
                                                2                   ,
                                   1    0       0       1
                                        1       1       1       1
                                        6       3       3       6

has the representation                  ⎡                                      ⎤
                                            0       0       0       0      1
                                        ⎢ 1                                  ⎥
                                      ⎢           0       0       0      1 ⎥
                                        ⎢ 2                                  ⎥
                           A   U        ⎢                                    ⎥
                                       =⎢
                                        ⎢ 0
                                                    1
                                                    2       0       0      1 ⎥
                                                                             ⎥.
                           B   V        ⎢                                    ⎥
                                        ⎢ 0         0       1       0      1 ⎥
                                        ⎣                                    ⎦
                                            1       1       1       1
                                            6       3       3       6      1

This is not a unique representation of this method. An alternative is to compute,
in step number n, the scaled derivative of the output result and export this as an
additional output. The method now has r = 2 and a starting step, consisting of
evaluating h f (y0 ) to serve as the second input in the following step. The modiﬁed
method now becomes                    ⎡                         ⎤
                                         0 0 0 0 1 12
                                      ⎢                         ⎥
                                      ⎢ 1 0 0 0 1 0 ⎥
                                      ⎢ 2                       ⎥
                                 ⎢                            ⎥
                                      ⎢                         ⎥
                          A U         ⎢ 0 1 0 0 1 0 ⎥
                                   =⎢                           ⎥.             (6.4 j)
                          B V         ⎢ 1 1 1 0 1 1 ⎥
                                      ⎢ 3 3 6                 6 ⎥
                                      ⎢                         ⎥
                                      ⎢ 1 1 1 0 1 1 ⎥
                                      ⎣ 3 3 6                 6 ⎦
                                         0 0 0 1 0 0

Although this method does not have any special advantages, it points the way to
methods for which the output at the end of step n contains approximations to each of
y(xn ), hy (xn ) and 12 h2 y (xn ). A method based on this generalisation is analysed in
Section 6.5 (p. 235).


Examples of linear multistep methods
The Adams–Bashforth method of order 2 is given by
6.4 Formulation of general linear methods                                                                                225

                            yn = yn−1 + 32 h f (yn−1 ) − 21 h f (yn−2 ).                                             (6.4 k)

This has the representation                          ⎡                                     ⎤
                                                         0        1           3
                                                                                  − 12
                                                   ⎢                        2
                                                                                    ⎥
                                                     ⎢                              ⎥
                                A       U            ⎢ 0          1           3
                                                                               − 12 ⎥
                                                    =⎢                        2     ⎥.
                                B       V            ⎢ 1                      0 0 ⎥
                                                     ⎣            0                 ⎦
                                                       0          0           1 0

Similarly, the third order method in the same family is

                               12 h f (yn−1 ) − 3 h f (yn−2 ) + 12 h f (yn−3 ),
                   yn = yn−1 + 23               4                5
                                                                                                                      (6.4 l)

with representation                             ⎡                                                   ⎤
                                                     0    1           23
                                                                              − 43          5
                                             ⎢                        12                   12⎥
                                           ⎢                                               ⎥
                                             ⎢ 0          1           23
                                                                              − 43          5⎥
                            A   U            ⎢                        12                   12⎥
                                            =⎢
                                             ⎢ 1          0           0           0
                                                                                             ⎥
                                                                                           0 ⎥.                     (6.4 m)
                            B   V            ⎢                                               ⎥
                                             ⎢ 0                                           0 ⎥
                                             ⎣            0           1           0          ⎦
                                               0          0           0           1        0




Examples of non-traditional methods

Examples of re-use methods

It is easy to adapt the pattern in (6.4 j) to more complicated methods, such as (6.3 g)
(p. 219), but with one of the stage derivatives re-used in a later step, as in (6.3 h)
(p. 219). The general linear representation becomes
                                    ⎡                                                                       ⎤
                                            0        0       0         0              0         1       0
                            ⎢ 5                                                                     1 ⎥
                            ⎢ 16                     0       0         0              0         1 − 16 ⎥
                            ⎢                                                                          ⎥
                            ⎢ 3                                                                        ⎥
                           ⎢
                                                                                                    1 ⎥
                           ⎢ 4                      0       0         0              0         1 −4 ⎥
                    A   U   ⎢ 15                                                                       ⎥
                           =⎢
                            ⎢− 16
                                                     3
                                                     4
                                                            9
                                                           16          0              0         1 38 ⎥ ⎥.            (6.4 n)
                    B   V   ⎢ 18                                                                       ⎥
                            ⎢                        0   − 12             8
                                                                                      0         1 −1 ⎥
                            ⎢ 7                             7             7                            ⎥
                            ⎢                                                                          ⎥
                            ⎢ 7                     16        2        16              7
                                                                                                1 0 ⎥
                            ⎣ 90                    45       15        45             90               ⎦
                                            0        0       1         0              0         0       0

Another example of a re-use method was given in (1.6 d) (p. 32).                                            For this method,
the second order Runge–Kutta method
226                                                                 6 B-series and multivalue methods

                                        0
                                        1     1
                                        2     2
                                                              ,
                                        1    0       1
                                              1      2   1
                                              6      3   6

applied in step number n, is modiﬁed by adding to the second stage an approxima-
tion to 18 h2 y (xn−1 ), given by 14 hy (xn−2 ) − 34 hy (xn−3/2 ) + 12 hy (xn−1 ), previously
computed in step number n − 1. The purpose of the starting method (1.6 e) (p. 33) is
to provide an approximation to 18 h2 y (x0 ), to use in step number 1.


Example of an off-step points method
The method given by (6.3 a) (p. 217) has a representation as an rs = 43 general linear
method. The input quantities and the stage values will be written as
                                        [n−1]
                                       y1         = yn−1 ,
                                        [n−1]
                                       y2     = yn−2 ,
                                              = hyn−1 ,
                                        [n−1]
                                       y3
                                              = hyn−2 ,
                                        [n−1]
                                       y4
                                            Y1 = yn−1/2 ,
                                            Y2 = yn ,
                                            Y3 = yn .

With this notation the method can be written
                          ⎡                                                           ⎤
                                                                           9      3
                             0     0     0               0          1
                          ⎢                                                8      ⎥
                                                                                  8
                          ⎢ 32                                                    ⎥
                          ⎢ 15     0     0               28
                                                                  − 23   −4 − 26
                                                                               15 ⎥
                          ⎢                              5           5            ⎥
                         ⎢
                       ⎢ 64                                                   1 ⎥
                                    5
                                         0               32
                                                                  − 31
                                                                     1     4
                                                                             −    ⎥
              A U         ⎢ 93     31                    31               31   93 ⎥
                        =⎢⎢ 93                                    − 31         1 ⎥⎥.
                                                                          31 − 93 ⎥
                             64     5                    32          1     4
              B V         ⎢        31    0               31
                          ⎢                                                       ⎥
                          ⎢ 0      0     0               1          0     0    0 ⎥
                          ⎢                                                       ⎥
                          ⎢ 0                                                  0 ⎥
                          ⎣        0     1               0          0     0       ⎦
                             0     0     0               0          0     1    0



Example of a cyclic composite method
The method (6.3 e) (p. 218) carries the approximations over two steps and, hence, it
will be convenient to rescale so that h is replaced by h/2 and renumber in half steps.
It will then be convenient to substitute yn−1/2 from the ﬁrst equation into the second.
6.4 Formulation of general linear methods                                                                      227

     yn−1/2 = − 11
                8
                   yn−1 + 19
                          11 yn−3/2

                                         22 h f (yn−1 ) + 11 h f (yn−3/2 ) − 66 h f (yn−2 ),
                      5
                    + 33 h f (yn−1/2 ) + 19               4                  1


               240 yn−1/2 + 30 yn−1 − 240 yn−3/2
          yn = 449          19        361


                                       60 h f (yn−1/2 ) − 480 h f (yn−1 ) − 144 h f (yn−3/2 )
                      251
                    + 1440 h f (yn ) + 19                 449               35

               = − 11
                   8
                      yn−1 + 19          4753                 251
                             11 yn−3/2 + 7920 h f (yn−1/2 ) + 1440 h f (yn )

                      660 h f (yn−1 ) + 7920 h f (yn−3/2 ) − 15840 h f (yn−2 ).
                    + 449               3463                  449


To recast the method in general linear notation, write
     [n−1]                           [n−1]                      [n−1]
    y1       = yn−1 ,               y2        = yn−3/2 ,       y3        = h f (yn−1 ),
     [n−1]                           [n−1]
    y4       = h f (yn−3/2 ),       y5        = h f (yn−2 ),
      [n]                              [n]
Y1 = y2 = yn−1/2 ,               Y2 = y1 = yn ,                  hF1 = h f (yn−1/2 ),             hF2 = h f (yn ),

and the method becomes
                        [n−1]         [n−1]           [n−1]             [n−1]         [n−1]
     5
Y1 = 33 hF1 − 11
              8
                 y1             + 19
                                  11 y2       + 19
                                                22 y3
                                                                4
                                                              + 11 y4           − 66
                                                                                  1
                                                                                     y5       ,
                            8 [n−1]       [n−1]        [n−1]         [n−1]    449 [n−1]
     7920 hF1 + 1440 hF2 − 11 y1
Y2 = 4753                                                                  − 15840
                251
                                    + 19
                                      11 y2     + 449
                                                  660 y3     + 3463
                                                               7920 y4             y5

or, using coefﬁcient tableaux,
                         ⎡                                                                           ⎤
                                     5
                                                0      − 11
                                                         8       19        19       4
                                                                                          − 66
                                                                                            1
                       ⎢             33                          11        22       11          ⎥
                       ⎢ 4753                                                                   ⎥
                       ⎢ 7920                  251
                                                       − 11
                                                         8       19        449
                                                                                   7920 − 15840
                                                                                   3463    449
                                                                                                ⎥
                       ⎢                       1440              11        660                  ⎥
                     ⎢
                       ⎢ 335
                                                0      − 11
                                                         8       19        19       4
                                                                                         −  1   ⎥
                                                                                                ⎥
               A   U   ⎢                                         11        22       11     66   ⎥
                      =⎢
                       ⎢ 4753                  251
                                                       − 11
                                                         8       19        449     3463
                                                                                        −  449 ⎥⎥.
               B   V   ⎢ 7920                  1440              11        660     7920   15840 ⎥
                       ⎢                                                                        ⎥
                       ⎢ 1                      0        0          0      0        0       0 ⎥
                       ⎢                                                                        ⎥
                       ⎢ 0                                                                  0 ⎥
                       ⎣                        1        0          0      0        0           ⎦
                          0                     0        0          0      1        0       0




Example of an Almost Runge–Kutta method


The following “ARK” method, introduced in [18] (Butcher, 1997), is intended to
re-use past information in a special way, which makes its behaviour very similar to
that of a classical Runge–Kutta method:
228                                                                6 B-series and multivalue methods
                             ⎡                                                           ⎤
                                                                                 1
                                  0      0         0       0       1       1
                         ⎢                                                       ⎥
                                                                                 2
                         ⎢ 1                                                     ⎥
                         ⎢ 16            0         0       0       1    7
                                                                                 ⎥
                                                                                 1
                         ⎢                                              16       ⎥
                                                                                 16
                         ⎢ 1                                                  5 ⎥
                      ⎢− 16            1         0       0       1   − 16 − 16 ⎥
                                                                         7
                         ⎢                                                       ⎥
                A   U    ⎢                                                       ⎥
                       = ⎢ 16            1         1
                                                           0       1      1
                                                                              0  ⎥.
                B   V    ⎢               3         3                      6      ⎥
                         ⎢ 1                                                     ⎥
                         ⎢               1         1
                                                           0       1      1
                                                                              0 ⎥
                         ⎢ 6             3         3                      6      ⎥
                         ⎢                                                       ⎥
                         ⎢ 0             0         0       1       0     0    0 ⎥
                         ⎣                                                       ⎦
                           −1            4
                                         3       − 43      2       0   −1     0



Transformations

Let T denote a non-singular r × r matrix. If the quantities evaluated in step number n
are replaced by independent linear combinations

                     y[n] = (T −1 ⊗ I)y[n] ,            y[n] = (T ⊗ I)
                                                                       y[n] ,

then (6.4 h) transforms to
                      ⎡                                               ⎤               
                 Y           A⊗I                        (UT ) ⊗ I                F
                       =⎣                                               ⎦                    .
                             −1
                           (T B) ⊗ I               (T −1V T ) ⊗ I
                y[n]                                                          y[n−1]

That is, the transformation is
                                                                    
                            A U                     A          UT
                                      →                                    .
                             B V                 T −1 B T −1V T

    In working with a speciﬁc method, it is sometimes convenient to transform it to a
representation that helps with the understanding of the method or makes its analysis
more convenient. For example, a diagonal form of the matrix V might be preferable,
if this is possible.


Transformation of the Adams–Bashforth methods

Using                                  ⎡                       ⎤
                                                          1
                                             1      0
                                    ⎢                     2 ⎥
                                 T =⎢
                                    ⎣ 0             1     0 ⎥
                                                            ⎦,
                                             0      0     1
the method (6.4) transforms to
6.4 Formulation of general linear methods                                                        229
                                                         ⎡                          ⎤
                                                                         3
                                                        0        1              0
                                                    ⎢                  2        ⎥
                                                      ⎢                           ⎥
                               A       UT             ⎢ 0        1       1      0 ⎥
                                                     =⎢                           ⎥.
                            T −1 B T −1V T            ⎢ 1        0       0      0 ⎥
                                                      ⎣                           ⎦
                                                        0        0       1      0

We see in this example that the method has been reduced to one with r = 2 because
the third input is not used in generating the ﬁrst or second output. Hence, the method
can be written                              ⎡             ⎤
                                             0 1 32
                                A U         ⎢             ⎥
                                         =⎢ ⎣  0 1 1 ⎥    ⎦.                     (6.4 o)
                                B V
                                               1 0 0
By carrying out a further transformation, it is possible to diagonalize V :
                                                      ⎡             ⎤
                                                     0 1 12
                  A U             A      UT           ⎢             ⎥
                          →                        =⎢ ⎣  1    1  0  ⎥.
                                                                    ⎦                       (6.4 p)
                  B V           T −1 B T −1V T
                                                         1 0 0



Exercise 56 Find the transformation required to convert the representation (6.4 o) to (6.4 p).


     The order 3 Adams–Bashforth method (6.4 m) transforms, using
                               ⎡                   ⎤
                                  1 0 0 − 12   5
                               ⎢                   ⎥
                               ⎢                   ⎥
                               ⎢ 0 1 0 0 ⎥
                           T =⎢                    ⎥,
                               ⎢ 0 0 1 0 ⎥
                               ⎣                   ⎦
                                  0 0 0 1

to                                                   ⎡                                  ⎤
                                                         0   1       2
                                                                             − 43   0
                                                  ⎢                  3                ⎥
                                                ⎢                                   ⎥
                                                  ⎢ 0        1       2
                                                                         − 11       0 ⎥
                        A           UT            ⎢                  3     12         ⎥
                                                 =⎢
                                                  ⎢ 1        0       0         0
                                                                                      ⎥
                                                                                    0 ⎥.
                      T −1 B       T −1V T        ⎢                                   ⎥
                                                  ⎢ 0        0       1         0    0 ⎥
                                                  ⎣                                   ⎦
                                                    0        0       0         1    0
This simple transformation has converted this method so that r becomes 3, instead of
                                                                     [n−1]
4, because the zeros in the last column indicate that the value of y4     is not needed
in the computation of step number n. Hence, we could represent the method as
230                                                           6 B-series and multivalue methods
                                 ⎡                        ⎤
                                     0    1    2
                                                   − 43
                                 ⎢             3
                                                     ⎥
                                 ⎢                   ⎥
                                 ⎢ 0      1    2
                                                − 11
                                                  12 ⎥
                                 ⎢             3     ⎥.
                                 ⎢                   ⎥
                                 ⎣ 1      0    0 0 ⎦
                                   0      0    1 0

The reduction of r, by the use of a transformation, in the general linear representation
of an Adams method, was considered in [22] (Butcher, Hill, 2006).


Backward differences
In the pre-computer days, the tedious process of solving differential equations by
hand calculations was prone to error, and checks to recognize when something had
gone wrong, were valuable. The well-established practice of compiling difference
tables, to check for errors and to facilitate interpolation, can be incorporated into
Adams–Bashforth methods, by using a modiﬁed formulation. For example, in the
third order case, we could use ﬁrst and second order differences of the fn values
instead of the fn values themselves. That is, (6.4 l) could be rewritten as

             yn = yn−1 + h f (yn−1 ) + 12 h(∇ f )(yn−1 ) + 12
                                                           5
                                                              h(∇2 f )(yn−1 ),

where
                     (∇ f )(yn−1 ) := f (yn−1 ) − f (yn−2 ),
                   (∇2 f )(yn−1 ) := f (yn−1 ) − 2 f (yn−2 ) + f (yn−3 ).

To rewrite the general linear formulation (6.4 m) (p. 225), so that y[n−1] transforms
to a vector with components yn−1 , h f (yn−1 ), h(∇ f )(yn−1 ), h(∇2 f )(yn−1 ), transform
according to
                                                            ⎡                           ⎤
      ⎡                 ⎤                                       0 1 1 12 12          5
                                                            ⎢                           ⎥
                                                        ⎢                           5 ⎥
         1 0 0 0
      ⎢                 ⎥                                  ⎢ 0 1 1 12 12               ⎥
      ⎢ 0 1 0 0 ⎥                      A        UT          ⎢                           ⎥
T =⎢  ⎢ 0 1 −1 0 ⎥
                        ⎥,                               =  ⎢
                                                            ⎢   1   0     0   0      0
                                                                                        ⎥.
                                                                                        ⎥
      ⎣                 ⎦          T −1 B T −1V T           ⎢                           ⎥
                                                            ⎢ 1 0 −1 0 0 ⎥
         0 1 −2 1                                           ⎣                           ⎦
                                                                1 0 −1 −1            0



The Nordsieck representation
For Adams–Bashforth methods in general, and for the third order method in particular,
it was proposed in [76] (Nordsieck, 1962) and [44] (Gear, 1967) to use approxima-
tions to y(xn−1 ) and to the scaled derivatives derivatives hy (xn−1 ), 2!1 h2 y (xn−1 ),
3! h y (xn−1 ), . . . , rather than to the data in the original formulation, as input to step
 1 3 (3)

n. This has the advantage that a change of step-size can be carried out by a simple
6.5 Order of general linear methods                                                     231


               y[0] = S h y(x0 )                Mh                       y[1]
                                                                                error



                 Sh                        h
                                               ◦S h
                                       M                                    Sh
                                                    Eh
                                               S h◦

                y(x0 )                     Eh                   y(x1 )
                         Figure 10 Illustrating local truncation error




rescaling by powers of h. In the general linear methods formulation the change to the
new format is accomplished by the transformation:        ⎡                       ⎤
      ⎡                ⎤                                    0 1 1 1 1
                                                         ⎢                       ⎥
                                                     ⎢                          ⎥
         1 0 0 0
      ⎢                ⎥                                ⎢ 0 1 1 1 1 ⎥
      ⎢ 0 1 0 0 ⎥                    A        UT         ⎢                       ⎥
 T =⎢ ⎢ 0 1 −2 3 ⎥ ,
                       ⎥                              =⎢ ⎢
                                                            1 0 0 0 0 ⎥.
                                                                                 ⎥
                                     −1       −1
      ⎣                ⎦           T B T VT              ⎢ 3                     ⎥
                                                         ⎢ 4 0 − 34 − 12 43 ⎥
         0 1 −4 12                                       ⎣                       ⎦
                                                            1
                                                            6   0  − 1
                                                                     6  − 1
                                                                          3
                                                                              1
                                                                              2


   The idea of combining the features of Runge–Kutta methods with various other
types of numerical integrators is an old one and examples of these “mixed” methods
abound. We refer back to Section 1.6 (p. 28) for a ﬁrst introduction to mixed or
“general linear methods”.



6.5 Order of general linear methods

A general linear method operates, in each step, on r input values and, at the end of the
step, exports the same number of approximations for use as input by the following
step. In many cases the input vectors will have a natural interpration, but we might
need to avoid making such an assumption a priori. Let S h denote the mapping from a
given initial value y0 and the input to the ﬁrst step, y[0] . Also let M h be the mapping
which moves the solution forward through a single step y[0] → y[1] and E h the ﬂow
of the solution through a time step h. That is, E h is the mapping y(x0 ) → y(x0 + h).
The basic idea is based on Figure 10.
   The “error” in this ﬁgure is assumed to be of order p. That is,
                    error := M h (S h (y0 )) − S h (E h (y0 )) = O(h p+1 ).
232                                                      6 B-series and multivalue methods

To convert this ﬁgure to a B-series formulation, introduce η ∈ Bs to represent the
vector of stage values and ηD ∈ (B0 )s to represent the vector of stage derivatives and
ζ ∈ B∗ r to represent the starting method. For order p, these quantities are connected
by
                                 η = AηD +Uζ ,
                               Eζ = BηD +V ζ + O p+1 .



Starting and ﬁnishing methods

In addition to the starting method S h , we introduce a “ﬁnishing method” F h , which
acts as a one sided inverse of S h so that F h ◦ S h = id. The practical role of F h is to
generate approximations to the solution to a given initial value problem after any
desired number of steps. From a theoretical point of view, it gives an alternative
interpretation of truncation error, in which Figure 11 is substituted for Figure 10.


An example of order analysis
As an example of this analysis, consider the method (6.4 n) (p. 225). The conditions
for order 5 are
               η1 = ζ1 ,
               η2 = ζ1 − 16
                         1
                            ζ2 + 16
                                 5
                                    η1 D,
               η3 = ζ1 − 14 ζ2 + 34 η1 D,
               η4 = ζ1 + 38 ζ2 − 15
                                 16 η1 D + 4 η2 D + 16 η3 D,
                                           3        9

               η5 = ζ1 − ζ2 + 18
                              7 η1 D − 7 η3 D + 7 η4 D,
                                       12       8

              Eζ1 = ζ1 + 90
                         7
                            η1 D + 16
                                   45 η2 D + 15 η3 D + 45 η4 D + 90 η5 D,
                                             2         16        7

             Eζ2 = η3 D.
In terms of the tableau matrices, this is
                            ⎡                                           ⎤
                                0      0   0        0     0    1    0
                            ⎢ 5                                    1 ⎥
                            ⎢ 16       0   0        0     0    1 − 16 ⎥
                            ⎢                                         ⎥
                            ⎢ 3                                       ⎥
                         ⎢
                                                                   1 ⎥
                           ⎢ 4        0   0        0     0    1 −4 ⎥
                 A U        ⎢ 15                                      ⎥
                         =⎢ ⎢− 16
                                       3
                                       4
                                            9
                                           16       0     0    1 38 ⎥ ⎥.
                 B V        ⎢ 18                                      ⎥
                            ⎢          0 − 12       8
                                                          0    1 −1 ⎥
                            ⎢ 7             7       7                 ⎥
                            ⎢                                         ⎥
                            ⎢ 7       16    2       16    7
                                                               1 0 ⎥
                            ⎣ 90 45 15              45   90           ⎦
                                 0     0     1     0      0 0 0
The aim will be to obtain order 5 with the trivial starting method for the ﬁrst compo-
nent. That is, ζ1 = 1. We will, as a ﬁrst step, attempt to interconnect the ﬁrst three
stages, with the assumption that
6.5 Order of general linear methods                                                                    233


                   y[0] = S h y(x0 )                  Mh                       y[1]



                      Sh                         h
                                                     ◦S h                      Fh
                                            M

                                                                          error

                     y(x0 )                      Eh                      y(x1 )
               Figure 11 Illustrating an alternative view of local truncation error




                          ζ2 (∅) = 0,
                           ζ2 (ti ) = θi ,   i = 1, 2, . . . , 8.
The values of θi need to satisfy
                                  ζ2 = E−1 η3 D + O5 ,                      (6.5 a)
as we see from the second row of B. It follows also from this row that θ1 = 1. The
calculations will be shown in a tabular fashion.

           ∅
  η1       1 0       0        0      0       0         0           0                      0
  η1 D     0 1       0        0      0       0         0           0                      0
  η2       1   4 − 16 θ2 − 16 θ3 − 16 θ4 − 16 θ5 − 16 θ6
               1   1       1        1       1       1
                                                              − 16
                                                                1
                                                                   θ7                − 16
                                                                                       1
                                                                                          θ8
                                                                                                     (6.5 b)
  η2 D     0   1     1
                     4       16 − 16 θ2
                             1      1
                                              64 − 64 θ2
                                               1    1
                                                              − 16
                                                                1
                                                                   θ3                − 16
                                                                                       1
                                                                                          θ4
  η3       1   2 − 4 θ2 − 4 θ3 − 4 θ4 − 4 θ5 − 4 θ6
               1   1       1        1       1       1
                                                               − 14 θ7                − 14 θ8
  η3 D     0   1     1
                     2
                             1
                             4   − 4 θ2
                                    1          1
                                               8 − 18 θ2       − 14 θ3                − 14 θ4
  E−1η3D 0     1 − 12        1
                             4   − 14 θ2 − 18       8 θ2
                                                    1
                                                            6 + 2 θ 2 − 4 θ3
                                                            1   1       1
                                                                                  12 + 4 θ2 − 4 θ4
                                                                                  1    1      1



From (6.5 a), and the last row of (6.5 b), we ﬁnd that θ2 = − 12 , θ3 = 14 , θ4 = 18 ,
θ5 = − 18 , θ6 = − 16
                   1
                      , θ7 = − 48
                               7
                                  , θ8 = − 96
                                            7
                                              . We can now rewrite (6.5 b) with some
additional data added.
234                                                                        6 B-series and multivalue methods


                                   ∅
               ζ2                  0 1 − 12            1
                                                       4
                                                             1
                                                             8     − 18        − 16
                                                                                 1
                                                                                    − 48
                                                                                       7
                                                                                                 − 96
                                                                                                    7

               η1                  1 0           0     0     0       0           0         0         0
               η1 D                0 1           0     0     0       0           0         0         0
               η2                  1       1
                                           4    32 − 64 − 128
                                                  1    1    1        1
                                                                    128
                                                                                  1
                                                                                256
                                                                                       7
                                                                                      768
                                                                                                   7
                                                                                                 1536
               η2 D                0 1            1
                                                  4
                                                       1
                                                      16
                                                            1
                                                           32
                                                                     1
                                                                    64          128 − 64
                                                                                  1    1
                                                                                                − 128
                                                                                                   1

               η3                  1       1
                                           2      8 − 16 − 32
                                                  1    1    1        1
                                                                    32
                                                                                  1
                                                                                 64
                                                                                       7
                                                                                      192
                                                                                                   7
                                                                                                  384
               η3 D                0 1            1
                                                  2
                                                       1
                                                       4
                                                            1
                                                            8
                                                                     1
                                                                     8           16 − 16
                                                                                  1    1
                                                                                                − 32
                                                                                                   1
                                                                                                               (6.5 c)
               η4                  1       3
                                           4    32
                                                  9    9
                                                      32
                                                            9
                                                           64
                                                                     9
                                                                    256         512 − 128
                                                                                  9   13
                                                                                                − 256
                                                                                                  13

               η4 D                0 1            3
                                                  4
                                                       9
                                                      16
                                                            9
                                                           32
                                                                    27
                                                                    64
                                                                                 27
                                                                                128
                                                                                       9
                                                                                       32
                                                                                                   9
                                                                                                   64
               η5                  1 1            2 − 28 − 56
                                                  1    1    1       11
                                                                    28
                                                                                 11
                                                                                 56
                                                                                      193
                                                                                      336
                                                                                                  193
                                                                                                  672
               η5 D                0 1            1    1    1
                                                            2        1            2 − 28
                                                                                  1    1
                                                                                                − 56
                                                                                                   1

               1 + ∑5i=1 bi ηi D   1 1            1
                                                  2
                                                       1
                                                       3
                                                            1
                                                            6
                                                                      1
                                                                      4
                                                                                  1
                                                                                  8
                                                                                       1
                                                                                       12
                                                                                                   1
                                                                                                   24
                                                  1    1    1         1           1    1           1
               E                   1 1            2    3    6         4           8    12          24
               E−1η3D              0 1         − 12    1
                                                       4
                                                            1
                                                            8      − 18        − 16
                                                                                 1
                                                                                    − 48
                                                                                       7
                                                                                                − 96
                                                                                                   7


and the fourth order conditions are veriﬁed. To explore the order conditions for order
5, we note that the C(2) conditions are satisﬁed because ξ (∅) = 1 and ξ ( ) = 12 ξ ( )2 ,
for ξ = ηi , i = 1, 2, 3, 4, 5. Hence we need only consider the trees , , , , , , ,
  , . Reconstructing the information for these trees, but only as far as it involves ζ1 ,
we obtain
                          ∅

      ζ1                  1 0       0           0      0      0           0          0         0         0
      ζ2                  0   1 − 12            1
                                                4    − 18   − 48
                                                              7


      η1                  1 0       0           0      0      0
      η1 D                0 1       0           0      0      0           0          0         0         0
      η2                  1   1
                              4    32 − 64
                                    1   1              1
                                                      128
                                                            7
                                                          768
      η2 D                0 1      1
                                   4
                                         1
                                        16            64 − 64
                                                       1   1
                                                                      256 − 256
                                                                       1     1                  1
                                                                                               128
                                                                                                          7
                                                                                                         768

      η3                  1 12         8 − 16
                                       1   1           1
                                                       32
                                                               7
                                                             192                                               (6.5 d)
      η3 D                0 1          1
                                       2
                                            1
                                            4          8
                                                        1
                                                            − 16
                                                              1           1
                                                                          16    − 32
                                                                                  1            1
                                                                                               32
                                                                                                          7
                                                                                                         192

      η4                  1 34      9
                                   32
                                                9
                                               32     256 − 128
                                                        9   13

      η4 D                0 1      3
                                   4
                                                9
                                               16
                                                       27
                                                      64
                                                              9
                                                             32
                                                                       81
                                                                      256
                                                                                      27
                                                                                     256       512 − 128
                                                                                                9    13


      η5                  1 1          2 − 28
                                       1   1           11
                                                       28
                                                             193
                                                             336
      η5 D                0 1       1           1      1    − 28
                                                              1
                                                                          1 − 28
                                                                              1                11
                                                                                               28
                                                                                                         193
                                                                                                         336

      1 + ∑5i=1 bi ηi D   1 1          1
                                       2
                                                1
                                                3
                                                       1
                                                       4
                                                               1
                                                              12
                                                                          1
                                                                          5
                                                                                      1
                                                                                     15
                                                                                                1
                                                                                               20
                                                                                                          1
                                                                                                         60
                                       1        1      1       1          1           1         1         1
      E                   1 1          2        3      4      12          5          15        20        60
6.5 Order of general linear methods                                                           235

Starting method for ζ2

Strictly speaking, the value of ζ2 used in (6.5 d) does not satisfy the order 5 condition
Eζ2 = η3 D + O6 , but it is satisfactory to use a starting method based on ζ2 without
causing a loss of global order. Hence we will derive a generalized four stage Runge–
Kutta method
                                         c A
                                                 ,                                 (6.5 e)
                                         0 bT
satisfying
                                    Φ(t) = θ (t),      |t| ≤ 4.                            (6.5 f)


Exercise 57 Show that for the four stage method (6.5 e). satisfying (6.5 f), c4 = − 12 .


   A possible solution of these conditions gives the starting method

                                 − 12
                                   7
                                      − 12
                                        7

                                  − 76      0 − 76
                                                                  .
                                  − 12   − 11
                                           28     0 − 28
                                                      3

                                    0       0     0     0     1



A generalization of a classical Runge–Kutta method


The idea of adding additional inputs to a Runge–Kutta method was foreshadowed in
Section 6.4 (p. 225). As an example, we will use the ansartz (6.5 g), in an attempt
to construct a pqrs = 4333 method, where a21 , . . . , u22 , . . . , b31 , . . . , v32 are to be
determined.
                               ⎡                                         ⎤
                                                         1           1
                                  0      0    0 1
                               ⎢                         2           8 ⎥
                               ⎢ a                                       ⎥
                           ⎢                                        23 ⎥
                                   21    0    0   1     u22       u
                              ⎢ 2                                       ⎥
                  A U          ⎢ 3       1
                                              0 1        1
                                                                    0 ⎥
                           =⎢  ⎢         6               6               ⎥.
                                                                         ⎥
                  B V          ⎢ 23      1
                                              0   1      1
                                                                    0    ⎥
                               ⎢         6               6               ⎥
                               ⎢                                         ⎥
                               ⎣ 0       0    1 0 0                 0 ⎦
                                 b31 b32 b33 0 v32                  0

Use a similar notation to (6.5 d) but because we are deriving a pqrs method with
q = 3, we need only use a single third order tree and a single fourth order tree .
236                                                       6 B-series and multivalue methods

                              ∅

      ζ1                      1    0             0               0             0
      ζ2                      0    1             0               0             0
      ζ3                      0    0             1               1
                                                                 3             θ
      η1                      1    1
                                   2
                                                 1
                                                 8
                                                                1
                                                                24
      η1 D                    0    1             1
                                                 2
                                                                1
                                                                4
                                                                               1
                                                                               8

      a21 η1 D + ∑2i=1 u2i ζi 1 a21 + u22   1
                                            2 a21 + u23
                                                           1       1
                                                           4 a21 + 3 u23
      η2                      1    1             1
                                                 2
                                                                 1
                                                                 3
      η2 D                    0    1             1               1             1
      η3                      1    1             1
                                                 2
                                                                 1
                                                                 3
      η3 D                    0    1             1               1             1
      ∑3i=1 b3i ηi D+v32 ζ2   0 b31 +b32    1
                                            2 b31
                                                            1
                                                            4 b31
                                                                           1
                                                                           8 b31
                                +b33 +v32    +b32 +b33       +b32 +b33      +b32 +b33
      Eζ3                     0     0            1               7
                                                                 3           θ +4

 From a21 η1 D + ∑2i=1 u2i ζi = η2 + O4 , it is found that a21 = 2, u21 = −1, u22 = − 12
and from ∑3i=1 b3i ηi D + v32 ζ2 = Eζ3 + O5 , θ = −1, b31 = − 16                     11
                                                                     3 , b32 + b33 = 3 ,
v32 = 53 . Making the arbitrary choice b33 = 0, we arrive at the 4333 method:
                                    ⎡                                ⎤
                                                            11
                                        0   0    0   1
                                  ⎢                         28    ⎥
                                  ⎢ 2                1 −1 − 12    ⎥
                                 ⎢                               ⎥
                                            0    0
                                 ⎢ 2                             ⎥
                         A    U   ⎢         1
                                                 0   1 16 0       ⎥
                                 =⎢
                                  ⎢ 2
                                      3     6                     ⎥.
                                                                  ⎥                 (6.5 g)
                         B    V   ⎢ 3       1
                                                 0   1 16 0       ⎥
                                  ⎢         6                     ⎥
                                  ⎢ 0                           0 ⎥
                                  ⎣         0    1   0     0      ⎦
                                   − 16
                                     3
                                            11
                                             3   0   0      5
                                                            3   0



An example of method derivation

We will construct a method based on a speciﬁc design
                       ⎡                              ⎤
                           0    0     0    0 1 u1
                       ⎢                              ⎥
                       ⎢ a21 0        0    0 1 u2 ⎥
                       ⎢                              ⎥
                       ⎢ a31 a32 0         0 1 u3 ⎥
                       ⎢                              ⎥
                       ⎢ a41 a42 a43 0 1 u4 ⎥ .
                       ⎢                              ⎥
                       ⎢                              ⎥
                       ⎣ 1b    b 2    b 3  b4   1   0 ⎦
                          β1 β2 β3 β4 0 0
6.5 Order of general linear methods                                                              237

Our aim will be to obtain order 5, with the additional assumptions that the ﬁrst
component of the starting method is the identity mapping and that each of the stages
has internal order 3.
                                                                                     51
1. First choose c1 , c2 , c3 , c4 so that an order 5 quadrature formula               0 φ (x)dx ≈
   ∑4i=1 bi φ (ci ) is possible. The choice actually made was

                                        3
                             cT = 0 34 10 1 ,         bT =   5 32 250 1
                                                             54 81 567 14     .

2. To obtain stage order 3 for the ﬁrst stage, choose u1 = 0. Without loss of generality
   choose u2 = 1. Also choose a21 = 34 ; this will have the effect of forcing ∑4i=1 βi to
   equal zero in the following item.
3. Solve for u3 , a31 , a32 , a41 , a42 , a43 , β1 , β2 , β3 , to ensure that the stage order is 3,
   with u4 , β4 free parameters. Note that the conditions for these stage orders are
   equivalent to the requirements that the following quadrature formulae are exact
   for φ any polynomial of degree less than 3:
               c2
                    φ (x) d x ≈ β1 φ (−1) + β2 φ (c2 − 1)
           0
                                            + β3 φ (c3 − 1) + (β4 + a21 )φ (0),
               c3
                    φ (x) d x ≈ u3 β1 φ (−1) + u3 β2 φ (c2 − 1) + u3 β3 φ (c3 − 1)
           0
                                            + (u3 β3 + a31 )φ (0) + a32 φ (c2 ),
                1
                    φ (x) d x ≈ u4 β1 φ (−1) + u4 β2 φ (c2 − 1) + u4 β3 φ (c3 − 1)
            0
                                            + (u4 β3 + a41 )φ (0) + a42 φ (c2 ) + a43 φ (c3 ).

4. Impose an additional condition so that the remaining order condition is satisﬁed.
   This remaining condition is

                                      bT (Ac3 + β T (c − 1)3 u) = 60
                                                                  1
                                                                     ,

   corresponding to t14 . This gives

                                              27 )(β4 − 7 ) = 0.
                                        (u4 + 208       27


   We consider two cases:
    (i) u4 = − 208
                27 .
   (ii) β4 = 27
             7
5. The remaining parameter is chosen so that β T u = 0. This condition is considered
                                                                         [n−1]
   advantageous because, at least for small h, a small perturbation of y2      will have
                      [n]
   a small effect on y2 . The solutions are: Case (i): β4 = 448 . Case (ii): u4 = 691
                                                             27
                                                                                   540 .

The methods found using these steps are
238                                                                          6 B-series and multivalue methods
                                           ⎡                                                     ⎤
                                                    0          0        0        0       1   0
                                   ⎢                                                             ⎥
                                   ⎢ 3     0                            0        0       1    1 ⎥
                                   ⎢ 4                                                           ⎥
                                 ⎢
                                   ⎢ 93 − 9
                                                                                                 ⎥
                                                                                              44 ⎥
                         A     U   ⎢ 250 125                            0        0       1 125 ⎥
       (i)                        =⎢
                                   ⎢− 139 148
                                                                                                 ⎥,
                                                                                                 ⎥
                         B     V   ⎢ 27    81
                                                                        350
                                                                         81      0       1 − 208
                                                                                             27 ⎥
                                   ⎢                                                             ⎥
                                   ⎢ 5     32                           250       1              ⎥
                                   ⎢ 54                                                  1 0 ⎥
                                   ⎣       81                           567      14
                                                                                                 ⎦
                                                             24 − 672
                                                 113         41  2375             27
                                                 64                              448     0 0
                                           ⎡                                                     ⎤
                                       0    0      0                                 0   1 0
                                   ⎢                                                             ⎥
                                   ⎢ 3      0      0                                 0   1 1 ⎥
                                   ⎢ 4                                                           ⎥
                                 ⎢
                                   ⎢ 93 − 9
                                                                                                 ⎥
                                                                                              44 ⎥
                         A     U   ⎢ 250 125       0                                 0   1 125 ⎥
      (ii)                        =⎢
                                   ⎢
                                                                                                 ⎥.
                                                                                                 ⎥    (6.5 h)
                                   ⎢ 8640 3240 − 5184
                                      8881 1069  1855
                         B     V                                                     0   1 691
                                                                                             540 ⎥
                                   ⎢                                                             ⎥
                                   ⎢ 5      32    250                              1             ⎥
                                   ⎢ 54                                                  1 0 ⎥
                                   ⎣        81    567                             14
                                                                                                 ⎦
                                       16 − 6
                                     − 19   37   1175
                                                  336
                                                                                  27
                                                                                  7      0 0




Constructing a starting method


We will conﬁne our attention to Case (ii), given by (6.5 h).
  To get the right-hand sides for the starting order conditions, we need a Runge–
Kutta method (with y0 deleted)

                                      0
                                     c2       a21
                                                                         ,
                                     c3       a31       a32
                                      0        
                                               b1         
                                                          b2       
                                                                   b3

with
                         
                         b1 + b2 + 
                                    b3 = β T 1                           = 0,
                         b2 c2 + 
                                 b3 c3 = β T (c − 1)                     9
                                                                         = 32 ,
                             b2 c22 + 
                                        b3 c23 = β T (c − 1)2 = 64
                                                                  9
                                                                     ,
                                   b3 a32 c2 = β (c − 1) =
                                                   1 T         2   9
                                                                       ,
                                                      2                        128


with possible solution
6.6 An algorithm for determining order                                                        239




 error

   10−4




  10−10



                  10−2                          10−1                         100
                                                                     h

    Figure 12 The general linear method (6.5 h), compared with a standard ﬁfth order Runge–
    Kutta method for the Kepler problem




                                     0
                                     1      1
                                     4      4
                                     1            1
                                                            .
                                     2      0     2

                                     0   − 16
                                           9
                                                  0    9
                                                       16




Recursive evaluation of starting methods

Let Rh be a given starting method for the non-principal values. Calculate y[0] . y[0] =
                                                                  [1]
Rh y0 . Use the method to ﬁnd y[1] . Evaluate y[1] . Evaluate Rh y1 . Then evaluate
                        
(I −V )−1 y[1] − Rh y1 . Add this to y[0] to get R+
                     [1]
                                                  h.
  As a numerical test for (6.5 h), the Kepler problem with eccentricity e = 0.1
was solved over a half period. The results compared with a standard ﬁfth order
Runge–Kutta method (shown with dashed lines) are presented in Figure 12.



6.6 An algorithm for determining order

In this section we will conﬁne our attention to methods which can be written using
partitioned matrices:
240                                                                       6 B-series and multivalue methods
                                                       ⎡                         ⎤
                                                         A        1    U
                                  A    U            ⎢                       ⎥
                                                   =⎢
                                                    ⎣ b
                                                       T
                                                                    1    0T ⎥
                                                                            ⎦,                      (6.6 a)
                                  B    V
                                                           B        0    V


where 1 ∈ σ (V ).


The invariant subspace and the underlying one-step method

Following [64] (Kirchgraber, 1986), and [87] (Stoffer, 1993), we consider an ap-
proach to order of accuracy in which the “error” in Figure 10 (p. 231) is eliminated
but, at the same time, E h is replaced by an approximation with S h replaced by the
mapping S h ∗ . We now have
                                                           
                           M h S h ∗ (y0 ) = S h ∗ E h ∗ (y0 ) ,              (6.6 b)

   Associated with the underlying one step method is “the invariant subspace”, see
[87]. In this brief introduction to these important concepts, our aim will be limited to
ﬁnding B-series for S h∗ and E h∗ .


Transformations

Let T h : RN → RN be a central mapping then
                                                                          
                                  M h Sh (      h E
                                           y0 ) = S   h (
                                                          y0 ) ,

where y0 = T h −1 y0 and
                    S h ∗ = Sh ◦ T h ,          Sh = S h ∗ ◦ T h −1 ,
                                      h ◦ T h , E
                     E h ∗ = T h −1 ◦ E           h = T h ◦ E h ∗ ◦ T h −1 .

This can be veriﬁed by substitution into (6.6 b).
  If S h ∗ is partitioned as           ⎡          ⎤
                                                       (S1∗ )h
                                      Sh = ⎣
                                           ∗
                                                                ∗
                                                                    ⎦,
                                                           Sh
a convenient choice of T h is
                                           T h = (S1∗ )h −1
because the ﬁrst component of S   h will be the identity mapping and the analysis is
simpliﬁed.
   The quantities involved in the calculation of a single step, with input y[0] , and
output y[1] , followed by the corresponding B-series quantities, are
6.6 An algorithm for determining order                                                         241
            ⎡                    ⎤              ⎡                        ⎤
                 (S1∗ )h (y0 )                        E ∗h (S1∗ )h (y0 )
      y[0] = ⎣                   ⎦,      y[1] = ⎣  ∗ ∗          ⎦,           Y,       hF,
                  S ∗h (y0 )                        E h S h (y0 )
             ⎡        ⎤                        ⎡        ⎤
                                                   ∗
                 ζ1                              E   ζ1
       ζ =⎣           ⎦,                E∗ ζ = ⎣        ⎦,                     η ∗,   η ∗ D,
                 ζ                               E∗ ζ

The calculation of the single step uses the formulae
                                                        [0]
                                       Y = hAF + 1y1 + U y[0] ,
                                       [1]             [0]
                                      y1 = hbT F + y1 ,
                                      y[1] = hBT F + V y[0] ,

corresponding to the B-series relations,

                                       η ∗ = A(η ∗ D) + 1ζ1 + U ζ ,
                                     E∗ ζ1 = bT (η ∗ D) + ζ1 ,                             (6.6 c)
                                     E∗ ζ = B(η ∗ D) + V ζ .

Substitute η = ζ1 η, ζ ∗ = ζ1 ξ and it follows that

                                         η = A(ηD) + 1 + Uξ ,
                                          = bT (ηD) + 1,
                                         E
                                        = B(ηD) + V ξ ,
                                       Eξ

       = ξ −1 E∗ ξ .
where E
   To obtain tree-by-tree formulae for η, E and ξ , start with η(∅) = 1, E(∅)
                                                                              = 1,
ξ (∅) = 0. Then for t = [t 1 t2 · · · t m ], we ﬁnd

                     η(t) = A(ηD)(t) + Uξ (t),
                      = bT (ηD)(t),
                     E(t)                                                                 (6.6 d)
                                                                   
                                                     
                     ξ (t) = (I − V )−1 B(ηD)(t) − ∑ E(t t )ξ (t ) ,
                                                              t  <t

with (ηD)(t) = ∏mi=1 η(t i ) and with exponentiations taking precedence over other
operations. The details of these calculations, to order 5, are shown in Table 18
(p. 242).


Test for conjugacy
                   the ﬁnal step in the test for order p is to determine if ξ exists
Having calculated E,
such that
242                                                                       6 B-series and multivalue methods


                                         Table 18 Details of (6.6 d)

            n
 n tn |tn | E          ξn                                                                    ηn
 0 ∅    0 1            0                                                                     1
                                         
 1      1   bT 1       (I − V )−1 B1                                                         A1 + Uξ1
                                                     
 2      2   bT η1      (I − V )−1 Bη      1 − E1 ξ1                                          Aη1 + Uξ2
                                                            
 3      3 bT η12       (I − V )   −1         21 ξ1 − 2E
                                      Bη12 − E         1 ξ2                                 Aη12 + Uξ3
                                                               
 4      3 bT η2        (I − V )   −1         2 ξ1 − E
                                      Bη2 − E         1 ξ2                                  Aη2 + Uξ4
                                               3           2            
 5      4 bT η13       (I − V )   −1 1 ξ1 − 3E
                              Bη13 − E          1 ξ2 − 3E 1 ξ3                             Aη13 + Uξ5
                                                     2                           
 6      4 bT η1 η2 (I − V )−1 Bη1 η2 −E 1 E
                                           2 ξ1 −(E 1 +E2 )ξ2 −E
                                                                   1 ξ3 − E
                                                                            1 ξ4            Aη1 η2 + Uξ6
                                                              
 7      4 bT η3    (I − V )−1 Bη3 − E         21 ξ2 − 2E
                                      3 ξ1 − E          1 ξ4                               Aη3 + Uξ7
                                                                
 8      4 bT η4     (I − V )−1 Bη4 − E 4 ξ1 − E  2 ξ2 − E
                                                           1 ξ4                             Aη4 + Uξ8
                                                                                
 9      5 bT η14    (I − V )−1 Bη14 − E41 ξ1 − 4E 32 ξ2 − 6E21 ξ3 − 4E  1 ξ5             Aη14 + Uξ9
                              
10      5 bT η12 η2 (I − V )−1 Bη12 η2 − E 21 E
                                               2 ξ1 − (E31 + 2E 31 )ξ2                    Aη12 η2 + Uξ10
                                                                                         
                                            21 + E
                                         −(2E              21 ξ4 − E
                                                  2 )ξ3 − E        1 ξ5 − 2E
                                                                              1 ξ6
                             
11      5 bT η1 η3 (I − V )−1 Bη1 η3 − E  3 ξ1 − (E
                                       1 E         31 + E        21 ξ3
                                                          3 )ξ2 − E                         Aη1 η3 + Uξ11
                                                                          
                                            −2E 21 ξ4 − 2E
                                                           1 ξ6 − E
                                                                    1 ξ7
                             
12                                     1 E
        5 bT η1 η4 (I − V )−1 Bη1 η4 − E           1 E
                                          4 ξ1 − (E       4 )ξ2
                                                      2 + E                                 Aη1 η4 + Uξ12
                                                                                     
                                                    −E       21 ξ4 − E
                                                     2 ξ3 − E         1 ξ6 − E
                                                                               1 ξ8
                                                                                      
13      5 bT η22                         22 ξ1 − 2E
                       (I − V )−1 Bη22 − E              2 ξ2 − E
                                                    1 E         21 ξ3 − 2Eξ
                                                                            4 − 2E
                                                                                  1 ξ6 Aη 2 + Uξ13
                                                                                           2
                                                   3          2            
14      5 bT η5                −1                          
                       (I − V ) Bη5 − E5 ξ1 − E1 ξ2 − 3E1 ξ4 − 3E1 ξ7                   Aη5 + Uξ14
                                 
15      5 bT η6                          6 ξ1 − E
                       (I − V )−1 Bη6 − E        1 E        21 + E
                                                    2 ξ2 − (E     2 )ξ4                    Aη6 + Uξ15
                                                                                         
                                                                       1 ξ7 − E
                                                                      −E        1 ξ8
                                                                          
16      5 bT η7                         7 ξ1 − 4E
                       (I − V )−1 Bη7 − E                 21 ξ4 − 2E
                                                  3 ξ2 − E          1 ξ8                   Aη7 + Uξ16

                                                                         
17      5 bT η8                          8 ξ1 − E
                       (I − V )−1 Bη8 − E         4 ξ2 − E
                                                           2 ξ4 − E
                                                                    1 ξ8                    Aη8 + Uξ17




                                                E = ξ −1 Eξ ,
                             
to order p; that is, that (ξ E)(t) = (Eξ )(t) for all t such that |t| ≤ p. Write E = a,
E = b, ξ = x.
6.6 An algorithm for determining order                                                                    243

Test that x exists such that xa = bx + O p+1
Given two members a, b, ∈ B we will consider the question “For a given integer
p, does there exist x ∈ B such that xax−1 = b + O p+1 ?” To avoid uninteresting
complications, we will assume that b(τ) = 0.


Conjugacy to order 4
Using the notation based on xi = x(ti ), we can write down the conditions (xa)i − xi =
(bx)i − xi for i ≤ 4 (where the term xi is subtracted from each side for convenience):
                                                  a1 = b1 ,
                                         x1 a1 + a2 = b2 + b1 x1 ,
                                 x1 a2 + x2 a1 + a4 = b4 + b2 x1 + b1 x2 ,                              (6.6 e)
                       a2 x12 + a1 x3 + 2a4 x1 + a7 = b7 + b21 x2 + 2b1 x4 + b3 x1 ,
                         a1 x4 + a2 x2 + a4 x1 + a8 = b8 + b1 x4 + b2 x2 + b4 x1 .

                              x12 a1 + 2x1 a2 + a3 = b3 + b21 x1 + 2b1 x2 ,
                   a1 x1 + 3a2 x12 + 3a3 x1 + a5 = b5 + b31 x1 + 3b21 x2 + 3b1 x3 + x5 ,
                       3
                                                                                                        (6.6 f)
   x1 a1 x2 + a2 (x12 + x2 ) + x1 a3 + a4 x1 + a6 = b6 + b1 x1 b2 + x2 (b21 + b2 ) + b1 x3 + x4 b1 .

From the equations (6.6 e), solve for a1 , a2 , a4 , a7 , a8 ; and for the equations in (6.6 f),
solve for x2 , x3 , x4 , to obtain
       −b21 x1 + b1 x12 + 2b2 x1 + a3 − b3
   x2 =                                       ,
                        2b1
       b3 x1 − 3b21 x12 − 2b1 x13 − 6b1 b2 x1 + 6b2 x12 − 3a3 b1 + 6a3 x1 + 3b1 b3 + 2a5 − 2b5
   x3 = 1                                                                                      , (6.6 g)
                                                  6b1
       2b3 x1 −3b21 x12 +b1 x13 −6b1 b2 x1 +6b2 x12 +3a3 x1 −3b3 x1 +6b4 x1 −2a5 +6a6 +2b5 −6b6
   x4 = 1                                                                                          .
                                                       6b1

 From these equations, we can summarize the conditions for conjugacy to order
p ≤ 4:
                                  a1 = b1 ,
                                  a 2 = b2 ,
                                          a 4 = b4 ,
                      −a1 a3 + a5 − 2a6 + a7 = b7 − b1 b3 + b5 − 2b6 ,                                 (6.6 h)
                                                  a8 = b8 .



The role of x1
In the derivation of the order 4 conjugacy conditions, the transformation parameters
x(t), |t| ≤ 3, were allowed to take on arbitrary values. However, x(τ) = x1 does not
seemed to be constrained in any way even though (6.6 h) might have been expected
to depend on x1 . It would have been a simpler computation to take this quantity to be
244                                                                        6 B-series and multivalue methods

zero so that
                                       x1 = 0,
                                            a3 − b3
                                       x2 =         ,
                                               2b1
                                            b3 − a3 a5 − b5
                                       x3 =          +      ,
                                                2      3b1
                                            a6 − b6 b5 − a5
                                       x4 =          +      .
                                                b1     3b1
The fact that the conjugate order conditions do not depend on x1 , for any order,
follows from the observation that if xa ∼ bx then x a ∼ bx , where x = bx and, if
the order conditions are polynomials in x1 then the value of these polynomials are
unchanged if x1 is replaced by x1 = x1 + b1 , which is impossible if b1 = 0.


Order by order conjugation
We now return to the case of arbitrary p. For |t| = 1, we have the single necessary
condition x(τ) + a(τ) = b(τ) + x(τ), implying

                                                   a(τ) = b(τ).

Before moving on to higher orders, we remark that the value of x(τ) is irrelevant
because a = x−1 bx is equivalent to a = x−1 bx, where x = bθ x, for any real θ , so that
x(τ) = x(τ) + θ .
   For t = [τ], we have

                           x(t) + x(τ) + a(t) = b(t) + x(τ) + x(t),

so that
                                                a([τ]) = b([τ]).
For p > 2, we will assume that the order is already known to be at least equal to
p − 1.
   Divide the trees of order p into two subsets

S1 t = [τ nt1 · · ·tm ], n ≥ 1, τ ∈
                                  / {t1 , . . . ,tm }

S2 the remaining trees of order p.

For t ∈ S1 , let t = [τ n−1 t1 · · · t m ]. Then the order condition becomes

               nb(τ)x(t ) +           ∑                 b(t t )x(t ) = ∑ x(t t )a(t ).
                               t  ≤t,|t  |<|t  |                     t  ≤t

This deﬁnes x(t ) for all t of order p − 1.
   For t ∈ S2 , using the known values of x for all orders less than p, we obtain an
order condition for each t in this set.
6.6 An algorithm for determining order                                                   245


      Algorithm 7 Determine whether two central B-series for which τ → 0 are conjugate

Input:      a, b ∈ B, a = b + O p
Output: if ∃x(a := xax−1 = b + O p+1 ) then [true, a , x] else [ false]
    %
             p := T =
    %       T        p      p−1 ∗ τ)
                         (T =
    % μ(t) := m, where t = [τ m−1 t 1 t 2 · · · t k ], |t i | > 1, i = 1, 2, . . . , k
    %
  1 x ← 1 + Op
  2 for t ∈ T = p−1 do
  3     x(t) ← (a(t ∗ τ) − b(t ∗ τ))/μ(t)b(τ)
  4 end for
  5 TEST ← true
  6 for t ∈ T = p      p−1 ∗ τ) do
                    (T =
  7     if (xa)(t) = (bx)(t) then
  8         TEST ← false
  9     end if
 10 end for
 11 if TEST then
 12     return [ true, xax−1 ]
 13 else
 14     return [ false]
 15 end if




Summary of Chapter 6 and the way forward

Summary

Multivalue methods have a long history in the form of linear multistep methods. In
this chapter, an amalgam of multivalue and multistage (Runge–Kutta) methods is
considered as a family of method, in its own right and given the name “general linear
methods”.
    After a review of linear multistep methods, the prototypical multivalue methods,
it is shown by example that new methods ﬂow from these by allowing multiple
vector ﬁeld calculations. Similarly, Runge–Kutta methods, the prototypical one-step
methods, are also simply examples of known, and not so well known, multistage
multivalue methods.
    The insight provided by this wide range of example methods underlines the use of
the natural and highly ﬂexible general linear formulation. The fundamental questions
of consistency, order, and convergence, take on a natural and straightforward meaning
in the general context.
    The theory of order for these methods is an important application of B-series
analysis. This is closely related to the existence of the underlying one-step method
and the theory of invariant subspaces.
    Throughout the chapter new methods are introduced and analysed.
246                                                            6 B-series and multivalue methods

The way forward

The B-series approach is adapted to structure-preserving algorithms, as exempliﬁed
in the cases of symplectic preservation and energy-preservation, in Chapter 7.


Teaching and study notes

The following is a sample of the many publications on multivalue and general linear
methods.
Burrage K. and Butcher, J.C. Non-linear stability of a general class of differential
equation methods (1980) [6]
Butcher, J.C. On the convergence of numerical solutions to ordinary differential
equations (1966) [12]
Butcher, J.C. General linear methods (2006) [19]
D’Ambrosio, R. and Hairer, E. Long-term stability of multi-value methods for or-
dinary differential equations (2014) [40]
Hairer, E. and Wanner, G. On the Butcher group and general multi-value methods
(1974) [52]
Jackiewicz, Z. General Linear Methods for Ordinary Differential Equations (2009)
[63]
Stoffer, D. General linear methods: connection to one step methods and invariant
curves (1993) [87]



Projects
Project 22   Read up about predictor-corrector methods.
Project 23   Find criteria for the matrix V in a method (A,U, B,V ) to be stable.
Project 24 Read up about the underlying one-step method and invariant sub-spaces, starting with
[87].
Project 25   Learn about “Peer methods”, as a special class of general linear methods.
