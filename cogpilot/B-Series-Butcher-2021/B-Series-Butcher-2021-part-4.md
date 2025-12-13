# B-Series: Algebraic Analysis of Numerical Methods - Part 4

**Author:** John C. Butcher

**Series:** Springer Series in Computational Mathematics, Volume 55

---

Chapter 3
B-series and algebraic analysis




3.1 Introduction

This chapter is built on the broad introduction to numerical methods for initial value
problems in Chapter 1 and the study of trees and related graph-theoretic structures in
Chapter 2.
   Initial value problems, and methods for obtaining numerical approximations to
their solutions, are typically described in terms of triples (y0 , f , h), where

                        y0 ∈ R N ,         f : R N → RN ,           h ∈ R.

For example, an initial value problem can be written as

                           y = f (y),        y(x0 ) = y0 ,     x0 ∈ R,                 (3.1 a)

and the aim could be to evaluate y(x0 + h). A classical requirement is to ﬁnd the
“order” of a Runge–Kutta method. This single question can be viewed as a combina-
tion and a comparison of the solutions of two individual Taylor series questions.

1. Find the series expansion of the solution to (3.1 a) in the form

                     y(x0 + h) = y0 + h f (y0 ) + 12 h2 f  (y0 ) f (y0 ) + · · · ,     (3.1 b)

2. Find the series expansion of the numerical result computed by a Runge–Kutta
   method
                                      c A
                                              ,
                                          bT
   in the form

                    y1 = y0 + h(bT 1) f (y0 ) + h2 (bT c) f  (y0 ) f (y0 ) + · · · .   (3.1 c)


© Springer Nature Switzerland AG 2021                                                       99
J. C. Butcher, B-Series, Springer Series in Computational Mathematics 55,
https://doi.org/10.1007/978-3-030-70956-3_3
100                                                       3 B-series and algebraic analysis

The term“Question” will be used in a technical sense much like “problem” is often
used. It will always be concerned with a generic problem with the three ingredients:
y0 , f and h. The questions discussed above are two of several questions that can be
asked, for which the answers have a common pattern. That is, each of the solutions
can be written as a formal series in which the terms are always the same, except for
the assignment of different numerical coefﬁcients. These are known as B-series and
the coefﬁcients referred to are “B-series coefﬁcients”. What will be referred to as
“B-series analysis”, which is the principal subject of this book, is the use of B-series
coefﬁcients as a means of analysing the underlying approximations.
   The question of order is solved by equating the coefﬁcient of h, h2 , . . . in each of
the two series (3.1 b) and (3.1 c), starting with

                                       bT 1 = 1,
                                       bT c = 12 .

A fuller suite of model questions is introduced in Section 3.2



Chapter outline

Section 3.2 begins with a discussion of covariance of possible questions that can be
posed in terms of the two components of an initial value problem and a time unit.
“Elementary differentials” are informally introduced and an elementary question set
is introduced.
   In Section 3.3, Taylor series will be reviewed in a multi-dimensional setting for
use throughout the chapter. The basic components are written in terms of Fréchet
derivatives f (n) (y), written as n-linear operators.
   The central component of this chapter, Section 3.4, is concerned wih elementary
differentials. These enable the terms in B-series to be constructed. Section 3.5
considers the derivation of B-series in two important cases. The principal techniques
used ar repeated differentiation and evolution. The so called “elementary weights”,
which arise as B-series coefﬁcients for Runge–Kutta methods, and allow the order
conditions to be expressed, are the subject of Section 3.6.
   In Section 3.7, elementary differentials are found for a Kronecker product based
on a tableau for a Runge–Kutta method, combined with a standard initial value
problem. This leads to an alternative construction of “elementary weights”, and the
order of Runge–Kutta methods. Constructibility of Runge–Kutta methods of arbitrary
order, and the independence of elementary differentials follow from “attainable value”
results given in Section 3.8. Finally, in Section 3.9, the composition of B-series is
considered.
   The principal references for this chapter are [7, 14] (Butcher, 1963, 1972) and
[51, 52] (Hairer, Wanner, 1973, 1974).
3.2 Autonomous formulation and mappings                                                  101

3.2 Autonomous formulation and mappings

Given three ingredients
y0     A point in the vector space y0 ∈ RN ,
 f     A mapping f : RN → RN ,
 h     A non-zero real number,
there are many calculations that can be made and questions that can be formulated.
In this discussion, the dimension N will be an arbitrary positive integer.
   In this chapter, “problem” will refer to a triple (y0 , f , h). This might be used to
answer a “question” such as:
   Given an initial-value problem

                            y (x) = f (y(x)),      y(x0 ) = y0 ,

what is the solution at x = x0 + h?
   Another question might be to evaluate the approximate solution to such a problem,
using a speciﬁc numerical method.
   We will distinguish between problems and questions.
Covariance of problems and questions

Given a problem formulated in terms of the triple (y0 , f , h), it is possible to rewrite
the same problem in different ways by rescaling the time variable h → sh or by
carrying out an afﬁne transformation η → Mη + d, with M a nonsingular linear
operator on RN , so that (y0 , f , h) is transformed to (y0 , f , h) = (My0 + d, M f + d, sh).
Consider, for example, the question of calculating a single step of the Euler method,
with stepsize h. That is, evaluating y1 , where

                                    y1 = y0 + h f (y0 ).                              (3.2 a)

What does it mean to transform to the (y0 , f , h) formulation?
  This can be interpreted in two different ways:
     Solve the problem using transformed variables to give

                                      y1 = y0 + h f (y0 ),                            (3.2 b)

     Solve the problem using the original variables and then rewrite in the new coordi-
     nates leading to

                          (M y1 + d) = (M y0 + d) + sh f (M y0 + d),

     which simpliﬁes to
                                y1 = y0 + shM −1 f (M y0 + d).                        (3.2 c)
The two results (3.2 b) and (3.2 c) are identical, if and only if

                           f (y) = sM −1 f (M y + d),      for all y,
102                                                         3 B-series and algebraic analysis

or, what is equivalent,
                                                  
                       f (y) = s−1 M f M −1 (y − d) ,     for all y.

This example singles out the question of evaluating (3.2 a) as having the property
referred to in Deﬁnition 3.2A below. But there are many questions also satisfying the
same property and these will be the questions that are of signiﬁcance in this book

 Deﬁnition 3.2A A question Q(y0 , h, f ) is covariant if the following diagram com-
 mutes
                                           Q
                             (y0 , h, f ) −−−−→ Q(y0 , h, f )
                                ⏐                  ⏐
                                ⏐                  ⏐
                                ⏐T                 ⏐T                              (3.2 d)
                                )                  )
                                           Q
                             (y0 , h, f ) −−−−→ Q(y0 , h, f )

 where the transformation T : (y0 , h, f ) −→ (y0 , h, f ) satisﬁes

                               y0 = M y0 + d,                                       (3.2 e)
                                 h = sh,                                            (3.2 f)
                                                        
                             f (y) = s−1 M f M −1 (y − d) .                        (3.2 g)

In studying the properties of covariant questions in more detail, write corresponding
mappings as ah , bh , . . . and the mapped values as ah y0 , bh y0 , . . . .

 Deﬁnition 3.2B If a0 y0 = y0 , then ah is a central mapping; if a0 y0 = 0, then ah is
 a differential mapping.

                                                                                  
 For example, y0 → y0 + h f (y0 ) is a central mapping and y0 → h f y0 + h f (y0 ) is a
differential mapping. Note the factor h with every use of f because of the factors s
and s−1 in (3.2 f) and (3.2 g) respectively. Furthermore, a factor of h is possible only
in this context.
   The factor M appearing in Deﬁnition 3.2A corresponds merely to a linear change
of basis and many properties of covariance can be deduced using a simpliﬁed trans-
formation in which s = 1 and M = I.
   That is, we can argue from a form of the commutation diagram (3.2 d) in which
(3.2 e)–(3.2 g) are replaced by
                                   y0 = y0 + d,                            (3.2 h)
                                       h = h,
                                    f (y) = f (y − d).                                (3.2 i)
3.2 Autonomous formulation and mappings                                                                              103



 Lemma 3.2C The mapping ah = h f (θ y0 ), for θ a real parameter, is covariant if
 and only if θ = 1.

Proof. Apply the covariance criterion, using transformation rules (3.2 h) – (3.2 i), to
ah ; it is found that                      
                         h f θ (y0 + d) − d = h f (θ y0 ),
implying that θ = 1.



Two introductory questions

Two questions given below, (3.2 m) and (3.2 n), will be solved to within O(h4 ) using
Taylor series. This discussion also has the role of a ﬁrst introduction to “elementary
differentials”.
Approximations using Taylor series
It is interesting to ask what approximations of the form
                             E = C1 h +C2 h2 + · · · +C p h p + O(h p+1 )
can be found by iterated use of expressions of the form
                                                  h f (y0 + E),                                                (3.2 j)

where E has a similar form. We will conﬁne ourselves, in this informal discussion, to
p = 3. We will start the iterations using the single term E = θ h f (y0 ), where θ is a
constant. By Taylor’s theorem we obtain the result
                                                                                
      h f (y0 ) + θ h2 f  (y0 ) f (y0 ) + θ 2 2!1 h3 f  (y0 ) f (y0 ), f (y0 ) + O(h4 ), (3.2 k)
                                                         
where the expression f  (y0 ) f (y0 ), f (y0 ) means the evaluation of the bilinear
operator f  (y0 ) acting on two copies of f (y0 ). Taking linear combinations of (3.2 k),
for several distinct values of θ , the individual terms are found, to within O(h4 ). By
carrying out additional iterations of (3.2 j), further terms can be found, such as
    h3 f  (y0 ) f  (y0 ) f (y0 ),   h4 f  (y0 ) f  (y0 ) f  (y0 ) f (y0 ),   h4 f  (y0 ) f  (y0 ) f (y0 )2

The ﬁrst four elementary differentials
In Section 3.4, we will introduce quantities, known as elementary differentials, of
which those up to order 3 are
                                        F1 := f (y0 ),
                                        F2 := f  (y0 ) f (y0 ),
                                        F3 := f  (y0 ) f (y0 )2 ,
                                        F4 := f  (y0 ) f  (y0 ) f (y0 ).
104                                                             3 B-series and algebraic analysis

Our aim, in this preliminary discussion, is to use series of the form
                       y0 + a1 hF1 + a2 h2 F2 + 12 a3 h3 F3 + a4 h3 F4 ,                  (3.2 l)
to answer the two important questions:
Question 1: Find y(x0 + h), where y satisﬁes the initial value problem:
                                        
                         y (x) = f y(x) , y(x0 ) = y0 .                                (3.2 m)
Question 2: Find y1 , satisfying the functional equation:
                                     y1 = y0 + h f (y1 ).                                (3.2 n)
In each case the answer is intended to be a series approximation to within O(h4 ).
Note that the factor 12 , in the term 12 a3 h3 F3 in (3.2 l), will be seen to be an instance
of a systematic scheme to simplify the manipulations.

  As a preliminary to answering the two questions, evaluate the ﬁrst terms of h f (y),
where y is given by (3.2 l). We have
        h f (y0 + a1 hF1 + a2 h2 F2 + 12 a3 h3 F3 + a4 h3 F4 )
                                                                         2
        = h f (y0 ) + h f  (y0 ) a1 hF1 + a2 h2 F2 + 21 h f  (y0 ) a1 hF1 + O(h4 )
         = hF1 + a1 h2 F2 + 12 a21 h3 F3 + a2 h3 F4 + O(h4 ).

For Question 1, we write the two sides of (3.2 m) separately. The left-hand side is
                 h(d / d h)(y0 + a1 hF1 + a2 h2 F2 + 12 a3 h3 F3 + a4 h3 F4 )
                  = a1 hF1 + 2a2 h2 F2 + 32 a3 h3 F3 + 3a4 h3 F4 + O(h4 ),

with the right-hand side equal to

                     h f (y0 + a1 hF1 + a2 h2 F2 + 12 a3 h3 F3 + a4 h3 F4 )
                     = hF1 + a1 h2 F2 + 12 a21 h3 F3 + a2 F4 + O(h4 ),

so that matching coefﬁcients gives
                         a1 = 1,     a2 = 21 ,   a3 = 13 ,   a4 = 16 .


   For Question 2, (3.2 l) becomes, to within O(h4 ),
                   a1 hF1 + a2 h2 F2 + 12 a3 h3 F3 + a4 h3 F4
                   = h f (y0 + a1 hF1 + a2 h2 F2 + 12 a3 h3 F3 + a4 h3 F4 )
                   = hF1 + a1 h2 F2 + 12 a21 h3 F3 + a2 h3 F4 ,

and we deduce that
                                   a1 = a2 = a3 = a4 = 1.
3.2 Autonomous formulation and mappings                                                               105

   Questions 1 and 2 that have been considered are ﬂow h and implicit h respectively,
from the easy question set, which we now introduce. These will be used from time to
time as examples of various results as they are introduced.

The easy question set

The questions in this set, which range from trivial to challenging, are as follows,
where in each case, ah is acting on y0 .

             id h Give the value of y0

          slopeh Calculate h f (y0 )

         Euler h Calculate y1 = y0 + h f (y0 )

      implicit h Calculate y1 , where y1 = y0 + h f (y1 )
                                                                           
   mid -point h Calculate y1 , where y1 = y0 + h f 12 (y0 + y1 )

          ﬂow h Evaluate y1 = y(x0 + h),
                                        where y(x) satisﬁes the initial value
                    problem y (x) = f y(x) , y(x0 ) = y0

   ﬂow-slopeh Evaluate hy 
               
                        (x0+h), where y(x) satisﬁes the initial value problem
                    y (x) = f y(x) , y(x0 ) = y0

     Runge-I h The numerical method given by (1.5 c) (p. 20)

    Runge-II h The numerical method given by (1.5 d) (p. 20)


Exercise 32 Find the Taylor series for mid-point h to within O(h4 ) in terms of F1 , F2 , F3 , F4 .

   As an example, we consider Runge-I h We have in turn, working to within O(h4 ),
              Y1 = y0                = y0
                 hF1 = h f (Y1 )                 =        hF1 ,
                  Y2 = y0 + hF1                  = y0 + hF1 ,
                 hF2 = h f (Y2 )                 =        hF1 + 21 h2 F2 + 16 h3 F3 ,
                   y1 = y0 + 12 hF1 + 12 hF2 = y0 + hF1 + 14 h2 F2 + 12
                                                                     1 3
                                                                        h F3 ,

giving a result, identical with ﬂow h to within O(h3 ).
Exercise 33 Carry out a similar analysis for Runge-II h .


Exercise 34 Carry out a similar analysis for ﬂow-slopeh .
106                                                                          3 B-series and algebraic analysis

3.3 Fréchet derivatives and Taylor series

Use of Polish notation
In this section, and throughout this book, we will use a Polish notation for writing the
result of applying a linear or multilinear operator to a vector or sequence of vectors
[70] (Łukasiewicz,Tarski,1930). For example, if d 1 is a linear operator, and d 2 is a
bilinear operator, then d 1 v and d 2 vw will denote d 1 (v)and d 2 (v, w), respectively. A
similar notation has been adopted in [2] (Azamov, Bekimov, 2016).


Fréchet derivatives

The principal reference for this section is [81] (Rudin, 1976).
   If f is Fréchet differentiable at y = y0 , then a linear operator f  (y0 ) exists so that
the approximation
                               f (y0 + ε) ≈ f (y0 ) + f  (y0 )ε                       (3.3 a)
holds to within o(ε). That is, (3.3 a) should hold so accurately that

                                f (y0 + ε) − f (y0 ) − f  (y0 )ε
                                            ε

can be made arbitrarily small by making ε small enough but non-zero.
   In terms of individual components,
                                         N
           f i (y0 + ε) = f i (y0 ) + ∑ f ji (y0 )ε j + o(ε),                     i = 1, 2, . . . , N,
                                         j=1

where f ji denotes the partial derivative

                                                           ∂ fi
                                                f ji =          .
                                                           ∂yj


Second and higher derivatives
If, for arbitrary δ , the function f  (y)δ is also Fréchet differentiable at y = y0 , with
derivative f  (y0 )δ then, for a vector ε with small norm, it holds that

                      f  (y0 + ε)δ = f  (y0 )δ + f  (y0 )δ ε + o(ε).

Repeating this process, we can, for a multiply-differentiable function, deﬁne a
sequence of linear and multi-linear operators

                            f  (y0 ),       f  (y0 ),      f (3) (y0 ),    ...
3.3 Fréchet derivatives and Taylor series                                                          107

Acting with these operators on a sequence of arbitrary vectors δ1 , δ2 , . . . , we obtain

                      f  (y0 )δ1 ,    f  (y0 )δ1 δ2 ,      f (3) (y0 )δ1 δ2 δ3 ,   ...

The differentiability of each member of this sequence provides the deﬁnition of the
next term in the sequence of high-order derivatives. That is, assuming the estimates
exist,
          f (n) (y0 )δ1 δ2 , . . . δn−1 ε
            = f (n−1) (y0 + ε)δ1 δ2 , . . . δn−1 − f (n−1) (y0 )δ1 δ2 , . . . δn−1 + o(ε).

Write individual components in partial derivative form and we have

                                      (ei )T f  (y0 )δ1 = ∑ f ji δ1j ,
                                                                j
                                      i T 
                                  (e ) f (y0 )δ1 δ2 = ∑ f jk
                                                          i
                                                             δ1j δ2k ,
                                                                jk
                                i T (3)
                             (e ) f       (y0 )δ1 δ2 δ3 = ∑ f jk
                                                              i
                                                                  δ1j δ2k δ3 ,
                                                               jk

and similarly for higher order derivatives.
   This chapter will work largely with Taylor expansions about a standard base
point y0 and, hence, it will be convenient to have a special way of writing Fréchet
derivatives evaluated at this point. Accordingly we will write

                                                  f := f (y0 ),
                                                 f  := f  (y0 ),
                                                f  := f  (y0 ),
                                                                                                (3.3 b)
                                                   ..     ..
                                                    .      .
                                               f (n) := f (n) (y0 ).

If y0 is replaced by some other argument, such as y1 , then we will write

                                          f (n) (y1 ) := f (n) (y1 ).

However, it is emphasized, that f (n) , without an argument, will always denote
f (n) (y0 ).

Taylor series

The approximation of a function of a real variable by its Taylor expansion

   f (y0 + δ ) = f (y0 ) + f  (y0 )δ + 2!1 f  (y0 )δ 2 + · · · + n!1 f (n) (y0 )δ n + o(δ n ), (3.3 c)

is at the heart of numerical analysis but we will need to use it in a multi-dimensional
context. If f : RN → RN , then, as we have seen, f  (y0 ) becomes a linear operator
108                                                                        3 B-series and algebraic analysis

characterized by
                             f (y0 + δ ) = f (y0 ) + f  (x0 )δ + o(δ )
or, in terms of individual components,
                                          N
           f i (y0 + δ ) = f i (y0 ) + ∑ f ji (y0 )δ j + o(δ ),               i = 1, 2, . . . , N,
                                         j=1


where f ji denotes ∂ f i /∂ y0j . We will adopt the “summation convention” so that (3.3 c)
can be written more compactly as

                           f i (y0 + δ ) = f i (y0 ) + f ji (y0 )δ j + o(δ ).

In this notation, higher order Taylor series can also be written

  f i (y0 + δ ) = f i (y0 ) + f ji (y0 )δ j + 2!1 f jk
                                                    i
                                                       (y0 )δ j δ k + o(δ 2 ),
  f i (y0 + δ ) = f i (y0 ) + f ji (y0 )δ j + 2!1 f jk
                                                    i
                                                       (y0 )δ j δ k + 3!1 f jk
                                                                            i
                                                                                (y0 )δ j δ k δ  + o(δ 3 ).

To write the multi-dimensional case in a similar style to (3.3 c), we will use the
sequence deﬁned by (3.3 b), where we note that f denotes the vector f (y0 ) and f 
denotes the linear operator f (1) (y0 ). Similarly f  = f (2) denotes the bilinear operator
that gives the result
                                     f  δ1 δ2 = f  δ2 δ1 ,
with component i equal to
                                                 i
                                               f jk (y0 )δ1j δ2k ,
and a similar pattern for higher order derivatives, evaluated at y0 .
   The designation of speciﬁc components denoted by superscripts will be avoided as
much as possible and will not be used without any warning. This enables us to use the
notation δ n to denote the juxtaposition of n copies of δ . Hence, the multi-dimensional
version of Taylor’s theorem can be written
                                                     p
                                                       1 (n) n
                            f (y0 + δ ) = f + ∑           f δ + o(δ  p ).                            (3.3 d)
                                                   n=1 n!

A typical term in this series, containing the factor f (n) δ n , is traditionally written

                                            f (n) (δ , δ , . . . , δ ).

However, writing this instead as

                                        f (n) δ δ · · · δ or f (n) δ n ,

within the Taylor formula (3.3 d), gives an advantage and an added clarity because
of the linearity inherent in the notation. It is also advantageous to adopt the Polish
notation in understanding terms like this. When the operations are performed in order
3.3 Fréchet derivatives and Taylor series                                                                      109

from the left, the ﬁrst step gives f (n) δ which becomes an n − 1-fold linear operator.
This, in turn, operates on a second copy of δ to yield an n − 2-fold linear operator and
so on, until f (n) δ n becomes a constant vector. Alternatively, we can simply regard
f (n) as an operator requiring n operands which, in this case, are each equal to δ .

Symmetry of higher derivatives
Under sufﬁcient smoothness assumptions the Fréchet derivative f (n) , as an n-linear
operator is symmetric. That is, if π is any permutation of {1, 2, . . . , n}, then

                             f (n) δ1 δ2 · · · δn = f (n) δπ1 δπ2 · · · δπn .

This is a standard result [81] (Rudin, 1976).

Taylor’s series for a sum of perturbations

If δ in (3.3 d) is replaced by a sum of the form

                                        δ = δ1 + δ2 + · · · ,

then terms like δ n can be evaluated using the multinomial theorem. For example, if
δ = δ1 + δ2 + δ3 , then

                 δ 2 = δ12 + 2δ1 δ2 + 2δ1 δ3 + δ22 + 2δ2 δ3 + δ32 ,
                 δ 3 = δ13 + 3δ12 δ2 + 3δ12 δ3 + 3δ1 δ22 + 6δ1 δ2 δ3 + 3δ1 δ32
                           + δ23 + 3δ22 δ3 + 3δ2 δ32 + δ33 ,

which leads to the Taylor series up to cubic terms:

     f (y0 + δ1 + δ2 + δ3 ) = f + f  δ1 + f  δ2 + f  δ3
                             + 12 f  δ12 + f  δ1 δ2 + f  δ1 δ3 + 12 f  δ22 + f  δ2 δ3 + 12 f  δ32
                             + 16 f (3) δ13 + 12 f (3) δ12 δ2 + 12 f (3) δ12 δ3
                             + 12 f (3) δ1 δ22 + f (3) δ1 δ2 δ3 + 12 f (3) δ1 δ32
                             + 16 f (3) δ23 + 12 f (3) δ22 δ3 + 12 f (3) δ2 δ32 + 16 f (3) δ33 .



Arbitrary number of terms
In a more general case, where there is an arbitrary number of terms in the sum
δ = ∑ δi , the coefﬁcient of f (n) δ1k1 δ2k2 · · · is equal to

                                      1          1      n         
                                               =                     ,                                   (3.3 e)
                                 k1 !k2 ! · · · n! k1 , k2 , . . .
in accordance with the multinomial theorem.
110                                                                       3 B-series and algebraic analysis

    Introduce the set N of sequences of non-negative integers, such that for n ∈ N, ni
is eventually zero. Conventionally, an inﬁnite sequence of zeros can be omitted from
the notation. For given n, n! := ∏∞i=1 ni !. We are now in a position to write Taylor’s
theorem in the convenient form

 Theorem 3.3A
                                                          1 (|n|) n
                                    f (y 0 + δ ) = ∑         f δ .                                (3.3 f)
                                                      n∈N n!

Proof. This is a consequence of the remarks following (3.3 e).
The set N is denumerable; that is, it is possible to write its members in a sequence. A
convenient way of doing this is to list the members in increasing values of ∑∞  i=0 i ni !,
with equally ranked items sorted arbitrarily amongst themselves. This would give a
sequence beginning with the members

   [0, 0, 0, 0, 0], [1, 0, 0, 0, 0], [2, 0, 0, 0, 0], [0, 1, 0, 0, 0], [3, 0, 0, 0, 0], [1, 1, 0, 0, 0],
   [0, 0, 1, 0, 0], [4, 0, 0, 0, 0], [2, 1, 0, 0, 0], [1, 0, 1, 0, 0], [0, 0, 0, 1, 0]

and the sum in (3.3 f) becomes

                     f + f  δ1 + 12 f  δ12 + f  δ2 + 16 f (3) δ13 + f  δ1 δ2 + f  δ3
                          f δ1 + 12 f (3) δ12 δ2 + f  δ1 δ3 + f  δ4 + · · ·
                       1 (4) 4
                     + 24




3.4 Elementary differentials and B-series
B-series to order 4

We will reconsider the questions in the easy question set, not for a particular one-
dimensional problem, but for a general N-dimensional autonomous problem of the
form
                            dy
                                = f (y), y(x0 ) = y0 .
                            dx
   Of the given problems, the ﬁrst which might create difﬁculties is implicit h and we
will attempt to solve this by iteration, starting from y1 ≈ y0 . The next iteration will be
                                                          
                               y1 ≈ y0 + h f y0 + h f (y0 ) .

To explore this approximation in greater detail we need to remind ourselves of
the nature of f (y0 + δ ), where δ = h f (y0 ). We will adopt a notation f := f (y0 ).
Assuming f is analytic, Taylor’s theorem can be used to obtain

         y0 + h f (y0 + hf) = y0 + hf + h2 f  f + 12 h3 f  f 2 + 16 h4 f (3) f 3 + O(h5 ).
3.4 Elementary differentials and B-series                                                                             111

For a second iteration, use Theorem 3.2C with

                                                 δ1 = hf,
                                                 δ2 = h2 f  f,
                                                 δ3 = 12 h3 f  f 2 .

Note that, because we are developing the series only to h4 terms, the term 16 h4 f (3) f 3
is omitted from h f (y0 + hf + h2 f  f + 12 h3 f  f 2 + 16 h4 f (3) f 3 ) in the second iteration.
For the same reason, we need only a limited number of n terms; these are
                                         ⎧
                                         ⎪
                                         ⎪     [0, 0, 0],
                                         ⎪
                                         ⎪
                                         ⎪
                                         ⎪
                                         ⎪
                                         ⎪     [1, 0, 0],
                                         ⎪
                                         ⎪
                                         ⎪
                                         ⎪ [2, 0, 0],
                                         ⎪
                                         ⎨
                                  n=           [3, 0, 0],
                                         ⎪
                                         ⎪
                                         ⎪
                                         ⎪
                                         ⎪
                                         ⎪     [0, 1, 0],
                                         ⎪
                                         ⎪
                                         ⎪
                                         ⎪     [1, 1, 0],
                                         ⎪
                                         ⎪
                                         ⎪
                                         ⎩ [0, 0, 1],

and the second iteration gives the series

   y0 + h f (y0 + hf + h2 f  f + 12 h3 f  f 2 )
         [0, 0, 0]   [1, 0, 0]   [2, 0, 0]       [3, 0, 0]       [0, 1, 0]      [1, 1, 0]        [0, 0, 1]
= y0 + hf + h2 f  f + 12 h3 f  f 2 + 16 h4 f (3) f 3 + h3 f  f  f + h4 f  ff  f + 12 h4 f  f  f 2 +O(h5 ),

with the corresponding components of n written above the terms.
    A ﬁnal iteration adds one more term h4 f  f  f  f leading to the ﬁnal approximation
to this order; which we show with the terms arranged in a more natural order.

   y1 = y0 + hf + h2 f  f + 12 h3 f  f 2 + h3 f  f  f
                                    + 16 h4 f (3) f 3 + h4 f  ff  f + 12 h4 f  f  f 2 + h4 f  f  f  f + O(h5 ).

We can write the Taylor series for all members of the easy question set, up to order 4,
in a similar way to implicit h with only the numerical coefﬁcients in the various terms
altered.
   For mid -point h the result can be derived by replacing h by h/2 in the series for
implicit h to obtain a series for the solution to
                                                                   
                                             y = y0 + h f y0 + 12 hy ,

and then writing y = 2y − y0 for the series for mid -point h . For ﬂow h the quoted
result can be checked by ﬁrst evaluating ﬂow-slope and comparing this with the h
derivative of ﬂow . For Runge-I h and Runge-II h the calculations are straightforward.
The results we have already found for implicit h will be used as a base and the other
112                                                                             3 B-series and algebraic analysis


                Table 12 Coefﬁcients for the easy question set, up to h4 terms


  Problem           y0    hf   h2 f  f   1 3  2
                                          2h f f     h3 f  f  f   1 4 (3) 3
                                                                    6h f f      h4 f  ff  f   1 4   2
                                                                                                 2h f f f       h4 f  f  f  f

  id h               1    0       0          0           0             0              0              0                0
  slopeh             0    1       0          0           0             0              0              0                0
  Euler h            1    1       0          0           0             0              0              0                0
  implicit h         1    1       1          1           1             1              1              1                1
                                  1          1            1            1              1              1                 1
  mid-point h        1    1       2          2            2            2              2              2                 2
                                  1          1            1            1              1             1                  1
  ﬂow h              1    1       2          3            6            4              8             12                24
                                                          1                           1              1                 1
  ﬂow-slopeh         0    1       1          1            2            1              2              3                 6
                                  1          1                         1
  Runge-I h          1    1       2          2           0             2              0              0                0
                                  1          1                         1
  Runge-II h         1    1       2          4           0             8              0              0                0



coefﬁcients will be written in terms of these. This information will be presented in
Table 12.

Elementary differentials

The limited results we have considered up to order 4 suggest a general form for the
terms appearing in the Taylor expansions for arbitrary orders.

 Deﬁnition 3.4A The elementary differential associated with a tree t = [t1 t 2 · · · t n ]
 is
                       F(t) = f (n) F(t1 )F(t 2 ) · · · F(t n ),              (3.4 a)
 with F(τ) = f.

Note that t can be written as τn t 1 t2 · · · t n , closely matching the structure of (3.4 a).
   To see how (3.4 a) is used, evaluate the elementary differentials up to order 4 in
detail:
            F(t1 )            = f,
            F(t2 ) = F([t1 ]) = f  F(t1 )                    = f  f,
               F(t3 ) = F([t21 ]) = f  F(t1 )F(t1 )                      = f  ff,
               F(t4 ) = F([t2 ]) = f  F(t2 )                              = f  (f  f) = f  f  f,
               F(t5 ) = F([t31 ]) = f (3) F(t1 )F(t1 )F(t1 ) = f (3) fff,
               F(t6 ) = F([t1 t2 ]) = f  F(t1 )F(t2 )      = f  f(f  f) = f  ff  f,
               F(t7 ) = F([t3 ]) = f  F(t3 )                              = f  (f  ff) = f  f  ff,
               F(t8 ) = F([t4 ]) = f  F(t4 )                              = f  (f  f  f) = f  f  f  f.
3.4 Elementary differentials and B-series                                                           113

Elementary differentials up to order 6 are shown, for reference, in Table 13 (p. 114).


B-series


B-series are a formalism for expressing the Taylor series for the solution to questions
written in terms of the triple (y0 , h, f ). They are always written in terms of elementary
differentials which in turn are indexed by the trees introduced in Chapter 2 (p. 39).
   We will now look at some possible choices of coefﬁcients. Let B∗ denote the set
of all mappings from ∅ ∪ T to R. Also write B0 for the linear subspace of B∗ such
that if a ∈ B0 then a(∅) = 0. Similarly write B for the subset of B∗ such that if a ∈ B
then a(∅) = 1. Before we consider these properties systematically, we will look at
some examples.
   Given a ∈ B∗ , we want to use series covered by


 Deﬁnition 3.4B The B-series (Bh y0 )a is a formal series deﬁned by

                                                       h|t| a(t)
                            (Bh y0 )a = a(∅)y0 + ∑               F(t).                         (3.4 b)
                                                    t∈T σ (t)


Recall the use of the symmetry σ (t) in (3.4 b), deﬁned in Deﬁnition 2.5A (p. 58).
    series for which a ∈ B have a special role because, for such a series,
Those
h f (Bh y0 )a also exists as a B-series. These correspond to the central mappings
referred to in Deﬁnition 3.2B (p. 102).


 Deﬁnition 3.4C A B-series (Bh y0 )a is said to be a central series if a ∈ B.




B-series as a formal inner product

The sequence of elementary differentials, together with the value of the base point,
usually y0 , when suitably scaled by h|t| /σ (t), can be written as an inﬁnite row vector

                                                                                          
       Bh y0 =       y0 hf h2 f  f   1 3 
                                      2 h f ff   h3 f  f  f · · · σ (t)−1 h|t| F(t) · · · .

Similarly, the B-series coefﬁcients are conveniently written as an inﬁnite column
vector
                                                                                     T
           a=        a(∅)    a(t1 )   a(t1 )   a(t3 )   a(t4 )   ···     a(t)   ···        .
114                                                          3 B-series and algebraic analysis


                  Table 13 Elementary differentials to order 6

      |t|    t                F(t)               |t|    t                       F(t)

      1     t1                f                  6     t18                      f (5) f 5

                                                 6     t19                      f (4) f 3 f  f

      2     t2                f f               6     t20                      f (3) f 2 f  f 2

                                                 6     t21                      f (3) f 2 f  f  f

      3     t3                f  f 2           6     t22                      f 3) ff  ff  f

      3     t4                f f f            6     t23                      f  ff (3) f 3

                                                 6     t24                      f  ff  ff  f

      4     t5                f (3) f 3          6     t25                      f  ff  f  f 2

      4     t6                f  ff  f        6     t26                      f  ff  f  f  f

      4     t7                f  f  f 2       6     t27                      f  f  ff  f 2

      4     t8                f f f f         6     t28                      f  f  ff  f  f

                                                 6     t29                      f  f (4) f 4

      5     t9                f (4) f 4          6     t30                      f  f (3) f 2 f  f

      5     t10               f (3) f 2 f  f    6     t31                      f  f  ff  f 2

      5     t11               f  ff  f 2     6     t32                      f  f  ff  f  f

      5     t12               f  ff  f  f    6     t33                      f  f  f  ff  f

      5     t13               f  f  ff  f    6     t34                      f  f  f  f (3) f 3

      5     t14               f  f (3) f 3      6     t35                      f  f  f  ff  f

      5     t15               f  f  ff  f    6     t36                      f  f  f  f  f 2

      5     t16               f  f  f  f 2   6     t37                      f f f f f f

      5     t17               f f f f f
3.4 Elementary differentials and B-series                                                          115

Up to trees of order 3, these can be written in the form
                                                                                    ⎡          ⎤
                                                                                      a(∅)
                                                                                    ⎢      ⎥
                                                                              ···   ⎢ a( ) ⎥.
                                                   1 3
    (Bh y0 )a =     y0      hF( )    h2 F( )       2 h F(   )     h3 F
                                                                                    ⎢      ⎥
                                                                                    ⎢      ⎥
                                                                                    ⎢ a( ) ⎥
                                                                                    ⎢      ⎥
                                                                                    ⎢ a( ) ⎥
                                                                                    ⎢      ⎥
                                                                                    ⎢  ⎥
                                                                                    ⎢ a    ⎥
                                                                                    ⎢      ⎥
                                                                                    ⎣      ⎦
                                                                                        ..
                                                                                         .




B-series for the easy question set

Our aim will now be to extend the results given in Table 12 (p. 112) to all trees so
that we know the exact B-series for all members of the easy question set.


id h , slopeh and Euler h
We can immediately write down the B-series for these three questions

 Theorem 3.4D The B-series coefﬁcients for the questions referred to are:

                     id h : a(∅) = 1, a(t) = 0,             t ∈ T,
                  slopeh : a(∅) = 0, a(τ) = 1, a(t) = 0, |t| > 1,
                  Euler h : a(∅) = 1, a(τ) = 1, a(t) = 0, |t| > 1.




Some special members of B∗ : E ∈ B

Write E for the B-series for for unit step h. That is

                                     E(∅) = 1,
                                            1
                                      E(t) = ,              t ∈ T.
                                            t!
For the ﬂow through a distance θ h, we write

                                E(θ ) (∅) = 1,
                                                  θ |t|
                                    E(θ ) (t) =         ,       t ∈ T.
                                                   t!
116                                                                          3 B-series and algebraic analysis

We can also write Eθ = E(θ ) .


Some special members of B∗ : D ∈ B0

Scaled differentiation to produce h f (y0 ) = hF(τ)(y0 ), that is slope , is given by

                                         D(∅) = 0,
                                          D(τ) = 1,
                                           D(t) = 0,           |t| > 1.

In Section 3.9 (p. 133) we will introduce compositions of B-series. We will give a
single example here. If a ∈ B, then the central
                                               series
                                                     (Bh y0 )a can be substituted for y0
in (Bh y0 )D to give an expression for h f (Bh y0 )a . This expression can be expanded
as a B-series in its own right, which will be written as (Bh y0 )aD. The notation aD
will later be seen to be part of the general and self-consistent terminology introduced
in Section 3.9.

 Theorem 3.4E If a ∈ B, then

                         (aD)(∅) = 0,                                                               (3.4 c)
                          (aD)(τ) = 1,                                                              (3.4 d)
                                               m
                           (aD)(t) = ∏ a(ti ),                  t = [t 1 t 2 · · · t m ].            (3.4 e)
                                              i=1


Proof. Use Theorem 3.3A with δ = B(a, y0 , h) − y0 . First verify (3.4 c,3.4 d), from
                                  
                      h f y0 + O(h) = h f (y0 ) + O(h2 ).

To prove (3.4 e) with

                        t = [t k11 tk22 · · · t kmm ],    t 1 , t 2 , . . . , t m distinct,

write n = [k1 , k2 , . . . , km ]. The n term in (3.3 f) becomes
              m
                    a(ti )ki                                                                  h|t|
  h1+∑ ki |ti | ∏                  f (∑ ki |ti |) F(t1 )k1 F(t2 )k2 · · · F(t m )km = (aD)(t)       F(t).
             i=1 (ki )!σ (t i )
                                ki                                                            σ (t)

Written another way, Theorem 3.4E states that
                               
                   h f (Bh y0 )a = ∑ h|t| σ (t)−1 (aD)(t)F(t).                                         (3.4 f)
                                                    t∈T

   The following result is proved in the same way as Theorem 3.4E and expressed as
a generalization of (3.4 f)
3.5 B-series for ﬂow h and implicit h                                                                        117



 Corollary 3.4F For n = 1, 2, 3, . . . ,
                              
             hn f (n) (Bh y0 )a = ∑ h|t|+n−1 σ (t)−1 (aD)(t)F(t∗n−1 ).
                                                     t∈T


Exercise 35 Show that
                                                                       |t|
                                                      (ED)(t) =            .
                                                                       t!

3.5 B-series for ﬂow h and implicit h

B-series based on repeated differentiation

Recall the discussion on partitions introduced in Chapter 2, Section 2.6 (p. 65). Our
aim in this section is to derive the B-series coefﬁcients for ﬂow h and implicit h based
on repeated differentiation, starting from the two equation systems

                                     y (x) = f (y(x)),      y(x0 ) = y0 ,                                (3.5 a)
                                                                   
                                      y(x) = y0 + (x − x0 ) f y(x) ,                                      (3.5 b)

respectively, and making use of partitions.


B-series for ﬂow h
Starting from the initial value problem (3.5 a) further differentiations give in turn
                                   
                 y (x) = f  y(x) y (x),
                                                         
               y(3) (x) = f  y(x) y (x)y (x)+ f  y(x) y (x),
                                                                               (3.5 c)
               y(4) (x) = f (3) y(x) y (x)y (x)y (x)
                                                                
                              + 3 f  y(x) y (x)y (x)+ f  y(x) y(3) (x).

Evaluating the Taylor coefﬁcients at x = x0 , we ﬁnd the equations given in (3.5 d),
where we have included also y (5)

           y  = f,
           y  = f  y  ,
       y (3) = f  y  y  + f  y  ,                                                                  (3.5 d)
                      (3)                           (3)
       y   (4)
                 =f      y y y + 3f y y + f y                      ,
                      (4)                (3)   
       y   (5)
                 =f      y y y y + 6f           y y y + 4f  y  y (3) + 3f  y  y  + f  y (4) .

   It is not necessary to refer back to (3.5 c) to go from one line to the next. It is
sufﬁcient to note that each term in (3.5 d) corresponds to a speciﬁc term in (3.5 c)
118                                                                                              3 B-series and algebraic analysis

which is differentiated using the product rule and mapped back to speciﬁc terms with
h = 0. This is illustrated below for the two types of factor.

           f (m) (y(x))               f (m+1) (y(x))y (x)                               y(n) (x)                 y(n+1) (x)




               f (m)                           f (m+1) y                                   y (n)                    y (n+1)

We will illustrate the derivation of y (4) from y (3) in (3.5 d).
                                                                                                             
        f  y  y  + f  y  → f (3) y  y  y  + f  y  y  + f  y  y  + f  y  y  + f  y (3)
                                = f (3) y  y  y  + 3f  y  y  + f  y (3) .


Exercise 36 Verify the formula given for y (5) in (3.5 d).




Comparison with tree evolution

Now compare the derivation in (3.5 d), shown in Figure 9, with the evolution of
partitions shown in Figure 2.6 (p. 71). In Figure 9 every node in column number


                                                                                                              f (4) y  y  y  y 
                                                                             f (3) y  y  y 
                                       f  y  y                                                             f (3) y  y  y 

          f                                                                    f  y  y                      f  y  y 3)

                                         f  y                                                                  f  y  y 
                                                                                f  y (3)
                                                                                                                   f  y (4)


              Figure 9 Succesive differentiation of f (y(x)) with evaluation at x = x0




n = 1, 2, . . . is labelled with a term of the form

                                                    f (m) y (k1 ) y (k2 ) · · · y (km ) ,

where ∑m
       i=1 ki = n. This term evolves to
3.5 B-series for ﬂow h and implicit h                                                                                       119

f (m+1) y  y (k1 ) y (k2 ) · · · y (km ) + f (m) y (k1+1) y (k2 ) · · · y (km ) + · · · + f (m) y (k1 ) y (k2 ) · · · y (km+1) .

In exactly the same way, the partition k1 + k2 + · · · + km evolves to the sum of the
m + 1 partitions
                     1 + k1      + k2       + · · · + km ,
                                       (k1 + 1) + k2                    + · · · + km ,
                                         k1            +(k2 + 1) + · · · + km ,
                                                         ..
                                                          .
                                         k1            + k2      + · · · +(km + 1).

Deﬁne
                            z(p)(x) = f (card(p)) (y(x))y(p1 ) (x)y(p2 ) (x) · · · y(pcard(p) ) (x),
             z(p) := z(p)(x0 ) = f (card(p)) y (p1 ) y (p2 ) · · · y (pcard(p) ) .

This notation can be generalized to a linear combination of partitions
                                         j                        j
                                           ∑ Ci pi (x) = ∑ Ci z( pi )(x),
                                           i=1                    i=1
                                                j                 j
                                           z     ∑ Ci pi = ∑ Ci z( pi ),
                                                 i=1              i=1

and, in a special case,
                                          d                         
                                         d x z(p)(x) = z evolve(p)(x) .

This means that each term corresponds to a partition of n which can be written

                                  z(p) := f (card(p)) y (p1 ) y (p2 ) · · · y (pcard(p) ) .

For convenience in this section, deﬁne

                     z(p) (x) = f (card(p)) (y(x))y(p1 ) (x)y(p2 ) (x) · · · y(pcard(p) ) (x),
                         z (p) = z(p) (x0 ),
                                = f (card(p)) y (p1 ) y (p2 ) · · · y (pcard(p) ) .

With this notation, the formulae for y (i) , for i = 1, 2, 3, 4, are

                                                 y (1) = z () ,
                                                 y (2) = z (1) ,
                                                                                                                       (3.5 e)
                                                 y (3) = z (2) + z (1+1) ,
                                                 y (4) = z (3) + 3z (1+2) + z (1+1+1) ,
120                                                                                     3 B-series and algebraic analysis

or, in terms of partitions of a set using the function φ , which maps partitions of a set
to partitions of a number,

       y (1) = z (φ ()) ,
       y (2) = z (φ (1)) ,
       y (3) = z (φ (12)) + z (φ (1+2)) ,                                                                         (3.5 f)
       y   (4)
                 =z   (φ (123))
                                  +z   (φ (1+23))
                                                    +z   (φ (2+13))
                                                                      +z   (φ (3+12))
                                                                                        +z   (φ (1+2+3))
                                                                                                           ,
       y (4) = z (φ (123)) + z (φ (1+23)) + z (φ (2+13)) + z (φ (3+12)) + z (φ (1+2+3)) .



Informal explanation of (3.5 e) and (3.5 f)

A partition in P(n) is written as a sum of positive integer with total n. For n = 3 as
in the ﬁnal line of (3.5 e), the partitions are 3, 1+2 and 1+1+1, and correspond to
f  y (3) , f  y (1) y (2) , and f  y (1) y (1) y (1) , respectively. The ﬁve partitions in P[{1, 2, 3}]
are 123, a compact expression for {1, 2, 3}, and four further partitions. The way φ
interrelates the number and set partitions in (3.5 e) and (3.5 f) is shown in (3.5 g).

                                                          φ (1) = 1,
                                                         φ (12) = 2,
                                                    φ (1 + 1) = 1 + 1,
                                                   φ (123) = 3,
                                                        ⎫                                                        (3.5 g)
                                              φ (1 + 23)⎪
                                                        ⎬
                                              φ (2 + 13) = 1 + 2,
                                                        ⎪
                                                        ⎭
                                              φ (3 + 12)
                                              φ (1 + 2 + 3) = 1 + 1 + 1.

In the case of the empty set, corresponding to n = 0, there are no partitions, but for
the current application, an empty partition with zero cardinality is conventionally
used to represent y (1) = z (φ ()) as z(x0 ) = f.


Combining partitions

It is convenient to combine the partitions of n in the formula for y(n) into a single
term. Let                                       
                                 Z(x) = f y(x) ,
so that
                                   Z (n) (x) =           ∑ p-weight(p) z(p) (x)
                                                     p∈P(n)

with
3.5 B-series for ﬂow h and implicit h                                                       121

               Z (n) =     ∑ p-weight(p) z(p)
                         p∈P(n)

                     =     ∑ p-weight(p)f(card(p)) y(p1 ) y(p2 ) · · · y(pcard(p) ) .    (3.5 h)
                         p∈P(n)



B-series for implicit h

Starting from (3.5 b), and carrying out further differentiations, we havet

                               y (x) = Z(x) + (x − x0 )Z  (x),
                               y (x) = 2Z  (x) + (x − x0 )Z  (x),
                              y(3)(x) = 3Z  (x) + (x − x0 )Z (3) (x).

Substituting x = x0 , we obtain
                              y  = Z = f,
                             y  = 2Z  = 2f  y  ,
                                                                   
                           y (3) = 3Z  = 3 f  y  y  + f  y  .

This suggests a general result which we now present.


 Theorem 3.5A The Taylor coefﬁcients for implicit h are

                               y (n) = nZ (n−1) .        n = 1, 2, . . .                (3.5 i)

Proof. Write (3.5 b) in the form

                                    y(x) = y0 + (x − x0 )Z(x),

leading to
                                  y (x) = Z(x) + (x − x0 )Z  (x).
This is the case n = 1 of

                  y(n) (x) = nZ (n−1) (x) + (x − x0 )Z (n) (x),       n = 1, 2, . . .

For n > 1, use induction
                                                                       
               y(n) (x) = ddx (n − 1)Z (n−2) (x) + (x − x0 )Z (n−1) (x)
                                                                               
                        = (n − 1)Z (n−1) (x) + (x − x0 )Z (n) (x) + Z (n−1) (x)
                         = nZ (n−1) (x) + (x − x0 )Z (n) (x).

Substituting x = x0 , (3.5 i) follows.
122                                                                3 B-series and algebraic analysis

Taylor series written as B-series


B-series for ﬂow h

Starting from y  = f = F(t1 ), we can in turn derive formulae for y  and y (3) and
continue. In this successive derivation, the details are omitted for y (4) and y (5) .

       y  = f = F(t1 ),
       y  = f  F(t1 ) = F([t 1 ]) = F(t2 ),
      y (3) = f  F(t1 )F(t1 ) + f  F(t2 ) = F([t21 ]) + F([t2 ]) = F(t 3 ) + F(t4 ),
                                                                                             (3.5 j)
      y (4) = F(t5 ) + 3F(t6 ) + F(t7 ) + F(t8 ),
      y (5) = F(t9 ) + 6F(t10 ) + 4F(t11 ) + 3F(t12 ) + 4F(t13 )
                + F(t14 ) + 3F(t15 ) + F(t16 ) + F(t17 ).

Referring back to Chapter 2 Section 2.5 (p. 58) and the function α introduced there,
with value given in Theorem 2.5F (p. 61), we observe that, as far as we have carried
out the calculations, the coefﬁcient of F(t) in the expression for y (|t|) is identical
with α(t). This observation is true in general.


 Lemma 3.5B For ﬂow h ,
                                       y (n) = ∑ α(t)F(t).
                                             |t|=n



Note the close relationship between this result and Theorem 2.6F (p. 73).
Proof. Use induction starting with n = 1, for which the result holds. To ﬁnd the
coefﬁcient of F(t) in y (n) , where t = [t1 t 2 · · · t m ], and |t| = n and ∑m
                                                                              i=1 ni = n − 1,
where ni = |ti |, assume that the result is true for lower orders so that the coefﬁcient
of F(ti ) in y (|ti |) is α(ti ). The term F(t) appears in the term of (3.5 h) for which
p = ∑i=1 |t i |. The required coefﬁcient is
                                  m
                                                   (n − 1)!      m
                                                                      ni !
                 p-weight(p) ∏ α(ti ) =                          ∏
                                 i=1             ∏i=1 ni !(i!) i=1 σ (ti )t i !
                                                  m           ni

                                                                n!
                                           =                                                (3.5 k)
                                                   i=1 (i!) σ (t i ))(n ∏i=1 t i !)
                                                 (∏m       ni            m

                                               n!
                                           =
                                             σ (t)t!
                                           = α(t).


We can now rewrite the B-series coefﬁcients
3.5 B-series for ﬂow h and implicit h                                                        123

 Theorem 3.5C The B-series for ﬂow h is given by (Bh y0 )T a, where

                                        a(∅) = 1,
                                                1
                                         a(t) = t! ,     t ∈ T.

Proof. From Lemma 3.5B the coefﬁcient of F(t)/|t|! is α(t)/|t|! = 1/σ (t)t!.


B-series for implicit h
In a similar way to the ﬂow, we start with y  = f = F(t1 ) and carry out similar
calculations for y  and y (3) , showing details, and state the results, without details of
the derivation, for y (4) and y (5) .


   y  = f = F(t1 ),
   y  = 2f  F(t1 ) = 2F([t 1 ]) = 2F(t2 ),
 y (3) = 3f  F(t1 )F(t1 ) + 3f  F(t2 ) = 3F([t21 ]) + 6F([t2 ]) = 3F(t3 ) + 6F(t4 ),
                                                                                          (3.5 l)
 y (4) = 4F(t5 ) + 24F(t6 ) + 12F(t7 ) + 24F(t8 ),
 y (5) = 5F(t9 ) + 60F(t10 ) + 60F(t11 ) + 120F(t12 ) + 60F(t13 )
           + 20F(t14 ) + 120F(t15 ) + 60F(t16 ) + 120F(t17 ).

The examples in (3.5 l) give results identical to those in (3.5 j) except that for each
tree t, α(t) is replaced by β (t). The general result holds in the form

 Lemma 3.5D For implicit h ,
                                        y (n) = ∑ β (t)F(t).
                                              |t|=n


Proof. The proof is as for Theorem 3.5B except for the consequences of the factor n
in (3.5 i). The manipulations in (3.5 k) are replaced by
                                        m
                                                        n(n − 1)! m ni !
                     n p-weight(p) ∏ β (t i ) =                     ni ∏
                                        i=1             i=1 ni !(i!) i=1 σ (t i )
                                                       ∏m
                                                             n!
                                                 =             ni σ (t ))
                                                   (∏m i=1 (i!)       i
                                                     n!
                                                 =
                                                   σ (t)
                                                 = β (t).
124                                                               3 B-series and algebraic analysis



 Theorem 3.5E The B-series for implicit h is given by (Bh y0 )T a, where

                                  a(∅) = 1,
                                   a(t) = 1,       t ∈ T.

Proof. From Lemma 3.5D the coefﬁcient of F(t)/|t|!, is β t)/|t|! = 1/σ (t).


B-series for implicit h by antipode
We introduce an alternative approach to ﬁnding the result of Theorem 3.5E.
  Reversing y0 and y1 in
                                y1 = y0 + h f (y1 ),
we ﬁnd the Euler method with negative stepsize

                                       0
                                                    .
                                             −1

Hence, the B-series for implicit h is a−1 , where

                               a(∅) = 1,
                                a(τ) = −1,
                                a(t) = 0.              |t| ≥ 2.

To ﬁnd the antipode, use Deﬁnition 2.9B. For any particular tree t, the only partition
that contributes to the antipode is the complete partition into single vertices, because
a(t ) = 0 when |t  | > 1. Hence,
                                             |t|
                              a−1 (t) = − a(τ) = 1.




3.6 Elementary weights and the order of Runge–Kutta methods

To express the B-series of a Runge–Kutta method we need to use “elementary
weights”. Consider a generic method

                                        c      A
                                                   .                                       (3.6 a)
                                             bT
3.6 Elementary weights and the order of Runge–Kutta methods                                             125



 Deﬁnition 3.6A The elementary weight Φ(t) and the stage weight
 varphii (t) (i = 1, 2, . . . , s) of (3.6 a) corresponding to t are

                                     ϕi (τ) = ci ,
                                     Φ(τ) = bT 1,
                                                     s         m
                         ϕi ([t1 t 2 · · · t m ]) = ∑ ai j ∏ ϕ j (t k ), ,                         (3.6 b)
                                                 j=1           k=1
                                                  s            m
                         Φ([t 1 t 2 · · · t m ]) = ∑ b j ∏ ϕ j (t k ).
                                                 j=1        k=1


It is convenient to expand the recursions in 3.6A, as in the example

                                                          s
              t=       m k                Φ(t) =          ∑           bi ain aim ai j a jk ak .
                     n   j
                                                     i, j,k,,m,n=1
                         i

Because the summations over the leaves of the tree can all be carried out explicitly,
to give ∑n ain = ∑m aim = ci , ∑ ak = ck , we can also write
                                             s
                               Φ(t) =       ∑ bi c2i ai j a jk ck .
                                          i, j,k=1

It is also convenient to write the values of Φ in a combination of matrix-vector
notation and component-by-component products. Thus we can write

                                    Φ(t) = bT (c2 A2 c).

In this notation, pointwise powers of vectors have ﬁrst priority, conformable vector
and matrix multiplications have second priority and pointwise vector products have
third priority. Parentheses are used where necessary to overrule these priorities.
   The values of Φ(t), in this compact notation are given in Table 14 up to order 6.
   In the following theorem, the meaning of elementary weights is extended for the
sake of convenience by deﬁning

                                   ϕi (∅) = Φ(∅) = 1.



 Theorem 3.6B The B-series for stage i of (3.6 a) is (Bh ϕi )y0 . The B-series for the
 output of (3.6 a) is (Bh Φ)y0 .

Proof. The stages Yi and output y1 are deﬁned by
126                                                                           3 B-series and algebraic analysis


                           Table 14 Elementary weights to order 6
 |t|    t                          Φ(t)             |t|    t                                       Φ(t)
  1    t1       τ                     bT 1           6    t18       [τ 5 ]                         bT c5
                                                     6    t19    [τ 3 [τ]]                      bT (c3 Ac)
  2    t2      [τ]                    bc  T
                                                     6    t20    [τ 2 [τ 2 ]]                   bT (c2 Ac2 )
                                                     6    t21 [τ 2 [2 τ]2 ]                     bT (c2 A2 c)
  3    t3     [τ 2 ]                  bT c2          6    t22    [τ[τ]2 ]                      bT (c(Ac)2 )
  3    t4     [2 τ]2                  bT Ac          6    t23    [τ[τ 3 ]]                      bT (cAc3 )
                                                     6    t24 [τ[τ[τ]]]                        bT (cA(cAc))
  4    t5     [τ 3 ]                  b c3T
                                                     6    t25   [τ[2 τ 2 ]2 ]                   bT (cA2 c2 )
  4    t6    [τ[τ]]              bT (cAc)            6    t26    [τ[3 τ]3 ]                     bT (cA3 c)
  4    t7    [2 τ 2 ]2            bT Ac2             6    t27 [[τ][τ 2 ]]                       bT (AcAc2 )
  4    t8     [3 τ]3              bT A2 c            6    t28 [[τ][2 τ]2 ]                      bT (AcA2 c)
                                                     6    t29 [τ[2 τ 2 ]2 ]                       bT Ac4
  5    t9     [τ 4 ]                  bT c4          6    t30    [τ[3 τ]3 ]                     bT A(c2 Ac)
  5    t10   [τ 2 [τ]]           b (c2 Ac)
                                  T
                                                     6    t31    [[τ][τ 2 ]]                    bT A(cAc2 )
  5    t11 [τ[τ 2 ]]             bT (cAc2 )          6    t32 [[τ][2 τ]2 ]                      bT A(cA2 c)
  5    t12 [τ[2 τ]2 ]            bT (cA2 c)          6    t33    [2 [τ]2 ]2                     bT A(Ac)2
  5    t13   [[τ]2 ]             bT (Ac)2            6    t34     [3 τ 3 ]3                      bT A2 c3
  5    t14   [2 τ 3 ]2            bT Ac3             6    t35    [3 τ[τ]]3                      bT A2 (cAc)
  5    t15 [2 τ[τ]]2             bT A(cAc)           6    t36     [4 τ 2 ]4                      bT A3 c2
  5    t16   [3 τ 2 ]3            b A2 c2
                                      T
                                                     6    t37      [5 τ]5                         bT A4 c
  5    t17    [4 τ]4              bT A3 c



                                              s
                         Yi = y0 + h ∑ ai j f (Y j ),           i = 1, 2, . . . , s,
                                              j=1
                                               s
                         y1 = y0 + h ∑ b j f (Y j ).
                                              j=1

Let ηi ∈ B and ξ ∈ B represent Yi and y1 resepctively, so that
                                                    s
                                      ηi = 1 + ∑ ai j (η j D),
                                                    j=1
                                                     s
                                      ξ = 1 + ∑ b j (η j D).
                                                    j=1
3.7 Elementary differentials based on Kronecker products                                       127

For t = [t 1 t2 · · · t m ], i = 1, 2, . . . , s, use (ηi D)(t) = ∏m
                                                                   k=1 ηi (t k ), so that

                                           ηi (τ) = ci ,
                                                        s
                                            ξ (τ) = ∑ bi = bT 1,
                                                       i=1
                                                         s       m
                                                                                            (3.6 c)
                               ηi ([t1 t 2 · · · t m ]) = ∑ ai j ∏ η j (t k ),
                                                       j=1     k=1
                                                        s      m
                               ξ ([t 1 t 2 · · · t m ]) = ∑ b j ∏ η j (t k ).
                                                       j=1     k=1

. By comparing (3.6 b) and (3.6 c), we see that, by induction, ηi = ϕi , i = 1, 2, . . . , s,
and ξ = Φ.



 Theorem 3.6C For an initial value problem of arbitrary dimension, a Runge–Kutta
 method (A, bT , c) has order p if and only if
                                                      1
                                               Φ(t) = t!

 for all trees such that |t| ≤ p.




3.7 Elementary differentials based on Kronecker products

Given a Runge–Kutta method M = (A, bT , c), let
                                              
                                     A 0
                             A=                  ,                                          (3.7 a)
                                    bT 0

and write n = s + 1. For a direct approach to ﬁnding the Taylor expansions of the
stages and the output of M, deﬁne the problem (  y0 , f, h). where f : RnN → RnN and
y0 ∈ R are deﬁned by
       nN

                     ⎛⎡ ⎤⎞               ⎛⎡          ⎤⎞               ⎡      ⎤
                        y1                     y1 )
                                             f (                         y0
                     ⎜⎢ ⎥⎟               ⎜⎢          ⎥⎟               ⎢      ⎥
                     ⎜⎢y2 ⎥⎟            ⎜⎢ f ( 2 ⎥⎟                 ⎢ y0 ⎥
                     ⎜⎢ ⎥⎟               ⎜⎢ y )⎥⎟                     ⎢      ⎥
                   ⎜ ⎢     ⎥ ⎟         ⎜
                                  A ⊗ I) ⎜⎢⎢         ⎥ ⎟        y0 = ⎢ . ⎥
           f (
              y) = f ⎜⎢ . ⎥⎟ = (A                    ⎥⎟ ,             ⎢ . ⎥
                     ⎜⎢ .. ⎥⎟            ⎜⎢ ... ⎥⎟                    ⎣ . ⎦
                     ⎝⎣ ⎦⎠               ⎝⎣          ⎦⎠
                         yn
                                             f (n
                                                y )                       y0
128                                                                       3 B-series and algebraic analysis

                                                                            y0 , f, h),
The results we want are embedded in the B-series for implicit h applied to (
That is the series for y1 , given by
                                    y1 = y0 + h f(
                                                     y1 ),     y(x0 ) = y0 .
It will not be necessary to evaluate the partial derivatives, and hence the elementary
differentials, of f at all points in RnN → RnN , but only where
                                         y1 = y2 = · · · = yn = y0 .                             (3.7 b)
Furthermore, it will only be necessary to evaluate f(m) acting only on operands of
the form
                          (φ1 ⊗ δ1 , φ2 ⊗ δ2 , . . . , φm ⊗ δm ) ,
where
                               φi ∈ Rn ,      δi ∈ R N ,        i = 1, 2, . . . , m.
The value of f(m) is found to be
                                A dotm φ1 φ2 · · · φm ) ⊗ (f (m) δ1 δ2 · · · δm ).
                               (A
and this leads to

  Theorem 3.7A The elementary differentials of f, evaluated at 
                                                                y 0 are
                                             = Φ (t) ⊗ F(t),
                                            F(t)
 where
                       '
                           A 1,                                         t = τ,
           Φ (t) =                                                                                 (3.7 c)
                                          Φ (t 2 ) · · · Φ (t m ),
                           A dotm Φ (t 1 )Φ                             t = [t 1 t2 · · · t m ].


Proof. For t = τ, we have
                          = (A
                         F(t) A ⊗ I)(1 ⊗ f) = A 1 ⊗ f = Φ (τ) ⊗ F(τ).

For t = [t 1 t2 · · · t m ],
                                                                                           
       = (A
      F(t) A ⊗ I)(1 ⊗ f (m) ) Φ (t 1 ) ⊗ F(t1 ), Φ (t 2 ) ⊗ F(t2 ), . . . , Φ (tm ) ⊗ F(tm ) ,

with component number i equal to
    n        m
                                                                         Φ (t 2 ) · · · Φ (t m )F(t)
   ∑ a i j ∏ Φ (t j )k f(m) F(t1 )F(t2 ) · · · F(tm ) = ei A dotm Φ (t1 )Φ
                                                                 T


   j=1      k=1

                                                             = (eTi Φ (t))F(t).

We are now in a position to give an alternative proof for Theorem 3.6B (p. 125).
    Write Φ i (t) = ϕi (t), i = 1, 2, . . . , s, Φ s+1 (t) = Φi (t), so that (3.7 c) becomes
3.8 Attainable values of elementary weights and differentials                                      129
                       '
                           A1,                                        t = τ,
              ϕ(t) =
                           A dotm ϕ(t1 )ϕ(t2 ) · · · ϕ(t m ),         t = [t 1 t2 · · · t m ].
                       '
                           bT 1,                                       t = τ,
             Φ(t) =
                           b dotm ϕ(t1 )ϕ(t2 ) · · · ϕ(t m ),
                            T
                                                                       t = [t 1 t 2 · · · t m ].

These give the values of ϕ(t) and Φ(t) in Deﬁnition 3.6A (p. 125) and the result
follows.



3.8 Attainable values of elementary weights and differentials
Elementary weights

Given a ﬁnite set of trees, we ﬁrst consider the question “What values can the
elementary weights take on over this set of trees?” It is assumed that there is no
restriction on the number of stages of the Runge–Kutta methods which generate the
values of these elementary weights.
   The answer to this question is that any values of the elementary weights can
arise. An important consequence is that there is no bound on the attainable order of
Runge–Kutta methods if there is no restriction on the number of stages.
   As a preliminary step we introduce Lemma 3.8A on a class of functions on a ﬁnite
set. This is a trivial case of the Stone–Weierstrass Theorem [81] (Rudin,1976).

 Lemma 3.8A Let I be a ﬁnite set and let F denote the set of real-valued functions
 on I such that
  1. F is a vector space.
  2. F contains the unity function 1(i) = 1.
  3. F is closed under pointwise multiplication.
  4. F distinguishes points.
 Then F contains every function I → R.

Proof. Let I = {1, 2, . . . , n} and let fi , i = 1, 2, . . . , n, be a given vector. A function
f ∈ F will be constructed such that f (i) = fi . For i, j distinct points in I, let ϕi j ∈ F
be chosen such that ϕi j (xi ) = ϕi j (x j ). Deﬁne f by

                                                ϕi j − ϕi j (x j )1
                                   f = ∑ fi ∏                           ,
                                      i∈I   j=i i j (xi ) − ϕi j (x j )
                                               ϕ

where the product is pointwise.

Now use this result to consider the possible values of the elementary differentials
generated by some Runge–Kutta method. Let T denote a ﬁnite subset of the rooted
130                                                                          3 B-series and algebraic analysis

trees and deﬁne F to be the set of vectors of values that can be taken by the elementary
weights evaluated for t ∈ T.

 Theorem 3.8B For any distinct set of trees {t 1 , t 2 , . . . , tN }, and a corresponding
 set of real numbers, {θ1 , θ2 , . . . , θN }, there exists a Runge–Kutta method (A, bT , c)
 such that
                            Φ(t i ) = θi ,         i = 1, 2, . . . , N.

Proof. Assume |ti | ≤ n, i = 1, 2, . . . , N, for some positive integer n. For n = 1, the
result is clear. Proceed by induction for n > 1, and assume the result holds when n is
replaced by a lower positive integer.
   Let F denote the set of possible values of {Φ(t1 ), Φ(t 2 ), . . . , Φ(tN )}. We will
prove that a Runge–Kutta method exists satisfying the assumptions in Lemma 3.8A,
where F denotes the set of possible values of {Φ(t1 ), Φ(t 2 ), . . . , Φ(tN )}, over all
Runge–Kutta methods. We will show that F satisﬁes the conditions of Lemma 3.8A.
1. F is a vector space
   Given tableaux
                                       c       
                                                A                 c     A
                                                    ,                                                 (3.8 a)
                                             
                                             bT                         bT
   with corresponding elementary weight vectors
                             ⎡      ⎤           ⎡   ⎤
                                 ϕ               ϕ
                          =⎣
                         Φ          ⎦,     Φ =⎣     ⎦,
                                Φ                Φ

      then the linear combination CΦ
                                     + CΦ is generated by

                                            c          
                                                        A         0
                                            c           0         A .
                                                    C
                                                      bT         C bT

2. F contains the unity function 1(i) = 1.
   The unity function is generated by

                                                    1        1
                                                                 .
                                                             1

3. F is closed under pointwise multiplication.
   Let I = {1, 2, . . . , s}, I = {1, 2, . . . , s} be the index sets for the tableaux in (3.8 a).
   Consider the tableau formed by Kronecker multiplication on the index set I⊗ I,

                                            c⊗ c           ⊗ A
                                                            A
                                                                        .
                                                            
                                                            bT ⊗ bT
3.8 Attainable values of elementary weights and differentials                                131

   For this tableau,
                 ⊗ A)(
        ϕ(τ) = (A        1 ⊗ 1)
               
             = ϕ(τ) ⊗ ϕ(τ),
                bT ⊗ bT )(
        Φ(τ) = (         1 ⊗ 1)
                 Φ(τ),
             = Φ(τ)
                                                                                        
                 ⊗ A) (dotm ϕ(t
        ϕ(t) = (A                  2 ) · · · ϕ(t
                              1 )ϕ(t          m ) ⊗ (dotm ϕ(t 1 )ϕ(t 2 ) · · · ϕ(t m )
                ⊗ ϕ(t),
             = ϕ(t)
                                                                                           
        Φ(t) = (                1 )ϕ(t
                bT ⊗ bT ) (dotm ϕ(t   2 ) · · · ϕ(t
                                                  m ) ⊗ (dotm ϕ(t 1 )ϕ(t 2 ) · · · ϕ(t m )
                 Φ(t),
              = Φ(t)

                                                               ⊗ Φ(t) = Φ(t)
   where t = [t1 t2 · · · t m ]. Hence, for all t ∈ T, Φ(t) = Φ(t)        Φ(t).

4. F distinguishes points
   In this part of the proof, it is sufﬁcient to assume N = 2. Let t1 = [t k111 · · · ], t2 =
   [t21 m1 · · · ], assumed to be distinct. For a given method (A, bT , c), construct the
   tableau
                                           c    A 0
                                           bT 1     bT      0 ,
                                                    0   T
                                                            1
   so that F contains an arbitrary linear combination of terms like

                                  Φ(t11 )k1 · · ·   Φ(t 21 )m1 · · ·   .

   By the induction hypothesis, Φ(t 11 ), . . . , Φ(t 21 ), . . . can have any values.



Elementary differentials

A similar result on the attainable values of elementary differentials is given in
Theorem 3.8D. This is preceded by an example of the values of elementary differen-
tials in a special case.


Preliminary lemma

 Lemma 3.8C Let                                       
                                  f (y) = A exp diag(y) 1,
 then                                                 
                              f (n) (y) = A exp diag(y) . dotn .                       (3.8 b)
132                                                                    3 B-series and algebraic analysis

Proof. For small δ ,
                                                       
         f (y + δ ) = A exp diag(y)1 + diag(δ )1
                                                               
                     = A exp diag(y) 1 + A exp diag(y) diag(δ )1 + o(δ )
                                                            
                     = f (y) + A diag(δ ) exp diag(y) 1 + o(δ ).
                                              
Hence f  (y)δ = A diag(δ ) exp diag(y) 1. By applying this result n times, we have
                                                                                   
         f (n) (y)δ1 δ2 · · · δn = A diag(δ1 ) diag(δ2 ) · · · diag(δn ) exp diag(y) 1
                                                  
                                 = A exp diag(y) diag(dotn δ1 δ2 · · · δn )1
                                                  
                                 = A exp diag(y) dotn δ1 δ2 · · · δn ,

implying (3.8 b).



Main result



 Theorem 3.8D For any positive integer n, let Tn denote the set {t1 , t 2 , . . . , t N }
 of all trees satisfying |t i | ≤ n, i = 1, 2, . . . , N. Then for any real sequence θi , i =
 1, 2, . . . , N, there exists a ﬁnite-dimensional vector space X = RN , f : X → Xand
 a non-zero vector x ∈ X such that for any t ∈ T n ,

                                            xT F(t i ) = θi .


Proof. Let (A, bT , c) denote a Runge–Kutta method satisfying the requirements of
Theorem 3.8B. Let N = s, and deﬁne A from (3.7 a), f (y) = A exp(diag(y))1 and
y0 = 0 with xT = eTs+1 . From Lemma 3.8C, f (n) (y) = A exp(diag(y)). dotn , and

                               f = A1 = Φ ,             f (n) = A dotn .

Use B+ induction Chapter 2, Section 2.2 (p. 45) to show that F(t) = Φ (t), where
t = [t 1 t2 · · · t n ] starting from t = τ. We have

                    xT F(t) = xT f (n) F(t 1 )F(t 2 ) · · · F(t n )
                            = xT A exp(y0 I) dotn Φ (t 1 )Φ     Φ (t 2 ) · · · Φ (t n )
                            = x dotn (t 1 )Φ (t 2 ) · · · Φ (t n )
                               T
                                 A       Φ        Φ
                             = Φ(t),

and the result follows from Theorem 3.8B.
3.9 Composition of B-series                                                         133

3.9 Composition of B-series
Introducing the composition theorem

The composition of Runge–Kutta methods, or more generally as integration methods,
interpreted as the composition of their B-series, was considered in [14] (Butcher,
1972). Because these B-series are dense in the sense of sequence density, this implies
the composition formula for B-series in general. A direct proof of the composition
theorem was provided in [52] (Hairer, Wanner, 1974).
   The principal result in the current section, Theorem 3.9C (p. 139), is an alternative
approach to the composition of B-series.



Compositions of mappings and B-series

Consider two mappings ah , bh , where ah is a central mapping; that is y0 → y0 + O(h).
Several maps constructed from ah and bh are shown in the diagram (3.9 a). The map
y3 = bh y1 is the composition bh ◦ ah as in the diagram and represents the substitution
of y1 = ah y0 in place of y0 in y2 = bh y0

                                                bh
                                    y1                    y3

                                                ah
                              ah         b h◦                                    (3.9 a)

                                          bh
                               y0                    y2

The principal idea of B-series analysis is to represent ah by Bh a and bh by Bh b and
to represent the composition by Bh ab, where ab ∈ B∗ can be written in terms of a
and b. In the special case that b ∈ B, or b ∈ B0 , then ab is also a member of B or B0 ,
respectively. In diagram (3.9 b), the B-series counterpart of (3.9 a) is shown

                                              b
                                   a                     ab

                              a           ab                                     (3.9 b)

                                          b
                               1                     b
A common application of compositions is in the alteration of the base-point in a par-
ticular Taylor expansion by a substitution y0 → y1 = (Bh y0 )a with the result written
in terms of a Taylor series about y0 . The relationship between the various mappings
is shown on the left of (3.9 c) with the corresponding B-series representation shown
on the right.
134                                                                                 3 B-series and algebraic analysis

                            bh                                                            b
             y1                                  y3                      a                               ab
                           ah
       ah           b h◦            (a)h                         a                 ab           L(a)         (3.9 c)

                    bh                                                          b
      y0                             y2                       1                                   b
The mapping (a)h in the left diagram is necessarily linear because if bh and ch are
mappings, and θ scalar, then
                                                               
                 (bh + ch ) ◦ ah (y0 ) = (bh ◦ ah (y0 ) + (ch ◦ ah (y0 ),
                                                
                    (θ bh ) ◦ ah (y0 ) = θ bh ◦ ah (y0 ).
The linear operator in the left diagram L(a) : B∗ → B∗ is deﬁned by
                                                      L(a)b = ab,
where ab is deﬁned by the extended group operation (a, b) ∈ B × B∗ → ab ∈ B∗ .


The groups of central mappings and central B-series

If ah , bh , ch are central mappings, represented by a, b, c ∈ B respectively, then we
have a rich structure partly represented by the diagram

                                           b h ◦ ah                     ch ◦ b h
                             ah                           bh                              ch
               y0                            y1                            y2                           y3
                            ah −1                        bh −1                           ch −1

                                      ah −1 ◦ bh −1                   bh −1 ◦ ch −1

The diagram representing the corresponding members of B is

                                            ab                            bc
                                a                         b                               c
              1                              a                            ab                          abc
                            a−1                          b−1                             c−1

                                          b−1 a−1                      c−1 b−1

This section is devoted to understanding the nature of the composition (a, b) → ab as
a representation of bh ◦ ah and using this for practical analysis of numerical questions.


Analysis of substitutions

Our principal aim in this section is to show how (ab)(t) can be written in terms of
the mappings a : T → R and b : T → R, where
3.9 Composition of B-series                                                                                       135
                                                      
                             (Bh y0 )(ab) = Bh (Bh y0 )a b.
                                                               
    A convenient ﬁrst step is to write the terms in Bh (Bh y0 )a as linear combinations
of the terms inBh and a second step is to evaluate the coefﬁcients of σ (t)−1 h|t| F(t),
(t ∈ T # ) in Bh (Bh y0 )a b.


Introductory analysis to order 4
We will illustrate the nature of composition by doing the detailed calculation up
to order 4 terms. Starting with Bh (b)(y0 ), we will then substitute Bh (a)(y0 ) for y0
and rearrange to ﬁnd Bh (ab)(y0 ). Note that the usual convention of omitting y0 in
fy0 , f  y0 , f  y0 , . . . , will be followed.
   Begin with

    Bh (b)y0 = b0 y0 + hb1 f + h2 b2 f  f + h3 ( 12 b3 f  ff + b4 f  f  f)
                                                                                                         (3.9 d)
               + h4 ( 16 b5 f  fff + b6 f  ff  f + 12 b7 f  f  ff + b8 f  f  f  f) + O(h5 ),
                                                    
and transform this into Bh (b) Bh (a)y0 by replacing y0 by Bh (a)y0 and every oc-
currence of f (n) = f (n) (y0 ) by f (n) (Bh (a)y0 ), for n = 1, 2, 3. These transformations,
taken to as many terms as required for our purpose, and with appropriate O(hm )
terms omitted for convenience, are
                                                                               
         y0 → y0 + ha1 f + h2 a2 f  f + h3 12 a3 f  ff + a4 f  f  f
                                                                                               
                  + h4 16 a5 f  fff + a6 f  ff  f + 12 a7 f  f  ff + a8 f  f  f  f ,
                                                                       
         hf → hf + h2 a1 f  f + h3 12 a21 f  ff + a2 f  f  f
                                                                                                 
                  + h4 16 a31 f  fff + a1 a2 f  ff  f + 12 a3 f  f  ff + a4 f  f  f  f ,     (3.9 e)
                               
                                         
                                        3 1 2                     
                                                                           
        hf → hf + h a1 f f + h 2 a1 f ff + a2 f f f ,
                         2

          hf  → hf  + h2 a1 f  f,
          hf  → hf  .
Evaluate the transformed coefﬁcients of b0 , . . . , b8 in (3.9 d), in turn, using (3.9 e).
The transformations of y0 and hf have already been given. The coefﬁcient of b2
becomes

  h2 f  f →
                                                                                                                
   hf + h2 a1 f  f + 12 h3 a21 f  ff + h3 a2 f  f  f hf + h2 a1 f  f + 12 h3 a21 f  ff + h3 a2 f  f  f
  = h2 f  f + h3 a1 f  ff + 12 h3 a1 f  f  f
      + 12 h4 a31 f  fff + h4 (a21 + a2 )f  ff  f + 12 h4 a21 f  f  ff + h4 a2 f  f  f  f.

In a similar way we ﬁnd the transformations
                            1 3            1 3                                           
                            2 h f ff → 2 h f ff + 2 h a1 f fff + h a1 f ff f,
                                                             1 4                     4

                              h3 f  f  f → h3 f  f  f + h4 a1 f  ff  f + h4 a1 f  f  f  f,
136                                                               3 B-series and algebraic analysis

                    1 4      1 4 
                    6 h f fff → 6 h f fff,
                       4      4  
                     h f ff f → h f ff f,
                    1 4       1 4  
                    2 h f f ff → 2 h f f ff,
                       4       4   
                     h f f f f → h f f f f.

Write y1 = (Bh y0 )a.


   The second step, of ﬁnding the coefﬁcients of σ (t)−1 h|t| F(t) in
                                           8
                                 b0 y1 + ∑ bi h|ti | F(ti )y1 ,
                                         i=1

gives the results

       (ab)0 := (ab)(∅) = b0 ,
       (ab)1 := (ab)(t1 ) = b0 a1 + b1 ,
       (ab)2 := (ab)(t2 ) = b0 a2 + b1 a1 + b2 ,
       (ab)3 := (ab)(t3 ) = b0 a3 + b1 a21 + 2b2 a1 + b3 ,
       (ab)4 := (ab)(t4 ) = b0 a4 + b1 a2 + b2 a1 + b4 ,
       (ab)5 := (ab)(t5 ) = b0 a5 + b1 a31 + 3b2 a21 + 3b3 a1 + b5 ,
       (ab)6 := (ab)(t6 ) = b0 a6 + b1 a1 a2 + b2 (a21 + a2 ) + b3 a1 + b4 a1 + b6 ,
       (ab)7 := (ab)(t7 ) = b0 a7 + b1 a3 + b2 a21 + 2b4 a1 + b7 ,
       (ab)8 := (ab)(t8 ) = b0 a8 + b1 a4 + b2 a2 + b4 a1 + b8 ,

which, written in matrix form, become
 ⎡         ⎤ ⎡                                                               ⎤⎡         ⎤
    (ab)0          1      0      0     0  0                 0 0 0 0                b0
 ⎢         ⎥ ⎢                                                      ⎥⎢     ⎥
 ⎢ (ab)1 ⎥ ⎢ a1           1      0     0  0                 0 0 0 0 ⎥ ⎢ b1 ⎥
 ⎢         ⎥ ⎢                                                      ⎥⎢     ⎥
 ⎢         ⎥ ⎢ a                                            0 0 0 0 ⎥ ⎢    ⎥
 ⎢ (ab)2 ⎥ ⎢ 2 a1                1     0  0                         ⎥ ⎢ b2 ⎥
 ⎢         ⎥ ⎢                                                      ⎥⎢     ⎥
 ⎢ (ab) ⎥ ⎢ a3 a           2    2a1    1  0                 0 0 0 0 ⎥ ⎢    ⎥
 ⎢       3 ⎥    ⎢          1                                        ⎥ ⎢ b3 ⎥
 ⎢         ⎥ ⎢                                                      ⎥⎢     ⎥
 ⎢ (ab)4 ⎥ = ⎢ a4 a2                                        0 0 0 0 ⎥ ⎢    ⎥
 ⎢         ⎥ ⎢                   a1    0  1                         ⎥ ⎢ b4 ⎥ . (3.9 f)
 ⎢         ⎥ ⎢                                                      ⎥⎢     ⎥
 ⎢ (ab)5 ⎥ ⎢ a5 a31             3a21  3a1 0                 1 0 0 0 ⎥ ⎢ b5 ⎥
 ⎢         ⎥ ⎢                                                      ⎥⎢     ⎥
 ⎢         ⎥ ⎢                                                      ⎥⎢     ⎥
 ⎢ (ab)6 ⎥ ⎢ a6 a1 a2 a21 + a2 a1 a1                        0 1 0 0 ⎥ ⎢ b6 ⎥
 ⎢         ⎥ ⎢                                                      ⎥⎢     ⎥
 ⎢ (ab) ⎥ ⎢ a                    a21                        0 0 1 0 ⎥ ⎢    ⎥
 ⎣       7 ⎦    ⎣ 7 a3                 0 2a1                        ⎦ ⎣ b7 ⎦
    (ab)8          a8 a4         a2    0  a1                0 0 0 1     b8

The elements of the matrix appearing in (3.9 f), written with each ai replaced by ti ,
are
3.9 Composition of B-series                                                                                 137


                    ∅
          ∅         1         0          0          0        0         0          0            0   0
                              1          0          0        0         0          0            0   0
                                         1          0        0         0          0            0   0
                               2        2           1        0         0          0            0   0
                                                    0        1         0          0            0   0
                               3       3 2          3        0         1          0            0   0
                                       2+                              0          1            0   0
                                          2         0        2         0          0            1   0

                                                    0                  0          0            0   1

It will be observed that, up to trees of order 4, the elements of this matrix agree with
those of Λ , introduced in Chapter 2, Section 2.9 (p. 95). Our aim will be to prove
this in general and thus establish Theorem 3.9B and Corollary 3.9F.



Perturbed elementary differentials


For a ∈ B, we will obtain a formula for
                                      
     h|t | F(t ) (Bh a)y0 = h|t | F(t ) y1 ,           where       y1 = y0 + ∑ h|t| a(t)F(t),
                                                                                      t∈ T

written in the form of a B-series.

 Lemma 3.9A                                     
                                   hF(τ) (Bh a)y0 = (Bh b)y0
 where b is deﬁned by

                        b(∅) = 0,
                         b(τ) = 1,                                                                     (3.9 g)
           b([t1 t2 · · · t m ]) = a(t1 )a(t2 ) · · · a(t m ),   t 1 , t 2 , . . . , tm ∈ T.


Proof. The result is equivalent to Theorem 3.4E (p. 116).

Note that b(t) in this result is equal to a(t τ). Now generalize Lemma 3.9A.
138                                                                              3 B-series and algebraic analysis

 Theorem 3.9B For t  ∈ T,
                          
                       h|t |      
                                                          h|t|
                              F(t   )   (B h a)y0   = ∑           a(t t )F(t).                         (3.9 h)
                      σ (t )                         t≥t 
                                                            σ (t)



Proof. The case t = τ is covered by Lemma 3.9A. The general case will be proved
by beta-induction for t = t0 ∗ t 1 , where t0 = [t1 n−1 f  ], and t1 is not a factor of the
forest f . Write t = t 0 ∗ t 1 , where t 0 = [t 1 n−1 f], and t 1 is not a factor of the forest f.
From the induction hypothesis,
                       
                    h|t0 |      
                                                       h|t0 |
                   σ (t0 )
                            F(t 0 )   (B h a)y0   =  ∑  σ (t) a(t0 t0 )F(t0 ),
                                                    t ≥t0
                       
                    h|t1 |                         h|t1 |
                          F(t1 ) (Bh a)y0 = ∑              a(t1 t 1 )F(t),
                   σ (t1 )                   t ≥t 
                                                    σ (t 1 )
                                                        1   1


so that (3.9 h), after division by n , becomes
                          
                       h|t |C                              h|t|
                            
                              F(t ) (Bh a)y0 = ∑ ∑                F(t),
                       σ (t )                  t ≥t  t ≥t 
                                                             σ (t)
                                                            1     1 0        0

where
                                       C = nn a(t0 t0 )a(t1 t1 )
and where we have used the facts that σ (t ) = n σ (t0 )σ (t1 ), σ (t) = nσ (t0 )σ (t1 ).
Simplify C using the formulae
                                                                             
                                                                               n−1
                                  t0     t0 = (t 1    t1 )n −1 tn−n                   (f   f),
                                                                  1              n − 1
                                                                         
                                                                        n
                  (t0 ∗ t1 ) (t0 ∗ t1 ) = (t 1 t1 )n t n−n                (f       f)
                                                          1               n

to give                                                          
                                    C = a (t 0 ∗ t1 ) (t0 ∗ t1 ) .
By adding up C for all pairs for which t0 ∗ t1 = t, we arrive at (3.9 g).


As an example, use t = t2 = τ ∗ τ, t = t6 : We have two choices of the pair (t1 , t 0 )

                             (t 1 , t 0 ) = ( , )     and       (t1 , t 0 ) = ( , ),

leading to
                                                       = +
3.9 Composition of B-series                                                              139

Formula for ab

For each tree, (ab)(t) is a linear combination of terms of the form b(t ) where
|t | ≤ |t|, with an additional term corresponding to t  , replaced by ∅. We will ﬁnd
these linear combinations.

 Theorem 3.9C Let a ∈ B, b ∈ B∗ . Then

                    (ab)(∅) = b(∅),
                     (ab)(t) = b(∅)a(t) + ∑ b(t )a(t t ),               t ∈ T.
                                                    t  ≤t

                                                                        
Proof. Let y1 = (Bh a)y0 . Add b(∅) y1 = y0 + ∑t∈T (σ (t)−1 h|t| a(t)F(t) to the sum
over t ∈ T of b(t  ) times (3.9 h). The result is

                     |t|
  b(∅)y1 + ∑ σh(t) F(t)(y1 )b(t)
             t∈T
                                                 |t|                              
                                  = b(∅)y0 + ∑ σh(t) F(t) a(t)b(∅) + ∑ a(t t)b(t ) ,
                                              t∈T                            t  ≤t

which can be written
                           |t|
  (ab)(∅)y0 + ∑ σh(t) F(t)(ab)(t)
                   t∈T
                                                |t|                                
                                 = b(∅)y0 + ∑ σh(t) F(t) a(t)b(∅) + ∑ a(t t )b(t ) .
                                              t∈T                           t  ≤t

Compare the coefﬁcients of y0 and F(t) for all t ∈ T and the result follows.

   A detailed example of Theorem 3.9C, for a particular order 6 tree t, will be given
in Table 15. The speciﬁc tree chosen is

                                           t = t20 =                  .               (3.9 i)




Sweedler notation

The results of Theorem 3.9C can be written in Sweedler form as

                                 Δ (∅) = 1 ⊗ ∅,
                                  Δ (t) = a(t) ⊗ ∅ + ∑ a(t t) ⊗ t .
                                                             t  ≤t
140                                                                                            3 B-series and algebraic analysis


                           Table 15 Details of terms in (ab)(t), with t given by (3.9 i)


              t       t                   t                   Term                  t       t                       t                Term

          e   f
      c                                                                                            e       f
  b           d                                                                           c
                                           ∅                   a20 b0     b                                d           a                a21 a3 b1
      a
                                                                                                                       c
              e       f                b                                              e       f
      c
                      d                                a       a1 a3 b2   b                   d                        a                a1 a3 b2

                                                                                                                                f
                                                   d                                                                        d
                  c        e   f                                                          c            e
      b                                a                        a41 b2    b                                        a                     a31 b4

                                           e
                                                                                                                                c
                                                   d                      e       f                                b
                  c        f
      b                                a                        a31 b4            d                                             a        a3 b3

                                           e           f                                                                        f
                                                                                                                   c
                                                   d                                                                        d
                  c                                                                       e
      b                                a                        a21 b7    b                                        a                     a21 b6

                                           e                                                                                        f
                                       c
                                                   d                                                           b                    d
                  f                                                           c           e
      b                                a                        a21 b6                                                 a                 a21 b6

                                                   e
                                                                                                                       c
                                   b                       d                                                   b                    d
      c           f                                                           e           f
                                               a                a21 b6                                                 a                 a21 b5

                                           e           f                                                                    e       f
                                       c
                                                   d                                                           b                    d
      b                                                                       c
                                       a                       a1 b11                                                  a                a1 b11

                                                           f                                                                e
                                               c                                                                       c
                                   b                       d                                                   b                    d
      e                                                                       f
                                               a               a1 b10                                                  a                a1 b10

                                               e           f
                                           c
                                   b                       d
      1                                                          b20
                                           a
3.9 Composition of B-series                                                                                          141

Using the standard tree numbering and the notation an := a(tn ), these examples are,
in Sweedler notation,

                             Δ (t1 ) = a1 ⊗ ∅ + 1 ⊗ t1 ,
                             Δ (t2 ) = a2 ⊗ ∅ + a1 ⊗ t1 + 1 ⊗ t2 ,
                             Δ (t3 ) = a3 ⊗ ∅ + a21 ⊗ t1 + 2a1 ⊗ t2 + 1 ⊗ t3 ,
                             Δ (t4 ) = a4 ⊗ ∅ + a2 ⊗ t1 + a1 ⊗ t3 + 1 ⊗ t4 .



Using λ (a, t)
Now recall a notation based on λ , ﬁrst introduced in [14] (Butcher 1972).

 Deﬁnition 3.9D For a ∈ B, t ∈ T,

                                              λ (a, t) = ∑ a(t t )t .
                                                           t  ≤t


In the following statement, the beta product notation will be generalized so that it
becomes linear when its operands are applied to a formal linear combination of trees.
That is                              
                 ∑ x(t)t ∗ ∑ y(t )t = ∑ ∑ x(t)y(t )(t ∗ t ).
                      t∈T                     t  ∈T                  t∈T t  ∈T



 Theorem 3.9E Let a ∈ B, t 1 , t 2 ∈ T. Then

             λ (a, τ) = τ,
        λ (a, t 1 ∗ t2 ) = a(t 2 )λ (a, t 1 ) + λ (a, t1 ) ∗ λ (a, t2 ),                       t 1 , t 2 ∈ T.   (3.9 j)

Proof. Let t = t 1 ∗ t2 and write

              λ (a, t 1 ) = ∑ a(t1 t1 )t1 ,                       λ (a, t 2 ) = ∑ a(t2 t2 )t2 .
                                  t 1 ≤t 1                                        t 2 ≤t 2


The trees such that t ≤ t are of the form
(i) t 1 , with t t  = t 2 (t1 t1 ) or (ii) t 1 ∗ t2 , with t t = (t 1 t1 )(t2 t2 ).
Hence,

     λ (a, t) = ∑ a(t2 )a(t1 t1 )t 1 + ∑                          ∑ a(t1 t1 )a(t2 t2 )(t1 ∗ t2 )
                 t 1 ≤t 1                             t 1 ≤t 1 t 2 ≤t 2
                                                                                                              
              = a(t 2 ) ∑ a(t1 t1 )t 1 +                   ∑ a(t1 t1 )t1 ∗ ∑ a(t2 t2 )t 2
                             t 1 ≤t 1                      
                                                           t 1 ≤t 1                            t 2 ≤t 2

              = a(t2 )λ (a, t 1 ) + λ (a, t1 ) ∗ λ (a, t2 ).
142                                                                 3 B-series and algebraic analysis


                                      Algorithm 5 Evaluate λ

Input:      order, ﬁrst, last, L, R, prod from Algorithm 3 (p. 64); p top, a
Output: λ
    %
    % λ [1 : p top, 1 : p top] is the matrix of λ values
    % p top ≤ p max
    % a[1:last[p top] is the vector of a values
    %
  1 for i from 1 to last[p top] do
  2     for j from 1 to last[p top] do
  3        λ [i, j] ← 0
  4     end for
  5 end for
  6 λ [1, 1] ← 1
  7 for n from 2 to last[p top] do
  8     i ← L[n]
  9     j ← R[n]
 10     for k from 1 to i do
 11        λ [n, k] ← a[ j] ∗ λ [i, k]
 12     end for
 13     for k from 1 to i do
 14        for  from 1 to j do
 15            λ [n, prod(k, )] ← λ [n, prod(k, )] + λ [i, k] ∗ λ [ j, ]
 16        end for
 17     end for
 18 end for



Examples of λ (a, t)
Start with t = τ = t1 and calculate for all trees such that |t| ≤ 4.

      λ (a, t1 )                                                          = t1 ,
      λ (a, t2 ) = λ (a, t1 ∗ t1 ) = a1 t1 + t1 ∗ t1                      = a1 t1 + t2 ,
      λ (a, t3 ) = λ (a, t2 ∗ t1 ) = a1 (a1 t1 + t2 ) + (a1 t1 + t2 ) ∗ t1 = a21 t1 + 2a1 t2 + t3 ,
      λ (a, t4 ) = λ (a, t1 ∗ t2 ) = a2 t1 + t1 ∗ (a1 t1 + t2 )           = a2 t1 + a1 t2 + t4 .


Exercise 37 Evaluate λ (a, t6 ) based on t6 = t4 ∗ t1 .


Exercise 38 Evaluate λ (a, t6 ) based on t6 = t2 ∗ t2 .


Algorithm for λ

An algorithm to evaluate λ is presented in Algorithm 5. As a partial explanation of
how Algorithm 5 works, consider the evaluation of λ (a, t6 ). From earlier steps it will
already be known that
3.9 Composition of B-series                                                                   143

                                  λ (a, t1 ) = t1 ,
                                  λ (a, t2 ) = a1 t1 + t2 ,
                                  λ (a, t4 ) = a2 t1 + a1 t2 + t3 .


These are represented by vectors in R8 (to allow for all trees to order 4):

                             λ (a, t1 ) → [ 1 0 0 0 0 0 0 0 ],
                             λ (a, t2 ) → [ a1 1 0 0 0 0 0 0 ],
                             λ (a, t4 ) → [ a2 a1 0 1 0 0 0 0 ].

Two alternative calculations to evaluate the representation of λ (a, t6 ) are based on
t6 = t2 ∗ t2 and t6 = t4 ∗ t1 , respectively. These give the results

      [ 0 a21 a1 a1 0 1 0 0 ] + [ a1 a2 a2 0 0 0 0 0 0 ]
                                          = [ a1 a2 a21 + a2 a1 a1 0 1 0 0 ],

      [ 0 a2 a1 0 0 1 0 0 ] + [ a1 a2 a21 0 a1 0 0 0 0 ]
                                          = [ a1 a2 a21 + a2 a1 a1 0 1 0 0 ].




Efﬁcient representation of λ coefﬁcients

In applications of Algorithm 5, it will be convenient to work with a modiﬁed version
of λ , in which the diagonal elements are omitted. That is, we ﬁnd it convenient to
work with
                                λ (a,t) := λ (a,t) − t,
rather than λ (a,t) itself.
Corresponding to the result of Theorem 3.9B, we have

 Corollary 3.9F

             λ (a, τ) = 0,
        λ (a, t1 ∗ t2 ) = a(t 2 )λ (a, t 1 ) + λ (a, t1 ) ∗ λ (a, t2 )
                          + a(t2 )t 1 + λ (a, t1 ) ∗ t2 + t1 ∗ λ (a, t2 ),   t 1 , t 2 ∈ T.


Proof. Substitute into (3.9 j).
144                                                                        3 B-series and algebraic analysis


                                         Algorithm 6 Evaluate λ

Input:       order, ﬁrst, last, L, R, prod from Algorithm 3 (p. 64); p top, a
Output: λ
    %
    % λ [n, 1 : last[order[n] − 1], n = 1, 2, . . . , last[p top] is the sequence of λ values
    % p top ≤ p max
    % a[1 : last[p top] is the vector of a values
    %
  1 λ [1, 1] ← 0
  2 for n from 2 to last[p top] do
  3     for i from 1 to last[order[i] − 1]] do
  4         λ [n, i] ← 0
  5     end for
  6     i ← L[n]
  7      j ← R[n]
  8     λ [n, i] ← a[ j]
  9     for k from 1 to last[order[i] − 1] do
 10         λ [n, k] ← a[ j] ∗ λ [i, k]
 11     end for
 12     for  from 1 to last[order[ j] − 1] do
 13         λ [n, prod[i, ]] ← λ [n, prod[i, ]] + λ [ j, ]
 14     end for
 15     for k from 1 to last[order[i] − 1] do
 16         λ [n, prod[k, j]] ← λ [n, prod(k, j]] + λ [i, k]
 17     end for
 18     for k from 1 to last[order[i] − 1] do
 19         for  from 1 to last[order[ j] − 1] do
 20             λ [n, prod(k, )] ← λ [n, prod(k, )] + λ [i, k] ∗ λ [ j, ]
 21         end for
 22     end for
 23 end for




Implementation of algorithms

A realization of Algorithm 5 (p. 142) and Algorithm 6 gave consistent results. A
slightly edited sample of results from Algorithm 6 are, as a list of lists,
 
   {}, {a1 }, {a21 , 2a1 }, {a2 , a1 }, {a31 , 3a21 , 3a1 , 0}, {a1 a2 , a2 + a21 , a1 , a1 },
 {a3 , a21 , 0, 2a1 }, {a4 , a2 , 0, a1 }, {a41 , 4a31 , 6a21 , 0, 4a1 , 0, 0, 0},
 {a2 a21 , 2a1 a2 + a31 , a2 + 2a21 , a21 , a1 , 2a1 , 0, 0}, {a1 a3 , a3 + a31 , a21 , 2a21 , 0, 2a1 , a1 , 0},
 {a1 a4 , a4 + a1 a2 , a2 , a21 , 0, a1 , 0, a1 }, {a22 , 2a1 a2 , a21 , 2a2 , 0, 2a1 , 0, 0},
 {a5 , a31 , 0, 3a21 , 0, 0, 3a1 , 0}, {a6 , a1 a2 , 0, a2 + a21 , 0, 0, a1 , a1 },
                                                                      
 {a7 , a3 , 0, a21 , 0, 0, 0, 2a1 }, {a8 , a4 , 0, a2 , 0, 0, 0, a1 }
3.9 Composition of B-series                                                          145

Inverse of B-series

Given an invertible map A h = Bh (a) we want to ﬁnd the coefﬁcients a−1 to represent
Bh (a−1 ) corresponding to A −1
                             h . This can be done very simply from the composition
Theorem 3.9C by writing (3.9 h) in one of the forms

                          b(t) = −a(t) −         ∑ a(t t )b(t ),
                                            ∅<t <t
                          b(t) = −a(t) −         ∑ b(t t )a(t ),
                                            ∅<t <t

where (ab)(t) = 0, so that b = a−1 .


Antipode and Newton iteration
A second formulation of a B-series inverse is to use the antipode.
   Finally, the inverse may be found by a Newton-like iteration

                    x[0] = 1,
                                                                                  (3.9 k)
                    x[k] = 2x[k−1] − x[k−1] ax[k−1] ,       k = 1, 2, . . . .

This iteration scheme is quadratically convergent in the sense that the order of the
trees, for which it is exact, doubles in each iteration.

 Theorem 3.9G Deﬁne x[k] by the iteration scheme (3.9 k). Then

                              (ax[k] )(t) = 0,       |t| ≤ 2k−1 .

 Proof. Let y = ax[k−1[ . We will show that y(τ) = 0 when k = 1 and that, if y(t) = 0
for |t| ≤ n, then
                            (2y − y2 )(t) = 0,    |t| ≤ 2n.
From the composition rule,

                         2y(t) − (y2 )(t) =       ∑ y(t t )y(t )
                                                 ∅<t <t

and all terms on the right-hand side are zero.



Compositions using Λ

Recall the introduction of Λ in Chapter 2, Section 2.9 (p. 95). Λ is the lower triangular
inﬁnite matrix indexed by the ∅, followed by the sequence of all trees in the standard
order. The diagonal elements are given by Λ (∅, ∅) = 1, Λ (t, t) = 1 for all t ∈ T and
the lower diagonals are given by Λ (t, ∅) = t and Λ (t, t  ) = t t . We also write v as
146                                                               3 B-series and algebraic analysis

an inﬁnite vector with the same index set, where v(∅) = ∅ and v(t) = t. Thus,
                   ⎡                             ⎤        ⎡      ⎤
                       1                                     ∅
                   ⎢                             ⎥        ⎢      ⎥
                   ⎢ t1 1                        ⎥        ⎢ t1 ⎥
                   ⎢                             ⎥        ⎢      ⎥
                   ⎢                             ⎥        ⎢ t ⎥
                   ⎢ t2 t1 1                     ⎥        ⎢   2 ⎥
                   ⎢                             ⎥
              Λ = ⎢ t t2 2t 1                    ⎥,    v=⎢       ⎥
                                                          ⎢ t3 ⎥ .
                   ⎢ 3 1 1                       ⎥        ⎢      ⎥
                   ⎢                             ⎥        ⎢ t ⎥
                   ⎢ t4 t2 t1 0        1         ⎥        ⎢      ⎥
                   ⎢                             ⎥        ⎣
                                                              4
                                                                 ⎦
                   ⎣                             ⎦            .
                        .. .. .. ..    ..   ..                .
                         . . . .        .      .              .

   Expressions like Λ (x) will denote the matrix Λ with every element replaced by a
real number formed by evaluating x(t) for each t appearing in this element. Similarly,
v(u) is the result of evaluating u in the case of each element. This means
              ⎡                                              ⎤                 ⎡          ⎤
                   1                                                               u(∅)
              ⎢                                              ⎥                ⎢         ⎥
              ⎢ x(t 1 )  1                                   ⎥                ⎢ u(t1 ) ⎥
              ⎢                                              ⎥                ⎢         ⎥
              ⎢                                              ⎥                ⎢         ⎥
              ⎢ x(t2 ) x(t1 )    1                           ⎥                ⎢ u(t 2 ) ⎥
              ⎢                                              ⎥                ⎢         ⎥
      Λ (x) = ⎢                                              ⎥,        v(u) = ⎢         ⎥.
              ⎢ x(t3 ) x(t1 )2 2x(t1 ) 1                     ⎥                ⎢ u(t 3 ) ⎥
              ⎢                                              ⎥                ⎢         ⎥
              ⎢ x(t4 ) x(t2 ) x(t 1 ) 0     1                ⎥                ⎢ u(t 4 ) ⎥
              ⎢                                              ⎥                ⎢         ⎥
              ⎣                                              ⎦                ⎣         ⎦
                  ..      ..     ..    ..   ..      ..                            ..
                   .       .      .     .    .           .                         .

It is convenient to rewrite Theorem 3.9C (p. 139) in the form

                                  v(ab) = Λ (a)v(b).                                        (3.9 l)

By regarding the inﬁnite representation of a member of B∗ in terms of v as the
standard representaion, we can write (3.9 l) in the form

                                     ab = Λ (a)b,

where ab on the left is an algebraic expression and Λ (a)b on the right is a matrix–
vector product.

Properties of Λ applied to functions of trees
The following identities hold

                               Λ (y) = Λ (xy),
                          Λ (x)Λ
                          Λ (x)v(u) = v(xu),
                               x(yz) = (xy)z,
                            x(u + v) = xu + xv,
                               x(cu) = c(xu)        (c scalar).
3.9 Composition of B-series                                                       147

Fractional powers

The integer powers of a ∈ B can be calculated as Λ (a)n−1 a or as the ﬁrst column of
Λ (a)n . To extend this to fractional powers, write Λ (a) = I + L, with I the inﬁnite
dimensional identity matrix. We then have the binomial theorem formula
                                       ∞
                                            x(x−1)···(x−i+1) i
                         Λ (a)x = I + ∑            i!       L.                (3.9 m)
                                      i=1

The following result has an application in Chapter 6, Section 6.6 (p. 241).


 Lemma 3.9H
                                    ax (τ) = xa(τ).


Proof. ax (τ) is the (τ, ∅) element of the i = 1 term in (3.9 m).




Summary of Chapter 3 and the way forward


Summary


One of the key questions in the analysis of numerical approximations to differen-
tial equations, and related problems, is in the comparison of two mappings. One
mapping would be the exact ﬂow through a speciﬁed time step and the other would
be a numerical scheme, such as a Runge–Kutta method. The B-series approach to
questions like this is to write the Taylor expansions of the two mappings in a special
way, in terms of “elementary differentials”, and to then compare the coefﬁcients in
corresponding terms. The terms themselves can be indexed in terms of the objects
known as “rooted trees” and the theory of B-series hinges on this indexing.

    The chapter includes a discussion of multi-dimensional Taylor series and it is
shown how this leads to the formulation of elementary differentials and B-series.
Some sample problems, which are both easy and fundamental, are solved, ﬁrst in
a low order introduction and then in full generality. Special attention is given to
the ﬂow through a unit step, followed by an implicit variant of the Euler method. It
is remarkable that the latter simple example is a direct path to the large family of
Runge–Kutta methods. The composition rule for B-series is introduced and some of
its wider ramiﬁcations are explored.
148                                                                   3 B-series and algebraic analysis

The way forward

Group and algebraic structures
In Chapter 4 we will look at the algebra associated with B∗ as a mathematical system
in its own right. It will be seen that the elements of B form a group with the same
group operation as required for the B-series composition rule to hold.
   We will continue to use D ∈ B0 and E ∈ B but with the algebraic meaning as well
as their meaning as representing B-series coefﬁcients. The meaning of aD given in
Section 3.4 can now be compared with aD evaluated by the composition rule.


Linear operators on B0
Corresponding to unit valency stumps introduced in Section 2.7, and their algebra,
we can construct an algebra of linear operators on B0 . The multiplicative semigroup
of linear operators consists of the identity operator and arbitrary products of

                               hf  ,     h2 f  f,   h2 f  f  ,

corresponding to the valency one stumps

                                   τ1 ,     τ 2 τ1 ,    τ12 .


Runge–Kutta methods
The order conditions, established in Theorem 3.6C (p. 127), are applied in Chapter 5
to the derivation of speciﬁc practical methods.


Teaching and study notes

This is the central chapter of this volume and it should be studied in detail before
later chapters are attempted. The key ideas are the multi-dimensional Taylor series
and the Taylor expansion of the ﬂow and related problems in terms of “elementary
differentials”. The concept of B-series is introduced through the formula

                                                          h|t| a(t)
                 ah y0 = (Bh y0 )a = a(∅)y0 + ∑                     (F(t)y0 ).
                                                       t∈T σ (t)

If a(∅) = 1, then bh ◦ ah is deﬁned by the mapping y0 → bh (ah y0 ) and is given by
the B-series composition rule

                         Bh (ab)(y0 ) = Bh (b)(Bh (a)(y0 )).

An important consideration is the nature of the product

                                        (a, b) → ab.
3.9 Composition of B-series                                                                      149

Possible supplementary reading includes
Butcher, J.C. An algebraic theory of integration methods (1972) [14]
Butcher, J.C. Numerical Methods for Ordinary Differential Equations (2016) [20]
Hairer, E., Nørsett, S.P. and Wanner, G. Solving Ordinary Differential Equations
I: Nonstiff Problems (1993) [50]
Hairer, E. and Wanner, G. On the Butcher group and general multi-value method.
(1974) [52]

Projects
Project 9 Carry out a detailed study of the analytical theory of Fréchet differentiation. A possible
starting point is [81].
Project 10   Study Taylor series in RN , including error estimates.
                                                                                         
Project 11 Learn about a modiﬁed B-series theory suitable for the problem y (x) = f y(x) ,
with y and y given at an initial point.
