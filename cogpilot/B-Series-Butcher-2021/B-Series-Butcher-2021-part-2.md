# B-Series: Algebraic Analysis of Numerical Methods - Part 2

**Author:** John C. Butcher

**Series:** Springer Series in Computational Mathematics, Volume 55

---

Chapter 1
Differential equations, numerical methods and
algebraic analysis




1.1 Introduction

Differential equations and numerical methods

Ordinary differential equations are at the heart of mathematical modelling. Typically
ordinary differential equation systems arise as initial value problems
                                       
                      y (x) = f x, y(x) ,     y(x0 ) = y0 ∈ RN .

or, if y does not depend directly on x,
                                      
                        y (x) = f y(x) ,         y(x0 ) = y0 ∈ RN .              (1.1 a)

The purpose of an equation like this is to describe the behaviour of a physical or
other system and, at the same time, to predict future values of the time-dependant
variable y(x), whose components represent quantities of scientiﬁc interest.
   It is often more convenient, in speciﬁc situations, to formulate (1.1 a) in different
styles. For example, the components of y(x) might represent differently named
variables, and the formulation should express this. In other situations the problem
being modelled might be more naturally represented using a system of second, or
higher, order differential equations. However, we will usually use (1.1 a) as a standard
form for a differential system.
   Given x > x0 , the ﬂow of (1.1 a) is the solution to this initial value problem
evaluated at x. This is sometimes written as e(x−x0 ) f y0 , but our preference will be to
write it as ﬂow x−x y0 , where the nature of f is taken for granted.
                    0
   The predictive power of differential equations is used throughout science, even
when solutions cannot be obtained analytically, and this underlines the need for
numerical methods. This usually means that we need to approximate ﬂow h y0 to
obtain a usable value of y(x0 + h). This can be repeated computationally to obtain, in
turn, y(x0 + h), y(x0 + 2h), y(x0 + 3h), . . . .
   Although many methods for carrying out the approximation to the ﬂow are known,
we will emphasize Runge–Kutta methods, because these consist of approximating

© Springer Nature Switzerland AG 2021                                                   1
J. C. Butcher, B-Series, Springer Series in Computational Mathematics 55,
https://doi.org/10.1007/978-3-030-70956-3_1
2                           1 Differential equations, numerical methods and algebraic analysis

the solution at x0 + nh, n = 1, 2, 3, . . . , step by step. As an example of these methods,
choose one of the famous methods of Runge [82] (Runge, 1895), where the mapping
R h y0 = y1 is deﬁned by
                                                              
                             y1 = y0 + h f y0 + 12 h f (y0 ) .                      (1.1 b)


Accuracy of numerical approximations
Accuracy of numerical methods will be approached, in this volume, through a study
of the formal Taylor expansions of the solution, and of numerical approximations
to the solution. The ﬂavour of the questions that arise is both combinatorial and
algebraic, because of the common structure of many of the formal expansions.
   For the problem (1.1 a), we will need to compare the mappings ﬂow h and, for a
particular Runge–Kutta method, the mapping RK h . This leads us to consider the
difference
                                ﬂow h y0 − RK h y0 .
If it were possible to expand this expression in a Taylor series, then it would be
possible to seek methods for which the terms are zero up to some required power of
h, say to terms like h p . It would then be possible to estimate the asymptotic accuracy
of the error as O(h p+1 ). This would be only for a single step but this theory, if it
were feasible, would also give a guide to the global accuracy.


Taylor expansions and trees

Remarkably, ﬂow h y0 and RK h y0 have closely related Taylor expansions, and one
of the ﬁrst aims of this book is to enunciate and analyse these expansions. The ﬁrst
step, in this formulation, is to make use of the graphs known as rooted trees, or
arborescences, but referred to throughout this book simply as trees.
   The formal introduction of trees will take place in Chapter 2 but, in the meantime,
we will introduce these objects by illustrative diagrams:

             ,       ,       ,       ,          ,         ,      ,       ,     ...

The set of all trees will be denoted by T.
    If t denotes an arbitrary tree, then |t| is the “order”, or number of vertices, and σ (t)
is the symmetry of t. The symmetry is a positive integer indicating how repetitive a
tree diagram is. The formal statement will be given in Deﬁnition 2.5A (p. 58).
    The common form for ﬂow h y0 and RK h y0 is

                                 y0 + ∑ χ(t) σ 1(t) F(t)h|t| ,                        (1.1 c)
                                         t

where, for a given tree, F(t) depends only on the differential equation being solved
and χ(t) depends only on the mapping, ﬂow h or RK h .
1.1 Introduction                                                                        3

   The formulation of various Taylor expansions, given by (1.1 c), is the essential
idea behind the theory of B-series, and is the central motivation for this book. We
will illustrate the use of this result, using three numerical methods from the present
chapter, together with the ﬂow itself. The methods are the Euler method, Euler h ,
(1.4 a), the Runge–Kutta method, Runge-I h :
                                                                      
                        y1 = y0 + 12 h f (y0 ) + 12 h f y0 + h f (y0 )

and Runge-II h , given by (1.1 b). Alternative formulations of these Runge–Kutta
methods in Section 1.5 (p. 19) are, for Runge-I h , (1.5 c) and, for Runge-II h , (1.5 d).
  The coefﬁcients, that is the values of Ψ (t), for |t| ≤ 3, are

                   Mapping             Ψ( ) Ψ( ) Ψ( ) Ψ( )
                                                    1         1          1
                   ﬂow h                1           2         3          6
                   Euler h              1           0         0          0
                                                    1         1
                   Runge-I h            1           2         2          0
                                                    1         1
                   Runge-II h           1           2         4          0
    Independently of the choice of the differential equation system being solved, we
can now state the orders of the three methods under consideration. Because the same
entry is given for the single ﬁrst order tree, each of the three numerical methods is
at least ﬁrst order as an approximation to the exact solution. Furthermore, the two
Runge methods are order two but not three, as we see from the agreement with the
ﬂow for the order 2 tree, but not for the two order 3 trees. Also, from the table entries,
we see that the Euler method does not have an order greater than one.


Fréchet derivatives and gradients
In the formulation and analysis of both initial value problems, and numerical methods
for solving them, it will be necessary to introduce various structures involving partial
derivatives. In particular, the ﬁrst Fréchet derivative, also known as the Jacobian
matrix, with elements
                                 ⎡                             ⎤
                                   ∂ f1     ∂ f1         ∂ f1
                                                   · · · ∂ yN
                                 ⎢ ∂y 1     ∂y   2             ⎥
                                 ⎢ ∂ f2     ∂ f2         ∂ f2 ⎥
                                 ⎢                 · · · ∂ yN ⎥
                                 ⎢                             ⎥
                       f  (y) = ⎢ ∂ y      ∂ y2
                                      1
                                                                 .
                                 ⎢ ..         ..           .. ⎥⎥
                                 ⎢ .           .            . ⎥
                                 ⎣                             ⎦
                                   ∂ fN    ∂ fN          ∂ fN
                                                   · · · ∂ yN
                                      1∂y      ∂y2


Similarly the Fréchet derivative of a scalar-valued function has the form

                           H  (y) =    ∂H
                                        ∂ y1
                                               ∂H
                                               ∂ y2
                                                        ···   ∂H
                                                              ∂ yN
                                                                     .
4                             1 Differential equations, numerical methods and algebraic analysis

This is closely related to the gradient ∇(H) = H  (y)T which arises in many speciﬁc
problems and classes of problems.


Chapter outline

In Section 1.2, a review of differential equations is presented. This is followed
in Section 1.3 by examples of differential equations, The Euler and Taylor series
methods are introduced in Section 1.4 followed by Runge–Kutta methods (Section
1.5), and multivalue methods (Section 1.6). Finally, a preliminary introduction to
B-series is presented in Section 1.7.


1.2 Differential equations

An ordinary differential equation is expressed in the form
                      dy            
                      d x = f x, y(x) ,            f : R × R N → RN                     (1.2 a)

or, written in terms of individual components,
                        d y1      1                          
                        d x = f x, y (x), y (x), . . . , y (x) ,
                               1           2              N

                        d y2      1                          
                        d x = f x, y (x), y (x), . . . , y (x) ,
                               2           2              N

                         ..          ..                                                 (1.2 b)
                          .           .
                       d yN       1                          
                        d x = f x, y (x), y (x), . . . , y (x) .
                               N           2              N


This can be formulated as an autonomous problem
                              dy         
                              d x = f y(x) ,       f : R N → RN ,                       (1.2 c)

by increasing N if necessary and introducing a new dependent variable y0 which is
forced to always equal x. This autonomous form of (1.2 b) becomes

                     d y0
                           = 1,
                      dx
                     d y1                                             
                           = f 1 y0 (x), y1 (x), y2 (x), . . . , yN (x) ,
                      dx
                     d y2                                             
                           = f 2 y0 (x), y1 (x), y2 (x), . . . , yN (x) ,
                      dx
                      ..        ..
                       .         .
                     dy  N                                            
                           = f N y0 (x), y1 (x), y2 (x), . . . , yN (x) .
                     dx
1.2 Differential equations                                                                           5

Initial value problems

A subsidiary condition

                              y(x0 ) = y0 ,        x0 ∈ R,        y 0 ∈ RN ,                    (1.2 d)

is an initial value and an initial value problem consists of the pair of equations (1.2 a),
(1.2 d) or the pair (1.2 c), (1.2 d).
    Initial value problems have applications in applied mathematics, engineering,
physics and other sciences, and ﬁnding reliable and efﬁcient numerical methods for
their solution is of vital importance.

Exercise 1 Reformulate the initial value problem

                    u (x) + 3u (x) = 2u(x) + v(x) + cos(x),     u(1) = 2,   u (1) = −2,
             v (x) + u (x) − v (x) = u(x) + v(x)2 + sin(x),    v(1) = 1,   v (1) = 4,
                          
in the form y (x) = f y(x) , y(x0 ) = y0 , where y0 = x, y1 = u, y2 = u , y3 = v, y4 = v .


Scalar problems
If N = 1, we obtain a scalar initial value problem
                                        
                      y (x) = f x, y(x) ,      y(x0 ) = y0 ∈ R.                                (1.2 e)

Scalar problems are useful models for more general problems, because of their sim-
plicity and ease of analysis. However, this simplicity can lead to spurious conclusions.
A speciﬁc case is the early analysis of Runge–Kutta order conditions [82] (Runge,
1895), [56] (Heun, 1900), [66] (Kutta, 1901), [77] Nyström, 1925), in which, above
order 4, the order conditions derived using (1.2 e) give an incomplete set.

Complex variables
Sometimes it is convenient to write a differential equation using complex variables
                           dz           
                              = f t, z(t) ,            f : R × C N → CN .
                           dt
For example, the system
                                 dx
                                    = 2x + 3 cos(t),         x(0) = 1,
                                 dt
                                 dy
                                    = 2y + sin(t),           y(0) = 0,
                                 dt
can be written succinctly as
                         dz
                            = 2z + 2 exp(it) + exp(−it),              z(0) = 1,                 (1.2 f)
                         dt
with z(t) = x(t) + iy(t).
6                              1 Differential equations, numerical methods and algebraic analysis

Exercise 2 Find the values of A, B, C such that z = A exp(2t) + B exp(it) +C exp(−it) is the
solution to (1.2 f).

Exercise 3 Write the solution to Exercise 2 in terms of the real and imaginary components.




Well-posed problems

An initial value problem is well-posed if it has a solution, this solution is unique
and the solution depends continuously on the initial value. In this discussion we will
conﬁne ourselves to autonomous problems.

    Deﬁnition 1.2A A function f : RN → RN satisﬁes a Lipschitz condition if there
    exists a constant L > 0 (the Lipschitz constant) such that

                          f (y) − f (z) ≤ Ly − z,                y, z ∈ RN .

Given an initial value problem
                                            
                              y (x) = f y(x) ,             y(x0 ) = y0 ,

where f satisﬁes a Lipschitz condition with constant L, we ﬁnd by integration that
for x ≥ x0 ,
                                         x      
                           y(x) = y0 +     f y(x) d x.                     (1.2 g)
                                                   x0

If x ∈ I := [x0 , x], and y denotes supx∈I y(x), we can construct a sequence of
approximations y[k] , k = 0, 1, . . . , to (1.2 g), from

                    y[0] (x) = y0 ,
                                         x              
                      y[k] (x) = y0 +        f y[k−1] (x) d x,        k = 1, 2, . . . .
                                        x0

If r := |x − x0 | L < 1, we obtain the estimates

                    y[1] − y[0]  ≤ |x − x0 |  f (y0 ),                                (1.2 h)
                 y [k+1]
                            − y  ≤ ry − y
                              [k]            [k]   [k−1]
                                                            ≤ r |x − x0 |  f (y0 ).
                                                                 k
                                                                                          (1.2 i)

This shows that the sequence y[k] , k = 0, 1, . . . , is convergent. Denote the limit by y.
It can be veriﬁed that the conditions for well-posedness are satisﬁed.
    By adding (1.2 h) and (1.2 i), with k = 1, 2, . . . , we see that every member of the
sequence satisﬁes
                                           1
                         y[k] − y[0]  ≤      |x − x0 |  f (y0 ).
                                          1−r
1.2 Differential equations                                                           7

To overcome the restriction |x − x0 | L < 1, a sequence of x values can be inserted
between x0 and x, sufﬁciently close together to obtain convergent sequences in each
subinterval in turn.
   While a Lipschitz condition is very convenient to use in applications, it is not
a realistic assumption, because many well-posed problems do not satisfy it. It is
perhaps better to use the property given in the following.


 Deﬁnition 1.2B A function f : RN → RN satisﬁes a local Lipschitz condition
 if there exists a constant L (the Lipschitz constant) and a positive real R (the
 inﬂuence radius) such that

               f (y) − f (z) ≤ Ly − z,           y, z ∈ RN ,     y − z ≤ 2R.

If f satisﬁes the conditions of Deﬁnition 1.2B, then for a a given y0 ∈ RN , deﬁne a
disc D by
                            D = {y ∈ RN : y − y0  ≤ R}

and a function f by
                               ⎧
                               ⎪
                               ⎨              f (y),                 y ∈ D,
                     f(y) =
                               ⎩ f y + R (y − y ),
                               ⎪
                                                                     y ∈ D.
                                     0 y−y    0
                                                 0




Exercise 4 Show that f satisﬁes a Lipschitz condition with Lipschitz constant L.




The ﬁrst numerical methods

The method of Euler [42] (Euler, Collected works, 1913), proposed in the eighteenth
century, is regarded as the foundation of numerical time-stepping methods for the
solution of differential equations. We will refer to it here as the “explicit Euler”
method to distinguish it from the closely related “implicit Euler” method. Given a
problem                                   
                         y (x) = f x, y(x) ,   y(x0 ) = y0 ,
we can try to approximate the solution at a nearby point x1 = x0 + h, by the formula

                               y(x1 ) ≈ y1 := y0 + h f (x0 , y0 ).

  This is illustrated in the one-dimensional case by the diagram on the left (Explicit
Euler).
8                            1 Differential equations, numerical methods and algebraic analysis

         Explicit Euler                                    Implicit Euler
                                                                            y(x)
                          y(x)

                                                                                    y1
                                  y1
                                                               y0
             y0
                     h                                                 h
           x0                    x1                           x0                   x1

    According to this diagram, y1 − y0 is calculated as the area of the rectangle with
width h and height f (x0 , y0 ). This is not the correct answer, for which h should be
multiplied by the average value of f (y(x)), but it is often close enough to give useful
results for small h. In the diagram on the right (Implicit Euler), the value of y1 − y0
is h is multiplied by f (y1 ), which is not known explicitly but can be evaluated by
iteration in the formula
                                  y1 = y0 + h f (x1 , y1 ).
We will return to the Euler method in Section 1.4.



1.3 Examples of differential equations

Linear problems

Exponential growth and decay
                                    dy
                                        = λ y.
                                    dx
If λ > 0, the solution represents exponential growth and, if λ < 0, the solution
represents exponential decay. Two cases can be combined into a single system
                                               
                                d y1           y1
                                        =           .
                               d x y2       −y2

This can also be written
                                                     
                                               0 1
                                      y =                 ∇(y1 y2 )
                                             −1 0
and is an example of a Poisson problem

                                          y (x) = S∇(H(y)),                             (1.3 a)

where S is a skew-symmetric matrix. For such problems H(y(x)) has a constant value,
because                         
                       d H y(x)        ∂ H   ∂ H T
                                  =         S         = 0.
                           dx           ∂y      ∂y
1.3 Examples of differential equations                                                9

It is an important aim in numerical analysis to preserve this invariance property, in
computational results.


A four-dimensional linear problem
The problem                                ⎡          ⎤
                                            −2 1 0 0
                                           ⎢          ⎥
                                           ⎢ 1 −2 1 0 ⎥
                         y = My,        M=⎢          ⎥
                                           ⎢ 0 1 −2 1 ⎥ ,
                                           ⎣          ⎦
                                             0 0 1 −2
is a trivial special case of the discretized diffusion equation on an interval domain. A
transformation M → M = T −1 MT , where
                      ⎡                  ⎤            ⎡                 ⎤
                         1 0 0 1                        −2 1 0 0
                      ⎢                  ⎥            ⎢                 ⎥
                      ⎢ 0 1 1 0 ⎥                     ⎢ 1 −1 0 0 ⎥
                T =⎢  ⎢ 0 1 −1 0 ⎥
                                         ⎥,      M  = ⎢
                                                      ⎢ 0 0 −3 1 ⎥ ,
                                                                        ⎥
                      ⎣                  ⎦            ⎣                 ⎦
                         1 0 0 −1                        0 0 1 −2

partitions the problem into symmetric and anti-symmetric components. Also write
y = T −1 y, y0 = T −1 y0 so that the partitioned initial value problem becomes

                                 y = M y,     y(x0 ) = y0 .

Making this transformation converts the problem into two separate two dimensional
problems which can be solved independently and the results recombined.


Harmonic oscillator and simple pendulun
The harmonic oscillator:                      
                                     d y1     y2
                                           =       .
                                    d x y2   −y1
This equation can be recast in scalar complex form by introducing a new variable
z = y1 + iy2 . It then becomes
                                     dz
                                        = −iz.
                                    dx
The harmonic oscillator can also be written in the form (1.3 a), with
                                                        
                                H(y) = 12 (y1 )2 + (y2 )2 .

The simple pendulum:                              
                                  d y1        y2
                                        =              .
                                 d x y2   − sin(y1 )
10                             1 Differential equations, numerical methods and algebraic analysis

This problem is not linear but, if y(0) is sufﬁciently small, the simple pendulum is
a reasonable approximation to a linear problem, because sin(y1 ) ≈ y1 . It also has the
form of (1.3 a) with H(y) = 12 (y2 )2 − cos(y1 ).


Stiff problems

Many problems arising in scientiﬁc modelling have a special property known as
“stiffness”, which makes numerical solution by classical methods very difﬁcult. An
early reference is [35] (Curtiss, Hirschfelder,1952). For a contemporary study of
stiff problems, and numerical methods for their solution, see [53] (Hairer, Nørsett,
Wanner,1993) and [86] (Söderlind, Jay, Calvo, 2015).
    When attempting to determine the most appropriate stepsize to use with a par-
ticular method, and a particular problem, many considerations come into play. The
ﬁrst is the requirement that the truncation error is sufﬁciently small to match the
requirements of the physical application, and the second is that the numerical results
are not corrupted unduly by unstable behaviour.
    To illustrate this idea, consider the use of the Euler method (see Section 1.4
(p. 14)), applied to the three-dimensional problem
        ⎡     ⎤ ⎡                         ⎤ ⎡             ⎤ ⎡           ⎤
           y1         −y2 + 0.40001(y3 )2          y1 (0)         0.998
     d ⎢      ⎥ ⎢
        ⎢ y2 ⎥ = ⎢
                                          ⎥ ⎢             ⎥ ⎢           ⎥
                                          ⎥ , ⎢ y2 (0) ⎥ = ⎢ 0.00001 ⎥ , (1.3 b)
                               y1
     dx ⎣     ⎦ ⎣                         ⎦ ⎣             ⎦ ⎣           ⎦
           y3                −100y3                y3 (0)           1

with exact solution
                 ⎡             ⎤    ⎡                                      ⎤
                      y1 (x)             cos(x) − 0.002 exp(−200x)
                 ⎢        ⎥ ⎢                               ⎥
                 ⎢ y2 (x) ⎥ = ⎢ sin(x) + 0.00001 exp(−200x) ⎥ .
                 ⎣        ⎦ ⎣                               ⎦
                   y3 (x)                exp(−100x)

A solution by the Euler method consists of computing approximations

            y1 ≈ y(x0 + h),        y2 ≈ y(x0 + 2h),      y3 ≈ y(x0 + 3h),      ...,

using yn = F(yn−1 ), n = 1, 2, . . . , where
                            ⎡                                ⎤
                                 u1 + h − u2 + 0.40001(u3 )2
                            ⎢                                  ⎥
                   F(u) = ⎢ ⎣                u2 + hu1          ⎥.
                                                               ⎦
                                           (1 − 100h)u3


   For sequences like this, stability, for the third component, depends on the condition
1 − 100h ≥ −1 being satisﬁed, so that h ≤ 0.02. If this condition is not satisﬁed,
unstable behaviour of y3 will feed into the ﬁrst two components and the computed
1.3 Examples of differential equations                                                                      11

results cannot be relied on. However, if the initial value for y3 were zero, and this
component never drifted from this value, there would be no such restriction on
obtaining reliable answers.

Exercise 5 If problem (1.3 b) is solved using the implicit Euler method (1.4 d), ﬁnd the function F
such that yn = F(yn−1 ), and show that there is no restriction on positive h to yield stable results.




Test problems

A historical problem

The following one-dimensional non-autonomous problem was used by Runge and
others to verify the behaviour of some early Runge–Kutta methods:

                           dy y−x
                               =       ,     y(0) = 1.                        (1.3 c)
                           dx y+x
                                                   
A parametric solution t → y(t), x(t) := y1 (t), y2 (t) can be found from the system
                                                                                               
              d        y1                   1 −1              y1               y1 (0)               1
                                    =                                  ,                    =
              dt       y2                   1   1             y2               y2 (0)               0

and, by writing z = y1 + iy2 , we obtain

                           dz
                               = (1 + i)z,       z(0) = 1,
                           dt
                             
with solution z = exp (1 + i)t , so that, reverting to the original notation,

                                                y(t) = exp(t) cos(t),
                                                x(t) = exp(t) sin(t).
                                                     
The solution on 0, exp( 12 π) corresponds to t ∈ 0, 12 π and is shown in the diagram

                                                    t = π/4
                       y

                            1 t =0


                                                                                  t = π/2
                            0
                                0                         x                exp(π/2)
12                        1 Differential equations, numerical methods and algebraic analysis

A problem from DETEST

One of the pioneering developments in the history of numerical methods for differ-
ential equations is the use of standardized test problems. These have been useful in
identifying reliable and accurate software. This problem from the DETEST set [57]
(Hull, Enright, Fellen, Sedgwick, 1972) is an interesting example.

                             dy
                                = cos(x)y,      y(0) = 1.
                             dx
                                          
The exact solution, given by y = exp sin(x) , is shown in the diagram

      exp(1)


      y

          1

     exp(−1)

                           π/2                 π                3π/2                 2π
                                                                            x



The Prothero–Robinson problem

The problem of Prothero and Robinson [79] (1974),
                                                       
                            y (x) = g (x) + L y − g(x) ,

where g(x) is a known function, was introduced as a model for studying the behaviour
of numerical methods applied to stiff problems. A special case is
                                              
                    y = cos(x) − 10 y − sin(x) ,         y(0) = 0,

with general solution y(x) = C exp(−10x) + sin(x), where C = 0 when y(0) = 0.



A problem with discontinuous derivatives

The two-dimensional “diamond problem”, as we will name it, is deﬁned to have
piecewise constant derivative values which change from quadrant to quadrant as
follows
1.3 Examples of differential equations                                                13
                             ⎧    
                             ⎪
                             ⎪ −1
                             ⎪
                             ⎪       ,             y1 > 0,   y2 ≥ 0,
                             ⎪
                             ⎪
                             ⎪
                             ⎪   1
                             ⎪
                             ⎪    
                             ⎪
                             ⎪
                             ⎪
                             ⎪  −1
                             ⎪
                             ⎪       ,             y1 ≤ 0,   y2 > 0,
                        d y ⎨ −1
                           =      
                        dx ⎪ ⎪
                             ⎪
                             ⎪   1
                             ⎪
                             ⎪       ,             y1 < 0,   y2 ≤ 0,
                             ⎪
                             ⎪  −1
                             ⎪
                             ⎪    
                             ⎪
                             ⎪
                             ⎪
                             ⎪   1
                             ⎪
                             ⎪       ,             y1 ≥ 0,   y2 < 0.
                             ⎩
                                 1

   Using the initial value y = [ 1 0 ]T , the orbit, with period 4, is as in the diagram:

                                              1


                                               0         1




   This problem is interesting as a numerical test because of the non-smoothness of
the orbit as it moves from one quadrant to the next.


The Kepler problem
                                       ⎡ ⎤ ⎡ 3⎤
                                         y1      y
                                       ⎢ ⎥ ⎢  ⎢
                                                    ⎥
                                                   4⎥
                                       ⎢    ⎥
                                    d ⎢y ⎥ ⎢ 1 ⎥
                                          2   ⎢  y
                                       ⎢ ⎥=      y ⎥,                            (1.3 d)
                                   d x ⎢y3 ⎥ ⎢  − 3⎥
                                       ⎣ ⎦ ⎣ r ⎥
                                              ⎢
                                                    ⎦
                                                 y2
                                         y4     − 3
                                                     r
                         1/2
where r = (y1 )2 + (y2 )2      . The Kepler problem satisﬁes conservation of energy
H  = 0, where                                     
                           H(x) = 12 (y3 )2 + (y4 )2 − r−1
and also conservation of angular momentum A = 0, where

                                    A(x) = y1 y4 − y2 y3 .


Exercise 6 Show that H(x) is invariant.


Exercise 7 Show that A(x) is invariant.
14                               1 Differential equations, numerical methods and algebraic analysis

1.4 The Euler method

The explicit Euler method as a Taylor series method

Given a differential equation and an initial value,

                                 y (x) = f (x, y),        y(x0 ) = y0 ,

the Taylor series formula is a possible approach to ﬁnding an approximation to
y(x0 + h):
            y(x0 + h) ≈ y(x0 ) + hy (x0 ) + 2!
                                             1 2                  1 p (p)
                                                h y (x0 ) + · · · + p! h y (x0 ).

If y is a sufﬁciently smooth function, then we would expect the error in this ap-
proximation to be O(h p+1 ). When p = 1, this reduces to the Euler method. This is
very convenient to use, because both y(x0 ) = y0 and y (x0 ) = f (x0 , y0 ) are known
in advance. However, for p = 2, we would need the value of y (x0 ), which can be
found from the chain rule:
                            d           ∂ f ∂ f dy
                  y (x) = dx f x, y(x) = ∂ x + ∂ y dx = fx + fy f ,

where the subscripts in fx and fy denote partial derivatives, and, for brevity, the
arguments have been suppressed. Restoring the arguments, we can write

                         y (x0 ) = fx (x0 , y0 ) + fy (x0 , y0 ) f (x0 , y0 ).

The increasingly more complicated expressions for y(3) , y(4) , . . . , have been worked
out at least to order 6 [59] (Hut’a, 1956), and they are summarized here to order 4.

                  y = f ,
                 y = fx + fy f ,
                y(3) = fxx + 2 fxy f + fyy f 2 + fx fy + fy2 f ,
                y(4) = fxxx + 3 fxxy f + 3 fxy fx + 5 fxy fy f + 3 fxyy f 2 + fy fxx
                             + 3 fx fyy f + fy2 fx + fy3 f + 4 fy f 2 fyy + f 3 fyyy .

We will return to the evaluation of higher derivatives, in the case of an autonomous
system, in Section 1.7 (p. 33).

Exercise 8 Given the differential equation y = y + sin(x), ﬁnd y(n) for n ≤ 7.


The explicit Euler method

The Euler method produces the result

                      yk = yk−1 + h f (xk−1 , yk−1 ),             k = 1, 2, . . . .        (1.4 a)
1.4 The Euler method                                                                    15

In this introduction, it will be assumed that h is constant. Now consider a numerical
method of the form
                               yk = yk−1 + hΨ(xk−1 , yk−1 ),                   (1.4 b)
used in the same way as the Euler method.

 Deﬁnition 1.4A The method deﬁned by (1.4 b) is convergent if, for a problem
 deﬁned by f (x, y), y(x0 ) = y0 , with the solution Yn , at x, approximated using n
 steps with h = (x − x0 )/n, then

                                        lim Yn = y(x).
                                       n→∞



 Theorem 1.4B The Euler method is convergent.

This result from [36] (Dahlquist, 1956), with an exposition in the classic textbook
[55] (Henrici, 1962), is also presented in the more recent books [50] (Hairer, Nørsett,
Wanner, 1993) and [20] (Butcher, 2016).


Variable stepsize
The standard formulation of a one-step method is based on a single input y0 , and its
purpose is to calculate a single output y1 . However, it is also possible to consider the
input as being the pair [y0 , h f0 ], with f0 = f (y0 ). In this case the output would be a
pair [y1 , h f1 ]. Apart from the inconvenience of passing additional data between steps,
the two formulations are identical.
   However, the two input approach has an advantage if the Euler method is required
to be executed as a variable stepsize method, as in the Octave function (1.4 c).As
we will see in Section 1.5, the Runge–Kutta method (1.5 c) has order 2. This would
mean that half the difference between the result computed by Euler, and the result
computed by this particular Runge–Kutta method, could be used as an error estimator
for the Euler result because y0 + 12 h f0 + 12 h f1 is identical to the result computed
by (1.5 c). This is the basis for the function represented in (1.4 c). Note that this
estimation does not require additional f calculations.
   function [yout,hfyout,hout] = Euler(y,hfy,tolerance)
      yout = y + hfy;
      hfyout = h * f(yout);
      error = 0.5 * norm(hfy - hfyout);
      r = sqrt(tolerance / error);                                                 (1.4 c)
      hout = r * h;
      hfyout = r * hfyout;
   endfunction


Exercise 9 Discuss the imperfections in (1.4 c).
16                            1 Differential equations, numerical methods and algebraic analysis

The implicit Euler method
As we saw in Section 1.3, through the problem (1.3 b), there are sometimes advan-
tages in using the implicit version of (1.4 a), in the form

                          yk = yk−1 + h f (xk , yk ),      k = 1, 2, . . .                 (1.4 d)

This method also reappears as an example of the implicit theta Runge–Kutta method
(1.5 g) with θ = 1.
     In the calculation of yk in (1.4 d), we need to solve an algebraic equation

                             Y − h f (Y ) = C, where C = yk−1 .

If f satisﬁes a Lipschitz condition with |h|L < 1, then it is possible to use functional
iteration. That is, Y can be found numerically from the sequence Y [0] ,Y [1] ,Y [2] , . . . ,
where
                        Y [0] = C,
                          Y [n] = C + h f (Y [n−1] ),     n = 1, 2, . . .

To obtain rapid convergence, this simple iterative system can be replaced by the
quadratically-convergent Newton scheme:

     Y [0] = C,
                                           −1  [n−1]                    
     Y [n] = Y [n−1] − I − h f  (Y [n−1] )      Y      −C − h f (Y [n−1] ) ,   n = 1, 2, . . .



Experiments with the explicit Euler method

The Kepler problem
The Kepler problem (1.3 d), with initial value y0 = [1, 0, 0, 1]T , has a circular orbit
solution with period 2π. To see how well the Euler method is able to solve this
problem over a single orbit, a constant stepsize h = 2π/n is used over n steps in each
of the cases n = 1000 × 2k , k = 0, 1, . . . , 5. As a typical case, n = 2000 is shown in
the following diagrams, where the ﬁrst and second components are shown in the
left-hand diagram, and the third and fourth components on the right:
                              y2                                     y4



                                          y1                                    y3
1.4 The Euler method                                                                  17

   To assess the accuracy, in each of the six cases, it is convenient to calculate
yn − y0 2 . For example, if n = 1000, then

                    yn = [1.015572, −0.358194, 0.319112, 0.907596]T ,
               yn − y0 = [−0.015572, 0.358194, −0.319112, 0.092404]T ,
           yn − y0 2 = 0.488791.

This single result gives only limited information on the accuracy to be expected from
the Euler method when carrying out this type of calculation. It will be more interesting
to use the sequence of six n values, n = 1000, 2000, . . . .32000, with corresponding
stepsizes h = 2π/1000, 2π/2000, . . . , 2π/32000, displayed in a single diagram. As
we might expect, the additional work as n doubles repeatedly gives systematic
improvements. To illustrate the behaviour of this calculation for increasingly high
values of n, and increasingly low values of h, the following diagram is presented

                         error
                                10−0.5


                                   10−1
                                                            1

                                10−1.5
                                                     1


                                          10−3.5   10−3   10−2.5       10−2
                                                                   h


The triangle shown beside the main line suggests that the slope is close to 1.
   The slope of lines relating error to stepsize is of great importance since it predicts
the behaviour that could be expected for extremely small h. For example, if we needed
10−6 accuracy this ﬁgure suggests that we would need a stepsize of about 10−8 and
this would require a very large number of steps and therefore an unreasonable amount
of computer time. If, on the other hand, the slope were 2 or greater, we would obtain
much better performance for small h.


Experiments with diamond
In the case of the diamond problem, it is possible to evaluate the accumulated
error in a single orbit, evaluated using the Euler method. If n, the number of steps
to be evaluated, is a multiple of 4, there is zero error. We will consider the case
n = 4m + k, with m + k ≥ 4, where 1 ≤ k ≤ 3. Because the period is 4, the stepsize
is h = 4/n. In the ﬁrst quadrant, m + 1 steps moves the solution to the second
quadrant and a further m + 1 advances the solution to the interface with the third
quadrant. It then takes m + 2 steps to move to the fourth quadrant. This leaves
4m + k − 2(m + 1) − (m + 2) = m − (4 − k) steps to move within the fourth quadrant.
The ﬁnal position, relative to the initial point, is then
18                           1 Differential equations, numerical methods and algebraic analysis
                                               
m + 1 −1     m + 1 −1     m+2 1      m+k−4 1      4 k−4
           +            +          +            =         .
 n/4   1      n/4 −1      n/4 −1      n/4   1     n k−6

Computer simulations for this calculation can be misleading because of round-off
error.
Exercise 10 Find the error in calculating two orbits of diamond using n = 8m + k steps with
1 ≤ k ≤ 7, with m sufﬁciently large.


An example of Taylor series

From the many choices available to test the Taylor series method, we will look at the
initial value problem
                           y = x2 + y2 ,     y(0) = 1.                       (1.4 e)
In [55] (Henrici, 1962), this problem was used to illustrate the disadvantages of
Taylor series methods, because of rapid growth of the complexity of the formulae
for y , y(3) , . . . . This was in the relatively early days of digital computing, and the
situation has now changed because of the feasibility of evaluating Taylor terms
automatically.
   But going back to hand calculations, the higher-derivatives do indeed blossom in
complexity, as we see from the ﬁrst few members of the sequence
                    y = x2 + y2 ,
                    y = 2x + 2x2 y + 2y3 ,
                  y(3) = 2 + 4xy + 2x4 + 8x2 y2 + 6y4 ,
                  y(4) = 4y + 12x3 + 20xy2 + 20x4 y + 40x2 y3 + 24y5 .
Recursive computation of derivatives
Although we will not discuss the systematic evaluation of higher derivatives for a
general problem, we can at least ﬁnd a simple recursion for the example problem
(1.4 e), based on the formula

                              y(n) = ∂∂x y(n−1) + ∂∂y y(n−1) f .

This gives the sequence of formulae
                 y(1) = x2 + (y(0) )2 ,
                 y(2) = 2x + 2y(0) y(1) ,
                 y(3) = 2 + 2y(0) y(2) + 2(y(1) )2 ,
                 y(4) = 2y(0) y(3) + 6y(1) y(2) ,
                 y(5) = 2y(0) y(4) + 8y(1) y(3) + 6(y(2) )2 ,
                 y(6) = 2y(0) y(5) + 10y(1) y(4) + 20y(2) y(3) ,
                 y(7) = 2y(0) y(6) + 12y(1) y(5) + 30y(2) y(4) + 20(y(3) )2 ,
1.5 Runge–Kutta methods                                                                       19

            y
          1.0



          0.8
                                                                          p=4
                                                                             p=3
          0.6

                                                                              p=2
          0.4
                                                                              p=1


          0.2



          0.0
            0.0           0.2            0.4         0.6            0.8         1.0   x

      Figure 1 Taylor series approximations of orders p = 1, 2, 3, 4 for y = x2 + y2 ,
      y(0.2) = 0.3



and the general result
                                 n−1          
                         y(n) = ∑        n−1 (i) (n−1−i)
                                          i
                                             y y         ,        n ≥ 4.
                                 i=0

   To demonstrate how well the Taylor series works for this example problem,
Figure 1 is presented.



1.5 Runge–Kutta methods

One of the most widely used families of methods for approximating the solutions of
differential equations is the Runge–Kutta family. In one of these methods, a sequence
of n steps is taken from an initial point, x0 , to obtain an approximation to the solution
at x0 + nh, where h is the “stepsize”.
    Each step has the same form and we will consider only the ﬁrst. Write the input
approximation as y0 ≈ y(x0 ). The method involves ﬁrst obtaining approximations
Yi ≈ y(x0 + hci ), i = 1, 2, . . . , s, where c1 , c2 , . . . , cs are the stage abscissae. Write
Fi = f (Yi ) for each stage so that Fi ≈ y (x0 + hci ). The actual approximations used
for the stage values take the form

                         Yi = y0 + h ∑ ai j Fj ,       i = 1, 2, . . . , s.               (1.5 a)
                                         j<i
20                           1 Differential equations, numerical methods and algebraic analysis

After the stage values, Y1 , Y2 , . . . , and the stage derivatives, F1 , F2 , . . . , have been
evaluated, the output to the step is found from
                                                    s
                                     y1 = y0 + h ∑ bi Fi .                              (1.5 b)
                                                   i=1


Examples of explicit Runge–Kutta methods
The Runge second order methods
The method Runge-I is deﬁned by the equations

                      Y1 = y0 ,                         F1 = f (x0 ,Y1 ),
                      Y2 = y0 + hF1 ,                   F2 = f (x0 + h,Y2 ),            (1.5 c)
                      y1 = y0 + 12 (hF1 + hF2 ).

Because F1 ≈ y (x0 ) and F2 ≈ y (x1 ), (1.5 c) can be seen as a generalization of the
trapezoidal rule:
                                                                  
                   y(x0 + h) − y(x0 ) ≈ 12 hy (x0 ) + hy (x0 + h) .

     The method Runge-II is deﬁned by the equations

                         Y1 = y0 ,             F1 = f (x0 ,Y1 ),
                         Y2 = y0 + 12 hF1 ,    F2 = f (x0 + 12 h,Y2 ),                  (1.5 d)
                          y1 = y0 + hF2 .

Because F1 ≈ y (x0 ) and F2 ≈ y (x0 + 12 h), (1.5 c) can be seen as a generalization of
the midpoint rule:
                          y(x0 + h) − y(x0 ) ≈ hy (x0 + 12 h).

Third and fourth order methods
There are many possible methods with three stages and order three, and the following
is an example:
                 Y1 = y0 ,                   F1 = f (x0 ,Y1 ),
                 Y2 = y0 + 13 hF1 ,          F2 = f (x0 + 13 h,Y2 ),
                                                                               (1.5 e)
                 Y3 = y0 + 23 hF2 ,          F3 = f (x0 + 23 h,Y3 ),
                 y1 = y0 + 14 hF1 + 34 hF3 .
Similarly, the following four stage fourth order method is one of a large family:
                Y1 = y0 ,                                  F1 = f (x0 ,Y1 ),
                Y2 = y0 + 14 hF1 ,                         F2 = f (x0 + 14 h,Y2 ),
                Y3 = y0 + 12 hF2 ,                         F3 = f (x0 + 12 h,Y3 ),       (1.5 f)
                Y4 = y0 + hF1 − 2hF2 + 2hF3 ,              F4 = f (x0 + h,Y4 ),
                 y1 = y0 + 16 hF1 + 23 hF3 + 16 hF4 .
1.5 Runge–Kutta methods                                                                         21

Naive veriﬁcation of order

Although the criteria for order of a Runge–Kutta method are quite sophisticated, it is
possible to demonstrate why (1.5 c) and (1.5 d) each has order 2, using very simple
arguments. We will assume that f is a sufﬁciently smooth function so that we can
always use Taylor series in the form
                                                  n    i
                      y(x0 + h) = y(x0 ) + ∑ hi! y(i) (x0 ) + O(hn+1 ).
                                                 i=1

Thus for the method (1.5 c), we can write for the truncation error of Y2 , as an
approximation to y(x0 + h),

         y(x0 + h) −Y2 = y(x0 + h) − y(x0 ) − hy (x0 ) = 12 h2 y (x0 ) + O(h3 ).

Assuming the existence and smoothness of fy , we can also write

                  y (x0 + h) − F2 = f (x0 + h, y(x0 + h)) − f (x0 + h,Y2 )
                                     = 12 h2 fy (x0 , y0 )y (x0 ) + O(h3 )

For the truncation error in y1 , as an approximation to y(x0 + h), we have

             y(x0 + h) − y(x0 ) − 12 hF1 − 12 hF2
                                                                     
                = y(x0 + h) − y(x0 ) − 12 hy (x0 ) − 12 hy (x0 + h)
                                                                
                                      + 12 h y (x0 + h) − F2
                  
                = hy (x0 ) + 12 h2 y (x0 ) + 16 h3 y(3) (x0 )
                                                                                            
                          − 12 hy (x0 ) − 12 hy (x0 ) − 12 h2 y (x0 ) − 14 h3 y(3) (x0 )
                                                                          
                                        + 12 h 12 h2 fy (x0 , y0 )y (x0 ) + O(h4 )
                = − 12 h y (x0 ) + 14 h3 fy (x0 , y0 )y (x0 ) + O(h4 ).
                    1 3 (3)



Exercise 11 Find a similar error formula for (1.5 d).


Exercise 12 Show that the method (1.5 e) has order 3.


Exercise 13 Show that the method (1.5 f) has order 4.



Representing methods with tableaux

It is customary to represent a particular Runge–Kutta method using only the coefﬁ-
cients ai j , bi , ci appearing in (1.5 a, 1.5 b). These are, for the classical explicit case,
arranged in a tableau as follows
22                         1 Differential equations, numerical methods and algebraic analysis

                           0
                           c2       a21
                           c3       a31 a32
                            ..       ..  .. . .          .
                             .        .   .     .
                           cs       as1 as2 · · · as,s−1
                                    b1 b2 · · · bs−1 bs
For the methods we have already introduced, the corresponding tableaux are

                                    0
                                    1    1                                         method (1.5 c)
                                         1       1
                                         2       2

                                    0
                                    1    1
                                    2    2                                         method (1.5 d)
                                         0       1
                                    0
                                     1   1
                                     3   3                                         method (1.5 e)
                                     2           2
                                     3   0       3
                                         1               3
                                         4       0       4

                                     0
                                     1   1
                                     4   4
                                     1
                                     2   0 12                                      method (1.5 f)
                                     1   1 −2            2
                                         1               2   1
                                         6  0            3   6


Implicit Runge–Kutta methods

If the coefﬁcient matrix is full — that is, it contains non-zero elements on and above
the diagonal — the stages cannot be computed sequentially, and in order, using
explicit computations. Hence, an iteration scheme is normally required for their
evaluation. For example, the “theta methods” with tableaux of the form

                                             θ       θ
                                                                                          (1.5 g)
                                                     1

are explicit only if θ = 0. Two important special cases are θ = 12 (the implicit
mid-point rule method) and θ = 1 (the implicit Euler method). If f is sufﬁciently
smooth and h is sufﬁciently small, then the single stage Y is to be a solution of
Y = y0 + hθ f (Y ) and can be evaluated by functional iteration:

                     Y [0] = y0 ,
                                             
                     Y [k] = y0 + hθ f Y [k−1] ,                 k = 1, 2, . . .
1.5 Runge–Kutta methods                                                                23

For many problems, this iteration scheme is not efﬁcient, because of the severe
limitation that might need to be imposed on |h|, and some variant of Newton iteration
must be used.
   For s = 2, a well-known example of an implicit method is the so-called Radau
IIA method, with order 3, given by the tableau

                                              12 − 12
                                          1    5   1
                                          3
                                               3    1                              (1.5 h)
                                          1    4    4
                                               3    1
                                               4    4

A second fully-implicit method with s = 2 is known as a Gauss method and has order
p = 4 [54] (Hammer, Hollingsworth,1955). The tableau is
                               √                       √
                         2−6 3                   4−6 3
                         1   1           1       1   1
                               √         4
                                           √
                         1   1
                         2+6 3
                                     1   1
                                     4+6 3
                                                    1
                                                    4
                                                          .                 (1.5 i)
                                              1               1
                                              2               2

This is one of a family of methods based on Gaussian quadrature and with order
p = 2s [65] (Kuntzmann, 1961), [9] (Butcher, 1964).



Inverse and adjoint methods


The stages and ﬁnal output for a generic Runge–Kutta method, assuming input value
y0 , are given by
                                      s
                      Yi = y0 + h ∑ ai j f (Y j ),      i = 1, 2, . . . , s,       (1.5 j)
                                      j=1
                                       s
                     y1 = y0 + h ∑ b j f (Y j ).                                   (1.5 k)
                                      j=1


If y1 is already known, y0 can be found by solving from (1.5 k), and the Yi can be
found by subtracting (1.5 k) from (1.5 j). This gives the method

                                 s
                  Yi = y1 + h ∑ (ai j − b j ) f (Y j ),     i = 1, 2, . . . , s,
                                j=1
                                 s
                  y0 = y1 + h ∑ (−b j ) f (Y j ),
                                j=1


which exactly undoes the work of the original method. This leads to the deﬁnition
24                        1 Differential equations, numerical methods and algebraic analysis

 Deﬁnition 1.5A Given a tableau

                                          c      A
                                                 bT

 the inverse method (inverse tableau) is

                                c − (bT 1)1          A − 1bT
                                                      −bT


Closely related are “adjoint methods” in which the sign of h is changed in an inverse
method. For example, the adjoint method of (1.5 i) is

                          1 1
                               √           1             1 1
                                                               √
                          2+6 3                          4+6       3
                              √            4
                                             √
                          2−6 3          −
                          1 1          1   1               1
                                       4   6 3             4
                                           1               1
                                           2               2

which becomes identical with (1.5 i) if the stages are numbered in reverse order.
Methods with this property are “self adjoint” and have important properties computa-
tionally.



Methods with general index sets


The ﬂow of an autonomous initial value problem on the interval [0, 1] can be written
as the solution to the integral equation

                                          ξ              
               y(x0 + ξ h) = y0 + h           f y(x0 + ηh) d η
                                      0
                                          1                       
                           = y0 + h           H(ξ − η) f y(x0 + ηh) d η,
                                      0

where                                ⎧
                                     ⎪
                                     ⎨0, x < 0,
                                     ⎪
                               H(x) = 12 , x = 0,
                                     ⎪
                                     ⎪
                                     ⎩1, x > 0,

denotes the Heavyside function.
  This can be regarded as the continuous analogue of the s-stage Runge–Kutta
method with the coefﬁcient matrix given by
1.5 Runge–Kutta methods                                                                    25
                                                 ⎧
                                                 ⎪
                                                 ⎪                i < j,
                                                 ⎨0,
                                              1
                                      ai j = 2s ,                 i = j,
                                            ⎪
                                            ⎪
                                            ⎩1,                   i > j.
                                              s

It is possible to place these two methods on a common basis by introducing an “index
set” I [14] (Butcher, 1972) which, in these examples, could be [0, 1] or {1, 2, 3, . . . , s}.
Adapting Runge–Kutta terminology slightly, the stage value function becomes a
bounded mapping I → RN and the coefﬁcient matrix A becomes a bounded linear
operator on the space of bounded mappings I → R to this same space. The ﬁnal
component of a Runge–Kutta method speciﬁcation, that is the row vector bT , becomes
a linear functional on the bounded mappings I → R. More details will be presented
in Chapter 4.
    Even though energy-preserving Runge–Kutta methods, with ﬁnite I, do not exist,
the following method, the “Average Vector Field” method [80] (Quispel, McLaren,
2008) ) does satisfy this requirement [29] (Celledoni et al, 2009).

                                             1                   
                        y 1 = y0 + h             f (1 − η)y0 + ηy1 d η.
                                         0

For this method we have
                                         I = [0, 1],
                                                              1
                                 A(ξ )φ = ξ                       φ (η) d η,
                                                          0
                                                         1
                                      bT φ =              φ (η) d η.
                                                     0

Methods based on the index set [0, 1] are referred to as “Continuous stage Runge–
Kutta methods”.


Equivalence classes of Runge–Kutta methods


The two Runge–Kutta methods

                            0                                        0
                             1    1
                             2    2                  ,               1
                                                                     2
                                                                           1
                                                                           2
                            1    0      1
                                 0      1        0                         0   1

are equivalent in the sense that they give identical results because the third stage of
the method on the left is evaluated and never used. This is an example of Dahlquist–
Jeltsch equivalence [39] (Dahlquist, Jeltsch, 2006). Similarly the two implicit meth-
ods
26                              1 Differential equations, numerical methods and algebraic analysis
            √                               √                    √                            √
      2−6 3                          4−6
      1 1                 1          1 1                     1 1             1          1 1
                                                 3           2+6 3                      4+6       3
          √               4
                            √                                    √           4
                                                                               √
      1 1
      2+6 3
                      1
                      4 + 1
                          6 3
                                       1
                                       4
                                                     ,       2−6 3
                                                             1 1         1
                                                                         4 − 1
                                                                             6 3
                                                                                          1
                                                                                          4
                          1            1                                     1            1
                          2            2                                     2            2

are equivalent because they are the same method with their stages numbered in a
different order.
   Another example of an equivalent pair of methods, is

                 1
                 3
                            1
                            3
                                     1
                                    12   − 12
                                           1
                                                         0
                 1
                         − 12
                           1        1       1
                                                     − 16
                                                                     1
                                                                     3
                                                                           5
                                                                           12   − 12
                                                                                   1
                 3                  2       12
                 1          1        1      1
                                                     − 14    ,       1     3
                                                                           4
                                                                                    1
                                                                                    4
                                                                                        .
                            2       4       2
                            1        1      1           1                  3        1
                 1          4       2       8           8                  4        4
                            3
                            8       8
                                     3      1
                                            3        − 12
                                                       1


Suppose Y1∗ , Y2∗ are the solutions computed using the method on the right. Then
Y1 = Y2 = Y1∗ and Y3 = Y4 = Y2∗ satisfy the stage conditions for the method on the
left. Hence, the outputs for each of the methods are equal to the same result

                y1 = y0 + 38 h f (Y1 ) + 38 h f (Y2 ) + 13 h f (Y3 ) − 12
                                                                        1
                                                                          h f (Y4 )
                     = y0 + 38 h f (Y1∗ ) + 38 h f (Y1∗ ) + 13 h f (Y2∗ ) − 12
                                                                            1
                                                                               h f (Y2∗ )
                     = y0 + 34 h f (Y1∗ ) + 14 h f (Y2∗ ).

This is an example of Hundsdorfer–Spijker reducibility [58] (Hundsdorfer, Spijker,
1981).



Experiments with Runge–Kutta methods


The advantages of high order methods

As methods of higher and higher order are used, the cost also increases because the
number of f evaluations increases with the number of stages. But using a high order
method is usually an advantage over a low order method if sufﬁcient precision is
required.
   We will illustrate this in Figure 2, where a single half-orbit of the Kepler problem
with zero eccentricity is solved using four Runge–Kutta methods ranging from the
order 1 Euler method to the methods (1.5 c), (1.5 e) and (1.5 f). The orders of the
methods are attached to the plots of their error versus h behaviours on a log-log
scale. Also shown are triangles showing the exact slopes for comparison.
1.5 Runge–Kutta methods                                                                    27


                                                                  p=1

                   error
                                                          1                2
                                                                  p=
                                                  1
                        10−3


                                                          2                3
                                                                  p=
                        10−6                      1
                                                      1                    4
                                                                       =
                                              3                    p
                        10−9


                                                              4

                       10−12                              1

                                           10−3               10−2             h

    Figure 2 Error behaviour for Runge–Kutta methods with orders p = 1, 2, 3, 4, for the
    Kepler problem with zero eccentricity on the time interval [0, π]




Methods for stiff problems

The aim in stiff methods is to avoid undue restriction on stepsize for stability reasons
but at the same time, to avoid excessive computational cost. In this brief introduction
we will compare two methods from the points of view of stepsize restriction, accuracy
and cost.
   The methods are the third order explicit method (1.5 e) and the implicit Radau
IIA method (1.5 h). In each case the problem (1.3 b) (p. 10) was solved with output
at x = 1 taking n steps with n ranging from 1 to 51200. The dependence of the
computational error on n, and therefore on h = 1/n is shown in the Figure 3, where
the method used in each result is attached to the curve. Note that the error in the
computation is only for a representative component y1 .
   From the ﬁgure we see that the output for the explicit method is useless unless
h < 0.02, approximately. This is a direct consequence of the stiffness of the problem.
But for the implicit Radau IIA method, there is no constraint on the stepsize except
that imposed by the need to obtain sufﬁcient accuracy. Because the computational
cost is much greater for the implicit method, many scientists and engineers are willing
to use explicit methods in spite of their unstable behaviour and the need to use small
stepsizes.
28                             1 Differential equations, numerical methods and algebraic analysis



            |error|
                10−3



                10−6                               t
                                               ici
                                             pl
                                          ex           ici
                                                          t
                                                  pl
                                              im
                10−9



                10−12


                              10−4         10−3               10−2   10−1          1
                                                                                       h

     Figure 3 Errors for the stiff problem (1.3 b), solved by an explicit and an implicit method



1.6 Multivalue methods

Linear multistep methods

Instead of calculating a number of stages in working from yn−1 to yn , a linear
multistep method makes use of past information evaluated in previous steps. That is,
yn is found from

          yn = a1 yn−1 + · · · + ak yn−k + hb1 f (yn−1 ) + · · · + hbk f (yn−k ).             (1.6 a)

In this terminology we will always assume that |ak | + |bk | > 0 because, if this were
not the case, k could be replaced by a lower positive integer. With this understanding,
we refer to this as a k-step method. The “explicit case” (1.6 a) is generalized in (1.6 c)
below.
   In the k-step method (1.6 a), the quantities ai , bi , i = 1, 2, . . . , k, are numbers
chosen to obtain suitable numerical properties of the method. It is convenient to
introduce polynomials ρ, σ deﬁned by

                               ρ(w) = wk − a1 wk−1 − · · · − ak ,
                                                                                              (1.6 b)
                               σ (w) = b1 wk−1 + · · · + bk ,

so that the method can be referred to as (ρ, σ ) [36] (Dahlquist, 1956).
   The class of methods in this formulation can be extended slightly by adding a
term hb0 f (yn ) to the right-hand side of (1.6 a) or, equivalently, a term b0 wk to the
expression for σ (w). Computationally, this means that yn is deﬁned implicitly as the
1.6 Multivalue methods                                                                       29

solution to the equation

     yn − hb0 f (yn ) = a1 yn−1 + · · · + ak yn−k + hb1 f (yn−1 ) + · · · + hbk f (yn−k ).

In this case, (1.6 b) is replaced by

                           ρ(w) = wk − a1 wk−1 − · · · − ak ,
                                                                                         (1.6 c)
                           σ (w) = b0 wk + b1 wk−1 + · · · + bk .

The most well-known examples of (1.6 b) are the Adams–Bashforth methods [3]
(Bashforth, Adams,1883), for which ρ(w) = wk − wk−1 and the coefﬁcients in σ (w)
are chosen to obtain order p = k. Similarly, the well-known Adams–Moulton methods
[74] (Moulton, 1926) also have ρ(w) = wk − wk−1 in (1.6 c), but the coefﬁcients in
σ (w) are chosen to obtain order p = k + 1.


Consistency, stability and convergence


 Deﬁnition 1.6A A method (ρ, σ ) is preconsistent if ρ(1) = 0. The method is
 consistent if it is preconsistent and also ρ  (1) = σ (1).

The signiﬁcance of Deﬁnition 1.6A is that for the problem y (x) = 0, y(0) = 1, if
yn−i = 1, i = 1, 2, . . . , k, then the value computed by the method in step number n is
also equal to the correct value yn = 1 if and only if ∑ki=1 ai = 1, which is equivalent
to preconsistency. Furthermore, if the method is preconsistent and is used to solve
y (x) = 1, y(0) = 0, and the values yn−i = h(n − i) then the result computed in step n
has the correct value yn = nh if and only if nh = ∑ki=1 h(n − i)ai + h ∑ki=0 bi , which is
equivalent to the consistency condition, k − ∑ki=1 (k − i)ai = ∑ki=0 bi .

 Deﬁnition 1.6B A method (ρ, σ ) is stable if all solutions of the difference equa-
 tion
                        yn = a1 yn−1 + · · · + ak yn−k
 are bounded.



 Deﬁnition 1.6C A polynomial ρ satisﬁes the root condition if all zeros are in the
 closed unit disc and all multiple zeros are in the open unit disc.

The following result follows from the elementary theory of linear difference equa-
tions

 Theorem 1.6D A method (ρ, σ ) is stable if and only if ρ satisﬁes the root condi-
 tion.
30                            1 Differential equations, numerical methods and algebraic analysis

Exercise 14 Find the values of a1 and b1 for which the method (w2 − a1 w + 12 , b1 w + 1) is
consistent. Is the resulting method stable?



Order of linear multistep methods
Dahlquist [36] (Dahlquist, 1956) has shown that

 Theorem 1.6E Given ρ(1) = 0, the pair (ρ, σ ) has order p if and only if

                                            ρ(1 + z)/z
                             σ (1 + z) =                + O(z p ),
                                            ln(1 + z)/z

 where ln denotes the principal value so that ln(1 + z)/z = 1 + O(z).

For convenience in applications of this result, note that
         1           1      1 2    1 3    19 4     3 5     863 6     275 7
     ln(1+z)/z = 1 + 2 z − 12 z + 24 z − 720 z + 160 z − 60480 z + 24192 z
                       33953 8       8183 9     3663197 10
                   − 3628800   z + 1036800 z + 43545600 z + O(z11 ).



Examples of linear multistep methods
The Euler method can be deﬁned by ρ(w) = w − 1, σ (w) = 1 and is the ﬁrst member
of the Adams–Bashforth family of methods [3] (Bashforth, Adams, 1883) The next
member is deﬁned by

                          ρ(w) = w2 − w,           σ (w) = 32 w − 12 ,

because
                                         ρ(1+z)
                           σ (1 + z) =      z    (1 + 12 z) + O(z2 )
                                      = (1 + z)(1 + 12 z) + O(z2 )
                                      = 1 + 32 z
                                      = 32 w − 12 ,     (w = 1 + z)

and has order 2 if correctly implemented. By this is meant the deﬁnition of y1 which
is required, in addition to y0 , to enable later values of the sequence of y values to be
computed. A simple choice is to deﬁne y1 by a second order Runge–Kutta method,
such as (1.5 c) or (1.5 d).

Exercise 15 Show that the order 3 Adams-Bashforth method is deﬁned by ρ(w) = w3 − w2 ,
        12 w − 3 w + 12 .
σ (w) = 23  2  4      5
1.6 Multivalue methods                                                                                    31

  Adams–Moulton methods [74] (Moulton, 1926) are found in a similar way to
Adams–Bashforth methods, except that σ (1 + z) is permitted to have a term in zk .
For k = 2 and k = 3, we have in turn ρ(w) = w − 1 = z, ρ(w) = w2 − w = (1 + z)z,
where we will always write w = 1 + z. The formulae for σ (w) are, respectively

   σ (w) = 1 + 12 z = 12 w + 12 ,                                                              (k = 2),
   σ (w) = (1 + z)(1 + 12 z − 12
                              1 2                 5 2
                                 z ) = 1 + 32 z + 12     5 2
                                                     z = 12 w + 23 w − 12
                                                                       1
                                                                          ,                    (k = 3).


Exercise 16 Show that the order 4 Adams-Moulton method is deﬁned by ρ(w) = w3 − w2 ,
                24 w − 24 w + 24 .
σ (w) = 38 w3 + 19  2  5      1




General linear methods

Traditionally, practical numerical methods for differential equations are classiﬁed
into Runge–Kutta methods and linear multistep methods.
      Combining these two families of methods into a single family gives methods
characterized by two complexity parameters r, the number of quantities passed from
step to step, and s, the number of stages. As for Runge–Kutta methods, the stages
will be denoted by Y1 , Y2 , . . . ,Ys and the corresponding stage derivatives by F1 , F2 ,
                                                                             [n−1] [n−1]
. . . , Fs . The r components of input to step number n will be denoted by y1 , y2 ,
         [n−1]                                      [n] [n]      [n]
. . . , yr , and the output from this step by y1 , y2 , . . . , yr . These quantities are
interrelated in terms of a partitioned (s + r) × (s + r) matrix
                                                   
                                            A U
                                              B       V

using the equations
                       s            r
                                           [n−1]
              Yi = h ∑ ai j Fj + ∑ ui j y j        ,      Fi = f (Yi ),   i = 1, 2, . . . s,
                      j=1           j=1
                       s             r
              [n]                         [n−1]
             yi = h ∑ bi j Fj + ∑ vi j y j        ,                       i = 1, 2, . . . r.
                      j=1           j=1

The essential part of these relations can be written more compactly as

                              Y = h(A ⊗ I)F + (U ⊗ I)y[n−1] ,
                             y[n] = h(B ⊗ I)F + (V ⊗ I)y[n−1] ,

or, if no confusion is possible, as

                                     Y = hAF +Uy[n−1] ,
                                    y[n] = hBF +V y[n−1] .
32                          1 Differential equations, numerical methods and algebraic analysis

Consistency, stability and convergence

Generalizing the ideas of consistency to general linear methods is complicated
by the lack of a single obvious interpretation of the information passed between
steps of the method. However, we will try to aim for an interpretation in which
y[n−1] = uy(xn−1 ) + hvy (xn−1 ) + O(h2 ) for some u, v ∈ RN with the parameters
chosen so that at the completion of the step, y[n] = uy(xn ) + hvy (xn ) + O(h2 ), and
also so that the stage values satisfy Yi = y(xn−1 ) + O(h).
   We will explore the consequences of these assumptions by analysing the case
n = 1. We ﬁnd in turn
                                                              
                   1y(x0 ) = hAy (x0 ) +U uy(x0 ) + hvy (x0 ) + O(h),
                          U1 = 1,
                                                                
         u(y(x0 ) + hy (x0 ) = hB 1y (x0 ) +V uy(x0 ) + hvy (x0 ) + O(h2 ).
                      


For Runge–Kutta methods, there is only a single input and accordingly, r = 1. For
the method (1.5 f) the deﬁning matrices are
                                   ⎡                      ⎤
                                       0 0 0 0 1
                                   ⎢                      ⎥
                               ⎢ 1 0 0 0 1 ⎥
                                   ⎢   4                  ⎥
                        A U        ⎢                      ⎥
                                = ⎢ 0 12 0 0 1 ⎥ .
                        B V        ⎢                      ⎥
                                   ⎢ 1 −2 2 0 1 ⎥
                                   ⎣                      ⎦
                                       1      2    1
                                       6    0 3    6   1

By contrast, for a linear multistep method, s = 1. In the case of the order 3 Adams–
Bashforth method, the deﬁning matrices are
                                    ⎡                         ⎤
                                       0 1 23       − 4     5
                                    ⎢           12    3    12 ⎥
                                ⎢ ⎢  0   1    23
                                                    − 4     5 ⎥
                                                           12 ⎥
                        A U         ⎢           12    3
                                                              ⎥
                                  =⎢⎢  1 0 0 0 0 ⎥            ⎥.
                        B V         ⎢                         ⎥
                                    ⎢ 0 0 1 0 0 ⎥
                                    ⎣                         ⎦
                                          0    0    0     1     0

   Moving away from traditional methods consider the method with r = 2, s = 3,
with matrices                    ⎡                    ⎤
                                   0 0 0 1 0
                                 ⎢                    ⎥
                            ⎢     1
                                 ⎢ 2 0 0 1 1 ⎥
                                                      ⎥
                     A U         ⎢                    ⎥
                              =⎢ ⎢ 0 1 0 1 0 ⎥        ⎥.               (1.6 d)
                     B V         ⎢ 1 2 1              ⎥
                                 ⎢                    ⎥
                                 ⎣ 6 3 6 1 0 ⎦
                                    4 −4
                                    1   3   1
                                            2   0 0
1.7 B-series analysis of numerical methods                                               33

For a person acquainted only with traditional Runge–Kutta and linear multistep
methods, (1.6 d) might seem surprising. However, it is for the analysis of methods
like this that the theory of B-series has a natural role. In particular, we note that if the
                                                 [n]
method is started in a suitable manner, then y1 ≈ y(xn ) to a similar accuracy as for
the fourth order Runge–Kutta method. One possible starting scheme is based on the
tableau
                                        0
                                             1     1
                                 Rh =        2
                                             1
                                                   2
                                                        1
                                                                  .
                                             2     0    2
                                                 − 14   1
                                                        8
                                                              1
                                                              8


Starting with the initial value y0 , the initial y[0] can be computed by
                                       [0]
                                      y1 = y0 ,
                                                                                     (1.6 e)
                                       [0]
                                      y2 = R h y0 − y0 .

   In Chapter 6, Section 6.4 (p. 225), the method (1.6 d), together with (1.6 e) as
starting method, will be used as an illustrative example.



1.7 B-series analysis of numerical methods

Higher derivative methods

The Euler method was introduced in Section 1.4 (p. 14) as the ﬁrst order case of the
Taylor series method. The more sophisticated methods are attempts to improve this
basic approximation method.
   The practical advantage of methods which require the evaluation of higher deriva-
tives hinges on the relative cost of these evaluations compared with the cost of just
the ﬁrst derivative. But there are other reasons for obtaining formulae for higher
derivatives in a systematic way; these are that this information is required for the
analysis of so-called B-series.
   For a given autonomous problem,
                        
          y (x) = f y(x) , y(x0 ) = y0 ,      y : R → R N , f : R N → RN ,

written in component by component form

                      d yi
                      d x = f (y , y , . . . , y ),         i = 1, 2, . . . , N,
                             i 1 2              N


we will ﬁnd a formula for the second derivative of yi . This can be obtained by
the chain-rule followed by a substitution of the known ﬁrst derivative of a generic
34                               1 Differential equations, numerical methods and algebraic analysis

component f j . That is,
                                                      N
                                          d2 yi            ∂ f i dyj
                                                =    ∑ ∂yj dx
                                          d x2
                                                     j=1
                                                      N
                                                           ∂ fi
                                                 = ∑ ∂ y j f j.
                                                     j=1

This can be written in a more compact form by using subscripts to indicate partial y
derivatives. That is, f ji := ∂ f i /∂ y j . A further simpliﬁcation results by adopting the
“summation convention”, in which repeated sufﬁxes in expressions like f ji f j imply
summation, without this being written explicitly. Hence, we can write

                                               d2 yi
                                                     = f ji f j .
                                               d x2

Take this further and ﬁnd formulae for the third and fourth derivatives
               d 3 yi
                             f f + f ji fkj f k ,
                          i j k
                      = f jk
               d x3
               d 4 yi     i
                      = f jk f j f k f  + 3 f jk
                                                i j k 
                                                   f f f + f ji fkj f k f  + f ji fkj fk f  .
               d x4

From the sequence of derivatives, evaluated at y0 , the Taylor series can be evaluated.
   In further developments, we will avoid the use of partial derivatives, in favour of
                                                                i , . . . , we will use the total
Fréchet derivatives. That is, in place of the tensors f ji , f jk
                
derivatives f , f , . . . . Evaluated at y0 , these will be denoted by

                                                f = f (y0 ),
                                               f  = f  (y0 ),
                                               f  = f  (y0 ),
                                                  ..     ..
                                                   .      .


Formal Taylor series

The ﬁrst few terms of the formal Taylor series for the solution at x = x0 + h are

                y(x0 + h) = y0 + hf + 12 h2 f  f + 16 h3 f  ff + 16 h3 f  f  f + · · ·          (1.7 a)


Application to the theta method

The result computed by the theta method (1.5 g) (p. 22) has a Taylor expansion, with
a resemblance to (1.7 a). That is,

                  y1 = y0 + hf + θ h2 f  f + 12 θ 2 h3 f  ff + θ 2 h3 f  f  f + · · ·           (1.7 b)
1.7 B-series analysis of numerical methods                                             35

A comparison of (1.7 a) and (1.7 b) suggests that the error in approximating the exact
solution by the theta method is O(h2 ) for θ = 12 and O(h3 ) for θ = 12 . Useful though
this observation might be, it is just the start of the story. We want to be able to carry
out straight-forward analyses of methods using this type of “B-series” expansion.
We want to be able to do manipulations of B-series as symbolic counterparts to the
computational equations deﬁning the result, and the steps leading to this result, in a
wide range of numerical methods.

Elementary differentials and trees

The expressions f, f  f, f  ff and f  f  f are examples of “elementary differentials”
and, symbolically, they have a graph-theoretical analogue. Corresponding to f is an
individual in a genealogical tree; corresponding to f  is an individual with a link to a
possible child. The term f  f corresponds to this link having been made to the child
represented by f. The bi-linear operator f  corresponds to an individual with two
possible links and in f  ff these links are ﬁlled with copies of the child represented
by f.
   Finally, in these preliminary remarks, f  f  f corresponds to a three generation
family with the ﬁrst f  playing the role of grandparent, the second f  playing the role
of a parent, and the child of the grandparent; and the ﬁnal operand f playing the role
of grandchild and child, respectively, of the preceding f  operators.
   The relationship between elementary differentials and trees can be illustrated in
diagrams.
                                                                  f
                                      f              f                f    f
                          f           f                       f         f
We can extend these ideas to trees and elementary differentials of arbitrary complexity,
as shown in the diagram

                                                                      f
                                       f f f      f                   f
                                       f f f 
                                                                      f
                                                     f (4)
   The elementary differential corresponding to this diagram can be written in
a variety of ways. For instance one can insert spaces to emphasize the separation
between the four operands of f (4) , or use power notation to indicate repeated operands
and operators:
                                  f (4) f  ff  ff  fff  f  f
                                = f (4) f  f f  f f  ff f  f  f
                                = f (4) (f  f)2 f  f 2 f  f  f
                                = f (4) (f  f)2 f  f 2 f  f  f
36                                        1 Differential equations, numerical methods and algebraic analysis

As further examples, we show the trees with four vertices, together with the corre-
sponding elementary differentials:




                              f (3) f 3        f  ff  f       f  f  f 2       f f f f


Exercise 17 Find the trees corresponding to each of the elementary differentials:
(a) f  (f  f)2 , (b) f (4) f 3 f  f, (c) f  f  f 2 f  f.

Exercise 18 Find the elementary differentials corresponding to each of the trees:
(a)   , (b)       , (c)   .




Summary of Chapter 1 and the way forward
Summary

Although this book is focussed on the algebraic analysis of numerical methods, a
good background in both ordinary differential equations and numerical methods for
their solution is essential.
   In this chapter a very basic survey of these important topics has been presented.
That is, the fundamental theory of initial value problems is discussed, partly through a
range of test problems. These problems arise from standard physical modelling, with
the addition of a number of contrived and artiﬁcial problems. This is then followed
by a brief look at the classical one-step and linear multistep methods, and an even
briefer look at some all encompassing multivalue-multistage methods (“general
linear methods”). Some of the methods are accompanied by numerical examples,
underlining some of their properties.
   As a preview for later chapters, B-series are brieﬂy introduced, along with trees
and elementary differentials.


The way forward

The current chapter includes preliminary notes on some of the later chapters. This is
indicated in the following diagram by a dotted line pointing to these speciﬁc chapters.
A full line pointing between chapters indicates a stronger prerequisite.

                                                                                5

              1                  2             3             4                                   7

                                                                                6
1.7 B-series analysis of numerical methods                                                      37

Teaching and study notes

It is a good idea to supplement the reading of this chapter using some of the many
books available on this subject. Those best known to the present author are
Ascher, U.M. and Petzold, L.R. Computer Methods for Ordinary Differential Equa-
tions and Differential-Algebraic Equations (1998) [1]
Butcher, J.C. Numerical Methods for Ordinary Differential Equations (2016) [20]
Gear, C.W. The Numerical Integration of Ordinary Differential Equations (1967)
[44]
Hairer, E., Nørsett, S.P. and Wanner, G. Solving Ordinary Differential Equations
I: Nonstiff Problems (1993) [50]
Hairer, E. and Wanner G. Solving Ordinary Differential Equations II: Stiff and
Differential-Algebraic Problems (1996) [53]
Henrici, P. Discrete Variable Methods in Ordinary Differential Equations (1962)
[55]
Iserles, A. A First Course in the Numerical Analysis of Differential Equations (2008)
[61]
Lambert, J.D. Numerical Methods for Ordinary Differential Systems (1991) [67]

Projects
Project 1 Explore existence and uniqueness questions for problems satisfying a local Lipschitz
condition.
Project 2 Find numerical solutions, using a variety of methods, for the simple pendulum. Some
questions to ask are (i) does the quality of the approximations deteriorate with increased initial
energy? and (ii) how well preserved is the Hamiltonian.?
Project 3   Learn all you can about fourth order Runge–Kutta methods.
Project 4   Read about predictor-corrector methods in [67] or some other text-book.
