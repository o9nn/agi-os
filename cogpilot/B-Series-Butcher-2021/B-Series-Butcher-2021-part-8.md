# B-Series: Algebraic Analysis of Numerical Methods - Part 8

**Author:** John C. Butcher

**Series:** Springer Series in Computational Mathematics, Volume 55

---

Chapter 7
B-series and geometric integration




7.1 Introduction

In geometric integration, the success of a numerical method is judged, not only
by its order of accuracy, but also by its ability to preserve structural properties of
speciﬁc problems and problem classes. For example, numerical methods for the
Kepler problem are regarded as preferable if they respect the conservation of energy
and of angular momentum. More complicated gravitational models, as used in the
simulations of the solar system over extended time periods, will inevitably drift from
the true solution but the principles of geometric integration require us to understand
and limit the loss of integrity of important physical characteristics, both qualitative
and quantitative. The practical aim will be to control, and possibly even eliminate
completely, the drift away from exact preservation of invariants.
   Geometric integration has grown into a signiﬁcant subject in its own right. This
short review will comment on just a handful of questions, each of which has a close
connection with B-series, and possibly some connection with integration methods
in the sense of Chapter 4. Reference is made to the treatise [49] (Hairer, Lubich,
Wanner, 2006), for a broad coverage of geometric integration.


Chapter outline

In Section 7.2 we will discuss some of the problem classes for which conservation
properties are fundamentally important.
   We then devote several sections to numerical methods which are known to be
successful in solving many of these problems in a geometric manner. The ﬁrst
of these, introduced in Section 7.3, focusses on canonical and symplectic Runge–
Kutta methods. The theoretical questions are expressed in B-series terms and the
development leads to important methods and families of methods. Also, Runge–Kutta
methods which use “processing” to obtain improved performance are discussed.

© Springer Nature Switzerland AG 2021                                              247
J. C. Butcher, B-Series, Springer Series in Computational Mathematics 55,
https://doi.org/10.1007/978-3-030-70956-3_7
248                                                     7 B-series and geometric integration

    The so-called “G-symplectic methods”, introduced in Section 7.4, represent an
attempt to generalize symplectic properties to a multivalue setting. B-series play a
central role in the development of these relatively new methods and this application
illustrates many of the techniques and theoretical B-series results. However, a detailed
study of G-symplectic methods aims for more than this.
   The methods actually constructed are more efﬁcient than Runge–Kutta methods,
of comparable order and accuracy, and their fundamental limitation, the destruction
of useful accuracy because of incipient parasitism, does not seem to manifest itself
within a large time-period, covering millions of time steps. In particular, Section
7.6 outlines the derivation of a sixth order method which satisﬁes many of the
requirements of slow loss of integrity, because of parasitism.
   The simulations presented in Section 7.8 provide some reasons for conﬁdence in
the quality of the methods, in spite of the evident dangers.
   Section 7.9 contains an introduction to energy preserving methods. It is known
that Runge–Kutta methods cannot preserve energy but, as exempliﬁed by the Average
Vector Field method, integration methods, in the sense of Chapter 4, also known
as Continuous Stage Runge–Kutta methods, overcome this limitation. For these
methods, necessary and sufﬁcient conditions based on B-series are given for energy
preservation. For continuous methods in general, a sufﬁcient condition for energy
preservation is given.



7.2 Hamiltonian and related problems

Quadratic invariants

Inequality

In the study of non-linear stability, differential equations on an inner-product space
were considered in [15] (Butcher, 75) with special application to Runge–Kutta
methods (see Section 7.3 (p. 252)). For the system

                            y (x) = f (y),      y(x0 ) = y0 ,

where                               6       7
                                    f (Y ),Y ≤ 0,                                   (7.2 a)
                       86           7
the value of y(x) :=    y(x), y(x) is bounded for all time, because

                           d 6          7   6           7
                          d x y(x), y(x) = 2 y (x), y(x)
                                             6            7
                                          = 2 f y(x) , y(x)                         (7.2 b)
                                          ≤ 0.
7.2 Hamiltonian and related problems                                              249

If the inner product in (7.2 a) is replaced by a positive smi-deﬁniite iinner product,
so that the norm becomes a semi-norm, the non-increasing property of y(x)2 still
holds.
    The way this generalization can give a useful stability condition is based on the
2N-dimensional system
                            y (x) = f (y),    y(x0 ) = y0 ,
                             z (x) = f (z),        z(x0 ) = z0 .

This models the behaviour of y(x) − z(x)2 for increasing x, using the indeﬁnite
inner product
              9           :
                 Y       Y
                     ,          := Y, Y  − Y, Z
                                                   − Z, Y  + Z, Z.
                                                                     
                 Z        Z

The indeﬁnite inner product is based on the partitioned symmetric positive indeﬁnite
matrix                                       
                                       I −I
                                                ,
                                     −I I
and the stability rule becomes

                               y(x) − z(x) ≤ y0 − z0 .


Equality
If (7.2 a) is replaced by              6           7
                                           f (Y ),Y = 0,
then (7.2 b) becomes
                                    d 6          7
                                   d x y(x), y(x) = 0
and y(x) is constant over all time.
   In the case of exact conservation of a quadratic form, there is no special reason
why this should be a positive deﬁnite, or even a positive semi-deﬁnite, form and we
will therefore look at problems for which

                                       [Y, f (y)]Q = 0,                        (7.2 c)

with
                                   [Y,Y ]Q := Y, QY ,
for Q symmetric.
   A particular family of problems which satisfy (7.2 c) are of the form

                                           y = S(y)y,                         (7.2 d)

where S(y) is skew-symmetric. In this case, Q is the identity matrix.
250                                                     7 B-series and geometric integration

Euler rigid body rotations

Two famous examples of (7.2 d) are derived from the Euler equations of rigid body
rotation in the absence of external moments:
                                   Au = (B −C)vw,
                                   Bv = (C − A)wu,                                 (7.2 e)
                                      
                                   Cw = (A − B)uv,

with A, B,C the (positive) principal moments of inertia and u, v, w the components of
angular velocity about the principal inertial
                                           √ axes in the√ body. The √ conservation of
rotational energy is found by writing y1 = Au, y2 = Bv, y3 = Cw. We have
                        ⎡             8        8         ⎤
                        ⎢      0     − AB y
                                         C 3       B 2
                                                  AC y ⎥
                        ⎢ 8                    8         ⎥
                        ⎢                                ⎥
                     
                   y =⎢ ⎢      C   3          − BC y ⎥
                                                   A   1
                               AB y      0               ⎥ y = Sy,
                        ⎢ 8           8                  ⎥
                        ⎣                                ⎦
                         − AC yB 2       A 1
                                        BC y       0

with S skew-symmetric, so that 12 [y, y] = 12 (Au2 + Bv2 + Cw2 ) is conserved. The
conservation of the norm of the angular momentum vector form of the equations is
found from y1 = Au, y2 = Bv, y3 = Cw, with
                               ⎡                      ⎤
                                    0       y − B1 y2
                                          1 3
                               ⎢          C           ⎥
                               ⎢                 1 1 ⎥
                          y = ⎢− C1 y3    0       y  ⎥ y,
                               ⎣                 A    ⎦
                                   1 2
                                   B y  − 1 1
                                          A y     0




Poisson and Hamiltonian problems


Poisson systems

A Poisson system has the form

                                 y = S∇H := S(H  )T ,

where H, the Hamiltonian, is a function of y and S is skew-symmetric and may also
depend on y. For such a system, H is conserved because

                             d
                                H = H  y = H  S(H  )T = 0,
                             dx
7.2 Hamiltonian and related problems                                         251
                                                                               T
because of the skew-symmetry of S. In the case of (7.2 e), deﬁne y = u v w         ,
and this problem can be formulated in Poisson form in two different ways:
                                                     ⎡                  ⎤
                                                               Cy3 By2
                                                           0 − AB AC
                                                     ⎢                  ⎥
                                                     ⎢                  ⎥
                         1 T
         y = S∇H, H = 2 y diag(A, B,C)y,         S=⎢     Cy3
                                                     ⎢ AB 0 − BC ⎥
                                                                    Ay1 ⎥ ,
                                                     ⎣                  ⎦
                                                             2 Ay1
                                                       − By
                                                          AC BC      0
                                                     ⎡                  ⎤
                                                               y3   y2
                                                          0 AB    − AC
                                                     ⎢                  ⎥
                                                     ⎢ 3             1 ⎥
                         1 T       2 2 2            ⎢
         y = S∇H, H = 2 y diag(A , B ,C )y, S = ⎢− AB 0 BC ⎥
                                                          y         y
                                                                        ⎥.
                                                     ⎣                  ⎦
                                                          y2   y1
                                                         AC − BC     0

The Poisson formulation is a direct path to the conservation laws of rigid body
rotation.


Hamiltonian problems
The Hamiltonian formulation of classical mechanics is a special case of a Poisson
system based on a “Hamiltonian function”, H(y), and the governing equations take
the form
                                  y = S∇H,
where S is skew-symmetric, constant and non-singular. The non-singularity of S
implies that the dimension N is even. Write N = 2n.
   To adhere to the classical formulation of Hamiltonian mechanics, attention is
restricted to the special case
                                                       
                                 0 I                0 −I
                     S=J = T
                                         ,    J=            ,
                                −I 0                I 0

so that (7.2 a) can be written
                                    ∂H
                            qi =        ,
                                    ∂ pi
                                                 i = 1, 2, . . . , n,
                                    ∂H
                            pi = −      ,
                                    ∂ qi
where                                               
                                                 q
                                       y=                .
                                                 p
In many applications in mechanics, H is separable:
                                 H(p.q) = T (p) +V (q).
252                                                          7 B-series and geometric integration

Variational problems

For an initial value problem
                               y = f (y),          y(x0 ) = y0 ,                         (7.2 f)
with solution x → y(x), the variational problem
                          Y  (x) = f  (y)Y (x),        Y (x0 ) = I,                    (7.2 g)
is used to model the behaviour of inﬁnitesimal deviations from the trajectory in
(7.2 f). That is, if the initial value is modiﬁed by y0 → y0 + δ , then the solution at x
is modiﬁed by y(x) → y(x) +Y (x)δ + O(δ 2 ), where Y is the solution to (7.2 g).
   Now consider the case that (7.2 f) is a Hamiltonian problem f (x) = J T ∇H(y). In
this case f  (x) = J TW (y), where W (y) is the Hessian matrix with (i, j) element equal
to ∂ 2 H/∂ yi ∂ y j . Note that W (x) is symmetric.


Symplectic ﬂows


 Theorem 7.2A Let Y (x) denote the solution to the initial value problem (7.2 g),
 where the underlying problem (7.2 f) is Hamiltonian, then
                                     Y (x)T JY (x) = J.                                 (7.2 h)

Proof. Because Y (x0 ) = I we see that (7.2 h) holds when x = x0 . It remains to show
that
                                 d       T
                                d x Y (x) JY (x) = 0.
We have
               d       T             T             T  
              d x Y (x) JY (x) = Y (x) JY (x) +Y (x) JY (x)
                              = Y (x)TW (y)JJY (x) +Y (x)T JJ TW (y)Y (x)
                              = −Y (x)TW (y)Y (x) +Y (x)TW (y)Y (x) = 0.

Numerical methods which conserve quantities related to invariants of a given problem
are the main subject of this chapter.



7.3 Canonical and symplectic Runge–Kutta methods

Canonical Runge–Kutta methods, also known as symplectic Runge–Kutta methods,
have the ability to preserve quadratic invariants and symplectic structures. They will
be deﬁned in terms of a matrix, usually denoted by M.
7.3 Canonical and symplectic Runge–Kutta methods                                                       253

The matrix M and an identity

Given a Runge–Kutta tableau, (A, bT , c), we will be interested in the symmetric matrix
M = [mi j ]si, j=1 deﬁned as

 M = diag(b)A + AT diag(b) − bbT
     ⎡                                                                           ⎤
           2b1 a11 − b21     b1 a12 +b2 a21 − b1 b2 · · · b1 a1s +bs as1 − b1 bs
     ⎢                                                                           ⎥
     ⎢b a +b a − b b              2b2 a22 − b22     · · · b2 a2s +bs as2 − b2 bs ⎥
     ⎢ 2 21 1 12         2 1                                                     ⎥ (7.3 a)
   =⎢⎢
                                                                                 ⎥.
                                                                                 ⎥
     ⎢           .
                 ..                     .
                                        ..                           .
                                                                     ..          ⎥
     ⎣                                                                           ⎦
        bs as1 +b1 a1s − bs b1 bs as2 +b2 a2s − bs b2 · · ·                          2bs ass − b2s


The quadratic identity

 Theorem 7.3A Let the stages and the stages derivatives be given by the usual
 Runge–Kutta equations
                                                 s
                                Yi = y0 + h ∑ ai j Fj ,              i = 1, 2, . . . , s,
                                                j=1
                                Fi = f (Yi ),
                                                 s
                               y1 = y0 + h ∑ bi Fi ,
                                                i=1

 then, for any symmetric inner product, Q,
                                     s                         s                             s     s
  [y1 , y1 ]Q = [y0 , y0 ]Q + h ∑ bi [Yi , Fi ]Q + h ∑ b j [Fj ,Y j ]Q − h2 ∑ ∑ mi j [Fi , Fj ]Q .
                                    i=1                        j=1                          i=1 j=1


Proof. Evaluate three expressions as follows:
                  1 , y1 ]Q −s [y0 , y0 ]Q
           E1 := [y                           s                            
                =     y0 + h ∑ bi Fi , y0 + h ∑ b j Fj                             − [y0 , y0 ]Q
                                    i=1                        j=1             Q
                           s                           s                              s
                = h2 ∑ bi b j [Fi , Fj ]Q + h ∑ bi [Fi , y0 ]Q + h ∑ b j [y0 , Fj ]Q ,
                      i, j=1                          i=1                            j=1
                           s
            E2 := −h ∑ bi [Yi , Fi ]Q
                           i=1s                            s
                = −h   2
                               ∑ bi ai j [Fj , Fi ]Q − ∑ bi [y0 , Fi ]Q ,
                           i, j=1                      i=1
                             s
            E3 := −h ∑ b j [Fj ,Y j ]Q
                           j=1
                             s                             s
                = −h2 ∑ b j a ji [Fj , Fi ]Q − ∑ b j [Fj , y0 ]Q ,
                           i, j=1                       j=1
254                                                        7 B-series and geometric integration

with E1 + E2 + E3 = −h2 ∑si=1 ∑sj=1 mi j [Fi , Fj ]Q .


Stability applications
A problem satisfying [Y, F]Q ≤ 0, where Q is positive indeﬁnite is “dissipative”. The
following result is from [15] (Butcher, 1975).

 Theorem 7.3B Let (A, bT , c) be a Runge–Kutta method satisfying (i) M is positive
 indeﬁnite and (ii) bi > 0, i = 1, 2, . . . , s. Then for any dissipative problem, [yn , yn ]Q
 is non-increasing.

Proof. Because M is positive indeﬁnite, it is the sum of squares of linear forms and
hence
                                   s   s
                                  ∑ ∑ mi j [Fi , Fj ]Q ≥ 0.
                                  i=1 j=1

From Theorem 7.3A, [yn , yn ]Q − [yn−1 , yn−1 ]Q is the sum of three non-positive terms.

The conservation case
For problems satisfying [Y, F]Q = 0, where it is not necessary to assume that Q
has any special properties other than symmetry, the stability condition becomes a
conservation property of the differential equation.

 Theorem 7.3C Let (A, bT , c) be a Runge–Kutta method with M = 0. Then for a
 problem satisfying [Y, F]Q = 0, [yn , yn ]Q is constant.

Proof. From Theorem 7.3A, [yn , yn ]Q − [yn−1 , yn−1 ]Q is the sum of three zero terms.

For applications of this result to problems possessing quadratic invariants, see [32]
(Cooper, 1987) and [68] (Lasagni. 1988). For applications to Hamiltonian problems
see [83] (Sanz-Serna,1988).

Order conditions

The order conditions for Runge–Kutta methods have a remarkable property in the
case of symplectic methods (see [84] (Sanz-Serna, Abia, 1991)). Rather than impose
sufﬁciently many additional restrictions as to make canonical methods elusive, and
difﬁcult to construct, the conditions M = 0 actually lead to simpliﬁcations.
   To illustrate this effect, look at the usual conditions for order 4, where the under-
lying trees are also shown
                                                bT 1 = 1,                        (7.3 b)
                                                bT c = 12 ,                       (7.3 c)
                                              bT c2 = 13 ,                       (7.3 d)
                                             bT Ac = 16 ,                         (7.3 e)
                                              bT c3 = 14 ,                        (7.3 f)
                                            bT cAc = 18 ,                        (7.3 g)
7.3 Canonical and symplectic Runge–Kutta methods                                                    255

                                             bT Ac2 = 12
                                                      1
                                                         ,                                      (7.3 h)
                                              T      12
                                             b A c = 24 .                                        (7.3 i)
Write M = 0 in the form
                                 diag(b)A + AT diag b = bbT                                      (7.3 j)
and form the inner product
                        uT diag(b)Av + uT AT diag(b)v = uT bbT v,
for various choices of u and v, to obtain the results
               u = 1,   v = 1,      yields                          2bT c = (bT 1)2 ,           (7.3 k)
               u = 1,   v = c,      yields            T       2
                                               b c + b Ac = (b 1)(b c), T       T       T
                                                                                                 (7.3 l)
               u = c, v = c,        yields                        2b cAc = (b c) ,
                                                                    T           T   2
                                                                                                (7.3 m)
               u = 1,   v=c , 2
                                    yields        T       3         T
                                             b c + b Ac = (b 1)(b c ),      2   T       T   2
                                                                                                (7.3 n)
               u = 1,   v = Ac,     yields    T
                                             b cAc + b A = (b 1)(b Ac). T   2   T       T
                                                                                                (7.3 o)
Starting from (7.3 b), we ﬁnd in turn
                              (7.3 k) =⇒ (7.3 c),
                               (7.3 l) =⇒ (7.3 d) + (7.3 e),
                             (7.3 m) =⇒ (7.3 g),
                              (7.3 n) =⇒ (7.3 f) + (7.3 h),
                              (7.3 o) =⇒ (7.3 g) + (7.3 i).
In summary, instead of the 8 independent order conditions (7.3 b)–(7.3 i), it is only
necessary to impose the three conditions (7.3 b), (7.3 d), (7.3 f) to obtain order 4,
given that the method is canonical.
   To extend this approach to any order, consider a sequence of steps that could be
taken to verify that all order conditions are satisﬁed..

1. Show that the order 1 condition is satisﬁed.
2. For p = 2, . . . , up to the required order, show that the order condition for one tree
    within each non-superﬂuous class of order p is satisﬁed.
3. For p = 2, . . . , up to the required order, show that the order condition for one tree
    within each superﬂuous class of order p is satisﬁed.
4. Show that if the order condition for one tree within each order p class is satisﬁed
    then the same is true for all trees in the class.

   In the case of canonical methods, for which Theorem 7.3A holds, Steps 3 and 4 in
this sequence are automatically satisﬁed and only Steps 1 and 2 needs to be veriﬁed.
256                                                                 7 B-series and geometric integration



 Theorem 7.3D For a canonical Runge–Kutta method, of order p − 1, let t1 = t ∗ t ,
 t 2 = t  ∗ t where |t| + |t | = p. Then
                                             
                    Φ(t 1 ) − t1! + Φ(t2 ) − t1! = Φ(t)Φ(t  ) − t!
                                                                 1 1
                                                                    t !
                                                                         . (7.3 p)
                            1                          2


Proof. To show that Φ(t1 ) + Φ(t2 ) = Φ(t)Φ(t  ), write Φ(t) = bT φ , Φ(t ) = bT φ  , so
that
                        Φ(t1 ) = bT φ Aφ  = φ T diag(b)Aφ  ,
                          Φ(t 2 ) = bT φ  Aφ = φ T AT diag(b)φ  .
From (7.3 j), it follows that
                                          
                  φ T diag(b)A + AT diag(b) = φ T bbT φ  = Φ(t)Φ(t  ).

To show that (t1 !)−1 + (t2 !)−1 = (t!)−1 (t !)−1 , use the recursions
                                      t!t  !|t 1 |             t  !t!|t 2 |
                             t1 ! =      |t  | ,      t2 ! =        |t| ,
so that                                                           
                         1       1      1 1           t!      t!          1 1
                        t 1 ! + t 2 ! = t! t  !      t1 ! + t2 !       = t! t !
                                                                                  .


 Theorem 7.3E For a canonical Runge–Kutta method, the number of independent
 conditions for order p is equal to the number of non-superﬂuous free trees of order
 up to p.

Proof. From (7.3 p), we deduce that the order conditions for t 1 and t2 are equivalent
and hence only one condition is required for each non-superﬂuous free tree. In the
case of a superﬂuous tree t1 = t 2 = t ∗ t, (7.3 j) implies 2Φ(t 1 ) = 2(t 1 !)−1 .



Particular methods

Gauss methods
For the classical Gauss methods it was shown in [15] (Butcher, 1975) that M is
positive indeﬁnite and therefore that the method is algebraically stable. But in the
present context, these methods are symplectic because M = 0.

 Theorem 7.3F Let (A, bT , c) be the Gauss method with s stages,.then

                                diag(b)A + AT diag(b) = bbT .

Proof. Let V denote the Vandermonde matrix with (i, j) element equal to cij−1 . From
the order conditions for the trees [τ i−1 [τ j−1 ]], the product V T diag(b)AV has (i, j)
7.3 Canonical and symplectic Runge–Kutta methods                                                 257

element equal to 1/ j(i + j). Hence, the (i, j) element of V T (diag(b)A + AT diag(b) −
bbT )V is equal to 1/ j(i + j) + 1/i(i + j) − 1/i j = 0. Because V is non-singular, the
result follows.




Diagonally implicit methods



Methods in which A is lower triangular are canonical only if they have the form

                             1                  1
                             2 b1               2 b1

                          b1 + 12 b2            b1      1
                                                        2 b2
                                 ..              ..        ..      ..                 .
                                  .               .         .           .
                    b1 + b2 + · · · + 21 bs     b1         b2      ···      1
                                                                            2 bs
                                                b1         b2      ···          bs

This can also be looked at as the product of a sequence of s scaled copies of the
implicit mid-point rule method. That is, the product method

                  1       1             1      1                    1            1
                  2 b1    2 b1          2 b2   2 b2                 2 bs         2 bs
                                                        ···                               .
                          b1                   b2                                bs

For consistency, which will guarantee order 2, we must have ∑si=1 bi = 1. To obtain
order 3, we must have ∑si=1 b3i = 0 and, assuming this holds, order 4 is also possible
if bT is symmetric, in the sense that bi = bs+1−i .
    The simplest case of order 4 can then be found with b3 = b1 and satisfying

                                         2b1 + b2 = 1,                                        (7.3 q)
                                         2b31 + b32 = 0.                                      (7.3 r)
                    √
From (7.3 r), b2 = − 3 2 b1 and from (7.3 q) we then ﬁnd
                                                 √
                         bT =           1√      − 3√2             1√        .
                                      2− 3 2   2− 3 2           2− 3 2

This gives the method [34] (Creutz, Gocksch, 1989), [88] (Suzuki, 1990), [92]
(Yoshida, 1990)
258                                                         7 B-series and geometric integration

                           1√          1√
                         4−2 3 2     4−2 3 2
                                                    √
                           1           1√          − 3√2
                           2         2− 3 2      4−2 3 2
                             √
                             3                      √                     .                 (7.3 s)
                         3−2 √ 2       1√          − 3√2          1√
                         4−2 3 2     2− 3 2       2− 3 2        4−2 3 2
                                                    √
                                       1√          − 3√2          1√
                                     2− 3 2       2− 3 2        2− 3 2

Many similar schemes exist of which the following is particularly convenient and
efﬁcient [88] (Suzuki, 1990)
                                                 √
              bT =         1√         1√        − 3√4        1√           1√      .
                         4− 3 4     4− 3 4     4− 3 4      4− 3 4       4− 3 4



Block diagonally implicit
Nesting of known methods to obtain higher orders is possible using block diagonal
structures. For example, if (A, bT , c) is a symmetric canonical method with order 4,
then the composition of three methods forming the product

            θc    θA               (1−2θ )c     (1− 2θ )A                 θc     θA
                            ·                                       ·                   ,
                  θb T
                                                (1−2θ )b    T
                                                                                 θ bT
                √
where θ = (2 − 5 2)−1 , will be canonical and have order 6.
  For example, the method (A, bT , c) could be the 2-stage Gauss method or the
method (7.3 s).


Order with processing

In [69] (López-Marcos, Skeel, Sanz-Serna, 1996), it was proposed to precede a
sequence of symplectic Runge–Kutta steps with a “processing step”, which can have
its effects reversed at the conclusion of the integration steps. This makes it possible
to obtain adequate accuracy with an inexpensive integrator. This can be seen as an
application of effective order, or conjugate order [13] (Butcher, 1969).
    Let ξ denote the B-series for the input to each step so that the order conditions
become
                                η = A(ηD) + 1ξ ,
                               Eξ = bT (ηD) + ξ + O p+1 ,                        (7.3 t)

where η is the stage B-series vector. For classical order, ξ = 1.
7.3 Canonical and symplectic Runge–Kutta methods                                               259

Conformability and weak conformability

The conformability and weak conformability conditions refer to the starting method
(that is, the processor). To obtain the highest possible order, the values of ξ for any
pair of equivalent trees need to be related in a special way.

 Deﬁnition 7.3G The starting method ξ is conformable of order p if, for t, t  , such
 that |t| + |t ] ≤ p − 1,
                           ξ (t ∗ t ) + ξ (t ∗ t) = ξ (t)ξ (t  ).


 Deﬁnition 7.3H The starting method ξ is weakly conformable of order p if, for t,
 t  , such that |t| + |t ] ≤ p,
    (Eξ )(t ∗ t ) + (Eξ )(t ∗ t) − (Eξ )(t)(Eξ )(t )
                                             = ξ (t ∗ t ) + ξ (t ∗ t) − ξ (t)ξ (t ).   (7.3 u)

We now present a series of results interconnecting the two levels of conformability
and order. Write O to mean that a method has order p relative to a speciﬁc choice of
ξ , WC to mean that ξ is weakly conformable, C to mean that ξ is conformable and
P to mean that if the order condition holds for a tree in each non-superﬂuous class,
then the order is p.
    The results can be summarized in the diagram.

                                  O =⇒ WC ⇐⇒ C =⇒ P.                                       (7.3 v)

 Theorem 7.3I Let (A, bT , c) be a canonical Runge–Kutta method with order p
 relative to the starting method ξ . Then ξ is weakly conformable of order p.

Proof. Write (7.3 t) in the form

                        (Eξ )(t) − ξ (t) = bT (ηD)(t),           |t| ≤ p,

and substitute t → t ∗ t  , noting that

                         bT (ηD)(t ∗ t ) = (ηD)(t)T diag(b)η(t  ).

This gives

       (Eξ )(t ∗ t )−ξ (t ∗ t ) = (ηD)(t)T diag(b)η(t )
                                                                          
                                  = (ηD)(t)T diag(b) A(ηD)(t ) + 1ξ (t )
                                 = (ηD)(t)T diag(b)A(ηD)(t ) + bT (ηD)(t)ξ (t ).

Add a copy of this equation, with t and t  interchanged, and the result is
260                                                            7 B-series and geometric integration

           (Eξ )(t ∗ t ) − ξ (t ∗ t )+(Eξ )(t  ∗ t) − ξ (t ∗ t)
               = (ηD)(t)T diag(b)A(ηD)(t )+(ηD)(t  )T diag(b)A(ηD)(t)
                    + bT (ηD)(t)ξ (t ) + bT (ηD)(t )ξ (t)
                                                
               = (ηD)(t)T diag(b)A + AT diag(b) (ηD)(t )
                     + bT (ηD)(t)ξ (t ) + bT (ηD)(t )ξ (t)
                          
               = (ηD)(t)T bbT (ηD)(t ) + bT (ηD)(t)ξ (t ) + bT (ηD)(t )ξ (t)
                                                        
               = bT (ηD)(t) + ξ (t) bT (ηD)(t ) + ξ (t ) − ξ (t)ξ (t )
               = (Eξ )(t)(Eξ )(t  ) − ξ (t)ξ (t ),

which is equivalent to (7.3 u).
Before showing the equivalence of conformability and weak conformability, we
establish a utility deﬁnition and a utility lemma.

 Deﬁnition 7.3J Let t = [t 1 t2 · · · t m τ n ], where ti = τ, i = 1, 2, . . . , m. Then the bushi-
 ness of t is deﬁned by bush(t) = n.



 Lemma 7.3K For ξ ∈ B and t, t  ∈ T,
      (Eξ )(t ∗ t  ) + (Eξ )(t ∗ t) − (Eξ )(t)(Eξ )(t)
                                                                              
         = ∑ E(t x )E(t x  ) ξ ( x ∗ x  ) + ξ ( x  ∗ x ) − ξ ( x )ξ ( x  ) .         (7.3 w)
            x≤t,x ≤t


Proof. The subtrees of t ∗ t  are of the form x ∗ x  and x and hence

                   (Eξ )(t ∗ t ) =      ∑  E(t x)E(t x )ξ (x ∗ x )
                                      x ≤t, x ≤t
                                         + E(t ) ∑ E(t x )ξ ( x ) + E(t ∗ t ).
                                                 x≤t

Using this and the same formula, with t and t  interchanged, we ﬁnd

          (Eξ )(t ∗ t ) + (Eξ )(t ∗ t) − (Eξ )(t)(Eξ )(t)
                                            
           = ∑ E(t x )E(t x  ) ξ ( x ∗ x  )
             x ≤t, x ≤t
                             
            + ξ ( x  ∗ x ) + E(t ) ∑ E(t x )ξ ( x )
                                       x≤t
                                                                     
                + E(t) ∑ E(t  x  )ξ ( x  ) + E(t ∗ t ) + E(t ∗ t)
                          x ≤t
                                                                         
                   − ∑ E(t x )ξ ( x ) + E(t) ∑ E(t  x  )ξ ( x  ) + E(t ) .
                         x≤t                          x ≤t
7.3 Canonical and symplectic Runge–Kutta methods                                                261

Noting that E(t ∗ t) + E(t ∗ t) = E(t)E(t ), we see that this reduces to the result of
the lemma.

We now have:

 Theorem 7.3L The starting method ξ is weakly conformable of order p, if and
 only if it is conformable of order p.

Proof. The ‘if’ part of the proof follows from Lemma 7.3K because, if ξ is con-
formable of order p, all terms on the right of (7.3 w) are zero. To prove the only if
result by induction, assume that ξ is conformable of order p − 1, so that it is only nec-
essary to show that for | x |+| x  | = p−1, ξ ( x ∗ x  )+ξ ( x  ∗ x )−ξ ( x )ξ ( x  ) = 0. With-
out loss of generality, assume bush( x ) ≥ bush( x  ). Note that bush( x ) ≤ p − 3, corre-
sponding to x = [τ p−3 ], x  = τ. We will carry out induction on K = p−3, p−4, . . . , 0.
For each K consider all x , x  pairs such that bush( x ) = K. Deﬁne t = x ∗ τ, t = x  ,
and substitute into (7.3 w). All terms on the right-hand side vanish because they
correspond to a higher value of K and the single term corresponding to the current
value of K. Hence we have
                                                                           
                    (K + 1) ξ ( x ∗ x  ) + ξ ( x  ∗ x ) − ξ ( x )ξ ( x  ) = 0.



 Theorem 7.3M Let (A, bT , c) be a canonical Runge–Kutta method such that, for
 each non-superﬂuous free tree, at least one of the trees has order p relative to a
 conformable starting method ξ , then all trees have order p relative to ξ .

Proof. Use an induction argument, so that the result can be assumed for all trees up
to order p − 1. It remains to show that if

                     (Eξ )(t ∗ t ) − ξ (t ∗ t ) − bT (ηD)(t ∗ t ) = 0, then
                     (Eξ )(t  ∗ t) − ξ (t ∗ t) − bT (ηD)(t ∗ t) = 0.

Add these expressions and use the fact that

              bT (ηD)(t ∗ t ) + bT (ηD)(t ∗ t) = (Eξ )(t)(Eξ )(t )ξ (t)ξ (t ).

It is found that

  (Eξ )(t ∗ t ) − ξ (t ∗ t ) − bT (ηD)(t ∗ t ) + (Eξ )(t ∗ t ) − ξ (t ∗ t) − bT (ηD)(t ∗ t)
   = (Eξ )(t ∗ t ) − ξ (t ∗ t ) + (Eξ )(t ∗ t ) − ξ (t ∗ t)(Eξ )(t)(Eξ )(t ) + ξ (t)ξ (t )
   = 0.
262                                                                  7 B-series and geometric integration

7.4 G-symplectic methods

The multivalue form of the matrix M, and an identity

For a general linear method,                           
                                           A     U
                                                            ,
                                           B     V
the partitioned matrix                                                          
                               DA + AT D − BT GB                DU − BT GV
                     M=                                                                          (7.4 a)
                                 U T D −V T GB                      G −V T GV

was introduced in [6] (Burrage, Butcher, 1980) to characterize quadratic stability for
multivalue methods and it has a similar role in the general linear case as the matrix
(7.3 a) with the same name.
   The matrix G appearing in M has a similar role as in the deﬁnition of G-stability
[38]. In the general linear case, G is used to construct the quadratic form
                                              r      [n] [n] 
                         [y[n] , y[n] ]G⊗Q := ∑ gi j yi , y j Q ,
                                               i, j=1

whose behaviour, as n increases, can be used to study non-linear stability ([6]) or
conservation.


The quadratic identity for multivalue methods
The result given in Theorem 7.3A has a natural extension to the G-symplectic case

 Theorem 7.4A

      [y[n] , y[n] ]G⊗Q = [y[n−1] , y[n−1] ]G⊗Q + h[Y, F]D⊗Q + h[F,Y ]D⊗Q − [v, v]M⊗Q ,

 where                                                 
                                                 hF
                                     v=                         .
                                               y[n−1]

Proof. Rewrite (7.4 a) in the form
                             ;     <  T      ; <
           BT                  0 0    A       D     
                 G BV =               +     D0 +      A U − M.
           VT                    0G     UT        0

Apply the linear operation
                                      X → [v, Xv]Q
to each term in (7.4 a) and the result follows.
7.4 G-symplectic methods                                                           263

Non-linear stability
As for Runge–Kutta methods, we will consider problems for which [Y, f (Y )] ≤ 0
with the aim of achieving stable behaviour.

 Deﬁnition 7.4B A general linear method (A,U, B,V ) for which there exist D, a
 non-negative diagonal matrix, and G a positive semi-deﬁnite symmetric matrix is
 algebraically stable if M, given by (7.4 a), is positive semi-deﬁnite.


 Theorem 7.4C For a problem for which [Y, f (Y )]Q is non-positive for posi-
 tive indeﬁnite Q, a numerical solution y[n] , found from an algebraically stable
 Runge–Kutta method, has the property that [y[n] , y[n] ]G⊗Q is non increasing for
 n = 0, 1, 2, . . . .

Proof. This follows from Theorem 7.4A.


Conservation properties
We now turn our attention to problems for which [Y, f (Y )]Q = 0 and methods for
which M = 0, where M is given by (7.4 a). That is, we are considering methods
covered by the following deﬁnition:

 Deﬁnition 7.4D A general linear method (A,U, B,V ) for which there exist D, a
 non-negative diagonal matrix and G such that
                                DA + AT D = B∗ GB,                           (7.4 b)
                                     DU = B∗ GV,                              (7.4 c)
                                        G = V ∗ GV.
 is G-symplectic.

In this deﬁnition we have allowed for complex coefﬁcients in U, B and V , by writing
Hermitian transposes.

 Theorem 7.4E Let (A,U, B,V ) denote a G-symplectic method. Then for a prob-
 lem for which [Y, f (Y )]Q = 0, [y[n] , y[n] ]Q is constant for n = 0, 1, 2, . . . .

Proof. The result follows from the identity in Theorem 7.4A, with [Y, F]D⊗Q and M
deleted.

Two methods based on Gaussian quadrature
                                                                                  √
The two methods P and N were introduced in [21] and differ only in the sign of          3
which appears in the coefﬁcients. For P we have the deﬁning matrices
264                                                        7 B-series and geometric integration
                                     ⎡   √                         √          ⎤
                                       3+ 3
                                     ⎢ 6           0       1 − 3+26 3         ⎥
                                   ⎢ √            √              √          ⎥
                                     ⎢− 3         3+ 3
                                                           1 3+26 3           ⎥
                      A     U        ⎢ 3                                      ⎥
                                    =⎢              6
                                                                              ⎥.
                      B     V        ⎢   1         1                          ⎥
                                     ⎢                     1       0          ⎥
                                     ⎣   2         2
                                                                              ⎦
                                         1
                                         2        −2
                                                   1
                                                           0    −1

This method can be veriﬁed to be G-symplectic with
                                          √
                          G = diag(1, 3+26 3 ).    D = diag( 12 , 12 ).




Dealing with parasitism

Parasitism, and methods for overcoming its deleterious effects. were discussed in
[21] (Butcher, Habib, Hill, Norton, 2014).
   The stability function for P is                  ⎡                              ⎤
                                                         1+z              0
   V + zB(I − zA)−1U = V + zBU + O(z2 ) = ⎣                                   √    ⎦ + O(z2 ).
                                                          0 −1 − 3+26 3 z

For a high-dimensional problem, z represents the value of an eigenvalue of the
Jacobian of f at points in the step being taken. In general, it is not possible to
guarantee that the real parts of these eigenvalues will not be positive and hence the
method cannot be guaranteed to be stable.
   In numerical experiments with the simple pendulum, unstable behaviour does
occur both for P and N. This is manifested by the loss of apparently bounded
behaviour of the deviation of the Hamiltonian from its initial value. The onset
depends on the initial amplitude of the pendulum swings and also appears later for N,
compared with P. This behaviour is illustrated, in the case of P, in Figure 13. This
shows the deviation of H from its initial value for the simple pendulum problem with
p = 0 and two different values of q0 .



Conformability properties for general linear methods

We will extend Deﬁnitions 7.3G (p. 259) and 7.3H to the multivalue case. For Runge–
Kutta methods, the need for these concepts only arose for methods with processing
but, in the more general case, they are always needed because, even if the principal
input might have a trivial starting method, the supplementary components will not.
Recall (7.3 v) (p. 259) which applies, suitably interpreted, also to G-symplectic
methods.
7.4 G-symplectic methods                                                                       265

   H − H0
  2 × 10−11
  1 × 10−11
  5 × 10−12



              0 0.01           0.1              1               10              102        103
                                                                                      x
   H − H0
  5 × 10−11

  2 × 10−11
  1 × 10−11
  5 × 10−12

−5 × 10−12
−1 × 10−11
   Figure 13 Variation in the Hamiltonian in attempts to solve the simple pendulum problem
   using method P with h = 0.01 and 105 time steps, with initial value y0 = [1.5, 0]T (upper
   ﬁgure) and y0 = [2, 0]T (lower ﬁgure)




 Deﬁnition 7.4F The starting method ξ is conformable of order p if, for t, t  , such
 that |t| + |t ] ≤ p − 1,
                              ξ1 (t ∗ t ) + ξ1 (t ∗ t) = ξ (t)T Gξ (t ).




 Deﬁnition 7.4G The starting method ξ is weakly conformable of order p if, for t,
 t  , such that |t| + |t ] ≤ p,
                       (Eξ )1 (t ∗ t )+(Eξ )1 (t ∗ t)−(Eξ )(t)T G(Eξ )(t  )
                                      = ξ1 (t ∗ t )+ξ1 (t  ∗ t)−ξ (t)T Gξ (t ).



 Theorem 7.4H The starting method ξ is conformable of order p if and only if ξ
 is weakly conformable of order p.



 Theorem 7.4I Let (A,U, B,V ) be a G-symplectic method with order p relative to
 the starting method ξ . Then ξ is weakly conformable of order p.
266                                                      7 B-series and geometric integration

 Theorem 7.4J Let (A,U, B,V ) be a G-symplectic method with order at least p − 1
 relative to a starting method ξ , which is conformable of order p. Then the method
 satisﬁes the order condition for t ∗ t  , where |t ∗ t | = p, if and only if it satisﬁes
 the order condition for t ∗ t.

Theorems 7.4H, 7.4I, 7.4J are proved in [24] (Butcher, Imran, 2015).


7.5 Derivation of a fourth order method
The method G4123

The method G4123, with pqrs = 4123, was derived in [24] (Butcher, Imran, 2015).
We will consider methods with a partitioned coefﬁcient matrix
                                        ⎡             ⎤
                                         A 1 U
                          A U           ⎢             ⎥
                                     =⎢ ⎣ b T
                                               1   0  ⎥,
                                                      ⎦
                          B V
                                           B 0 V

with the eigenvalues of V distinct from 1 but lying on the unit circle.
   It will be found that, with s = 3 and r = 2, fourth order G-symplectic methods
exist such that A is lower-triangular with only a single non-zero diagonal element,
and such that the parasitic growth factors are zero. A suitable ansatz is
                ⎡                                                         ⎤
                   1           2
                   2 b1 (1 + gx1 )        0               0        1−gx1
                ⎢                                                         ⎥
            ⎢                    1
                ⎢ b1 (1 + gx1 x2 ) 2 b2 (1 + gx22 )       0        1−gx2 ⎥⎥
     A U        ⎢                                                         ⎥
                ⎢
             = ⎢ b1 (1 + gx1 x3 ) b2 (1 + gx2 x3 ) 1 b3 (1 + gx2 ) 1−gx3 ⎥⎥ , (7.5 a)
     B V        ⎢                                   2          3
                                                                          ⎥
                ⎢                                                         ⎥
                ⎣        b  1            b 2             b 3       1    0 ⎦
                        b 1 x1          b2 x2           b3 x3      0 −1

based on
                      D = diag(b1 , b2 , b3 ),     G = diag(1, g).
For efﬁciency, we will attempt to obtain order 4 with a11 = a22 = 0. We achieve this
by choosing g = −1 (for simplicity, noting that g cannot be positive), together with
x1 = 1, x2 = −1. Substitute into (7.5 a) to obtain the simpliﬁed coefﬁcient matrices
                      ⎡                                                   ⎤
                              0              0            0        1 1
                      ⎢                                                   ⎥
                 ⎢         2b1             0            0        1 −1 ⎥
                      ⎢                                                   ⎥
           A U        ⎢                                                   ⎥
                   = ⎢ b1 (1 − x3 ) b2 (1 + x3 ) 21 b3 (1 − x32 ) 1 x3 ⎥ .
           B V        ⎢                                                   ⎥
                      ⎢                                                   ⎥
                      ⎣       b1            b2            b3       1 0 ⎦
                              b1          −b2            b3 x3     0 −1
7.5 Derivation of a fourth order method                                                               267


                           Table 19 Solution and veriﬁcation of (7.5 b)


             ∅
    ξ         1       0        − 32
                                 1
                                        − 4320
                                            7       149
                                                    8640        0         0         0          0

    ζ         0       1
                      4        − 16
                                 1
                                         − 960
                                           49
                                                  − 384
                                                    13        2543
                                                              57600
                                                                        193
                                                                        7680
                                                                                    619
                                                                                   34560
                                                                                               163
                                                                                              69120

    η1        1       1
                      4        − 32
                                 3
                                        − 1728
                                           91
                                                  − 17280
                                                     287      2543
                                                              57600
                                                                        193
                                                                        7680
                                                                                    619
                                                                                   34560
                                                                                               163
                                                                                              69120

    η2        1        5
                      12
                                 19
                                 96
                                           787
                                           8640   − 17280
                                                     197
                                                            − 57600
                                                              1943
                                                                      − 7680
                                                                        313
                                                                                − 103680
                                                                                   5497
                                                                                           − 41472
                                                                                              557


    η3        1       11
                      20
                                 37
                                 160
                                           1147
                                           8640
                                                     739
                                                    17280
                                                              377
                                                              6400
                                                                         313
                                                                        12800
                                                                                   2489
                                                                                  172800   − 345600
                                                                                             15487


   η1 D       0       1           1
                                  4
                                            1
                                            16     − 32
                                                      3        1
                                                               64     − 128
                                                                         3
                                                                                − 1728
                                                                                   91
                                                                                           − 17280
                                                                                              287


   η2 D       0       1          5
                                 12
                                           25
                                           144
                                                     19
                                                     96
                                                              125
                                                              1728
                                                                         95
                                                                        1152
                                                                                   787
                                                                                   8640    − 17280
                                                                                              197


   η3 D       0       1          11
                                 20
                                           121
                                           400
                                                     37
                                                     160
                                                              1331
                                                              8000
                                                                        407
                                                                        3200
                                                                                   1147
                                                                                   8640
                                                                                               739
                                                                                              17280
                                 15        1163     1319       109       3         187        187
    Eξ        1       1          32        4320     8640       720       32        2160       4320

    Eζ        0       1
                      4
                                 3
                                 16
                                           71
                                           960
                                                     11
                                                     384    − 57600
                                                              2677
                                                                      − 2560
                                                                         73
                                                                                − 34560
                                                                                   1001
                                                                                           − 69120
                                                                                              1457




It was shown in [24] (Butcher, Imran, 2015) how the free parameters and the starting
vectors can be chosen to achieve order 4 accuracy and also to ensure that parasitic
growth factors are zero. The method parameters, are
                          ⎡                       ⎤
                             0 0       0 1 1
                          ⎢                       ⎥           ⎡      ⎤
                          ⎢ 2          0 1 −1 ⎥                   1
                      ⎢ 3 0                     ⎥
                          ⎢                       ⎥           ⎢ 4 ⎥
              A U         ⎢ 2−3        1
                                             − 1 ⎥            ⎢ 5 ⎥
                        = ⎢ 5 10 2         1   5 ⎥,       c = ⎢ 12   ⎥,
              B V         ⎢                       ⎥           ⎣      ⎦
                          ⎢ 1 − 3 25 1 0 ⎥
                          ⎢ 3 8 24                ⎥              11
                          ⎣                       ⎦              20

                                  8 − 24   0 −1
                             1    3    5
                             3

and they were chosen to satisfy the order conditions

                                        η = AηD + 1ξ + U ζ ,
                                       Eξ = bT ηD + ξ + O5 ,                                   (7.5 b)
                                       Eζ = BηD + V ζ + O5 .

The values of the starting methods, ξ and ζ , and the stage values and derivatives, η
and ηD, are given in Table 19, together with a veriﬁcation that the conditions are
satisﬁed. It was assumed from the start, without loss of generality, that ξ1 and ξ5 , ξ6 ,
ξ7 , ξ8 are zero. Note that the entries for Eξ and Eζ in (7.5 b) are identical, to within
O5 and these lines are the ﬁnal steps of the order veriﬁcation.
268                                                                    7 B-series and geometric integration

Implementation questions

Starting and ﬁnishing methods for ξ

We will write S h and F h for the mappings corresponding to the starting and ﬁnishing
                              [0]
methods, respectively, for y1 . Because each of the mappings is only required to be
correct to within O4 , with the proviso that F h ◦ S h = id + O5 , we will ﬁrst construct a
Runge–Kutta tableau with only three stages which gives the B-series ξ −1 + O4 from
which a corresponding approximation to ξ can be found cheaply to within O5 .
   Calculate the coefﬁcients of ξ −1 for the ﬁrst 4 trees and write down the order
conditions for the required tableau
                               b1 + b2 + b3 = ξ −1 ( ) = 0,
                                   b2 c2 + b3 c3 = ξ −1 ( ) = 32
                                                              1
                                                                 ,
                                   b2 c22 + b3 c23 = ξ −1 ( ) = 4320
                                                                 7
                                                                     ,
                                         b3 a32 c2 = ξ −1 ( ) = − 8640
                                                                  149
                                                                       .

A possible solution to this system is

                                      0
                                      1        1
                                      2        2
                                                                             .
                                      1      − 121
                                                28       149
                                                         121

                                            − 4320
                                              391        16
                                                         135       − 4320
                                                                     121


If the ﬁnishing method for the ﬁrst component is given by F h , then S h can be
approximated by
                       S h = 3id − F h − F h ◦ (2id − F h ).
Let
                     a=        1     0     a2   a3       a4       a5   a6        a7     a8
be the B-series coefﬁcients for F h , for ∅ . . . t8 , so that the corresponding coefﬁcient
vector for (2id − F h ) is

          b=         1   0     −a2        −a3       −a4        −a5     −a6            −a7      −a8      .

The series for F h ◦ (2id − F h ) is found to be

                  ba =         1      0     0   0    0        0    −a22      0        −a22     ,

with the ﬁnal result

            1    0       −a2       −a3      −a4      −a5          a22 − a6       −a7         a22 − a8       ,
7.5 Derivation of a fourth order method                                                           269

which is identical to the series for a−1 , corresponding to S h .
                                                   [0]
   Now consider the starting method for y2 . This can be found using a generalized
Runge–Kutta method with order conditions Φ(t) = ζ (t) for |t| ≤ 4 and coefﬁcient
of y0 equal to zero. A suitable tableau for this starter is

                   0
                 − 14    − 14
                 − 14 − 11609760
                        9319973         6417533
                                       11609760
                                                                                     .
                   1
                   4    − 7417
                          6432             0               9025
                                                           6432

                 − 34   − 5536
                          887
                                           0                0       − 3265
                                                                      5536

                   0       28
                           675     − 1583059879
                                     5775779700
                                                         43875218
                                                         57757797   − 450
                                                                       67
                                                                          − 1350
                                                                            173




                                                          [0]
Exercise 58 Find an alternative starting method for y2 , using a generalized ﬁve stage
Runge–Kutta method with a42 = a52 = a53 = 0, and with c2 = c3 = − 13 , c4 = − 23 , c5 = −1.


Exercise 59 If a four stage generalized Runge–Kutta method is used for the starting method for
 [0]
y2 and c2 = 15924
            14305 , what is c4 ?




Initial approximation for Y3
Because the ﬁrst two stages of the method are explicit, the most important implemen-
tation question is the evaluation of the third stage. We will consider only the Newton
method for this evaluation and we will need to ﬁnd the most accurate method for
obtaining an initial estimate to commence the iterations.
    Information available when the ﬁrst two stage derivatives have been computed in,
                                                      [0[] [0[]
for example, the ﬁrst step of the solution, includes y1 , y2 , hF1 and hF2 and we will
need to obtain a useful approximation to F3 . In terms of B-series coefﬁcients we have

             ξ (∅) = 1,          ξ ( ) = 0,        ξ ( ) = − 32
                                                             1
                                                                ,          ξ ( ) = − 4320
                                                                                      7
                                                                                          ,

             ζ (∅) = 0,          ζ ( ) = 14 ,      ζ ( ) = − 16
                                                             1
                                                                ,          ζ ( ) = − 960
                                                                                     49
                                                                                         ,
          η1 D(∅) = 0, η1 D( ) = 1, η1 D( ) =                       4,
                                                                    1
                                                                         η1 D( ) =        16 ,
                                                                                          1


          η2 D(∅) = 0, η2 D( ) = 1, η2 D( ) =                     12 , η2 D(
                                                                   5
                                                                               )=         144 ,
                                                                                          25


            η3 (∅) = 1,         η3 ( ) = 11
                                         20 ,     η3 ( ) =        160 , η3 (
                                                                  37
                                                                               )=        8640 .
                                                                                         1147


A short calculation suggests that the approximation

                                 η ≈ ξ + η − 65 η1 D + 32 η2 D
270                                                            7 B-series and geometric integration


      Table 20 Trees to order 6, grouped together as free trees with superﬂuency, symmetry
      and possible deletion if the C(2) condition holds

 order      serial number      free tree         tree count   superﬂuous    symmetric      C(2)
      1           1                                     1
      2           2                                     1         X             X
      3           3                                     2                                   X
      4           4                                     2                       X
                  5                                     2         X             X           X
      5           6                                     2
                  7                                     4                                   X
                  8                                     3                                   X
      6           9                                     2                       X
                  10                                    4                       X           X
                  11                                    4                       X           X
                  12                                    2         X             X           X
                  13                                    5                       X           X
                  14                                    3         X             X           X




is exact, based on just 0,
                        / , and . Accordingly, the approximation
                                           [0]    [0]
                               Y3 ≈ y1 + y2 − 65 hF1 + 32 hF2

is suggested to initialize the iterative computation of Y3 .




7.6 Construction of a sixth order method


This discussion is based on [25] (Butcher, Imran, Podhaisky, 2017). Of the 37 trees
up to order 6, which contribute to the order requirements, these can be immediately
reduced to 14 because of the role played by the equivalences which deﬁne free trees.
Some of these can immediately be discarded because of superﬂuency. If the method is
symmetric then further trees become candidates for deletion from the set of required
order conditions [23] (Butcher, Hill, Norton, 2016). If it is possible to impose the
C(2) condition, further deletions are possible. These simpliﬁcations are summarized
in Table 20.
7.6 Construction of a sixth order method                                             271

Design requirements

Time-reversal symmetry

Methods with time-reversal symmetry were considered in [23] (Butcher, Hill, Norton,
2016). This property is an important attribute of numerical schemes for the long-
term integration of mechanical problems. Furthermore, the symmetric general linear
methods perform well over long time intervals. We can deﬁne a general linear
method to be symmetric in a similar fashion to a Runge–Kutta method. A general
linear method is symmetric if it is equal to its adjoint general linear method, where
the adjoint general linear method takes the stepsize with opposite sign. However,
symmetry in general linear methods is not as simple as for Runge–Kutta methods,
because the output approximations contain the matrix V , which is multiplied by the
input approximations, and it is possible that the inverse matrix V −1 is not equal to V .
For this reason, an involution matrix L is introduced, such that L2 = I and LV −1 L = V .
We also introduce the stage reversing permutation P deﬁned as Pi j = δi,s+1− j for
i, j = 1, . . . , s.
   In particular, because of time-reversal symmetry, trees with even order can be
ignored because the corresponding conditions will be automatically satisﬁed.


 Deﬁnition 7.6A A method (A,U, B,V ) is time-reversal symmetric with respect
 to the involution L if
                           A + PAP = UV −1 B,                         (7.6 a)
                             V LBP = B,                               (7.6 b)
                             PULV = U,                                (7.6 c)
                                      (LV )2 = I.                               (7.6 d)


From results in [23], it follows that, for a method with this property, with starting
method Sh , it can be assumed that Sh = LS−h . Methods which are both G-symplectic
and symmetric have many advantages, and some of these were derived in [23]. For
methods with lower-triangular A, the two properties are closely related.


 Theorem 7.6B Let (A,U, B,V ) be a method with the properties
 1. A is lower triangular,
 2. The method is G-symplectic,
 3. (7.6 c) is satisﬁed,
 then (7.6 a), (7.6 b) and (7.6 d) are satisﬁed.


This result is proved in [20] (Butcher, 2016).
272                                                          7 B-series and geometric integration

Structure of the method G6245

The method, which will be referred to as G6245, because pqrs = 6245, was orig-
inally derived in [25]. It achieves order 6 by combining symmetry, C(2) with G-
symplecticity.


Symmetry requirements

An arbitrary choice is made to deﬁne

                   V = diag(1, i, −i, −1),                                               (7.6 e)
                   G = diag(1, − 12 , − 12 , 1),
                  U=          1       2 (−β − iα)
                                      1              1
                                                     2 (−β + iα)     −γ     ,

where                ⎡            ⎤             ⎡        ⎤            ⎡         ⎤
                         α1                         β1                    γ1
                    ⎢    ⎥                      ⎢    ⎥                 ⎢    ⎥
                    ⎢ α2 ⎥                      ⎢ β2 ⎥                 ⎢ γ2 ⎥
                    ⎢    ⎥                      ⎢    ⎥                 ⎢    ⎥
                    ⎢    ⎥                      ⎢    ⎥                 ⎢    ⎥
               α := ⎢ 0 ⎥ ,                β := ⎢ β3 ⎥ ,          γ := ⎢ γ3 ⎥ .
                    ⎢    ⎥                      ⎢    ⎥                 ⎢    ⎥
                    ⎢−α ⎥                       ⎢ β ⎥                  ⎢ γ ⎥
                    ⎣ 2 ⎦                       ⎣ 2 ⎦                  ⎣ 2 ⎦
                     −α1                          β1                     γ1

Also write bT =     b1 b2 b3 b2 b1 , D = diag(b). From (7.4 c), we deduce
                                          ⎡                  ⎤
                                                    bT
                                        ⎢                ⎥
                                        ⎢ (α T + iβ T )D ⎥
                                      B=⎢                ⎥
                                        ⎢ (α T − iβ T )D ⎥ .
                                        ⎣                ⎦
                                               γ TD

Deﬁne the 5 × 5 symmetric matrix W with elements wi j = αi α j + βi β j − γi γ j , i, j =
1, 2, . . . , 5, which can be written

                                      W = αα T + β β T − γγ T .                           (7.6 f)

Because of the symmetries and anti-symmetries in α, β , γ, it follows that W has the
form                     ⎡                                  ⎤
                            w11 w21 w31 w41 w51
                         ⎢                                  ⎥
                         ⎢ w21 w22 w32 w42 w41 ⎥
                         ⎢                                  ⎥
                         ⎢                                  ⎥
                   W = ⎢ w31 w32 w33 w32 w31 ⎥ .                             (7.6 g)
                         ⎢                                  ⎥
                         ⎢ w                                ⎥
                         ⎣ 41 w42 w32 w22 w21 ⎦
                            w51 w41 w31 w21 w11
7.6 Construction of a sixth order method                                            273

From (7.4 b) (p. 263), assuming A is lower triangular, the elements of this matrix are
found to be
             ⎧
             ⎪
             ⎪ b j (1 − αi α j − βi β j + γi γ j ) = 12 b j (1 − wi j ), j < i,
             ⎪
             ⎨
       ai j = 12 b j (1 − αi α j − βi β j + γi γ j ) = b j (1 − wi j ), j = i,  (7.6 h)
             ⎪
             ⎪
             ⎪
             ⎩ 0,                                                        j > i.


   Symmetry also requires Pc + c = 1 and bT P = bT and we choose the abscissae
vector as
                   c = 0 12 (1 − t) 12 12 (1 + t) 1

and the vector bT =      b1    b2   b3     b2     b1      such that

                                            bT 1 = 1,
                                           bT c2 = 13 ,
                                           bT c4 = 15 .



The choice of t = 1 − 2c2

The choice of t must yield a negative coefﬁcient amongst b1 , b2 , b3 to ensure that the
parasitism growth factors can be eliminated. It is found that this is possible in three
cases

Case 1:                          0 < t 2 < 15 :           b3 < 0,

                                 5 <t <1 :                b1 < 0,
                                 3    2                                          (7.6 i)
Case 2:
Case 3:                          1 < t2  :                b2 < 0.

From the C(2) conditions, the ﬁrst three rows of A are given by
                         ⎡                             ⎤
                              0      0     0 0 0
                         ⎢ 1                           ⎥
                         ⎢ c2 1 c2 0 0 0 ⎥ ,                                     (7.6 j)
                         ⎣ 2        2                  ⎦
                             a31 a32 a33 0 0

where a31 and a32 are written in terms of the parameter a33 , as solutions of the system

                                 a31 + a32 = c3 − a33 ,
                                     a32 c2 = 12 c23 − a33 c3 .

To ﬁnd a33 , use (7.6 h) and the symmetry of W to see that
274                                                                     7 B-series and geometric integration

                    5
                   ∑ bi wii = 2(b1 − 2a11 ) + 2(b2 − 2a22 ) + (b3 − 2a33 )
                   i=1
                                  = 1 − 2c2 − 2a33 .

However, to guarantee that the parasitism growth factors are zero, we must have
                              5              5                    5         5
                          ∑ bi wii = ∑ bi αi2 + ∑ bi βi2 − ∑ bi γi2 = 0.
                          i=1               i=1               i=1       i=1

Hence,
                                                a33 = 12 − c2 = 12 t,
and (7.6 j) can be rewritten as
     ⎡                                      ⎤ ⎡                                                      ⎤
               0           0        0   0 0           0                            0         0 0 0
     ⎢                                      ⎥ ⎢                                                      ⎥
     ⎢                                      ⎥   ⎢
                                        0 0 ⎥ = ⎢ 4 (1 − t)                                  0 0 0⎥
                                                                                4 (1 − t)
             1            1                       1                             1
     ⎢       2 c2         2 c2      0                                                                ⎥.
     ⎣                                      ⎦ ⎣                                                      ⎦
        c2 − 12 + 8c12 12 − 8c12 12 −c2 0 0       1−2t+2t 2                       1−2t       1
                                                     4−4t                         4−4t       2 t 0 0




Derivation of the method

Transform to the W formulation, impose the symmetry pattern given by (7.6 g) and
transform back to ﬁnd all elements of A except a41 , a42 a51 , a52 . These are found in
turn from the C(2) conditions for the ﬁnal two rows.
    We will now show that the rank of W cannot exceed 3. This follows because bTW =
b diag(c)W = 0, which can be veriﬁed by detailed calculations. A consequence of
  T


this is
          5               5             5                5              5               5
         ∑ bi αi = ∑ bi βi = ∑ bi γi = ∑ bi ci αi = ∑ bi ci βi = ∑ bi ci γi = 0,
         i=1             i=1           i=1              i=1           i=1              i=1

implying
                                                         bTW = 0,                                     (7.6 k)
                                                 bT diag(c)W = 0.                                        (7.6 l)



A special case

As a special case, choose t = 13 in Case 1 of (7.6 i). This gives
                                                            T
      c=       0         1
                         3
                                   1
                                   2
                                            2
                                            3       1             ,
7.6 Construction of a sixth order method                                                                              275
                                                        
     bT =       11
                120
                            27
                            40    − 15
                                    8        27
                                             40
                                                  11
                                                  120     ,
            ⎡                                            ⎤              ⎡                                        ⎤
                    1 − 11
                        9
                           − 14
                             11 − 297 − 121
                                  83     39
                                                                              0        0      0      0         0
        ⎢ 9                          ⎥                                 ⎢ 1                                       ⎥
        ⎢−                        83 ⎥                                 ⎢ 6             1
                                                                                                               0 ⎥
                           729 − 297 ⎥
                  41   22  209                                                                0      0
        ⎢ 11      81   27                                              ⎢                6                        ⎥
        ⎢                         14 ⎥                                 ⎢ 5              1    1
                                                                                                               0 ⎥
    W = ⎢− 14     22   13
                            27 − 11 ⎥
                            22
                                       ,                           A = ⎢ 24                          0           ⎥.
        ⎢ 11      27    8            ⎥                                 ⎢ 19             8    6
                                                                                                                 ⎥
        ⎢−                           ⎥                                 ⎢                                       0 ⎥
                                                                                       27 − 81
                                                                                       13   8         1
        ⎣ 297 729 27 81 − 11 ⎦
           83    209   22   41    9
                                                                       ⎣ 162                          6          ⎦
                                                                                       22 − 33
                                                                            4          19   40       27
         − 121 − 297 − 11 − 11
           39    83    14   9
                                  1                                         33                       22        0

To recover the vectors α, β , γ from (7.6 f), form the two symmetric matrices W =
          = TTW T, where
T TW T , W
                        ⎡           ⎤            ⎡            ⎤
                             1                      1
                                  0                     0   0
                        ⎢ 2       1 ⎥            ⎢ 2 1        ⎥
                        ⎢ 0         ⎥            ⎢ 0 2 0 ⎥
                        ⎢         2 ⎥            ⎢            ⎥
                    T =⎢ 0 0 ⎥              T = ⎢ 0 0 1 ⎥ .
                        ⎢         1 ⎥            ⎢            ⎥
                        ⎣ 0 −2 ⎦                         1
                                                 ⎣ 0 2 0 ⎦
                          − 12    0                 1
                                                    2   0 0

                         is singular because, making use of (7.6 k) and (7.6 l),
Note that each of W and W

        b1 (1 − 2c1 )            b2 (1 − 2c2 )    W=             b1 (1 − 2c1 )
                                                                         b2 (1 − 2c2 )                     T TW T
                                                         1 T            
                                                       = 2 b − bT diag(c) W T
                                                       = 0,
                        2b1         2b2      b3   =
                                                  W              2b1    2b2       b3       TTW T
                                                       = bTW T
                                                       = 0.

It is found that                                      ⎤       ⎡ ⎤                 ⎡
                                                             ⎡      ⎤
                                                   β1           γ1
                                    α1        ⎢ ⎥  ⎢ ⎥
                          
        W = αα , W = β β − γγ , α =
              T          T   T      ⎣    ⎦ , β = ⎣ β2 ⎦ , γ = ⎣ γ2 ⎦ ,
                                      α2
                                                   β3           γ3

leading to                                        ⎡   √ ⎤
                                                     4 5
                                                  ⎢ 11   ⎥                  ⎡                                  ⎤
                ⎡                        ⎤        ⎢ 4√5 ⎥                                  − 163
                                                                                             297 − 11
                                                                                  41               14
                                                  ⎢ − 27 ⎥         ⎢              121                          ⎥
                      80
                                 − 297
                                   80
                                                  ⎢      ⎥
       W =⎣
                      121              ⎦, α =⎢           0 ⎥  ,  =⎢
                                                                   ⎢ − 163                                22 ⎥ .
                                                                                                          27 ⎥
                                                                W                            289
                                             ⎢           √ ⎥       ⎣ 297                     729             ⎦
                    − 297
                      80           80
                                             ⎢              ⎥
                                   729       ⎢          4 5⎥
                                                                     − 14                     22          13
                                             ⎣           √ ⎦
                                                         27            11                     27           8
                                                      − 4115
276                                                       7 B-series and geometric integration

                                  + γγT has rank 1 and that b1 γ2 + b2 γ2 + 1 b3 γ2 = 0.
Choose γ by the conditions that W                                  1        2   2      3
This gives                                    ⎡                       ⎤
                                                      √
                                                 65274 330−347009
                                              ⎢       1265902     ⎥
                                              ⎢ 70518√330+318613 ⎥
                                              ⎢−                  ⎥
                       65274√330−347009 − 1 ⎢
                                              ⎢
                                                      3107214
                                                      √           ⎥
                   β=                         ⎢  18285 330+162856 ⎥
                                                                  ⎥,
                                            2
                                               −
                             1265902          ⎢       460328      ⎥
                                              ⎢ 70518√330+318613 ⎥
                                              ⎢−                  ⎥
                                              ⎣       3107214
                                                      √           ⎦
                                                 65274 330−347009
                                                      1265902
                                              ⎡       √           ⎤
                                                  5934 330−70541
                                              ⎢       115082      ⎥
                                              ⎢ 23506√330−462231 ⎥
                                              ⎢−                  ⎥
                        5934√330−70541 − 1 ⎢⎢
                                                      1035738
                                                      √           ⎥
                   γ=                         ⎢  18285 330−423016 ⎥
                                                                  ⎥.
                                           2
                                               −
                              115082          ⎢       460328      ⎥
                                              ⎢ 23506√330−462231 ⎥
                                              ⎢−                  ⎥
                                              ⎣       1035738
                                                      √           ⎦
                                                  5934 330−70541
                                                      115082

This completes the construction of the method G6245.


7.7 Implementation

For practical use the method is ﬁrst transformed to real coefﬁcients so that (A,U, B,V )
is replaced by (A,UT, T −1 B, T −1V T ), where
                       ⎡               ⎤           ⎡               ⎤
                         1 0 0 0                      1 0 0 0
                       ⎢0 1 i 0⎥                   ⎢0 1 1 0⎥
                       ⎢               ⎥           ⎢               ⎥
                   T =⎢                ⎥ , T −1 = ⎢       2   2
                                                                   ⎥.
                       ⎣ 0 1 −i 0 ⎦                ⎣ 0 − 2i 2i 0 ⎦
                         0 0 0 1                      0 0 0 1

   For the remainder of this section, the notations (A,U, B,V ) will refer to the
transformed matrices with real coefﬁcients. That is,
                 ⎡                                                      ⎤
                     0      0     0       0          0   1 β1 α1 −γ1
                 ⎢                                                      ⎥
                 ⎢ a21 a22        0       0          0   1 β2 α2 −γ2 ⎥
                 ⎢                                                      ⎥
                 ⎢ a31 a32 a33            0          0   1 β3   0 −γ3 ⎥
                ⎢
              ⎢ a                                                      ⎥
                 ⎢ 41 a42 a43            a44         0   1 β2 −α2 −γ2 ⎥ ⎥
         AU
              =⎢ ⎢  a51 a52 a53          a54         0   1 β1 −α1 −γ1 ⎥ ⎥,
         BV      ⎢ b                                            0 0⎥
                 ⎢     1   b2     b3     b2        b1 1 0               ⎥
                 ⎢                                                      ⎥
                 ⎢ b1 α1 b2 α2 0 −b2 α2 −b1 α1 0 0 −1 0 ⎥
                 ⎢                                                      ⎥
                 ⎣ b1 β1 b2 β2 b3 β3 b2 β2        b 1 β1 0 1    0 0⎦
                    b1 γ1 b2 γ2 b3 γ3   b2 γ2     b1 γ1 0 0     0 −1

with G transformed to
7.8 Numerical simulations                                                                    277

                 G = T ∗ diag(1, − 12 , − 12 , 1)T = diag(1, −1, −1, 1).
   To satisfy the technical requirements of order six, a starting method needs to be
supplied such that
                             M h ◦ S h = S h ◦ E h + O(h7 ),
where S h : RN → (RN )4 is the mapping corresponding to the starting method, M h :
(RN )4 → (RN )4 is the mapping corresponding to a single step of the main method
and E h : RN → RN is the mapping corresponding to the ﬂow through a stepsize h. In
the case of this method, the ﬁrst component of S h should be chosen as the identity
mapping.


Trivial and enhanced starting methods
In our ﬁrst experiments, the remaining components were, for simplicity, set to the
zero mappings. Although this worked well it is possible to gain improvements with
little additional effort.
     Let Rh be a given starting method for the non-principal values. Calculate y[0] .
                                                                      [1]
y = Rh y0 . Use the method to ﬁnd y[1] . Evaluate y[1] . Evaluate Rh y1 . Then evaluate
  [0]
                          
(I −V )−1 y[1] − Rh y1 . Add this to y[0] to get R+
                       [1]
                                                  h.




7.8 Numerical simulations

These experiments are intended to test the ability of the new G-symplectic method
to approximately conserve the Hamiltonian in both short and long time integrations.
In each case the constant stepsize is chosen to be 0.1. For the short runs, 100 steps
are taken and, for the long runs, the number of steps is 106 , with the deviation of
the Hamiltonian from its initial value, Δ H = H(y(x)) − H(y0 ), sampled every 1000
steps.


Test problems
The two test problems are:

         Simple pendulum,     H(y) = 12 y22 − cos(y1 ),
                                                      T
                               y(0) =       5
                                            2    0        ,
         Hénon–Heiles,       H(y) = 12 (y23 + y24 ) + 12 (y21 + y22 ) + y21 y2 − 13 y32 ,
                                                                     T
                               y(0) =       0    3
                                                 10
                                                          9
                                                          25
                                                               11
                                                               50        .

Tests will be given for the fourth order G4123 method with the standard Gauss
method, with s = 2, p = 4, used for calibration. Tests will also be given for the sixth
order G6245, calibrated against the Gauss method with s = 3, p = 6.
278                                                          7 B-series and geometric integration

       ΔH
                                     Simple pendulum
      10−7



                                                                                            10
         0
                                                                               x
       ΔH

      10−7s                            Hénon–Heiles


                                                                                            10
         0
                                                                               x


      Figure 14 A comparison between the G4123 method (full line) and the 2-stage Gauss
      method (dotted line) for the Simple Pendulum and the Hénon–Heiles problems. For each
      method, h = 0.1, n = 100 (lower ﬁgure)




       ΔH
                                     Simple pendulum
      10−7

         0
                                                                                            10
                                                                               x

      ΔH
                                       Hénon–Heiles
      10−8

         0
                                                                                           10
                                                                               x



      Figure 15 Deviation of H from its initial value for the simple pendulum and the Hénon–
      Heiles problems, using G4123, with h = 0.05 and n = 200, compared with Gauss order 4
      (dotted line), with h = 0.1 and n = 100




Fourth order methods

   The ﬁrst experiment presented here, with results shown in Figure 14, compares
G4123 with the 2 stage Gauss method. These results are misleading because the
G4123 method is less costly by a factor of at least two, assessed in terms of the effort
expended on the iterations. To obtain a more realistic comparison, the simulations
are repeated with h = 0.05 and n = 200 steps for G4123 compared with h = 0.1
and n = 100 for the Gauss method. These adjusted results are shown in Figure 15.
Further comparisons between G4123 with h = 0.05 and the Gauss order 4 method
with h = 0.1 are presented in Figure 16. In this case, the results are for the time
interval [0, 105 ]. Based on this experiment, there seems to be no reason to discount
the advantages of G4123.
7.8 Numerical simulations                                                                    279

      ΔH
      10−7
                                       Simple pendulum; G4123
                                                                                            105
         0
                                                                          x

      ΔH
      10−7                              Simple pendulum; Gauss
                                                                                            105
         0
                                                                          x

      ΔH
      10−8
                                         Hénon–Heiles; G4123                               105
         0
                                                                          x

      ΔH

   2 × 10−8



                                         Hénon–Heiles; Gauss
                                                                                            105
         0
                                                                          x




    Figure 16 Deviation of H from its initial value for the simple pendulum and Hénon–
    Heiles problems using G4123 (n = 2 × 106 time steps and h = 0.05), with Gauss order 4
    (n = 106 , h = 0.1, dotted line) shown for comparison



      ΔH
    10−10




                                                                                            10
         0

                                                                              x


   −10−10




    Figure 17 Deviation of H from initial value for the simple pendulum using G6245, with
    Gauss order 6 (dotted line) shown for comparison




Experiments with G6245

The results of the short term simulation for the simple pendulum are given in Figure 17
and for the Hénon–Heiles problem in Figure 18 (p. 280).
280                                                             7 B-series and geometric integration


       ΔH

  5×10−12



         0
                                                                                               10
                                                            x

 −5×10−12




      Figure 18 Deviation of H from initial value for the Hénon–Heiles problem using G6245,
      with Gauss order 6 method (dotted line) shown for comparison



       ΔH
  5 × 10−11



          0
                                                                                                    105


 −5 × 10−11
                                               G6245




       ΔH
                                                Gauss
  5 × 10−11



          0
                                                                                                    105


 −5 × 10−11




      Figure 19 Deviation of H from initial value for the simple pendulum using G6245 (upper
      ﬁgure), with the Gauss order 6 method (lower ﬁgure) shown for comparison



   Integrations through only 100 time steps are not demanding but we can at least
conclude that the new sixth order method has comparable conservation behaviour as
for the much more expensive fully implicit Gauss method of the same order.
   For more stringent tests, the same problems can be attempted with the same
methods but for 106 time steps. These are given in Figure 19 for the simple pendulum
and Figure 20 for the Hénon–Heiles problem.


Promising results to be treated with caution
The result of this and other numerical tests have given very encouraging results for
millions of time steps and it is tempting to assume that there is no real limit as to
how far stable behaviour would continue.
7.9 Energy preserving methods                                                               281

       ΔH
  5 × 10−12

                                              G6245


          0
                                                                                            105


       ΔH
          0
                                                                                            105

                                              Gauss


 −5 × 10−12



    Figure 20 Deviation of H from initial value for the Hénon–Heiles problem using G6245
    (upper ﬁgure), with the Gauss order 6 method (lower ﬁgure) shown for comparison



   However, this is an unrealistic expectation because, from the analysis in [40]
(D’Ambrosio, Hairer, 2014), parasitism will eventually take over and destroy the
integrity of the numerical results.



7.9 Energy preserving methods

We will consider Poisson problems y = S∇H in the case that the skew matrix S is
constant. This includes Hamiltonian problems for which the dimension is even and
S = JT.
   For a given t = [t 1 t2 · · · t m ], deﬁne

                            h(t) = H (m) F(t1 )F(t 2 ) · · · F(t m ),

with h(τ) = H. No meaning is given to h(∅).
   Examples to order 4 are
                                   h( ) = h(t1 )         = H,
                                   h( ) = h(t2 )       = H  f,
                                  h( ) = h(t3 ) = H  ff,
                                   h( ) = h(t4 ) = H  f  f,
                                 h( ) = h(t5 )= H  fff,
                                  h( ) = h(t6 )= H  ff  f,
                                  h( ) = h(t7 )= H  f  ff,
                                     
                                  h     = h(t8 )= H  f  f  f.
282                                                              7 B-series and geometric integration

Exercise 60 Find h(t13 ).

   Analogously to the inﬁnite row vector Bh , indexed on T # , deﬁne

                 Hh =       h(t1 )   hh(t2 )      h2 21 h(t3 )   h2 h(t4 )   ··· ,

with typical term
                                 Hh (t) = h|t|−1 σ (t)−1 h(t).

Because there is no term corresponding to y0 , we use a truncated version of Λ ,
denoted by Λ , with the ﬁrst row and column deleted and the remaining rows and
columns indexed by T × T.


 Theorem 7.9A Let y1 = (Bh y0 )a, where a ∈ B . Then

                                     Hh y1 = (Hh y0 )Λ (a).

Proof. Use Theorem 3.3A (p. 110), applied to
                                                            
                               H (n) y0 + ∑ σ (t)−1 h|t| F(t) .
                                           t∈ T



In particular


 Corollary 7.9B For a Runge–Kutta method with elementary weights t → Φ(t),

                                     Hh (y1 ) = (Hh y0 )ΦD.

For conservation of energy, we would need H(y1 ) = H(y0 ) and our aim now is to
ﬁnd conditions for this.


Operating with S on H (n+1)

Expressing the n + 1-fold derivative of H (n+1) and then operating on this with the
matrix S requires some care because we really need to combine the contravariant
derivative ∇H with an n-fold covariant derivative. The operation that needs to be
performed gives a result
                               ∂n            ∂n
                                   S∇H  =  S      ∇H,                       (7.9 a)
                              ∂ yn           ∂ yn
with component i ∈ {1, 2, . . . , N} of the n-linear operator in (7.9 a) acting on
v1 , v2 , . . . vn given by
7.9 Energy preserving methods                                                                                     283

                                        N
                                       ∑ Si j H j,k1 k2 ···kn vk11 vk22 · · · vk22 .
                                       j=1

In the remainder of this chapter, we will denote by Ṡ, the operator which acts on H (n)
to produce the quantity given by (7.9 a). That is

                                                                 ∂n
                                              ṠH (n) := S            ∇H.
                                                                 ∂ yn


Taylor expansion for H(Bh y0 a)
Because of the special form of Poisson problems, we have

  Theorem 7.9C Given t, t  ∈ T,

                                              h(t ∗ t ) = −h(t  ∗ t)

Proof. Let t = [t 1 t 2 · · · t m ], t  = [t 1 t2 · · · t m ] and write F i := F(t i ), F i := F(t i ). Then,
assuming the summation convention,
                                                                                                      
               h(t ∗ t ) = Hi j1 j2 ··· jm F 1j1 F 2j2 · · · F mjm Sik Hk1 2 ···n F 11 F 21 · · · F nn ,
                                                               
               h(t ∗ t) = Hk1 2 ···n F 11 F 21 · · · F nn Ski Hi j1 j2 ··· jm F 1j1 F 2j2 · · · F mjm

and the result follows because Sik = −Ski .

   By applying this result, step-by-step, through all trees in a free tree class, we ﬁnd
that
                        h(t) = ±h(t ) whenever t ∼ t  ,
with the actual sign determined by the parity of the number of steps between the two
roots.

A non-superﬂuous example
We will give two examples. The ﬁrst is given by a diagram in which a representative
tree has a + sign attached to the root and an appropriate sign attached to the other
vertices which would apply if this vertex became the root.
                                                         +       +
                                                             −                                                 (7.9 b)
                                                     −               −
                                                             +

This diagram indicates that

                  h([[[τ[τ]]]]) = −h([τ[τ[τ]]]) = −h([[[τ]2 ]]) = h([τ[τ]2 ]),                                 (7.9 c)
284                                                      7 B-series and geometric integration

where we recall the notation

               [[[τ[τ]]]] = ,    [τ[τ[τ]]] = ,    [[[τ]2 ]] = ,   [τ[τ]2 ] =

In the Taylor expansion of H(y1 ), given in Theorem 7.9A, four terms involving the
trees arising in the present discussion will be
                                                           
  h5       1           2         2
           2 (ΦD) [τ[τ] ] h [τ[τ] ]+ (ΦD) [[[τ[τ]]]] h [[[τ[τ]]]]
                                                                        
                + (ΦD) [τ[τ[τ]]] h [τ[τ[τ]]] + 12 (ΦD) [[[τ]2 ]]     h([[[τ]2 ]] .    (7.9 d)

Because of (7.9 c), (7.9 d) collapses to a single term
                                             
       1 5
       2 h   (ΦD) [τ[τ]2 ] + 2(ΦD) [[[τ[τ]]]]
                                                                       
                               − 2(ΦD) [τ[τ[τ]]] − (ΦD) [[[τ]2 ]] h [τ[τ]2 ]
                                                             
    = 12 h5 Φ(t1 )Φ(t2 )2 + 2Φ(t15 ) − 2Φ(t1 )Φ(t6 ) − Φ(t13 ) h(t22 ).



An example based on a superﬂuous tree

Now consider the example of a superﬂuous tree, again with signs attached to the
vertices, in accordance with Theorem 7.9C and its consequences.

                                                 ++
                                           −−
                                                 −
                                             +


If we now list the corresponding terms in the Taylor expansion of H(y1 ), it can be
seen that each of these terms disappears because, for any tree t in the equivalence
class, any term (ΦD)(t)h(t) is matched by a corresponding −(ΦD)(t)h(t).


A sufﬁcient condition for energy preservation


 Theorem 7.9D An integration method is energy preserving if for every similarity
 class the following is true based on a representative t in this class. For every tree
 t ∼ t  let n(t) be the number of steps from the root of t to the root of t. Then

                                   ∑ (−1)n(t) ΦD(t) = 0.                             (7.9 e)
                                   t∼t 
7.9 Energy preserving methods                                                          285

The Average Vector Field method

The Average Vector Field method [80] (Quispel, McLachlan, 2008) is an integration
method (A, bT , c) on the index set [0, 1] with
                                                                 1
                                  (Aφ )ξ = ξ                         φη d η,
                                                             0
                                                            1
                                     bT φ =                      φη d η,
                                                        0
                                      cξ = ξ .

The stages and output in a step are given by
                                                                 1
                                Yξ = y0 + ξ h                        f (Yη ) d η,   (7.9 f)
                                                             0
                                                            1
                                y 1 = y0 + h                     f (Yη ) d η,       (7.9 g)
                                                        0

from which it follows that Yξ = (1 − ξ )y0 + ξ y1 and the method can be written in
the compact form
                                           1                     
                       y 1 = y0 + h            f (1 − ξ )y0 + ξ y1 d ξ .
                                       0




Energy preservation of the Average Vector Field method

Let Φ(t), t ∈ T, be the elementary weights, that is, the B-series coefﬁcients for y1 .


The values of Φ(t)
The B-series coefﬁcients for stage ξ are ξ Φ(t) and hence,
                                                   1             m
                                Φ(t) =                 ξ m ∏ Φ(ti ) d ξ
                                               0             i=1
                                                              m
                                            1
                                      =            Φ(ti ).
                                           m+1 ∏
                                               i=1

It now follows that

 Theorem 7.9E For the Average Vector Field method, Φ(t)−1 is the product over
 each vertex of t of (1+ the number of children of this vertex). Furthermore,
 (ΦD)(t)−1 is the product over each vertex of t, except for the root, of (1+ the
 number of children of this vertex).
286                                                                   7 B-series and geometric integration

Approaching this result in a different way by writing t in the form (V, E, r), where V
and E are ﬁxed and r runs through all members of V , makes it possible to use the
criterion for energy preservation given by Theorem 7.9D.

 Theorem 7.9F The AVF method is energy preserving.

Proof. Given a free tree (V, E) and r ∈ V , deﬁne valency(r) as the number of members
{x, y} ∈ E such that x = r or y = r. Then from Theorem 7.9E,

                                                      valency(r)
                            (aD)(t)−1 =                              .
                                                     ∏ j∈V valency j

It now follows that the coefﬁcient in (7.9 e) is

                                  ∑i∈V ±valency(i)
                                                   ,                                              (7.9 h)
                                  ∏∈V valency( j)
where the sign ± alternates along each edge. There is now a cancellation of the terms
in the numerator of (7.9 h)
To illustrate the cancellation that takes place in this proof, consider the following
diagram, based on (7.9 b).
                                    +                         +
                                    –                –        –
                                         –                –
                                                  +
                                                 + +
   The number of edges ending at any vertex is equal to the valency and these edges
also meet a vertex with the opposite sign. Hence the cancellation.


An alternative proof
The second proof is a special case of the proof of Theorem 7.9E, below, and should
be looked on as an introduction to that result.
Proof. We have
                                     1 d
               H(y1 ) − H(y0 ) =            H(Yη ) d η
                                   0 dη
                                     1           d
                               =       H  (Yη ) Yη d η
                                   0            dη
                                     1        T  1         
                               =        ∇Hη h          S∇Hξ dξ d η
                                     0                            0
                                                1          T               1             
                                =h                   ∇Hη d η S                    ∇Hη d η
                                             0                            0
                                = 0,

because of the skew-symmetry of S.
7.9 Energy preserving methods                                                                              287

A generalization of the AVF method

We consider the method deﬁned by replacing (7.9 f) and (7.9 g) by
                                                      1
                                Yξ = y0 + h               Ψ (ξ , η) f (Yη ) d η,                      (7.9 i)
                                                  0
                                y1 = Y1 ,                                                             (7.9 j)

                              (ξ , η) := ∂Ψ (ξ , η)/∂ ξ exists and is continuous for
where the partial derivative Ψ
ξ , η ∈ [0, 1].


 Theorem 7.9G If Ψ    (ξ , η) = Ψ
                                  (η, ξ ), then the method deﬁned by (7.9 i) and
 (7.9 j) preserves energy.

Proof. Given ε > 0, by the Weierstrass approximation theorem there exists a polyno-
mial in two variables Π such that Ψ   (ξ , η) = Π (ξ , η) + E(ξ , η), with |E(ξ , η)| ≤ ε
for
   ξ , η ∈  [0, 1]. Without
                            loss of generality (because Π (ξ , η) can be replaced by
  Π (ξ , η) + Π (η, ξ ) /2), assume that Π (ξ , η) = Π (η, ξ ).
   Let
                                                               T
     Π (ξ , η) =        1   ξ     ξ2    ···       ξ n−1            M   1   η       η2   ···   η n−1    ,

where M is an n × n symmetric matrix. From standard decomposition results for
symmetric matrices, there exists an m × n matrix N and a diagonal m × m matrix D,
such that M = N T DN. It then follows that Π (ξ , η) = ∑m
                                                        i=1 di ϖi (ξ )ϖi (η), where the
polynomial ϖi has coefﬁcients given by row number i in N. We can now write
                                            m
                             (ξ , η) = ∑ di ϖi (ξ )ϖi (η) + E(ξ , η)
                            Ψ
                                            i=1

and we obtain

         H(y1 ) − H(y0 )
                d1
         =          H(Yξ ) d ξ
           0 dξ
             1           d
         =     H  (Yξ ) Yξ d ξ
           0            dξ
             1        T  1                   
         =      ∇Hξ h           (ξ , η)S∇Hη d η d ξ
                               Ψ
             0                   0
             1         T     1 m                                                   
         =
             0
                     ∇Hξ h
                                0
                                  ∑ di ϖi (ξ )ϖi (η)+E(ξ , η) S∇Hη d η d ξ.                           (7.9 k)
                                  i=1

The coefﬁcient of hdi in (7.9 k) is
288                                                                          7 B-series and geometric integration
                       1            T             1                        
                           ∇Hξ                           ϖi (ξ )ϖi (η)S∇Hη d η d ξ
                   0                             0
                               1                                    1                
                  =                 ϖi (ξ )∇Hξ d ξ S                       ϖi (η)∇Hη d η ,
                            0                                      0

which vanishes because of skew-symmetry of S. Hence,
                   3               3
                   3H(y1 ) − H(y0 )3
                       3 1 1                              3
                       3                  T             3
                       3
                    ≤ h3       E(ξ , η) ∇Hξ S∇Hη d η d ξ 33
                                    0        0
                                                 1 3  T    3
                                         1
                                                  3          3
                       ≤ εh                       3 ∇Hξ S∇Hη 3 d η d ξ .
                                     0       0

Because this can be made arbitrarily small, H(y1 ) = H(y0 ).




A fourth order method

We will construct an energy preserving method based on a polynomial

                Ψ (ξ , η) = 2aξ 2 η + 2bξ η + bξ 2 + (1 − a − 2b)ξ ,

where the coefﬁcients are chosen subject to the symmetry of ∂Ψ /∂ ξ and the consis-
                          5
tency condition Φ(t1 ) = 01 Ψ (1, η) d η = 1. Evaluation of the remaining elementary
differentials up to order 4 give

             Φ(t2 ) = 12 ,
             Φ(t3 ) = 13 ,
             Φ(t4 ) = 14 − 36
                           1       1
                              a + 36 (a + b)2 ,
             Φ(t5 ) = 14 ,
             Φ(t6 ) = 16 − 72
                           1       1
                              a + 360                     1
                                      (a + b)(6a + 5b) − 360 (a + b)3 ,
             Φ(t7 ) = 16 − 36
                           1       1
                              a + 180                     1
                                      (a + b)(6a + 5b) − 180 (a + b)3 ,
             Φ(t8 ) = 18 − 36
                           1       1
                              a + 36 (a + b)2 ,

and to obtain order 4, by requiring that Φ(t) = 1/t!, up to this order we need to
satisfy
                                          (a + b)2 − a = 3,
                       −(a + b)3 + (a + b)(6a + 5b) − 5a = 15,

with solution −a = b = 3.
7.9 Energy preserving methods                                                     289

Summary of Chapter 7

Although it has not been possible to survey all aspects of the burgeoning subject of
Geometric Integration, symplectic Runge–Kutta methods and their generalization
to general linear methods are introduced to the extent that their main properties are
studied and explained. It is perhaps surprising that G-symplectic methods perform
well over millions of time steps, even though, according to [40], they will eventually
fail.
    In Section 7.9, energy preserving methods were introduced, based on integration
methods, in the sense of Chapter 4, also known as Continuous Stage Runge–Kutta
methods.


Teaching and study notes

The following books and articles are essential reading, and provide a starting point
for further studies on Geometric Integration.
Cohen, D. and Hairer, E. Linear energy-preserving integrators for Poisson systems,
(2011) [31]
Hairer, E. Energy-preserving variant of collocation methods (2010) [48]
Hairer, E., Lubich, C. and Wanner, G. Geometric Numerical Integration: Structure-
Preserving Algorithms for Ordinary Differential Equations, (2006) [49]
Iserles, A., Munthe-Kaas, H.Z., Nørsett, S.P. and Zanna, A. Lie-group methods,
(2000) [62]
Miyatake, Y. An energy-preserving exponentially-ﬁtted continuous stage Runge–
Kutta method (2014) [73]
McLachlan, R. and Quispel, G. Six lectures on the geometric integration of ODEs
(2001) [71]
Sanz-Serna, J.M. and Calvo, M.P. Numerical Hamiltonian Problems, (1994) [85]


Projects
                                                            1
Project 26   Derive a method similar to G6245 but with c2 = 10 .
Project 27 Consider the consequence of replacing (7.6 e) (p. 272) by
V = diag(1, exp(iθ ), exp(−iθ ), −1), for 0 < θ < π, in the G6245 method.
