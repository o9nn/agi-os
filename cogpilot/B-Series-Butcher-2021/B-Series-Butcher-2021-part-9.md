# B-Series: Algebraic Analysis of Numerical Methods - Part 9

**Author:** John C. Butcher

**Series:** Springer Series in Computational Mathematics, Volume 55

---

Answers to the exercises




Chapter 1


Exercise 1 (p. 5)

The function f and the components of y0 are
                           f 0 = 1,                                 y00 = 1,
                           f 1 = y2 ,                               y10 = 2,
                           f = 2y − 3y + y + cos(y ),
                            2         1       2    3        0
                                                                    y20 = −2,
                           f 3 = y4 ,                               y30 = 1,
                           f = y − y + (y ) + y + sin(y ),
                            4     1       2       3 2   4       0
                                                                    y40 = 4.



Exercise 2 (p. 6)

Substitute
                                z = A exp(2t) + B exp(it) +C exp(−it)
into
                                 d z/ dt − 2z − 2i exp(iz) − i exp(−iz)
and obtain
               (2A − 2A) exp(2t) + (iB − 2B − 2) exp(it) + (−iC − 2C − 1) exp(−it).
This is zero for all t iff B = − 45 = 25 i and C = − 25 + 15 i. Add the condition z(0) = 1 to obtain
A + B +C = 1. Hence, A = 11          1
                               5 + 5 i.




Exercise 3 (p. 6)


                                          5 exp(2t) − 5 cos(t) + 5 sin(t),
The real and imaginary components are x = 11          6          3

y = 5 exp(2t) − 5 cos(t) − 5 sin(t).
    1           2          1




© Springer Nature Switzerland AG 2021                                                                  291
J. C. Butcher, B-Series, Springer Series in Computational Mathematics 55,
https://doi.org/10.1007/978-3-030-70956-3
292                                                                                     Answers to the exercises

Exercise 4 (p. 7)

Given y, z ∈ RN , let
                               R                                                  R
            y = y0 +                     (y − y0 ),            
                                                                z = y0 +                     (z − y0 ),
                        max(y − y0 , R)                                  max(z − y0 , R)
where y and 
             z are shown in three cases, relative to {y : y − y0  ≤ R},
                                                                                               y

                  y                                y                                      y
                                                                           z                                  z

                                                                     
                                                                     z                                    
                                                                                                          z
                            z
             y0                               y0                                        y0




In each case the Lipschitz condition follows from
                          f(y) − f(z) ≤  f (
                                                 y) − f (       y −
                                                         z) ≤ L  z ≤ Ly − z.


Exercise 5 (p. 11)
                                          ⎡                                      ⎤
                                               u1 −hu2
                                                       + (1+h0.40001h
                                        ⎢       1+h2          2 )(1+100h)2
                                                                                 ⎥
                                        ⎢                                        ⎥
                                        ⎢      u2 +hu1           0.40001h2       ⎥
                                 F(u) = ⎢                 +                      ⎥.
                                        ⎢       1+h2          (1+h2 )(1+100h)2   ⎥
                                        ⎣                                        ⎦
                                                                u3
                                                              1+100h
Stability is guaranteed by the power-boundedness of the matrix
                                       ⎡                ⎤
                                             2 − 1+h2
                                           1      h
                                       ⎢  1+h           ⎥
                                       ⎣                ⎦,
                                                    h          1
                                                   1+h2       1+h2

and the boundedness of (1 + 100h)−n for positive integral n.


Exercise 6 (p. 13)

                                                          1/2
In this and the following answer, r := (y1 )2 + (y2 )2 )         so that
                         
H(x) = 12 (y3 )2 + (y4 )2 − r−1 , ∂ r−1 /∂ y1 = −y1 /r3 ∂ r−1 /∂ y2 = −y2 /r3 .
We now ﬁnd
H  = (∂ H/∂ y1 )(y1 ) + (∂ H/∂ y2 )(y2 ) + (∂ H/∂ y3 )(y3 ) + (∂ H/∂ y4 )(y3 )
    = −(y1 /r3 )y3 − (y2 /r3 )y4 + y3 y1 /r3 + y4 y2 /r3 = 0.


Exercise 7 (p. 13)

A = (∂ A/∂ y1 )(y1 ) + (∂ A/∂ y2 )(y2 ) + (∂ A/∂ y3 )(y3 ) + (∂ A/∂ y4 )(y3 )
   = y4 y3 − y3 y4 + y2 y1 /r3 − y1 y2 /r3 = 0.
Answers to the exercises                                                                           293

Exercise 8 (p. 14)

Evaluate in turn
                                 y = y + sin(x),
                                 y = y + cos(x) = y + sin(x) + cos(x),
                               y(3) = y − sin(x) = y + cos(x),
                               y(4) = y(3) − cos(x) = y,
                               y(5) = y(4) + sin(x) = y + sin(x),
                               y(6) = y(5) + cos(x) = y + sin(x) + cos(x),
                               y(7) = y(6) − sin(x) = y + cos(x).



Exercise 9 (p. 15)

(a) It is possible that the result error vanishes so that the evaluation of r fails because of the zero
    division.
(b) Even if error is non-zero but small, the value of r might be very large, resulting in an
    unreasonably large value of yout. In practical solvers, the value of the stepsize ratio is not
    allowed to exceed some heuristic bound such as 2.
(c) Similarly a very small value of r needs to be avoided and a heuristic lower bound, such as 0.5
    is imposed in practical solvers.



Exercise 10 (p. 18)

For 2 orbits with n steps, h = 8/n. The number of steps in successive quadrants are m + 1, m + 1,
m + 2, m + 2, m + 3, m + 3, m + 4, m + k − 16, giving a ﬁnal position
                                                               
   2m+4 −1           2m+4 −1         2m+6     1      2m+k−14       1
    n/8           +   n/8 −1       +  n/8 −1       +     n/8
             1                                                     1
                                                                                            
                                                                        1   8m + 9k − 128
                                                                      =n                       ,
                                                                               8(k − 20)
which is                                                        
                                                8       k − 16
                                                n       k − 20
from the starting point.



Exercise 11 (p. 21)

                   y(x0 + h) − y(x0 ) − hF2
                                                                                         
                   = y(x0 + h) − y(x0 ) − hy (x0 + 12 h) + y (x0 + 12 h) − F2
                       
                   = hy (x0 ) + 12 h2 y (x0 ) + 16 h3 y(3) (x0 ) − hy (x0 ) − 12 h2 y (x0 )
                                                                             
                            − 18 h3 y(3) (x0 ) + h 18 h2 fy (x0 , y0 )y (x0 ) + O(h4 )
                          h y (x0 ) + 18 h3 fy (x0 , y0 )y (x0 ) + O(h4 ).
                       1 3 (3)
                     = 24
294                                                                                 Answers to the exercises

Exercise 12 (p. 21)

              y(x0 + 13 h) −Y2 = O(h2 ),hy (x0 + 13 h) − hF2 = O(h3 ),
              y(x0 + 23 h) −Y3 = O(h3 ),hy (x0 + 23 h) − hF3 = O(h4 ),
                y(x0 + h) − y1 = y(x0 + h) − y0 − 14 hy (x0 ) − 34 hy (x0 + 23 h) + O(h4 )
                                                                  = O(h4 ).


Exercise 13 (p. 21)
                                          1 3                          1 4 2 
In this answer J := fy (x0 , y0 ), Δ2 := 32 h Jy (x0 ), Δ3 := 192
                                                               1 4 (3)
                                                                  h Jy + 64 h J y (x0 ),
                                              1           1 2 
        y(x0 + 4 h)−Y2 = y(x0 + 4 h)−y0 − 4 hy (x0 ) = 32 h y (x0 )+O(h ),
               1                   1                                     3

   hy (x0 + 14 h)−hF2 = Δ2 +O(h4 ),
       y(x0 + 12 h)−Y3 = y(x0 + 12 h)−y0 − 12 hy (x0 + 14 h)+ 12 Δ2 +O(h4 )
                          1 3 (3)   1 3 
                       = 192 h y + 64 h Jy (x0 )+O(h4 ),
   hy (x0 + 12 h)−hF3 = Δ3 +O(h5 ),
        y(x0 +h)−Y4 = y(x0 +h)−hy0 +2hy (x0 + 14 h)
                       −2hy (x0 + 12 h)−2Δ2 +O(h4 )
                                     1 3 
                       = − 48 h y − 16
                           1 3 (3)
                                       h Jy (x0 )+O(h4 ) hy (x0 +h)−hF4                = −4Δ3 +O(h4 ),
        y(x0 +h)−y1 = y(x0 +h)−y0
                       − 16 hy (x0 )− 23 hy (x0 + 12 h) − 16 hy (x0 +h)+O(h5 )
                       = O(h5 ).


Exercise 14 (p. 30)

The preconsisitency condition is ρ(1) = 32 − a1 = 0, implying a1 = 32 . The consistency condition
then becomes ρ  (1) − σ (1) = (2 − 32 ) − (b1 + 1) = 0, implying b1 = − 12 . The method
(w2 − 32 w + 12 , − 12 w + 1) is stable because the roots of ρ(w) = 0 are 1 and 12 .


Exercise 15 (p. 30)

Using the relation w = 1 + z and writing every series in z only to z2 terms, we have
                        ρ(1 + z)/z = (w3 − w2 )/(w − 1) = w2 = 1 + 2z + z2 ,
                          σ (1 + z) = (1 + 2z + z2 )(1 + 12 z − 12
                                                                 1 2
                                                                   z )
                                                  12 = 12 w − 3 w + 12 .
                                           5           23 2
                                     = 1 + 12 z + 23          4     5




Exercise 16 (p. 31)

Use the relation w = 1 + z and write every series up to terms in z3 .
                          ρ(1 + z)/z = (1 + z)2 ;
                            σ (1 + z) = (1 + 2z + z2 )(1 + 12 z − 12
                                                                  1 2    1 3
                                                                     z + 24 z )
                                                        2  3 3
                                       = 1 + 52 z + 23
                                                    12 z + 8 z

                                                 24 w − 24 w + 24 .
                                       = 38 w3 + 19  2   5      1
Answers to the exercises                                                                                                      295

Exercise 17 (p. 36)


(a)       , (b)              , (c)            .


Exercise 18 (p. 36)

(a) f  ff  f 2 , (b) f  f  ff  f  f, (c) f  f(f  f)2 .



Chapter 2

Exercise 19 (p. 40)
The result uses induction on n = #V . For n = 1 there are no edges and each of the statements is true.
For n > 1, the result is assumed for #V = n − 1. Add an additional vertex and an additional edge is
also required to maintain connectivity without creating a loop. However, any additional edge will
produce a loop.


Exercise 20 (p. 47)
                                               t = [[τ 2 ][2 τ 2 ]2
                                                  = (τ ∗ ((τ ∗ τ) ∗ τ)) ∗ (τ ∗ ((τ ∗ τ) ∗ τ))
                                                  = τ2 τ2 τ 2 τ1 τ2 τ 2 .


Exercise 21 (p. 47)
                                   
[[τ 3 ]2 ], τ ∗ ((τ ∗ τ) ∗ τ) ∗ (τ ∗ τ ∗ τ).


Exercise 22 (p. 49)
The four trees, with the ∼ links shown symbolically, are


                   t33 = t1 ∗ t13 ∼ t13 ∗ t1 =t22 = t6 ∗ t2 ∼ t2 ∗ t6 =t24 = t15 ∗ t1 ∼ t1 ∗ t15 =t35


Exercise 23 (p. 49)
The ﬁve trees, with the ∼ links shown symbolically, are



      t32 = t1 ∗ t6 ∼ t6 ∗ t1 =t21 = t3 ∗ t4 ∼ t4 ∗ t3 =t27


                                                                            = t7 ∗ t2 ∼ t2 ∗ t7 =t25 = t16 ∗ t1 ∼ t1 ∗ t16 =t35


Exercise 24 (p. 56)
In the factors on the left of (2.4 a), the factor (1 − [τ])−1 must be removed because no descendants
of any vertexcan contain .
296                                                                                             Answers to the exercises

Exercise 25 (p. 69)

First calculate p-weight(1 + 22 ) = 5!/1!2!2!2 = 15. The 15 results are
1 + 23 + 45, 1 + 24 + 35, 1 + 25 + 34, 2 + 13 + 45, 2 + 14 + 35, 2 + 15 + 34, 3 + 12 + 45, 3 + 14 + 25, 3 + 15 + 24,
4 + 12 + 35, 4 + 13 + 25, 4 + 15 + 23, 5 + 12 + 34, 5 + 13 + 24, 5 + 14 + 23.



Exercise 26 (p. 78)

s01 s21 s01 s10 and s01 s01 s21 s10 .


Exercise 27 (p. 79)


             1           τ1         τ2 τ            τ 1 τ1           τ3 ττ         τ2 ττ1         τ1 τ2 τ         τ 1 τ1 τ1
   1         1           τ1         τ2 τ            τ 1 τ1           τ3 ττ         τ2 ττ1         τ1 τ2 τ         τ 1 τ1 τ1
   τ1        τ1         τ1 τ1      τ1 τ2 τ         τ1 τ1 τ1         τ1 τ3 ττ      τ1 τ2 ττ1      τ1 τ1 τ2 τ      τ1 τ1 τ1 τ1
  τ2 τ      τ2 τ        τ2 ττ1    τ2 ττ2 τ        τ2 ττ1 τ1        τ2 ττ3 ττ     τ2 ττ2 ττ1     τ2 ττ1 τ2 τ     τ2 ττ1 τ1 τ1
 τ1 τ1     τ1 τ1       τ1 τ1 τ1   τ1 τ1 τ2 τ      τ1 τ1 τ1 τ1      τ1 τ1 τ3 ττ   τ1 τ1 τ2 ττ1   τ1 τ1 τ1 τ2 τ   τ1 τ1 τ1 τ1 τ1
 τ3 ττ     τ3 ττ       τ3 τττ1    τ3 τττ2 τ      τ3 τττ1 τ1        τ3 τττ3 ττ    τ3 τττ2 ττ1    τ3 τττ1 τ2 τ    τ3 τττ1 τ1 τ1
 τ2 ττ1 τ2 ττ1 τ2 ττ1 τ1 τ2 ττ1 τ2 τ τ2 ττ1 τ1 τ1 τ2 ττ1 τ3 ττ τ2 ττ1 τ2 ττ1 τ2 ττ1 τ1 τ2 τ τ2 ττ1 τ1 τ1 τ1
 τ1 τ2 τ τ1 τ2 τ τ1 τ2 ττ1 τ1 τ2 ττ2 τ τ1 τ2 ττ1 τ1 τ1 τ2 ττ3 ττ τ1 τ2 ττ2 ττ1 τ1 τ2 ττ1 τ2 τ τ1 τ2 ττ1 τ1 τ1
τ1 τ1 τ1 τ1 τ1 τ1 τ1 τ1 τ1 τ1 τ1 τ1 τ1 τ2 τ τ1 τ1 τ1 τ1 τ1 τ1 τ1 τ1 τ3 ττ τ1 τ1 τ1 τ2 ττ1 τ1 τ1 τ1 τ1 τ2 τ τ1 τ1 τ1 τ1 τ1 τ1



Exercise 28 (p. 87)

Use the recursion t n = t n−1 ∗ τ starting with Δ (t 0 ) = Δ (τ) = 1 ⊗ τ + τ ⊗ ∅. By Theorem 2.8D,
Δ (t n ) = Δ (t n−1 ) ∗ Δ (τ)
                  n−1 n−1−i                                  
         = ∑n−1
              i=0     i τ      ⊗ t i + t n−1 ⊗ ∅ ∗ 1 ⊗ τ + τ ⊗ ∅
            n−1 n−1 n−1−i                                                      
         = ∑i=0 i τ            ⊗ t i ∗ 1 ⊗ τ + τ ⊗ ∅ + (t n−1 ⊗ ∅) ∗ 1 ⊗ τ + τ ⊗ ∅
                  n−1 n−1−i                       n−1 n−1−i      
         = ∑n−1
              i=0     i τ      ⊗ t i ∗(1 ⊗ τ)+ ∑n−1
                                                  i=0    i τ       ⊗ t i ∗(τ ⊗ ∅)+(t n−1 ⊗ ∅)∗(τ ⊗ ∅)
                                                           
                 i τ
                     n−1−i ⊗ t
         = ∑n−1                            i τ    ⊗ t i + (t n ⊗ ∅)
                n−1                   n−1 n−1 n−i
            i=0                i+1 + ∑i=0
                   
         = ∑ni=0 ni τ n−i ⊗ t i + (t n ⊗ ∅).


Exercise 29 (p. 87)

Write Δ (t n ) = Dn + t n ⊗ ∅, with D0 = 1 ⊗ τ. To ﬁnd Dn ,
                                   Δ (t n ) = (1 ⊗ τ + τ ⊗ ∅) ∗ (Dn−1 + t n−1 ⊗ ∅)
                                             = (1 ⊗ τ) ∗ Dn−1 + t n−1 ⊗ τ + t n ⊗ ∅,
and it follows that Dn = (1 ⊗ τ) ∗ Dn−1 + t n−1 ⊗ τ. It can be veriﬁed by induction that
Dn = ∑n−1i=1 t n−i ⊗ t i so that
                                                           n−1
                                               Δ (t n ) = ∑ t I ⊗ t n−i + t n ⊗ ∅.
                                                             i=1
Answers to the exercises                                                                            297

Exercise 30 (p. 90)


Denote the vertices of t = [τ n ] by 0, 1, 2, . . . , n, where 0 is the root. The partitions of t are
(a) n + 1 singleton vertices,
(b) n − i singleton vertices and an additional tree [τ i ], i = 1, 2, . . . n − 1, and
(c) the one element partition t.
The signed partition contributed
                                 by (a) is (−1)n+1 τ n+1 , the signed partitions contributed by (b),
with 1 ≤ i ≤ n − 1, are ni copies of −(−1)n−i [τ i ]τ n−i , and (c) contributes −[τ n ].



Exercise 31 (p. 90)


The partitions of [3 τ]3 are



and the signed partitions, term by term, and then totalled, are
                       τ 4 − τ 2 [τ] − τ 2 [τ] − τ 2 [τ] + τ[2 τ]2 + [τ]2 + τ[2 τ]2 − [3 τ]3
                               = τ 4 − 3τ 2 [τ] + 2τ[2 τ]2 + [τ]2 − [3 τ]3 .




Chapter 3


Exercise 32 (p. 105)

Write the solution in the form
                               y1 = y0 + a1 hF1 + a2 h2 F2 + 12 a3 h3 F3 + a4 F4
so that y1 = y0 + h f ( 12 (y0 + y1 )) implies
                            a1 hF1 + a2 h2 F2 + 12 a3 h3 F3 + a4 F4

                             = hF1 + 12 a1 h2 F2 + 18 a21 h3 F3 + 12 a2 F4 + O(h3 ).

By comparing coefﬁcients, it follows that a1 = 1, a2 = 12 , a3 = a4 = 14 .



Exercise 33 (p. 105)

                           Y1 = y0               = y0 ,
                          hF1 = h f (Y1 )        hE1 ,
                                                 =
                           Y2 = y0 + 2 hF1 = y0 + 12 hE1 ,
                                     1

                          hF2 = h f (Y2 )        =        hE1   + 14 h2 E2 + 24 h E3 ,
                                                                              1 3

                            y1 = y0 + hF2 = y0 + hE1            + 14 h2 E2 + 24 h E3 ,
                                                                              1 3

giving a result identical with ﬂow h to within O(h3 ).
298                                                                                                          Answers to the exercises

Exercise 34 (p. 105)

Write the output from ﬂow h as y1 and derive the coefﬁcients a1 , a2 , a3 , a4 in the following lines
                                 y1 = y0 + ha1 f + h2 a2 f  f + 12 h3 a3 f  ff + h3 a4 f  f  f + O(h3 ),
                          h f (y1 ) = hf + ha1 f  f + 12 h3 a21 f  ff + h3 a2 f  f  f + O(h3 ),                              (1)
                    h(d / d h)y1 = ha1 f + ha2 f f + 12 h3 a3 f  ff + h3 a4 f  f  f + O(h3 ).
                                                           
                                                                                                                                  (2)


Compare the coefﬁcients in (1) and (2) to ﬁnd a1 = 1, a2 = 12 , a3 = 13 , a4 = 16 . Finally substitute
into (1) to give
                      h f (y1 ) = hf + hf  f + 12 h3 f  ff + h3 12 f  f  f + O(h3 ).



Exercise 35 (p. 117)

Let t = [t 1 t 2 · · · t n ]. Then
                               (ED)(∅) = 0 = |∅|/∅!,
                             (ED)(τ) = 1 = |τ|/τ!,
                                               n                 = n                  =        n
                             (ED)(t) = ∏ E(t i ) = 1                 ∏ ti ! = |t| |t| ∏ ti ! = |t|/t!.
                                              i=1                    i=1                     i=1




Exercise 36 (p. 118)


Differentiate y (4) = f (3) y  y  y  + 3f  y  y  + f  y (3) , to obtain
                                                                    
                y (5) = f (4) y  y  y  y  + 3f (3) y  y  y 
                                                                                                               
                            + 3 f (3) y  y  y  + f  y  y  + f  y  y (3) + f  y  y (3) + f  y (4)
                          = f (4) y  y  y  y  + 6f (3) y  y  y  + 4f  y  y (3) + 3f  y  y  + f  y (4) .



Exercise 37 (p. 142)


                            λ (a, t6 ) = a1 (a2 t1 + a1 t2 + t4 ) + (a2 t1 + a1 t2 + t4 ) ∗ (t1 )
                                        = a1 a2 t1 + a21 t2 + a1 t4 + a2 t2 + a1 t3 + t6
                                        = a1 a2 t1 + (a21 + a2 )t2 + a1 t4 + a1 t3 + t6 .



Exercise 38 (p. 142)


                                 λ (a, t6 ) = a2 (a1 t1 + t2 ) + (a1 t1 + t2 ) ∗ (a1 t1 + t2 )
                                             = a1 a2 t1 + a2 t2 + a21 t2 + a1 t3 + a1 t4 + t6
                                             = a1 a2 t1 + (a21 + a2 )t2 + a1 t4 + a1 t3 + t6 .
Answers to the exercises                                                                                         299

Chapter 4

Exercise 39 (p. 155)

                                  ξ                                                          ξ      1 2
                 ϕξ (τ) =             dξ,           = ξ,                  ϕξ ([τ]) =             ξ dξ,ξ ,    =
                              0                                                          0          2
                                  ξ         1 3                                      ξ 1            1
              ϕξ ([τ 2 ]) =           ξ2 dξ,  ξ ,   =                  ϕξ ([[τ]] =       ξ 2 d ξ , = ξ 3,
                              0             3                                       0 2             6
                               ξ            1                                        ξ 1            1
              ϕξ ([τ 3 ]) =      ξ 3 d ξ , = ξ 4,                    ϕξ ([τ[τ]]) =       ξ 3 d ξ , = ξ 4,
                             0              4                                       0 2             8
                               ξ 1           1 4                                     ξ 1             1 4
             ϕξ ([[τ 2 ]]) =       ξ3 dξ, =    ξ ,                   ϕξ ([[[τ]]]) =      ξ3 dξ, =      ξ .
                             0 2            12                                      0 6             24
To ﬁnd the Φ(t), substitute ξ = 1. The results are Φ(τ) = 1, Φ([τ]) = 12 , Φ([τ 2 ]) = 13 ,
Φ([[τ]]) = 16 , Φ([τ 3 ]) = 14 , Φ([τ[τ]]) = 18 , Φ([[τ 2 ]]) = 12
                                                                1
                                                                   , Φ([[[τ]]]) = 24
                                                                                  1
                                                                                     .


Exercise 40 (p. 155)

                                          1                                                      11
                  ϕξ (τ) = ξ                  dξ,    = ξ,                 ϕξ ([τ]) = ξ               ξ dξ,
                                                                                                    ξ,       =
                                      0                                                      0    2
                                          1 1                                          1 1        1
                ϕξ ([τ 2 ]) = ξ               ξ,
                                              ξ2 dξ, =                 ϕξ ([[τ]] = ξ       ξ dξ, = ξ,
                                      0     3                                         0 2         4
                                 1          1                                          1 1        1
               ϕξ ([τ 3 ]) = ξ     ξ3 dξ, = ξ,                       ϕξ ([τ[τ]]) = ξ       ξ dξ, = ξ,
                                                                                             2
                               0            4                                         0 2         6
                                 1 1        1                                          1 1        1
              ϕξ ([[τ ]]) = ξ
                     2
                                     ξ dξ, = ξ,                      ϕξ ([[[τ]]]) = ξ      ξ dξ, = ξ.
                               0 2          4                                         0 4         8
To ﬁnd the Φ(t), substitute ξ = 1. The results are Φ(τ) = 1, Φ([τ]) = 12 , Φ([τ 2 ]) = 13 ,
Φ([[τ]]) = 14 , Φ([τ 3 ]) = 14 , Φ([τ[τ]]) = 16 , Φ([[τ 2 ]]) = 14 , Φ([[[τ]]]) = 18 .


Exercise 41 (p. 158)

It is observed that the stages can be reducd using P1 = {1, 4}. P2 = {2, 3}, giving the tableau
                                                         1       1
                                                         2       2        0
                                                         2       1        1    .
                                                         3       3        3

                                                                 1        0
Only the ﬁrst reduced stage is essential, and we get the ﬁnal result
                                                             1        1
                                                             2        2
                                                                           .
                                                                     1


Exercise 42 (p. 169)
The given set is a subgroup because
                                              
     α1 12 α 2 α3 12 α3       β1 12 β 2 β3 12 β3
                                                                                         
          = α1 + β1 21 (α1 + β1 )2 α1 β1 (α1 + β1 ) + α3 + β3 12 α1 β1 (α1 + β1 ) + α3 + β3 .
300                                                                                      Answers to the exercises

Exercise 43 (p. 169)

The H4 is a subgroup because (ab)1 = a1 + b1 , (ab)2 = a2 + a1 b1 + b2 = (a1 + b1 )2 = (ab)21 . To
be a normal subgroup, x must exist such that xa = ab. This is solved by writing x1 = b1 , x2 = b2 ,
with xi , i = 3, 4, . . . , found recursively.



Chapter 5

Exercise 44 (p. 188)

Expand (I − zA)−1 as a geometric series noting that As = 0. This gives
1 + ∑sn=1 bT An−1 = 1 + ∑sn=1 Φ([n 1]n )zn .



Exercise 45 (p. 188)

Since p = s, Φ([n 1]n ) = 1/[n 1]n ! and it is only ncessary to verify by induction that [n 1]n ! = n!.



Exercise 46 (p. 189)

Use (5.3 f).
                                                        >                       ?
                                                                 1 + 38 z 38 z
                                                det
                    det(I + z(1bT − A))                             0      1                1 + 38 z
           R(z) =                       =       ⎛⎡                                ⎤⎞ =                      .
                        det(I − zA)                         1 − 24
                                                                 7
                                                                   z           1         1 − 58 z + 18 z2
                                                                               24 z ⎦⎠
                                            det ⎝⎣
                                                             − 23 z          1 − 13 z



Exercise 47 (p. 191)

                                                0
                                                1
                                                2
                                                        1
                                                        2            .
                                                        0        1



Exercise 48 (p. 192)

                                            0
                                            2       2
                                            3       3
                                            2       1        1           .
                                            3       3        3
                                                    1                3
                                                    4        0       4
Answers to the exercises                                                                                    301

Exercise 49 (p. 194)

                                         0
                                         1         1
                                         3         3
                                         3
                                         4   − 21
                                               32
                                                         45
                                                         32                    .
                                         1         7
                                                   3   − 12
                                                          5
                                                                16
                                                                15
                                                   1     9      16         1
                                                   9     20     45        12



Exercise 50 (p. 203)
                                 √                                       √                √
                       2 − 10                                 9 − 30               36 − 20
                       1    1                  5              2    1                5   1
                                  15          36                          15               15
                                               √                                          √
                                                                                   36 − 24 15
                             1           5   1                       2              5   1
                             2          36 + 24 15                   9
                                 √             √                         √                      .
                       1    1            5   1                2    1                    5
                       2 + 10     15    36 + 20 15            9 + 30      15           36
                                               5                     4                  5
                                              18                     9                 18



Exercise 51 (p. 203)

Pn (1) = 1, for all n. Therefore, Ps (1) − Ps−1 (1) = 0, for s ≥ 1.


Exercise 52 (p. 203)
                                                            √
The zeros of P3 − P2 = 20x3 − 36x2 + 18x − 2 are 25 ∓ 10
                                                         1
                                                              6 and 1. Solve linear equations for A and
 T
b . The ﬁnal tableau is
                           √                √                  √                 √
                    5 − 10 6      45 − 360 6      225 − 1800 6       − 225
                    2    1        11     7         37      169           2     1
                                                                           + 75    6
                           √                 √                √                  √
                    2    1
                    5 + 10 6
                                  37     169
                                 225 + 1800 6
                                                    11      7
                                                    45 + 360 6       − 225
                                                                         2
                                                                           − 751
                                                                                   6
                                           √                  √                       .
                                    9 − 36 6
                                    4    1           4     1                1
                        1                            9 + 36 6               9
                                           √                  √
                                    9 − 36 6
                                    4    1           4     1                1
                                                     9 + 36 6               9



Exercise 53 (p. 208)

From the equations in (5.7 c), it follows that ∑i j bi (1 − ci )ai j c j (c j − c3 ) = 60
                                                                                        1
                                                                                          − 24
                                                                                             1
                                                                                               c3 . Since the
                             2
left-hand side is zero, c3 = 5 .



Chapter 6

Exercise 54 (p. 216)

In each case, z is in the stability region if the difference equation (1 − b0 z)yk = ∑ki=1 (ai + bi z)yk−i
has only bounded solutions.
302                                                                                     Answers to the exercises

Exercise 55 (p. 218)

The characteristic polynomial of M is found to be w(w − 1)(w − 240μ+361
                                                                      121    ). The zeros of this
polynomial are 0, 1, w , where w = 240μ+361
                                        121   , which satisﬁes |w | < 1 for μ ∈ (− 241 , −1).
                                                                                     120


Exercise 56 (p. 229)
             
       1 −1
T=              .
       0 1


Exercise 57 (p. 235)

(θ5 − (c2 + c4 )θ3 + c2 c4 θ2 )θ8 − (θ6 − c4 θ4 )(θ7 − c4 θ2 ) = 0 simpliﬁes to c2 (1 + 2c4 ) = 0.



Chapter 7

Exercise 58 (p. 269)

                           0
                        − 13     − 13
                        − 13      896237
                                  950913     − 1213208
                                               950913
                                                                                              .
                        − 23 − 15257
                               23193               0          − 23193
                                                                 205


                        −1 − 4736
                             3591                  0               0          1145
                                                                              3591

                           0 − 12800
                                4537
                                             15529062400 − 3105812480 − 12800
                                             17759035623    89068851     7731          1197
                                                                                      12800



Exercise 59 (p. 269)
The matrix
                                                       ⎡                                              ⎤
          bT (c − c4 )                                       bT (c − c4 )(c − c2 )c   bT (c − c4 )Ac
                                 (c − c2 )c Ac         =⎣                                               ⎦
                 bT A                                           bT A(c − c2 )c           bT A2 c
                                                         ⎡                                              ⎤
                                                             bT (c − c4 )(c − c2 )c   bT (c − c4 )Ac
                                                       =⎣                                               ⎦
                                                                bT A(c − c2 )c           bT A2 c
                                                         ⎡                                                  ⎤
                                                             ζ5 − (c2 + c4 )ζ3 + c2 c4 ζ 2    ζ6 − c4 ζ4
                                                       =⎣                                                   ⎦
                                                                        ζ7 − c2 ζ4                 ζ8
has rank 1 and its determinant is zero. This simpliﬁes to
                       2574900c2 c4 + 1453965c2 + 967440c4 + 688748 = 0.
Substitute c2 = 14305 , with the result c4 = − 10331
                15924
                                               17166 .


Exercise 60 (p. 282)

Because t13 = [t22 ], the result is H  f  ff  f.
