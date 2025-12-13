# B-Series: Algebraic Analysis of Numerical Methods - Part 3

**Author:** John C. Butcher

**Series:** Springer Series in Computational Mathematics, Volume 55

---

Chapter 2
Trees and forests




2.1 Introduction to trees, graphs and forests

Trees, series and numerical methods

It was pointed out by A. Cayley in 1857 [28] that trees, in the sense of this chapter,
have an intimate link with the action of differential operators on operands which, in
their turn, might also have something of the nature of operators. This is now extended
to numerical methods through the order conditions and the use of B-series. As we
saw in Chapter 1, the form of B-series, which provides the link between differential
equations and numerical approximations, is given by (1.1 c) (p. 2).
   The present chapter aims to present a detailed background to the graph-theoretical
and combinatorial aspects of this subject, for use in Chapter 3 and later chapters.
In this introductory section, trees and forests will be introduced together with an
appreciation of Cayley’s fundamental work.


Graphs and trees

A graph in mathematics is a set of points (vertices) and a set of connections (edges)
between some of the pairs of vertices. It is convenient to name or label each of the
vertices and use their names in specifying the edges. If V is the set of vertices and E
the set of edges, then the graph is referred to as (V, E).


Basic terminology and observations

   A path is a sequence of vertices in which each successive pair is an edge.
   A graph is connected if there is a path from any vertex to any other vertex.
   A loop is a path from a vertex to itself, in which the other vertices comprising the
    path are distinct.

© Springer Nature Switzerland AG 2021                                               39
J. C. Butcher, B-Series, Springer Series in Computational Mathematics 55,
https://doi.org/10.1007/978-3-030-70956-3_2
40                                                                                2 Trees and forests

     The order of a graph is the number of vertices.
     A tree is a connected graph with at least one vertex and with no loops.
     For a tree, the number of edges is one less than the number of vertices.
     The “empty tree” ∅, with V = E = 0,  / is somtimes included, as an additional tree.
     The set of trees with a positive number of vertices will be denoted by T and the
      set of trees, with ∅ included by T# . That is, T # = T ∪ {∅}.
     The set of trees t with a positive number of vertices, not exceeding p, will be
      denoted by T p .



Examples of graphs

In these examples, the standard notation, V for the set of vertices and E for the set of
edges, is used.

         b
                                                                          
                      V = {a, b, c}             E = {a, b}, {b, c}, {c, a}                     (2.1 a)
     a        c
         3
     2    0 4                                                                    
                      V = {0, 1, 2, 3, 4}       E = {0, 1}, {0, 2}, {0, 3}, {0, 4}             (2.1 b)
          1
     α β          γ                                       
                      V = {α, β , γ}            E = {β , γ}                                    (2.1 c)

   The graph (2.1 a) is connected but contains a loop; hence it is not a tree. In contrast,
(2.1 b) is connected and has no loops and is therefore a tree. Note that there are 5
vertices and 4 edges. The ﬁnal example (2.1 c) has no loops but it is not connected
and is therefore not a tree.

Exercise 19 For a graph with a positive number of vertices, show that any two of the following
three statements imply the third: (i) the graph is connected, (ii) there are no loops, (iii) the number
of vertices is one more than the number of edges.


Rooted and unrooted trees

In using trees in mathematics, it is sometimes natural to treat all vertices in an even-
handed way. To specify the structure of such a tree, the sets V and E are all that is
needed. However, it is often more useful to specify a particular vertex as constituting
the “root” r of the tree. That is, a triple of the form (V, E, r) is needed to fully specify
a “rooted tree”. Throughout this volume, the single word “tree” will mean such a
rooted tree. In contrast, a tree where no root is speciﬁed, is called a “free tree” or an
“unrooted tree”. Throughout this volume, “free tree” and “unrooted tree” will be used
interchangeably. The set of free trees will be denoted by U.
2.1 Introduction to trees, graphs and forests                                          41

   There is a good reason for studying trees, both rooted and unrooted. Trees play
a central role in the formulation of order conditions for Runge–Kutta and other
numerical methods. In particular, elementary differentials, which are the building
blocks of B-series, are indexed on the set of rooted trees. Furthermore, unrooted
trees emerge as fundamental concepts in the theory of symplectic methods and their
generalizations.


Trees and forests

A forest, as a collection of trees, possibly containing duplications, is a natural
extension of the idea of trees. We will always include the empty forest in the set of
forests and, for a non-empty forest, no account will be taken of the order in which the
constituent trees are listed. Algebraically, the set of forests is a monoid (semi-group
with identity) and is related to the set of trees via the B+ operation. The identity, that
is the empty set of trees, is denoted by 1.


Terminology

   A typical tree, such as t, t , t 1 , t2 , . . . , is a member of T.
   A typical unrooted tree, such as u, is a member of the set U.
   A typical forest, such as f, is a member of the set of all forests F.
   A weighted sum of trees, such as T, is a member of the tree space T.
   A weighted sum of forests, such as F, is a member of the forest space F.



Some examples

Members of F
(a)         1

(b)
(c)
Members of T
(a)         2       −

(b)             + 2     +3
(c)         5
Members of F
(a)             +       +4
(b)         1+ +             +
(c)         5
42                                                                                           2 Trees and forests

Diagrammatic representations

In this book we will invariably use diagrams with the root at the lowest point and
other vertices rising upwards. To represent free trees, we will use diagrams with no
special point empahasized in any way.
Examples:


           T:
           U:


Cayley, trees and differential equations

The pioneering work of Cayley [28] (1857) directly connected trees with differential
operators. In this discussion we adapt these ideas to the ordinary differential equation
context. Consider the system

                      d yi
                           = f i = f i (y1 , y2 , . . . , yN ),          i = 1, 2, . . . , N.           (2.1 d)
                      dx
Using elementary calculus and the summation convention,

                                                 d2 yi
                                                       = f ji f j ,                                     (2.1 e)
                                                 d x2

where subscript on f ji denotes partial differentiation (∂ y j )−1 .
  Take this further

             d3 yi
                   = f jk f f + f ji fkj f k ,
                       i j k
                                                                                                         (2.1 f)
             d x3
             d4 yi     i
                   = f jk f j f k f  + 3 f jk
                                             i j k 
                                                f f f + f ji fkj f k f  + f ji fkj fk f  .         (2.1 g)
             d x4
                                                   i , . . . into analogous statements about
   If we translate expressions like f i , f ji , f jk
parent-child relationships involving people whose names are i, j, k, . . . , we see that
the terms occurring in (2.1 d), (2.1 e), (2.1 f), (2.1 g) translate to kinship statements,
such as
         f i translates to
               “i has no children”,
         f ji f j translates to
                  “i has one child named j, who has no children”,
       i
     f jk f j f k translates to
                  “i has two children named j and k”, each of whom has no children,
2.1 Introduction to trees, graphs and forests                                                 43

    f ji fkj f k translates to
                “i has a child j who has a child k” who has no children.

We can write these “kinship statements” as labelled trees shown, respectively, in the
following diagrams
                                                                 k
                                 j          j      k             j
                        i               i                         i                   i


Subtrees, supertrees and prunings

For the two given trees,

                                       t =      ,           t=       ,                   (2.1 h)
t is a subtree of t and t is a supertree of t , because t can be constructed by deleting
some of the vertices and edges from t or, alternatively, t can be constructed from t 
by adding additional vertices and edges. The pruning associated with a tree and a
possible supertree is the member of the forest space, written as t t , formed from
the collection of possible ways t can be “pruned” to form t . For example, for the
trees given by (2.1 h),
                                        t t =        .
In two further examples,


                            t =   ,        t=       ,        t t = 2 ,                  (2.1 i)

                            t =   ,        t=           ,    t t =      3
                                                                              +   ,       (2.1 j)

where the factor 2 in t t (2.1 i) is a consequence of the fact that t could equally
well have been replaced by its mirror image. Note that t t in (2.1 j) has two terms
indicating that the pruning can be carried out in two different ways.


Chapter outline

Elementary properties of graphs, trees and forests are discussed in Sections 2.2 – 2.4.
Functions of trees are introduced in Section 2.5 and partitions and evolution of trees
in Section 2.6. The generalization known as the “stump” of a tree is introduced in
Section 2.7.
   Prunings of trees, and subtrees and supertrees, to be introduced in Section 2.8, are
essential precursors to the composition rule to be introduced in Chapter 3. This is
followed in 2.9 by a consideration of antipodes of trees and forests.
44                                                                                     2 Trees and forests

2.2 Rooted trees and unrooted (free) trees

To illustrate the relationship between rooted and unrooted trees, consider u, given by
(2.1 b) (p. 40) and reproduced in (2.2 a). If u = (V, E) and r ∈ V , the triple (V, E, r)
is a rooted tree. In this case, if r = 0 we obtain t and if r = 1 we obtain t . Note that,
because of symmetry, r = 2, 3, 4 we also obtain t  .
                   3
                                                                                        
        u=2            0 4     (V, E)   = {0, 1, 2, 3, 4}, {0, 1}, {0, 2}, {0, 3}, {0, 4}

                       1
               1 2 3 4                                                                   
        t=                     (V, E, r) = {0, 1, 2, 3, 4}, {0, 1}, {0, 2}, {0, 3}, {0, 4} , 0     (2.2 a)
                   0
               2   3       4
                                                                                         
        t =                   (V, E, r) = {0, 1, 2, 3, 4}, {0, 1}, {0, 2}, {0, 3}, {0, 4} , 1
                       0
                       1

   Even though free trees, that is, trees without a root speciﬁed, have been introduced
as a basic structure, we will now consider rooted trees as being the more fundamental
constructs, and we will deﬁne unrooted trees in terms of rooted trees.
   However, we will ﬁrst introduce recursions to generate trees from the single tree
with exactly one vertex.


Building blocks for trees

Let τ denote the unique tree with only a single vertex. We will show how to write an
arbitrary t ∈ T in terms of τ using additional operations.

 Deﬁnition 2.2A The order of the tree t, denoted by |t|, is the cardinality of the
 set of vertices. That is, if t = {V, E, r}, then |t| = card(V ).




Using the B+ operation
If trees t1 , t 2 , . . . , t m are given, then
                                              t = [t 1 t 2 . . . t m ],                           (2.2 b)
also written B+ (t1 , t 2 , . . . , t m ) because of the connection with Hopf algebras, will
denote a tree formed by introducing a new vertex, which becomes the root of t, and
m new edges from the root of t to each of the roots of ti , i = 1, 2, . . . , m. Expressed
diagrammatically, we have
2.2 Rooted trees and unrooted (free) trees                                                         45



                                        t1      t2              tm
                               t=

If a particular tree is repeated, this is shown using a power notation. For example,
[t31 t22 ] is identical to [t 1 t 1 t1 t 2 t2 ]. Similarly subscripts on a pair of brackets [m · · · ]m
will indicate repetition of this bracket pair. For example, [2 t1 t2 t 3 · · · ]2 has the same
meaning as [[t1 t 2 t3 · · · ]].
B+ induction
Many results concerning trees can be proved by an induction principle which involves
showing the result is true for t = τ and for t = [f], where the forest f is assumed to
consist of trees with order less than |t|. Thus, effectively, B+ induction is induction
on the value of |t|.
Balanced parentheses
By adjusting the terminology slightly we see that trees can be identiﬁed with balanced
sequences of parentheses and forests by sequences of balanced parentheses sequences.
To convert from the B+ terminology it is only necessary to replace each occurrence of
τ by () and to replace each matching bracket pair [·] by the corresponding parenthesis
pair (·).
   For example,
                                       τ → ();
                                     [τ] → (());
                                             [τ 2 ] → (()());
                                          [2 τ]2 → ((()));
In the BNF notation, widely used in the theory of formal languages [75] (Naur, 1963),
we can write
                           <tree> ::= (<forest>)
                         <forest> ::=    | <forest> <tree>

Using the beta-product
A second method of forming new trees from existing trees is to use the unsymmet-
rical product introduced in [14]. Given t 1 and t2 , the product t 1 ∗ t 2 is formed by
introducing a new edge between the two roots and deﬁning the root of the product to
be the same as the root of t1 . Hence, if t = t 1 ∗ t2 then we have the diagram


                                     t=                    t2
                                                 t1
This operation will be used frequently throughout this book and will be given the
distinctive name of “beta-product”.
46                                                                             2 Trees and forests

Using iterated beta-products

Because the beta product is not associative, it has been customary to use parentheses
to resolve ambiguities of precedence. For example, (τ ∗ τ) ∗ τ gives and τ ∗ (τ ∗ τ)
gives . Ambiguity can also be avoided by the use of formal powers of the symbol ∗ so
that t1 ∗2 t2 t3 = t 1 ∗ ∗t 2 t3 denotes (t1 ∗ t2 ) ∗ t 3 , whereas t1 ∗ t2 ∗ t3 will conventionally
denote t1 ∗ (t2 ∗ t3 ).



           Table 1 Trees to order 4 with B+ notation, beta products and Polish subscripts

     |t|             t             B+                                 beta          Polish

     1                              τ                                  τ              0

     2                             [τ]                            τ ∗τ               10

     3                            [τ 2 ]                         τ ∗2 τ 2            200

     3                           [2 τ]2                         τ ∗τ ∗τ              110

     4                            [τ 3 ]                         τ ∗3 τ 3           3000

     4                           [τ[τ]]            τ ∗2 ττ ∗ τ = τ ∗2 τ ∗ ττ    2010 = 2100

     4                           [2 τ 2 ]2                    τ ∗ τ ∗2 τ 2          1200

     4                           [3 τ]3                      τ ∗τ ∗τ ∗τ             1110




Using a Polish operator

Finally we propose a “Polish” type of tree construction. Denote by τn the operation,
of forming the tree [t 1 t2 · · · t n ], when acting on the sequence of operands t1 , t 2 , . . . ,
tn . This operation is written in Polish notation as

                                             τn t 1 t 2 · · · t n .

From [τ] = τ1 τ we can for example write

                                   [τ[τ]] = τ2 ττ1 τ = τ2 τ0 τ1 τ0 ,

where τ0 is identiﬁed with τ. For compactness, this tree can be designated just by
the subscripts; in this case 2010, or because the operation τ2 is symmetric, as 2100.
These Polish codes have been added to Table 1.
2.2 Rooted trees and unrooted (free) trees                                                                                               47

  In the spirit of Polish notation, it is also convenient to introduce a preﬁx operator
β which acts on a pair of trees to produce the beta-product of these trees. That is

                                                       β t1 t2 := t 1 ∗ t2 .

Examples to order 4 are
                                                                               τ = τ,
                                                                          β ττ = [τ],
                                                                     β β τττ = [τ 2 ],
                                                                     β τβ ττ = [2 τ]2 ,
                                                               β β β ττττ = [τ 3 ],
                                        β β τβ τττ = β β ττβ ττ = [τ[τ]],
                                                               β τβ β τττ = [2 τ 2 ]2 ,
                                                               β τβ τβ ττ = [3 τ]3 .

Other uses of Polish notation are introduced in Chapter 3, Section 3.3 (p. 106).

Exercise 20 Write the following tree in a variety of notations such as (i) iterated use of the B+
operation, (ii) iterated use of the beta-product, (iii) Polish notation using a sequence of factors τ, τ1 ,
τ2 , . . .

                                              t=




An inﬁx iterated beta operator
It is sometimes convenient to replace ∗n t1 t2 · · · t n by t1 t 2 · · · t n . The operator 
operates on whatever trees lie to its right, and parentheses are typically needed to
indicate a departure from strict Polish form. For example,

     (τm  t1 t 2 · · · t n )t 1 t2 · · · t m = τm+n t1 t2 · · · t n t1 t 2 · · · t m = [t 1 t2 · · · t n t1 t 2 · · · t m ].

                                       2
Exercise 21 Write the tree τ2  τ3  τ 3 in B+ and beta-prodict forms.




Equivalence classes of trees

If t1 = t  ∗ t , t2 = t ∗ t for some t  , t  then we write t 1 ∼ t 2 . We extend this to an
equivalence relation by deﬁning t 1 ≈ t n if there exist t 2 , . . . , t n−1 such that

                                                t 1 ∼ t 2 ∼ t 3 · · · t n−1 ∼ t n .
48                                                                      2 Trees and forests



 Deﬁnition 2.2B An unrooted tree u is an equivalence class of T. The set U is the
 partition of T into unrooted trees.


As an example, we show how four speciﬁc trees, with order 6, are connected using
the ∼ relation.

                          =        ∗       ∼         ∗       =
                          =        ∗       ∼         ∗       =

                          =         ∗      ∼        ∗        =
Hence,
                                ≈           ≈            ≈
This deﬁnes the corresponding unrooted tree in terms of its equivalence class
                                                            
                          =          ,        ,      ,         .



S-trees and N-trees

In the theory of symplectic integration, as in Chapter 7 (p. 247), unrooted trees have
an important role [84]. In this theory a distinction has to be made between two types
of trees.

 Deﬁnition 2.2C A tree t with the property that t ∼ t ∗ t , for some t , is said to
 be an S-tree. An equivalence class of S-trees is also referred to as an S-tree. An
 N-tree is a rooted or unrooted tree which is not an S-tree.

The terminology “S-tree” is based on their superﬂuous role in the order theory
for symplectic integrators. Hence, following the terminology in [84], S-trees will
also be referred to as “superﬂuous trees”. Similarly N-trees are referred to as “non-
superﬂuous trees.”


Unrooted trees to order 5
For orders 1 and 2 there is only a single tree and hence, the equivalence classes
contain only these single members. For order 3, the two trees are similar to each other
and constitute the only similarity class of this order. For order 4, the 4 trees break into
two similarity classes and, for order 5, they break into 3 classes. We will adopt the
2.2 Rooted trees and unrooted (free) trees                                                     49

convention of drawing unrooted trees with all vertices spread out at approximately
the same level so that there is no obvious root.
   The unrooted trees, separated into S-trees and N-trees, together with the corre-
sponding rooted trees, up to order 5, are shown in Table 2.


               Table 2 Rooted and unrooted trees (S-trees and N-trees) to order 5

                 U
                                                                      T
           S
         U             UN




Exercise 22 For the following unrooted tree, ﬁnd the four corresponding trees, in the style of
Table 2. Show how these trees are connected by a sequence of ∼ links between the four rooted trees
which make up the unrooted tree




Exercise 23 As for Exercise 22, ﬁnd the ﬁve corresponding trees for the following unrooted tree,
in the style of Table 2, and show the ∼ connections
50                                                                                     2 Trees and forests

Classifying trees by partitions

For a tree t = [t 1 , t 2 , . . . , t m ], with |t| = p, the corresponding partition of t is the
partition of p − 1 given by

                               p − 1 = |t 1 | + |t2 | + · · · + |tm |.

The number of components in this partition, that is, the value of m, will be referred
to as the “order of the partition”.
    Given the value of p, the set of all partitions of p − 1 gives a convenient classiﬁca-
tion of the trees of the given order. For example, to identify the trees of order 5, and
to list them in a systematic manner, ﬁrst ﬁnd the partitions of 4:

                    1 + 1 + 1 + 1,        1 + 1 + 2,         1 + 3,       2 + 2,   4

and then write down the required list of trees using this system.
   The result of this listing procedure, up to p = 6, is shown in Table 3.



2.3 Forests and trees

A forest is a juxtaposition of trees. The set of forests F can be deﬁned recursively by
the generating statements
                                       1 ∈ F,
                              t f = f t ∈ F,     t ∈ T, f ∈ F,
                         f 1 f2 = f 2 f1 ∈ F,    f 1 , f 2 ∈ F.

The structure used in (2.2 b) can now be interpreted as a mapping from F to T

                                             f → [f] ∈ T                                          (2.3 a)

with the special case [1] = τ.
   We will sometimes use the terminology B+ for the mapping in (2.3 a) together
with the inverse mapping B− . That is,

                         B+ (f) = [f] ∈ T,               B− ([f]) = f ∈ F.

The order of a forest is an extension of Deﬁnition 2.2A (p. 44):

 Deﬁnition 2.3A The order of f ∈ F is deﬁned by

                                       |1| = 0,
                           |t 1 t2 · · · t m | = |t 1 | + |t2 | + · · · + |tm |.
2.3 Forests and trees                                                          51


                        Table 3 Trees up to order 6, classiﬁed by partitions

order       partition                                        trees

  2            1

  3          1+1

               2

  4        1+1+1

             1+2


               3

  5      1+1+1+1

           1+1+2


             1+3

             2+2



               4

  6     1+1+1+1+1

         1+1+1+2


           1+1+3

           1+2+2



             1+4


             2+3




               5
52                                                                         2 Trees and forests



 Theorem 2.3B Amongst elementary consequences of Deﬁnitions 2.2A and 2.3A,
 we have

                                        |t f| = |t| + |f|,
                                     |f1 f2 | = |f 1 | + |f2 |,
                                   |t 1 ∗ t2 | = |t 1 | + |t2 |,
                                   |B+ (f)| = 1 + |f|.


   In contrast to the width of a forest, we will also use the “span” of a forest,
according to the recursive deﬁnition:

 Deﬁnition 2.3C The span of a forest f is deﬁned by

                               1 = 0,
                              ft = f + 1,         f ∈ F,          t ∈ T.




Miscellaneous tree terminology and metrics

The root of a tree t will be denoted by root(t). Every other vertex is a member of the
set nonroot(t). Because a tree is connected and has no loops, there is a unique path
from root(t) to any v ∈ nonroot(t). The number of links in this path will be referred
to as the “height” of v and the height of a tree is the maximum of the heights of its
vertices. If the last link is the edge {v , v} then v is the “parent” of v and v is a “child”
of v . If v has no children it is a “leaf” of t. If v is present in the path from the root
to v then v is an “ancestor” of v and v is a “descendant” of v . The “dependants” of
a vertex v are its descendants together with v itself. Every vertex, except the root, has
a unique parent. The “width” of a tree is deﬁned recursively by

                                      width(τ) = 1,
                                                          m
                           width([t 1 t2 · · · t m ]) = ∑ width(t i ).
                                                          i=1

For the tree

                                                              f
                                                  d           e
                                      t=                  c
                                              b
                                                      a

the various relationships are given for the 6 vertices a, b, . . . , f as follows
2.4 Tree and forest spaces                                                                            53

          v   height(v) child(v) parent(v) descendant(v) ancestor(v)
          a           0       {b, c}              0/           {b, c, d, e, f }              0/
          b           1            0/             a                    0/                   {a}
          c           1       {d, e}              a                {d, e, f }               {a}
          d           2            0/             c                    0/                  {a, c}
          e           2        {f}                c                  {f}                   {a, c}
          f           3            0/             e                    0/                 {a, c, e}

For this tree, root(t) = a, nonroot(t) = {b, c, d, e, f }, height(t) = width(t) = 3.



2.4 Tree and forest spaces

The tree space

A formal linear combination of trees, including ∅, constitutes a member of the “tree
space”. The sum, and multiplication by a scalar, are deﬁned by
                                                                       
       a0 ∅ + ∑ a(t)t + b0 ∅ + ∑ b(t)t = (a0 + b0 )∅ + ∑ a(t) + b(t) t,
               t∈ T                        t∈ T                                    t∈ T
                                                      
                             c a0 ∅ + ∑ a(t)t = ca0 ∅ + ∑ ca(t)t.
                                           t∈ T                             t∈ T

The forest space

A formal linear combination of forests will constitute a member of the “forest space”
F. That is, F ∈ F, for an expression of the form

                                  F=    ∑ a(f)f,           a : F → R.
                                        f ∈F
If φ : R → R is given, with a Taylor expansion of the form
                                  φ (x) = a0 + a1 x + a2 x2 + · · · ,
then for a speciﬁc t ∈ T, F = φ (t) ∈ F is deﬁned by

                                   F = a0 1 + a1 t + a2 t
                                                               2
                                                                   +··· .
For example,
                                                           ∞
                                         (1 − t)−1 = ∑ tn .
                                                           n=0
54                                                                      2 Trees and forests

 Theorem 2.4A The sum of all forests is given by the formal product

                                  ∏ (1 − t)−1 = ∑ f.                              (2.4 a)
                                  t∈ T             f ∈F

The forest space as a ring
A ring is one of the fundamental algebraic structures. A unitary commutative ring is
a set R with two binary operations, known as addition and multiplication, with the
following properties

1. (R, +) is an abelian group with identity 0.
2. (R, ·) is associative and commutative with identity 1. That is, it is a commutative
   semi-group with identity (or a commutative monoid).
3. The left-distributive rule holds: x(y + z) = xy + xz

Without providing details, we assert that the forest space F is a unitary commutative
ring.


Enumeration of trees and free trees

Introduce the three generating functions

                               A (x) = a1 x + a2 x2 + · · · ,
                               B(x) = b1 x + b2 x2 + · · · ,                       (2.4 b)
                               C (x) = c1 x + c2 x + · · · ,
                                                   2


where, for each n = 1, 2, . . . ,
an is the number of trees with order n,
bn is the number of unrooted (free) trees with order n,
cn is the number of non-superﬂuous unrooted trees with order n.


Low order terms in A , B and C
By inspection of Table 2 (p. 49), we see that

              a1 = 1,      a2 = 1,       a3 = 2,          a4 = 4,    a5 = 9,
              b1 = 1,      b2 = 1,       b3 = 1,          b4 = 2,    b5 = 3,
              c1 = 1,       c2 = 0,      c3 = 1,          c4 = 1,    c5 = 3.
Hence we can insert the coefﬁcients in (2.4 b):
                        A (x) = x + x2 + 2x3 + 4x4 + 9x5 + · · · ,
                        B(x) = x + x2 + x3 + 2x4 + 3x5 + · · · ,
                        C (x) = x + x3 + x4 + 3x5 + · · · .
2.4 Tree and forest spaces                                                          55

Let ξ be the surjection from F to the space of power series in some indeterminant
x such that ξ (t) → x|t| , for any tree t. For example, ξ (1 − t) = 1 − x|t| . ξ is a
homomorphism because, in additional to the required vector space properties,
                                                            
                    ξ (ff  ) = x|ff | = x|f|+|f | = x|f|| x|f | = ξ (f)ξ (f  ).


 Theorem 2.4B The coefﬁcients in A (x) are given by
                   a1 + a2 x + a3 x2 + · · · = (1 − x)−a1 (1 − x2 )−a2 . . .

Proof. Apply the operation ξ to each side of (2.4 a). The left-hand side maps to
            n −an and, because the number of trees of order n is the same as the
∏∞n=1 (1 − x )
number of forests of order n − 1 (note the equivalence [f] ↔ t), the right-hand side of
(2.4 a) maps to ∑∞        n−1 .
                 n=1 an x


   Picking out the coefﬁcients of the ﬁrst few powers of x we obtain in turn
                      a1 = 1,
                      a2 = a1 = 1,
                      a3 = 12 a1 (a1 + 1) + a2 = 2,
                      a4 = 16 a1 (a1 + 1)(a1 + 2) + a1 a2 + a3 s = 4.

The conclusions of Theorem 2.4B are used in Algorithm 1.



                    Algorithm 1 Find the number of trees of each order

Input:      p max
Output: Ntrees[1 : p max]
    %
    %      Calculate the number of trees of each order 1 : p max and place
    %      the results in Ntrees, using Theorem 2.4B
    %
  1 for i from 1 to p max do
  2    Ntrees[i] ← 1
  3 end for
  4 for i from 2 to p max − 1 do
  5    for j from p max step − 1 to 1 do
  6        a ← Ntrees[i]
  7        for k from 1 to ﬂoor(( j − 1)/i) do
  8           Ntrees[ j] ← Ntrees[ j] + a ∗ Ntrees[ j − i ∗ k]
  9           a ← a ∗ (Ntrees[i] + k)/(k + 1)
 10        end for
 11    end for
 12 end for
56                                                                                      2 Trees and forests

    Let an = ∑ni=1 ai denote the number of trees with order not exceeding n. Values
of an and an up to n = 10 are given below using a Julia realization of Algorithm 1
(p. 55). Further information is available in Table 4 (p. 58)

           n    1 2 3 4             5      6         7      8       9     10           11    12
           an   1 1 2 4             9 20 48 115 286                       719 1842 4766
           an   1 2 4 8 17 37 85 200 486 1205 3047 7813

                 denote the subset of T in which no vertex has exactly one descendant. That is,
Exercise 24 Let T
the ﬁrst few members are                                          
                         =
                         T      ,        ,        ,       , ...     .
                                    with order n, show that
If an is the number of members of T
                                                                    ∞
                       1 + x + a3 x2 + a4 x3 + · · · = (1 − x)−1 ∏ (1 − xn )−an .
                                                                    n=3




 Lemma 2.4C
                                     B(x) − C (x) = A (x2 ).

Proof. Because each superﬂuous tree is made up by joining two copies of a rooted
tree, b2n − c2n = an , b2n−1 − c2n−1 = 0.


 Lemma 2.4D
                              B(x) + C (x) = 2A (x) − A (x)2 .

Proof. Let T ∈ T be the sum of all rooted trees and consider
                                               
                                           T       = 2 T − T ∗ T.

For any u ∈ UN , write the corresponding equivalence class of trees as a sequence t1 ,
t2 , . . . , tn , where ti−1 ∼ ti , i = 1, 2, . . . , n. Hence, the total of the coefﬁcients of these
trees is 2n, from the term 2T, minus 2n − 2 from the term −T ∗ T, making a total of 2.
If u ∈ US , then the result is 2n − (2n − 1) = 1. Hence, ξ (T ) = B(x) + C (x), which
equals ξ (2T − T ∗ t)S = 2A (x) − A (x)2 .
   To illustrate the proof of Lemma 2.4D, 2T − T ∗ T is evaluated, with only trees up
to order 5 included. Note that trees of a higher order that appear in the manipulations
are deleted.
   
  2 + + + + + + + + + + + + + + + +
                                                                    
  − + + + + + + + ∗ + + + + + + +
2.4 Tree and forest spaces                                                                 57
                                                                                        
  =       + + +       + + +          +     + +       + + + + +                 +     + +

A ﬁnal result consists of two terms; the ﬁrst has a representative of each equivalence
class, with superﬂuous trees included, and the second term contains a different
representative of each class but with superﬂuous trees omitted.
   Finally the generating functions for members of U and US can be deduced.

 Theorem 2.4E
                             B(x) = A (x) − 12 (A (x)2 − A (x2 ),
                             C (x) = A (x) − 12 (A (x)2 + A (x2 ).


Proof. Use Lemmas 2.4C and 2.4D.

   The calculation of the coefﬁcients in B(x) and C (x) is shown in Algorithm 2.
Results from Algorithms 1 (p. 55) and 2 are presented in Table 4. In this table,
an = Ntrees, bn = Utrees, cn = UNtrees. Accumulated totals are denoted by a, b, c.



    Algorithm 2 Find the number of unrooted and non-superﬂuous unrooted trees of each
    order

Input:      p max and Ntrees from Algorithm 1 (p. 55)
Output: Utrees, UNtrees
    %
    %      Calculate the number of unrooted trees of each order 1 : p max and place
    %      the results in Utrees, and the number of non-superﬂuous
    %      unrooted trees and place these in UNtrees, using the result of Theorem 2.4E
    %
  1 for i from 1 to p max do
  2    Xtrees[i] ← 0
  3    Ytrees[i] ← 0
  4 end for
  5 for i from 1 to p max − 1 do
  6    for j from 1 to p max − i do
  7        Xtrees[i + j] ← Xtrees[i + j] + Ntrees[i] ∗ Ntrees[ j]
  8    end for
  9 end for
 10 for i from 1 to ﬂoor(p max/2) do
 11    Ytrees[2 ∗ i] ← Ntrees[i]
 12 end for
 13 for i from 1 to p max do
 14    Utrees[i] ← Ntrees[i] − (Xtrees[i] − Ytrees[i])/2
 15    UNtrees[i] ← Ntrees[i] − (Xtrees[i] + Ytrees[i])/2
 16 end for
58                                                                                       2 Trees and forests


      Table 4 Tree enumerations for rooted trees, unrooted trees, and non-superﬂuous unrooted
      trees, with accumulated totals.

  n     1 2 3 4        5    6   7    8     9        10            11        12      13       14        15
 an     1 1 2 4        9 20 48 115 286             719       1842      4766      12486   32973      87811
 an     1 2 4 8 17 37 85 200 486 1205                        3047      7813      20299   53272     141083
 bn     1 1 1 2        3    6 11    23    47       106           235       551   1301      3159      7741
 bn     1 2 3 5        8 14 25      48    95       201           436       987   2288      5447     13188
 cn     1 0 1 1        3    4 11    19    47        97           235       531   1301      3111      7741
 cn     1 1 2 3        6 10 21      40    87       184           419       950   2251      5362     13103



2.5 Functions of trees

A number of special functions on the set of trees have applications in Chapter 3.
We will continue the present chapter by introducing these and, at the same time,
establishing a standard ordering for the trees up to order 6.
   For a tree t deﬁned as the graph (V, E, r), consider a permutation π of the members
of V which acts also on the vertex pairs comprising E and on the root r. That is,
v ∈ V maps to πv, an edge {v1 , v2 } maps to {πv1 , πv2 } and r maps to πr. In compact
notation (V, E, r) maps to (πV, πE, πr). Because π is a permutation, πV = V . If also
πE = E and πr = r, then π is said to be an automorphism of t. Deﬁne the group A(t)
as the group of automorphisms of t. Finally, we have the deﬁnition

 Deﬁnition 2.5A The symmetry of t, written as σ (t), is the order of the group
 A(t).

     For example, consider the tree deﬁned by

                     V = {a, b, c, d, e, f , g},
                     E = {{a, b}, {a, c}, {a, d}, {d, e}, {d, f }, {d, g}},
                      r = a,

and represented by the diagram
                                                    e        f         g
                                     t=        b    c             d
                                                         a                                          (2.5 a)

By inspection we see that the only automorphisms are the permutations for which a
and d are invariant, the set {b, c} is invariant and the set {e, f , g} is invariant. Hence,
σ (t) = 2!3! = 12.
2.5 Functions of trees                                                                                 59



 Theorem 2.5B The function σ satisﬁes the recursion

                         σ (τ) = 1,
                                    m
                         σ (t) = ∏ ki !σ (ti )ki ,              t = [t k11 tk22 · · · t kmm ].   (2.5 b)
                                   i=1


Proof. Write the set of vertices of t given in (2.5 b) in the form

                                                     !
                                                     m !
                                                       km
                                         V = V0 ∪                 Vi j ,
                                                     i=1 j=1

where V0 contains the label assigned to the root and Vi j contains the labels assigned
to copy number j of ti . The permutations of the members of V which retain the
connections, as represented by the set of all edges, consist of the compositions of two
types of permutations. These are (1) the permutations within each of the ki copies of
ti , for i = 1, 2, . . . , m, and (2) for each i = 1, 2, . . . , m, the permutations of the sets Ei j ,
 j = 1, 2, . . . , ki , amongst the k j copies of t i . To contribute to the ﬁnal result, (1) gives
a factor of ∏m     i=1 ki ! and (2) gives a factor of ∏i=1 σ (t i ) .
                                                          m           ki


   Another characterization of σ (t) is found by attaching an integer Si to each vertex
                                                                                         kim
i of t. Let the forest of descendant trees from i be tki1i1 tki2i2 · · · t imii , where ti1 , ti2 , . . . ,
timi are distinct. Deﬁne
                                 Si = ki1 !ki2 ! · · · kimi !.                                 (2.5 c)


 Corollary 2.5C With Si deﬁned by (2.5 c),

                                             σ (t) = ∏ Si .
                                                        i


Proof. The result follows by B+ induction.

   The factorial (sometimes referred to as the density) is deﬁned as the product of
the number of dependants of each vertex. For t given by (2.5 a), the numbers of
dependants is shown in (2.5 d), giving t! = 28.
                                                   1        1           1
                                        t=    1    1                        .                     (2.5 d)
                                                                 4
                                                       7
It is convenient to give a formal deﬁnition in the form of a B+ recursion.
60                                                                           2 Trees and forests

 Deﬁnition 2.5D The factorial of t, written as t!, is

                                                    τ! = 1,
                                                                 m
                             t! = ([t 1 t2 · · · t m ])! = |t| ∏(t i !).
                                                                i=1


     A recursion based on the beta operation is sometimes useful.

 Theorem 2.5E The factorial function satisﬁes the following recursion, where t
 and t  are any trees,
                                       τ! = 1,
                                                t! t  ! (|t| + |t |)
                                (t ∗ t  )! =                          .
                                                          |t|

   We next introduce three combinatorial functions which have important applica-
tions in Chapter 3.

α For a given tree t with |t| = n, α(t) is the number of distinct ways of writing t in
  the form (V, E, r), where V is a permutation of {1, 2, . . . , n} and

         every vertex is assigned a lower label than each of its descendants.           (2.5 e)

β For a given tree t with |t| = n, β (t) is the number of distinct ways of writing
  t in the form (V, E, r), where V is a permutation of {1, 2, . . . , n}; there is no
  requirement to satisfy (2.5 e).
β For a given tree t with |t| = n, β (t) is the number of distinct ways of writing t
  in the form (V, E, r), where V is a permutation of {1, 2, . . . , n}; where there is no
  requirement to satisfy (2.5 e) but the root is always labelled 1.

     For t given by (2.5 a), α(t) = 15. The labelled trees contributing to this total are

           5 6   7        5 6   7           4 6   7               4 5   7     4 5   6
         2 3   4        2 4   3           2 5   3               2 6   3     2 7   3
             1              1                 1                     1           1
           5 6   7        4 6   7           4 5   7               4 5   6     3 6   7
         3 4   2        3 5   2           3 6   2               3 7   2     4 5   2
             1              1                 1                     1           1
           3 5   7        3 5   6           3 4   7               3 4   6     3 4   5
         4 6   2        4 7   2           5 6   2               5 7   2     6 7   2
             1              1                 1                     1           1

     The value of β (t) is found to be 420 and β (t) to be 60. The general results are
2.5 Functions of trees                                                                  61



 Theorem 2.5F
                                             |t|!
                                   α(t) =           ,                             (2.5 f)
                                           σ (t)t!
                                            |t|!
                                   β (t) =        ,                               (2.5 g)
                                           σ (t)
                                           (|t| − 1)!
                                   β (t) =            .                           (2.5 h)
                                              σ (t)

 Proof. The value of β (t) given by (2.5 g) is found by assigning labels to the vertices in
all possible ways and then dividing by σ (t) to compensate for duplicated numbering
of the same tree. The factor t! in the denominator of (2.5 f) compensates for the
fact that each vertex v must receive the lowest label amongst the dependants of v.
Similarly, (2.5 h) is found by dividing (2.5 g) by |t| because of the allocation of 1 to
the root.



Standard numbering of trees

We will need to make use of individual rooted trees throughout this volume and it will
be convenient to assign standardized numbers. Denote tree number n in the sequence
of numbered trees as tn . We will now describe the procedure for constructing the
sequence of all trees in the standard order.
    Starting with t1 = τ, we generate trees of orders 2, 3, . . . recursively. For a given
order p > 1 suppose all trees of lower order have been assigned sequence numbers.
To generate the numbered trees of order p, write t = t ∗ tr , where r runs through the
sequence numbers of previously assigned trees of order less than p. For each choice
of r,  runs, in order, through all integers such that |t | + |tr | = p.
    If t constructed by this process has not been assigned a sequence number already,
then it is given the next number available, say n, and we write L(n) =  and R(n) = r.
However, if it is found that t = t ∗ tr is identical with an existing numbered tree, then
t is regarded as a duplicate and no new number needs to be assigned.
    While the numbers are being assigned, it is also convenient to assign standard
factors to each tree tn , in the form tL(n) and tR(n) say. This is done the ﬁrst time each
new tree arises and the same factors automatically apply to any duplicates.
    Table 5 (p. 62) illustrates how this procedure works for trees up to order 4. The
values of t = t and t  = tr are shown for each tree together with its sequence number.
However, in the case that this is a duplicate, no diagram for the tree is given and no
entries are given for L or R.
    In the numbering system based on these rules, the partition orders, for a given tree
order, are automatically in decreasing order as was illustrated in Table 3 (p. 51). The
standard numbering is shown to order 6 in Table 6 (p. 63), classiﬁed according to the
partition order.
    For example, the numbered trees up to order 4 are
62                                                                      2 Trees and forests


  Table 5 Illustration of tree numbering to order 4, showing the beta-product and unique
  factorization

       t                                   t  ∗ t           L(t)              R(t)

       t1

       t2                                 t1 ∗ t1               1                 1

       t3                                 t2 ∗ t1               2                 1

       t4                                 t1 ∗ t2               1                 2

       t5                                 t3 ∗ t1               3                 1

       t6                                 t4 ∗ t1               4                 1

       t6                                 t2 ∗ t2

       t7                                 t1 ∗ t3               1                 3

       t8                                 t1 ∗ t4               1                 4




            t1 =                 t2 =                   t3 =              t4 =


            t5 =                 t6 =                   t7 =              t8 =
 A Julia realization of Algorithm 3 (p. 64), using p max = 5, gives the expected
results:
                                                                         
        L[2] L[3] · · · L[17]          1 2 1 3 4 1 1 5 6 7 8 4 1 1 1 1
                                 =
        R[2] R[3] · · · R[17]          1 1 2 1 1 3 4 1 1 1 1 2 5 6 7 8
                                                      ⎡                        ⎤
⎡                                                   ⎤    2 4 7 8 14 15 16 17
  prod[1, 1] prod[1, 2] · · · prod[1, 7] prod[1, 8]   ⎢ 3 6 11 12              ⎥
⎢                                                   ⎥ ⎢                        ⎥
⎢prod[2, 1] prod[2, 2] · · · prod[2, 7]               ⎢
                                                    ⎥ ⎢ 5 10                   ⎥
⎢                                                   ⎥ ⎢                        ⎥
⎢                                                   ⎥                          ⎥
⎢     ..                                              ⎢
                                                    ⎥=⎢  6 13                  ⎥
⎢                                                   ⎥ ⎢ 9                      ⎥,
⎢
       .
                                                    ⎥ ⎢                        ⎥
⎢prod[7, 1]                                         ⎥ ⎢10                      ⎥
⎣                                                   ⎦ ⎢                        ⎥
                                                                               ⎥
 prod[8, 1]                                           ⎣ 11                     ⎦
                                                        12
where the blank entries are not relevant to this algorithm if the maximum order is 5.
2.5 Functions of trees                                                                            63


    Table 6 Standard numbering of trees to order 6 classiﬁed by the order of their partitions


  t1 =


  t2 =


  t3 =


  t4 =



  t5 =


  t6 =


  t7 =         t8 =


  t9 =


  t10 =


  t11 =        t12 =      t13 =



  t14 =        t15 =      t16 =      t17 =


  t18 =


  t19 =


  t20 =        t21 =      t22 =



  t23 =        t24 =      t25 =      t26 =     t27 =       t28 =




  t29 =        t30 =      t31 =      t32 =     t33 =       t34 =      t35 =     t36 =     t37 =
64                                                                          2 Trees and forests


                                Algorithm 3 Generate trees

Input:      p max
Output: order, ﬁrst, last, L, R, prod
    %
    % order[1:last[p max]] is the vector of orders of each tree
    % ﬁrst[1 : p max] is the vector of ﬁrst serial numbers for each order
    % last[1 : p max] is the vector of last serial numbers for each order
    % L[2 : p max] is the vector of left factors for each tree
    % R[2 : p max] is the vector of right factors for each tree
    % prod is the array of products of trees  and r such that order[] + order[r] ≤ p max
    %
  1 ﬁrst[1] ← 1
  2 last[1] ← 1
  3 order[1] ← 1
  4 n←1
  5 for p from 2 to p max do
  6    ﬁrst[p] ← n + 1
  7    for r from 1 to last[p − 1] do
  8        for  from ﬁrst[p − order[r]] to last[p − order[r]] do
  9           if  = 1 or r ≤ R[] then
 10               n ← n+1
 11               prod[, r] ← n
 12               L[n] ← 
 13               R[n] ← r
 14               order[n] ← p
 15           else
 16               prod[l, r] ← prod[prod[L[], r], R[]]
 17           end if
 18        end for
 19    end for
 20    last[p] ← n
 21 end for



   The standard numbering of the 37 trees to order 6 is shown in Table 7 (p. 66).
Also shown are σ (t) and t! for each tree t.


Generation of various tree functions

We will temporarily introduce Bminus to denote some aspects of the structure of a tree
t. Let n = |t|, then Bminus(t) will denote a vector with dimension m := last[n − 1],
where last[n] is the total number of trees with order up to n. Thus last is one of the
results computed in Algorithm 3. The elements of Bminus(t) are the exponents k1 ,
k2 , . . . , km such that
                             t = tk11 tk22 · · · tkmm .

   We will present Algorithm 4 to generate values of Bminus together with the
factorial and symmetry functions.
2.6 Trees, partitions and evolutions                                                                       65


                              Algorithm 4 Generate tree functions

Input:     order, ﬁrst, last, L, R, prod, p max from Algorithm 3
Output: Bminus, factorial, symmetry
    %
    % Bminus[n][1 : order[n] − 1], n = 1 : lastp top, is the sequence of Bminus values
    % factorial[1 : last[p top] is the array of factorial values
    % symmetry[1 : last[p top] is the array of symmetry values
    %
  1 Bminus[1] ← []
  2 factorial[1] ← 1
  3 symmetry[1] ← 1
  4 for n from 2 to last[p max] do
  5    factorial[n] ← factorial[L[n]] ∗ factorial[R[n]] ∗ order[n]/order[L[n]]
  6    Bminus[n] = pad(last[order[n]], Bminus[L[n]])
  7    Bminus[n][R[n]] ← Bminus[n][R[n]] + 1
  8    symmetry[n] = symmetry[L[n]] ∗ symmetry[R[n]] ∗ Bminus[n][R[n]]
  9 end for



   A realization in Julia, up to order 5, gave the results

 symmetry = {1, 1, 2, 1, 6, 1, 2, 1, 24, 2, 2, 1, 2, 6, 1, 2, 1}
  factorial = {1, 2, 3, 6, 4, 8, 12, 24, 5, 10, 15, 30, 20, 20, 40, 60, 120}
              
   Bminus = {}, {1}, {2, 0}, {0, 1}, {3, 0, 0, 0}, {1, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1},
                {4, 0, 0, 0, 0, 0, 0, 0}, {2, 1, 0, 0, 0, 0, 0, 0}, {1, 0, 1, 0, 0, 0, 0, 0},
                {1, 0, 0, 1, 0, 0, 0, 0}, {0, 2, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 1, 0, 0, 0},
                                                                                            
                {0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 0, 0, 1}


2.6 Trees, partitions and evolutions

For a tree t = [t 1 t2 · · · t m ], with |t| = n + 1, the set {|t1 |, |t2 |, . . . , |t m |} is a partition of
n into m components, in the sense that

                                     |t1 | + |t2 | + · · · + |tm | = n.

In this section, we will consider partitions of sets and numbers and their relationship
with trees. Partitions have a central role in the study of B-series in Chapter 3, with
speciﬁc applications in Section 3.5 (p. 117).


Partitions of numbers and sets

Given a ﬁnite set S with cardinality n, it is convenient to distinguish between the
partitions of S, denoted by P[S], and of n, denoted by P(n).
66                                                                        2 Trees and forests


                        Table 7 Trees to order 6 showing σ (t) and t!

 |t|   t                          σ (t)    t!    |t|   t                        σ (t)    t!
 1     t1      τ                   1        1     6 t18       [τ 5 ]             120      6

                                                  6 t19     [τ 3 [τ]]             6      12

 2     t2     [τ]                  1        2     6 t20    [τ 2 [τ 2 ]]           4      18

                                                  6 t21 [τ 2 [2 τ]2 ]             2      36

 3     t3    [τ 2 ]                3        3     6 t22     [τ[τ]2 ]              2      24

 3     t4   [2 τ]2                 1        6     6 t23     [τ[τ 3 ]]             6      24

                                                  6 t24 [τ[τ[τ]]]                 1      48

 4     t5    [τ 3 ]                6        4     6 t25 [τ[2 τ 2 ]2 ]             2      72


 4     t6   [τ[τ]]                 1        8     6 t26    [τ[3 τ]3 ]             1     144

 4     t7   [2 τ 2 ]2              2       12     6 t27 [[τ][τ 2 ]]               2      36

 4     t8   [3 τ]3                 1       24     6 t28 [[τ][2 τ]2 ]              1      72

                                                  6 t29 [τ[2 τ 2 ]2 ]            24      30

 5     t9    [τ 4 ]                24       5     6 t30    [τ[3 τ]3 ]             2      60

 5 t10 [τ 2 [τ]]                   2       10     6 t31 [[τ][τ 2 ]]               2      90


 5 t11 [τ[τ 2 ]]                   2       15     6 t32 [[τ][2 τ]2 ]              1     180

 5 t12 [τ[2 τ]2 ]                  1       30     6 t33     [2 [τ]2 ]2            2     120

 5 t13      [[τ]2 ]                2       20     6 t34     [3 τ 3 ]3             6     120


 5 t14      [2 τ 3 ]2              6       20     6 t35    [3 τ[τ]]3              1     240


 5 t15 [2 τ[τ]]2                   1       40     6 t36     [4 τ 2 ]4             2     360



 5 t16      [3 τ 2 ]3              2       60     6 t37      [5 τ]5               1     720


 5 t17      [4 τ]4                 1      120
2.6 Trees, partitions and evolutions                                                            67


                           Table 8 Systematic generation of trees to order 6


            t1


            t2 = t1 ∗ t1


            t3 = t2 ∗ t1                t4 = t1 ∗ t2


            t5 = t3 ∗ t1                t6 = t4 ∗ t1            t6 = t2 ∗ t2     t7 = t1 ∗ t3


            t8 = t1 ∗ t4


            t9 = t5 ∗ t1               t10 = t6 ∗ t1           t11 = t7 ∗ t1    t12 = t8 ∗ t1


           t10 = t3 ∗ t2               t13 = t4 ∗ t2           t11 = t2 ∗ t3    t12 = t2 ∗ t4



           t14 = t1 ∗ t5               t15 = t1 ∗ t6           t16 = t1 ∗ t7    t17 = t1 ∗ t8


           t18 = t9 ∗ t1               t19 = t10 ∗ t1          t20 = t11 ∗ t1   t21 = t12 ∗ t1


           t22 = t13 ∗ t1              t23 = t14 ∗ t1          t24 = t15 ∗ t1   t25 = t16 ∗ t1



           t26 = t17 ∗ t1              t19 = t5 ∗ t2           t22 = t6 ∗ t2    t27 = t7 ∗ t2


           t28 = t8 ∗ t2               t20 = t3 ∗ t3           t28 = t4 ∗ t3    t21 = t3 ∗ t4


           t28 = t4 ∗ t4               t23 = t2 ∗ t5           t24 = t2 ∗ t6    t25 = t2 ∗ t7


           t26 = t2 ∗ t8               t29 = t1 ∗ t9           t30 = t1 ∗ t10   t31 = t1 ∗ t11



           t32 = t1 ∗ t12              t33 = t1 ∗ t13          t34 = t1 ∗ t14   t35 = t1 ∗ t15




           t36 = t1 ∗ t16              t37 = t1 ∗ t17
68                                                                            2 Trees and forests



 Deﬁnition 2.6A A partition P of a set S is a set of subsets P = {S1 , S2 , . . . , Sm },
                                                                      "
 such that for i = j ∈ {1, 2, . . . , m}, Si ∩S j = 0/ and such that m  i=1 Si = S. A partition
 p of a positive integer n is a set of integers p = {p1 , p2 , . . . , pm }, where repetitions
 are permitted, such that n = ∑m      i=1 pi . If the components are permuted, it is regarded
 as the same partition.

For example, if S = {1, 2, 3, 4}, then the members of P[S] are {{1}, {2}, {3}, {4}},
{{1}, {2}, {3, 4}} and 13 further partitions. We will, for reasons of brevity, write these
in a compressed notation, starting with: 1+2+3+4, 1+2+34 . . . . The complete list is
              (line 1)     1+2+3+4,

              (line 2)     1+2+34, 1+3+24, 1+4+23, 2+3+14, 2+4+13, 3+4+12,

              (line 3)     1+234, 2+134, 3+124, 4+123,                                   (2.6 a)
              (line 4)     12+34, 13+24, 14+23,

              (line 5)     1234,


where the ﬁve lines correspond to the ﬁve members of P(4):

                                        (line 1)   1+1+1+1,
                                        (line 2)   1+1+2,
                                        (line 3)   1+3,                                  (2.6 b)
                                        (line 4)   2+2,
                                        (line 5)   4.


 Deﬁnition 2.6B Let P = {S1 , S2 , . . . , Sm }, be a partition of a set S, then φ (P) is
 the corresponding partition of n = card(P), where

                         φ (P) = {card(S1 ), card(S2 ), . . . , card(Sm )}.

 Furthermore, if p is a partition of the integer n then the p-weight is

                                   p-weight(p) := card(φ −1 (p)).


   As examples of these ideas, φ (P) for any of the partitions in line i = 1, 2, 3, 4, 5
of (2.6 a) are given in the corresponding lines of (2.6 b). Furthermore, by comparing
(2.6 a) and (2.6 b), we see that

     p-weight(1+1+1+1) = 1, p-weight(1+1+2) = 6, p-weight(1+3) = 4,
           p-weight(2+2) = 3,     p-weight(4) = 1.

  In referring to a particular partition of n, it will often be convenient to specify the
number of repetitions of each particular component. Thus we will write
2.6 Trees, partitions and evolutions                                                         69

                                       1n1 + 2n2 + · · · + mnm ,

as an alternative way of writing
                         n                  nm n
                 #  $%1  & #  $%2  &     #  $%   &
                 1+1+···+1+2+2+···+2+···+m+m+···+m

to specify the number of occurrences of each part. In this notation, kn j may be omitted
from the sum if n j = 0, for 1 ≤ j ≤ m. For example, the 7 partitions of 5 can be
written in several alternative but equivalent ways, such as

                             1+1+1+1+1 = 15 ,
                                 1+1+1+2 = 13 +2,
                                     1+1+3 = 12 +20 +3 = 12 +3,
                                     1+2+2 = 1+22 ,
                                         1+4 = 1+20 +30 +4,
                                         2+3 = 10 +2+3,
                                              5 = 10 +20 +30 +40 +5.

The value of p-weight

Using this terminology, it is possible to ﬁnd a formula for p-weight.


 Theorem 2.6C Let p = 1n1 + 2n2 + · · · + mnm ∈ P(n). Then
                                                             n!
                                 p-weight(p) =                         .
                                                       ∏m
                                                        i=1 ni !(i!)
                                                                    ni




Proof. Given P ∈ P[S], with card(S) = n, let
                                     !
                                     m !
                                       ni
                               P=             Si j ,      card(Si j ) = i,
                                    i=1 j=1

where φ (P) = 1n1 + 2n2 + · · · + mnm ∈ P(n), The number of ways of assigning n
labels to the sets Si j is given by the multinomial coefﬁcient n!/∏m                 ni
                                                                             i=1 (i!) . However,
for each i = 1, 2, . . . , m, the ni sets Si j , j = 1, 2, . . . , ni can be permuted without
altering the set-partition. Hence, to compensate for the over-counting, it is necessary
to divide by i!.



Exercise 25 Find the partitions of {1, 2, 3, 4, 5} such that φ (P) = 1 + 22 .
70                                                                              2 Trees and forests

Evolution of partitions of numbers and sets
Evolution of P[S] for an embedded sequence of sets
Let S1 ⊂ S2 ⊂ · · · be a sequence of sets, such that card(Sn ) = n, n = 1, 2, . . . Equiva-
lently, given a sequence of distinct symbols s1 , s2 , . . . , deﬁne Sn = {s1 , s2 , . . . , sn }. In
the examples given below, we will use the sequence starting with {1, 2, 3, 4, 5, . . . }.

 Deﬁnition 2.6D Given partitions P ∈ P[Sn−1 ], P ∈ P[Sn ], P is the parent of P
 and P is a child of P if (i) card(P) = card(P ) − 1 and P = P + {sn }, or (ii) P0 ,
 P1 , P2 exist such that

                                         P = P0 ∪ {P1 },
                                         P = P0 ∪ {P2 },
                                        P2 = P1 ∪ {sn }.

We will explore the evolutionary structure of the sequence of partitions P[Sn ], n =
1, 2, . . . , as we move from step to step and indicate with the notation P −→ P that P
is a child of P.
    Given P ∈ P[Sn−1 ], the expression evolve(P) will denote the formal sum of the
children of P. Furthermore, evolve acting on a formal sum of members of P[Sn−1 ]
will denote                                
                                          m          m
                              evolve     ∑ Pi = ∑ evolve(Pi ).
                                         i=1        i=1

     For the sequence, {1}, {1, 2}, {1, 2, 3}, {1, 2, 3, 4}, this structure is shown in Figure 4.



                                                                                  1+2+3+4
                                                                                  1+2+34
                                                          1+2+3                   1+3+24
                                                                                   1+4+23
                                 1+2
                                                                                   2+3+14
                                                          1+23
                                                                                   2+4+13
                                                                                   3+4+12
          1                                               2+13                      1+234
                                                                                    2+134
                                                                                    3+124
                                                          3+12
                                                                                    4+123
                                  12
                                                                                    14+23
                                                            123                     13+24
                                                                                    12+34
                                                                                    1234



     Figure 4 Evolution of the partitions from P[{1}], through P[{1, 2}], P[{1, 2, 3}] to
     P[{1, 2, 3, 4}]
2.6 Trees, partitions and evolutions                                                     71


                                                                         1+1+1+1
                                                       1+1+1
                                1+1                                        1+1+2

        1                                               1+2                 1+3

                                 2                                          2+2
                                                         3
                                                                              4


      Figure 5 Evolution of the partitions from P(1), through P(2), P(3) to P(4)




Evolution of P(n) for n = 1, 2, 3, 4


 Deﬁnition 2.6E Given partitions p ∈ P(n − 1), p ∈ P(n), p is the parent of p
 and p is a child of p if (i) either p = p ∪ {1} or (ii) a set p0 , and an integer m > 1
 exist such that

                                        p = p0 ∪ {m − 1},
                                       p = p0 ∪ {m}.

Given p ∈ P(n−1 ), the expression evolve(p) will denote the formal linear combina-
tion of the children of p, where the factor m is introduced in mp if p is a child of
p in m different ways. Furthermore, evolve acting on a formal sum of members of
P(n − 1) will denote
                                      m      m
                           evolve      ∑ i i = ∑ Ci evolve(pi ).
                                        C p
                                       i=1       i=1

For example,
                           evolve(13 ) = (14 ) + 3(12 + 2),
                        evolve(1 + 2) = (12 + 2) + (1 + 3) + (22 ),
                            evolve(3) = (1 + 3) + (4),

compared with evolve(P) for some corresponding sets, as read off from Figure 4,

            evolve(1+2+3) = (1+2+3+4)+(1+2+34)+(1+3+24)+(1+4+23),
             evolve(1+23) = (1+234)+(1+4+23)+(14+23),
               evolve(123) = (4+123)+(1234).

   Corresponding to Figure 4 the evolutionary scheme for partitions of numbers is
shown in Figure 2.6, where p −→ p indicates that p is a child of p.
72                                                                               2 Trees and forests



     0/         1          1+1          2           1+1+1                  1+2                3




     Figure 6 Evolution of the partitions from P(0), through P(1), P(2) to P(3) together
     with associated trees from t1 to t8




   As partitions evolve, related trees evolve at the same time. This will be illustrated
in Figure 6, showing how the evolution from P(0) to P(3) is accompanied by trees
with orders 1 to 4.

Evolution of labelled trees

We will look at the production of the α(t) copies of t as the result of an evolutionary
process in which, in each step, one new vertex is added in every possible way to an
existing tree. Denote each of the steps as the result of an operator evolve acting on
an existing linear combination of trees. Examples of the action on low order trees are
                                 evolve(∅) = t1 ,
                                 evolve(t1 ) = t2 ,
                                 evolve(t2 ) = t3 + t4 ,
                                 evolve(t3 ) = t5 + 2t6 ,
                                 evolve(t4 ) = t6 + t7 + t8 ,
where the action on ∅ is conventional.
     These correspond to the diagrams

                            ∅    →          ,
                                 →          ,
                                 →              +          ,
                                 →              + 2            ,

                                 →              +                  +   .

Write evolven as the composition of n applications of evolve and we have
2.6 Trees, partitions and evolutions                                                                  73


                                                                                 2        3       4
                                                                                          1
                                                                                              4
                                                       2        3
                                                                                     2        3
                                                            1
                                                                                          1
                                                                                              4
                                  2
         1                                                                           3        2
                                  1
                                                                                          1
                                                                                              3
                                                                                     4        2
                                                                                          1
                                                           3                         3        4
                                                           2                              2
                                                           1                              1
                                                                                          4
                                                                                          3
                                                                                          2
                                                                                          1



                       Figure 7 Evolution of labelled trees up to order 4



 Theorem 2.6F
                                  evolven (∅) = ∑ α(t)t
                                                   |t|=n


Up to order 5 we have
             evolve(∅) = t1 ,
         evolve2 (∅) = t2 ,
         evolve3 (∅) = t3 + t4 ,
         evolve4 (∅) = t5 + 3t6 + t7 + t8 ,
         evolve5 (∅) = t9 + 6t10 + 4t11 + 4t12 + 3t13 + t14 + 3t15 + t16 + t17 .
It will now be shown diagrammatically how trees evolve by the addition of a further
labelled vertex in each step. For order n, the labelled vertex n is attached to existing
vertices in Sn−1 in all possible ways. This gives the evolution diagram shown in
Figure 7.
   Strip off the labels and consolidate the identical unlabelled trees that remain. The
result is shown in Figure 8 (p. 74).
Recursions for evolve

 Theorem 2.6G evolve(t) satisﬁes the recursion
                    '
                      t2 ,                                              t = t1 ,
        evolve(t) =
                      evolve(t  ) ∗ t + t ∗ evolve(t ),           t = t  ∗ t .
74                                                                                        2 Trees and forests




                                                                                           3
                                                                                                6

                                                                                                4
                                                                    2
                                                                          3                      4

                                                                                                 3



                                                                                            2

                                                                                                 3




                              Figure 8 Evolution of trees up to order 5



Proof. The result for evolve(τ) is clear. It remains to show that evolve(t ∗ t ) =
evolve(t  ) ∗ t  + t  ∗ evolve(t  ). The ﬁrst term is the result of adding an additional
child to each vertex in t in turn and the second term is the result of adding the
additional child to the vertices of t .


 Theorem 2.6H evolve(t) satisﬁes the recursion
                '
                  [t1 ],                                                 t = t1 ,
    evolve(t) =
                  t ∗ τ + ∑i=1 [t1 , . . . , evolve(t i ), . . . , tm ], t = [t 1 , t 2 , . . . , t m ].
                           m



Proof. To prove the second option, note that an additional child is added in turn to the
root, resulting in the term t ∗ τ or, for each i, to one one of the vertices in ti , resulting
in the term [t1 , . . . , evolve(t i ), . . . , tm ].

The contributions of R. H. Merson
In 1957 a remarkable paper [72] appeared. This described the evolved combinations
of trees, as illustrated here in Figure 8. As shown in Merson’s work, and further
discussed in Chapter 3 of the present volume, these expressions give the Taylor
2.6 Trees, partitions and evolutions                                                                 75

series for the ﬂow of a differential equation and, when compared with the series for a
numerical approximation, enable the order conditions for Runge–Kutta method to be
written down.

Labelling systems for forests

Using tuples

Let tuple denote the set of all n-tuples of positive integers, for n = 1, 2, 3, . . . . That is
                                                                                                     
    tuple = (1), (2), (3), . . . , (1, 1), (1, 2), . . . , (2, 1), . . . , (1, 1, 1), (1, 1, 2), . . . .

If x ∈ tuple is an n-tuple, denote by x− the (n − 1)-tuple formed from x by omitting
the ﬁnal integer from x. Conventionally, if n = 1, write x− = ( ) ∈ tuple.
   If (V, E) is connected then it is a labelled tree. The connected components combine
together to comprise a forest. If (V, E) = (0, / 0)
                                                  / then this is the empty tree ∅.

 Deﬁnition 2.6I For V ∈ tuple, a ﬁnite subset, deﬁne the graph (V, E) generated
 by V as the labelled graph for which
                                                   
                             E = (x− , x), x, x− ∈ V .



 Lemma 2.6J Every forest corresponds to a graph generated by some ﬁnite subset
 of tuple.

Proof. It will be enough to restrict the result to trees and we prove this case by beta
induction. For τ we can choose V = {(1)}. Assume the result has been proved for t
and t , and that these correspond to V and V  , respectively. Without loss of generality,
assume that the roots are (1) and (m), where m is chosen so that no member of V
is of the form (1, m). We note that every member of V  begins with the sequence
(m, . . . ). Form V  by prepending 1 to each member of V  so that the members of V 
are of the form (1, m, . . . ). The tree t ∗ t  corresponds to V ∪V  .



Convenient shortened form for tuples

For convenience a tuple such as (1, 2, 1, 1) will sometimes be written as 1211, if no
confusion is possible.
   We illustrate the formation of the beta product of two trees labelled using tuples,
in a diagram:
76                                                                         2 Trees and forests
              1211                                                    1211

                      122                                                       122   131
             121                                                     121
       111                                                     111
                                            31
                                   ∗
              11     12                                               11     12            13
                                                          =
                          1                      3                                1




The universal forest

We can construct a connected graph with a denumerable set of vertices V = tuple,
rather than a ﬁnite subset. If we restrict the members of tuple to be based on {1, 2, 3},
rather than all the positive integers, we obtain a graph shown partially in this diagram.




                                                                  1
                                                                  2
                                                                  3

                                                                  1
                                                                  2
                                                                  3

                                                                  1
                                                                  2
                                                                  3
         3



         3

         1
         2
         3




                                          3



                                          3

                                          1
                                          2
                                          3
         1
         2


         1
         2




                                          1
                                          2


                                          1
                                          2




                                                               31
                                                               31
                                                               31

                                                               32
                                                               32
                                                               32

                                                               33
                                                               33
                                                               33
      11



      12

      13
      13
      13




                                       21



                                       22

                                       23
                                       23
                                       23
      11
      11


      12
      12




                                       21
                                       21


                                       22
                                       22




        11           12       13       21            22   23   31          32         33
                     1                               2                     3




2.7 Trees and stumps

We now consider a generalization of trees, known as “stumps”. These can be regarded
as modiﬁed trees with some leaves removed, but the edges from these leaves to their
parents retained.
  In the examples given here, a white disc represents the absence of a vertex. The
number of white discs is the “valency”. Note that trees are stumps with zero valency.




             Valency 0


             Valency 1


             Valency 2


Right multiplication by one or more additional stumps implies grafting to open
valency positions.
2.7 Trees and stumps                                                                    77

Products of stumps

Given two stumps s, s , the product ss has a non-trivial product if s is not the trivial
stump and s has valency at least 1; that is, it is not a tree, the product is formed by
grafting the root of s to the rightmost open valency in s.
   Two examples of grafting illustrate the signiﬁcance of stump orientations



                                               =

                                               =

If the s1 is a tree, no contraction takes place.


Atomic stumps

An atomic stump is a graph of the form




Note that no more than two generations can be present.
   If m of the children of the root are represented by black discs and n are repre-
sented by white discs, then the atomic stump is denoted by smn . The reason for the
designation “atomic” is that every tree can be written as the product of atoms.
   This is illustrated up to trees of order 4:

                 = s00                          = s30

                 = s10                          = s11 s10     =

                 = s20                          = s01 s20     =


                 = s01 s10 =                    = s01 s01 s10 =



Isomeric trees

In the factorization of trees into products of atoms the factors are written in a speciﬁc
order with each factor operating on later factors. However, if we interpret the atoms
just as symbols that can commute with each other, we get a new equivalence relation:
78                                                                       2 Trees and forests



 Deﬁnition 2.7A Two trees are isomeric if their atomic factors are the same.

Nothing interesting happens up to order 4 but, for order 5, we ﬁnd that



                                   = s11 s01 s10 ∼ s01 s11 s10 =

It is a simple exercise to ﬁnd all isomeries of any particular order but, as far as the
author knows, this has not been done above order 6.
     For orders 5 and 6, the isomers are


               = s11 s01 s10                 = s01 s11 s10



               = s02 s10 s01 s10             = s01 s02 s10 s10



               = s11 s01 s20                 = s01 s11 s20



               = s21 s01 s10                 = s01 s21 s10



               = s11 s01 s01 s10             = s01 s11 s01 s10     = s01 s01 s11 s10




Exercise 26 Find two isomers of s21 s01 s01 s10 .




Isomers and Runge–Kutta order conditions

In Chapter 5, Section 5.2 (p. 183), structures related to stumps and isomeric trees will
play a special role in distinguishing between the order conditions for Runge–Kutta
methods intended for scalar problems, as distinct from more general high-dimensional
systems.
2.7 Trees and stumps                                                                           79

The algebra of uni-valent stumps

Let S denote the semi-group under multiplication of stumps with valency 1. The
multiplication table for this semi-group, restricted to stumps of orders 1 and 2, is
shown on the left diagram below. Associativity is easy to verify.


                                                              τ1         τ2 τ    τ1 τ1

                                                   τ1        τ1 τ1   τ1 τ2 τ    τ1 τ1 τ1

                                                  τ2 τ       τ2 ττ1 τ2 ττ2 τ τ2 ττ1 τ1


                                                  τ1 τ1 τ1 τ1 τ1 τ1 τ1 τ2 τ τ1 τ1 τ1 τ1



Exercise 27 Extend the right table to include stumps to order 3.



Sums over S

By taking free sums of members of S, we can construct an algebra of linear operators.
Because of the important applications to the analysis of Rosenbrock and related
methods, and to applications to exponential integrators, we will focus on members of
this algebra of the form

                               a0 1 + a1 τ1 + · · · + an τ12 + · · · .                     (2.7 a)

Let φ (z) = a0 + a1 z + a2 z2 + · · · , then we will conventionally write (2.7 a) as φ (τ1 ).
   Important examples in applications are

                                      φ (z) = (1 − γz)−1 ,
                                                 1+ 12 z
                                      φ (z) =            ,
                                                 1− 12 z
                                                 exp(z)−1
                                      φ (z) =        z    .

Taking this further, we can give a meaning to expressions such as

                         φ (τ1 )t = a0 t + a1 [t] + · · · + an [n t]n + · · ·              (2.7 b)

   This topic will be considered again, in the context of B-series and numerical
applications, in Chapter 4, Section 4.8 (p. 174).
80                                                                         2 Trees and forests

Sub-atomic stumps

The special stumps s0,n = τn , with order 1, have a special role in Chapter 5, Section 5.2
(p. 183).
    It is possible to write these in terms of an incomplete use of the beta product.
That is, t∗ is a stump in which an additional valency is assigned to the root of t. This
would mean that
                                     (t∗)t  = t ∗ t .
Write ∗n for an iterated use of this notation and we have

                                      τ∗n = s0,n = τn .                                (2.7 c)




 Deﬁnition 2.7B The objects occurring in (2.7 c) are sub-atomic stumps.

Note the identity
                                      smn = τ ∗m+n τ m ,
which allows any tree written as a product of atomic stumps to be further factorized
into sub-atomic stumps.



2.8 Subtrees, supertrees and prunings

A subtree, of a given tree, is a second tree embedded within the given tree, and
sharing the same root. The vertices, and their connecting edges, cut off from the
original tree to expose the subtree, comprise the offcut. Since the offcut is a collection
of trees, they will be interpreted as a forest. If t is a subtree of t, then t is said to be a
“supertree” of t . The words “pruning” and “offcut” will be used interchangeably.
    For the two given trees,


                                   t =    ,      t=       ,                           (2.8 a)

t  is a subtree of t, and t is a supertree of t , because t can be constructed by deleting
some of the vertices and edges from t or, alternatively, t can be constructed from t 
by adding additional vertices and edges.
     The member of the forest space, written as t t , is formed from the collection
of possible ways t can be “pruned” to form t  . For example, for the trees given by
(2.8 a),
                                         t t = 2       ,
2.8 Subtrees, supertrees and prunings                                                             81

where the factor 2 is a consequence of the fact that t could equally well have been
replaced by its mirror image.
   These simple ideas can lead to ambiguities and the possibility of duplications.
For example, the tree t = [[τ][τ 2 ]], regarded as a labelled tree, can have the offcut
removed in three ways to reveal the subtree t = [2 τ]2 . This can be illustrated by
using the orientations of the edges of t to distinguish them, as shown in the diagram


                             case     t showing cuts       t showing orientation       offcut


                              (i)
       cut =

                              (ii)


                             (iii)


   In this diagram, cases (i) and (ii) give the same t and the same prunings. Hence,
a complete list of these for a given t would need to contain a duplicate of this case.
Further, case (iii) needs to be included separately even though they both have the
same subtree t . We can deal with cases like this by reverting to the use of labelled
trees. That is, we can use the (V, E, r) characterization of trees.
      For the present example, we would identify t with the triple

         t = (V, E, r) := ({a, b, c, d, e, f }, {(a, b), (b, c), (b, d), (a, e), (e, f )}, a),

as in the diagram
                                              fd c
                                                 be
                                               a
                        
and the three cases of t = (V1 , E1 , r) and f = (V2 , E2 ) are

(i)     (V1 , E1 , r) = ({a, b, c}, {(a, b), (b, c)}, a), (V2 , E2 ) = ({d, e, f }, {(e, f )}),
(ii) (V1 , E1 , r) = ({a, b, d}, {(a, b), (b, d)}, a), (V2 , E2 ) = ({c, e, f }, {(e, f )}),
(iii) (V1 , E1 , r) = ({a, e, f }, {(a, e), (e, f )}, a), (V2 , E2 ) = ({b, c, d}, {(b, c), (b, d)}).

   In specifying a subgraph (V1 , E1 ) of (V, E), it will be enough to specify only V1
because the vertex pairs which comprise E1 are the same as for E except for the
deletion of any pair containing a member of V V1 ; hence the members of E1 do not
need to be stated. In this example, we can write

                                                  = 2        +
82                                                                              2 Trees and forests

Main deﬁnition


 Deﬁnition 2.8A Consider a labelled tree t = (V, E, r). A subgraph deﬁned by V1 ,
 with the property that parent(v) ∈ V1 whenever v ∈ V1 , is a subtree t  of t. If V1 ⊂ V
 then t is a proper subtree. The corresponding offcut is the forest formed from
 V V1 .


   In using subtrees in later discussions, we will not give explicit labels for the
elements of V , but we will take the existence of labels into account.

Notations
Notationally, t ≤ t will denote “t  is a subtree of t” and t < t will denote “t is a
proper subtree of t”; Also ∅ < t  ≤ t and ∅ < t  < t will have their obvious meanings.
The offcuts corresponding to V V1 will be written t t .
   Using the Sweedler notation [89] (Underwood, 2011), adapted from co-algebra
theory, a “pruning” consisting of an offcut-subtree pair will be written (t t ) ⊗ t .

 Deﬁnition 2.8B The function Δ (t) denotes the combination, with integer weights
 to indicate repetitions, of all prunings of t. That is,

                                Δ (t) = t ⊗ ∅ + ∑ (t t  ) ⊗ t .
                                                    t ≤t


     For example,
            
         Δ     = 1⊗          + ⊗       +2 ⊗        +3 ⊗          + ⊗      +    ⊗
                       +2      ⊗ +         ⊗ +         ⊗ +         ⊗ +        ⊗ +     ⊗∅

or, using the standard numbering established in Table 6 (p. 63),

     Δ (t27 ) = 1 ⊗ t27 + t1 ⊗ t11 + 2t1 ⊗ t13 + 3t21 ⊗ t6 + t2 ⊗ t7 + t31 ⊗ t3
                    + 2t1 t2 ⊗ t4 + t3 ⊗ t4 + t21 t2 ⊗ t2 + t1 t3 ⊗ t2 + t2 t3 ⊗ t1 + t27 ⊗ ∅.

     For later reference, Δ (t), for |t| ≤ 5, is given in Table 9.

Subtrees in Polish notation
If ∅ < t ≤ t, write
                                t  = τm1 τm2 · · · τmk ,   k = |t  |.
then we can write a representation of t using the operator , introduced in Section 2.2
(p. 47). This result is given without proof.
2.8 Subtrees, supertrees and prunings                                                                    83


                                           Table 9 Δ (t), for |t| ≤ 5

 |t|   t   Δ (t)
 0     ∅ 1⊗∅
 1     t1 1 ⊗ t1 + t1 ⊗ ∅
 2     t2 1 ⊗ t2 + t1 ⊗ t1 + t2 ⊗ ∅
 3     t3 1 ⊗ t3 + 2t1 ⊗ t2 + t21 ⊗ t1 + t3 ⊗ ∅
 3     t4 1 ⊗ t4 + t1 ⊗ t2 + t2 ⊗ t1 + t4 ⊗ ∅
 4     t5 1 ⊗ t5 + 3t1 ⊗ t3 + 3t21 ⊗ t2 + t31 ⊗ t1 + t5 ⊗ ∅
 4     t6 1 ⊗ t6 + t1 ⊗ t3 + t1 ⊗ t4 + t21 ⊗ t2 + t2 ⊗ t2 + t1 t2 ⊗ t1 + t6 ⊗ ∅
 4     t7 1 ⊗ t7 + 2t1 ⊗ t4 + t21 ⊗ t2 + t3 ⊗ t1 + t7 ⊗ ∅
 4     t8 1 ⊗ t8 + t1 ⊗ t4 + t2 ⊗ t2 + t4 ⊗ t1 + t8 ⊗ ∅
 5     t9 1 ⊗ t9 + 4t1 ⊗ t5 + 6t21 ⊗ t3 + 4t31 ⊗ t2 + t41 ⊗ t1 + t9 ⊗ ∅
 5 t10 1⊗t10 +2t1 ⊗t6 +t1 ⊗t5 +2t21 ⊗t3 +t21 ⊗t4 +t2 ⊗t3 +t31 ⊗t2 +2t1 t2 ⊗t2 +t21 t2 ⊗t1 +t10 ⊗∅
 5 t11 1 ⊗ t11 + 2t1 ⊗ t6 + t1 ⊗ t7 + t21 ⊗ t3 + 2t21 ⊗ t4 + t31 ⊗ t2 + t3 ⊗ t2 + t1 t3 ⊗ t1 + t11 ⊗ ∅
 5 t12 1 ⊗ t12 + t1 ⊗ t6 + t1 ⊗ t8 + t21 ⊗ t4 + t2 ⊗ t3 + t1 t2 ⊗ t2 + t4 ⊗ t2 + t1 t4 ⊗ t1 + t12 ⊗ ∅
 5 t13 1 ⊗ t13 + 2t1 ⊗ t6 + t21 ⊗ t3 + 2t2 ⊗ t4 + 2t1 t2 ⊗ t2 + t22 ⊗ t1 + t13 ⊗ ∅
 5 t14 1 ⊗ t14 + 3t1 ⊗ t7 + 3t21 ⊗ t4 + t31 ⊗ t2 + t5 ⊗ t1 + t14 ⊗ ∅
 5 t15 1 ⊗ t15 + t1 ⊗ t7 + t1 ⊗ t8 + t21 ⊗ t4 + t2 ⊗ t4 + t1 t2 ⊗ t2 + t6 ⊗ t1 + t15 ⊗ ∅
 5 t16 1 ⊗ t16 + 2t1 ⊗ t8 + t21 ⊗ t4 + t3 ⊗ t2 + t7 ⊗ t1 + t16 ⊗ ∅
 5 t17 1 ⊗ t17 + t1 ⊗ t8 + t2 ⊗ t4 + t4 ⊗ t2 + t8 ⊗ t1 + t17 ⊗ ∅




 Theorem 2.8C Any tree t ≥ t  can be written in the form

                                t = (τm1  f1 )(τm2  f2 ) · · · (τmk  fn ),

 where f 1 , f 2 , . . . , f k ∈ F.

For example, consider

                                       4                                4
                     
                    t =           2                  ≤            2               = t.           (2.8 b)
                                           3                                3
                                      1                               1
We have
                                      t = (τ2 ∗ τ)(τ ∗ τ1 τ)(τ1 ∗ τ)(τ ∗ τ 2 ),
                                      t  = τ2 ττ1 τ,                                            (2.8 c)
                            f 1 f 2 f 3 f 4 = τ 4 τ1 τ =      .                                  (2.8 d)
84                                                                                               2 Trees and forests

Note that in (2.8 b), the labelled vertices in t  and t, correspond to the the four factors
τ2 , τ, τ1 , τ in (2.8 c). To convert t  to t, additional trees are attached as follows:
to label 1: an additional tree τ,
to label 2: an additional tree τ1 τ,
to label 3: an additional tree τ,
to label 4: two additional trees, each of them τ.
Combining these additional trees together, we obtain (2.8 d) as the forest

                     t t = (τ)(τ1 τ)(τ 2 )(τ) = ττ1 ττ 2 τ = τ 4 τ1 τ.

Starting with the example case we have already considered, the possible choices of
f1 , f 2 , f 3 , f 4 , to convert t  to t, are

                       f1 = ,          f2 = ,               f3 = ,                f4 =       ,
                       f1 = ,          f2 = ,               f3 =         ,        f 4 = 1,
                       f1 = ,          f2 =           ,     f3 = 1,               f4 = ,

                       f1 =       ,    f 2 = 1,             f 3 = 1,              f4 = ,

                       f1 = ,          f 2 = 1,             f3 = ,                f4 =       ,

                       f1 = ,          f 2 = 1,             f3 =         ,        f 4 = 1.

     Using the numbered tree notation, we have

                   t t  = t41 t2 + t1 t2 t3 + t31 t3 + t1 t11 + t1 t2 t4 + t3 t4 .
   Subtrees, supertrees and prunings have their principal applications in Chapter
3,Section 3.9 (p. 133).


Recursions

The two tree-building mechanisms that have been introduced:

                                 t 1 , t 2 , . . . , t n → [t 1 t2 · · · t n ],
                                            t1 , t2 → t1 ∗ t2 ,

have been used to deﬁne and evaluate a number of functions recursively. We will
now apply this approach to Δ .


Recursion for Δ using the beta-product
For a given tree t, Δ (t) is a combination of terms of the form f ⊗ t with the proviso
that for the term for which t = ∅, f = t. We need to extend the meaning of the
beta-product to the individual terms of these types and extend the meaning further by
treating the product as a bi-linear operator.
2.8 Subtrees, supertrees and prunings                                                                   85



 Theorem 2.8D Given trees t1 , t2 ,

                                       Δ (t 1 ∗ t2 ) = Δ (t 1 ) ∗ Δ (t2 ),

 where the product on the right is treated bi-linearly and speciﬁc term by term
 products are evaluated according to the rules

                             (f1 ⊗ t1 ) ∗ (f2 ⊗ t2 ) = (f 1 f2 ) ⊗ (t1 ∗ t2 ),
                             (t 1 ⊗ ∅) ∗ (f2 ⊗ t2 ) = 0,
                             (f 1 ⊗ t1 ) ∗ (t2 ⊗ ∅) = (f 1 t2 ) ⊗ t1 ,
                             (t 1 ⊗ ∅) ∗ (t2 ⊗ ∅) = (t 1 ∗ t2 ) ⊗ ∅.

Proof. If t = t 1 ⊗ t2 , then the prunings of t consist of terms of the form

                                       (t 1 t 1 )(t2 t 2 ) ⊗ (t1 ∗ t2 ),

corresponding to t1 ≤ t1 , t 2 ≤ t2 , where t1 > ∅, together with (t1 ∗ t2 ) ⊗ ∅, corre-
sponding to t = ∅. Now calculate

          Δ (t 1 ∗ t2 ) =        ∑       ∑ (t 1 t 1 )(t2 t 2 ) ⊗ (t1 ∗ t2 ) + (t1 ∗ t2 ) ⊗ ∅
                                         
                            ∅<t 1 ≤t 1 t 2 ≤t 2

and compare with
                                                                                                 
      Δ (t 1 ) ∗ Δ (t2 ) =          ∑ (t1 t1 ) ⊗ t1 + t1 ⊗ ∅ ⊗ ∑ (t2 t2 ) ⊗ t2
                                 ∅<t 1 ≤t 1                                        t 2 ≤t 1

                       =          ∑         ∑ (t1 t1 )(t2 t2 ) ⊗ (t1 ∗ t2 ) + (t1 ∗ t2 ) ⊗ ∅
                             ∅<t 1 ≤t 1 t 2 ≤t 2

                       = Δ (t1 ∗ t2 ).

Examples of the beta-product recursion for Δ
To see how this works we will verify the formula

                                          Δ (t1 ) ∗ Δ (t2 ) = Δ (t4 ).

We have
                                                            
 Δ (t1 )∗Δ (t2 ) = 1 ⊗ t1 + t1 ⊗ ∅ ∗ 1 ⊗ t2 + t1 ⊗ t1 + t2 ⊗ ∅
                 = (1 ⊗ t1 ) ∗ (1 ⊗ t2 ) + (1 ⊗ t1 ) ∗ (t1 ⊗ t1 ) + (1 ⊗ t1 ) ∗ (t2 ⊗ ∅)
                     + (t1 ⊗ ∅) ∗ (1 ⊗ t2 ) + (t1 ⊗ ∅) ∗ (t1 ⊗ t1 ) + (t1 ⊗ ∅) ∗ (t2 ⊗ ∅)
                 = 1 ⊗ (t1 ∗t2 )+t1 ⊗ (t1 ∗t1 )+t2 ⊗ (t1 ∗∅)+(0)+(0)+(t1 ∗t2 ) ⊗ ∅
                 = 1 ⊗ t4 + t1 ⊗ t2 + t2 ⊗ t1 + t4 ⊗ ∅
                 = Δ (t4 ).
86                                                                            2 Trees and forests

As a second example, we will verify the formula

                                       Δ (t2 ) ∗ Δ (t1 ) = Δ (t3 ).

We have
                                                           
Δ (t2 )∗Δ (t1 ) = 1 ⊗ t2 + t1 ⊗ t1 + t2 ⊗ ∅ ∗ 1 ⊗ t1 + t1 ⊗ ∅
                = (1 ⊗ t2 ) ∗ (1 ⊗ t1 ) + (1 ⊗ t2 ) ∗ (t1 ⊗ ∅) + (t1 ⊗ t1 ) ∗ (1 ⊗ t1 )
                    + (t1 ⊗ t1 ) ∗ (t1 ⊗ ∅) + (t2 ⊗ ∅) ∗ (1 ⊗ t1 ) + (t2 ⊗ ∅) ∗ (t1 ⊗ ∅)
                = 1 ⊗ (t2 ∗t1 )+t1 ⊗(t2 ∗∅)+t1 ⊗(t1 ∗t1 )+t21 ⊗(t1 ∗∅)+0+(t2 ∗t1 )⊗∅
                = 1 ⊗ t3 + 2t1 ⊗ t2 +t21 ⊗ t1 +t3 ⊗ ∅
                = Δ (t3 ).



Recursion for Δ using the B+ operation


 Theorem 2.8E Let t = [t 1 t 2 · · · t m ], then
                                                 m
                                    Δ (t) =     ∏ Δ (ti ) + t ⊗ ∅,
                                                i=1

 where the expansion of the product ∏m
                                     i=1 and the term-by-term evaluation of [·]
 are according to the rules

                             (f1 ⊗ f2 )(f3 ⊗ f4 ) = (f 1 f3 ) ⊗ (t2 ∗ t4 ),
                             (t1 ⊗ ∅)(f2 ⊗ f3 ) = (f 1 f2 ) ⊗ f3 ,
                             (f2 ⊗ f3 )(t1 ⊗ ∅) = (f1 f2 ) ⊗ f3 ,
                             (f1 ⊗ ∅)(f2 ⊗ ∅) = (f1 f2 ) ⊗ ∅,
                                       [(f1 ⊗ f2 )] = f 1 [f 2 ],
                                       [(f 1 ⊗ ∅)] = f 1 .

Proof. The proof is by induction on the integer m. We will verify the result for the
two cases
 (i)   t = [t 1 ],
(ii)   t = [(t1 t 2 · · · t m−1 )(tm )].
For case (i), we have, using Theorem 2.8D,

                                Δ ([t1 ]) = Δ (τ ∗ t1 )
                                           = Δ (τ) ∗ Δ (t1 )
                                           = (1 ⊗ τ + τ ⊗ ∅) ∗ Δ (t1 )
                                           = [Δ (t 1 )] + [t1 ] ⊗ ∅.
2.8 Subtrees, supertrees and prunings                                                  87

For case (ii),
                                       m−1 
                      Δ (t) = Δ (τ) ∗ Δ ∏ (ti ) ∗ Δ tm
                                                i=1
                                 m−1                                        
                            =       ∏ Δ ([ti ]) + ([t1 t2 · · · tm−1 ] ⊗ ∅ ∗ Δ (tm )
                                     i=1
                                 m
                            = ∏ Δ ([ti ]) + [t1 t 2 · · · t m ] ⊗ ∅.
                                i=1



Two examples

The ﬁrst example is to evaluate Δ (t4 ) = Δ ([t2 ]) and we see that m = 1. We have
                                3
                               ∑ f j ⊗ f j = 1 ⊗ t2 + t1 ⊗ t1 + t2 ⊗ ∅,
                              j=1

and in accordance with Theorem 2.8E,

                           Δ (t4 ) = 1 ⊗ t4 + t1 ⊗ t2 + t2 ⊗ t1 + t4 ⊗ ∅.

The second example is to evaluate Δ (t3 ) = Δ ([t21 ]) and we have
                 3
                ∑ f j ⊗ f j = (1 ⊗ t1 + t1 ⊗ ∅)2 = 1 ⊗ t21 + 2t1 ⊗ t1 + t21 ⊗ ∅,
                j=1

leading to
                          Δ (t3 ) = 1 ⊗ t3 + 2t1 ⊗ t2 + t21 ⊗ t1 + t3 ⊗ ∅.

Exercise 28 Let t n = [τ n ], n = 0, 1, 2, . . . Show that
                                               n 
                                                     n  n−i
                                     Δ (t n ) = ∑       τ ⊗ t i + t n ⊗ ∅,
                                              i=0    i

where, conventionally, τ 0 = 1.


Exercise 29 Let t 0 = τ, t n = [n τ]n , n = 1, 2, . . . Find the value of Δ (t n ).



Reversing the roles of subtrees and supertrees

The relation t ≤ t has been introduced as deﬁning the subtrees of t but it can also be
used to deﬁne the supertrees of t . Looked at this way, t t  becomes the forest of
trees that need to be grafted to t to construct t.
88                                                                                 2 Trees and forests

                                                                   
   Corresponding to the term (t t ) ⊗ t in Δ (t), we will use t ⊗(t   t ) as a term
      
in Δ (t ) to represent this same relationship. However, this needs to be modiﬁed by
inserting scale factors related to the symmetry of the various trees.

 Deﬁnition 2.8F
                                                      
                                                σ (t ) 
                                   Δ(t  ) = ∑ σ (t) t⊗(t t ).
                                             t≥t 


   Because Δ(t  ) is an inﬁnite series, it will not be possible to give complete infor-
mation in the form of examples. However, four cases will be given and these will be
expanded as far as terms with |t| ≤ 5.

     Δ(t1 ) = t1 ⊗1
                   + t2 ⊗t
                          1 + 12 t3 ⊗t
                                      21 + t4 ⊗t
                                                2 + 16 t5 ⊗t
                                                            31 + t6 ⊗t
                                                                      1 t2 + t7 ⊗t
                                                                                  3 + t8 ⊗t
                                                                                           4
                 1
              + 24     41 + 12 t10 ⊗t
                   t9 ⊗t             21 t2 + 12 t11 ⊗t
                                                      1 t2 + t12 ⊗t
                                                                   1 t4 + 12 t13 ⊗t
                                                                                   22 + 16 t14 ⊗t
                                                                                                 5
                     6 + 12 t16 ⊗t
              + t15 ⊗t             7 + t17 ⊗t
                                              8,
     Δ(t2 ) = t2 ⊗1
                   + t3 ⊗t
                          1 + t4 ⊗t
                                   1 + 12 t5 ⊗t
                                               21 + t6 ⊗(t
                                                         21 + t2 ) + 12 t7 ⊗t
                                                                             21 + t8 ⊗t
                                                                                       2
                       31 + t10 ⊗(
              + 16 t9 ⊗t          12 t31 + t1 t2 ) + 12 (t11 ⊗(t
                                                               31 + t3 ) + t12 ⊗(t
                                                                                 1 t2 + t4 )
                     1 t2 + 16 t14 ⊗t
              + t13 ⊗t               31 + t15 ⊗t
                                                1 t2 + 12 t16 ⊗t
                                                                3 + t17 ⊗t
                                                                          4,
     Δ(t3 ) = t3 ⊗1
                   + t5 ⊗t
                          1 + 2t6 ⊗t
                                    1 + 21 t9 ⊗t
                                                21 + t10 ⊗(2t
                                                           21 + t2 ) + t11 ⊗t
                                                                             21 + 2t12 ⊗t
                                                                                         2
                     21 ,
              + t13 ⊗t
     Δ(t4 ) = t4 ⊗1
                   + t6 ⊗t
                          1 + t7 ⊗t
                                   1 + t8 ⊗t
                                            1 + 12 t10 ⊗t
                                                         21 + t11 ⊗t
                                                                    21 + t12 ⊗t
                                                                               21
                     2 + 12 t14 ⊗t
              + t13 ⊗t            21 + t15 ⊗(t
                                             21 + t2 ) + 12 t16 ⊗t
                                                                  21 + t17 ⊗t
                                                                             2.


2.9 Antipodes of trees and forests

The name “antipode” is adopted from Hopf-algebra terminology. In the present
context, the antipode is a particular mapping antipode : F → F with the properties of
an involution. That is antipode ◦ antipode = id.
   We will construct antipode in three steps. First, antipode : T → F will be deﬁned
and this will then be extended to antipode : F → F and then to antipode : F → F.

The antipode of a tree


 Deﬁnition 2.9A A partition of a tree t = (V, E, r) is the forest formed from the
 set of trees induced by a partition of V . The set of all partitions is denoted by P(t).
 For f = t 1 t2 · · · t n ∈ P(t), the corresponding signed product is
                                                      n
                                            (−1)n ∏ t i .                                       (2.9 a)
                                                     i=1
2.9 Antipodes of trees and forests                                                                               89


                                  Table 10 antipode(t), for |t| ≤ 5

    |t|          t          antipode(t)
     1          t1          −t1
     2          t2            t21 − t2
     3          t3          −t31 + 2t1 t2 − t3
     3          t4          −t31 + 2t1 t2 − t4
     4          t5            t41 − 3t21 t2 + 3t1 t3 − t5
     4          t6            t41 − 3t21 t2 + t1 t3 + t1 t4 + t22 − t6
     4          t7            t41 − 3t21 t2 + t1 t3 + 2t1 t4 − t7
     4          t8            t41 − 3t21 t2 + 2t1 t4 + t22 − t8
     5          t9          −t51 + 4t31 t2 − 6t21 t3 + 4t1 t5 − t9
     5          t10         −t51 + 4t31 t2 − 3t21 t3 − t21 t4 − 2t1 t22 + t1 t5 + 2t1 t6 + t2 t3 − t10
     5          t11         −t51 + 4t31 t2 − 2t21 t3 − 2t21 t4 − 2t1 t22 + 2t1 t6 + t1 t7 + t2 t3 − t11
     5          t12         −t51 + 4t31 t2 − t21 t3 − 2t21 t4 − 3t1 t22 + 2t1 t5 + t1 t7 + t2 t3 + t2 t4 − t12
     5          t13         −t51 + 4t31 t2 − t21 t3 − 2t21 t4 − 3t1 t22 + 2t1 t6 + 2t2 t4 − t13
     5          t14         −t51 + 4t31 t2 − 3t21 t3 − 3t21 t4 + t1 t5 + 3t1 t7 − t14
     5          t15         −t51 + 4t31 t2 − t21 t3 − 2t21 t4 − 3t1 t22 + t1 t6 + t1 t8 + t2 t3 + t2 t4 − t15
     5          t16         −t51 + 4t31 t2 − t21 t3 − 3t21 t4 − 2t1 t22 + t1 t7 + 2t1 t8 + t2 t3 − t16
     5          t17         −t51 + 4t31 t2 − 3t21 t4 − 3t1 t22 + 2t1 t8 + 2t2 t4 − t17




 Deﬁnition 2.9B The antipode of a tree t is the sum of all the signed partitions of
 t, written antipode(t).

   For example, the partitions of t6 are found from the following diagrams, where
thin lines show where edges have been removed to reveal each partition:




This leads to
                      antipode(t6 ) = t41 − 3t21 t2 + t1 t3 + t1 t4 + t22 − t6 .
For later reference, antipode(t), for |t| ≤ 5, is given in Table 10.
90                                                                                         2 Trees and forests

The antipode of forests



 Deﬁnition 2.9C The antipode of a forest is the formal product of the antipodes of
 the trees which comprise the forest. That is,
                                        n                  n
                           antipode         ∏ ti = ∏ antipode(ti ).
                                            i=1             i=1

 The antipode of a linear combination of forests is the corresponding linear combi-
 nation of the antipodes of the individual forests. That is
                                       n                   n
                        antipode       ∑ Ci fi = ∑ Ci antipode(fi ).
                                       i=1                  i=1




 Theorem 2.9D The antipode of a forest can also be deﬁned in the same way as
 the antipode of a tree, based on the partitions of the given forest.




 Theorem 2.9E The antipode of t ∈ T satisﬁes the recursion:

                        antipode(τ) = −τ,                                                            (2.9 b)
                                                                           
                        antipode(t) = − ∑ antipode(t t )t
                                                  t  ≤t
                                                                                                     (2.9 c)
                                        = − ∑ antipode(t t  )t  − t.
                                                  t  <t


Proof. The case (2.9 b) follows because there is only a single partition for τ. For
(2.9 c) collect all partitions for which t  is the component containing the root. For
each of the partitions of t t , (−t  ) is the additional factor to construct the signed
product of (t t  )t  .


Exercise 30 Show that
                                                           n−1    n
                antipode([τ n ]) = (−1)n+1 τ n+1 − ∑ (−1)n−i               [τ i ]τ n−i − [τ n ].
                                                           i=1     i



Exercise 31 Find antipode([3 τ]3 ).
2.9 Antipodes of trees and forests                                                                          91

The involution and other antipode properties



 Lemma 2.9F For any t ∈ T,

                      ∑ antipode(t t )t = ∑ (t t ) antipode(t  ).                                 (2.9 d)
                      
                     t <t                                      t <t


Proof. The proof is by induction on n = |t|. For n = 1, t = τ and the two sides of
(2.9 d) both vanish. For n > 1, use (2.9 c) from Theorem 2.9E in the form

                          antipode(t  ) = − ∑ antipode(t t )t .
                                                          t  ≤t

Substitute this formula into the right-hand side of (2.9 d) and replace the variable t 
by t on the left hand side. The result is

              ∑  antipode(t t  )t = − ∑ (t t  ) ∑ antipode(t t )t 
              
             t <t                                    t <t               t ≤t

and we will prove that for t < t,

                 antipode(t t  ) = −                    ∑            (t t  ) antipode(t  t ),
                                                     t : t ≤t <t

which can be written

                          ∑            (t t ) (t t ) antipode(t  t ) = 0.                      (2.9 e)
                     t : t ≤t ≤t

Write t t = ∏ni=1 ti and t t  = ∏ni=1 t i , where t i ≤ t i , so that (2.9 e) becomes
                              n                                                   
                            ∏ ti + ∑ (t i ti ) antipode(ti ) = 0,
                            i=1            ti ≤ti

and this follows from (2.9 d) with t replaced by t i , where we note that |ti | < n.




 Theorem 2.9G The antipode is an involution. That is

                                         antipode ◦ antipode = id.

Proof. The case antipode(antipode(t)) = t follows from Lemma 2.9F. This extends
to a linear combination of forests.
92                                                                                 2 Trees and forests

Recursions for antipodes

Beta product recursion

The signed product (2.9 a), used in Deﬁnition 2.9A, can be written in the form
                                 n                  n
                                ∏(−ti ) = − ∏(−ti )t1 ,
                                i=1                i=2

where the unique component of the partition which contains the root is designated as
number 1.
  Hence, the sum of the signed products can be written as

                                 θ1 (t)t1 + θ2 (t)t2 + · · · ,

where θ1 , θ2 , . . . , are formed from the factors in the various signed partitions which
do contain the root.
   For example,

               antipode(t1 ) = (−1)t1                                     = θ (t1 )T T,
               antipode(t2 ) = (t1 )t1 + (−1)t2                           = θ (t2 )T T,
               antipode(t3 ) = (−t21 )t1 + (2t1 )t2 + (−1)t3 = θ (t3 )T T,
               antipode(t3 ) = (t2 − t21 )t1 + (t1 )t2 + (−1)t4 = θ (t4 )T T,


where T is the list of all trees, written as an inﬁnite-dimensional vector
                                                                      T
                               T=       t1    t2    t3      ···

and, for each t, θ (t)T is a terminating row vector,

                        θ (t1 )T = −1         ,

                        θ (t2 )T =      t1 −1           ,

                        θ (t3 )T = −t21           2t1 −1          ,

                        θ (t4 )T =     t2 − t21     t1       0 −1          ,

and in the evaluation of θ (t)T T, only non-zero terms are included.
   As an alternative to this formulation, we introduce a variant of Sweedler’s notation
[89] (Underwood, 2011), where we deﬁne

                                      ∇(t) = θ (t)T ⊗ T,
2.9 Antipodes of trees and forests                                                   93

with “⊗” inserted between the two factors indicates how the inner product is to be
interpreted.
   As examples we have

                         ∇(t1 ) = −1 ⊗ t1 ,
                         ∇(t2 ) = t1 ⊗ t1 − 1 ⊗ t2 ,
                         ∇(t3 ) = −t21 ⊗ t1 + 2t1 ⊗ t2 − 1 ⊗ t3 ,
                         ∇(t4 ) = (t2 − t21 ) ⊗ t1 + t1 ⊗ t2 − 1 ⊗ t4 .

   We will use the notation expand(∇(t)) to denote θ (t)T T, which is just ∇(t), with
every occurrence of ⊗ replaced by multiplication in the forest ring. Recursion under
the beta product satisﬁes the following properties:

 Theorem 2.9H The values of ∇(t) and antipode(t) satisfy the recursions

                            ∇(τ) = −1 ⊗ τ,                                     (2.9 f)
                      antipode(τ) = −τ,                                       (2.9 g)
                          ∇(t ∗ t  ) = antipode(t  )∇(t) − ∇(t) ∗ ∇(t ),   (2.9 h)
                   antipode(t ∗ t ) = expand(∇(t ∗ t  )).                   (2.9 i)

Proof. The results of (2.9 f), (2.9 g), (2.9 i) follow from the above discussions. To
prove (2.9 h), we see that antipode(t )∇(t) includes all partitions for which the
component containing the root does not contain any vertex from t , while −∇(t) ∗
∇(t ) contains partitions for which the component containing the root also contains
one or more vertex from t .

B+ recursion for antipodes
Let
                                           t = [t 1 t 2 · · · t n ].            (2.9 j)
Our aim is to write ∇(t) in terms of ∇(t1 ), ∇(t2 ), . . . , ∇(tn ). This will give an
alternative to Theorem 2.9H to evaluate ∇(t) and therefore antipode(t) for any tree.

 Theorem 2.9I If t is given by (2.9 j), then
                                      n                               
                        ∇(t) =       ∏ antipode(ti ) ⊗ ∅ − ∇(ti ) .
                                     i=1


Proof. The terms in ∇(t) are of the form
                                                        n
                                               C⊗     ∏ ti ,
                                                      i=1
94                                                                              2 Trees and forests

where the numbering of the factors is the same as in (2.9 j) but with the possibility that
some factors are missing. The factors that are present come from the corresponding
∇(ti ) with ti ≤ t i , and the missing factors come from antipode(ti ) ⊗ ∅.

Multiplicative functions on trees and forests
At this point we introduce a formal deﬁnition on mappings from F → R.

 Deﬁnition 2.9J A function on a : F → R is multiplicative if

                           a(1) = 1,
                          a(ft) = a(f)a(t),                     f ∈ F, t ∈ T.

Another way of looking at this property is:
                                       m                m
                                  a    ∏ ti = ∏ a(ti ),                                    (2.9 k)
                                       i=1                i=1

with the understanding that the forest ∏mi=1 t i is the identity forest if the product is
empty.
   If a : T → R, then we will write a : F → R as the multiplicative extension of a
deﬁned by (2.9 k).

Subtrees as mappings

As an alternative to the use of the Sweedler notation, it is convenient to use functions
on trees and related graphs to characterize speciﬁc tree prunings. If t ≤ t, then the
corresponding term f ⊗ t can be written as

                                            a(f)b(t ),

where a and b are unspeciﬁed mappings.
  We will formally write

                             (ab)(t) =         ∑ a(t t )b(t ),
                                            T #  t ≤t

where a is multiplicative.
  For example,

              (ab)(∅) = b(∅),
              (ab)(t1 ) = a(t1 )b(∅) + b(t1 ),
              (ab)(t2 ) = a(t2 )b(∅) + a(t1 )b(t1 ) + b(t2 ),
              (ab)(t3 ) = a(t3 )b(∅) + a(t1 )2 b(t1 ) + 2a(t1 )(t2 ) + b(t3 ),
              (ab)(t4 ) = a(t4 )b(∅) + a(t2 )b(t1 ) + a(t1 )(t2 ) + b(t4 ).
2.9 Antipodes of trees and forests                                                    95

Symbolic and algebraic interpretations

As a symbolic statement, a formula for (ab)(t) is nothing more than an alternative
to the Sweedler notation and expresses exactly the same information on the set of
all possible prunings of t. However, if two speciﬁc mappings, a, b, from T to R are
given then (ab)(t) is the deﬁnition of a new mapping constructed from a and b.


Linear algebra representations

A matrix group over F

Consider the set G of inﬁnite matrices with components of the form g ∈ G, where
                              ⎧
                              ⎪
                              ⎪ a member of F,     j < i,
                              ⎨
                        gi j = 1,                  j = i,
                              ⎪
                              ⎪
                              ⎩ 0,                 j > i.



 Theorem 2.9K G is a group.

In applications of G, it will be convenient to index the rows and columns using T
with the members of T, arranged in a convenient order, such as using the standard
numbering of trees.


Symbolic vector and matrix

Let v denote the symbolic vector [ ∅ t1 t2 · · · ]T , and Λ the symbolic matrix
                                     ⎡                             ⎤
                                          1
                                   ⎢                               ⎥
                                   ⎢ t1        1                   ⎥
                                   ⎢                               ⎥
                                Λ =⎢ t                             ⎥,
                                   ⎢ 2         t1    1             ⎥
                                   ⎣                               ⎦
                                      ..        ..   ..   ..
                                       .         .    .        .

in which the (t, t  ) element is the sum of the forests ∑ f i over all terms of the form
fi ⊗ t occurring in Δ (t). For example, from

                        Δ (t3 ) = t 3 ⊗ ∅ + t21 ⊗ t1 + 2t1 t 2 + 1 ⊗ t3 ,

it follows that row number 4 of Λ is
                                                             
                               t 3 t21        2t 1   1    ··· .
96                                                                           2 Trees and forests

If the product Λ ⊗ v is interpreted as the usual matrix-vector multiplication, but with
⊗ inserted within every term that arises, then we can write

                                           Λ ⊗ v = Δ (v).

Furthermore, many results arising in Sections 2.8 and 2.9 have a fresh and useful
interpretation when written in terms of v and Λ .


 Theorem 2.9L
                                                Λ ) = Λ −1 .
                                       antipode(Λ



Proof. This follows from Theorem 2.9E.



                                                            Λ ) have been calculated up
    To illustrate this result, the values of Λ and antipode(Λ
to trees of order 4. A simple matrix multiplication conﬁrms that Λ antipode(Λ   Λ ) = I.
The matrices are
               ⎡                                                        ⎤
                   1
              ⎢    t1   1                                               ⎥
              ⎢                                                         ⎥
              ⎢                                                         ⎥
              ⎢    t2   t1      1                                       ⎥
              ⎢                                                         ⎥
              ⎢                                                         ⎥
              ⎢    t3   t21     2t1         1                           ⎥
              ⎢                                                         ⎥
              ⎢                                                         ⎥
           Λ =⎢
              ⎢
                   t4   t2      t1          0     1                     ⎥,
                                                                        ⎥
              ⎢         t31     3t21                                    ⎥
              ⎢    t5                       3t1   0     1               ⎥
              ⎢                                                         ⎥
              ⎢                 t21 + t2                                ⎥
              ⎢    t6   t1 t2               t1    t1    0   1           ⎥
              ⎢                                                         ⎥
              ⎢                 t21                                     ⎥
              ⎣    t7   t3                  0     2t1   0   0   1       ⎦
                   t8   t4      t2          0     t1    0   0   0   1
               ⎡                                                                           ⎤
               1
             ⎢−t                                                                           ⎥
             ⎢ 1                                1                                          ⎥
             ⎢                                                                             ⎥
             ⎢ t2 −t                           −t                                          ⎥
             ⎢ 1 2                                1            1                           ⎥
             ⎢                                                                             ⎥
             ⎢−t3 +2t t −t                      t21           −2t1                         ⎥
             ⎢ 1      1 2    3                                          1                  ⎥
             ⎢                                                                             ⎥
             ⎢−t3 +2t t −t                      t21 −t2       −t1                          ⎥
          Λ)=⎢
 antipode(Λ  ⎢ 1
                      1 2    4                                          0 1                ⎥.
                                                                                           ⎥
             ⎢ 4                                                                           ⎥
             ⎢ t1 −3t21 t2 +3t1 t3 −t5         −t31            3t21    −3t1 0    1         ⎥
             ⎢                                                                             ⎥
             ⎢ 4                                                                           ⎥
             ⎢ t1 −3t1 t2 +t1 t3 +t1 t4 +t2 −t6 t1 t2 −t1
                      2                   2             3      2t1 −t2 −t1 −t1
                                                                  2              0 1       ⎥
             ⎢                                                                             ⎥
             ⎢ 4                                                                           ⎥
             ⎢ t1 −3t1 t2 +t1 t3 +2t1 t4 −t7 −t1 +2t1 t2 −t3 t21
                      2                           3                     0 −2t1   0 0 1     ⎥
             ⎣                                                                             ⎦
               t1 −3t1 t2 +2t1 t4 +t2 −t8
                 4    2              2         −t1 +2t1 t2 −t4 t1 −t2 0 −t1
                                                  3             2                0 0 0 1
2.9 Antipodes of trees and forests                                                                          97

Summary of Chapter 2 and the way forward

Summary

The basic terminology of graphs, as vertex-edge pairs is presented, leading to the
deﬁnitions of trees, in the sense of rooted trees, and unrooted, or free, trees. It is shown
how to build up trees from the tree τ, with a single vertex, using the beta-product
t ∗ t  and the B+ operation [t 1 t2 · · · t n ]. The preﬁx (Polish) operator τn (n = 1, 2, . . . )
is introduced as denoting τn f = [f], where n = f .
    Formal linear combinations of forests are introduced as the “forest space” and,
as an application, the enumeration of trees is considered. Partitions of sets and num-
bers are introduced and related to trees. The evolution of partitions and of trees, is
discussed as the complexity is increased step by step. Subtrees and prunings are
introduced and the function Δ (t) is introduced and its properties are reviewed. The
antipode is introduced and its properties discussed, including the involution property.
The interplay between groups, linear algebra and trees is brieﬂy investigated. Trun-
cated trees and forests are introduced. This leads to the algebra of linear combinations
of uni-valent stumps which act as linear operators on the tree space.

The way forward

Trees, and related graph-theoretic structures, are at the heart of this subject and a
good understanding of the present chapter is desirable before venturing too far further
ahead.

Teaching and study notes

The subject of this chapter is an essential component of any study of numerical
methods for ordinary differential equations. In particular it is a prelude to a serious
study of B-series. Many students of this subject will already have an acquaintance
with graph-theory and combinatorics and this will provide a useful background.
However, the presentation in this chapter should not be taken lightly because of the
distinctive notations and conventions introduced here; later chapters are built on this
and related terminology. Rooted trees are given a standard numbering, displayed
in Table 6 (p. 63), and this will be used throughout the book. The most important
constructions and results, which come as the culmination of earlier work, are in
Sections 2.8 and 2.9. These are essential background for the algebraic structures
introduced in Chapter 3.
Projects
Project 5 Read up about the asymptotic behaviour of the sequences [a1 , a2 , . . . ], [b1 , b2 , . . . ],
[c1 , c2 , . . . ], in (2.4 b).
Project 6    Learn about Hopf Algebras starting with [4] and [89].
Project 7    Find out about the life of J. Łukasiewicz, the founder of Polish notation.
Project 8    Write your own implementation of Algorithm 3 (p. 64).
