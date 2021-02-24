# Declarative Programming

Based on the Declarative Programming course of the Bachelor's program of Computer Science at Facultad de Ciencias, UNAM. The topics covered on this subject are different algorithms to learn a deep knowledge of Haskell using GHCI to execute all functions, for further information continue reading.

## Classwork 0 (Basic Haskell)

The purpose of this classwork is to learn Haskell's most powerful weapon, recursion, alongside with defined objects such as the Natural Numbers () and the operations defined on this group, addition, product and sucessor. Also includes topics such as list comprehension.

To use this module, you just have to load it (or reload it) to GHCi with

```
ghci Tarea0.hs
```

```
ghci
>:l Tarea0.hs
```

## Classwork 1 (Lambda calculus)

This classwork is to learn deep lambda calculus, the objective is to program Martelli-Montanari's Unification Algorithm, the Universal Lambda Calculus and the W algorithm for type inference.

Usage:

```
ghci Tarea1.hs
```

```
ghci
>:l Tarea1.hs
```

## Classwork 2 (SAT problem)

Main idea of this classwork is to try to solve the SAT problem using the DPLL algorithm (Davis–Putnam–Logemann–Loveland), for this there are several functions such as transforming a logic formula to its conjuctive normal form (CNF), which is not trivial.

Usage: 

```
ghci Tarea0.hs
```

```
ghci
>:l Tarea0.hs
```

And to check if a formula can be satisfiednot necesarily in CNF but only with conjunction, disyunction and negations of variables), then you have to use the ```data F``` which has the constructors ```Var At | Neg At | Conj F F | Disy F F``` to create your own formula, for instance (p ^ ¬q) v (¬t ^ q) would transform to `Conj (Disy (Var "p") (Neg "q")) (Disy (Neg "t") (Var "q"))`, then to use de DPLL algorithm, just execute

```
DPLL ( clausulas ( fnc (Conj (Disy (Var "p") (Neg "q")) (Disy (Neg "t") (Var "q")))))
```

## Classwork 3 (Arrays, Maybe, and Graphs)

On this classwork, the idea is to create arrays in haskell which basically is a well defined function that given an integer `i` returns an element of type `a`, which is exactly how arrays work, therefore we can create `data Array f n` where `f` is a function and `n` is the length of the array. An implementation of Binary Search and Heapsort on the `arreglos_clase.hs` script.

For the, yet to be discovered, `Maybe` monad, learn how it works and how to use it, basically this monad has two types of data `Just a | Nothing`, is very useful on certain cases because sometimes functions can return a `null` value, then `Nothing` is the equivalent to this, in contrast `Just a` returns a value of type `a` if computation was succesful, for instance.

As for the graphs, we can have and solve different famous graph problems in a recursive way, like detecting a Hamiltonean Cycle, this is made in a recursive and really naive search, is not optimized.

## Classwork 4 (Monads)

This classwork was my personal favourite, since it envolves deep knowledge in mathematics (group theory) and computer science. The main problems to solve here were to learn how to use `do` , how `Functor, Applicative` and `Monad` work to create your own monads (once that have been proven, of course). Learn how to use Continuations on Haskell with the Monad State using the Arrays of Classwork 3, implementation of Selecionsort using this monad, all of this on the file `Practica4.hs`.

The best comes last. The `Fractales.hs` contains a sequential and parallel implementations of the Mandelbrot Set using the library `Control.Strategies.Parallel`, returns a String with 0/1, and this is to be `Ctrl+C, Ctrl+V` on a file with extension `.ppm`, the usage of this particular function is not trivial, the correct way to use it is:

```
mandelbrot_set (plano n)
mandelbrot_set_parallel (plano n)
```

Where `n` is the number of points on the plain. Before the really large string int the `.ppm` file, you must add this lines:

```
P2
n n
<scale>
```

and then the long string result. Where `<scale>` is how large you want the fractal to be painted.

You must be sure to have installed the package `ghc-parallel-devel` (Check your documentation to find the correct package, this is for Fedora 31)
