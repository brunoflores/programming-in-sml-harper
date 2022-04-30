# programming-in-sml-harper

Programming in Standard ML, by [Robert Harper](https://www.cs.cmu.edu/~rwh/).

To install the `sml` compiler, follow the instructions in 
https://www.smlnj.org/dist/working/110.99.2/index.html.

Hints:

```bash
$ wget http://smlnj.cs.uchicago.edu/dist/working/110.99.2/config.tgz
$ tar -xzf config.tgz
$ config/install.sh
```

Highly recommended that you use rlwrap to be able to navigate the REPL:

```bash
$ apt-get install rlwrap
```

Then, whenever using the REPL, prefix `sml` as such:

```bash
$ rlwrap sml
```

## Chapter 1

Code I wrote while reading the first chapter.

```bash
$ cd ch_1
$ sml ch_1.sml
```

## Counter

Code for a very simple counter to compare with similar OCaml version.

```bash
$ sml counter.sml
```

## Clausal Function Expressions

Two different ways of saying the same thing.

Avoid repeating the function name with a `rec` value:

```sml
val rec tokenize = fn nil => nil
                    | (#"+" :: cs) => (PlusSign :: tokenize cs)
                    | (#"." :: cs) => (TimesSign :: tokenize cs)
                    | _ => nil
```

Repeat the function name with a `fun`:

```sml
fun tokenize nil = nil
    | tokenize (#"+" :: cs) = (PlusSign :: tokenize cs)
    | tokenize (#"." :: cs) = (TimesSign :: tokenize cs)
    | tokenize _ = nil
```

Important: clauses are considered in the order written.
Here, `n` is guaranteed to be non-zero:

```sml
val recip : int -> int =
  fn 0 => 0 | n : int => 1 div n
```

Case analysis on the values of a heterogeneous type is performed
by application of a clausally-defined function. The notation

```sml
case exp
  of pat_1 => exp_1
   | ...
   | pat_n => exp_n
```

is short for the application

```sml
(fn pat_1 => exp_1
  | ...
  | pat_n => exp_n) exp
```

## Self-Reference, Recursion and Iteration

A recursive process:

```sml
val rec factorial : int -> int =
  fn 0 => 1 | n : int => n * factorial (n-1)
```

```sml
fun factorial 0 = 1
  | factorial (n : int) = n * factorial (n-1)
```

An iterative process (less space required):

```sml
fun helper (0, r : int) = r
  | helper (n : int, r : int) = helper (n-1, n*r)

fun factorial (n : int) = helper (n, 1)
```
