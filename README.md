# programming-in-sml-harper

Programming in Standard ML, by [Robert Harper](https://www.cs.cmu.edu/~rwh/).

To install the `sml` compiler, follow the instructions in 
https://www.smlnj.org/dist/working/110.99.2/index.html.

Hints:

```
$ wget http://smlnj.cs.uchicago.edu/dist/working/110.99.2/config.tgz
$ tar -xzf config.tgz
$ config/install.sh
```

Highly recommended that you use rlwrap to be able to navigate the REPL:

```
$ apt-get install rlwrap
```

Then, whenever using the REPL, prefix `sml` as such:

```
$ rlwrap sml
```

## Chapter 1

Code I wrote while reading the first chapter.

```
$ cd ch_1
$ sml ch_1.sml
```

## Counter

Code for a very simple counter to compare with similar OCaml version.

```
$ sml counter.sml
```

## Clausal Function Expressions

Two different ways of saying the same thing.

Avoid repeating the function name with a `rec` value:

```
val rec tokenize = fn nil => nil
                    | (#"+" :: cs) => (PlusSign :: tokenize cs)
                    | (#"." :: cs) => (TimesSign :: tokenize cs)
                    | _ => nil
```

Repeat the function name with a `fun`:

```
fun tokenize nil = nil
    | tokenize (#"+" :: cs) = (PlusSign :: tokenize cs)
    | tokenize (#"." :: cs) = (TimesSign :: tokenize cs)
    | _ => nil
```
