Simple factorial calculator to demonstrate a SML/NJ image.

Build the heap image:

```sml
$ ml-build main.cm Factorial.main factorial
```

Run the CLI:

```sml
$ echo 5 | sml @SMLload=factorial
120
```
