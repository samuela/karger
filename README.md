# Karger's min-cut algorithm in OCaml
This is an implementation of Karger's min-cut algorithm implemented in OCaml. Check out the *.in files in this repo for some example graphs. You can run them with
```
$ ocaml karger.ml b0.in
starting...
Best cut so far: 14
Best cut so far: 8
...
```
The program will keep searching for min-cuts and print updates each time it finds a better one.

See https://en.wikipedia.org/wiki/Karger%27s_algorithm for more information.
