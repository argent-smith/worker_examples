How to build/run the example
============================

First, install OPAM. On Mac, for example, the command would be:

``` shell
$ brew install opam
$ opam init
```

Second, go to the example dir. Then, run the following commands%

``` shell
$ opam switch create . 4.07.1 --deps-only
$ dune build
```

Third, cd to the `files` and run:

``` shell
$ make
```

Fourth, cd back and run the example:

``` shell
$ dune exec worker_parmap
```
