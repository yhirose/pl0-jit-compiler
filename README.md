PL/0 JIT compiler
=================

A tiny [PL/0](https://en.wikipedia.org/wiki/PL/0) JIT compiler in less than 900 LOC with LLVM and PEG parser which handles 'Divide by Zero'.

Library dependencies:

  * Parse and AST build: [cpp-peglib](https://github.com/yhirose/cpp-peglib)
  * Code Generation: [LLVM 13.0.1](http://releases.llvm.org/13.0.1/docs/index.html)

Build on Mac OS
---------------

```sh
brew install llvm
export PATH="$PATH:/usr/local/opt/llvm/bin"
make
```

Usage
-----

```sh
> cat samples/square.pas
VAR x, squ;

PROCEDURE square;
BEGIN
   squ := x * x
END;

BEGIN
   x := 1;
   WHILE x <= 10 DO
   BEGIN
      CALL square;
      ! squ;
      x := x + 1
   END
END.

> pl0 samples/square.pas
1
4
9
16
25
36
49
64
81
100
```

Benchmark with Fibonacci number [0, 35)
---------------------------------------

```sh
> make bench
*** Python ***
real	0m8.367s
user	0m8.153s
sys	0m0.129s

*** Ruby ***
real	0m3.064s
user	0m2.916s
sys	0m0.097s

*** PL/0 ***
real	0m0.249s
user	0m0.234s
sys	0m0.008s
```

License
-------

[MIT](https://github.com/yhirose/pl0-jit-compiler/blob/master/LICENSE)
