PL/0 JIT compiler
=================

A tiny [PL/0](https://en.wikipedia.org/wiki/PL/0) JIT compiler in less than 900 LOC with LLVM and PEG parser. It features runtime error handling for division by zero using C++ exception handling via LLVM IR, and supports nested procedures with free variable capture.

Library dependencies:

  * Parsing and AST: [cpp-peglib](https://github.com/yhirose/cpp-peglib)
  * JIT Compilation: [LLVM](https://llvm.org/) (ORC JIT / LLJIT)

Build on macOS
--------------

```sh
brew install llvm
export PATH="$PATH:$(brew --prefix llvm)/bin"
make
```

Usage
-----

```sh
> ./pl0 samples/square.pas
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

Use `-emit-llvm` to dump the generated LLVM IR:

```sh
> ./pl0 samples/square.pas -emit-llvm
```

Sample programs
---------------

### Square (`samples/square.pas`)

```pascal
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
```

### Nested procedures with free variables (`samples/nested-functions.pas`)

```pascal
VAR x;
  PROCEDURE func0;
    VAR x0;
    PROCEDURE func1;
      VAR x1;
    BEGIN
      x1 := 789;
      out x;
      out x0;
      out x1;
    END;
  BEGIN
    x0 := 456;
    CALL func1
  END;
BEGIN
  x := 123;
  CALL func0
END.
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
