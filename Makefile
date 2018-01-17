bench: pl0
	@echo -n '*** Python ***'
	@echo `time python samples/fib.py > /dev/null`
	@echo -n '*** Ruby ***'
	@echo `time ruby samples/fib.rb > /dev/null`
	@echo -n '*** PL/0 ***'
	@echo `time ./pl0 samples/fib.pas > /dev/null`

pl0: pl0.cc ./lib/peglib.h
	clang++ -std=c++14 -O3 -o pl0 `llvm-config --cxxflags --ldflags --system-libs --libs` -Ilib pl0.cc
