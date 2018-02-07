bench: pl0
	@echo '*** Python ***'
	@echo `python --version`
	@echo `time python samples/fib.py > /dev/null`
	@echo '*** Ruby ***'
	@echo `ruby --version`
	@echo `time ruby samples/fib.rb > /dev/null`
	@echo '*** PL/0 ***'
	@echo `time ./pl0 samples/fib.pas > /dev/null`

pl0: pl0.cc ./vendor/cpp-peglib/peglib.h
	clang++ -std=c++14 -O3 -o pl0 `llvm-config --cxxflags --ldflags --system-libs --libs` -Ivendor/cpp-peglib pl0.cc
