bench: pl0
	@echo '*** Python ***'
	@echo `python3 --version`
	@echo `time python3 samples/fib.py > /dev/null`
	@echo '*** Ruby ***'
	@echo `ruby --version`
	@echo `time ruby samples/fib.rb > /dev/null`
	@echo '*** PL/0 ***'
	@echo `time ./pl0 samples/fib.pas > /dev/null`

pl0: pl0.cc ./vendor/cpp-peglib/peglib.h
	clang++ `llvm-config --cxxflags --ldflags --system-libs --libs` -std=c++17 -O3 -o pl0 -Ivendor/cpp-peglib pl0.cc
