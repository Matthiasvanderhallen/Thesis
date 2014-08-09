EXECUTABLE=test

all: test.ll
	llvm-as test.ll
	llc test.bc
	clang -o $(EXECUTABLE) test.s
	./$(EXECUTABLE)

clean:
	rm $(EXECUTABLE)
	rm test.s
	rm test.bc