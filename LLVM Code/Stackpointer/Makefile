EXECUTABLE=sp

all: sp.ll
	llvm-as sp.ll
	llc sp.bc
	clang -o0 $(EXECUTABLE) sp.s
	./$(EXECUTABLE)

clean:
	rm sp.bc
	rm sp.s
	rm $(EXECUTABLE)