EXECUTABLE=mask

all: mask.ll
	llvm-as mask.ll
	llc mask.bc
	clang -o $(EXECUTABLE) mask.s
	./$(EXECUTABLE)

clean:
	rm mask.bc
	rm mask.s
	rm $(EXECUTABLE)