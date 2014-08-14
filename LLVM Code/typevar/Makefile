EXECUTABLE=typevar

all: typevar.ll
	llvm-as typevar.ll
	llc typevar.bc
	clang -o $(EXECUTABLE) typevar.s
	./$(EXECUTABLE)

clean:
	rm typevar.bc
	rm typevar.s
	rm $(EXECUTABLE)