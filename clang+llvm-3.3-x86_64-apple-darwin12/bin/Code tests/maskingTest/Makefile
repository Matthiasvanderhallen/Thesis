EXECUTABLE=mask

all: mask.ll
	llvm-as mask.ll
	lli mask.bc

clean:
	rm mask.bc