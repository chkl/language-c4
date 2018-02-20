compile: a.out

runner.ll: runner.c
	clang -S -emit-llvm runner.c

function.ll: function.c
	c4 --compile function.c
#clang -S -emit-llvm function.c

linkage.ll: runner.ll function.ll
	llvm-link -S -v -o linkage.ll runner.ll function.ll

a.out: linkage.ll
	llc -relocation-model pic linkage.ll
	clang linkage.s


clean:
	rm *.ll
	rm *.s


.PHONY: clean
