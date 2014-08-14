;declare i32 @llvm.read_register.i32(i8*)
;declare i64 @llvm.read_register.i64(metadata)
;declare void @llvm.write_register.i32(metadata, i32)
;declare void @llvm.write_register.i64(metadata, i64)
%int = type i64
@spm_sp = internal global i8 15




define %int @main(){
	ptrtoint %int ()* @main to %int
	;%result = alloca %int, %int 4
	;%r = load %int* %result
	;%t = add %int %r, 1
	;call void asm sideeffect "movl $0, %edi", "=r,r" (%int %r)
	;%X.1 = call i32 asm "bswap $0", "=r,r"(i32 5)
	%t1 = call i8* @malloc(%int 8)
	%t = load i8* %t1
	store i8 %t, i8* @spm_sp
	call void asm sideeffect "movq _spm_sp(%rip), %rsp", ""()

	ret %int 0
}

declare i8* @malloc(%int)
