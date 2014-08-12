declare i32 @llvm.read_register.i32(metadata)
declare i64 @llvm.read_register.i64(metadata)
declare void @llvm.write_register.i32(metadata, i32)
declare void @llvm.write_register.i64(metadata, i64)
!0 = metadata !{metadata !"sp\00"}

%int = type i32

define private %int @main(){
	call void @llvm.write_register.i32(metadata !0, %int 3)
	ret %int 0
}