declare i32 @llvm.read_register.i32(metadata)
declare i64 @llvm.read_register.i64(metadata)
declare void @llvm.write_register.i32(metadata, i32)
declare void @llvm.write_register.i64(metadata, i64)
!0 = metadata !{metadata !"sp\00"}

%int = type i32

%frame = type {[0 x i8]*, %frame*, {%int, [0 x %int (...)*]}*, {%int, [0 x %int]}*, %metaframe*}
%metaframe = type {{%int, [0 x %typedef]}*, {%int, [0 x %valuedef]}*}
%typedef = type {[0 x i8]*, %int, %int, %type*, i1}
%valuedef = type {[0 x i8]*, %type*}
%type = type {%int, %int, %int}
%closure = type {%int, {%int, [0 x %int]}*, i1}


define private %int @main(){
	ptrtoint i32 ()* @main to %int
	ret %int 0
}