target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.9.0"

declare i32 @llvm.read_register.i32(i8*)
declare i64 @llvm.read_register.i64(metadata)
declare void @llvm.write_register.i32(metadata, i32)
declare void @llvm.write_register.i64(metadata, i64)

!0 = metadata !{metadata !"%rsp\00"}
!1 = metadata !{metadata !"esp\00"}
!2 = metadata !{metadata !"sp\00"}

@rsp = internal constant [3 x i8] c"RSP"


%int = type i32

%frame = type {[0 x i8]*, %frame*, {%int, [0 x %int (...)*]}*, {%int, [0 x %int]}*, %metaframe*}
%metaframe = type {{%int, [0 x %typedef]}*, {%int, [0 x %valuedef]}*}
%typedef = type {[0 x i8]*, %int, %int, %type*, i1}
%valuedef = type {[0 x i8]*, %type*}
%type = type {%int, %int, %int}
%closure = type {%int, {%int, [0 x %int]}*, i1}


define private %int @main(){
	ptrtoint i32 ()* @main to %int
	call i64 @llvm.read_register.i64(metadata !{metadata !"RSP\00"})
	ret %int 0
}
