%int = type i64

%frame = type {[0 x i8]*, %frame*, i1, {%int, [0 x %int]}*, {%int, [0 x %int (%frame*, i8*)*]}*, {%int, [0 x %int]}*, %metaframe*}
%metaframe = type {{%int, [0 x %typedef]}*, {%int, [0 x %valuedef]}*} %typedef = type {[0 x i8]*, %int, %int, %type*, i1}
%valuedef = type {[0 x i8]*, %type*}
%type = type {%int, %int, %int}
%closure = type {%int, {%int, [0 x %int]}*, i1}

@.flist = private global {%int, [0 x %frame*]}* null

define %int @StructureEntryPoint(%int %findex, %int %index, ...){
    %flist.ptr = load {%int, [0 x %frame*]}** @.flist
    %flist = load {%int, [0 x %frame*]}* %flist.ptr
    %elems = extractvalue {%int, [0 x %frame*]} %flist, 0
    
    %check = icmp ult %int %findex, %elems
    br i1 %check, label %Continue, label %Error
    
    Continue:
    %frame.ptr2 = getelementptr {%int, [0 x %frame*]}* %flist.ptr, i32 0, i32 1, %int %index
    %frame.ptr = load %frame** %frame.ptr2
    %frame = load %frame* %frame.ptr
    %valuelist.ptr = extractvalue %frame %frame, 4
    %valuelist = load {%int, [0 x %int (%frame*, i8*)*]}* %valuelist.ptr
    %list.ptr = getelementptr {%int, [0 x %int (%frame*, i8*)*]}* %valuelist.ptr, i32 0, i32 1
    %elems2 = extractvalue {%int, [0 x %int (%frame*, i8*)*]} %valuelist, 0
    %check2 = icmp ult %int %index, %elems2
    br i1 %check2, label %Continue2, label %Error
    
    Continue2:
    %val.ptr = getelementptr [0 x %int (%frame*, i8*)*]* %list.ptr, i32 0, %int %index
    %val = load %int (%frame*, i8*)** %val.ptr

    %ap = call i8* @malloc(%int 8)
    call void @llvm.va_start(i8* %ap)
    %ret = call %int %val(%frame* %frame.ptr,i8* %ap)
    call void @llvm.va_end(i8* %ap)
    call void @free(i8* %ap)
    ret %int %ret
    
    Error:
    call void @exit(i32 -1)
    unreachable
}




declare void @exit(i32)
declare i8* @malloc(%int)
declare void @free(i8*)
declare %int @unmask(%int)
declare void @llvm.va_start(i8*)
declare void @llvm.va_copy(i8*, i8*)
declare void @llvm.va_end(i8*)