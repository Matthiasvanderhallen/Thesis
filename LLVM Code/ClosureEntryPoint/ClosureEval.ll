%int = type i64

%frame = type {[0 x i8]*, %frame*, i1, {%int, [0 x %int]}*, {%int, [0 x %int (%frame*, i8*)*]}*, {%int, [0 x %int]}*, %metaframe*}
%metaframe = type {{%int, [0 x %typedef]}*, {%int, [0 x %valuedef]}*} %typedef = type {[0 x i8]*, %int, %int, %type*, i1}
%valuedef = type {[0 x i8]*, %type*}
%type = type {%int, %int, %int}
%closure = type {%int, {%int, [0 x %int]}*, i1}

define %int @GenericClosureEvaluation(%int %closure.mask, ...){
    %closure.unmask = call %int @unmask(%int %closure.mask)
    %type = call %int @unmasktype(%int %closure.mask) ; Check that it really is a closure
    switch %int %type, label %Error [%int 5, label %Continue1]
    
    Continue1:
    %closure.valptr = inttoptr %int %closure.unmask to %closure*
    %closure = load %closure* %closure.valptr
    %closure.ptr = extractvalue %closure %closure, 0
    %closure.env = extractvalue %closure %closure, 1
    %closure.type = extractvalue %closure %closure, 2;Check that the closure is a secure closure.
    switch i1 %closure.type, label %Error [i1 1, label %Continue2]
    
    Continue2:
    %closure.fn = inttoptr %int %closure.ptr to %int (i1, {%int, [0 x %int]}*, i8*)*
    %ap = call i8* @malloc(%int 8)
    call void @llvm.va_start(i8* %ap)
    %ret = call %int %closure.fn(i1 0, {%int, [0 x %int]}* %closure.env, i8* %ap)
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
declare %int @unmasktype(%int)
declare void @llvm.va_start(i8*)
declare void @llvm.va_copy(i8*, i8*)
declare void @llvm.va_end(i8*)