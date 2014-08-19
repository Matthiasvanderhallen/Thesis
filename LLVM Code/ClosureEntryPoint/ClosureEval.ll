%int = type i64

%frame = type {[0 x i8]*, %frame*, i1, {%int, [0 x %int]}*, {%int, [0 x %int (%frame*, i8*)*]}*, {%int, [0 x %int]}*, %metaframe*}
%metaframe = type {{%int, [0 x %typedef]}*, {%int, [0 x %valuedef]}*} %typedef = type {[0 x i8]*, %int, %int, %type*, i1}
%valuedef = type {[0 x i8]*, %type*}
%type = type {%int, %int, %int}
%closure = type {%int, {%int, [0 x %int]}*, i1}

define %int @GenericClosureEvaluation(%int %closure.mask, i8* %arg){
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
    %ret = call %int %closure.fn(i1 0, {%int, [0 x %int]}* %closure.env, i8* %arg)
    ;%test.int = ptrtoint i8* %arg to %int
    ;%test.ptr = inttoptr %int %test.int to {%int, i1, %int, %int}*
    ;%test = bitcast i8* %arg to {%int, i1, %int, %int}*
    ret %int %ret
    
    Error:
    call void @exit(i32 -1)
    unreachable
}

define %int @main() {
    %t.1 = call i8* @malloc(%int 26)
    %t.2 = bitcast i8* %t.1 to {%int,%int,i1,%int}*
    %args.0 = insertvalue {%int, %int, i1, %int} undef, %int 15, 0
    %args.1 = insertvalue {%int, %int, i1, %int} %args.0, %int 16, 1
    %args.2 = insertvalue {%int, %int, i1, %int} %args.1, %int 17, 3
    %args.3 = insertvalue {%int, %int, i1, %int} %args.2, i1 6, 2
    store {%int, %int, i1, %int} %args.3, {%int,%int, i1, %int}* %t.2
    
    %t = call %int @test(i8* %t.1)
    
    ret %int 1
}

@.strval = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define %int @test(i8* %vargs) {
    %vargs.typed = bitcast i8* %vargs to {%int,%int,i1,%int,%int}*
    %vargs.val = load {%int,%int,i1, %int, %int}* %vargs.typed
    %v0 = extractvalue {%int,%int, i1, %int, %int} %vargs.val, 0
    %v1 = extractvalue {%int,%int, i1, %int, %int} %vargs.val, 1
    %v2 = extractvalue {%int,%int, i1, %int, %int} %vargs.val, 2
    %v3 = extractvalue {%int,%int, i1, %int, %int} %vargs.val, 3
    %v4 = extractvalue {%int,%int, i1, %int, %int} %vargs.val, 4

    call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), %int %v0)
    call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), %int %v1)
    call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i1 %v2)
    call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), %int %v3)
    call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), %int %v4)

    call void @free(i8* %vargs)

    ret %int 1
}

declare void @exit(i32)
declare i8* @malloc(%int)
declare void @free(i8*)
declare %int @unmask(%int)
declare %int @unmasktype(%int)
declare i32 @printf(i8*, ...)