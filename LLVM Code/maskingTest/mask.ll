%int = type i64

declare i32 @printf(i8*, ...)
declare i32 @puts(i8*)
declare i8* @malloc(%int)
declare void @free(i8*)
declare void @exit(i32)

%masktype = type {%int, %masktype*, %int}

;@.str = private unnamed_addr constant [12 x i8] c"maskptr:%d\0A\00", align 1
@.strval = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.nonnull = private unnamed_addr constant [9 x i8] c"nonnull\0A\00", align 1
;@.noneq = private unnamed_addr constant [7 x i8] c"noneq\0A\00", align 1

;Initial table pointer
@vtable = private global %masktype* null

define %int @mask(%int %val, %int %type){
	%ret = call %int @mask_rec(%int %val, %int %type, %masktype** @vtable, %int 0)
	ret %int %ret
}

define private %int @mask_rec(%int %val, %int %type, %masktype** %cptr, %int %index){
	;Load the current pointer to %masktype* & check if its null. If it is, add a new record to the linked list.
	%current = load %masktype** %cptr
	%check = icmp eq %masktype* %current, null
	switch i1 %check, label %Valcheck [i1 1, label %Add]

	;If the current pointer is null, we're at the end of the linked list
	;Add a record to the linked list by allocating memory for an element, and storing the pointer to it in the current pointer
	;Store value and initialize pointer of the element to null.
	Add:
		%ptr1 = call i8* @malloc(%int 24)
		%ptr = bitcast i8* %ptr1 to %masktype*
			;%loc = ptrtoint %masktype* %ptr to i32
			;call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str, i32 0, i32 0), i32 %loc)
		store %masktype* %ptr, %masktype** %cptr
		%locval = getelementptr inbounds %masktype* %ptr, i32 0, i32 0
			;%locvalint = ptrtoint i32* %locval to i32
			;call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i32 %locvalint)
		store %int %val, %int* %locval
		%locptr = getelementptr inbounds %masktype* %ptr, i32 0, i32 1
			;%locptrint = ptrtoint i32* % to i32
			;call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i32 %locptrint)
		store %masktype* null, %masktype** %locptr
		%loctype = getelementptr inbounds %masktype* %ptr, i32 0, i32 2
		store %int %type, %int* %loctype
		ret %int %index
	
	;If the current pointer is allocated, check the value.
	;if eq -> jump to the return
	;else -> change accumulator, rec jump
	Valcheck:
			;call i32 @puts(i8* getelementptr inbounds ([9 x i8]* @.nonnull, i32 0, i32 0))
		%locvalcheck = getelementptr inbounds %masktype* %current, i32 0, i32 0
		%valcheck = load %int* %locvalcheck
			;call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i32 %valcheck)
			;call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i32 %val)
		%check2 = icmp eq %int %valcheck, %val
		switch i1 %check2, label %Return [i1 0, label %Loop]
	
	Loop:
		;get pointer to next
			;call i32 @puts(i8* getelementptr inbounds ([7 x i8]* @.noneq, i32 0, i32 0))
		%nextptr = getelementptr inbounds %masktype* %current, i32 0, i32 1
		%newindex = add %int %index, 1
		%tailindex = tail call %int @mask_rec(%int %val, %int %type, %masktype** %nextptr, %int %newindex)
		ret %int %tailindex

	Return:
		ret %int %index
}

define %int @unmask(%int %index){
	%ret = call %int @unmask_rec(%int %index, %masktype** @vtable)
	ret %int %ret
}

define private %int @unmask_rec(%int %cindex, %masktype** %cpointer){
	%current = load %masktype** %cpointer ;Get pointer to current masktype.
	%check = icmp eq %masktype* %current, null
	br i1 %check, label %Error, label %ZeroTest

	Error:
		call void @exit(i32 -1)
  		unreachable

	ZeroTest:
		%check2 = icmp eq %int %cindex, 0
		br i1 %check2, label %RetVal, label %Loop

	RetVal:
		%locval = getelementptr inbounds %masktype* %current, i32 0, i32 0 ; Get pointer to val pointer
		%val = load %int* %locval
		ret %int %val

	Loop:
		%nextptr = getelementptr inbounds %masktype* %current, i32 0, i32 1 ; Get pointer to next ll-element pointer.
		%newindex = sub %int %cindex, 1
		%ret = tail call %int @unmask_rec(%int %newindex, %masktype** %nextptr)
		ret %int %ret
}

define %int @unmasktype(%int %index){
	%ret = call %int @unmasktype_rec(%int %index, %masktype** @vtable)
	ret %int %ret
}

define private %int @unmasktype_rec(%int %cindex, %masktype** %cpointer){
	%current = load %masktype** %cpointer ;Get pointer to current masktype.
	%check = icmp eq %masktype* %current, null
	br i1 %check, label %Error, label %ZeroTest

	Error:
		call void @exit(i32 -1)
  		unreachable

	ZeroTest:
		%check2 = icmp eq %int %cindex, 0
		br i1 %check2, label %RetVal, label %Loop

	RetVal:
		%loctype = getelementptr inbounds %masktype* %current, i32 0, i32 2  ; Get pointer to type pointer
		%type = load %int* %loctype
		ret %int %type

	Loop:
		%nextptr = getelementptr inbounds %masktype* %current, i32 0, i32 1  ; Get pointer to next ll-element pointer.
		%newindex = sub %int %cindex, 1
		%ret = tail call %int @unmasktype_rec(%int %newindex, %masktype** %nextptr)
		ret %int %ret
}

define i32 @main(){
	%a = call %int @mask(%int 0, %int 1)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), %int %a) ;0
	%b = call %int @mask(%int 5, %int 1)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), %int %b) ;1
	%d = call %int @mask(%int 2, %int 1)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), %int %d) ;2
	%e = call %int @mask(%int 5, %int 1)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), %int %e) ;1
	%f = call %int @unmask(%int 1)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), %int %f) ;5
	%g = call %int @unmasktype(%int 1)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), %int %g) ;1
	%h = call %int @unmask(%int 5)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), %int %h) ; unreach
	ret i32 0
}