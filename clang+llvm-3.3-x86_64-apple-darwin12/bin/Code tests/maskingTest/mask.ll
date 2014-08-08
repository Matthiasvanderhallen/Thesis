declare i32 @printf(i8*, ...)
declare i32 @puts(i8*)
declare i8* @malloc(i64)

%masktype = type {i32, %masktype*}

;@.str = private unnamed_addr constant [12 x i8] c"maskptr:%d\0A\00", align 1
@.strval = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
;@.nonnull = private unnamed_addr constant [9 x i8] c"nonnull\0A\00", align 1
;@.noneq = private unnamed_addr constant [7 x i8] c"noneq\0A\00", align 1

;Initial table pointer
@vtable = private global %masktype* null

define i32 @mask(i32 %val){
	%ret = call i32 @mask_rec(i32 %val, %masktype** @vtable, i32 0)
	ret i32 %ret
}

define private i32 @mask_rec(i32 %val, %masktype** %cptr, i32 %index){
	;Load the current pointer to %masktype* & check if its null. If it is, add a new record to the linked list.
	%current = load %masktype** %cptr
	%check = icmp eq %masktype* %current, null
	switch i1 %check, label %Valcheck [i1 1, label %Add]

	;If the current pointer is null, we're at the end of the linked list
	;Add a record to the linked list by allocating memory for an element, and storing the pointer to it in the current pointer
	;Store value and initialize pointer of the element to null.
	Add:
		%ptr1 = call i8* @malloc(i64 16)
		%ptr = bitcast i8* %ptr1 to %masktype*
			;%loc = ptrtoint %masktype* %ptr to i32
			;call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str, i32 0, i32 0), i32 %loc)
		store %masktype* %ptr, %masktype** %cptr
		%locval = getelementptr inbounds %masktype* %ptr, i32 0, i32 0
			;%locvalint = ptrtoint i32* %locval to i32
			;call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i32 %locvalint)
		store i32 %val, i32* %locval
		%locptr = getelementptr inbounds %masktype* %ptr, i32 0, i32 1
			;%locptrint = ptrtoint i32* % to i32
			;call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i32 %locptrint)
		store %masktype* null, %masktype** %locptr
		ret i32 %index
	
	;If the current pointer is allocated, check the value.
	;if eq -> jump to the return
	;else -> change accumulator, rec jump
	Valcheck:
			;call i32 @puts(i8* getelementptr inbounds ([9 x i8]* @.nonnull, i32 0, i32 0))
		%locvalcheck = getelementptr inbounds %masktype* %current, i32 0, i32 0
		%valcheck = load i32* %locvalcheck
			;call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i32 %valcheck)
			;call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i32 %val)
		%check2 = icmp eq i32 %valcheck, %val
		switch i1 %check2, label %Return [i1 0, label %Loop]
		;%current = phi i
	
	Loop:
		;get pointer to next
			;call i32 @puts(i8* getelementptr inbounds ([7 x i8]* @.noneq, i32 0, i32 0))
		%nextptr = getelementptr inbounds %masktype* %current, i32 0, i32 1
		%newindex = add i32 %index, 1
		%tailindex = tail call i32 @mask_rec(i32 %val, %masktype** %nextptr, i32 %newindex)
		ret i32 %tailindex

	Return:
		ret i32 %index
}

define i32 @unmask(i32 %index){
	%ret = call i32 @unmask_rec(i32 %index, %masktype** @vtable)
	ret i32 %ret
}

define private i32 @unmask_rec(i32 %cindex, %masktype** %cpointer){
	%check = icmp ult i32 %cindex, 0
	br i1 %check, label %Error, label %ZeroTest

	Error:
		ret i32 -1

	ZeroTest:
		%current = load %masktype** %cpointer ;Get pointer to current masktype.
		%check2 = icmp eq i32 %cindex, 0
		br i1 %check2, label %RetVal, label %Loop

	RetVal:
		%locval = getelementptr inbounds %masktype* %current, i32 0, i32 0 ; Get pointer to val pointer
		%val = load i32* %locval
		ret i32 %val

	Loop:
		%nextptr = getelementptr inbounds %masktype* %current, i32 0, i32 1 ; Get pointer to next ll-element pointer.
		%newindex = sub i32 %cindex, 1
		%ret = call i32 @unmask_rec(i32 %newindex, %masktype** %nextptr)
		ret i32 %ret
}

define i32 @main(){
	;%1 = load i32* @vtable
	%a = call i32 @mask(i32 0)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i32 %a)
	%b = call i32 @mask(i32 5)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i32 %b)
	%d = call i32 @mask(i32 2)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i32 %d)
	%e = call i32 @mask(i32 5)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i32 %e)
	%f = call i32 @unmask(i32 1)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i32 %f)
	ret i32 0
}