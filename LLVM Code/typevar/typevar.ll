%int = type i64 ; 1
%tyvar = type {%int, %int} ; 2
%pair = type {%tyvar*, %tyvar*} ; 3
%array = type {%int, [0 x %tyvar*]} ; 4
%Pair.t = type {%pair} ; 5

@.typeOfTypevar = private unnamed_addr constant [10 x i8] c"type: %d\0A\00", align 1
@.strval = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
declare i32 @printf(i8*, ...)

declare i8* @malloc(%int)
declare void @free(i8*)
declare void @exit(i32)

define private %Pair.t* @Pair.createPair_internal(%tyvar* %p1, %tyvar* %p2) nounwind noinline{
	%ptr = call i8* @malloc(%int 16)
	%ptr1 = bitcast i8* %ptr to %pair*
	%fst = getelementptr %pair* %ptr1, i32 0, i32 0 ;%tyvar**
	store %tyvar* %p1, %tyvar** %fst
	%snd = getelementptr %pair* %ptr1, i32 0, i32 1 ;%tyvar**
	store %tyvar* %p2, %tyvar** %snd

	%ptr2 = bitcast %pair* %ptr1 to %Pair.t*
	ret %Pair.t* %ptr2
}

define %int @Pair.createPair(%int %left, i2 %left.mask, %int %right, i2 %right.mask) nounwind noinline{
	;We need extra parameters, because we don't know the type that was passed. Is it an externally defined type, is it an int, or is it a mask?
	%leftTyvar.ptr.1 = call i8* @malloc(%int 16)
	%leftTyvar.ptr = bitcast i8* %leftTyvar.ptr.1 to %tyvar*
	switch i2 %left.mask, label %Unmask1 [i2 0, label %External1
											i2 1, label %Int1]
	Unmask1:
	%left.ptr = call %int @unmask(%int %left)
	%left.type = call %int @unmasktype(%int %left)
	%leftAsTyvar.unmask.1 = insertvalue %tyvar undef, %int %left.ptr, 0
	%leftAsTyvar.unmask.2 = insertvalue %tyvar %leftAsTyvar.unmask.1, %int %left.type, 1
	br label %Create1
	
	External1:
	%leftAsTyvar.ext.1 = insertvalue %tyvar undef, %int %left, 0
	%leftAsTyvar.ext.2 = insertvalue %tyvar %leftAsTyvar.ext.1, %int 0, 1
	br label %Create1

	Int1:
	%leftAsTyvar.int.1 = insertvalue %tyvar undef, %int %left, 0
	%leftAsTyvar.int.2 = insertvalue %tyvar %leftAsTyvar.int.1, %int 1, 1
	br label %Create1

	Create1:
	%leftAsTyvar = phi %tyvar [%leftAsTyvar.unmask.2, %Unmask1], [%leftAsTyvar.ext.2, %External1], [%leftAsTyvar.int.2,%Int1]
	store %tyvar %leftAsTyvar, %tyvar* %leftTyvar.ptr

	%rightTyvar.ptr.1 = call i8* @malloc(%int 16)
	%rightTyvar.ptr = bitcast i8* %rightTyvar.ptr.1 to %tyvar*
	switch i2 %right.mask, label %Unmask2 [i2 0, label %External2
										   i2 1, label %Int2]

	Unmask2:
	%right.ptr = call %int @unmask(%int %right)
	%right.type = call %int @unmasktype(%int %right)
	%rightAsTyvar.unmask.1 = insertvalue %tyvar undef, %int %right.ptr, 0
	%rightAsTyvar.unmask.2 = insertvalue %tyvar %rightAsTyvar.unmask.1, %int %right.type, 1
	br label %Create2

	External2:
	%rightAsTyvar.ext.1 = insertvalue %tyvar undef, %int %left, 0
	%rightAsTyvar.ext.2 = insertvalue %tyvar %rightAsTyvar.ext.1, %int 0, 1
	br label %Create2

	Int2:
	%rightAsTyvar.int.1 = insertvalue %tyvar undef, %int %right, 0
	%rightAsTyvar.int.2 = insertvalue %tyvar %rightAsTyvar.int.1, %int 1, 1
	br label %Create2

	Create2:
	%rightAsTyvar = phi %tyvar [%rightAsTyvar.unmask.2, %Unmask2], [%rightAsTyvar.ext.2, %External2], [%rightAsTyvar.int.2,%Int2]
	store %tyvar %rightAsTyvar, %tyvar* %rightTyvar.ptr

	call i1 @tyvarcheck(%tyvar* %leftTyvar.ptr, %tyvar* %rightTyvar.ptr) ; type equation

	%pair = call %Pair.t* @Pair.createPair_internal(%tyvar* %leftTyvar.ptr, %tyvar* %rightTyvar.ptr)

	%pair.ptr = ptrtoint %Pair.t* %pair to %int
	%pair.mask = call %int @mask(%int %pair.ptr, %int 4)

	ret %int %pair.mask
}

define private %tyvar* @Pair.getLeft_internal(%Pair.t* %pair.ptr) nounwind noinline{
	%pair.ptr.1 = bitcast %Pair.t* %pair.ptr to %pair*
	%left.ptr = getelementptr %pair* %pair.ptr.1, i32 0, i32 0 ;%tyvar**
	%left = load %tyvar** %left.ptr
	ret %tyvar* %left
}

define %int @Pair.getLeft(%int %pair) nounwind noinline{
	%pair.unmask = call %int @unmask(%int %pair)
	%pair.type = call %int @unmasktype(%int %pair)
	%pair.check = icmp eq %int %pair.type, 5
	br i1 %pair.check, label %Continue, label %Error

	Continue:
	%pair.ptr = inttoptr %int %pair.unmask to %Pair.t*
	%return = call %tyvar* @Pair.getLeft_internal(%Pair.t* %pair.ptr)
	%left = load %tyvar* %return
	%left.addr = extractvalue %tyvar %left, 0
	%left.type = extractvalue %tyvar %left, 1
	%retval = call %int @mask(%int %left.addr, %int %left.type)
	ret %int %retval

	Error:
	call void @exit(i32 -1)
	unreachable
}

define private %int @main() nounwind noinline{
	

	%argptr = call i8* @malloc(%int 16)
	%argptr1 = bitcast i8* %argptr to %tyvar*
	%arg1.1 = insertvalue %tyvar undef, %int 1, 0
	%arg1.2 = insertvalue %tyvar %arg1.1, %int 1, 1
	
	store %tyvar %arg1.2, %tyvar* %argptr1
	%pair.ptr = call %Pair.t* @Pair.createPair_internal(%tyvar* %argptr1, %tyvar* %argptr1)
	%pair.ptr.int = ptrtoint %Pair.t* %pair.ptr to %int

	%arg2ptr = call i8* @malloc(%int 16)
	%argptr2 = bitcast i8* %arg2ptr to %tyvar*
	%arg2.1 = insertvalue %tyvar undef, %int %pair.ptr.int, 0
	%arg2.2 = insertvalue %tyvar %arg2.1, %int 5, 1
	store %tyvar %arg2.2, %tyvar* %argptr2

	
	call i1 @tyvarcheck(%tyvar* %argptr1, %tyvar* %argptr1)	
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i1 1)
	call i1 @tyvarcheck(%tyvar* %argptr2, %tyvar* %argptr2)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i1 1)
	;call i1 @tyvarcheck(%tyvar* %argptr1, %tyvar* %argptr2)
		call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.strval, i32 0, i32 0), i1 1)


	;call %Pair.t* @Pair.createPair_internal()

	call void @free(i8* %argptr)

	ret i64 0
}

@.ImplTable = private constant [6 x %int] [%int 0, %int 1, %int 2, %int 3, %int 4, %int 3]

define private i1 @tyvarcheck(%tyvar* %v1, %tyvar* %v2) nounwind noinline{
	Start:
	%v1.1 = load %tyvar* %v1
	%v2.1 = load %tyvar* %v2
	%t1 = extractvalue %tyvar %v1.1, 1
	%t2 = extractvalue %tyvar %v2.1, 1
	%equal = icmp eq %int %t1, %t2
	br i1 %equal, label %Continue, label %Error

	Continue:
		%basetype = phi %int [%t1, %Start], [%t.1, %Continue]
		%t = getelementptr [6 x %int]* @.ImplTable, i32 0, %int %basetype
		%t.1 = load %int* %t
		switch %int %t.1, label %Continue [%int 0, label %Ok
										%int 1, label %Ok
									 	%int 3, label %PairRec
									 	%int 4, label %ArrayRec]

	PairRec:
		%pairptrv1 = extractvalue %tyvar %v1.1, 0
		%pairptrv1.1 = inttoptr %int %pairptrv1 to %pair*
		%tyvarlptrv1 = getelementptr inbounds %pair*  %pairptrv1.1, i32 0, i32 0
		%tyvarlptrv1.1 = load %tyvar** %tyvarlptrv1
		%tyvarrptrv1 = getelementptr inbounds %pair*  %pairptrv1.1, i32 0, i32 0
		%tyvarrptrv1.1 = load %tyvar** %tyvarrptrv1
		%pairptrv2 = extractvalue %tyvar %v2.1, 0
		%pairptrv2.1 = inttoptr %int %pairptrv2 to %pair*
		%tyvarlptrv2 = getelementptr inbounds %pair*  %pairptrv2.1, i32 0, i32 0
		%tyvarlptrv2.1 = load %tyvar** %tyvarlptrv2
		%tyvarrptrv2 = getelementptr inbounds %pair*  %pairptrv2.1, i32 0, i32 0
		%tyvarrptrv2.1 = load %tyvar** %tyvarrptrv2
		call i1 @tyvarcheck(%tyvar* %tyvarlptrv1.1, %tyvar*  %tyvarlptrv2.1)
		%pairret = call i1 @tyvarcheck(%tyvar* %tyvarrptrv1.1, %tyvar*  %tyvarrptrv2.1)
		ret i1 %pairret

	ArrayRec:
		%arrayptrv1 = extractvalue %tyvar %v1.1, 0
		%arrayptrv1.1 = inttoptr %int %arrayptrv1 to %array*
		%v1length = getelementptr inbounds %array* %arrayptrv1.1, i32 0, i32 0
		%v1length.1 = load %int* %v1length
		%zerolv1 = icmp eq %int 0, %v1length.1
		%arrayptrv2 = extractvalue %tyvar %v2.1, 0
		%arrayptrv2.1 = inttoptr %int %arrayptrv2 to %array*
		%v2length = getelementptr inbounds %array* %arrayptrv2.1, i32 0, i32 0
		%v2length.1 = load %int* %v2length
		%zerolv2 = icmp eq %int 0, %v2length.1
		br i1 %zerolv1, label %Ok, label %ArrayCont1

	ArrayCont1:
		br i1 %zerolv2, label %Ok, label %ArrayCont2

	ArrayCont2:
		%v1el1 = getelementptr inbounds %array* %arrayptrv1.1, i32 0, i32 1
		%v1el1.1 = getelementptr [0 x %tyvar*]* %v1el1, i32 0, i32 0
		%v1el1.2 = load %tyvar** %v1el1.1
		%v2el1 = getelementptr inbounds %array* %arrayptrv2.1, i32 0, i32 1
		%v2el1.1 = getelementptr [0 x %tyvar*]* %v2el1, i32 0, i32 0
		%v2el1.2 = load %tyvar** %v2el1.1
		%arrayret = call i1 @tyvarcheck(%tyvar* %v1el1.2, %tyvar* %v2el1.2)
		ret i1 %arrayret

	Ok:
		ret i1 1

	Error:
		call void @exit(i32 -1)
  		unreachable
}

%masktype = type {%int, %masktype*, %int}

;Initial table pointer
@vtable = private global %masktype* null

define %int @mask(%int %val, %int %type) nounwind noinline{
	%ret = call %int @mask_rec(%int %val, %int %type, %masktype** @vtable, %int 0)
	ret %int %ret
}

define private %int @mask_rec(%int %val, %int %type, %masktype** %cptr, %int %index) nounwind noinline{
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

define %int @unmask(%int %index) nounwind noinline{
	%ret = call %int @unmask_rec(%int %index, %masktype** @vtable)
	ret %int %ret
}

define private %int @unmask_rec(%int %cindex, %masktype** %cpointer) nounwind noinline{
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

define %int @unmasktype(%int %index) nounwind noinline{
	%ret = call %int @unmasktype_rec(%int %index, %masktype** @vtable)
	ret %int %ret
}

define private %int @unmasktype_rec(%int %cindex, %masktype** %cpointer) nounwind noinline{
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