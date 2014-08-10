%int = type i64
%Caesar.cred = type {%int, %int}

declare i8* @malloc(%int)
declare void @free(i8*)
declare void @exit(%int)

define %int @Caesar.newcredentials(){
   %credptr = call %Caesar.cred* @Caesar.newcredentials_internal()
   %credint = ptrtoint %Caesar.cred* %credptr to %int
   %ret = call %int @mask(%int %credint,%int 4)
   ret %int %ret
}

define private %Caesar.cred* @Caesar.newcredentials_internal(){
   %rand = call %int @Caesar.rand()
   %ptr1 = call i8* @malloc(%int 16)
   %ptr = bitcast i8* %ptr1 to %Caesar.cred*
   %locval = getelementptr inbounds %Caesar.cred* %ptr, i32 0, i32 0
   store %int %rand,%int* %locval
   %loctype = getelementptr inbounds %Caesar.cred* %ptr, i32 0, i32 0
   store %int 4, %int* %loctype
   ret %Caesar.cred* %ptr
}

define %int @Caesar.encrypt(%int %arg0, %int %arg1) {
   %arg1int = call %int @unmask(%int %arg1)
   %arg1type = call %int @unmasktype(%int %arg1)
   %check1 = icmp eq %int %arg1type, 4
   br i1 %check1, label %Continue, label %Error
   
   Continue:
   %arg1ptr = inttoptr %int %arg1int to %Caesar.cred*
   %ret = call %int @Caesar.encrypt_internal(%int %arg0, %Caesar.cred* %arg1ptr)
   ret %int %ret
   
   Error:
   call void @exit(%int -1)
   unreachable
}

define private %int @Caesar.encrypt_internal(%int %a, %Caesar.cred* %credptr) {
   %loccred = getelementptr inbounds %Caesar.cred* %credptr, i32 0, i32 0
   %cred = load %int* %loccred
   %x = add %int %a, %cred
   %y = urem %int 26, %x
   ret %int %y
}

define %int @Caesar.decrypt(%int %arg0, %int %arg1) {
   %arg1int = call %int @unmask(%int %arg1)
   %arg1type = call %int @unmasktype(%int %arg1)
   %check1 = icmp eq %int %arg1type, 4
   br i1 %check1, label %Continue, label %Error
   
   Continue:
   %arg1ptr = inttoptr %int %arg1int to %Caesar.cred*
   %ret = call %int @Caesar.decrypt_internal(%int %arg0, %Caesar.cred* %arg1ptr)
   ret %int %ret
   
   Error:
   call void @exit(%int -1)
   unreachable
}

define private %int @Caesar.decrypt_internal(%int %a, %Caesar.cred* %credptr) {
   %loccred = getelementptr inbounds %Caesar.cred* %credptr, i32 0, i32 0
   %cred = load %int* %loccred 
   %x = sub %int %a, %cred
   %y = urem %int 26, %x 
   ret %int %y
}

define private %int @Caesar.seed() {
   ret %int 3
}

define private %int @Caesar.rand() {
   %t = call %int @Caesar.seed()
   ret %int %t
}

%masktype = type {%int, %masktype*, %int}

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
      call void @exit(%int -1)
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
      %ret = call %int @unmask_rec(%int %newindex, %masktype** %nextptr)
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
      call void @exit(%int -1)
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
      %ret = call %int @unmasktype_rec(%int %newindex, %masktype** %nextptr)
      ret %int %ret
}