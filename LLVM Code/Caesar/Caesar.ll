%int = type i64
%Caesar.cred = type {%int}

declare i8* @malloc(%int)
declare void @free(i8*)
declare void @exit(i32)

define %int @Caesar.decrypt(%int %arg0, %int %arg1) {
   %ret = tail call %int @Caesar.decrypt_stub(%int %arg0, %int %arg1)
   ret %int %ret
}

define %int @Caesar.encrypt(%int %arg0, %int %arg1) {
   %ret = tail call %int @Caesar.encrypt_stub(%int %arg0, %int %arg1)
   ret %int %ret
}

define private %int @Caesar.newcredentials() {
   %ret = tail call %int @Caesar.newcredentials_stub()
   ret %int %ret
}

define private %int @Caesar.decrypt_stub(%int %arg0, %int %arg1) noinline {
   %argtest = add %int %arg0, 0
   %arg1int = call %int @unmask(%int %arg1)
   %arg1type = call %int @unmasktype(%int %arg1)
   %check1 = icmp eq %int %arg1type, 4
   br i1 %check1, label %Continue, label %Error
   
   Continue:
   %arg1ptr = inttoptr %int %arg1int to %Caesar.cred*
   %ret = call %int @Caesar.decrypt_internal(%int %arg0, %Caesar.cred* %arg1ptr)
   ret %int %ret
   
   Error:
   call void @exit(i32 -1)
   unreachable
}

define private %int @Caesar.decrypt_internal(%int %a, %Caesar.cred* %credptr) {
   %loccred = getelementptr inbounds %Caesar.cred* %credptr, i32 0, i32 0
   %cred = load %int* %loccred 
   %x = sub %int %a, %cred
   %y = urem %int 26, %x 
   ret %int %y
}

define private %int @Caesar.encrypt_stub(%int %arg0, %int %arg1) noinline {
   %arg1int = call %int @unmask(%int %arg1)
   %arg1type = call %int @unmasktype(%int %arg1)
   %check1 = icmp eq %int %arg1type, 4
   br i1 %check1, label %Continue, label %Error
   
   Continue:
   %arg1ptr = inttoptr %int %arg1int to %Caesar.cred*
   %ret = call %int @Caesar.encrypt_internal(%int %arg0, %Caesar.cred* %arg1ptr)
   ret %int %ret
   
   Error:
   call void @exit(i32 -1)
   unreachable
}

define private %int @Caesar.encrypt_internal(%int %a, %Caesar.cred* %credptr) {
   %loccred = getelementptr inbounds %Caesar.cred* %credptr, i32 0, i32 0
   %cred = load %int* %loccred
   %x = add %int %a, %cred
   %y = urem %int 26, %x
   ret %int %y
}

define private %int @Caesar.newcredentials_stub() noinline {
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

define private %int @Caesar.rand() {
   %t = call %int @Caesar.seed()
   ret %int %t
}

define private %int @Caesar.seed() {
   ret %int 3
}

define %int @main(){
   ret %int 0
}

declare %int @mask(%int, %int)
declare %int @unmask(%int)
declare %int @unmasktype(%int)
