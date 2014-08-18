%int = type i64
%Caesar.cred = type {%int}

declare i8* @malloc(%int)
declare void @free(i8*)
declare void @exit(i32)

define private %int @Caesar.decrypt_internal(%int %a, %Caesar.cred* %cred){
   %a.val = add %int 0, %a ; Integer reassign hack
   %cred.val = load %Caesar.cred* %cred
   %t0.right.int = extractvalue %Caesar.cred %cred.val, 0 ; Access
   %t0 = sub %int %a, %t0.right.int
   %t1 = add %int 0, 26
   %t2 = srem %int %t0, %t1
   ret %int %t2
}

define %int @Caesar.decrypt(%int %a.in, %int %cred.in){
   ;Switch stack, move parameters, add entry point in spm
   Process1:
      %a = add %int %a.in, 0 ; Renaming trick 
      br label %Process2

   Process2:
      %cred.addr = call %int @unmask(%int %cred.in)
      %cred.type = call %int @unmasktype(%int %cred.in)
      %cred = inttoptr %int %cred.addr to %Caesar.cred*
      %cred.check = icmp ne %int %cred.type, 6
      br i1 %cred.check, label %Error, label %Process3

   Process3:

   %ret = call %int @Caesar.decrypt_internal(%int %a, %Caesar.cred* %cred)
   ;Switch stack, clear registers and flags
   ret %int %ret

   Error:
      call void @exit(i32 -1)
      unreachable
}

define private %int @Caesar.encrypt_internal(%int %a, %Caesar.cred* %cred){
   %a.val = add %int 0, %a ; Integer reassign hack
   %cred.val = load %Caesar.cred* %cred
   %t0.right.int = extractvalue %Caesar.cred %cred.val, 0 ; Access
   %t0 = add %int %a, %t0.right.int
   %t1 = add %int 0, 26
   %t2 = srem %int %t0, %t1
   ret %int %t2
}

define %int @Caesar.encrypt(%int %a.in, %int %cred.in){
   ;Switch stack, move parameters, add entry point in spm
   Process1:
      %a = add %int %a.in, 0 ; Renaming trick 
      br label %Process2

   Process2:
      %cred.addr = call %int @unmask(%int %cred.in)
      %cred.type = call %int @unmasktype(%int %cred.in)
      %cred = inttoptr %int %cred.addr to %Caesar.cred*
      %cred.check = icmp ne %int %cred.type, 6
      br i1 %cred.check, label %Error, label %Process3

   Process3:

   %ret = call %int @Caesar.encrypt_internal(%int %a, %Caesar.cred* %cred)
   ;Switch stack, clear registers and flags
   ret %int %ret

   Error:
      call void @exit(i32 -1)
      unreachable
}

define private %Caesar.cred* @Caesar.newcredentials_internal(){
   %t0 = call %int @Caesar.rand_internal()
   %t0.val = add %int 0, %t0
   %t1.addr = call i8* @malloc(%int 8)
   %t1 = bitcast i8* %t1.addr to %Caesar.cred*
   %t1.val = insertvalue %Caesar.cred undef, %int %t0, 0
   store %Caesar.cred %t1.val, %Caesar.cred* %t1
   ret %Caesar.cred* %t1
}

define %int @Caesar.newcredentials(){
   ;Switch stack, move parameters, add entry point in spm
   %ret.ptr = call %Caesar.cred* @Caesar.newcredentials_internal()
   %ret.int = ptrtoint %Caesar.cred* %ret.ptr to %int
   %ret.mask = call %int @mask(%int %ret.int, %int 6)
   ;Switch stack, clear registers and flags
   ret %int %ret.mask
}

define private %int @Caesar.rand_internal(){
   %t0 = call %int @Caesar.seed_internal()
   %t0.val = add %int 0, %t0
   ret %int %t0
}

define private %int @Caesar.seed_internal(){
   %t0 = add %int 0, 3
   ret %int %t0
}


define %int @main(){
   ret %int 0
}

declare %int @mask(%int, %int)
declare %int @unmask(%int)
declare %int @unmasktype(%int)
