define i32 @newcredentials(){
entry:
	%0 = call i32 @rand()
	ret i32 %0
}

define i32 @encrypt(i32, i32) { 
entry:
	%x = add i32 %0, %1
	%y = urem i32 26, %x 
	ret i32 %x
}

define i32 @decrypt(i32, i32) { 
entry:
	%x = sub i32 %0, %1
	%y = urem i32 26, %x 
	ret i32 %x
}

@.seed = private constant i32 3

define private i32 @rand() {
entry:
	%t = load i32* @.seed
   ret i32 %t
}

define i32 @main(){
	;%ta = getelementptr i32* @.seed, i32 0
	;%tb = add i32 0, 3
	;%t = getelementptr i32* @.seed, i32 0
	;call i32 @puts(i32* %ta)
	;call i32 @puts(i32* %t)
	ret i32 -1
}

declare i32 @puts(i32*)
;declare i32 @(i32)
