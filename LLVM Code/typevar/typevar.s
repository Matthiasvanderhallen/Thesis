	.section	__TEXT,__text,regular,pure_instructions
	.align	4, 0x90
L_Pair.createPair_internal:             ## @Pair.createPair_internal
## BB#0:
	pushq	%r14
	pushq	%rbx
	pushq	%rax
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movl	$16, %edi
	callq	_malloc
	movq	%rbx, (%rax)
	movq	%r14, 8(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	ret

	.globl	_Pair.createPair
	.align	4, 0x90
_Pair.createPair:                       ## @Pair.createPair
## BB#0:
	pushq	%rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	movl	%ecx, %r12d
	movq	%rdx, %r15
	movl	%esi, %ebp
	movq	%rdi, %rbx
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %r14
	testb	$3, %bpl
	jne	LBB1_1
## BB#3:                                ## %External1
	xorl	%eax, %eax
	jmp	LBB1_5
LBB1_1:
	andb	$3, %bpl
	cmpb	$1, %bpl
	jne	LBB1_2
## BB#4:                                ## %Int1
	movl	$1, %eax
LBB1_5:                                 ## %Int1
	movq	%rbx, %rbp
	jmp	LBB1_6
LBB1_2:                                 ## %Unmask1
	movq	%rbx, %rdi
	callq	_unmask
	movq	%rax, %rbp
	movq	%rbx, %rdi
	callq	_unmasktype
LBB1_6:                                 ## %Create1
	movq	%rbp, (%r14)
	movq	%rax, 8(%r14)
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %rbp
	testb	$3, %r12b
	jne	LBB1_7
## BB#9:                                ## %External2
	xorl	%eax, %eax
	jmp	LBB1_11
LBB1_7:                                 ## %Create1
	andb	$3, %r12b
	cmpb	$1, %r12b
	jne	LBB1_8
## BB#10:                               ## %Int2
	movl	$1, %eax
	movq	%r15, %rbx
	jmp	LBB1_11
LBB1_8:                                 ## %Unmask2
	movq	%r15, %rdi
	callq	_unmask
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	_unmasktype
LBB1_11:                                ## %Create2
	movq	%rbx, (%rbp)
	movq	%rax, 8(%rbp)
	movq	%r14, %rdi
	movq	%rbp, %rsi
	callq	L_tyvarcheck
	movq	%r14, %rdi
	movq	%rbp, %rsi
	callq	L_Pair.createPair_internal
	movq	%rax, %rdi
	movl	$4, %esi
	callq	_mask
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	ret

	.align	4, 0x90
L_Pair.getLeft_internal:                ## @Pair.getLeft_internal
## BB#0:
	movq	(%rdi), %rax
	ret

	.globl	_Pair.getLeft
	.align	4, 0x90
_Pair.getLeft:                          ## @Pair.getLeft
## BB#0:
	pushq	%r14
	pushq	%rbx
	pushq	%rax
	movq	%rdi, %rbx
	callq	_unmask
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	_unmasktype
	cmpq	$5, %rax
	jne	LBB3_2
## BB#1:                                ## %Continue
	movq	%r14, %rdi
	callq	L_Pair.getLeft_internal
	movq	(%rax), %rdi
	movq	8(%rax), %rsi
	callq	_mask
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	ret
LBB3_2:                                 ## %Error
	movl	$-1, %edi
	callq	_exit

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
## BB#0:
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %rbx
	movq	$1, 8(%rbx)
	movq	$1, (%rbx)
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	L_Pair.createPair_internal
	movq	%rax, %r15
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %r14
	movq	%r15, (%r14)
	movq	$5, 8(%r14)
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	L_tyvarcheck
	leaq	L_.strval(%rip), %r15
	movq	%r15, %rdi
	movl	$1, %esi
	xorb	%al, %al
	callq	_printf
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	L_tyvarcheck
	movq	%r15, %rdi
	movl	$1, %esi
	xorb	%al, %al
	callq	_printf
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	L_tyvarcheck
	movq	%r15, %rdi
	movl	$1, %esi
	xorb	%al, %al
	callq	_printf
	movq	%rbx, %rdi
	callq	_free
	xorl	%eax, %eax
	popq	%rbx
	popq	%r14
	popq	%r15
	ret

	.align	4, 0x90
L_tyvarcheck:                           ## @tyvarcheck
## BB#0:                                ## %Start
	pushq	%r14
	pushq	%rbx
	pushq	%rax
	movq	8(%rsi), %rax
	movq	8(%rdi), %rdx
	cmpq	%rax, %rdx
	jne	LBB5_12
## BB#1:
	movq	(%rsi), %rcx
	movq	(%rdi), %rax
	leaq	L_.ImplTable(%rip), %rsi
LBB5_2:                                 ## %Continue
                                        ## =>This Inner Loop Header: Depth=1
	movq	(%rsi,%rdx,8), %rdx
	cmpq	$2, %rdx
	jb	LBB5_10
## BB#3:                                ## %Continue
                                        ##   in Loop: Header=BB5_2 Depth=1
	cmpq	$4, %rdx
	je	LBB5_6
## BB#4:                                ## %Continue
                                        ##   in Loop: Header=BB5_2 Depth=1
	cmpq	$3, %rdx
	jne	LBB5_2
## BB#5:                                ## %PairRec
	movq	(%rcx), %r14
	movq	(%rax), %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	L_tyvarcheck
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	L_tyvarcheck
	jmp	LBB5_11
LBB5_6:                                 ## %ArrayRec
	cmpq	$0, (%rax)
	je	LBB5_10
## BB#7:                                ## %ArrayCont1
	movq	(%rcx), %rdx
	testq	%rdx, %rdx
	jne	LBB5_8
LBB5_10:                                ## %Ok
	movb	$1, %al
LBB5_11:                                ## %Ok
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	ret
LBB5_8:                                 ## %ArrayCont2
	movq	8(%rcx), %rsi
	movq	8(%rax), %rdi
	callq	L_tyvarcheck
	jmp	LBB5_11
LBB5_12:                                ## %Error
	movl	$-1, %edi
	callq	_exit

	.globl	_mask
	.align	4, 0x90
_mask:                                  ## @mask
## BB#0:
	pushq	%rax
	leaq	L_vtable(%rip), %rdx
	xorl	%ecx, %ecx
	callq	L_mask_rec
	popq	%rdx
	ret

	.align	4, 0x90
L_mask_rec:                             ## @mask_rec
## BB#0:
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	pushq	%rax
	movq	%rcx, %rbx
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	(%r12), %rdx
	testq	%rdx, %rdx
	jne	LBB7_2
## BB#1:                                ## %Add
	movl	$24, %edi
	callq	_malloc
	movq	%rax, (%r12)
	movq	%r15, (%rax)
	movq	$0, 8(%rax)
	movq	%r14, 16(%rax)
	jmp	LBB7_3
LBB7_2:                                 ## %Valcheck
	cmpq	%r15, (%rdx)
	jne	LBB7_4
LBB7_3:                                 ## %Return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	ret
LBB7_4:                                 ## %Loop
	addq	$8, %rdx
	incq	%rbx
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	L_mask_rec              ## TAILCALL

	.globl	_unmask
	.align	4, 0x90
_unmask:                                ## @unmask
## BB#0:
	pushq	%rax
	leaq	L_vtable(%rip), %rsi
	callq	L_unmask_rec
	popq	%rdx
	ret

	.align	4, 0x90
L_unmask_rec:                           ## @unmask_rec
## BB#0:
	pushq	%rax
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	je	LBB9_3
## BB#1:                                ## %ZeroTest
	testq	%rdi, %rdi
	je	LBB9_4
## BB#2:                                ## %Loop
	decq	%rdi
	addq	$8, %rsi
	popq	%rax
	jmp	L_unmask_rec            ## TAILCALL
LBB9_4:                                 ## %RetVal
	movq	(%rsi), %rax
	popq	%rdx
	ret
LBB9_3:                                 ## %Error
	movl	$-1, %edi
	callq	_exit

	.globl	_unmasktype
	.align	4, 0x90
_unmasktype:                            ## @unmasktype
## BB#0:
	pushq	%rax
	leaq	L_vtable(%rip), %rsi
	callq	L_unmasktype_rec
	popq	%rdx
	ret

	.align	4, 0x90
L_unmasktype_rec:                       ## @unmasktype_rec
## BB#0:
	pushq	%rax
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	je	LBB11_3
## BB#1:                                ## %ZeroTest
	testq	%rdi, %rdi
	je	LBB11_4
## BB#2:                                ## %Loop
	decq	%rdi
	addq	$8, %rsi
	popq	%rax
	jmp	L_unmasktype_rec        ## TAILCALL
LBB11_4:                                ## %RetVal
	movq	16(%rsi), %rax
	popq	%rdx
	ret
LBB11_3:                                ## %Error
	movl	$-1, %edi
	callq	_exit

	.section	__TEXT,__cstring,cstring_literals
L_.strval:                              ## @.strval
	.asciz	 "%d\n"

	.section	__TEXT,__const
	.align	4                       ## @.ImplTable
L_.ImplTable:
	.quad	0                       ## 0x0
	.quad	1                       ## 0x1
	.quad	2                       ## 0x2
	.quad	3                       ## 0x3
	.quad	4                       ## 0x4
	.quad	3                       ## 0x3

.zerofill __DATA,__bss,L_vtable,8,3     ## @vtable

.subsections_via_symbols
