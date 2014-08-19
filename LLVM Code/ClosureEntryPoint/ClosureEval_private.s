	.section	__TEXT,__text,regular,pure_instructions
	.align	4, 0x90
L_GenericClosureEvaluation:             ## @GenericClosureEvaluation
	.cfi_startproc
## BB#0:
	pushq	%r15
Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
Ltmp6:
	.cfi_def_cfa_offset 32
Ltmp7:
	.cfi_offset %rbx, -32
Ltmp8:
	.cfi_offset %r14, -24
Ltmp9:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	_unmask
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	_unmasktype
	cmpq	$5, %rax
	jne	LBB0_3
## BB#1:                                ## %Continue1
	cmpb	$1, 16(%rbx)
	jne	LBB0_3
## BB#2:                                ## %Continue2
	movq	(%rbx), %rax
	movq	8(%rbx), %rsi
	xorl	%edi, %edi
	movq	%r14, %rdx
	callq	*%rax
	popq	%rbx
	popq	%r14
	popq	%r15
	ret
LBB0_3:                                 ## %Error
	movl	$-1, %edi
	callq	_exit
	.cfi_endproc

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	pushq	%rax
Ltmp11:
	.cfi_def_cfa_offset 16
	movl	$26, %edi
	callq	_malloc
	movq	$17, 24(%rax)
	movb	$0, 16(%rax)
	movq	$16, 8(%rax)
	movq	$15, (%rax)
	movq	%rax, %rdi
	callq	_test
	movl	$1, %eax
	popq	%rdx
	ret
	.cfi_endproc

	.globl	_test
	.align	4, 0x90
_test:                                  ## @test
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp19:
	.cfi_def_cfa_offset 16
	pushq	%r15
Ltmp20:
	.cfi_def_cfa_offset 24
	pushq	%r14
Ltmp21:
	.cfi_def_cfa_offset 32
	pushq	%r13
Ltmp22:
	.cfi_def_cfa_offset 40
	pushq	%r12
Ltmp23:
	.cfi_def_cfa_offset 48
	pushq	%rbx
Ltmp24:
	.cfi_def_cfa_offset 56
	pushq	%rax
Ltmp25:
	.cfi_def_cfa_offset 64
Ltmp26:
	.cfi_offset %rbx, -56
Ltmp27:
	.cfi_offset %r12, -48
Ltmp28:
	.cfi_offset %r13, -40
Ltmp29:
	.cfi_offset %r14, -32
Ltmp30:
	.cfi_offset %r15, -24
Ltmp31:
	.cfi_offset %rbp, -16
	movq	%rdi, %rbx
	movq	32(%rbx), %r14
	movq	24(%rbx), %r15
	movzbl	16(%rbx), %r13d
	movq	(%rbx), %rsi
	movq	8(%rbx), %rbp
	leaq	L_.strval(%rip), %r12
	movq	%r12, %rdi
	xorb	%al, %al
	callq	_printf
	movq	%r12, %rdi
	movq	%rbp, %rsi
	xorb	%al, %al
	callq	_printf
	movq	%r12, %rdi
	movl	%r13d, %esi
	xorb	%al, %al
	callq	_printf
	movq	%r12, %rdi
	movq	%r15, %rsi
	xorb	%al, %al
	callq	_printf
	movq	%r12, %rdi
	movq	%r14, %rsi
	xorb	%al, %al
	callq	_printf
	movq	%rbx, %rdi
	callq	_free
	movl	$1, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	ret
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_.strval:                              ## @.strval
	.asciz	 "%d\n"


.subsections_via_symbols
