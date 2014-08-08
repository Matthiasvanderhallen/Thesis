	.section	__TEXT,__text,regular,pure_instructions
	.globl	_mask
	.align	4, 0x90
_mask:                                  ## @mask
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp2:
	.cfi_def_cfa_offset 16
Ltmp3:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp4:
	.cfi_def_cfa_register %rbp
	movl	%edx, %eax
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	jne	LBB0_2
## BB#1:                                ## %Add
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rdx
	movq	%rdx, %rsp
	movq	%rdx, L_vtable(%rip)
	movl	%edi, -16(%rcx)
	movq	$0, -8(%rcx)
	jmp	LBB0_4
LBB0_2:                                 ## %Valcheck
	cmpl	%edi, (%rsi)
	je	LBB0_4
## BB#3:                                ## %Loop
	addq	$8, %rsi
	incl	%eax
	movl	%eax, %edx
	callq	_mask
LBB0_4:                                 ## %Return
	movq	%rbp, %rsp
	popq	%rbp
	ret
	.cfi_endproc

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	pushq	%rbx
Ltmp7:
	.cfi_def_cfa_offset 16
Ltmp8:
	.cfi_offset %rbx, -16
	leaq	L_vtable(%rip), %rbx
	xorl	%edi, %edi
	movq	%rbx, %rsi
	xorl	%edx, %edx
	callq	_mask
	movl	$5, %edi
	movq	%rbx, %rsi
	xorl	%edx, %edx
	callq	_mask
	movl	$2, %edi
	movq	%rbx, %rsi
	xorl	%edx, %edx
	callq	_mask
	movl	$3, %edi
	movq	%rbx, %rsi
	xorl	%edx, %edx
	callq	_mask
	leaq	L_.str(%rip), %rdi
	movl	%eax, %esi
	xorb	%al, %al
	callq	_printf
	xorl	%eax, %eax
	popq	%rbx
	ret
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	 "%d"

.zerofill __DATA,__bss,L_vtable,8,3     ## @vtable

.subsections_via_symbols
