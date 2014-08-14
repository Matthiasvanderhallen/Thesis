	.section	__TEXT,__text,regular,pure_instructions
	.globl	_mask
	.align	4, 0x90
_mask:                                  ## @mask
	.cfi_startproc
## BB#0:
	pushq	%rax
Ltmp1:
	.cfi_def_cfa_offset 16
	leaq	L_vtable(%rip), %rdx
	xorl	%ecx, %ecx
	callq	L_mask_rec
	popq	%rdx
	ret
	.cfi_endproc

	.align	4, 0x90
L_mask_rec:                             ## @mask_rec
	.cfi_startproc
## BB#0:
	pushq	%r15
Ltmp7:
	.cfi_def_cfa_offset 16
	pushq	%r14
Ltmp8:
	.cfi_def_cfa_offset 24
	pushq	%r12
Ltmp9:
	.cfi_def_cfa_offset 32
	pushq	%rbx
Ltmp10:
	.cfi_def_cfa_offset 40
	pushq	%rax
Ltmp11:
	.cfi_def_cfa_offset 48
Ltmp12:
	.cfi_offset %rbx, -40
Ltmp13:
	.cfi_offset %r12, -32
Ltmp14:
	.cfi_offset %r14, -24
Ltmp15:
	.cfi_offset %r15, -16
	movq	%rcx, %rbx
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	(%r12), %rdx
	testq	%rdx, %rdx
	jne	LBB1_2
## BB#1:                                ## %Add
	movl	$24, %edi
	callq	_malloc
	movq	%rax, (%r12)
	movq	%r15, (%rax)
	movq	$0, 8(%rax)
	movq	%r14, 16(%rax)
	jmp	LBB1_3
LBB1_2:                                 ## %Valcheck
	cmpq	%r15, (%rdx)
	jne	LBB1_4
LBB1_3:                                 ## %Return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	ret
LBB1_4:                                 ## %Loop
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
	.cfi_endproc

	.globl	_unmask
	.align	4, 0x90
_unmask:                                ## @unmask
	.cfi_startproc
## BB#0:
	pushq	%rax
Ltmp17:
	.cfi_def_cfa_offset 16
	leaq	L_vtable(%rip), %rsi
	callq	L_unmask_rec
	popq	%rdx
	ret
	.cfi_endproc

	.align	4, 0x90
L_unmask_rec:                           ## @unmask_rec
	.cfi_startproc
## BB#0:
	pushq	%rax
Ltmp19:
	.cfi_def_cfa_offset 16
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	je	LBB3_3
## BB#1:                                ## %ZeroTest
	testq	%rdi, %rdi
	je	LBB3_4
## BB#2:                                ## %Loop
	decq	%rdi
	addq	$8, %rsi
	popq	%rax
	jmp	L_unmask_rec            ## TAILCALL
LBB3_4:                                 ## %RetVal
	movq	(%rsi), %rax
	popq	%rdx
	ret
LBB3_3:                                 ## %Error
	movl	$-1, %edi
	callq	_exit
	.cfi_endproc

	.globl	_unmasktype
	.align	4, 0x90
_unmasktype:                            ## @unmasktype
	.cfi_startproc
## BB#0:
	pushq	%rax
Ltmp21:
	.cfi_def_cfa_offset 16
	leaq	L_vtable(%rip), %rsi
	callq	L_unmasktype_rec
	popq	%rdx
	ret
	.cfi_endproc

	.align	4, 0x90
L_unmasktype_rec:                       ## @unmasktype_rec
	.cfi_startproc
## BB#0:
	pushq	%rax
Ltmp23:
	.cfi_def_cfa_offset 16
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	je	LBB5_3
## BB#1:                                ## %ZeroTest
	testq	%rdi, %rdi
	je	LBB5_4
## BB#2:                                ## %Loop
	decq	%rdi
	addq	$8, %rsi
	popq	%rax
	jmp	L_unmasktype_rec        ## TAILCALL
LBB5_4:                                 ## %RetVal
	movq	16(%rsi), %rax
	popq	%rdx
	ret
LBB5_3:                                 ## %Error
	movl	$-1, %edi
	callq	_exit
	.cfi_endproc

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	pushq	%rbx
Ltmp26:
	.cfi_def_cfa_offset 16
Ltmp27:
	.cfi_offset %rbx, -16
	xorl	%edi, %edi
	movl	$1, %esi
	callq	_mask
	leaq	L_.strval(%rip), %rbx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	xorb	%al, %al
	callq	_printf
	movl	$5, %edi
	movl	$1, %esi
	callq	_mask
	movq	%rbx, %rdi
	movq	%rax, %rsi
	xorb	%al, %al
	callq	_printf
	movl	$2, %edi
	movl	$1, %esi
	callq	_mask
	movq	%rbx, %rdi
	movq	%rax, %rsi
	xorb	%al, %al
	callq	_printf
	movl	$5, %edi
	movl	$1, %esi
	callq	_mask
	movq	%rbx, %rdi
	movq	%rax, %rsi
	xorb	%al, %al
	callq	_printf
	movl	$1, %edi
	callq	_unmask
	movq	%rbx, %rdi
	movq	%rax, %rsi
	xorb	%al, %al
	callq	_printf
	movl	$1, %edi
	callq	_unmasktype
	movq	%rbx, %rdi
	movq	%rax, %rsi
	xorb	%al, %al
	callq	_printf
	xorl	%eax, %eax
	popq	%rbx
	ret
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_.strval:                              ## @.strval
	.asciz	 "%d\n"

L_.nonnull:                             ## @.nonnull
	.asciz	 "nonnull\n"

.zerofill __DATA,__bss,L_vtable,8,3     ## @vtable

.subsections_via_symbols
