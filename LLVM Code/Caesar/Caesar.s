	.section	__TEXT,__text,regular,pure_instructions
	.globl	_Caesar.decrypt
	.align	4, 0x90
_Caesar.decrypt:                        ## @Caesar.decrypt
	.cfi_startproc
## BB#0:
	jmp	L_Caesar.decrypt_stub   ## TAILCALL
	.cfi_endproc

	.globl	_Caesar.encrypt
	.align	4, 0x90
_Caesar.encrypt:                        ## @Caesar.encrypt
	.cfi_startproc
## BB#0:
	jmp	L_Caesar.encrypt_stub   ## TAILCALL
	.cfi_endproc

	.align	4, 0x90
L_Caesar.newcredentials:                ## @Caesar.newcredentials
	.cfi_startproc
## BB#0:
	jmp	L_Caesar.newcredentials_stub ## TAILCALL
	.cfi_endproc

	.align	4, 0x90
L_Caesar.decrypt_stub:                  ## @Caesar.decrypt_stub
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
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	%rbx, %rdi
	callq	_unmask
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	_unmasktype
	cmpq	$4, %rax
	jne	LBB3_2
## BB#1:                                ## %Continue
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	L_Caesar.decrypt_internal
	popq	%rbx
	popq	%r14
	popq	%r15
	ret
LBB3_2:                                 ## %Error
	movl	$-1, %edi
	callq	_exit
	.cfi_endproc

	.align	4, 0x90
L_Caesar.decrypt_internal:              ## @Caesar.decrypt_internal
	.cfi_startproc
## BB#0:
	subq	(%rsi), %rdi
	movl	$26, %eax
	xorl	%edx, %edx
	divq	%rdi
	movq	%rdx, %rax
	ret
	.cfi_endproc

	.align	4, 0x90
L_Caesar.encrypt_stub:                  ## @Caesar.encrypt_stub
	.cfi_startproc
## BB#0:
	pushq	%r15
Ltmp14:
	.cfi_def_cfa_offset 16
	pushq	%r14
Ltmp15:
	.cfi_def_cfa_offset 24
	pushq	%rbx
Ltmp16:
	.cfi_def_cfa_offset 32
Ltmp17:
	.cfi_offset %rbx, -32
Ltmp18:
	.cfi_offset %r14, -24
Ltmp19:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	%rbx, %rdi
	callq	_unmask
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	_unmasktype
	cmpq	$4, %rax
	jne	LBB5_2
## BB#1:                                ## %Continue
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	L_Caesar.encrypt_internal
	popq	%rbx
	popq	%r14
	popq	%r15
	ret
LBB5_2:                                 ## %Error
	movl	$-1, %edi
	callq	_exit
	.cfi_endproc

	.align	4, 0x90
L_Caesar.encrypt_internal:              ## @Caesar.encrypt_internal
	.cfi_startproc
## BB#0:
	addq	(%rsi), %rdi
	movl	$26, %eax
	xorl	%edx, %edx
	divq	%rdi
	movq	%rdx, %rax
	ret
	.cfi_endproc

	.align	4, 0x90
L_Caesar.newcredentials_stub:           ## @Caesar.newcredentials_stub
	.cfi_startproc
## BB#0:
	pushq	%rax
Ltmp21:
	.cfi_def_cfa_offset 16
	callq	L_Caesar.newcredentials_internal
	movq	%rax, %rdi
	movl	$4, %esi
	callq	_mask
	popq	%rdx
	ret
	.cfi_endproc

	.align	4, 0x90
L_Caesar.newcredentials_internal:       ## @Caesar.newcredentials_internal
	.cfi_startproc
## BB#0:
	pushq	%rbx
Ltmp24:
	.cfi_def_cfa_offset 16
Ltmp25:
	.cfi_offset %rbx, -16
	callq	L_Caesar.rand
	movq	%rax, %rbx
	movl	$16, %edi
	callq	_malloc
	movq	%rbx, (%rax)
	movq	$4, (%rax)
	popq	%rbx
	ret
	.cfi_endproc

	.align	4, 0x90
L_Caesar.rand:                          ## @Caesar.rand
	.cfi_startproc
## BB#0:
	pushq	%rax
Ltmp27:
	.cfi_def_cfa_offset 16
	callq	L_Caesar.seed
	popq	%rdx
	ret
	.cfi_endproc

	.align	4, 0x90
L_Caesar.seed:                          ## @Caesar.seed
	.cfi_startproc
## BB#0:
	movl	$3, %eax
	ret
	.cfi_endproc

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	xorl	%eax, %eax
	ret
	.cfi_endproc


.subsections_via_symbols
