	.section	__TEXT,__text,regular,pure_instructions
	.globl	_newcredentials
	.align	4, 0x90
_newcredentials:                        ## @newcredentials
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rax
Ltmp1:
	.cfi_def_cfa_offset 16
	callq	L_rand
	popq	%rdx
	ret
	.cfi_endproc

	.globl	_encrypt
	.align	4, 0x90
_encrypt:                               ## @encrypt
	.cfi_startproc
## BB#0:                                ## %entry
	leal	(%rdi,%rsi), %eax
	ret
	.cfi_endproc

	.globl	_decrypt
	.align	4, 0x90
_decrypt:                               ## @decrypt
	.cfi_startproc
## BB#0:                                ## %entry
	subl	%esi, %edi
	movl	%edi, %eax
	ret
	.cfi_endproc

	.align	4, 0x90
L_rand:                                 ## @rand
	.cfi_startproc
## BB#0:                                ## %entry
	movl	L_.seed(%rip), %eax
	ret
	.cfi_endproc

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	pushq	%rbx
Ltmp4:
	.cfi_def_cfa_offset 16
Ltmp5:
	.cfi_offset %rbx, -16
	leaq	L_.seed(%rip), %rbx
	movq	%rbx, %rdi
	callq	_puts
	movq	%rbx, %rdi
	callq	_puts
	movl	$-1, %eax
	popq	%rbx
	ret
	.cfi_endproc

	.section	__TEXT,__const
	.align	2                       ## @.seed
L_.seed:
	.long	3                       ## 0x3


.subsections_via_symbols
