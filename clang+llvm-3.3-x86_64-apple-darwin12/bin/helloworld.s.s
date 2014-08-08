	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rax
Ltmp1:
	.cfi_def_cfa_offset 16
	leaq	L_.str(%rip), %rdi
	callq	_puts
	movl	$0, (%rsp)
	movl	$0, 4(%rsp)
	movl	4(%rsp), %eax
	popq	%rdx
	ret
	.cfi_endproc

	.section	__TEXT,__const
L_.str:                                 ## @.str
	.asciz	 "Hello World!"


.subsections_via_symbols
