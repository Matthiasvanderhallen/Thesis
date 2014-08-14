	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	pushq	%rax
Ltmp1:
	.cfi_def_cfa_offset 16
	movl	$8, %edi
	callq	_malloc
	movb	(%rax), %al
	movb	%al, _spm_sp(%rip)
	## InlineAsm Start
	movq _spm_sp(%rip), %rsp
	## InlineAsm End
	xorl	%eax, %eax
	popq	%rdx
	ret
	.cfi_endproc

	.section	__DATA,__data
_spm_sp:                                ## @spm_sp
	.byte	15                      ## 0xf


.subsections_via_symbols
