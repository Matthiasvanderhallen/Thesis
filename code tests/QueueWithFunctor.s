	.literal16
	.align	4
_caml_negf_mask:	.quad   0x8000000000000000, 0
	.align	4
_caml_absf_mask:	.quad   0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF
	.data
	.globl	_camlQueue__data_begin
_camlQueue__data_begin:
	.text
	.globl	_camlQueue__code_begin
_camlQueue__code_begin:
	nop
	.data
	.quad	1024
	.globl	_camlQueue
_camlQueue:
	.space	8
	.data
	.quad	2295
_camlQueue__1:
	.quad	_camlQueue__Set_1015
	.quad	3
	.text
	.align	4
	.globl	_camlQueue__Set_1015
_camlQueue__Set_1015:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset	8
.L100:
.L101:	subq	$16, %r15
	movq	_caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L102
	leaq	8(%r15), %rax
	movq	$1024, -8(%rax)
	movq	$1, (%rax)
	addq	$8, %rsp
	.cfi_adjust_cfa_offset	-8
	ret
	.cfi_adjust_cfa_offset	8
.L102:	call	_caml_call_gc
.L103:	jmp	.L101
	.cfi_endproc
	.text
	.align	4
	.globl	_camlQueue__entry
_camlQueue__entry:
	.cfi_startproc
.L104:
	movq	_camlQueue__1@GOTPCREL(%rip), %rax
	movq	_camlQueue@GOTPCREL(%rip), %rbx
	movq	%rax, (%rbx)
	movq	$1, %rax
	ret
	.cfi_endproc
	.data
	.text
	nop
	.globl	_camlQueue__code_end
_camlQueue__code_end:
	.data
	.globl	_camlQueue__data_end
_camlQueue__data_end:
	.long	0
	.globl	_camlQueue__frametable
_camlQueue__frametable:
	.quad	1
	.quad	.L103
	.word	16
	.word	0
	.align	3
