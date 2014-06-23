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
	.quad	0
	.globl	_camlQueue
_camlQueue:
	.text
	.align	4
	.globl	_camlQueue__entry
_camlQueue__entry:
	.cfi_startproc
.L100:
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
	.quad	0
