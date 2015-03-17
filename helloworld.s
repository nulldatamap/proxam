	.file	"helloworld.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	push	rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	mov	edi, 31
	call	fib
	mov	edi, eax
	call	print_int
	pop	rax
	ret
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc

	.globl	fib
	.align	16, 0x90
	.type	fib,@function
fib:                                    # @fib
	.cfi_startproc
# BB#0:
	push	rbp
.Ltmp6:
	.cfi_def_cfa_offset 16
	push	rbx
.Ltmp7:
	.cfi_def_cfa_offset 24
	push	rax
.Ltmp8:
	.cfi_def_cfa_offset 32
.Ltmp9:
	.cfi_offset rbx, -24
.Ltmp10:
	.cfi_offset rbp, -16
	mov	ebx, edi
	cmp	ebx, 2
	jg	.LBB1_3
# BB#1:                                 # %_then
	xor	eax, eax
	test	ebx, ebx
	js	.LBB1_4
# BB#2:                                 # %_else2
	mov	eax, 1
	jmp	.LBB1_4
.LBB1_3:                                # %_else
	lea	edi, dword ptr [rbx - 1]
	call	fib
	mov	ebp, eax
	add	ebx, -2
	mov	edi, ebx
	call	fib
	add	eax, ebp
.LBB1_4:                                # %_merge
	add	rsp, 8
	pop	rbx
	pop	rbp
	ret
.Ltmp11:
	.size	fib, .Ltmp11-fib
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
