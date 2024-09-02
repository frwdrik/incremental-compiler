	.text
	.globl	scheme_entry
	.type	scheme_entry, @function
scheme_entry:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$39, %eax
	popq	%rbp
	ret
