	.text
	.globl _scheme_entry
_scheme_entry:
	pushq %rbp
	movq %rsp, %rbp
	movl $42.01, %eax
	popq %rbp
	ret
