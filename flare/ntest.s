.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	movl $9, %eax
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.section .note.GNU-stack,"",@progbits
