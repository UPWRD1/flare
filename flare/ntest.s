.text
.globl out
out:
	pushq %rbp
	movq %rsp, %rbp
	callq puts
	leave
	ret
.type out, @function
.size out, .-out
/* end function out */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	leaq string.7(%rip), %rdi
	callq out
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.data
.balign 8
.globl string.7
string.7:
	.byte 34
	.ascii "3 and 7 is:"
	.byte 34
	.byte 0
/* end data */

.section .note.GNU-stack,"",@progbits
