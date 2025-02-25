.text
.globl print_int
print_int:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rdi, -16(%rbp)
	leaq string.4(%rip), %rdi
	callq printf
	movl $0, -8(%rbp)
	movq -8(%rbp), %rax
	leave
	ret
.type print_int, @function
.size print_int, .-print_int
/* end function print_int */

.text
.globl out
out:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rdi, %rax
	movq %rsi, %rdi
	movq %rax, -16(%rbp)
	callq puts
	movl $0, -8(%rbp)
	movq -8(%rbp), %rax
	leave
	ret
.type out, @function
.size out, .-out
/* end function out */

.text
.globl new
new:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movl $0, -8(%rbp)
	movq -8(%rbp), %rax
	leave
	ret
.type new, @function
.size new, .-new
/* end function new */

.text
.globl print_str
print_str:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rdi, -16(%rbp)
	leaq string.20(%rip), %rdi
	callq printf
	movl $0, -8(%rbp)
	movq -8(%rbp), %rax
	leave
	ret
.type print_str, @function
.size print_str, .-print_str
/* end function print_str */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	pushq %rbx
	movl $5, %esi
	movl $3, %edi
	callq add
	movl %eax, %ebx
	callq new
	movl %ebx, %esi
	movq %rax, %rdi
	movl %esi, %ebx
	movl $3, %esi
	callq print_int
	movl %ebx, %esi
	movq %rax, %rdi
	movl %esi, %ebx
	leaq string.33(%rip), %rsi
	callq print_str
	movl %ebx, %esi
	movq %rax, %rdi
	movl %esi, %ebx
	movl $5, %esi
	callq print_int
	movl %ebx, %esi
	movq %rax, %rdi
	movl %esi, %ebx
	leaq string.36(%rip), %rsi
	callq print_str
	movl %ebx, %esi
	movq %rax, %rdi
	callq print_int
	movq %rax, %rdi
	leaq string.39(%rip), %rsi
	callq out
	popq %rbx
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.text
.globl add
add:
	pushq %rbp
	movq %rsp, %rbp
	movl %edi, %eax
	addl %esi, %eax
	leave
	ret
.type add, @function
.size add, .-add
/* end function add */

.data
.balign 8
.globl string.4
string.4:
	.ascii "%d"
	.byte 0
/* end data */

.data
.balign 8
.globl string.20
string.20:
	.ascii "%s"
	.byte 0
/* end data */

.data
.balign 8
.globl string.33
string.33:
	.ascii " + "
	.byte 0
/* end data */

.data
.balign 8
.globl string.36
string.36:
	.ascii " = "
	.byte 0
/* end data */

.data
.balign 8
.globl string.39
string.39:
	.byte 0
/* end data */

.section .note.GNU-stack,"",@progbits
