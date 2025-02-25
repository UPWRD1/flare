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
	leaq string.8(%rip), %rdi
	callq printf
	movl $0, -8(%rbp)
	movq -8(%rbp), %rax
	leave
	ret
.type print_str, @function
.size print_str, .-print_str
/* end function print_str */

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
.globl print_int
print_int:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rdi, -16(%rbp)
	leaq string.21(%rip), %rdi
	callq printf
	movl $0, -8(%rbp)
	movq -8(%rbp), %rax
	leave
	ret
.type print_int, @function
.size print_int, .-print_int
/* end function print_int */

.text
.globl wrap
wrap:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movl %edi, -8(%rbp)
	movq -8(%rbp), %rax
	leave
	ret
.type wrap, @function
.size wrap, .-wrap
/* end function wrap */

.text
.globl unwrap
unwrap:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rdi, -8(%rbp)
	movl -8(%rbp), %eax
	leave
	ret
.type unwrap, @function
.size unwrap, .-unwrap
/* end function unwrap */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	pushq %rbx
	pushq %r12
	pushq %r13
	movl $3, %edi
	callq wrap
	movq %rax, %rdi
	movq %rdi, %rbx
	movl $5, %edi
	callq wrap
	movq %rbx, %rdi
	movq %rax, %rbx
	callq unwrap
	movq %rbx, %rdi
	movl %eax, %ebx
	callq unwrap
	movl %ebx, %esi
	movl %eax, %r12d
	movl %esi, %r13d
	movl %r12d, %esi
	movl %r13d, %edi
	callq add
	movl %eax, %ebx
	callq new
	movl %r13d, %esi
	movq %rax, %rdi
	callq print_int
	movl %r12d, %esi
	movq %rax, %rdi
	movl %esi, %r12d
	leaq string.46(%rip), %rsi
	callq print_str
	movl %r12d, %esi
	movq %rax, %rdi
	callq print_int
	movl %ebx, %esi
	movq %rax, %rdi
	movl %esi, %ebx
	leaq string.49(%rip), %rsi
	callq print_str
	movl %ebx, %esi
	movq %rax, %rdi
	callq print_int
	movq %rax, %rdi
	leaq string.52(%rip), %rsi
	callq out
	popq %r13
	popq %r12
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
.globl string.8
string.8:
	.ascii "%s"
	.byte 0
/* end data */

.data
.balign 8
.globl string.21
string.21:
	.ascii "%d"
	.byte 0
/* end data */

.data
.balign 8
.globl string.46
string.46:
	.ascii " + "
	.byte 0
/* end data */

.data
.balign 8
.globl string.49
string.49:
	.ascii " = "
	.byte 0
/* end data */

.data
.balign 8
.globl string.52
string.52:
	.byte 0
/* end data */

.section .note.GNU-stack,"",@progbits
