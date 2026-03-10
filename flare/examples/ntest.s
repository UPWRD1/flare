
examples/ntest:	file format mach-o arm64

Disassembly of section __TEXT,__text:

0000000100000378 <_flare_f_0>:
100000378: d65f03c0    	ret

000000010000037c <_flare_f_1>:
10000037c: a9be4ff4    	stp	x20, x19, [sp, #-0x20]!
100000380: a9017bfd    	stp	x29, x30, [sp, #0x10]
100000384: 2a0003f3    	mov	w19, w0
100000388: d63f0020    	blr	x1
10000038c: a9417bfd    	ldp	x29, x30, [sp, #0x10]
100000390: 2a1303e0    	mov	w0, w19
100000394: a8c24ff4    	ldp	x20, x19, [sp], #0x20
100000398: d65f03c0    	ret

000000010000039c <_flare_f_2>:
10000039c: d65f03c0    	ret

00000001000003a0 <_flare_f_3>:
1000003a0: a9bf7bfd    	stp	x29, x30, [sp, #-0x10]!
1000003a4: 97fffffe    	bl	0x10000039c <_flare_f_2>
1000003a8: 2a1f03e0    	mov	w0, wzr
1000003ac: a8c17bfd    	ldp	x29, x30, [sp], #0x10
1000003b0: d65f03c0    	ret

00000001000003b4 <_flare_f_5>:
1000003b4: 1e202800    	fadd	s0, s0, s0
1000003b8: d65f03c0    	ret

00000001000003bc <_flare_f_4>:
1000003bc: a9bf7bfd    	stp	x29, x30, [sp, #-0x10]!
1000003c0: 1e249000    	fmov	s0, #10.00000000
1000003c4: 97fffff7    	bl	0x1000003a0 <_flare_f_3>
1000003c8: 90000001    	adrp	x1, 0x100000000
1000003cc: 910ed021    	add	x1, x1, #0x3b4
1000003d0: 97ffffeb    	bl	0x10000037c <_flare_f_1>
1000003d4: 97ffffe9    	bl	0x100000378 <_flare_f_0>
1000003d8: 1e369001    	fmov	s1, #-20.00000000
1000003dc: 1e212800    	fadd	s0, s0, s1
1000003e0: a8c17bfd    	ldp	x29, x30, [sp], #0x10
1000003e4: d65f03c0    	ret

00000001000003e8 <_main>:
1000003e8: a9bf7bfd    	stp	x29, x30, [sp, #-0x10]!
1000003ec: 97fffff4    	bl	0x1000003bc <_flare_f_4>
1000003f0: 1e380000    	fcvtzs	w0, s0
1000003f4: a8c17bfd    	ldp	x29, x30, [sp], #0x10
1000003f8: d65f03c0    	ret
