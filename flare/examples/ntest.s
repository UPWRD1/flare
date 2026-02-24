
examples/ntest:	file format mach-o arm64

Disassembly of section __TEXT,__text:

0000000100000378 <_flare_f_0>:
100000378: bd400000    	ldr	s0, [x0]
10000037c: d65f03c0    	ret

0000000100000380 <_flare_f_1>:
100000380: a9be4ff4    	stp	x20, x19, [sp, #-0x20]!
100000384: a9017bfd    	stp	x29, x30, [sp, #0x10]
100000388: bd400000    	ldr	s0, [x0]
10000038c: aa0003f3    	mov	x19, x0
100000390: aa0803f4    	mov	x20, x8
100000394: d63f0020    	blr	x1
100000398: bd000280    	str	s0, [x20]
10000039c: a9417bfd    	ldp	x29, x30, [sp, #0x10]
1000003a0: 39401268    	ldrb	w8, [x19, #0x4]
1000003a4: 39002288    	strb	w8, [x20, #0x8]
1000003a8: a8c24ff4    	ldp	x20, x19, [sp], #0x20
1000003ac: d65f03c0    	ret

00000001000003b0 <_flare_f_2>:
1000003b0: d65f03c0    	ret

00000001000003b4 <_flare_f_3>:
1000003b4: a9be4ff4    	stp	x20, x19, [sp, #-0x20]!
1000003b8: a9017bfd    	stp	x29, x30, [sp, #0x10]
1000003bc: aa0803f3    	mov	x19, x8
1000003c0: 97fffffc    	bl	0x1000003b0 <_flare_f_2>
1000003c4: a9417bfd    	ldp	x29, x30, [sp, #0x10]
1000003c8: 3900227f    	strb	wzr, [x19, #0x8]
1000003cc: bd000260    	str	s0, [x19]
1000003d0: a8c24ff4    	ldp	x20, x19, [sp], #0x20
1000003d4: d65f03c0    	ret

00000001000003d8 <_flare_f_5>:
1000003d8: 1e202800    	fadd	s0, s0, s0
1000003dc: d65f03c0    	ret

00000001000003e0 <_flare_f_4>:
1000003e0: d10083ff    	sub	sp, sp, #0x20
1000003e4: a9017bfd    	stp	x29, x30, [sp, #0x10]
1000003e8: 1e249000    	fmov	s0, #10.00000000
1000003ec: 910023e8    	add	x8, sp, #0x8
1000003f0: 97fffff1    	bl	0x1000003b4 <_flare_f_3>
1000003f4: 90000001    	adrp	x1, 0x100000000
1000003f8: 910f6021    	add	x1, x1, #0x3d8
1000003fc: 910003e8    	mov	x8, sp
100000400: 910023e0    	add	x0, sp, #0x8
100000404: 97ffffdf    	bl	0x100000380 <_flare_f_1>
100000408: 910003e0    	mov	x0, sp
10000040c: 97ffffdb    	bl	0x100000378 <_flare_f_0>
100000410: 1e369001    	fmov	s1, #-20.00000000
100000414: a9417bfd    	ldp	x29, x30, [sp, #0x10]
100000418: 1e212800    	fadd	s0, s0, s1
10000041c: 910083ff    	add	sp, sp, #0x20
100000420: d65f03c0    	ret

0000000100000424 <_main>:
100000424: a9bf7bfd    	stp	x29, x30, [sp, #-0x10]!
100000428: 97ffffee    	bl	0x1000003e0 <_flare_f_4>
10000042c: 1e380000    	fcvtzs	w0, s0
100000430: a8c17bfd    	ldp	x29, x30, [sp], #0x10
100000434: d65f03c0    	ret
