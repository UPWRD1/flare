
examples/ntest:	file format mach-o arm64

Disassembly of section __TEXT,__text:

0000000100000328 <_flare_f_0>:
100000328: 1e202008    	fcmp	s0, #0.0
10000032c: 1e2e1001    	fmov	s1, #1.00000000
100000330: 54000061    	b.ne	0x10000033c <_flare_f_0+0x14>
100000334: 1e204020    	fmov	s0, s1
100000338: d65f03c0    	ret
10000033c: 6dbe23e9    	stp	d9, d8, [sp, #-0x20]!
100000340: 1e3e1001    	fmov	s1, #-1.00000000
100000344: 1e204008    	fmov	s8, s0
100000348: a9017bfd    	stp	x29, x30, [sp, #0x10]
10000034c: 1e212801    	fadd	s1, s0, s1
100000350: 1e204020    	fmov	s0, s1
100000354: 97fffff5    	bl	0x100000328 <_flare_f_0>
100000358: 1e200901    	fmul	s1, s8, s0
10000035c: a9417bfd    	ldp	x29, x30, [sp, #0x10]
100000360: 6cc223e9    	ldp	d9, d8, [sp], #0x20
100000364: 1e204020    	fmov	s0, s1
100000368: d65f03c0    	ret

000000010000036c <_flare_f_1>:
10000036c: d65f03c0    	ret

0000000100000370 <_flare_f_2>:
100000370: d61f0000    	br	x0

0000000100000374 <_flare_f_3>:
100000374: 6dbe23e9    	stp	d9, d8, [sp, #-0x20]!
100000378: 1e221000    	fmov	s0, #4.00000000
10000037c: a9017bfd    	stp	x29, x30, [sp, #0x10]
100000380: 97ffffea    	bl	0x100000328 <_flare_f_0>
100000384: 1e204008    	fmov	s8, s0
100000388: 1e229000    	fmov	s0, #5.00000000
10000038c: 97ffffe7    	bl	0x100000328 <_flare_f_0>
100000390: a9417bfd    	ldp	x29, x30, [sp, #0x10]
100000394: 1e204100    	fmov	s0, s8
100000398: 6cc223e9    	ldp	d9, d8, [sp], #0x20
10000039c: d65f03c0    	ret

00000001000003a0 <_main>:
1000003a0: 6dbe23e9    	stp	d9, d8, [sp, #-0x20]!
1000003a4: 1e221000    	fmov	s0, #4.00000000
1000003a8: a9017bfd    	stp	x29, x30, [sp, #0x10]
1000003ac: 97ffffdf    	bl	0x100000328 <_flare_f_0>
1000003b0: 1e204008    	fmov	s8, s0
1000003b4: 1e229000    	fmov	s0, #5.00000000
1000003b8: 97ffffdc    	bl	0x100000328 <_flare_f_0>
1000003bc: a9417bfd    	ldp	x29, x30, [sp, #0x10]
1000003c0: 1e380100    	fcvtzs	w0, s8
1000003c4: 6cc223e9    	ldp	d9, d8, [sp], #0x20
1000003c8: d65f03c0    	ret
