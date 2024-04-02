segment data
	STR_VOWEL:
	bytes "Vowel\0"

	STR_NOT_VOWEL:
	bytes "Not vowel\0"

	STR_EMAIL:
	bytes "zili.luo@student.manchester.ac.uk\0"

	STR_ERROR:
	bytes "Error\0"

	STR_PROMPT:
	bytes "Enter an integer: \0"

	STR_FMT:
	bytes "%d\0"

segment text
	; r0: c
	; r1: cond
	; r2: tmp cond
	; r3: tmp for compare
	; r4: tmp for masking status
	; r9: STR_NOT_VOWEL
	; r10: STR_VOWEL
	; r11: sizeof(STR_EMAIL)
	; r13: STR_EMAIL

	load_imm	q r0, 8
	add		q sp, sp, r0

	load_imm	q r0, STR_PROMPT
	vtoreal		r0, r0
	libc_call	printf
	
	load_imm	q r0, 0
	vtoreal		r1, r0
	load_imm	q r0, STR_FMT
	vtoreal		r0, r0
	libc_call	scanf

	load_imm	q r11, 33	; 33 == sizeof(STR_EMAIL)
	load_imm	q r13, STR_EMAIL

	; c = email[i]
	load_imm	q r12, 0
	load_dir	q r12, r12, vmem
	add		q r0, r13, r12
	load_dir	b r0, r0, vmem

	; check bound
	cmp		q r11, r12
	b		_out_of_bound_end, g
	load_imm	q r0, STR_ERROR
	vtoreal		r0, r0
	libc_call	printf
	load_imm	q r0, 1
	libc_call	exit
	_out_of_bound_end:

	load_imm	b r4, 0b00010000

	; cond = (c == 'a')
	load_imm	b r3, 'a'
	cmp		b r0, r3
	mov		b r1, status
	and		b r1, r1, r4

	; cond |= (c == 'e')
	load_imm	b r3, 'e'
	cmp		b r0, r3
	mov		b r2, status
	and		b r2, r2, r4
	or		b r1, r1, r2

	; cond |= (c == 'i')
	load_imm	b r3, 'i'
	cmp		b r0, r3
	mov		b r2, status
	and		b r2, r2, r4
	or		b r1, r1, r2

	; cond |= (c == 'o')
	load_imm	b r3, 'o'
	cmp		b r0, r3
	mov		b r2, status
	and		b r2, r2, r4
	or		b r1, r1, r2

	; cond |= (c == 'u')
	load_imm	b r3, 'u'
	cmp		b r0, r3
	mov		b r2, status
	and		b r2, r2, r4
	or		b r1, r1, r2

	load_imm	q r9, STR_NOT_VOWEL
	load_imm	q r10, STR_VOWEL
	cmp		b r1, r1
	csel		q r0, r9, r10, z
	vtoreal		r0, r0
	libc_call	printf

	brk
