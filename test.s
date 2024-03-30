; r13: email (cstring)
; r12: i
; r11: sizeof(email)

start:
	; r0: c
	; r1: cond
	; r2: tmp cond
	; r3: tmp for compare
	; r4: tmp for masking status

	; c = email[i]
	add		qword r0, r13, r12
	load_dir	byte r0, r0, real

	breakpoint

	load_imm	byte r4, 0b00010000

	; cond = (c == 'a')
	load_imm	byte r3, 'a'
	cmp		byte r0, r3
	mov		byte r1, status
	and		byte r1, r1, r4

	; cond |= (c == 'e')
	load_imm	byte r3, 'e'
	cmp		byte r0, r3
	mov		byte r2, status
	and		byte r2, r2, r4
	or		byte r1, r1, r2

	; cond |= (c == 'i')
	load_imm	byte r3, 'i'
	cmp		byte r0, r3
	breakpoint
	mov		byte r2, status
	and		byte r2, r2, r4
	or		byte r1, r1, r2

	breakpoint

	; cond |= (c == 'o')
	load_imm	byte r3, 'o'
	cmp		byte r0, r3
	mov		byte r2, status
	and		byte r2, r2, r4
	or		byte r1, r1, r2

	; cond |= (c == 'u')
	load_imm	byte r3, 'u'
	cmp		byte r0, r3
	mov		byte r2, status
	and		byte r2, r2, r4
	or		byte r1, r1, r2

	breakpoint

	brk