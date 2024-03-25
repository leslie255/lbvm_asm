; r10: height
; r11: M_PI
; r12: radius

start:
  ; conjugate height if negative
  mov       qword r1, r10        ; r1 = height
  xor       qword r0, r0, r0     ; r0 = 0
  fsub      qword r2, r0, r1     ; r2 = 0 - height
  fadd      qword r13, r0, r1    ; r13 = 0 + height
  csel      qword r9, r1, r2, nn ; r9 = height > 0 ? height : -height

  ; r0 = radius * radius * M_PI * height
  fmul      qword r0, r12, r12
  fmul      qword r0, r0, r11
  fmul      qword r0, r0, r9

  brk