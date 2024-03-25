start:
  store_imm byte r0, vmem, 1000
  b start, z
  ret
