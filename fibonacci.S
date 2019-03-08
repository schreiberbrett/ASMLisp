main:
	addi x1, zero, 1
	addi x2, zero, 2
	addi x5, zero, 100

	addi x4, zero, 0
L0:
	bge x4, x5, L1
	add x3, x1, x2
	add x1, zero, x2
	add x2, zero, x4
	j L0
L1: