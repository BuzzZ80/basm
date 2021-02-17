	MOV Ix, 7
	MOV Ac, 0
	MOV Br, 10
Loop:
	ADD Br
	DEC Ix
	JMP Loop -nz
	HLT
