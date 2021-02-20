;This is a verry shitty algorithm btw owo
MOV Br, 0o10 ;load multiplier
MOV Ix, 0o42 ;load multiplicand

XOR Ac ;clears the accumulator
CMP Ac, Ix ;Checks if multiplicand is 0
JMP end -z ;if so, end the program with Ac = 0
multLoop:
 ADD Br
 DEC Ix
 JMP multLoop -nz

end:
 HLT ;stops the program, result is now in Ac
