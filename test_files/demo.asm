const mem = 0o4000

;const A = mem + 0
;const B = mem + 2
;const C = mem + 4

const A1 = 0o4000
const A2 = 0o4001
const B1 = 0o4002
const B2 = 0o4003
const C1 = 0o4004
const C2 = 0o4005

const We = 0o4010

MOV Ix, 0o37		;Ammount of memory to clear
clrMem:			;Clear memory
 MOV (mem+Ix), 0
 DEC Ix
 JMP clrMem -nz
INC (A1)		;Set up A
MOV Sp, 0o7777		;Set up the stack

addLoop:
 JSR Output		;Display C
 
 MOV Ac, (A1)		;Add A+B
 ADD (B1)		;
 MOV (A1), Ac		;
 			;
 MOV Ac, (A2)		;
 ADC (B2)		;
 MOV (A2), Ac		;
 
 MOV (B1), (C1)		;MOV C into B
 MOV (B2), (C2)		;
 
 MOV (C1), (A1)		;MOV A into C
 MOV (C2), (A2)		;

 JMP addLoop -nc	;Loop if no overflow
HLT			;end the program

Output:			;Displays C
 MOV (We), 1
 MOV (We), 0
 RET
HLT
