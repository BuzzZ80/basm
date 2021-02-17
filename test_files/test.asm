const print = 0x105
const 

start:
	mov ac, message
	jsr print
	hlt

; data
message:
	db "hello world\n\0"
	
