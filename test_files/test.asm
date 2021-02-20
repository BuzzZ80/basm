const print = 0x105

start:
	mov ac, message
	jsr print
	xor 0x024
	hlt

; data
message:
	db "hello world\n\0"
	
