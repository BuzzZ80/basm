const print = 0x105

start:
	mov ac, message
	jsr print2
	hlt

; data
message:
	db "hello world\n\0"
	
