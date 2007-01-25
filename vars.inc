; Vars


	cblock	0x10

	SCRATCH0	; general purpose scratch register
	COUNT		; bits-in-byte countdown

	PWM_R		; poor-man's PWM registers: '1' bits will be rotated around in them
	PWM_G		;
	PWM_B		;

	GROUPADDR	; Storage byte for Group Address

	CMDBUF		; buffer start - must be at end of vars
	CMDBUF2		; 2nd byte of cmd
	endc
