; $Id: vars.inc,v 1.2 2008/02/18 20:26:26 tgipson Exp $
; Memory register definitions for Blinkenlichten


	cblock	0x10

	SCRATCH0	; general purpose scratch register
	COUNT		; bits-in-byte countdown

	PWM_R		; poor-man's PWM registers: '1' bits will be rotated around in them
	PWM_G		;
	PWM_B		;

	GROUPADDR	; Storage byte for Group Address

	DEF_R		; Deferred Update regs
	DEF_G		;
	DEF_B		;
	DEF_VALID	; xxxxxRGB Flags indicate whether DEFx bufs contain valid update data

	CMDBUF		; buffer start - must be at end of vars
	CMDBUF2		; 2nd byte of cmd
	endc
