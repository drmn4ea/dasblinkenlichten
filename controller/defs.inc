; $Id: defs.inc,v 1.2 2005/07/31 21:31:31 tgipson Exp $

; Port and bit definitions

; FakeSDI serial
SDIO		equ	PORTB
SDIOTRIS	equ	TRISB
ACCEL_PWR	equ	1
S_DO		equ	3			; NOTE: SD_I / SD_O defined here relative to PIC, not ADC!
ACCEL_X		equ	0
;ACCEL_Y		equ	2


str_defs_ver:
	DW	"$Id: defs.inc,v 1.2 2005/07/31 21:31:31 tgipson Exp $\r\n\x00"
