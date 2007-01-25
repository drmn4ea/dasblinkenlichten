; $Id: vars.inc,v 1.2 2005/07/31 21:31:31 tgipson Exp $
str_vars_ver:
	DW	"$Id: vars.inc,v 1.2 2005/07/31 21:31:31 tgipson Exp $"


; Variable (RAM) allocation

; ------------------------------

	cblock 0

	; for fake SPI
		RWDATA
		ROTATE

	; for delay functions
		del3
		del2
		del1
		;waiter
		WAITER

	; for hex conv.
		HEXCONV
		HEXCONV1
		HEXTEMP
		HEXTEMP1
		SERR01
		SERR02

	; scratch
		SCRATCH0
		SCRATCH1
		SCRATCH2
		SCRATCH3
		SCRATCH4
		SCRATCH5

	; ADC result
		ADC3
		ADC2
		ADC1
		ADC0
		;RES_CHANNEL		; channel the result read was from. (DEBUG)


	; for ADC stuff
;		CHANNEL		; current ADC channel
;		MUXCH		; current mux channel
;		NCHAN		; number of channels of interest
;		USER_NAVG	; user-specified number of averages

		ANALOGH
		ANALOGL

	; for EEPROM
		EEADDRH
		EEADDRL
		ROLLOVER

	; blinky
		INTENS
		COLOR
		ADDR
		COMMAND

	; table
		ADDRTBL		; Must be last entry!...

	endc
