; $Id: 4220anlg.inc,v 1.1 2005/02/08 22:02:10 tgipson Exp $

; Stolen from lcmansen.inc 2005/02/07 by Tim Gipson for ACONDv2
; lcmansen.inc created 2004/01/29 by Tim Gipson for LCMEM::SDR
; PIC18F4220 (and others?) ADC channel read functions


;------------------------------------------------------------------------
; Reads analog channel stored in WREG (read_int_ADC_chan).
;	or
; Reads currently selected channel (read_int_ADC).
; Result returned in ANALOGH:ANALOGL

read_int_ADC_chan:
	call	set_int_ADC_chan

read_int_ADC:
	bsf		ADCON0, 1					; start conversion

read_int_ADC_wait:
	btfsc	ADCON0, 1					; conversion finished?
	goto	read_int_ADC_wait			; if no

	movf	ADRESH,w
	movwf	ANALOGH
	;call	SENDHEX
	movf	ADRESL,w
	movwf	ANALOGL
	;call	SENDHEX
	;call	SENDCMNDP2CR
	return


; -------------------------------------------------------------------------
; Write the analog channel # in WREG to ADCON0 (shifted per ADCON0 requirements), and turn A/D module on

set_int_ADC_chan:
	bcf		STATUS, C		; shift channel # left 2
	rrcf	WREG			; ...
	bcf		STATUS, C		;
	rrcf	WREG			;
	bsf		WREG, 0			; will turn A/D module on
	movwf	ADCON0
	return

; -------------------------------------------------------------------------
; Setup justification and acq. time

init_int_ADC:
	movlw	B'10111110'		; Right justified, acquisition time 20AD (slowest), conversion clock Fosc/64 (slowest)
	movwf	ADCON2
	return
