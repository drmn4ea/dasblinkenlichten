; $Id: $


; Routines for 1-wire write-only interface.

; -------------------------------------------------
; Send byte in WREG on 1-wire interface
; Assumes master clock running at 2x of slave clock

#define		delay_one	delay_40
#define		delay_zero	delay_20
;#define		delay_dt	delaybit

#define		delay_dt	delay_68

send1wire:
	call	send1wire_start
	movf	ADDR, w
	call	send1wire_byte
	movf	COMMAND, w
	call	send1wire_byte

;	call	delaybit		; long delay forced for WDT resets
;	call	delaybit
;	call	delaybit
;	call	delaybit
;	call	delaybit
;	call	delaybit
;	call	delaybit
;	call	delaybit
;	call	delaybit		; long delay forced for WDT resets
;	call	delaybit
;	call	delaybit
;	call	delaybit
;	call	delaybit
;	call	delaybit
;	call	delaybit
;	call	delaybit
	return


; -------------------------------

send1wire_byte:
	movwf	RWDATA
	movlw	H'08'
	movwf	ROTATE

send1wire2:
	bsf		SDIO, S_DO		; Start next bit
	call	delay_zero		; delay for '0' bit
	rlcf	RWDATA			; shift leftmost bit into carry
	btfsc	STATUS, C		; is 1?
	call	delay_one		; if yes - delay longer for '1' bit
	bcf		SDIO, S_DO		; end bit
;	call	delayhalfbit	; ...

	call	delay_dt		; long delay for extra PWM

	decfsz	ROTATE
	goto	send1wire2


;	bcf		SDIO, S_DO
;	call	delaybit
	return



; -------------------------------

send1wire_start:
	bsf		SDIO, S_DO		; Start condition
	call	delaybit		; = 1 timer rollover on slaves
;	call	delaybit		; ...
;	call	delaybit		; ...
;	call	delaybit		; ...
;	call	delaybit		; ...
	bcf		SDIO, S_DO		; end start condition
	call	delay_dt

;	bsf		SDIO, S_DO		; end start condition
;	call	delayhalfbit
;	bcf		SDIO, S_DO		; end start condition
	return
