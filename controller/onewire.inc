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
	;call	delaysec	
	;call	delay_dt
	call	delaybit

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

;send1wire_byte:
;	movwf	RWDATA
;	movlw	H'08'
;	movwf	ROTATE
;
;send1wire2:
;	bsf		SDIO, S_DO		; Start next bit
;	call	delay_zero		; delay for '0' bit
;	rlcf	RWDATA			; shift leftmost bit into carry
;	btfsc	STATUS, C		; is 1?
;	call	delay_one		; if yes - delay longer for '1' bit
;	bcf		SDIO, S_DO		; end bit
;;	call	delayhalfbit	; ...
;
;;	call	delay_dt		; long delay for extra PWM
;	call	delay_zero
;
;	decfsz	ROTATE
;	goto	send1wire2
;
;
;;	bcf		SDIO, S_DO
;;	call	delaybit
;	return
;
; zero: short low, long high
; one: long low, short high

send1wire_byte:
	movwf	RWDATA
	movlw	H'08'
	movwf	ROTATE

send1wire2:
	rlcf	RWDATA			; shift leftmost bit into carry
	btfss	STATUS, C		; is it 0?
	goto	send1wire_zero	; if yes
send1wire_one:
	bcf		SDIO, S_DO		; low: first half of bit
	call	delay_10
	;call	delay_8
	call	delay_10
	;call	delay_10

	bsf		SDIO, S_DO		; high: 2nd half of bit
	call	delay_8
	bcf		SDIO, S_DO		; end bit

	decfsz	ROTATE			; done byte?
	goto	send1wire2		; if no - next window please
	return					; if yes

send1wire_zero:
	bcf		SDIO, S_DO		; low: first half of bit
	call	delay_10
	call	delay_10
	;call	delay_8
	bsf		SDIO, S_DO		; high: 2nd half of bit
	call	delay_10
	call	delay_10
	call	delay_10
;	call	delay_10
	bcf		SDIO, S_DO		; end bit

	decfsz	ROTATE			; done byte?
	goto	send1wire2		; if no - next window please
	return					; if yes




;	bcf		SDIO, S_DO		; low: first half of bit
;	call	delay_10		; reference delay time
;
;	bsf		SDIO, S_DO		; high: 2nd half of bit
;	call	delay_8		; first half of delay
;	rlcf	RWDATA			; shift leftmost bit into carry
;	btfss	STATUS, C		; is 0?
;	call	delay_10		; if yes - delay longer for '0' bit
;	bcf		SDIO, S_DO		; end bit
;	decfsz	ROTATE
;	goto	send1wire2
;
;	return


; -------------------------------

;send1wire_start:
;	bsf		SDIO, S_DO		; Start condition
;	call	delaybit		; = 1 timer rollover on slaves
;;	call	delaybit		; ...
;;	call	delaybit		; ...
;;	call	delaybit		; ...
;;	call	delaybit		; ...
;	bcf		SDIO, S_DO		; end start condition
;	call	delay_dt
;
;;	bsf		SDIO, S_DO		; end start condition
;;	call	delayhalfbit
;;	bcf		SDIO, S_DO		; end start condition
;	return





send1wire_start:
	bsf		SDIO, S_DO		; Start condition
	call	delay_10
	call	delay_10
;	call	delaybit		; = 1 timer rollover on slaves
;	call	delaybit		; ...
;	call	delaybit		; ...
;	call	delaybit		; ...
;	call	delaybit		; ...
	bcf		SDIO, S_DO		; end start condition
	;call	delay_dt

;	bsf		SDIO, S_DO		; end start condition
;	call	delayhalfbit
;	bcf		SDIO, S_DO		; end start condition
	return