






; --------------------
; Test Accelerometer input

accel_test:
	call	init_int_ADC	; for the accelerometer that isn't attached yet...
	bsf		SDIO, ACCEL_PWR	; power up accelerometer
	call	delaysec		; wait...


test_adc:
	movlw	H'00'
	call	read_int_ADC_chan
	movf	ANALOGH, w
	call	SENDHEX
	movf	ANALOGL, w
	call	SENDHEX
	call	SENDCMNDP2CR

	movlw	H'0A'
	addwf	ANALOGL
	clrf	WREG
	addwfc	ANALOGH


	btfsc	ANALOGH, 0		; 'sign'
	goto	test_adc_pos

test_adc_neg:
	movlw	B'00000100'
	movwf	COLOR
	comf	ANALOGL		; invert LSByte
	goto	test_adc_show

test_adc_pos:
	movlw	B'00000001'
	movwf	COLOR	

test_adc_show:
	movlw	B'00001111'
	andwf	ANALOGL
	bcf		STATUS, C
	rrcf	ANALOGL				; scale /2
	bcf		STATUS, C
	rrcf	ANALOGL				; scale /2
	bcf		STATUS, C
	rrcf	ANALOGL				; scale /2

	movff	ANALOGL, INTENS
	call	sendcmdbyte
	comf	COLOR
	clrf	INTENS
	call	sendcmdbyte

	goto	test_adc

	return						; dummy return instruction (intentionally unreachable)


; -------------------------------------
; This was 'main' during initial testing, but actually just some debug stuff...

main:
	call	ui_show_addr
	call	ui_main_menu

	movlw	'\r'			; carriage return
	xorwf	SCRATCH0, w
	bz		intensloop

	movlw	B'11100000'		; "all colors off"
	movwf	COMMAND
	call	send1wire

	movlw	B'00000111'
	andwf	COLOR

	movf	COLOR, w
	call	SENDBIN
	call	SENDCMNDP2CR

	movf	INTENS, w
	call	SENDBIN
	call	SENDCMNDP2CR

	call	sendcmdbyte

	goto	main

intensloop:
	movlw	H'0A'
	movwf	SCRATCH1
;	clrf	INTENS
	movlw	H'00'
	movwf	INTENS

intensloop_up:
	incf	INTENS
	call	sendcmdbyte
	decfsz	SCRATCH1
	goto	intensloop_up

;	call	delay20sec		; delay to see what happened...
;	comf	ADDR

intensloop2:
	movlw	H'0A'
	movwf	SCRATCH1
intensloop_dn:
	decf	INTENS
	call	sendcmdbyte
	decfsz	SCRATCH1
	goto	intensloop_dn

colorchg:
;	call	delay20sec		; delay to see what happened...
;	call	delaysec
	call	delaysec

	incf	COLOR

;	goto	main
	goto	intensloop
