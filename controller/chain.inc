

; Node color table in memory:
;    [ADDR] [REDI] [GREENI] [BLUEI]


; FSR1: Color table ptr

; TBLPTRx: Random table ptr

chain:
chain_init:
	lfsr	FSR1, ADDRTBL	; point to start of address table
	movlw	UPPER	str_chain	; point to start of address chain in ROM
	movwf	TBLPTRU				; ...
	movlw	HIGH	str_chain
	movwf	TBLPTRH
	movlw	LOW		str_chain
	movwf	TBLPTRL

chain_init_2:
	TBLRD*+						; load address and post-increment
	movff	TABLAT, POSTINC1	; store address
	clrf	POSTINC1			; and clear 3 intensity bytes that follow
	clrf	POSTINC1			; ...
	clrf	POSTINC1			;

	tstfsz	TABLAT				; address byte from chain zero?
	goto	chain_init_2		; if no - more addresses to read, keep reading	

chain_init_3:
	movlw	UPPER	str_rand	; point to start of random number table
	movwf	TBLPTRU				; ...
	movlw	HIGH	str_rand
	movwf	TBLPTRH
	movlw	LOW		str_rand
	movwf	TBLPTRL

	lfsr	FSR1, ADDRTBL	; point to start of address table

chain_loop:
	call	fade
	goto	chain_loop


; -----------
; Given an address, color(s) and target intensity, fade from current intensity to that one

fade:
;	movlw B'11111100'		; 
;	andwf	FSR1L			; Point to 1st (address) byte of 4-byte table entry
	movff	POSTINC1, ADDR
	movlw	H'00'			;
	xorwf	ADDR, w			; 'ADDR' 0? (end of table)
	bnz		fade_red		; if no

	lfsr	FSR1, ADDRTBL	; if yes - Reset to start of table
	movff	POSTINC1, ADDR	; and load address

fade_red:
	movlw	B'00000100'
	movwf	COLOR
	call	fader
fade_green:
	movlw	B'00000010'
	movwf	COLOR
	call	fader
fade_blue:
	movlw	B'00000001'
	movwf	COLOR
	call	fader

	return



fader:
	movff	INDF1, SCRATCH1
	movff	SCRATCH1, INTENS
;	movff	SCRATCH1, SCRATCH2
;	movf	SCRATCH1, w
	incf	TBLPTRL
	TBLRD*
	movf	TABLAT, w

	andlw	B'00001111'			; constrain intensity


	movwf	SCRATCH2			; new intensity
	subwf	SCRATCH1, w			; w = SCRATCH1 - SCRATCH2 = old - new
	bz		fade_done
	bnc		fade_up				; if neg (new intensity greater)

fade_down:
	decf	INTENS
	call	sendcmdbyte
;	call	delaysec
	movf	INTENS, w
	cpfseq	SCRATCH2			; INTENS = desired value?
	goto	fade_down
	goto	fade_done



fade_up:
	incf	INTENS
	call	sendcmdbyte
;	call	delaysec
	movf	INTENS, w
	cpfseq	SCRATCH2			; INTENS = desired value?
	goto	fade_up
	goto	fade_done

fade_done:
	movff	INTENS, INDF1
	movf	POSTINC1, w			; dummy increment
	return
