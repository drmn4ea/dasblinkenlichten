




showfsr:
	movf	FSR0H, w
	call	SENDHEX
	movf	FSR0L, w
	call	SENDHEX
	movlw	' '
	call	SENDCMNDP2
	movf	FSR1H, w
	call	SENDHEX
	movf	FSR1L, w
	call	SENDHEX
	movlw	' '
	call	SENDCMNDP2
	movf	FSR2H, w
	call	SENDHEX
	movf	FSR2L, w
	call	SENDHEX
	call	SENDCMNDP2CR
	return


mathtest:
;	movlw	


	return
