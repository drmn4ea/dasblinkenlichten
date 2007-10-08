; $Id: $

; Respond to an event...





; ----------------------------------------------
; Respond to a (low-to-high) positive interrupt event.
; Pull light intensities from the RNG table and write them (R,G,B) to every chain node.

event_pos:



	lfsr	FSR0, ADDRTBL	; point to start of chain (first address)


;	movlw	H'04'	;  FSR0 = (node)
;	addwf	FSR1L
;	clrf	WREG	; FSR1 = (node+4)	(point to Rf)
;	addwfc	FSR1H		

event_pos1:

;	movf	FSR0H, w
;	call	SENDHEX
;	movf	FSR0L, w
;	call	SENDHEX
;	movlw	':'
;	call	SENDCMNDP2
;
;	movf	INDF0, w
;	call	SENDHEX	; debug
;	movlw	' '
;	call	SENDCMNDP2


	movf 	POSTINC0, w		; fetch ADDR, get zero status
	btfsc	STATUS, Z		; 0 (chain terminator)?
	return					; if yes
;	movwf	ADDR			; else

;	call	SENDHEX	; debug
;	call	SENDCMNDP2CR

	movf 	POSTINC0, w		; dummy read. Rc
	movf 	POSTINC0, w		; dummy read. Gc
	movf 	POSTINC0, w		; dummy read. Bc


							; FSR0 currently = Rf

	call	get_rand		; get next RNG entry
	movwf	POSTINC0		; Rf
;	call	SENDHEX
	call	get_rand		; get next RNG entry
	movwf	POSTINC0		; Gf
;	call	SENDHEX
	call	get_rand		; get next RNG entry
	movwf	POSTINC0		; Bf
;	call	SENDHEX
;	call	SENDCMNDP2CR

	movf	POSTINC0, w		; dummy read (flags byte)

	goto 	event_pos1		; this node written, next please...



; ----------------------------------------------
; Respond to a (high-to-low) positive interrupt event.
; Clear (R,G,B)f to every chain node.

event_neg:
	lfsr	FSR0, ADDRTBL	; point to start of chain (first address)


;	movlw	H'04'	;  FSR0 = (node)
;	addwf	FSR1L
;	clrf	WREG	; FSR1 = (node+4)	(point to Rf)
;	addwfc	FSR1H		

event_neg1:


	movf 	POSTINC0, w		; fetch ADDR, get zero status
	btfsc	STATUS, Z		; 0 (chain terminator)?
	return					; if yes
;	movwf	ADDR			; else


	movf 	POSTINC0, w		; dummy read. Rc
	movf 	POSTINC0, w		; dummy read. Gc
	movf 	POSTINC0, w		; dummy read. Bc


							; FSR0 currently = Rf

	movlw	0x00			; get next RNG entry
	movwf	POSTINC0		; Rf
	movwf	POSTINC0		; Gf
	movwf	POSTINC0		; Bf

	movf	POSTINC0, w		; dummy read (flags byte)

	goto 	event_neg1		; this node written, next please...

	return
