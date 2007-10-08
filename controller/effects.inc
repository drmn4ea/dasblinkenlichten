; $Id: $

; Effects modules

; Revised chain table: [ADDR] [Rc] [Gc] [Bc] [Rf] [Gf] [Bf] [flags]

; ----------------------------------------------
; Smoothly transition from values in xc to xf.
; Algo:	If xf>xc, inc xc
;		If xf<xc, dec xc
;		If xf==xc, no change

effect0:
fade:

	;call	delaysec	; debug
	call	delaybit
	call	delaybit
	call	delaybit
	call	delaybit
	call	delaybit

	lfsr	FSR0, ADDRTBL	; point to address
	lfsr	FSR1, ADDRTBL

	movlw	H'04'	;  FSR0 = (node)
	addwf	FSR1L
	clrf	WREG	; FSR1 = (node+4)	(point to Rf)
	addwfc	FSR1H		

fade1:

	movf 	POSTINC0, w		; fetch ADDR, get zero status
	btfsc	STATUS, Z		; 0 (chain terminator)?
	return					; if yes
	movwf	ADDR			; else

;	call	SENDHEX	; debug
;	call	SENDCMNDP2CR

	movlw	B'00000100'	; start with Red
	movwf	COLOR

							; now FSR0 = Rc
fade2:

	; for x= 1 to 3...

	movf	POSTINC1, w	; Rf

	; ---debug---
;	movwf	SCRATCH5
;	call	sendbin
;	movlw	' '
;	call	SENDCMNDP2
;	movf	INDF0, w
;	call	sendbin
;	call	SENDCMNDP2CR
;	movf	SCRATCH5, w
	; ---end debug---

	subwf	INDF0, w	; Rc

	bz		fade_same

	bc		fade_down			;;;; = bn ; Current value greater; fade down
								; else
fade_up:
	incf	INDF0
	movff	POSTINC0, INTENS
	goto	fade_send


fade_down:
	decf	INDF0
	movff	POSTINC0, INTENS
	goto	fade_send


fade_same:	; Next color...

;	call	delaysec	; debug
;	call	delaysec	; debug

	movf	POSTINC0, w			; dummy read
	goto	fade_nextcolor

fade_send:	; ADDR, INTENS and COLOR already set up
	call	sendcmdbyte

fade_nextcolor:

	rrncf	COLOR
	btfss	COLOR, 7	; wraparound?
	goto	fade2		; if no
						; else
	movlw	H'04'
	addwf	FSR0L
	clrf	WREG
	addwfc	FSR0H
	movlw	H'05'	; skip flags byte for now too...
	addwf	FSR1L
	clrf	WREG
	addwfc	FSR1H

	goto	fade1


; ----------------------------------------------
; Given value in Xf, distribute 1 of its intensity steps to each of its nearest neighbors
; For efficiency, requires 1 node worth of padding on either side of the node table.

; There are probably better ways to do this.


;	FC	Rf
;	FD	Gf
;	FE	Bf
;..FF	flags
;	0	addr0
;	1	Rc
;	2	Gc
;	3	Bc
;	4	Rf
;	5	Gf
;	6	Bf
;	7	flags
;	8	addr1
;	9	Rc
;	A	Gc
;	B	Bc
;	C	Rf
;	D	Gf
;	E	Bf
;	F	flags

effect1:
diffuse:

	lfsr	FSR0, ADDRTBL-D'4'		; Rf of "previous"
	lfsr	FSR1, ADDRTBL		; addr of current
	lfsr	FSR2, ADDRTBL+D'12'	; Rf of next

;	movlw	H'04'	;  FSR0 = (node)
;	addwf	FSR1L
;	clrf	WREG	; FSR1 = (node+4)
;	addwfc	FSR1H

diffuse1:

	call	showfsr	; debug

	movf 	POSTINC1, w		; fetch ADDR, get zero status
	btfsc	STATUS, Z		; 0 (chain terminator)?
	return					; if yes
	movwf	ADDR			; else

	call	SENDHEX	; debug
	call	SENDCMNDP2CR

	movlw	H'03'			; point to Rf
	addwf	FSR1L			;
	clrf	WREG			;
	addwfc	FSR1H			;

	movlw	H'03'			; iterations (==R,G,B)
	movwf	SCRATCH0




							; now FSR0 = Rc
							; FSR1,2 = addr(n+1)
diffuse2:
	movf	INDF1, w		; dummy fetch to test zero status
	bz		diffuse2end		; already depleted? If yes
							; else...
	decf	INDF1			; steal 1 from current node
	incf	INDF2			; pass it to next node - light will prefer to diffuse "left to right"

	movf	INDF1, w		; dummy fetch to test zero status, again...
	bz		diffuse2end		; already depleted? If yes
							; else...
	decf	INDF1			; steal 1 from current node
	incf	INDF0			; pass it to prev. node - light will prefer to diffuse "left to right"

diffuse2end
	movf	POSTINC0, w		; advance all pointers to next color
	movf	POSTINC1, w		;
	movf	POSTINC2, w		;

	decfsz	SCRATCH0		; all colors done?
	goto	diffuse2		; if no

	movlw	H'05'			; FSR0 = *(n Rf)
	addwf	FSR0L			;
	clrf	WREG			;
	addwfc	FSR0H			;

	movf	POSTINC1, w		; skip 'flags'

	movlw	H'05'			; FSR2 = *(n+1 Rf)
	addwf	FSR2L			;
	clrf	WREG			;
	addwfc	FSR2H			;



	movlw	'i'
	call	SENDCMNDP2

	goto	diffuse1


; ----------------------------------------------
; Set Xc = Xf and send the appropriate commands

effect_set:

	lfsr	FSR0, ADDRTBL	; point to start of chain (first address)
	lfsr	FSR1, ADDRTBL	; point to start of chain (first address)

	movlw	H'04'	;  FSR0 = (node)
	addwf	FSR1L
	clrf	WREG	; FSR1 = (node+4)	(point to Rf)
	addwfc	FSR1H		

set1:

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
	movwf	ADDR			; else

	movlw	B'00000100'	; start with Red
	movwf	COLOR
							; currently FSR0=Rc, FSR1 = Rf
	movf	POSTINC1, w
	movwf	POSTINC0
	movwf	INTENS

	call	sendcmdbyte

	; lather rinse repeat for Green
	movlw	B'00000010'	; Green
	movwf	COLOR
							; currently FSR0=Gc, FSR1 = Gf
	movf	POSTINC1, w
	movwf	POSTINC0
	movwf	INTENS

	call	sendcmdbyte

	; lather rinse repeat for Blue
	movlw	B'00000001'	; Blue
	movwf	COLOR
							; currently FSR0=Bc, FSR1 = Bf
	movf	POSTINC1, w
	movwf	POSTINC0
	movwf	INTENS

	call	sendcmdbyte


	movlw	H'04'
	addwf	FSR0L
	clrf	WREG
	addwfc	FSR0H
	movlw	H'05'	; skip flags byte for now too...
	addwf	FSR1L
	clrf	WREG
	addwfc	FSR1H


	goto 	set1			; this node written, next please...
