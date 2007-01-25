; $Id: delay.inc,v 1.2 2005/07/31 21:31:31 tgipson Exp $

; copied from pdasdl01.h 2004/03/17 by Tim Gipson for LCMEMII::HRU
; imdelay.h

str_delay_ver:
	DW	"$Id: delay.inc,v 1.2 2005/07/31 21:31:31 tgipson Exp $\r\n\x00"

;------------------------------------------------------------------------
delaymsec4M:	; delay 1 millisecond with 4MHz crystal
	movlw	17h
	movwf	del3
d4m3:
	movlw	2h
	movwf	del2
d4m2:
	movlw	6h
	movwf	del1
d4m1:
	decfsz	del1,f
	goto	d4m1

	decfsz	del2,f
	goto	d4m2

	decfsz	del3,f
	goto	d4m3

	retlw	0

;------------------------------------------------------------------------

delaysec:	; delay 1 second with 4MHz crystal
	movlw	02h		;17h
	movwf	del3
d3:
	movlw	80h
	movwf	del2
d2:
	movlw	70h
	movwf	del1
d1:
	decfsz	del1,f
	goto	d1

	decfsz	del2,f
	goto	d2

	decfsz	del3,f
	goto	d3

	retlw	0

;------------------------------------------------------------------------
delay20k:	; delay for 20 kHz with 8MHz crystal
	movlw	03h
	movwf	del2
d2m2:
	movlw	8h
	movwf	del1
d2m1:
	decfsz	del1,f
	goto	d2m1

	decfsz	del2,f
	goto	d2m2

	retlw	0


;------------------------------------------------------------------------
delaymsec:	; delay 1 millisecond with 10MHz crystal
	movlw	05h
	movwf	del3
dm3:
	movlw	13h
	movwf	del2
dm2:
	movlw	8h
	movwf	del1
dm1:
	decfsz	del1,f
	goto	dm1

	decfsz	del2,f
	goto	dm2

	decfsz	del3,f
	goto	dm3

	retlw	0

;at 0.4 instruction per uSec, 1mSec needs 2500 instructions
;done with any of the following values: time = a3 * (6 + (a2 * (6 + (a1 * 4))))
;a3 = 1 a2 = 14 a1 = 43.
;a3 = 1 a2 = 29 a1 = 20.
;a3 = 1 a2 = 43 a1 = 13.
;a3 = 1 a2 = 178 a1 = 2.
;a3 = 2 a2 = 2 a1 = 154.
;a3 = 5 a2 = 1 a1 = 122.
;a3 = 5 a2 = 13 a1 = 8.
;a3 = 5 a2 = 19 a1 = 5.
;a3 = 10 a2 = 2 a1 = 29.
;a3 = 25 a2 = 1 a1 = 22.
;a3 = 50 a2 = 2 a1 = 4.
;a3 = 125 a2 = 1 a1 = 2.
;------------------------------------------------------------------------

delayhmsec:	; delay 1/2 millisecond with 10MHz crystal
	movlw	05h
	movwf	del3
dhm3:
	movlw	13h
	movwf	del2
dhm2:
	movlw	4h
	movwf	del1
dhm1:
	decfsz	del1,f
	goto	dhm1

	decfsz	del2,f
	goto	dhm2

	decfsz	del3,f
	goto	dhm3

	retlw	0

;------------------------------------------------------------------------

delaysome:	; delay x usecond with 10MHz crystal
	movlw	04h
	movwf	del3
ds3:
	movlw	5h
	movwf	del2
ds2:
	movlw	4h
	movwf	del1
ds1:
	decfsz	del1,f
	goto	ds1

	decfsz	del2,f
	goto	ds2

	decfsz	del3,f
	goto	ds3

	retlw	0
;------------------------------------------------------------------------

delayhsome:	; delay 1/2 x usecond with 10MHz crystal
	movlw	04h
	movwf	del3
dhs3:
	movlw	5h
	movwf	del2
dhs2:
	movlw	2h
	movwf	del1
dhs1:
	decfsz	del1,f
	goto	dhs1

	decfsz	del2,f
	goto	dhs2

	decfsz	del3,f
	goto	dhs3

	retlw	0

;----------------------------------
delay20msec:
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	call	delaymsec
	return

;----------------------------------
delay20sec:
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	call	delaysec
	return



delaybit:
	clrf WAITER
	;movlw	H'01'
	;movwf	WAITER
delayb1:	
	decfsz	WAITER
	goto	delayb1

	return

delay_40:
delayhalfbit:
	movlw	H'40'	;80
	movwf 	WAITER

delay40_1:	
	decfsz	WAITER
	goto	delay40_1
	return


delay_20:
;delayqbit:
	movlw	H'20'	;40
	movwf 	WAITER

delay20_1:	
	decfsz	WAITER
	goto	delay20_1
	return

delay_68:
;delayqbit:
	movlw	D'68'
	movwf 	WAITER

delay68_1:	
	decfsz	WAITER
	goto	delay68_1
	return




delaybit_wusb:
;	return

;	;clrf WAITER
	movlw	H'01'
	movwf	WAITER
delayb1_wusb:	
	decfsz	WAITER
	goto	delayb1_wusb

	return
