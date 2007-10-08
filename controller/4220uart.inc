; $Id: 4220uart.inc,v 1.1 2005/07/29 14:58:52 tgipson Exp $
str_uart_ver:
	DW	"$Id: 4220uart.inc,v 1.1 2005/07/29 14:58:52 tgipson Exp $\r\n\x00"


; Drop-in SPI routines for PIC18F4220



INITUSART:

	BCF	RCSTA,SPEN	; Serial port disable
	BCF	RCSTA,SREN	; Disable single reception
	BCF	RCSTA,CREN	; Disable continious reception


	bsf		TRISC, 7	; RC7 input (Rx)
	bcf		TRISC, 6	; RC6 (Tx)


;--------------- 115,200 bps @ 48MHz
;	movlw	D'00'
;	movwf	SPBRGH
;	movlw	D'25'
;	movwf	SPBRG
;
;	bsf		BAUDCON, BRG16		; enable 16-bit BRG
;	bcf		TXSTA, BRGH			; disable hi-speed baud clock
;---------------


;--------------- 115,200 bps @ 8MHz
	movlw	D'00'
	movwf	SPBRGH
	movlw	D'16'
	movwf	SPBRG

	bsf		BAUDCON, BRG16		; enable 16-bit BRG
	bsf		TXSTA, BRGH			; enable hi-speed baud clock
;---------------



	;MOVWF	SPBRG		; Initialise baud rate generator
;	movff	BAUD, SPBRG	; Initialize baud rate generator with already-defined rate


	;bsf		TXSTA, BRGH						; Enable hi-speed baud clock
	BSF	TXSTA,CSRC	; Master mode, clk from BRG
	BCF	TXSTA,TX9	; Select 8 bit tansmission
	BCF	TXSTA,TXEN	; Transmission disabled
	BCF	TXSTA,SYNC	; Asynchronous mode

	BCF	RCSTA,RX9	; Select 8 bit reception

	BSF	RCSTA,SPEN	; Serial port enable
	BSF	TXSTA,TXEN	; Enable transmission
	;BCF	PIE1,RC1IE
	BSF	RCSTA,CREN	; Enable reception


	return

; ---------------------------------------------
SENDCMNDP2:
SENDCMD:			; Send Wreg contents to USART

	BTFSS	TXSTA,TRMT	; Wait until transmitter ready to send next byte
	GOTO	SENDCMD
	MOVWF	TXREG		; Transfer next byte from WREG to 
                        ; transmitter register
	RETURN


SENDCMNDP2CR:
	movlw	'\r'
	call	SENDCMD
	movlw	'\n'
	call	SENDCMD
	return

; ---------------------------------------------
getcmd:
GETCMD:
	btfss	PIR1, RCIF					; byte received on USART?
	goto	GETCMD						; if no
	movf	RCREG,0						; if yes - to W
	;movwf	BYTE						; store it here
	return


; ---------------------------------------------
; Return 0xFF if byte received, else 0x00
iscmd:
ISCMD:
	btfss	PIR1, RCIF
	retlw	H'00'
	movf	RCREG, 0
	retlw	H'FF'

; ---------------------------------------------

SCANCMD:
	btfss	PIR1, RCIF
	retlw	H'00'
	movf	RCREG, 0
	return
