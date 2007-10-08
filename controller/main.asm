; $Id: main.asm,v 1.2 2005/07/31 21:31:31 tgipson Exp $



	list	p=18f2450
	#include "p18f2450.inc"



; -----------------------



; -----------------------

;	org	0x0000
;	goto	start

;	org	0x0018
;	goto	start

	org	0x800
	goto	start

	org	0x818
	goto	start

;org	0x4000

#include	"string_m.inc"

start:
	bcf		WDTCON, SWDTEN

	call	init_ports
	call	INITUSART

	call	delaybit

	movlw	B'00000000'		; 8MHz
;	movlw	B'11100000'		; 4MHz
	movwf	OSCCON

testy:
	sendstring	str_ready
	call	SENDCMNDP2CR

init:

	call	init_int_ADC

	call	delaysec

	bsf		SDIO, ACCEL_PWR


	movlw	H'00'
	movwf	ADDR

	clrf	COLOR
	movlw	H'00'
	movwf	INTENS


	clrf	INTENS_R
	clrf	INTENS_G
	clrf	INTENS_B


	;movlw	B'01110000'		; "all colors off"
	;movwf	COMMAND
	;call	send1wire

;	call	chain
;	goto	intensloop
	goto	effecttest

	goto	main

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


effecttest:
	call	chain_init
	sendstring	str_menu
	sendstring	str_menu_2


effecttest2:
;	call	ui_main_menu_scan
;	tstfsz	SCRATCH0			; user cmd?
;	call	sendcmdbyte			; if yes
	goto	effecttest_doit		; if yes

	goto	effecttest2

effecttest_doit:
	btg		PORTC, 0
	call	event_pos
;	call	effect_set
	call	fade
	call	fade
	call	fade
	call	fade
	call	fade
	call	fade
	call	fade
	call	fade
	call	fade

	call	event_neg
	call	fade
	call	fade
	call	fade
	call	fade
	call	fade
	call	fade
	call	fade
	call	fade
	call	fade

;	call	diffuse
;	call	fade
;	movlw	'S'
;	call	SENDCMNDP2
;	call	delaysec

	goto	effecttest2



main:
	call	ui_show_addr
	call	ui_main_menu

	movlw	'\r'			; carriage return
	xorwf	SCRATCH0, w
	bz		intensloop

;	movlw	B'11100000'		; "all colors off"
;	movwf	COMMAND
;	call	send1wire

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


sendcmdbyte:

	movf	COLOR, w
	call	sendbin
	movlw	' '
	call	SENDCMNDP2
	call	SENDCMNDP2
	movf	INTENS, w
	call	sendbin
	movlw	' '
	call	SENDCMNDP2
	call	SENDCMNDP2

	movf	INTENS, w
	andlw	B'00001111'
	movwf	COMMAND		; only valid intensities... ; FIXME: CHECKME: kills 'set group address'?
	movf	COLOR, w
	andlw	B'00000111'	; only valid colors...
	swapf	WREG
	;bcf	STATUS, C
	;rlcf	WREG
	iorwf	COMMAND


;	movlw	B'011110111'
;	movwf	COMMAND
	call	send1wire
	;call	delay_68
	sendstring	str_sent_cmd
	movf	ADDR, w
	call	SENDHEX
	movlw	' '
	call	SENDCMNDP2
	call	SENDCMNDP2
	movf	COMMAND, w
	call	sendbin
	call	SENDCMNDP2CR
;	call	delaysec

	return



stop:
	goto	stop


#include	"ports.inc"
;#include	"fakespi.inc"
#include	"delay.inc"
#include	"4220uart.inc"
#include	"lcmhex01.inc"
#include	"defs.inc"
#include	"vars.inc"
#include	"sendbin.inc"
#include	"4220anlg.inc"
;#include	"bri_math.inc"	; average
;#include	"ltc2418.inc"
;#include	"node.inc"
;#include	"ui.inc"
;#include	"eeprom.inc"
;#include	"test.inc"
#include	"debug.inc"
#include	"onewire.inc"
#include	"ui.inc"
#include	"chain.inc"
#include	"rand.inc"
#include	"effects.inc"
#include	"event.inc"

str_ready:
	DW	"$Id: main.asm,v I don't know, CVS server is broken... $\r\n\x00"

str_main_ver:
	DW	"$Id: main.asm,v 1.2 2005/07/31 21:31:31 tgipson Exp $\r\n\x00"


str_sent_cmd:
	DW	"Cmd sent: \x00"


str_chain:
	DW	"\x17\x18\x19\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x00"

; \x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20
	end
