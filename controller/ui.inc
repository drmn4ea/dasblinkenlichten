; $Id: $



; -------------------------

ui_main_menu:
	sendstring	str_menu
	sendstring	str_menu_2
	sendstring	str_menu_3

	call	GETCMD
	movwf	SCRATCH0
	goto	ui_main_menu_2

ui_main_menu_scan:
	call	SCANCMD
	movwf	SCRATCH0

ui_main_menu_2:
	movlw	' '
	xorwf	SCRATCH0, w
	bz		ui_inc_color

	movlw	'R'
	xorwf	SCRATCH0, w
	bz		ui_toggle_r

	movlw	'G'
	xorwf	SCRATCH0, w
	bz		ui_toggle_g

	movlw	'B'
	xorwf	SCRATCH0, w
	bz		ui_toggle_b

	movlw	'+'
	xorwf	SCRATCH0, w
	bz		ui_intens_up

	movlw	'-'
	xorwf	SCRATCH0, w
	bz		ui_intens_dn

	movlw	'0'
	xorwf	SCRATCH0, w
	bz		ui_set_addr_0

	movlw	'1'
	xorwf	SCRATCH0, w
	bz		ui_set_addr_1

	movlw	'2'
	xorwf	SCRATCH0, w
	bz		ui_set_addr_2

	movlw	'A'
	xorwf	SCRATCH0, w
	bz		ui_set_addr_x

	movlw	'E'
	xorwf	SCRATCH0, w
	bz		ui_set_grp_addr

	movlw	'*'
	xorwf	SCRATCH0, w
	bz		ui_inc_addr

	movlw	'/'
	xorwf	SCRATCH0, w
	bz		ui_dec_addr

	movlw	'S'
	xorwf	SCRATCH0, w
	bz		ui_set_full

	movlw	'N'
	xorwf	SCRATCH0, w
	bz		ui_set_none

	movlw	'X'
	xorwf	SCRATCH0, w
	bz		ui_set_event

	movlw	'Z'
	xorwf	SCRATCH0, w
	bz		ui_clear_event

	return

;	movlw	'r'
;	xorwf	SCRATCH0, w
;	bz		ui_inc_r
;
;	movlw	'g'
;	xorwf	SCRATCH0, w
;	bz		ui_inc_g
;
;	movlw	'b'
;	xorwf	SCRATCH0, w
;	bz		ui_inc_b
;	return
;

ui_inc_color:
	incf	COLOR
	return

ui_toggle_r:
	btg		COLOR, 2
	return

ui_toggle_g:
	btg		COLOR, 1
	return

ui_toggle_b:
	btg		COLOR, 0
	return

ui_intens_up:
	incf	INTENS
	return

ui_intens_dn:
	decf	INTENS
	return

ui_set_addr_0
	movlw	H'00'
	movwf	ADDR
	return

ui_set_addr_1
	movlw	H'01'
	movwf	ADDR
	return

ui_set_addr_2
	movlw	H'02'
	movwf	ADDR
	return

ui_inc_addr:
	incf	ADDR
	return

ui_dec_addr:
	decf	ADDR
	return

ui_set_addr_x:
	sendstring	str_addr
	call	GETCMD
	movwf	SERR01
	call	SENDCMNDP2		; local echo
	call	GETCMD
	movwf	SERR02
	call	SENDCMNDP2		; local echo
	call	ASCII2HEX		; char[2] to byte, in WREG
	movwf	ADDR
	return

ui_set_grp_addr:
	sendstring	str_dest_addr
	call	GETCMD
	movwf	SERR01
	call	SENDCMNDP2		; local echo
	call	GETCMD
	movwf	SERR02
	call	SENDCMNDP2		; local echo
	call	ASCII2HEX		; char[2] to byte, in WREG
	andlw	B'00001111'		; low nibble only
	iorlw	B'10000000'		; set Extended Command bit (cmd is 000)
	movwf 	COMMAND
	call	ui_show_packet
	call	send1wire
	return

ui_set_event:
	call	event_pos
	return

ui_clear_event:
	call	event_neg
	return

ui_set_full:
	lfsr	FSR0, ADDRTBL+1 +3
	movlw	B'00000111'	; max. + 1
	movwf	POSTINC0	; R
	movwf	POSTINC0	; G
	movwf	POSTINC0	; B

	lfsr	FSR0, ADDRTBL+1+8 +3
	movlw	B'00000111'	; max. + 1
	movwf	POSTINC0	; R

	lfsr	FSR0, ADDRTBL+1+D'16'+1 +3
	movlw	B'00000111'	; max. + 1
	movwf	POSTINC0	; G

	lfsr	FSR0, ADDRTBL+1+D'24'+2 +3
	movlw	B'00000111'	; max. + 1
	movwf	POSTINC0	; B

	return

ui_set_none:
	lfsr	FSR0, ADDRTBL+1 +3
	movlw	B'00000000'	; 
	movwf	POSTINC0	; R
	movwf	POSTINC0	; G
	movwf	POSTINC0	; B

	lfsr	FSR0, ADDRTBL+1+8 +3
	movlw	B'00000000'	; 
	movwf	POSTINC0	; R
	movwf	POSTINC0	; G
	movwf	POSTINC0	; B

	lfsr	FSR0, ADDRTBL+1+D'16'+3
	movlw	B'00000000'	; 
	movwf	POSTINC0	; R
	movwf	POSTINC0	; G
	movwf	POSTINC0	; B

	lfsr	FSR0, ADDRTBL+1+D'24' +3
	movlw	B'00000000'	; 
	movwf	POSTINC0	; R
	movwf	POSTINC0	; G
	movwf	POSTINC0	; B

	return



; --------------------------
ui_inc_r:
	incf	INTENS_R
	goto	ui_inc_color_x

ui_inc_g:
	incf	INTENS_G
	goto	ui_inc_color_x

ui_inc_b:
	incf	INTENS_B
	goto	ui_inc_color_x

ui_inc_color_x:
	clrf	COLOR
	bsf		COLOR, 2
	movff	INTENS_R, INTENS
	call	sendcmdbyte

	clrf	COLOR
	bsf		COLOR, 1
	movff	INTENS_G, INTENS
	call	sendcmdbyte

	clrf	COLOR
	bsf		COLOR, 0
	movff	INTENS_B, INTENS
	call	sendcmdbyte
	clrf	COLOR
	return
; --------------------------

ui_show_addr:
	sendstring	str_addr
	movf	ADDR, w
	call	SENDHEX
	call	SENDCMNDP2CR
	return

; --------------------------

ui_show_packet:
	call	SENDCMNDP2CR
	movf	ADDR, w
	call	sendbin
	movf	COMMAND, w
	call	sendbin
	call	SENDCMNDP2CR
	return

; --------------------------

str_menu:
	DW	"<space> change color \r\n+       brightness up\r\n-       brightness down\r\nR/G/B   toggle color\r\n* and / change addr up or down\r\n\x00"
str_menu_2:
	DW	"A	set explicit address\r\nE	set target's group address\r\n\r\n\x00"	
str_menu_3:
	DW	"r	inc red only\r\ng	inc green only\r\nb	inc blue only\r\n\x00"


str_addr:
	DW	"Addr: \x00"

str_dest_addr:
	DW	"Group addr: \x00"
