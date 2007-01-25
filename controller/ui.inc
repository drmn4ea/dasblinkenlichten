; $Id: $



; -------------------------

ui_main_menu:
	sendstring	str_menu
	sendstring	str_menu_2

	call	GETCMD
	movwf	SCRATCH0

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

	return


ui_inc_color:
	incf	COLOR
	return

ui_toggle_r:
	btg		COLOR, 2
	return

ui_toggle_g:
	btg		COLOR, 0
	return

ui_toggle_b:
	btg		COLOR, 1
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
	iorlw	B'00010000'		; set Extended Command bit (cmd is 000)
	movwf 	COMMAND
	call	ui_show_packet
	call	send1wire
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
	DW	"A		set explicit address\r\nE		set target's group address\r\n\r\n\x00"	

str_addr:
	DW	"Addr: \x00"

str_dest_addr:
	DW	"Group addr: \x00"
