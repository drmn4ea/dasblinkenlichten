; $Id: string_m.inc,v 1.1 2005/07/29 14:58:52 tgipson Exp $
str_string_ver:
	DW	"$Id: string_m.inc,v 1.1 2005/07/29 14:58:52 tgipson Exp $\r\n\x00"

; This is a routine and macro for easily sending a text string to RS232.
; Since it contains a macro, it must be #INCLUDEd BEFORE it is used, but after an ORG xxxx directive.
; Could be easily modified to send the string elsewhere (e.g. LCD or other memory)

; Usage:
; To send a string with label str_test, add the following to your source:
; 	sendstring	str_test
;
; Wherever appropriate in your code, add the string as follows:
; 	str_test:
;		DW	"This is my string.\r\n\x00"
;
; The string can optionally contain the \r\n (return and linefeed), but MUST end with a null character (\x00).


sendstring	macro stringlabel
	movlw	upper	stringlabel
	movwf	TBLPTRU
	movlw	high	stringlabel
	movwf	TBLPTRH
	movlw	low		stringlabel
	movwf	TBLPTRL
	call	stringsender
	endm


stringsender:
	TBLRD*+
	movf	TABLAT, W
	TSTFSZ	WREG
	goto	sendstrbyte
	return

sendstrbyte: 
	call	SENDCMNDP2
	goto 	stringsender



; ----------

pstrcpy	macro stringlabel, address
	movlw	upper	stringlabel
	movwf	TBLPTRU
	movlw	high	stringlabel
	movwf	TBLPTRH
	movlw	low		stringlabel
	movwf	TBLPTRL

	lfsr	FSR0, address

	call	rom_to_ram
	endm


rom_to_ram:
	TBLRD*+
	movf	TABLAT, W
	TSTFSZ	WREG
	goto	rom_to_ram2
	return

rom_to_ram2: 
	movwf	POSTINC0
	goto 	rom_to_ram

