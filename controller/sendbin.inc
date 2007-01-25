; $Id: sendbin.inc,v 1.1 2005/07/29 14:58:52 tgipson Exp $
str_sendbin_ver:
	DW	"$Id: sendbin.inc,v 1.1 2005/07/29 14:58:52 tgipson Exp $\r\n\x00"


; Binary counterpart to SENDHEX in (lcmhex01.inc)

sendbin:
SENDBIN:
	movwf	SCRATCH0				; or any temporary variable

	movlw	H'08'
	movwf	ROTATE					; just another scratch variable

sendbin1:
	movlw	'0'
	btfsc	SCRATCH0, 7
	movlw	'1'
	call	SENDCMNDP2

	rlncf	SCRATCH0				; test next bit...
	decfsz	ROTATE
	goto	sendbin1

	return
