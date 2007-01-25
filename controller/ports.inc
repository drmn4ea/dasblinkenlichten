; $Id: ports.inc,v 1.2 2005/07/31 21:31:31 tgipson Exp $
str_ports:
	DW	"$Id: ports.inc,v 1.2 2005/07/31 21:31:31 tgipson Exp $\r\n\x00"



init_ports:

	MOVLW B'00001110' ; Configure A/D
	MOVWF ADCON1 ; for digital inputs // except AN0
	MOVLW 07h ; Configure comparators
	MOVWF CMCON ; for digital input


	bcf			TRISC, 6	; Tx
	bsf			TRISC, 7	; Rx

	bcf			SDIOTRIS, S_DO

	bsf			SDIOTRIS, ACCEL_X
	bcf			SDIOTRIS, ACCEL_PWR


	return
