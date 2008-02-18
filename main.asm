; $Id: main.asm,v 1.6 2007/10/08 19:39:56 tgipson Exp $
;
; Das Blinkenlichten: 10F 1-Wire RGB Receiver + Indicator

; Released under the GPL.


; Config bits: WDT ON, MCLR as GP3, CodeProtect OFF.


; ----------------------

#define		MYADDR		0x01		; Personalized address to be stored in THIS chip. Each chip on the bus
									; must have a unique address to be controlled independently...

#define		COMM_ANODE	0x01		; If indicator is common-anode, polarity of output drive and idle are inverted

; ----------------------

	list	p=10f200
	#include "p10f200.inc"



	org 0x00			; effective reset vector
	goto	start

	; First 64 bytes and last byte (osc. calibration word) are readable regardless of CodeProtect bits, so
	; put the revision there so we always have it, even if these bits get set.

; NOTES: This eats almost 1/4 of the code space; remove or shorten if things get tight.
; 'DT' stores in a (1 byte -> 1 word) readable format; probably decodes as RETLW xx
str_version:
	DT	"$Id: main.asm,v 1.6 2007/10/08 19:39:56 tgipson Exp $"



	org	D'64'
start:
	clrwdt
;	movlw	B'01111110'
;	movwf	OSCCAL		; disregard calibration and set osc. speed to maximum; GP2 on GP2

	movlw	B'11001000'	; wakeup-on-change DISabled, pullups DISabled, timer0 clk internal, source edge low-to-high (don't care), prescaler assigned to WDT, /1
	OPTION				; store into OPTION reg.

	movlw	B'00000000'	; set all pins as output which can be
	TRIS	GPIO		; ...
	clrf	GPIO

	bsf		GPIO, GREEN		; briefly show startup value to show running or reset...
	bcf		GPIO, BLUE
	bsf		GPIO, RED

	clrf	GROUPADDR		; initial Group 0x00 (none / same as broadcast address)

	clrw					; Initially clear PWM intensities
	#if (COMM_ANODE == 1)	; ...
	movlw	B'11111111'		; for common-anode, '1' (voltage) on the port extinguishes the LED.
	#endif					; ...
	movwf	PWM_R			;
	movwf	PWM_G			;
	movwf	PWM_B			;

debug_hang:		; show long blue to indicate startup and/or sync lost. Change according to how long you want to hang of course...
	movlw	0x80
	movwf	COUNT
	clrf	SCRATCH0
debug_hang_outer:
	clrwdt
	decfsz	SCRATCH0, f
	goto	debug_hang_outer
	decfsz	COUNT, f
	goto	debug_hang_outer


reset_sync:					; Sync with extended STOP condition (bus idle)
	btfsc	GPIO, SDI		; SDI low?
	goto	reset_sync		; if no

	clrwdt
	clrf	TMR0			; begin timing stop condition
reset_sync_wait1:
	btfsc	GPIO, SDI		; line still low?
	goto	reset_sync		; if no
	btfsc	TMR0, 7			; timer rollxxxer?
	goto	reset_sync_wait1; if no

							; else - fall through and begin running...

main:						; Where it all happens; check for START condition and (if none) advance one color's PWM.
	clrwdt					; WDT time-out will reset the chip if this loop is not returned to in a timely manner (1-wire framing error, e.g. waiting for a serial bit that never comes)
	btfsc	GPIO, SDI		; start bit?
	goto	getcmd			; fake 'call' - shallow stack
	call	pwm_r
	btfsc	GPIO, SDI		; start bit?
	goto	getcmd			; fake 'call' - shallow stack
	call	pwm_g
	btfsc	GPIO, SDI		; start bit?
	goto	getcmd			; fake 'call' - shallow stack
	call	pwm_b
	goto	main


; +17 processcmd
; 

; ---------------------------------------------------
; Check for incoming cmd byte on SDI. If cmd (start condition), receive the cmd packet to CMDBUF.
; ASSUMPTION: Start condition is long enough that the longest possible complete loop will still get us back here in time to catch it.

getcmd:
	movlw	CMDBUF			; init buffer ptr
	movwf	FSR				;

getstartbit:
	btfsc	GPIO, SDI		; spinlock until start condition released
	goto	getstartbit		; ...

getbyte1:
	clrf	TMR0				; begin timing low half of bit
	movlw	H'08'				; going to shift in this many bits
	movwf	COUNT				;

getbit_firsthalf:
	call	pwmjump					; ONCE, in dead time. Helps reduce flicker during saturated bus condition, at the expense of max. bus speed... 18 clocks including call/return
getbit_firsthalf_end:
	btfss	GPIO, SDI			; wait for SDI to go high - has it?
	goto	getbit_firsthalf_end; if no

	comf	TMR0, f				; if yes - begin timing high half of bit

getbit_secondhalf:
	btfsc	GPIO, SDI			; now waiting for bit to go low again - has it?
	goto	getbit_secondhalf	; if no

	movf	TMR0, w				; get timer's value
	movwf	SCRATCH0			; .. to scratch reg (ghetto chip doesn't allow operation on WREG..?)
	clrf	TMR0				; begin timing low half of bit. We know it's longer than a few clocks, plenty of time to...
	rlf		SCRATCH0,f			; rotate MSB of stored TMR0 into 'C'arry (don't care about other bits)
	rlf		INDF,f				; shift bit in 'C'arry into currently-addressed buffer position

	decfsz	COUNT,f				; got all bits?
	goto	getbit_firsthalf	; if no - wait for next
								; else - fall through and begin 2nd byte of cmd...

	clrf	TMR0				; begin timing low half of bit
	incf	FSR,f				; point to 2nd buffer byte

getbyte2:
	movlw	H'08'				; going to shift in this many bits
	movwf	COUNT				;

getbit_firsthalf2:
	call	pwmjump					; ONCE, in dead time.  18 clocks including call/return
getbit_firsthalf_end2:
	btfss	GPIO, SDI			; wait for SDI to go high - has it?
	goto	getbit_firsthalf_end2; if no

	comf	TMR0, f				; if yes - begin timing high half of bit

getbit_secondhalf2:
	btfsc	GPIO, SDI			; now waiting for bit to go low again - has it?
	goto	getbit_secondhalf2	; if no

	movf	TMR0, w				; get timer's value
	movwf	SCRATCH0			; .. to scratch reg (ghetto chip doesn't allow operation on WREG..?)
	clrf	TMR0				; begin timing low half of bit
	rlf		SCRATCH0,f			; rotate MSB of stored TMR0 into 'C'arry (don't care about other bits)
	rlf		INDF,f				; shift bit in 'C'arry into currently-addressed buffer position

	decfsz	COUNT,f				; got all bits?
	goto	getbit_firsthalf2	; if no - wait for next

	; 66 clocks from here to main (18+48)

	; check address to see if it's anything we respond to...

	decf	FSR, f			; point back to 1st byte of received cmd (the address)

	movlw	H'00'			; General Call addr? (everybody! everybody!)
	xorwf	INDF, w			;
	btfsc	STATUS, Z		;
	goto	processcmd		; if yes
							; else

	movlw	MYADDR			; This chip's addr? (NOTE: Hard-coded literal from above)
	xorwf	INDF, W			;
	btfsc	STATUS, Z		;
	goto	processcmd		; if yes
							; else

	movf	GROUPADDR, w	; This chip's Group addr? (NOTE: RAM variable)
	xorwf	INDF, W			;
	btfsc	STATUS, Z		;
	goto	processcmd		; if yes

	goto	main			; else - not for us...

processcmd:	; 48 incl. call/return

	movlw	CMDBUF+1		; point to 2nd byte of CMDBUF (cmd)
	movwf	FSR				; ...

	btfsc	INDF, 7			; Extended Command bit?
	goto	extcmd			; if yes
							; else

	btfsc	INDF, 6			; R cmd
	call	processcmd_r	; 13
	btfsc	INDF, 5			; G cmd
	call	processcmd_g
	btfsc	INDF, 4			; B cmd
	call	processcmd_b
	goto	main			; done



; If payload byte was '1xxxxxxx', decode as an Extended command...
; 11xxxxxx Set Group Address
; 10000000 Enter Power Save (not implemented)
; 10100000 Identify (not implemented)

extcmd:

	movf	INDF, w			; Decode against "Set Group"
	andlw	B'11000000'		; only highest 2 bits specify the cmd
	movwf	SCRATCH0		;

	movlw	B'11000000'
	xorwf	SCRATCH0, w
	btfsc	STATUS, Z
	goto	setgroup

	; ... other extended commands here ...

	goto	main



setgroup:
	movf	INDF, w
	andlw	B'00111111'		; Max. group addr 0x40 because 2 bits were taken up by extcmd
	movwf	GROUPADDR		; store remaining bits as group address
	goto	main			; done






; ---------------------------------------------------
; Process command byte in buffer for the given color

processcmd_r:
	call	setpwm				; 7
	#if (COMM_ANODE == 1)		; Switched in for common-anode drive
	xorlw	B'11111111'			; complement WREG directly (comm. anode: line LOW means LED is lit)
	#endif
	movwf	PWM_R
	retlw	0
processcmd_g:
	call	setpwm
	#if (COMM_ANODE == 1)		; Switched in for common-anode drive
	xorlw	B'11111111'			; complement WREG directly (comm. anode: line LOW means LED is lit)
	#endif
	movwf	PWM_G
	retlw	0
processcmd_b:
	call	setpwm
	#if (COMM_ANODE == 1)		; Switched in for common-anode drive
	xorlw	B'11111111'			; complement WREG directly (comm. anode: line LOW means LED is lit)
	#endif
	movwf	PWM_B
	retlw	0


; ---------------------------------------------------
; Given low 4 bits of cmd in INDF, return that many '1's in WREG.
;
; Want to return a value containing the number of '1's specified in the intensity
; value. But want to spread them out for faster switching and less perceivable flicker.
setpwm:
	movf	INDF, w		; cmd value
	andlw	B'00001111'	; mask off bogus bits
	addwf	PCL, f		; skip that many instructions
	retlw	B'00000000' ; 0x00
	retlw	B'00000001' ; 0x01
	retlw	B'00010001' ; 0x02
	retlw	B'01001001' ; 0x03
	retlw	B'01010101' ; 0x04
	retlw	B'01010111' ; 0x05
	retlw	B'01110111' ; 0x06
	retlw	B'01111111' ; 0x07
	retlw	B'11111111' ; 0x08	; last valid value
	retlw	B'11111111' ; 0x09	; Should not be sent any values this high; we can't represent them in the PWM registers anyway
	retlw	B'11111111' ; 0x0A
	retlw	B'11111111' ; 0x0B
	retlw	B'11111111' ; 0x0C
	retlw	B'11111111' ; 0x0D
	retlw	B'11111111' ; 0x0E
	retlw	B'11111111' ; 0x0F
	retlw	0x00		; pure paranoia

; ---------------------------------------------------
; Perform one iteration/rotation of "poor man's PWM" for each color's register

; 'pwmjump' tries to evenly distribute PWM advances during bit receives by the value of the COUNT register 
; (used during 1-wire receive to tell how many bits remaining). Well, we have 8 bits and 3 colors, so red will
; cycle a little slower than the rest during 1-wire receives, if anyone cares ;)

							; ---------------
	#if COMM_ANODE==0		; Switched in for common-cathode drive
pwmjump:
	movf	COUNT, w
	addwf	PCL, f
	goto	pwm_g	; 0x00
	goto	pwm_b	; 0x01
	goto	pwm_r	; 0x02
	goto	pwm_g	; 0x03
	goto	pwm_b	; 0x04
	goto	pwm_r	; 0x05
	goto	pwm_g	; 0x06
	goto	pwm_b	; 0x07

pwm:
pwm_r:
	rlf		PWM_R,f			; Rotate next PWM value into 'C'arry
	bcf		PWM_R, 0		; This is whatever random junk used to be in there, so clear it...
	btfsc	STATUS, C		; catch the bit that just fell off the left
	bsf		PWM_R, 0		; and stuff it back on the right.

	bcf		GPIO, RED		; Now set the port line according to the same 'C'arry contents.
	btfsc	STATUS, C		; comm. cathode: '1' = 'on'. Want to err on the side of not quite 100% duty cycle vs. dimly glowing when 'off'.
	bsf		GPIO, RED		; .
	retlw	0
pwm_g:
	rlf		PWM_G,f
	bcf		PWM_G, 0
	btfsc	STATUS, C
	bsf		PWM_G, 0

	bcf		GPIO, GREEN
	btfsc	STATUS, C
	bsf		GPIO, GREEN
	retlw	0
pwm_b:
	rlf		PWM_B,f
	bcf		PWM_B, 0
	btfsc	STATUS, C
	bsf		PWM_B, 0

	bcf		GPIO, BLUE
	btfsc	STATUS, C
	bsf		GPIO, BLUE
	retlw	0


	#endif
							; ---------------------
	#if COMM_ANODE == 1		; Switched in for common-anode drive

pwmjump:
	movf	COUNT, w
	addwf	PCL, f
	goto	pwm_g	; 0x00
	goto	pwm_b	; 0x01
	goto	pwm_r	; 0x02
	goto	pwm_g	; 0x03
	goto	pwm_b	; 0x04
	goto	pwm_r	; 0x05
	goto	pwm_g	; 0x06
	goto	pwm_b	; 0x07

pwm:
pwm_r:
	rlf		PWM_R,f			; Rotate next PWM value into 'C'arry
	bcf		PWM_R, 0		; This is whatever random junk used to be in there, so clear it...
	btfsc	STATUS, C		; catch the bit that just fell off the left
	bsf		PWM_R, 0		; and stuff it back on the right.

	bsf		GPIO, RED		; Now set the port line according to the same 'C'arry contents.
	btfss	STATUS, C		; ... Remember kids, comm. anode means '1' = 'off'.
	bcf		GPIO, RED		; .
	retlw	0
pwm_g:
	rlf		PWM_G,f
	bcf		PWM_G, 0
	btfsc	STATUS, C
	bsf		PWM_G, 0

	bsf		GPIO, GREEN
	btfss	STATUS, C
	bcf		GPIO, GREEN
	retlw	0
pwm_b:
	rlf		PWM_B,f
	bcf		PWM_B, 0
	btfsc	STATUS, C
	bsf		PWM_B, 0

	bsf		GPIO, BLUE
	btfss	STATUS, C
	bcf		GPIO, BLUE
	retlw	0
	#endif


; -------------------


; Command packet format: <start> <addr[7..0]><cmd[7..0]><stop>
;________________________-------......16 data bits......______

; The bus idles low.

; Start condition: Bus goes HIGH and lasts for longer than the longest possible loop run, so that all devices are guaranteed to catch it.
; Data bits consist of a low period (low half) followed by a high period (high half). A 0 is denoted by making the LOW half longer than the HIGH half,
;   and a 1 by making the HIGH half longer. Ideally, all bits should total the same length, but since the low half sets the baud rate on a bit-per-bit basis,
;   this is not required. However, the low half should be a minimum 18 device clocks (18/1MHz=18uS) for most accurate timing, 
;   and should not exceed 255 device clocks (255uS).


; Cmd BYTE format: ERGBIIII
; RGB: Which color(s) cmd applies to
; IIII: Set intensity (0 ~ 8)
; E: Extended Command flag. If '1', decode remaining bits as Extended Cmd as described below...

; --- Extended Commands ---
; 11xxxxxx : Set Group Addr to value xxxx
; 10000001 : Enter Power Save (not yet implemented)
; 10000100 : Identify (pull-up bus in response if own ID is called) (not yet implemented)

; Would be nice to have 'deferred update' cmd that writes the new RGB data into shadow registers but doesn't display it yet.
; A second, 'activate deferred' command would trigger all nodes with deferred data to show it at once. This would allow for instantaneous
; 'page flips' in scenarios with a large number of nodes, e.g. billboard displays. Currently this would require a special decoding of
; the RGB 'affected' portion of the cmd because 11XXxxxx is already reserved to set Group Address. 10XXxxxx might be an option where
; XX is a value from 1 to 3 and only sets one color at a time (where XX cannot be 00), with a separate code to activate the stored values.

; Maybe change above cmds to... 10XXxxxx: Poke Reg, where XX is the address of a virtual 4-bit reg and xxxx is the value to poke.
; VAddr 00: Flags [x	identify	activate_deferred	power_save]
; VAddr 01: Defer buf R
; VAddr 02: Defer buf G
; VAddr 03: Defer buf B

; Definitely painting ourselves into a corner, but might be able to squeak out a little more functionality by making that topmost flags bit
; a 'Use Indirection' flag that changes one of the existing VADDRs to a pointer to a numbered register, or even to a full register bank.
; This then gives 16 possible registers for each VADDR (this capacity exceeds the RAM available on the chip we're currently using!)


#include "vars.inc"		; Memory map
#include "defs.inc"		; Port and bit definitions

	end
