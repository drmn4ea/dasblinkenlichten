; $Id: $
;
; 10F 1-Wire Receiver + Indicator

; Config bits: WDT ON, MCLR as GP3, CodeProtect OFF.

; c08

; WARNING: There is no 'BZ' (Branch if Zero) instruction on PIC10! The assembler fakes it using 'BTFSS STATUS,Z', which
; will NOT work as intended unless immediately followed by an unconditional branch.

; ----------------------

#define		MYADDR		0x12		; Personalized address to be stored in THIS chip. Each chip on the bus
									; must have a unique address to be controlled independently...

#define		COMM_ANODE	0x01		; If indicator is common-anode, polarity of output drive and idle are inverted

; ----------------------



	list	p=10f200
	#include "p10f200.inc"

;	org	0x01FE
;stop:
;	goto	stop
;
;	org	0x1FF
;	movlw	H'08'


	org 0x00			; effective reset vector
;	movwf	OSCCAL		; set factory oscillator calibration value from last program memory byte (set as MOVLW H'xx' from factory)
start:
	clrwdt
	movlw	B'11001000'	; wakeup-on-change DISabled, pullups DISabled, timer0 clk internal, source edge low-to-high (don't care), prescaler assigned to WDT, /1
;	movlw	B'11001111'	; wakeup-on-change DISabled, pullups DISabled, timer0 clk internal, source edge low-to-high (don't care), prescaler assigned to WDT, /128
	OPTION

	movlw	B'00000000'
	TRIS	GPIO
	clrf	GPIO

	bsf		GPIO, GREEN		; Check for reset...
	bsf		GPIO, BLUE
	bcf		GPIO, RED

	clrf	PWM_R
	clrf	PWM_G
	clrf	PWM_B

	clrf	GROUPADDR

	movlw	B'11111110'		; debug - see if light lights / program running
	movwf	PWM_R			;
	movlw	B'11111110'
	movwf	PWM_G			;
	movlw	B'11111110'
	movwf	PWM_B			;


reset_sync:					; Sync with extended STOP condition
	btfsc	GPIO, SDI		; SDI low?
	goto	reset_sync		; if no

	clrwdt
	clrf	TMR0			; begin timing stop condition
reset_sync_wait1:
	btfsc	GPIO, SDI		; line still low?
	goto	reset_sync		; if no
	btfsc	TMR0, 7			; timer rollxxxer?
	goto	reset_sync_wait1; if no

	clrf	TMR0			; reset timer
reset_sync_wait2:
	btfsc	GPIO, SDI		; line still low?
	goto	reset_sync		; if no
	btfsc	TMR0, 7			; timer rollxxxer?
	goto	reset_sync_wait2; if no
							; else - fall through and begin running...

main:
	clrwdt
	call	pwm				; 25 clocks, including call. 11 inc. call if separated...
	btfss	GPIO, SDI		; start bit?
	goto	main
	goto	getcmd			; fake 'call' - shallow stack
;	movwf	SCRATCH0		; move return status somewhere usable
;	incfsz	SCRATCH0,f		; test it. Returned FF?
;	goto	main			; if no - no cmd

;	; else, process cmd...

	; --- debug ---
;	movlw	CMDBUF			; point to 1st byte of CMDBUF (addr)
;	movwf	FSR				; ...
;	movf	INDF, w	; debug
;	movwf	PWM_R	; - what address are we getting???
;	incf	FSR,f	;
;	movf	INDF, w	;
;	movwf	PWM_G	;
	; --- end debug ---


; ---------------------------------------------------
; Given low 4 bits of cmd in INDF, shift that many '1's into SCRATCH0

setpwm:
	clrf	SCRATCH0		; clear temporary reg
	movf	INDF, w			; cmd value
	andlw	B'00001111'		; mask off bogus bits
	movwf	COUNT			;
	iorlw	B'00000000'		; dummy op to affect 'Z'ero status. Intensity = 0?
	btfsc	STATUS, Z
	goto	setpwm_done
;	bz		setpwm_done		; if yes - leave register clear
;							; else...

setpwm1:
	bsf		STATUS, C
	rlf		SCRATCH0,f
	decfsz	COUNT,f			; done shifting in 1s?
	goto	setpwm1			; if no
setpwm_done:

	if COMM_ANODE==1
	comf	SCRATCH0, f		; HACK: common-anode LED
	endif

	retlw	0


; ---------------------------------------------------
; Perform one iteration/rotation of "poor man's PWM" for each color's register


pwm:
						; ---------------
	if COMM_ANODE==0	; Switched in for common-cathode drive
	rlf		PWM_R,f		; FIXME: Separate r/g/b PWM loops, see below
	bcf		PWM_R, 0
	btfsc	STATUS, C
	bsf		PWM_R, 0

	bcf		GPIO, RED
	btfsc	STATUS, C
	bsf		GPIO, RED

	rlf		PWM_G,f
	bcf		PWM_G, 0
	btfsc	STATUS, C
	bsf		PWM_G, 0

	bcf		GPIO, GREEN
	btfsc	STATUS, C
	bsf		GPIO, GREEN

	rlf		PWM_B,f
	bcf		PWM_B, 0
	btfsc	STATUS, C
	bsf		PWM_B, 0

	bcf		GPIO, BLUE
	btfsc	STATUS, C
	bsf		GPIO, BLUE
	endif
							; ---------------------
	if COMM_ANODE == 1		; Switched in for common-anode drive
pwm_r:
	rlf		PWM_R,f
	bcf		PWM_R, 0
	btfsc	STATUS, C
	bsf		PWM_R, 0

	bsf		GPIO, RED
	btfss	STATUS, C
	bcf		GPIO, RED
;	retlw	0
pwm_g:
	rlf		PWM_G,f
	bcf		PWM_G, 0
	btfsc	STATUS, C
	bsf		PWM_G, 0

	bsf		GPIO, GREEN
	btfss	STATUS, C
	bcf		GPIO, GREEN
;	retlw	0
pwm_b:
	rlf		PWM_B,f
	bcf		PWM_B, 0
	btfsc	STATUS, C
	bsf		PWM_B, 0

	bsf		GPIO, BLUE
	btfss	STATUS, C
	bcf		GPIO, BLUE
	endif

	retlw	0

; ---------------------------------------------------
; Check for incoming cmd byte on SDI. If cmd (start condition), receive the cmd packet to CMDBUF.
; ASSUMPTION: Start condition is long enough that the longest possible complete loop will still get us back here in time to catch it.

getcmd:
	movlw	CMDBUF			; init buffer ptr
	movwf	FSR				;

getstartbit:
	btfsc	GPIO, SDI		; spinlock until start condition released
	goto	getstartbit		; ...

;	clrwdt

	call	getbyte			; addr
	comf	INDF,f	;hack: bits inverted?
	incf	FSR,f
	call	getbyte			; cmd
	comf	INDF,f	;hack: bits inverted?


	; check address
	movlw	CMDBUF			; point to 1st byte of CMDBUF (addr)
	movwf	FSR				; ...

	movlw	H'00'			; General Call addr? (everybody! everybody!)
	xorwf	INDF, w			;
	btfsc	STATUS, Z
	goto	processcmd
;	bz		processcmd


	movlw	MYADDR			; This chip's addr?
	xorwf	INDF, W			;
	btfsc	STATUS, Z
	goto	processcmd
;	bz		processcmd

	movf	GROUPADDR, w	; This chip's Group addr?
	xorwf	INDF, W			;
	btfsc	STATUS, Z
	goto	processcmd
;	bz		processcmd


	goto	main			; else - not for us...

processcmd:

;	movlw	H'FF'		; debug
;	movwf	PWM_B		; - check for getting here

	movlw	CMDBUF+1		; point to 2nd byte of CMDBUF (cmd)
	movwf	FSR				; ...

	btfsc	INDF, 4			; Extended Command bit
	goto	extcmd

	btfsc	INDF, 7			; R cmd
	call	processcmd_r
	btfsc	INDF, 6			; G cmd
	call	processcmd_g
	btfsc	INDF, 5			; B cmd
	call	processcmd_b
	goto	main			; done


extcmd:						; Extended commands are handled here
;	movlw	b'11111110'		; debug
;	movwf	PWM_R			; debug

	movf	INDF, w			; Decode cmd to SCRATCH0
	andlw	B'11100000'		; ...
	movwf	SCRATCH0		;

	movlw	H'00'
	xorwf	SCRATCH0, w
	btfsc	STATUS, Z
	goto	setgroup
;	movlw	b'11111110'		; debug
;	movwf	PWM_G			; debug
	; ... other extended commands here ...

	goto	main



setgroup:
	movf	INDF, w
	andlw	B'00001111'
	movwf	GROUPADDR		; store low nibble of extended cmd as group address
;	movlw	b'11111110'		; debug
;	movwf	PWM_B			; debug
	goto	main

; ---------------------------------------------------
; Process command byte in buffer for the given color

processcmd_r:
	call	setpwm
	movf	SCRATCH0, w
	movwf	PWM_R
	retlw	0

processcmd_g:
	call	setpwm
	movf	SCRATCH0, w
	movwf	PWM_G
	retlw	0

processcmd_b:
	call	setpwm
	movf	SCRATCH0, w
	movwf	PWM_B
	retlw	0


; -------------------------------


getbyte:
	movlw	H'08'			; rotate this many times
	movwf	COUNT			;


getbit_waiting:
	call	pwm				; ONCE, in dead time. 25 clocks, including call; +10 after bit received
getbit_donewaiting:
	btfss	GPIO, SDI			; wait for SDI to go high - has it?
	goto	getbit_donewaiting	; if no

	clrf	TMR0			; if yes

getbit_ready:
	btfsc	GPIO, SDI		; now waiting for bit to go low again - has it?
	goto	getbit_ready	; if no

	movf	TMR0, w			; get timer's value
	movwf	SCRATCH0		; .. to scratch reg (ghetto chip doesn't allow operation on WREG..?)
	rlf		SCRATCH0,f		; rotate MSB of TMR0 into 'C'arry (don't care about other bits)
	rlf		SCRATCH0,f		; .. however many times for desired timer resolution
	rlf		SCRATCH0,f		; hbit=40h qbit=20h
;	rlf		SCRATCH0,f		; hbit=20h qbit=10h
	rlf		INDF,f			; shift bit in 'C'arry into currently-addressed buffer position
	decfsz	COUNT,f			; got all bits?
	goto	getbit_waiting	; if no - wait for next
	retlw	0				; else - done




; -------------------


; Command packet format: <start> <addr[7..0]><cmd[7..0]>
; Start bit is HIGH and lasts for longer than the longest possible loop run, so that all devices are guaranteed to catch it.
; Subsequent bits are HIGH for either <128 timer clocks (logic 0) or >128 clocks (logic 1) ... can shift timer0 MSB as detected bit
; ... assuming clocks aren't *insanely* skewed from one device to the next.


; Cmd BYTE format: RGBCIIII
; RGB: Which color(s) cmd applies to
; IIII: Set intensity (0 ~ 8)
; C: Indicates Extended Command as described below...

; --- Extended Commands ---
; 0001xxxx : Set Group Addr to value in xxxx

#include "vars.inc"
#include "defs.inc"



	end
