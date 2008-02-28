; $Id: main.asm,v 1.8 2008/02/19 05:48:41 tgipson Exp $
;
; Das Blinkenlichten: 1-Wire RGB Receiver + Indicator, optimized for wearable applications
; (c) 2008 T. R. Gipson
; http://tim.cexx.org/?page_id=374
; This software and protocol (source and binaries) are free for non-commercial use. The source code may be freely redistributed
; either in modified or unmodified forms. In all cases the source code and this license text must be included.


; Version History:
; v0.x (2005) Nora Nightlight edition. Quick n dirty hack with hardcoded timer to distinguish 1/0 data bits. Set Group Address is the only valid extended command. Didn't get around to touching it again for a long time.
; v1.0 (2007) Beloved edition, demoed at VNV Nation concert April 07. Changed from fixed-frequency to variable baudrate data encodeing/decoding; re-ordered some bits in the command packet format to make more sense.
; v1.1 (2008) Proper edition; first public release. Implemented remaining Extended commands: power save mode, deferred update stuff, and device identification.

; ----------------------


; Config bits: WDT ON, MCLR as GP3, CodeProtect OFF.


; ----------------------

#define		MYADDR		0x01		; Personalized address to be stored in THIS chip. Each chip on the bus
									; must have a unique address to be controlled independently...

#define		COMM_ANODE	0x01		; If indicator is common-anode, polarity of output drive and idle are inverted

; ----------------------

; Base contents of the OPTION register. OPTION is write-only, so can't do read-modify-write in code. We'll be needing to set/clear individual bits and don't want to hardcode this multiple places.
#define		OPTION_VALUES	B'01001000'	; wakeup-on-change ENabled, pullups DISabled, timer0 clk internal, source edge low-to-high (don't care), prescaler assigned to WDT, /1

	list	p=10f200
	#include "p10f200.inc"



	org 0x00			; effective reset vector. Real reset vector is the last byte of code memory, which should contain a RETLW xx instruction where xx is an oscillator calibration value.
	goto	start

	; First 64 bytes and last byte (osc. calibration word) are readable regardless of CodeProtect bits, so
	; put the revision there so we always have it, even if these bits get set.
	; Since the addition of the v1.1 cmds, the CVS string is much too long to put here, so adding a very short 'by hand' string instead.
	; (That's OK, the CVS string doesn't accurately reflect real version, since I compulsively commit it every time I change e.g. the device ID
	; and the file gets a red 'modified' mark...

str_version:
	DT "v1.1 tgipson"

start:
	; NOTE: These first few registers get reinitialized to defaults on ANY device Reset (including wakeup from SLEEP),
	; so we have to explicitly set them on any kind of startup.

	andlw	B'11111110'
	movwf	OSCCAL		; Use factory OSCCAL value from last ROM byte; GP2 on GP2
;	movlw	B'01111110'
;	movwf	OSCCAL		; disregard calibration and set osc. speed to maximum; GP2 on GP2

	movlw	OPTION_VALUES	; see define above
	OPTION				; store into OPTION reg.

	movlw	B'00000000'	; set all pins as output which can be
	TRIS	GPIO		; ...



	; Now that that's out of the way, determine how we woke up... 
	; WDT can't be shut off in code, so we check for both pin-change wakeup and WDT-wakeup-from-SLEEP.
	; In the latter case, branch immediately back to the powersave to execute SLEEP again, in effect hitting the snooze button
	
	btfsc	STATUS, GPWUF	; Wakeup on pin change bit set?
	goto	awaken			; if yes - just re-awakening from powersave, do not clear memory contents
;
	btfsc	STATUS, 3		; Was power down mode set? PD\ cleared = powerdown mode
	goto	init			; if we weren't poweredowned: always fully reset
	goto	poke_reg_0_power_save; else - only remaining option is that WDT timed out while waiting for bus activity. Back to sleep...

init:
	clrwdt				; Can't do this earlier; it resets power-up state bits

	movlw	B'00001011'		; Brief initial "i'm not dead" pulse: show blue in common-anode; yellow in common-cathode
	movwf	GPIO


	clrf	GROUPADDR		; initial Group 0x00 (none / same as broadcast address)
	clrf	DEF_VALID		; No deferred update buffers contain valid data

	#if (COMM_ANODE == 0)	; ...
	clrw					; Initially clear PWM intensities
	#endif					; ...
	#if (COMM_ANODE == 1)	; ...
	movlw	B'11111111'		; for common-anode, '1' (voltage) on the port extinguishes the LED.
	#endif					; ...

	movwf	PWM_R			;
	movwf	PWM_G			;
	movwf	PWM_B			;
	movwf	DEF_R			; Just being unnecessary anal; these don't really need to be initialized
	movwf	DEF_G			; as their respective DEF_VALID bit won't get set unless new data is written to them
	movwf	DEF_B			; ...

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

	; In the event of a communications glitch (i.e. framing error) we may lose track of where we are in the command/data bytes coming down the wire.
	; If this happens, we'll soon (~18ms) be reset by the WDT, which isn't cleared inside the bit-receive loops.
	; In this case we want to ignore the bus until the next STOP/START condition can be identified with some certainty, so we don't start receiving
	; in the middle of a byte and end up right back askew again. So, wait for data line to go low and stay low for one timer rollover.
	; It's definitely the lazy way out, but it's cheap (codespace) and reliable. Unfortunately it may take a while to resync on a very saturated bus.

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
awaken:
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
	movlw	CMDBUF				; init buffer ptr
	movwf	FSR					;

getstartbit:
	btfsc	GPIO, SDI			; spinlock until start condition released
	goto	getstartbit			; ...

getbyte1:
	clrf	TMR0				; begin timing low half of bit
	movlw	H'08'				; going to shift in this many bits
	movwf	COUNT				;

getbit_firsthalf:
	call	pwmjump				; ONCE, in dead time. Helps reduce flicker during saturated bus condition, at the expense of max. bus speed... 18 clocks including call/return
getbit_firsthalf_end:
	btfss	GPIO, SDI			; wait for SDI to go high - has it?
	goto	getbit_firsthalf_end; if no

	comf	TMR0, f				; if yes - begin timing high half of bit

getbit_secondhalf:
	btfsc	GPIO, SDI			; now waiting for bit to go low again - has it?
	goto	getbit_secondhalf	; if no

	movf	TMR0, w				; get timer's value
	movwf	SCRATCH0			; .. to scratch reg (ghetto chip doesn't allow operation on WREG..?)
	clrf	TMR0				; begin timing low half of next bit. We know it's longer than a few clocks, plenty of time to...
	rlf		SCRATCH0,f			; rotate MSB of stored TMR0 into 'C'arry (don't care about other bits)
	rlf		INDF,f				; shift bit in 'C'arry into currently-addressed buffer position

	decfsz	COUNT,f				; got all bits?
	goto	getbit_firsthalf	; if no - wait for next
								; else - fall through and begin 2nd byte of cmd...

	;clrf	TMR0				; begin timing low half of bit
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
	clrf	TMR0				; begin timing low half of next bit
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
; 11xxxxxx Set Group Address to xxxxxx
; 10XXxxxx Poke virtual reg address XX with value xxxx, where...
; VAddr 00: Flags [x	identify	activate_deferred	power_save]
; VAddr 01: Defer buf R
; VAddr 02: Defer buf G
; VAddr 03: Defer buf B

extcmd:
	btfsc	INDF, 6			; Decode against "Set Group"
	goto	setgroup		; if Set Group bit set
							; else...
	btfsc	INDF, 5			; Poke reg is 2 or 3?
	goto	poke_reg_2_3	; if yes
	goto	poke_reg_0_1	; else - must be 0 or 1

setgroup:
	movf	INDF, w
	andlw	B'00111111'		; Max. group addr 0x40 because 2 bits were taken up by extcmd
	movwf	GROUPADDR		; store remaining bits as group address
	goto	main			; done

poke_reg_2_3:	; Handle poke regs 2,3
	btfsc	INDF,4			; poke reg is 3?
	goto	poke_reg_3		; if yes
							; else...
poke_reg_2:					; Setting Green deferred update
	movf INDF, w
	andlw	B'00001111'		; only valid values
	movwf	DEF_G			;
	bsf		DEF_VALID, GREEN; and mark it as having a valid update
	goto	main

poke_reg_3:					; Setting Blue deferred update
	movf INDF, w
	andlw	B'00001111'		; only valid values
	movwf	DEF_B			;
	bsf		DEF_VALID, BLUE	; and mark it as having a valid update
	goto	main

poke_reg_0_1				; Handle poke regs 0,1
	btfsc	INDF, 4			; poke reg is 1?
	goto	poke_reg_1		; if yes
							; else...
poke_reg_0:					; Handling Flag bits
							; Remember each of these virtual regs is only 4 bits long because that's how many were left in the CMD byte to specify its value...
	; Reg 0 bit 3 is currently unused, so skipping it...

	btfsc	INDF, 2			; Identify?
	goto	poke_reg_0_identify; if yes
							; else...
	btfsc	INDF, 1			; Activate deferred updates?
	goto	poke_reg_0_activate_def; if yes

							; Power Save will be the last command we check for, since if one of those is coming down the pipe we shouldn't be getting further commands for a while
	btfss	INDF, 0			; Power Save bit?
	goto	main			; if no - that's all of them!
							; else, fall through...

poke_reg_0_power_save:		; For best results, this should be sent on addr 0 following globally setting all intensities to 0. Otherwise you aren't saving much power, and the next cmd will wake everyone up...
	movlw	OPTION_VALUES	; get initial OPTION contents (hardcoded)
	iorlw	B'00000111'		; We can't kill WDT entirely, but can set WDT prescaler as slow as possible
	OPTION					; write new contents
	movf	GPIO, w			; dummy read of I/O port to set clear any existing 'changes'
	sleep					; sleep...
;	goto	main			; should never be reached; PIC10s reset on wakeup


poke_reg_0_identify:
	movlw	OPTION_VALUES	; get initial OPTION contents (hardcoded)
	andlw	B'10111111'		; enable weak pullups (GP3/SDI) by clearing bit 6
	OPTION	
	clrf	SCRATCH0
identify_timer:	; count to a bunch...
	clrwdt
	decfsz	SCRATCH0,f		; done counting?
	goto	identify_timer	; if no
							; else...
	movlw	OPTION_VALUES	; re-get initial contents
	OPTION					; set everything back to normal (disable pullups)
identify_wait:				; Wait until freshly dropped line returns to '0' state. Could be slow to drop with sufficiently weak pulldown
	clrwdt
	btfsc	GPIO, SDI		; ...or with some other device still holding it up.
	goto	identify_wait	; if line still high
	goto	main			; else

poke_reg_0_activate_def:	; A longer command than most to accomplish; might want to delay following cmds a few ms...
	movlw	DEF_R			; point INDF to *address* of DEF_R
	movwf	FSR				; ...
	btfsc	DEF_VALID, RED
	call	processcmd_r
	incf	FSR, f			; pointing to DEF_G
	btfsc	DEF_VALID, GREEN
	call	processcmd_g
	incf	FSR, f			; pointing to DEF_B
	btfsc	DEF_VALID, BLUE
	call	processcmd_b
	clrf	DEF_VALID		; activated all; updates are no longer new
	goto	main			; done! FIXME: Count the clocks on this...


poke_reg_1:					; Setting Red deferred update
	movf INDF, w
	andlw	B'00001111'		; only valid values
	movwf	DEF_R			;
	bsf		DEF_VALID, RED	; and mark it as having a valid update
	goto	main

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
;	retlw	0x00		; pure paranoia

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

; Start condition: Bus goes HIGH and stays high for longer than the longest possible loop run, so that all devices are guaranteed to catch it.
; Data bits consist of a low period (low half) followed by a high period (high half). A 0 is denoted by making the LOW half longer than the HIGH half,
;   and a 1 by making the HIGH half longer. Ideally, all bits should total the same length, but since the low half sets the baud rate on a bit-per-bit basis,
;   this is not required. However, the low half should be a minimum 18 device clocks (18/1MHz=18uS) for most accurate timing, 
;   and should not exceed 255 device clocks (255uS).


; Cmd BYTE format: ERGBIIII
; RGB: Which color(s) cmd applies to
; IIII: Set intensity (0 ~ 8)
; E: Extended Command flag. If '1', decode remaining bits as Extended Cmd as described below...

; --- Extended Commands ---
; 11xxxxxx : Set Group Addr to value xxxxxx
; 10XXxxxx : Poke "Virtual reg" XX with contents xxxx (see below), where XX is the address of a virtual 4-bit reg and xxxx is the value to poke.


; Vreg 00: Flags [x	identify	activate_deferred	power_save]
; Vreg 01: Defer buf R
; Vreg 02: Defer buf G
; Vreg 03: Defer buf B

; Detailed description of the virtual registers:

; Vreg 01 ~ 03 allow a deferred update to be sent for the R, G and B channel respectively. The new intensity value(s) are stored in the 
; appropriate register(s), but the old intensity values continue to be displayed until an activate_deferred command is executed, at which point
; the new intensities are displayed. This will be particularly useful for trickling new values over the bus, then sending a single activate_deferred
; to all devices (addr 0) to give the appearance of a simultaneous update.

; Vreg 00 is a virtual register among virtual registers: Rather than writing a value to it, you write to it setting an individual bit to
; perform the requested action. Once the action is performed, the bit can be considered automatically cleared.
;	* Unused (bit 3): Doesn't do anything.
; 	* Identify (bit 2): On receipt of this cmd by a given device address, this device shall pull the data line HIGH (internal weak pull-up)
;		for a period of about 512 device clocks (or whatever, plenty long enough for master device to see it). Normal operation is then resumed.
;		Note that this may disrupt other devices on the bus, who interpret the pullup signal as a new START command. If this is bothersome an
;  		Identify command may be followed by a startless dummy command if a device responds.
;	* Activate_deferred (bit 1): Replaces the currently displayed intensities with the contents of the Defer (R,G,B) regs if they contain a valid update.
;	* Power_save (bit 0): This command will (hopefully) stop the CPU and any pulse modulation activities and enter a low-power SLEEP mode. The device will remain in 
;		SLEEP mode until the next bus activity occurs, at which point it will re-awaken. Technically it will be waking up occasionally due to WDT, but
;		these activity periods will be brief.

; Ok, so the protocol as shown leaves only 1 bit unused.
; Definitely painting ourselves into a corner, but might be able to squeak out a little more functionality by making that topmost flags bit
; a 'Use Indirection' flag that changes one of the existing VADDRs to a pointer to a numbered register, or even to a full register bank.
; This then gives 16 possible registers for each VADDR, but packing any further functionalities onto PIC10F200 would be challenging at best.
; Therefore, I think the above protocol definition is OK as-is, and any bigger chip will use a more robust 2-wire (clk,data) serial and its own protocol.


#include "vars.inc"		; Memory map
#include "defs.inc"		; Port and bit definitions

	end
