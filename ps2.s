.include "register_defs.inc"

.section .text

.globl init_ps2
init_ps2:
	push	{lr}							// supposed to keep stacks 8 byte aligned, for performance?
	ldr     r1, =GPIO_base

	mov		r0, #0b10
	str     r0, [r1, #GPIO_GPPUD]			// enable PULL_UP resistors...
	mov		r0, #150
	bl		delay							// (much longer than needed? need 150 µs. This is 150 <SUBS #1; BNE;>
	mov		r0, #1<<KBD_CLK
	orr		r0, #1<<KBD_DATA
	orr		r0, #1<<MOUSE_CLK
	orr		r0, #1<<MOUSE_DATA
	str     r0, [r1, #GPIO_GPPUDCLK0]		// ... on GPIO28 and 29 (and 30, 31)
	mov		r0, #150
	bl		delay
	mov		r0, #0
	str     r0, [r1, #GPIO_GPPUD]			// "remove control signal"
	str     r0, [r1, #GPIO_GPPUDCLK0]		// "remove the clock"

	ldr     r0, [r1, #GPIO_GPFSEL2]
	bic     r0, #0x7<<(8 * 3)				// CLEAR bits 0000.0111 << (8 * 3)  set GPIO28 as input KBD_CLK
	bic     r0, #0x7<<(9 * 3)				// CLEAR bits 0000.0111 << (9 * 3)  set GPIO29 as input KBD_DATA
	str     r0, [r1, #GPIO_GPFSEL2]
	ldr     r0, [r1, #GPIO_GPFSEL3]
	bic     r0, #0x7<<(0 * 3)				// CLEAR bits 0000.0111 << (0 * 3)  set GPIO30 as input MSE_CLK
	bic     r0, #0x7<<(1 * 3)				// CLEAR bits 0000.0111 << (1 * 3)  set GPIO31 as input MSE_DATA
	str     r0, [r1, #GPIO_GPFSEL3]
											// >>>>>>> if pull-ups not needed, and GPFSEL is 000 on reset, could delete all up to here
	mov		r0, #1<<KBD_CLK
	orr		r0, #1<<MOUSE_CLK
	str     r0, [r1, #GPIO_GPFEN0]			// falling edge enable on keyboard and mouse CLK signals

	ldr     r1, =IRQ_base
	mov		r0, #0b0010<<17					// enable gpio_int[1] (ie 18) (20:17 = gpio_int[3:0] = GPU intrpt 52:49 - 32)
	str		r0, [r1, #IRQ_IRQ2_EN]			// (- 32, to get 2nd bank of 32 intrpts)

	ldr		r1, =kbd_buff_info
	ldr		r0, =kbd_buffer
	str		r0, [r1, #KBD_BUFFPTR]
	add		r0, #32							// BUFFSIZE = 32 byte buffer.
	str		r0, [r1, #KBD_BUFFEND]
	mov		r0, #0
	str		r0, [r1, #KBD_STATUS]			// all flags cleared: RCVG = 0, MULTI = 0, KEYUP = 0
	str		r0, [r1, #KBD_NVALCHARS]

	pop		{pc}
											// sent "BS 000000 00000000" so n so msecs afer power up...
											// 0x66   <--- setup completed = 0xAA (not in LUT so 0x66 is some junk)
											// The other 7 0x00 are also junk due to vals not being in LUT?

.globl irq
irq:										// entered with interrupts disabled
	push	{r0,r1,lr}

	ldr		r1, =IRQ_base
	ldr		r0, [r1, #IRQ_IRQ2_PDG]
	tst		r0, #0b0010<<17					// bit 18 = gpio_int[1]		bit18 set for ANY event on ANY GPIO31:0 pin

	blne	kbd_irq							// ie. branch if bit 18 set. (interrupt cleared by kbd_irq at the moment)

//	ldr		r1, =GPIO_base
//	ldr		r0, [r1, #GPIO_GPEDS0]
//	tst		r0, #1<<KBD_CLK					// if both CLKs appear at *precisely* the same instant, both flags set
//	moveq	r0, #1<<KBD_CLK
//	streq	r0, [r1, #GPIO_GPEDS0]			// clear interrupt by writing a 1
//	bleq	kbd_irq							// 1st look at KBD_DATA.
//	tst		r0, #1<<MOUSE_CLK				// Then return here and do MSE_DATA if necessary.
//	moveq	r0, #1<<MOUSE_CLK
//	streq	r0, [r1, #GPIO_GPEDS0]			// clear interrupt by writing a 1
//	bleq	mouse_irq						// MSE_DATA will still be valid. It's valid for 58 µs after falling edge.

	pop		{r0,r1,lr}						// can reset CPSR using LDMFD r13!, {r0,r1,pc} ?

	subs	r15, lr, #4						// exit, reinstalling CPSR
	

.globl mouse_irq
mouse_irq:
	push	{r0-r4}
	pop		{r0-r4}
	mov		pc, lr


.globl kbd_irq
kbd_irq:
	push	{r0-r4,lr}

	ldr     r1, =GPIO_base
	mov		r2, #1<<KBD_CLK
	str		r2, [r1, #GPIO_GPEDS0]			// clear interrupt by writing a 1
	ldr     r0, [r1, #GPIO_GPLEV0]
	and		r0, #1<<KBD_DATA				// read level of DATA pin

	ldr		r1, =kbd_buff_info
	ldr		r3, [r1, #KBD_STATUS]			// get state flags

	tst		r3, #1<<KBD_RCVG				// are we in the middle of receiving a byte?
	bne		1f								// ne if flag set: yes...
	cmp		r0, #0							// no -> {is START bit low?
	bne		invalid							// 			no: invalid start bit
	str		r0, [r1, #KBD_BITS]				//          yes: (r0 = 0) => new byte. Zero count of high bits.
											// 				>>>>>>>>> TODO: should start timeout here, to resync if something goes wrong...
	mov		r4, #1<<31 						// 				Store ~start bit (bit 31 because we shift bits in from the left (LSB is
											//				sent first)). The actual start bit is low. We store a '1' so that we can
	str		r4, [r1, #KBD_BYTE]				//				detect when 11 bits have been received - by TSTing 11th bit from left
	orr		r3, #1<<KBD_RCVG
	str		r3, [r1, #KBD_STATUS]			// 	set receiving a char = YES
	b		exit							// 	EXIT (return to irq:) }
1:
	ldr		r4, [r1, #KBD_BYTE]
	subs	r2,	r0, #0						// is bit low? Copies r0 to r2 and sets flags as for CMP r0, #0 at the same time.
	beq		2f								// eq: bit was low. skip forward
	mov		r2, #1<<31						// ne: bit was high... (originally I was shifting in from the right - so this instr didn't exist)
	ldr		r3, [r1, #KBD_BITS]
	add		r3, #1							// add 1 to count of high bits.
	str		r3, [r1, #KBD_BITS]				// drop through to 2:
2:
	add		r4, r2, r4, lsr #1				// in both cases shift bit into byte. (If r0 was low, then r2 is low)
	str		r4, [r1, #KBD_BYTE]
	tst		r4, #1 << 21					// Is (~start bit) set in correct position?
	beq		exit							// eq: No, not end of byte yet. Wait for next IRQ. EXIT (ret to irq:)
											// ne: means bit WAS set, ie. receiving byte completed
	lsls	r4, #1							// shift out stop bit into C flag
	bcc		invalid							// invalid stop bit (if C-flag low)

											// parity bit is set if n(data bits set) = even to make n(data + parity) = ODD
	lsrs	r3, #1							// Therefore n(data + parity + stop) = even. And lowest bit of HiBitsCount should be 0.
	bcs		invalid							// invalid parity

	lsl		r4, #1							// shift out (get rid of) parity bit
	lsr		r4, #24							// gets rid of ~start bit and leaves the received byte in bits [7:0]

											// scan codes are either:
											// <code>+										(for simple key press. Repeated after delay)
											// <F0> <code>									(for simple key release)
											// [<E0|E1> <code>]+							(for extended key press. Repeated after delay)
											// <E0|E1> <F0> <code>							(for extended key release)
											// <E0> <12>  [<E0> <7C>]+						(for PrtScr press)
											// <E0> <F0> <7C>  <E0> <F0> <12>				(for PrtScr release)
											// [<E1> <14> <77> <E1> <F0> <14> <F0> <77>]+	(for Pse/Brk press. Repeated after delay)

valid_char_rcvd:							// r1 = kbd_buff_info. r4 = scancode byte  (r0,r2,r3 junk)

	str		r4, [r1, #KBD_SCANCODE]
	ldr		r2, [r1, #KBD_STATUS]
	cmp		r4, #0xE0						// is this scancode (the start of) a MULTI?
	cmpne	r4, #0xE1
	orreq	r2, #1<<KBD_MULTI				// 		if it is, then set the MULTI flag ...
	streq	r2, [r1, #KBD_STATUS]			// 		... store it ...
	beq		next_byte						// 		and get next code.

	tstne	r2, #1<<KBD_MULTI				// if it ISN'T (E0 or E1) then see if LAST code was a MULTI
	bicne	r2, #1<<KBD_MULTI				// 		if it was, then clear the MULTI flag (because this code isn't any more) ...
	strne	r2, [r1, #KBD_STATUS]			// 		... store it ...
	movne	r2, #0							//		... and set ...
	moveq	r2, #-1							// 		... r2 = last key was MULTI ? 0 : -1 (used later, to see if last key was MULTI)
											//      (it's easier to have this in a register rather than keep looking at the STATUS flags)

											// scan codes sequences are now either:
											// <code>+										(for simple key press. Repeated after delay)
											// <F0> <code>									(for simple key release)
											// [{M} <code>]+								(for extended key press. Repeated after delay)
											// <F0> <code>   {M}							(for extended key release)
											// {M} <12> [{M} <7C>]+							(for PrtScr press)
											// [{M} <F0> <7C>] {M} <F0> <12>				(for PrtScr release)
											// [{M} <14> <77> {M} <F0> <14> <F0> <77>]+		(for Pse/Brk press. Repeated after delay)

	cmp		r4, #0xF0						// the key-up scancode
	moveq	r0, #KEYUP						// myscii KEYUP
	beq		3f								// we don't look up 0xF0 (or 0xE0, 0xE1. Prefbly nothing above 0x7F) in the LUT
	ldrne	r3, =kbd_LUT					// >>>> SCANCODE -> MYSCII LOOKUP HERE <<<< 
	ldrneb	r0, [r3, r4]					// r0 is MYSCII val for scancode in BYTE ie a key or KEYUP (sans optional E0/E1)
											// <E0,E1> has effectively been ignored (except for setting a flag)

3: 											// r0 = LUT code.  r1 = kbd_buff_info.  r2 = MULTI?0:-1.  r3 = kbd_LUT(posbly) (junk).  r4 = scancode byte (junk)

											// sequences are now either:
											// <MYcode>+														(for simple key press. Repeated after delay)
											// <KEYUP> <MYcode>													(for simple key release)
											// [{M} <MYcode>]+													(for extended key press. Repeated after delay)
											// {M} <KEYUP> <MYcode>   											(for extended key release)
											// {M} <LSHIFT> [{M} <KPD*>]+										(for PrtScr press)
											// [{M} <KEYUP> <KPD*>] {M} <KEYUP> <LSHIFT>						(for PrtScr release)
											// [{M} <LCTRL> <NUMLCK> {M} <KEYUP> <LCTRL> <KEYUP> <NUMLCK>]+		(for Pse/Brk press. Repeated after delay)

	cmp		r2, #0							// r2 = 0 if was MULTI, -1 if wasn't
	bne		4f								// ne: last code wasn't MULTI. skip forward.
	cmp		r0, #0x90						// eq: last code WAS: most MULTI keys correspond to the same scancode without the E0 ...
	cmpne	r0, #0x92						// ... except a couple which we deal with here.
	addeq	r0, #1							// 90,92 -> 91,93 if was multi. Then it's RCTRL and RALT
											// LALT, LCTRL correspond to RALT, RCTRL.
											// the movement keys corrspond to the alternate functions on the keypad
											// so both keypad and movement give the SAME movement codes
											// if NUMLOCK were active, would need to check if MULTI. if not, change to numpad.

											// So, now it's either a normal keycode, or a KEYUP.
											// if it's a KEYUP then set WASKEYUP flag. (If it was also MULTI then reset the MULTI flag
											// (ie. pass it through to next byte)) ... and get next byte
4: 											// r0 = LUT code.  r1 = kbd_buff_info.  r2 = MULTI?0:-1.  r3,r4 junk
	ldr		r3, [r1, #KBD_STATUS]

	cmp		r0, #KEYUP
	bne		5f
	orr		r3, #1<<KBD_WASKEYUP
	cmp		r2, #0
	orreq	r3, #1<<KBD_MULTI
	str		r3, [r1, #KBD_STATUS]
	b		next_byte
5:
	tst		r3, #1<<KBD_WASKEYUP			// if it wasn't a KEYUP, see if LAST code was.
	bicne	r3, #1<<KBD_WASKEYUP			// if it was, clear the flag
	strne	r3, [r1, #KBD_STATUS]
	movne	r4, #0							// and set r4 = (0/-1) instead.
	moveq	r4, #-1
											// r0 = LUT code.  r1 = kbd_buff_info.  r2 = MULTI?0:-1.  r3 junk.  r4 = KEYUP?0:-1

											// sequences are now either:
											// <MYcode>+												(for simple key press. Repeated after delay)
											// {K} <MYcode>												(for simple key release)
											// [{M} <MYcode>]+											(for extended key press. Repeated after delay)
											// {M,K} <MYcode>   										(for extended key release)
											// {M} <LSHIFT> [{M} <KPD*>]+								(for PrtScr press)
											// [{M,K} <KPD*>] {M,K} <LSHIFT>							(for PrtScr release)
											// [{M} <LCTRL> <NUMLCK> {M,K} <LCTRL> {M,K} <NUMLCK>]+		(for Pse/Brk press. Repeated after delay)

											// <MYcode> is now either a simple key, or a modifer. {M(ie r2=0) and/or K{ie r4=0) flags may be set}

	cmp		r4, #0 							// if last code was KEYUP then ignore this code, unless it's a modifier, in which case it gets unset.
	bne		6f								// ne: last key wasn't a KEYUP, skip forward
	cmp		r0, #LSHIFT
	blo		7f								// lo: was KEYUP, but isn't a modifer
	cmp		r0, #RCMD						// if !(LSHIFT ≤ MYcode ≤ RCMD) skip ahead
	bhi		7f
											// else it IS a modifier. UNSET the flag for this modifer
	mov		r2, r0							// [save a copy of r0, the LUT val]
	sub		r0, #LSHIFT						// LUT vals for  LSHIFT,RSHIFT, LALT,RALT, WSHIFT,WCTRL, LCTRL,RCTRL, LCMD,RCMD = 0x8e-97   r0 --> 0-9
	lsr		r0, #1							// r0 --> 0,1,2,3,4 (ie. MODifier flag KBD_SHIFT, KBD_ALT, KBD_WEIRD, KBD_CTRL, KBD_CMD)
	mov		r3, #1
	ldr		r4, [r1, #KBD_MODS]
	bic		r4, r3, LSL r0
	str		r4, [r1, #KBD_MODS]

	cmp		r0, #2							// if it's a true modifier (SHIFT or ALT or WEIRD)
	bls		next_byte						// then get next byte
											// else put KEYUP and the pseudo-modifier in the buffer...
	mov		r0, #KEYUP						// (You don't get repeats of keyup events)
	bl		put_in_buff
	mov		r0, r2							// [the copy of r0]
	bl		put_in_buff						// ... and then fall through to get next byte
7:
	b		next_byte						// if last key WAS KEYUP, but this key isn't a modifier, then ignore it. get next byte.

6: 											// simple key, or any modifier keydown. (last code WASN'T a KEYUP)
	cmp		r0, #LSHIFT
	blo		8f
	cmp		r0, #RCMD						// if !(LSHIFT ≤ MYcode ≤ RCMD) skip ahead
	bhi		8f
											// else it IS a modifier. SET the modifier flag for this modifier
	mov		r2, r0							// [save copy of r0, the LUT val]
	sub		r0, #LSHIFT						// LUT vals for  LSHIFT,RSHIFT, LALT,RALT, WSHIFT,WCTRL, LCTRL,RCTRL, LCMD,RCMD = 0x8e-97   r0 --> 0-9
	lsr		r0, #1							// r0 --> 0,1,2,3,4 (ie. MODifier flag KBD_SHIFT, KBD_ALT, KBD_WEIRD, KBD_CTRL, KBD_CMD)

	ldr		r4, [r1, #KBD_MODS]
											// ----------------------------------------------------------
											// r0 = MOD flag (0-4).  r1 = kbd_buff_info.  r2=LUT code.  r3 = junk.  r4 = KBD_MODS
											// If this is a pseudo-modifier only add it to buffer if it's the first one
											// (ie flag not set - have to check here, before we set flags)
	mov		r3, r0							// [save a copy of r0, the MOD flag]
	cmp		r0, #KBD_CTRL
	bne		10f
	tsteq	r4, #1<<KBD_CTRL
	moveq	r0, #CTRL
	b		11f
10:
	cmp		r0, #KBD_CMD
	bne		12f
	tsteq	r4, #1<<KBD_CMD
	moveq	r0, #CMD
11:											// entered (if r0 was CTRL or CMD) with condition flags set by TST
	bne		12f								// ne: corresponding flag WAS set, so don't put key in buff
	bl		put_in_buff						// >>>> TODO   LUT vals are 0x92,93, 94,95 --> MYSCII save vals = CTRL:0x01   CMD:0x11 
	mov		r0, r3							// [restore r0 to MOD flag] and continue.
12: 										// ----------------------------------------------------------
	mov		r3, #1
	orr		r4, r3, LSL r0
	str		r4, [r1, #KBD_MODS]				// set modifier flags...

	b		next_byte						// ...then get next byte

8:											// simple key only. Apply true modifiers and put in the buffer
	ldr		r4, [r1, #KBD_MODS]
	tst		r4, #1<<KBD_WEIRD
											// do PrtScr or Pse/Brk
	beq		13f								// if not a weirdo, hop forward
	cmp		r0, #NUMLOCK
	movne	r0, #PRTSCR
	moveq	r0, #PSE_BRK
											// >>>>> TODO    deal with LUT codes > 0x7f
											// F1-F12 coded as LUT 0x81-0x8c.							Store as <FN> <n> ?
											// CapsLock, NumLock, ScreenLock coded as LUT 0xA0,A1,A2.	Don't store. Set flag?
											// PrtScr, PseBrk, RMenu = coded as LUT 0x[A3],[A4],A5.		Don't store. Do nothing.
13:
	tst		r4, #1<<KBD_SHIFT
	ldrne	r2, [r1, #KBD_SCANCODE]
	ldrne	r3, =kbd_LUT2
	ldrne	r0, [r3, r2]
	tst		r4, #1<<KBD_ALT
	addne	r0, #0x80
											// simple key. fall through to add it to buffer. (Hooray! At last :-)

put_in_buff:								// r0 = LUT/myscii code to store
	ldr		r1, =kbd_buff_info

	ldm		r1, {r2-r4}						// r2 = BUFFPTR, r3 = NVALCHARS, r4 = BUFFEND
	strb	r0, [r2], #1					// store the ascii val at buff_ptr and increment buff_ptr.
	cmp		r2, r4							// if buff_ptr is at end of buffer...
	ldreq	r2, =kbd_buffer					// ... then move it to beginning of buffer
	add		r3, #1							// increment num valid chars in buffer
	stm		r1, {r2,r3}						// r2 = BUFFPTR, r3 = NVALCHARS

	mov		r2, #32							// == BUFFSIZE
	cmp		r0, r2							// nval_chars >= buffsize?
	beq		overflow						// signal buffer overflow?
											// fall through to wait for new byte
overflow:
											// >>>>>> TODO  disable further interrupts till some chars are read? Or, at least don't put in buffer.
											// Need to listen for ^C, clr buffer etc...
											// handle really important keys like ESC, Pause/Break, ^C, ^Z ?
											// other kbd events are done by whichever process is reading key presses from the buffer.
next_byte:
invalid:
	ldr		r3, [r1, #KBD_STATUS]
	bic		r3, #1<<KBD_RCVG				// receiving a char = NO. Leave WASKEYUP and MULTI flags alone!
	str		r3, [r1, #KBD_STATUS]	
exit:
	pop		{r0-r4,pc}						// EXIT (return to irq:)


.align 5

.globl kbd_buff_info						// 32 bytes = Cache line length. => good for cacheing :-)
kbd_buff_info:
	.word	0								// KBD_BUFFPTR,		#0
	.word	0  			               		// KBD_NVALCHARS,	#4
	.word	0 								// KBD_BUFFEND,		#8
	.word	0		   			    		// KBD_SCANCODE,	#C
	.word	0         			        	// KBD_STATUS,		#10			// KBD_STATUS = RCVG   MULTI(E0, E1)   KEYUP
	.word	0          			        	// KBD_BYTE,		#14
	.word	0          			        	// KBD_BITS,		#18
	.word	0                  				// KBD_MODS,		#1C			// true modifiers: SHIFT ALT pseudo-modifiers: CTRL CMD


.globl kbd_buffer
kbd_buffer:
.space 32									// kbd buffer. BUFFSIZE = 32 bytes

kbd_LUT:
//       0     1     2     3     4     5     6     7     8     9     a     b     c     d     e     f
.byte 0000, 0x89,  000, 0x85, 0x83, 0x81, 0x82, 0x8c,  000, 0x8a, 0x88, 0x86, 0x84, 0x09,   '`,  000	// 0
.byte 0000, 0x90, 0x8e,  000, 0x92,   'q,   '1,  000,  000,  000,   'z,   's,   'a,   'w,   '2, 0x94	// 1
.byte 0000,   'c,   'x,   'd,   'e,   '4,   '3, 0x95,  000, 0x20,   'v,   'f,   't,   'r,   '5, 0xa5	// 2
.byte 0000,   'n,   'b,   'h,   'g,   'y,   '6,  000,  000,  000,   'm,   'j,   'u,   '7,   '8,  000	// 3
.byte 0000, 0x2c,   'k,   'i,   'o,   '0,   '9,  000,  000,   '.,   '/,   'l,   ';,   'p,   '-,  000	// 4
.byte 0000,  000, 0x27,  000,   '[,   '=,  000,  000, 0xa0, 0x8f, 0x0a,   '],  000, 0x5c,  000,  000	// 5
.byte 0000,   '<,  000,  000,  000,  000, 0x08,  000,  000, 0x07,  000, 0x02, 0x06,  000,  000,  000	// 6
.byte 0x0e, 0x7f, 0x05,   '5, 0x03, 0x04, 0x8d, 0xa1, 0x8b,   '+, 0x0d,   '-,   '*, 0x0c, 0xa2,  000    // 7
.byte 0000,  000,  000, 0x87																			// 8

kbd_LUT2:
//       0     1     2     3     4     5     6     7     8     9     a     b     c     d     e     f
.byte 0000, 0x89,  000, 0x85, 0x83, 0x81, 0x82, 0x8c,  000, 0x8a, 0x88, 0x86, 0x84, 0x09,   '~,  000	// 0
.byte 0000, 0x90, 0x8e,  000, 0x92,   'Q,   '!,  000,  000,  000,   'Z,   'S,   'A,   'W,   '@, 0x94	// 1
.byte 0000,   'C,   'X,   'D,   'E,   '$,   '#, 0x95,  000, 0x20,   'V,   'F,   'T,   'R,   '%, 0xa5	// 2
.byte 0000,   'N,   'B,   'H,   'G,   'Y,   '^,  000,  000,  000,   'M,   'J,   'U,   '&,   '*,  000	// 3
.byte 0000,   '<,   'K,   'I,   'O,   '),   '(,  000,  000,   '>,   '?,   'L,   ':,   'P,   '),  000	// 4
.byte 0000,  000, 0x27,  000,   '{,   '+,  000,  000, 0xa0, 0x8f, 0x0a,   '],  000, 0x5c,  000,  000	// 5
.byte 0000,   '>,  000,  000,  000,  000, 0x08,  000,  000, 0x17,  000, 0x12, 0x16,  000,  000,  000	// 6
.byte 0x1e, 0x1f, 0x15,   '5, 0x13, 0x14, 0x8d, 0xa1, 0x8b,   '+, 0x1d,   '-,   '*, 0x1c, 0xa2,  000    // 7
.byte 0000,  000,  000, 0x87																			// 8
