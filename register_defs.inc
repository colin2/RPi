// Frame Buffer Info Offsets
.set FB_WIDTH,			0x00000000
.set FB_HEIGHT,			0x00000004
.set FB_V_WIDTH,		0x00000008
.set FB_V_HEIGHT,		0x0000000C
.set FB_PITCH,			0x00000010
.set FB_DEPTH,			0x00000014
.set FB_X,				0x00000018
.set FB_Y,				0x0000001C
.set FB_PTR,			0x00000020
.set FB_SIZE,			0x00000024

// System Timer Registers p.172
.set ST_base,			0x20003000
.set ST_CS,				0x00000000
.set ST_CLO,			0x00000004
.set ST_CHI,			0x00000008
.set ST_C0,				0x0000000C
.set ST_C1,				0x00000010
.set ST_C2,				0x00000014
.set ST_C3,				0x00000018

// Interrupts p.109
.set IRQ_base,			0x2000B200
.set IRQ_BASC_PDG,		0x00000000
.set IRQ_IRQ1_PDG,		0x00000004
.set IRQ_IRQ2_PDG,		0x00000008
.set IRQ_FIQ_CTL,		0x0000000C
.set IRQ_IRQ1_EN,		0x00000010
.set IRQ_IRQ2_EN,		0x00000014
.set IRQ_BASC_EN,		0x00000018
.set IRQ_IRQ1_DIS,		0x0000001C
.set IRQ_IRQ2_DIS,		0x00000020
.set IRQ_BASC_DIS,		0x00000024

// Mailbox_base (Baking Pi/Screen01)
.set MBX_base,			0x2000B880
.set MBX_READ,			0x00000000
.set MBX_POLL,			0x00000010
.set MBX_SNDR,			0x00000014
.set MBX_STAT,			0x00000018
.set MBX_CONF,			0x0000001C
.set MBX_WRITE,			0x00000020

// Mailbox Property Tags (https://github.com/raspberrypi/firmware/wiki/Mailbox-property-interface)
.set    MBOX_CHANNEL_FRAMEBUFFER,           1
.set    MBOX_CHANNEL_PTAGS,                 8

.set    TAG_GET_CLK_STATE,          0x00030001
.set    TAG_SET_CLK_STATE,          0x00038001

.set    TAG_GET_CLK_RATE,           0x00030002
.set    TAG_SET_CLK_RATE,           0x00038002
.set    TAG_GET_MAX_CLK_RATE,       0x00030004
.set    TAG_SET_MAX_CLK_RATE,       0x00038004
.set    TAG_GET_MIN_CLK_RATE,       0x00030007
.set    TAG_SET_MIN_CLK_RATE,       0x00038007

.set    TAG_GET_VOLTAGE,           0x00030003
.set    TAG_SET_VOLTAGE,           0x00038003
.set    TAG_GET_MAX_VOLTAGE,       0x00030005
.set    TAG_SET_MAX_VOLTAGE,       0x00038005
.set    TAG_GET_MIN_VOLTAGE,       0x00030008
.set    TAG_SET_MIN_VOLTAGE,       0x00038008

.set    TAG_GET_TURBO,				0x00030009
.set    TAG_SET_TURBO,				0x00038009

.set    TAG_GET_TEMP,				0x00030006
.set    TAG_GET_MAX_TEMP,			0x0003000A

.set    TAG_CLKID_EMMC,             0x00000001
.set    TAG_CLKID_UART,             0x00000002
.set    TAG_CLKID_ARM,              0x00000003
.set    TAG_CLKID_CORE,             0x00000004
.set    TAG_CLKID_V3D,              0x00000005
.set    TAG_CLKID_H264,             0x00000006
.set    TAG_CLKID_ISP,              0x00000007
.set    TAG_CLKID_SDRAM,            0x00000008
.set    TAG_CLKID_PIXEL,            0x00000009
.set    TAG_CLKID_PWM,              0x0000000A

.set    TAG_VOLT_ID_CORE,			0x00000001
.set    TAG_VOLT_ID_SDRAM_C,		0x00000002
.set    TAG_VOLT_ID_SDRAM_P,		0x00000003
.set    TAG_VOLT_ID_SDRAM_I,		0x00000004


// GPIO Registers p.89
.set GPIO_base,			0x20200000
.set GPIO_GPFSEL0,		0x00000000
.set GPIO_GPFSEL1,		0x00000004
.set GPIO_GPFSEL2,		0x00000008
.set GPIO_GPFSEL3,		0x0000000C
.set GPIO_GPFSEL4,		0x00000010
.set GPIO_GPFSEL5,		0x00000014
.set GPIO_GPSET0,		0x0000001C
.set GPIO_GPSET1,		0x00000020
.set GPIO_GPCLR0,		0x00000028
.set GPIO_GPCLR1,		0x0000002C
.set GPIO_GPLEV0,		0x00000034
.set GPIO_GPLEV1,		0x00000038
.set GPIO_GPEDS0,		0x00000040
.set GPIO_GPEDS1,		0x00000044
.set GPIO_GPREN0,		0x0000004C
.set GPIO_GPREN1,		0x00000050
.set GPIO_GPFEN0,		0x00000058
.set GPIO_GPFEN1,		0x0000005C
.set GPIO_GPHEN0,		0x00000064
.set GPIO_GPLEN1,		0x00000068
.set GPIO_GPLEN0,		0x00000070
.set GPIO_GPHEN1,		0x00000074
.set GPIO_GPAREN0,		0x0000007C
.set GPIO_GPAREN1,		0x00000080
.set GPIO_GPAFEN0,		0x00000088
.set GPIO_GPAFEN1,		0x0000008C
.set GPIO_GPPUD,		0x00000094
.set GPIO_GPPUDCLK0,	0x00000098
.set GPIO_GPPUDCLK1,	0x0000009C
.set GPIO_Test,			0x000000B0

						// offsets
.set KBD_BUFFPTR,		0x0		// current position within buffer
.set KBD_NVALCHARS,		0x4
.set KBD_BUFFEND,		0x8		// ( BUFFSTART is =kbd_buffer label)
.set KBD_SCANCODE,		0xC
.set KBD_STATUS,		0x10	// status/mode/flags
.set KBD_BYTE,			0x14	// bits being accumulated during a read
.set KBD_BITS,			0x18	// count of high bits (used for parity check)
.set KBD_MODS,			0x1C	// active modifiers

						// flag bit nums
.set KBD_RCVG,			0x0		// are we already receiving bits?
.set KBD_MULTI,			0x1		// last byte was E0 or E1
.set KBD_WASKEYUP,		0x2		// last byte was KEYUP

						// flag bit nums
.set KBD_SHIFT,			0x0		// true modifers
.set KBD_ALT,			0x1		// 		"
.set KBD_WEIRD,			0x2		// stupid modifers treated as true modifier, so don't get stored (only used for PrtScr and PauseBrk)
.set KBD_CTRL,			0x3		// pseudo-modifiers
.set KBD_CMD,			0x4		// 		"				(aka WIN or GUI)
.set KBD_FN,			0x5		// 		"
.set KBD_MENU,			0x6		// 		"

						// GPIO nums (pins are on header P2)
.set KBD_CLK,			28
.set KBD_DATA,			29
.set MOUSE_CLK,			30
.set MOUSE_DATA,		31

						// some myscii values which get referred to specifically
.set CTRL,				0x01
.set META,				0x0F
.set KEYUP,				0x10
.set CMD,				0x11

						// some LUT values which get referred to specifically
.set LSHIFT,			0x8e
.set RSHIFT,			0x8f
.set LALT,				0x90
.set RALT,				0x91
.set LCTRL,				0x92
.set RCTRL,				0x93
.set LCMD,				0x94
.set RCMD,				0x95

.set CAPSLOCK,			0xa0
.set NUMLOCK,			0xa1
.set SCREENLOCK,		0xa2
.set PRTSCR,			0xa3
.set PSE_BRK,			0xa4
