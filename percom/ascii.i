		IFNDEF ASCII__I

NUL 	EQU $00  ;  NUL ^@	\0	Null
SOH		EQU $01  ;	SOH	^A		Start of Heading
CTRL_A	equ SOH
STX		EQU $02  ;	STX	^B		Start of Text
CTRL_B	equ STX
ETX		EQU $03  ;	ETX	^C		End of Text
BRK		EQU ETX
CTRL_C	equ ETX
EOT		EQU $04  ;	EOT	^D		End of Transmission
CTRL_D	equ EOT
ENQ		EQU $05  ;	ENQ	^E		Enquiry
CTRL_E	equ ENQ
ACK		EQU $06  ;	ACK	^F		Acknowledgement
CTRL_F	equ ACK
BEL		EQU $07  ;	BEL	^G	\a	Bell
CTRL_G	equ BEL
BS		EQU $08  ;	BS	^H	\b	Backspace[f][g]
CTRL_H	equ BS
HT		EQU $09  ;	HT	^I	\t	Horizontal Tab[h]
CTRL_I	equ HT
LF		EQU $0A  ;	LF	^J	\n	Line Feed
CTRL_J	equ LF
VT		EQU $0B  ;	VT	^K	\v	Vertical Tab
CTRL_K	equ VT
FF		EQU $0C  ;	FF	^L	\f	Form Feed
CTRL_L	equ FF
CR		EQU $0D  ;	CR	^M	\r	Carriage Return[i]
CTRL_M	equ CR
SO		EQU $0E  ;	SO	^N		Shift Out
CTRL_N	equ SO
SI		EQU $0F  ;	SI	^O		Shift In
CTRL_O	equ SI
DLE		EQU $10  ;	DLE	^P		Data Link Escape
CTRL_P	equ DLE
DC1		EQU $11  ;	DC1	^Q		Device Control 1 (often XON)
CTRL_Q	equ DC1
DC2		EQU $12  ;	DC2	^R		Device Control 2
CTRL_R	equ DC2
DC3		EQU $13  ;	DC3	^S		Device Control 3 (often XOFF)
CTRL_S	equ DC3
DC4		EQU $14  ;	DC4	^T		Device Control 4
CTRL_T	equ DC4
NAK		EQU $15  ;	NAK	^U		Negative Acknowledgement
CTRL_U	equ NAK
SYN		EQU $16  ;	SYN	^V		Synchronous Idle
CTRL_V	equ SYN
ETB		EQU $17  ;	ETB	^W		End of Transmission Block
CTRL_W	equ ETB
CAN		EQU $18  ;	CAN	^X		Cancel
CTRL_X	equ CAN
EM		EQU $19  ;	EM	^Y		End of Medium
CTRL_Z	equ EM
ESC		EQU $1B  ;	ESC	^[	\e	Escape
CTRL_LBRAKET	EQU ESC
FS		EQU $1C  ;	FS	^\		FILE SEPARATOR
CTRL_BACKSLASH	EQU FS
GS		EQU $1D  ;	GS	^]		GROUP SEPARATOR
CTRL_RBRAKET	EQU GS
RS		EQU $1E  ;	RS	^^		RECORD SEPARATOR
CTRL_CARROT	EQU RS
US		EQU $1F  ;	US	^_		Unit Separator
CTRL_UNDERSCORE	EQU US
SP		equ $20  ;  SP      	Space  
DEL		EQU $7F  ;	DEL	^?		Delete
CTRL_Question equ ESC

TILDE	EQU $7E  ; ~TILDE~

ASCII__I SET 1

		ENDC
