( 6809 FORTH ASSEMBLER                            JNS 08/28/83 )
." LOADING 6809 ASSEMBLER" CR
VOCABULARY ASSEMBLER IMMEDIATE
ASSEMBLER DEFINITIONS HEX

: :CASE
    <BUILDS ] DOES> SWAP DUP + + @ EXECUTE ;
( : CASE; [COMPILE] [ ; )
: IMM8 HERE 1- C@ F AND >R R 3 = R C = OR R> E = OR
    IF ( 16 bit imm ) , ELSE ( 8 bit imm ) C, THEN  ;
: IX1 DUP 7 AND IF DUP 1 AND
    IF ( 16 bit pcr )  C, HERE 2+ - ,
    ELSE ( 8 bit pcr ) C, HERE 1+ - C, THEN
        ELSE ( 8 bit disp ) C, C, THEN ;
:CASE  OPRANDS  ,  IMM8  IX1  C, [
                                                             -->
( 6809 FORTH ASSEMBLER                            JNS 08/28/83 )
0  VARIABLE OPTYPE
: OPCODE    ( IMM EXT INX PREFIX OPCODE <NMONIC> )
    <BUILDS  C,  >R  C,  C,  R>  DUP  C,  C,
    DOES>  DUP  C@  -DUP  IF  C,  THEN       ( PREFIX )
           OPTYPE  @  +  1+  C@  C,          ( OPCODE )
           OPTYPE  @  OPRANDS                ( OPRANDS )
           0  OPTYPE  !  ;
00 7E 6E  0  OPCODE  JMP,    00 BD AD  0  OPCODE  JSR,
86 B6 A6  0  OPCODE  LDA,    00 B7 A7  0  OPCODE  STA,
C6 F6 E6  0  OPCODE  LDB,    00 F7 E7  0  OPCODE  STB,
CC FC EC  0  OPCODE  LDD,    00 FD ED  0  OPCODE  STD,
8E BE AE  0  OPCODE  LDX,    00 BF AF  0  OPCODE  STX,
8E BE AE 10  OPCODE  LDY,    00 BF AF  0  OPCODE  STY,
CE FE EE 10  OPCODE  LDS,    00 FF EF 10  OPCODE  STS,
CE FE EE  0  OPCODE  LDU,    00 FF EF  0  OPCODE  STU,       -->
( 6809 FORTH ASSEMBLER                            JNS 08/28/83 )
89 B9 A9  0  OPCODE  ADCA,   82 B2 A2  0  OPCODE  SBCA,
C9 F9 E9  0  OPCODE  ADCB,   C2 F2 E2  0  OPCODE  SBCB,
8B BB AB  0  OPCODE  ADDA,   80 B0 A0  0  OPCODE  SUBA,
CB FB EB  0  OPCODE  ADDB,   C0 F0 E0  0  OPCODE  SUBB,
C3 F3 E3  0  OPCODE  ADDD,   83 B3 A3  0  OPCODE  SUBD,
84 B4 A4  0  OPCODE  ANDA,   C4 F4 E4  0  OPCODE  ANDB,
00 78 68  0  OPCODE  ASL,    00 77 67  0  OPCODE  ASR,
86 B5 A5  0  OPCODE  BITA,   C5 F5 E5  0  OPCODE  BITB,
00 7F 6F  0  OPCODE  CLR,    00 73 63  0  OPCODE  COM,
81 B1 A1  0  OPCODE  CMPA,   C1 F1 E1  0  OPCODE  CMPB,
83 B3 A3 10  OPCODE  CMPD,   8C BC AC  0  OPCODE  CMPX,
00 7A 6A  0  OPCODE  DEC,    00 7C 6C  0  OPCODE  INC,
88 B8 A8  0  OPCODE  EORA,   C8 F8 E8  0  OPCODE  EORB,
00 78 68  0  OPCODE  LSL,    00 74 64  0  OPCODE  LSR,
00 00 30  0  OPCODE  LEAX,   00 00 31  0  OPCODE  LEAY,      -->
( 6809 FORTH ASSEMBLER                            JNS 08/28/83 )
00 00 32  0  OPCODE  LEAS,    00 00 33  0  OPCODE  LEAU,
00 70 60  0  OPCODE  NEG,     8A BA AA  0  OPCODE  ORA,
CA FA EA  0  OPCODE  ORB,     00 79 69  0  OPCODE  ROL,
00 76 66  0  OPCODE  ROR,     00 7D 6D  0  OPCODE  TST,
: INH <BUILDS C, DOES> C@ C, ;
3A INH ABX,    48 INH ASLA,    58 INH ASLB,    47 INH ASRA,
57 INH ASRB,   4F INH CLRA,    5F INH CLRB,    43 INH COMA,
53 INH COMB,   19 INH DAA,     4A INH DECA,    5A INH DECB,
4C INH INCA,   5C INH INCB,    48 INH LSLA,    58 INH LSLB,
44 INH LSRA,   54 INH LSRB,    3D INH MUL,     40 INH NEGA,
50 INH NEGB,   12 INH NOP,     49 INH ROLA,    59 INH ROLB,
46 INH RORA,   56 INH RORB,    39 INH RTS,     1D INH SEX,
3F INH SWI,    4D INH TSTA,    5D INH TSTB,    3B INH RTI,
                                                            -->

( 6809 FORTH ASSEMBLER                            JNS 08/28/83 )
: INX                           ( <POSTBYTE> <MODE> INX <NAME> )
    <BUILDS C, C,
    DOES> DUP C@ OPTYPE ! 1+ C@ ;
88 2 INX ,X     A8 2 INX ,Y     C8 2 INX ,U     E8 2 INX ,S
84 3 INX 0,X    A4 3 INX 0,Y    C4 3 INX 0,U    E4 3 INX 0,S
83 3 INX ,--X   A3 3 INX ,--Y   C3 3 INX ,--U   E3 3 INX ,--S
81 3 INX ,X++   A1 3 INX ,Y++   C1 3 INX ,U++   E1 3 INX ,S++
82 3 INX ,-X    A2 3 INX ,-Y    C2 3 INX ,-U    E1 3 INX ,-S
80 3 INX ,X+    A0 3 INX ,Y+    C0 3 INX ,U+    E0 3 INX ,S+
86 3 INX A,X    A6 3 INX A,Y    C6 3 INX A,U    E6 3 INX A,S
85 3 INX B,X    A5 3 INX B,Y    C5 3 INX B,U    E5 3 INX B,S
8B 3 INX D,X    AB 3 INX D,Y    CB 3 INX D,U    EB 3 INX D,S

: [] ( indirect index ) 10 OR ; ( <indexmode> [] <opcode> )
: ## 1 OPTYPE ! ;               ( operand ## opcode )        -->
( 6809 FORTH ASSEMBLER                            JNS 08/28/83 )
: IF,      C,  HERE  0  C,  ;
: THEN,    HERE  OVER  -  1-  SWAP  C!  ;
: ELSE,    20  IF,  SWAP  THEN,  ;

: CBACK       HERE  -  1-  C,  ;
: BEGIN,     HERE  ;
: UNTIL,     C,  CBACK  ;
: WHILE,     IF,  ;
: REPEAT,    SWAP  20  UNTIL,  THEN,  ;

26 CONSTANT EQ   2A CONSTANT MI 2F CONSTANT GT 2D CONSTANT GE
23 CONSTANT .HI. 25 CONSTANT HS 27 CONSTANT NE 2B CONSTANT PL
2E CONSTANT LE   2C CONSTANT LT 22 CONSTANT LS 24 CONSTANT .LO.

                                                             -->
( 6809 FORTH ASSEMBLER                            JNS 08/28/83 )

0 VARIABLE PPB    0 VARIABLE TEB

: REGID    ( PP-BITS TE-BITS  REGID  .A )
    <BUILDS  C,  C,
    DOES>   @  100 /MOD  SWAP
            PPB @ OR PPB !    TEB @ 10 * OR TEB !  ;

01 A REGID .CC   02 8 REGID .A   04 9 REGID .B   08 B REGID .DP
10 1 REGID .X    20 2 REGID .Y   40 4 REGID .S   40 3 REGID .U
80 5 REGID .PC   06 0 REGID .D                   10 1 REGID .W

: RBUILD
    SWAP  C,  @  C,  0 PPB !  0 TEB !  ;
                                                             -->
( 6809 FORTH ASSEMBLER                            JNS 08/28/83 )
: TFR,   1F  TEB  RBUILD  ;
: EXG,   1E  TEB  RBUILD  ;

: PSHS,  34  PPB  RBUILD  ;
: PULS,  35  PPB  RBUILD  ;
: PSHU,  36  PPB  RBUILD  ;
: PULU,  37  PPB  RBUILD  ;

: MPX, 113F , C, ;                   ( embed MPX/9 system call )
: NEXT, ,Y++ LDX, 0,X [] JMP, ;

: CODE  CREATE SMUDGE ;

8C 2 INX <,PCR    8D 2 INX ,PCR
                                                             -->
( 6809 FORTH ASSEMBLER                            JNS 08/28/83 )

: SAVEIP, .Y PSHS, ;
: RESTOREIP, .Y PULS, ;

FORTH  DEFINITIONS  DECIMAL

( : ASSEMBLER   [COMPILE] ASSEMBLER  ;  IMMEDIATE              )

: CODE        [COMPILE] ASSEMBLER ASSEMBLER CODE ;     ;S
