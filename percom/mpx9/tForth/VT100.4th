( vt100 terminal dependent cursor addressing)

HEX

( NEW3.SCR Screen # 131 )
( forth extensions                                jns 03/23/84 )
: \ ( end of line comment )                                     
    IN @ 64  MOD MINUS 64  1- + IN +! ; IMMEDIATE               
                                                                
: ASCII \ create ascii character literal:  ascii A              
    BL WORD HERE 1+ C@ [COMPILE] LITERAL ; IMMEDIATE            
                                                                
: CTRL \ create control character literal:  ctrl x              
    BL WORD HERE 1+ C@ 64 - [COMPILE] LITERAL ; IMMEDIATE       


: ESC[ 1B EMIT ASCII [ EMIT ;
: HOME  ESC[ ASCII H EMIT ;
: XY    ESC[ . ASCII ; EMIT . ASCII H EMIT ; ( x y -- ; move cursor to coL x, )
                                                ( row y !!!! NEED TO REMOVE TRAILING SP ON NUMBERS )
: CLRS  ESC[ ASCII J EMIT ;                  ( Clear screen from cursor )
: CLRL  ESC[ ASCII K EMIT ;                  ( Clear line from cursor  )
: UP    ESC[ ASCII A EMIT ;                  ( Move cursor up )
: DOWN  ESC[ ASCII B EMIT ;                  ( Move cursor down )
: RIGHT ESC[ ASCII C EMIT ;                  ( Move cursor Right )
: LEFT  ESC[ ASCII D EMIT ;                  ( Move cursor Left )

: ERASE-CHAR ESC[ ASCII X EMIT ;                  ( Erase character )
: ERASE-SCREEN ESC[ ." 2J" ;
: ERASE-EOS ESC[ ." 0J" ;
: ERASE-LINE ESC[ ." 2K" ;
: ERASE-EOL ESC[ ." 0K" ;


;S



\ -------------- vt100 screen control --------------
[need] cls  0 0 in/out : cls ( -- )     esc[ ." 2J"  esc[ 'H emit  ; [then]
[need] page 0 0 in/out : page ( -- )    esc[ ." 2J"  esc[ 'H emit  ; [then]
[need] home 0 0 in/out : home ( -- )    esc[ 'H emit ;               [then]
[need] at   : at ( x y -- )   esc[ 1+ n; 1+ n  'H emit             ; [then]
[need] fg   : fg ( color -- ) esc[ 30 + n 'm emit  ;  [then]
[need] bg   : bg ( color -- ) esc[ 40 + n 'm emit  ;  [then]

[need] normal 0 0 in/out : normal ( -- ) esc[ ." 0m"               ; [then]
[need] bold   0 0 in/out : bold   ( -- ) esc[ ." 1m"               ; [then]
[need] underscore 0 0 in/out : underscore ( -- )  esc[ ." 4m"      ; [then]
[need] esc[ 0 0 in/out : esc[ ( -- )    27 emit '[ emit  ;    [then]

[need] n;   : n; ( n -- )    n '; emit  ;          [then]
[need] n    : n  ( n -- ) (base) @ swap decimal <# #s #>type (base) !  ; [then]

[need] black   0 constant black   [then]
[need] red     1 constant red     [then]
[need] green   2 constant green   [then]
[need] yellow  3 constant yellow  [then]
[need] blue    4 constant blue    [then]
[need] magenta 5 constant magenta [then]
[need] cyan    6 constant cyan    [then]
[need] white   7 constant white   [then]




---------------------------------------------------

The following is a list of control sequences recognized by screen. ‘(V)’ and ‘(A)’ indicate VT100-specific and ANSI- or ISO-specific functions, respectively.

     ESC E                           Next Line
     ESC D                           Index
     ESC M                           Reverse Index
     ESC H                           Horizontal Tab Set
     ESC Z                           Send VT100 Identification String
     ESC 7                   (V)     Save Cursor and Attributes
     ESC 8                   (V)     Restore Cursor and Attributes
     ESC [s                  (A)     Save Cursor and Attributes
     ESC [u                  (A)     Restore Cursor and Attributes
     ESC c                           Reset to Initial State
     ESC g                           Visual Bell
     ESC Pn p                        Cursor Visibility (97801)
         Pn = 6                      Invisible
              7                      Visible
     ESC =                   (V)     Application Keypad Mode
     ESC >                   (V)     Numeric Keypad Mode
     ESC # 8                 (V)     Fill Screen with E's
     ESC \                   (A)     String Terminator
     ESC ^                   (A)     Privacy Message String (Message Line)
     ESC !                           Global Message String (Message Line)
     ESC k                           Title Definition String
     ESC P                   (A)     Device Control String
                                     Outputs a string directly to the host
                                     terminal without interpretation.
     ESC _                   (A)     Application Program Command (Hardstatus)
     ESC ] 0 ; string ^G     (A)     Operating System Command (Hardstatus, xterm
                                     title hack)
     ESC ] 83 ; cmd ^G       (A)     Execute screen command. This only works if
                                     multi-user support is compiled into screen.
                                     The pseudo-user ":window:" is used to check
                                     the access control list. Use "addacl :window:
                                     -rwx #?" to create a user with no rights and
                                     allow only the needed commands.
     Control-N               (A)     Lock Shift G1 (SO)
     Control-O               (A)     Lock Shift G0 (SI)
     ESC n                   (A)     Lock Shift G2
     ESC o                   (A)     Lock Shift G3
     ESC N                   (A)     Single Shift G2
     ESC O                   (A)     Single Shift G3
     ESC ( Pcs               (A)     Designate character set as G0
     ESC ) Pcs               (A)     Designate character set as G1
     ESC * Pcs               (A)     Designate character set as G2
     ESC + Pcs               (A)     Designate character set as G3
     ESC [ Pn ; Pn H                 Direct Cursor Addressing
     ESC [ Pn ; Pn f                 same as above
     ESC [ Pn J                      Erase in Display
           Pn = None or 0            From Cursor to End of Screen
                1                    From Beginning of Screen to Cursor
                2                    Entire Screen
     ESC [ Pn K                      Erase in Line
           Pn = None or 0            From Cursor to End of Line
                1                    From Beginning of Line to Cursor
                2                    Entire Line
     ESC [ Pn X                      Erase character
     ESC [ Pn A                      Cursor Up
     ESC [ Pn B                      Cursor Down
     ESC [ Pn C                      Cursor Right
     ESC [ Pn D                      Cursor Left
     ESC [ Pn E                      Cursor next line
     ESC [ Pn F                      Cursor previous line
     ESC [ Pn G                      Cursor horizontal position
     ESC [ Pn `                      same as above
     ESC [ Pn d                      Cursor vertical position
     ESC [ Ps ;...; Ps m             Select Graphic Rendition
           Ps = None or 0            Default Rendition
                1                    Bold
                2            (A)     Faint
                3            (A)     Standout Mode (ANSI: Italicized)
                4                    Underlined
                5                    Blinking
                7                    Negative Image
                22           (A)     Normal Intensity
                23           (A)     Standout Mode off (ANSI: Italicized off)
                24           (A)     Not Underlined
                25           (A)     Not Blinking
                27           (A)     Positive Image
                30           (A)     Foreground Black
                31           (A)     Foreground Red
                32           (A)     Foreground Green
                33           (A)     Foreground Yellow
                34           (A)     Foreground Blue
                35           (A)     Foreground Magenta
                36           (A)     Foreground Cyan
                37           (A)     Foreground White
                39           (A)     Foreground Default
                40           (A)     Background Black
                ...                  ...
                49           (A)     Background Default
     ESC [ Pn g                      Tab Clear
           Pn = None or 0            Clear Tab at Current Position
                3                    Clear All Tabs
     ESC [ Pn ; Pn r         (V)     Set Scrolling Region
     ESC [ Pn I              (A)     Horizontal Tab
     ESC [ Pn Z              (A)     Backward Tab
     ESC [ Pn L              (A)     Insert Line
     ESC [ Pn M              (A)     Delete Line
     ESC [ Pn @              (A)     Insert Character
     ESC [ Pn P              (A)     Delete Character
     ESC [ Pn S                      Scroll Scrolling Region Up
     ESC [ Pn T                      Scroll Scrolling Region Down
     ESC [ Pn ^                      same as above
     ESC [ Ps ;...; Ps h             Set Mode
     ESC [ Ps ;...; Ps l             Reset Mode
           Ps = 4            (A)     Insert Mode
                20           (A)     ‘Automatic Linefeed’ Mode.
                34                   Normal Cursor Visibility
                ?1           (V)     Application Cursor Keys
                ?3           (V)     Change Terminal Width to 132 columns
                ?5           (V)     Reverse Video
                ?6           (V)     ‘Origin’ Mode
                ?7           (V)     ‘Wrap’ Mode
                ?9                   X10 mouse tracking
                ?25          (V)     Visible Cursor
                ?47                  Alternate Screen (old xterm code)
                ?1000        (V)     VT200 mouse tracking
                ?1047                Alternate Screen (new xterm code)
                ?1049                Alternate Screen (new xterm code)
     ESC [ 5 i               (A)     Start relay to printer (ANSI Media Copy)
     ESC [ 4 i               (A)     Stop relay to printer (ANSI Media Copy)
     ESC [ 8 ; Ph ; Pw t             Resize the window to ‘Ph’ lines and
                                     ‘Pw’ columns (SunView special)
     ESC [ c                         Send VT100 Identification String
     ESC [ x                 (V)     Send Terminal Parameter Report
     ESC [ > c                       Send Secondary Device Attributes String
     ESC [ 6 n                       Send Cursor Position Report


( http://paulbourke.net/dataformats/ascii/ )
ASCII codes

Compiled by Paul Bourke
August 1995

Decimal   Hex     Oct      Usage                  Control
----------------------------------------------------------------------------
   0       0       0       NUL (Null)             @
   1       1       1       SOH                    A
   2       2       2       STX                    B
   3       3       3       ETX                    C
   4       4       4       EQT                    D
   5       5       5       ENQ (Answerback)       E
   6       6       6       ACK                    F
   7       7       7       BEL (Bell)             G
   8       8      10       BS  (Backspace)        H
   9       9      11       HT  (Tab)              I
  10       a      12       LF  (Linefeed)         J
  11       b      13       VT                     K
  12       c      14       FF                     L
  13       d      15       CR  (Carriage Return)  M
  14       e      16       SO                     N
  15       f      17       SI                     O
  16      10      20       DLE                    P
  17      11      21       DC1 (Xon)              Q
  18      12      22       DC2                    R
  19      13      23       DC3 (Xoff)             S
  20      14      24       DC4                    T
  21      15      25       NAK                    U
  22      16      26       SYN                    V
  23      17      27       ETB                    W
  24      18      30       CAN (Cancel)           X
  25      19      31       EM                     Y
  26      1a      32       SUB                    Z
  27      1b      33       ESC (Escape)           [
  28      1c      34       FS                     \
  29      1d      35       GS                     ]
  30      1e      36       RS                     ~
  31      1f      37       US                     ?
  32      20      40       Space
  33      21      41       !
  34      22      42       "
  35      23      43       #
  36      24      44       $
  37      25      45       %
  38      26      46       &
  39      27      47       '
  40      28      50       (
  41      29      51       )
  42      2a      52       *
  43      2b      53       +
  44      2c      54       ,
  45      2d      55       -
  46      2e      56       .
  47      2f      57       /
  48      30      60       0
  49      31      61       1
  50      32      62       2
  51      33      63       3
  52      34      64       4
  53      35      65       5
  54      36      66       6
  55      37      67       7
  56      38      70       8
  57      39      71       9
  58      3a      72       :
  59      3b      73       ;
  60      3c      74       <
  61      3d      75       =
  62      3e      76       >
  63      3f      77       ?
  64      40     100       @
  65      41     101       A
  66      42     102       B
  67      43     103       C
  68      44     104       D
  69      45     105       E
  70      46     106       F
  71      47     107       G
  72      48     110       H
  73      49     111       I
  74      4a     112       J
  75      4b     113       K
  76      4c     114       L
  77      4d     115       M
  78      4e     116       N
  79      4f     117       O
  80      50     120       P
  81      51     121       Q
  82      52     122       R
  83      53     123       S
  84      54     124       T
  85      55     125       U
  86      56     126       V
  87      57     127       W
  88      58     130       X
  89      59     131       Y
  90      5a     132       Z
  91      5b     133       [
  92      5c     134       \
  93      5d     135       ]
  94      5e     136       ^
  95      5f     137       _
  96      60     140       `
  97      61     141       a
  98      62     142       b
  99      63     143       c
 100      64     144       d
 101      65     145       e
 102      66     146       f
 103      67     147       g
 104      68     150       h
 105      69     151       i
 106      6a     152       j
 107      6b     153       k
 108      6c     154       l
 109      6d     155       m
 110      6e     156       n
 111      6f     157       o
 112      70     160       p
 113      71     161       q
 114      72     162       r
 115      73     163       s
 116      74     164       t
 117      75     165       u
 118      76     166       v
 119      77     167       w
 120      78     170       x
 121      79     171       y
 122      7a     172       z
 123      7b     173       {
 124      7c     174       |
 125      7d     175       }
 126      7e     176       ~
 127      7f     177       DEL (Delete)
