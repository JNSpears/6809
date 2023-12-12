( vt100 terminal dependent cursor addressing - jns 03/23/84 )
CR ." LOADING VT100" CR
VOCABULARY VT100 IMMEDIATE
VT100 DEFINITIONS HEX

: N  ( n -- ) BASE @ SWAP DECIMAL 0 .R BASE ! ;
: N; ( n -- ) N ASCII ; EMIT ;

: ESC[  CTRL [ EMIT ASCII [ EMIT ;
: HOME  ESC[ ASCII H EMIT ;
                    ( x y -- ; move cursor to col x, row y )
: XY    ESC[ N; N ASCII H EMIT ;

: CLRS  ESC[ ASCII J EMIT ;     ( Clear screen from cursor )
: CLRL  ESC[ ASCII K EMIT ;     ( Clear line from cursor  )
-->
: UP    ESC[ ASCII A EMIT ;     ( Move cursor up )
: DOWN  ESC[ ASCII B EMIT ;     ( Move cursor down )
: RIGHT ESC[ ASCII C EMIT ;     ( Move cursor Right )
: LEFT  ESC[ ASCII D EMIT ;     ( Move cursor Left )

: ERASE-CHAR    ESC[ ASCII X EMIT ; ( Erase character )
: ERASE-SCREEN  ESC[ ." 2J" ;  : ERASE-EOS     ESC[ ." 0J" ;
: ERASE-LINE    ESC[ ." 2K" ;  : ERASE-EOL     ESC[ ." 0K" ;

: NORMAL        ESC[ ." 0m" ;
: BOLD          ESC[ ." 1m" ;
: UNDERSCORE    ESC[ ." 4m" ; 
: BLINK         ESC[ ." 5m" ; 
: NEGATIVE      ESC[ ." 7m" ; 

-->
0 CONSTANT BLACK        1 CONSTANT RED
2 CONSTANT GREEN        3 CONSTANT YELLOW
4 CONSTANT BLUE         5 CONSTANT MAGENTA
6 CONSTANT CYAN         7 CONSTANT WHITE

DECIMAL
: FG ( color -- ) ESC[ 30 + N ASCII m EMIT  ;
: BG ( color -- ) ESC[ 40 + N ASCII m EMIT  ;

: HORZ-X ( X-POS -- ; MOVE CURSOR TO COLUMN X )
  ESC[ N ASCII G EMIT ;

FORTH DEFINITIONS
;S