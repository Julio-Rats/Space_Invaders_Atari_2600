; Used DASM to compile
;   see https://github.com/Julio-Rats/dasm
;
; Command to compile:
;   dasm SpaceInvaders.asm -oSpaceInvaders.bin -f3
;

    PROCESSOR 6502
    INCLUDE "vcs.h"

;===================================================================
;                     TV SYSTEM DEFINITION
;===================================================================
SYSTEM_TV = "NTSC"  ; (NTSC, PAL)

;===================================================================
;                       NTSC 60 FPS
    IF SYSTEM_TV == "NTSC"

KERNEL_SCANLINE     = 217 ;192+25

VBLANK_TIMER        = 44
OVERSCAN_TIMER      = 6

BACK_COLOR          = $00
FLOOR_COLOR         = $E2
PLAYER_COLOR        = $C4
RIGHT_LIMIT_COLOR   = $C4
LEFT_LIMIT_COLOR    = $F6
ENEMY_COLOR         = $16
DEFENSE_COLOR       = $34

;===================================================================
;                       PAL 50 FPS
    ELSE
        IF SYSTEM_TV == "PAL"

KERNEL_SCANLINE     = 253 ;228+25

VBLANK_TIMER        = 53
OVERSCAN_TIMER      = 13

BACK_COLOR          = $00
FLOOR_COLOR         = $26
PLAYER_COLOR        = $36
RIGHT_LIMIT_COLOR   = $34
LEFT_LIMIT_COLOR    = $46
ENEMY_COLOR         = $2C
DEFENSE_COLOR       = $64

;===================================================================
;                          OTHERS
        ELSE
            ECHO "TV SYSTEM NOT SUPPORTED!"
        ENDIF
    ENDIF
;===================================================================

;===================================================================
;                       Global Constants
;===================================================================
FLOOR_SCAN    = (KERNEL_SCANLINE-25)
PLAYER_LEN    = 10
ALIEN_LEN     = 10
PLAYER_SCAN   = (FLOOR_SCAN-PLAYER_LEN)
SCORE_SCAN    = 5
MOVE_SPEED    = 1
LEFT_LIMIT    = 33
RIGHT_LIMIT   = 122
ENEMY_DIST    = KERNEL_SCANLINE-192
DEF_DIST      = PLAYER_SCAN-28
ALINES_LIMIT  = PLAYER_SCAN-18*5-15
LEFT_LIMIT_AL = 22
TAPS          = $B8
SEED          = 13

;===================================================================
;===================================================================
;           VARIABLES RAM ($0080-$00FF)(128B RAM)

    SEG.U   VARIABLES
    ORG     $80

RANDOM_NUMBER   ds  1   ; Random Number :)

FRAME_COUNT     ds  1   ; Frame Count
INVERS_SPEED    ds  1   ; Mask to frame invert aliens
FRAME_MASK      ds  1   ; Select reverse or original Aliens sprites
SCANLINE_COUNT  ds  1   ; Temp for ajust number of scanline after aliens

PLAYER_POS      ds  1   ; Player X-pos
PLAYER_GRP      ds  2   ; Player PTR Grp

ALIENS_POS      ds  1   ; Alien X-pos (Further to the left)
ALIENS_SCAN     ds  1   ; Alien Y-pos
SPRITE_HEIGHT   ds  1   ; Count for Draw Aliens

ALIENS_ATT      ds  7   ; Alien Status, 7º Pos is TEMP
COLUMNS_ALIVE   ds  1   ; Alien columns with at least one alive
TEMP_ROTATION   ds  1   ; Used to perform rotations and discover blank rows and columns

ALIENS_LINES    ds  12  ; Alien PTR Grp
ALIENS_TEMP     ds  2   ; Temp PTR Grp Aliens

ALIENS_DELAY    ds  2   ; PTR for time ajust for X-pos Aliens
ALIENS_NUM      ds  1   ; Number of lines of aliens alive
ALIENS_COUNT    ds  1   ; Number of current lines of aliens

RIGHT_LIMIT_AL  ds  1   ; Max Position for Aliens Sprite
SCAN_LIMIT_AL   ds  1   ; Aliens hit the Ground
DIRECTION_AL    ds  1   ; Direction of Aliens (0:Left; 1:Right)

DEFENSE_GRP     ds  27  ; Base Defense Shape
DEFENSE_POS     ds  1   ; Base Defense X-Pos

;===================================================================
;===================================================================
;                       CODE SEGMENT

    SEG   CODE
    ORG   $F000     ; Start of "Cart Area" (See Atari Memory Map)

;===================================================================
;                       CPU ENTRYPOINT
;===================================================================
BootGame:
    SEI
    CLD
;   Clear Memory
    LDX #0
    TXS
    TXA
    PHA
ClearMemory:
    PHA
    DEX
    BNE ClearMemory
;   Get Seed for random numbers generation
    LDA INTIM
    BNE NoNULLSeed      ; Check Null Seed (zero)
    LDA #SEED           ; Default seed with INTIM returns NULL (zero)
NoNULLSeed:
    STA RANDOM_NUMBER
;   Player X-pos set
    LDA #75
    STA PLAYER_POS
;   Player GRP set
    LDA #<SpritePlayer
    STA PLAYER_GRP
    LDA #>SpritePlayer
    STA PLAYER_GRP+1
;   Aliens X-Pos set
    LDA #22
    STA ALIENS_POS
;   Aliens Y-Pos set
    LDA #ENEMY_DIST
    STA ALIENS_SCAN
;   Aliens Alive set
    LDA #%00111111
    LDX #5
SetAttEnemiesLoop:
    STA ALIENS_ATT,X
    DEX
    BPL SetAttEnemiesLoop ; Jmp if x >= 0
;   Speed Move Aliens set
    LDA #%00100000
    STA INVERS_SPEED
    STA FRAME_MASK
;   Board Limits of Aliens
    LDA #49
    STA RIGHT_LIMIT_AL
;   Move Direction (Left, Right)
    LDA #0
    STA DIRECTION_AL
;   Base Defense GRP set
    LDX #8
LoadDefenseLoop:
    LDA SpriteDefense,X
    STA DEFENSE_GRP,X
    STA DEFENSE_GRP+9,X
    STA DEFENSE_GRP+18,X
    DEX
    BPL LoadDefenseLoop
;   Set X-Pos Base Defense
    LDA #42 ;42
    STA DEFENSE_POS

;   Start VBlank period
    LDA #%01000010      ; Starting Vblank
    STA VBLANK

;===================================================================
;                         NEW FRAME CYCLE
;===================================================================
StartFrame:
;   Sync with TV
    LDA #%00001110      ; Vertical sync is signaled by VSYNC's bit 1...
WsynWait:
    STA WSYNC           ; (WSYNC write => wait for end of scanline)
    STA VSYNC
    LSR
    BNE WsynWait
;   Set time for vertical blank period
    LDA #VBLANK_TIMER   ; Timing Vblank Scanlines
    STA TIM64T

;===================================================================
;===================================================================
;                       Vblank code area
;===================================================================
;===================================================================
;   Reset Back Color
    LDA #BACK_COLOR
    STA COLUBK
;   Set Direction and position of Aliens Sprites
    JSR CheckBoardLimitsAliens
    JSR SetSpritesAliens       ; Magic happens here !!!!
;   Set Based Aliens Grp
    LDA FRAME_MASK
    BNE InverseSprite
    LDA #>SpriteEnemieLine1
    LDY #<SpriteEnemieLine1
    JMP LoadTempSprite
InverseSprite:
;   Load Pointer of Base Sprites Aliens
    LDA #>SpriteEnemieLine1_I
    LDY #<SpriteEnemieLine1_I
LoadTempSprite:
    STA ALIENS_TEMP+1
    STY ALIENS_TEMP
;   Set First Line of Alines
    LDA ALIENS_ATT+5
    STA ALIENS_ATT+6
    LDX #11
SetAliensL1GrpLoop
    ROR ALIENS_ATT+6
    BCS AlienL1Alive
    LDA #>SpritEmpty
    LDY #<SpritEmpty
    JMP SetAlienL1
AlienL1Alive:
    LDA ALIENS_TEMP+1
    LDY ALIENS_TEMP
SetAlienL1:
    STA ALIENS_LINES,X
    DEX
    STY ALIENS_LINES,X
    DEX
    BPL SetAliensL1GrpLoop
;   Increment counter Frame
    INC FRAME_COUNT

;===================================================================
;                  INPUT CONTROL PROCESSING AREA
;===================================================================
;   Input Codes

    BIT SWCHA
    BPL RightMove
    BVC LeftMove
    JMP NoMove
RightMove:
;   Check Right move available
    LDA PLAYER_POS
    CMP #(RIGHT_LIMIT-5)
    BCS NoMove
    CLC
    ADC #MOVE_SPEED
    STA PLAYER_POS
    JMP NoMove
LeftMove:
;   Check Left move available
    LDA PLAYER_POS
    CMP #(LEFT_LIMIT+1)
    BCC NoMove
    SEC
    SBC #MOVE_SPEED
    STA PLAYER_POS
NoMove:
;   Set X-Pos of Aliens
    LDA ALIENS_POS
    LDX #0
    JSR SetHorizPos
    LDA ALIENS_POS
    CLC
    ADC #16
    INX
    JSR SetHorizPos

;   Wait Rest of Existing Vblank (Async Clock)
WaitVblankEnd:
    LDA INTIM
    BNE WaitVblankEnd

;   Register Y for Count Hot Scanlines
    TAY ; A:=0 -> Y
    STA WSYNC
;   Apply Moves in Buffers
    STA HMOVE
;   Clear Collisions (New Frame)
    STA CXCLR
    STA WSYNC
;   Clear Buffer of Moves
    STA HMCLR
;   Out VBlank (Magic Starts Here)
    STA VBLANK  ; Stop Vblank

;=============================================================================================
;=============================================================================================
;=============================================================================================
;                                      KERNEL
;=============================================================================================
;=============================================================================================
;=============================================================================================
;
;                         PRINT SCREEN MOMENT (HOT SCANLINES)
;
;   Start Visible Scanlines
;   Kernel Code

    LDA #44
    STA COLUPF
    LDA #$FF
    STA PF0
    STA PF1
    STA PF2
    LDA #0
    STA WSYNC
    INY
    STA PF0
    STA PF1
    STA PF2
;   Set Color, active 3 copies medium and Vertical Delay enable
    LDA #ENEMY_COLOR
    STA COLUP0
    STA COLUP1
    LDA #$06
    STA NUSIZ0
    STA NUSIZ1
    LDA #1
    STA VDELP0
    STA VDELP1

WaitEnemies:
    INY
    STA WSYNC
    CPY ALIENS_SCAN
    BCC WaitEnemies
LoopLines:
    LDX #9
RelativeDelayAliensLoop:
    DEX
    BPL RelativeDelayAliensLoop
    NOP
    LDA #ALIEN_LEN-1
    STA SPRITE_HEIGHT
    STY SCANLINE_COUNT
    JMP (ALIENS_DELAY)
JmpDelay:
    CMP $80     ; 121-129   -22
    CMP $80     ; 112-120   -20
    CMP $80     ; 103-111   -18
    CMP $80     ; 94-102    -16
    CMP $80     ; 85-93     -14
    CMP $80     ; 76-84     -12
    CMP $80     ; 67-75     -10
    CMP $80     ; 58-66     -8
    CMP $80     ; 49-57     -6
    CMP $80     ; 40-48     -4
    CMP $80     ; 31-39     -2
DrawEnemies:    ; 22-30      0
    LDY SPRITE_HEIGHT
    LDA (ALIENS_LINES+10),Y
    TAX
    LDA (ALIENS_LINES),Y
    STA GRP0
    LDA (ALIENS_LINES+2),Y
    STA GRP1
    LDA (ALIENS_LINES+4),Y
    STA GRP0
    LDA (ALIENS_LINES+6),Y
    STA GRP1
    LDA (ALIENS_LINES+8),Y
    STA GRP0
    STX GRP1
    STX GRP0
    .BYTE $EA,$EA,$EA,$EA,$EA,$EA ; 6 NOP -> 12 Cycles Wasted -> 36 Color Clock
    DEC SPRITE_HEIGHT
    BPL DrawEnemies
;   Stop Drawn Aliens
    LDA #0
    STA GRP0
    STA GRP1
    STA GRP0
;   Prepare for Missiles
    STA NUSIZ0
    STA NUSIZ1
;   Consume if line used Long JmpDelay (Adjust cycle synchronization)
    LDA ALIENS_DELAY
    SEC
    SBC #<JmpDelay
    SBC #14
    NOP
    BCC NotUseLineAlign
    .BYTE $EA,$EA,$EA
    CMP $80
NotUseLineAlign:
;   Check End of Alines lines
    LDX ALIENS_NUM
    BEQ ExitAliens
    LDX ALIENS_COUNT
    LDA ALIENS_ATT-1,X
    STA ALIENS_ATT+6
;   Set Next Line Sprites (pointer arithmetic)
    LDA ALIENS_TEMP
    CLC
    ADC #10
    STA ALIENS_TEMP
;   Set Aliens Sprite
    LDX #11
SetAliensGrpLoop:
    ROR ALIENS_ATT+6
    BCS AlienAlive      ; 2/3
    LDA #>SpritEmpty    ; 2 (2)
    LDY #<SpritEmpty    ; 2 (4)
    JMP SetAlien        ; 3 (6)
AlienAlive: ; 3
    LDA ALIENS_TEMP+1   ; 3 (3)
    LDY ALIENS_TEMP     ; 3 (6)
SetAlien:   ; 9
    STA ALIENS_LINES,X
    DEX
    STY ALIENS_LINES,X
    DEX
    BPL SetAliensGrpLoop
;   Restore Y-Count Scanline
    LDA #15
    CLC
    ADC SCANLINE_COUNT
    TAY

    LDX #2
VertSpaceAliensLoop:    ; Consume unused Scanlines between Aliens Lines
    INY
    DEX
    STA WSYNC
    BPL VertSpaceAliensLoop
;   Prepare for another loop
    LDX #20             ; Delay in RelativeDelayAliensLoop
    CMP $80
    LDA #6
    STA NUSIZ0
    STA NUSIZ1
    DEC ALIENS_COUNT
    DEC ALIENS_NUM
    JMP RelativeDelayAliensLoop
ExitAliens:


    LDA #13
    CLC
    ADC SCANLINE_COUNT
    TAY
;   Jump if Aliens land (No Print Player)
    CPY #PLAYER_SCAN-3
    BCC PlayerAlive
    LDA #$10
    STA NUSIZ0
    STA NUSIZ1
    LDA ALIENS_POS
    CMP #25
    BCC SyncScanEndGame
    INY
SyncScanEndGame:
    STA WSYNC

    LDX #2
PlayerDeadAjustScan:
    DEX
    BNE PlayerDeadAjustScan
    NOP
    NOP
    JMP PlayerDeadJmp

    
PlayerAlive:
    LDA #0
    STA VDELP0
    STA VDELP1
;   Jump if Aliens "destroy" Base Defense
    CPY #DEF_DIST-3
    BPL DefenseIsGone
;   Prepare GRP0 for 3 copies medium for Base Defense drawn
    LDA #6
    STA NUSIZ0
;   Set Color of Base Defense
    LDA #DEFENSE_COLOR
    STA COLUP0
;   Set Position of Base Defense
    LDA DEFENSE_POS
    LDX #0
    INY
    JSR SetHorizPos
    INY
    STA WSYNC
    STA HMOVE


WaitDefense:
    INY
    STA WSYNC
    CPY #DEF_DIST-1
    BCC WaitDefense

    STY SCANLINE_COUNT
    LDY #13
RelativeDelayDefenseLoop:
    DEY
    BPL RelativeDelayDefenseLoop
    CMP $80
    NOP
    NOP
    LDX #0
DrawDefenseLoop:
    .BYTE $48,$68,$48,$68       ; 2 par of PHA PLA (14 Cycles Wasted)
    LDA DEFENSE_GRP,X
    STA GRP0
    NOP
    NOP
    LDA DEFENSE_GRP+9,X
    STA GRP0
    NOP
    NOP
    LDA DEFENSE_GRP+18,X
    STA GRP0

    LDY #7
DelayDefenseLoop:
    DEY
    BPL DelayDefenseLoop
    CMP $80
    CMP $80

    LDA DEFENSE_GRP,X
    STA GRP0
    NOP
    NOP
    LDA DEFENSE_GRP+9,X
    STA GRP0
    NOP
    NOP
    LDA DEFENSE_GRP+18,X
    STA GRP0
    .BYTE $48,$68,$48,$68       ; 2 par of PHA PLA -> 14 Cycles Wasted -> 42 Color Clock
    NOP
    INC SCANLINE_COUNT
    INC SCANLINE_COUNT
    INX
    CPX #9
    BNE DrawDefenseLoop


    LDA #0
    STA GRP0
    INC SCANLINE_COUNT
    LDY SCANLINE_COUNT

DefenseIsGone:

    LDA PLAYER_POS
    LDX #0
    INY
    JSR SetHorizPos

WaitPlayer:
    INY
    STA WSYNC
    CPY #PLAYER_SCAN-2
    BCC WaitPlayer
    INY
    STA WSYNC
    STA HMOVE
;   Start Draw Player
;   Missele/Player Sets
    LDA #$10
    STA NUSIZ0
    STA NUSIZ1
;   Set Player Color
    LDA #PLAYER_COLOR
    STA COLUP0
;   Save in stack Y value, use Y for indirect address
    TYA
    TAX
    LDY #0
DrawPlayer:
    INX
    STA WSYNC
    LDA (PLAYER_GRP),Y
    STA GRP0
    INY
    CPY #PLAYER_LEN
    BNE DrawPlayer
    TXA
    TAY


PlayerDeadJmp:
;   Missele 0 color
    LDA #RIGHT_LIMIT_COLOR
    STA COLUP0
;   Missele 1 color
    LDA #LEFT_LIMIT_COLOR
    STA COLUP1
;   Delay M0 Limits Position
    STA RESM0
;   Fine Limits Position
    LDA #$C0
    STA HMM0
    LDA #$00
    STA HMM1
;   Delay M1 Limits Position
    LDX #3
DelayM1:
    DEX
    BNE DelayM1
    NOP
    STA RESM1
;   Apply Moves
    INY
    STA WSYNC
    STA HMOVE
;   Ground Color
    LDA #FLOOR_COLOR
    STA COLUBK
;   End Player
    LDA #0
    STA GRP0
    STA GRP1
    INY
    STA WSYNC
;   Enable Missiles
    LDA #2
    STA ENAM0
    STA ENAM1
;   Limits Height
    LDX #5
LimLen:
    INY
    STA WSYNC
    DEX
    BNE LimLen
;   End Limits
    LDA #0
    STA ENAM0
    STA ENAM1
    STA HMCLR
;
;=============================================================================================
;                                     OVERSCAN
;=============================================================================================
;   Wait hot Scanlines over
ScanlineEnd:
;   Increment Y-ScanLine Count
    INY
    STA WSYNC
    CPY #KERNEL_SCANLINE
    BCC ScanlineEnd

;=============================================================================================
;=============================================================================================
;=============================================================================================
;                                  END OF KERNEL
;=============================================================================================
;=============================================================================================
;=============================================================================================
Overscan:
    LDA #%01000010          ; "Turn Off Cathodic Ray"
    STA VBLANK

    LDA #OVERSCAN_TIMER     ; Timing OverScanlines
    STA TIM64T

;===================================================================
;===================================================================
;                      Overscan Code Area
;===================================================================
;===================================================================

;   Overscan Code
    JSR RandNumber

;===================================================================
;                   COLLISION PROCESSING AREA
;===================================================================

;   Collision Code

;=============================================================================================
;                                 END OVERSCAN
;=============================================================================================
;   Wait Rest of Existing OverScan (Async Clock)
WaitOverscanEnd:            ; Timing OverScanlines
    LDA INTIM
    BNE WaitOverscanEnd
    JMP StartFrame          ; Back to Start

;=============================================================================================
;=============================================================================================
;             				  FUNCTION DECLARATION
;=============================================================================================
;=============================================================================================
;   Functions Codes

; FUNCTION RandNumber (None):
;   Get next random number
;   Based in Linear-feedback Shift Register
RandNumber:
    LDA RANDOM_NUMBER
    LSR
    BCC NoEOR
    EOR #TAPS
NoEOR:
    STA RANDOM_NUMBER
    RTS

;FUNCTION SetHorizPos (A,X)
;
;
SetHorizPos
	STA WSYNC	    ; (0)
    BIT 0		    ; 3 (6)
	SEC		        ; 2 (15)
DivisionLoop: ; Each loop consumes 5 cycles, the last loop consumes 4. The minimum consumption is 4 cycles and the maximum is 59 cycles
	SBC #15		    ; 2
    BCS DivisionLoop; 2/3
	EOR #7		    ; 2 (27/177)
	ASL             ; 2 (33/183)
	ASL             ; 2 (39/189)
	ASL             ; 2 (45/195)
	ASL             ; 2 (51/211)
	STA RESP0,X	    ; 4 (57/207) --> (69/219) --> +5 --> (74/224) --> -68 --> (6/156)
	STA HMP0,X
	RTS

;FUNCTION AjustDelayAliens()
;
;
AjustDelayAliens:
    LDA #<DrawEnemies
    STA ALIENS_DELAY
    LDA #>DrawEnemies
    STA ALIENS_DELAY+1
    LDA ALIENS_POS
    SEC
    SBC #31
    BCC OutAjust
AjustDelayLoop:
    TAX
    LDA ALIENS_DELAY
    SBC #2
    STA ALIENS_DELAY
    BCS AjustDelayCarry
    DEC ALIENS_DELAY+1
    SEC
AjustDelayCarry:
    TXA
    SBC #9
    BCS AjustDelayLoop
OutAjust:
    RTS

;FUNCTION SetSpritesAliens
;
;
SetSpritesAliens:
    LDA FRAME_COUNT
    AND INVERS_SPEED
    CMP FRAME_MASK
    BEQ SetOut
    STA FRAME_MASK  ; Inverted Sprites
    LDA DIRECTION_AL
    BNE AliensMoveForward
    LDA ALIENS_POS
    CLC
    ADC #1
    CMP RIGHT_LIMIT_AL
    BPL ReverseDirection
    STA ALIENS_POS
    JMP PosMove
AliensMoveForward:
    LDA ALIENS_POS
    SEC
    SBC #1
    CMP #LEFT_LIMIT_AL
    BCC ReverseDirection
    STA ALIENS_POS
PosMove:
    ;   Set Delay Time for draw aliens
    JMP AjustDelayAliens    ; Caution !! Use Trick Change JSR to JMP
    ;   Any code entered here or below will not be executed!
SetOut:
    ;   Caution if need code here !!
    RTS

ReverseDirection:
    LDA ALIENS_SCAN
    CLC
    ADC #10
    STA ALIENS_SCAN
    CMP SCAN_LIMIT_AL
    BCC NoEndGame

    LDA #FLOOR_SCAN
    LDX ALIENS_NUM
    BEQ AjustEndScan
AjustEndScanLoop:
    SEC
    SBC #18
    DEX
    BNE AjustEndScanLoop

AjustEndScan:
    SEC
    SBC #14
    STA ALIENS_SCAN

    LDA #0
    STA INVERS_SPEED
NoEndGame:
    LDA DIRECTION_AL
    EOR #$FF
    STA DIRECTION_AL
    JMP SetSpritesAliens

;FUNCTION CheckBoardLimitsAliens
;
;
CheckBoardLimitsAliens:
    LDX #4
    LDA ALIENS_ATT+5
CheckLimitsLoop:
    ORA ALIENS_ATT,X
    DEX
    BPL CheckLimitsLoop
    CMP COLUMNS_ALIVE
    BEQ NoRLimitChange
    STA COLUMNS_ALIVE
    STA TEMP_ROTATION

    LDA #49
    STA RIGHT_LIMIT_AL

    LDX #5
BoardRightLimitLoop:
    ROR TEMP_ROTATION
    BCS CheckLeftLimit
    ADC #16
    STA RIGHT_LIMIT_AL
    DEX
    BPL BoardRightLimitLoop

CheckLeftLimit:
    LDA COLUMNS_ALIVE
    STA TEMP_ROTATION
    ROL TEMP_ROTATION
    ROL TEMP_ROTATION

    ROL TEMP_ROTATION
    BCS NoRLimitChange

    LDY #4
BoardLeftLimitLoop:
    LDX #5
ShiftAlines:
    ROL ALIENS_ATT,X
    DEX
    BPL ShiftAlines

    LDA ALIENS_POS
    CLC
    ADC #16
    STA ALIENS_POS

    ROL TEMP_ROTATION
    BCS ApplyMove

    DEY
    BPL BoardLeftLimitLoop

ApplyMove:
    LDX #0
    LDA ALIENS_POS
    JSR AjustDelayAliens
    LDA ALIENS_POS
    CLC
    ADC #16
    LDX #1
    JSR AjustDelayAliens

NoRLimitChange:
;   Set Base Hit Y-Ground (Relative to first line)
    LDA #ALINES_LIMIT
    STA SCAN_LIMIT_AL
    ;   Reset Alien Line Numbers
    LDA #5
    STA ALIENS_NUM
    STA ALIENS_COUNT
    LDX #0
CheckUnderLimit:
    LDA ALIENS_ATT,X
    BNE NoUnderChange
    LDA SCAN_LIMIT_AL
    CLC
    ADC #18
    STA SCAN_LIMIT_AL
    DEC ALIENS_NUM
    INX
    CPX #6
    BNE CheckUnderLimit
NoUnderChange:
    RTS

;=============================================================================================
;             				  DATA DECLARATION
;=============================================================================================
;   Numbers Sprite for Score
    ORG $FE00
Number0:


;   Player, Aliens and Defense Sprites
    ORG $FF00
SpritEmpty:
    .BYTE #0,#0,#0,#0,#0,#0,#0,#0,#0,#0

SpriteDefense:
    .BYTE #%00111100
    .BYTE #%01111110
    .BYTE #%01111110
    .BYTE #%01111110
    .BYTE #%01111110
    .BYTE #%11111111
    .BYTE #%11111111
    .BYTE #%11111111
    .BYTE #%11000011

SpritePlayer:
    .BYTE #%00010000
    .BYTE #%00111000
    .BYTE #%00111000
    .BYTE #%01111100
    .BYTE #%00111000
    .BYTE #%00111000
    .BYTE #%11111110
    .BYTE #%01111100
    .BYTE #%11111110
    .BYTE #%11111110

SpriteEnemieLine1:
    .BYTE #%11100111
    .BYTE #%01000010
    .BYTE #%01111110
    .BYTE #%01111110
    .BYTE #%01101011
    .BYTE #%00111110
    .BYTE #%10011000
    .BYTE #%10100100
    .BYTE #%01000010
    .BYTE #%00000001

SpriteEnemieLine2:
    .BYTE #%10100101
    .BYTE #%10100101
    .BYTE #%10100101
    .BYTE #%10011001
    .BYTE #%10011001
    .BYTE #%10100101
    .BYTE #%11111111
    .BYTE #%01011010
    .BYTE #%01111110
    .BYTE #%00111100

SpriteEnemieLine3:
    .BYTE #%11000011
    .BYTE #%00100100
    .BYTE #%00011000
    .BYTE #%00011000
    .BYTE #%00011000
    .BYTE #%01011010
    .BYTE #%10111101
    .BYTE #%10100101
    .BYTE #%10000001
    .BYTE #%10000001

SpriteEnemieLine4:
    .BYTE #%10000001
    .BYTE #%01000010
    .BYTE #%01000010
    .BYTE #%00100100
    .BYTE #%00100100
    .BYTE #%00111100
    .BYTE #%01111110
    .BYTE #%11101011
    .BYTE #%01111110
    .BYTE #%00111100

SpriteEnemieLine5:
    .BYTE #%01110111
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01011100
    .BYTE #%11111111
    .BYTE #%11110011
    .BYTE #%11110011
    .BYTE #%01111110
    .BYTE #%00111100

SpriteEnemieLine6:
    .BYTE #%10010000
    .BYTE #%10001000
    .BYTE #%10001000
    .BYTE #%01000100
    .BYTE #%01000010
    .BYTE #%01000010
    .BYTE #%11111111
    .BYTE #%11011011
    .BYTE #%01011010
    .BYTE #%00011000

SpriteEnemieLine1_I:
    .BYTE #%11000110
    .BYTE #%01000010
    .BYTE #%01111110
    .BYTE #%01111110
    .BYTE #%01010110
    .BYTE #%01111100
    .BYTE #%00011001
    .BYTE #%00100101
    .BYTE #%01000010
    .BYTE #%10000000

SpriteEnemieLine2_I:
    .BYTE #%01000010
    .BYTE #%01011010
    .BYTE #%01011010
    .BYTE #%01011010
    .BYTE #%10011001
    .BYTE #%10100101
    .BYTE #%11111111
    .BYTE #%01011010
    .BYTE #%01111110
    .BYTE #%00111100

SpriteEnemieLine3_I:
    .BYTE #%00100100
    .BYTE #%01000010
    .BYTE #%00100100
    .BYTE #%10100101
    .BYTE #%10011001
    .BYTE #%10011001
    .BYTE #%10111101
    .BYTE #%01011010
    .BYTE #%00011000
    .BYTE #%00000000

SpriteEnemieLine4_I:
    .BYTE #%00000000
    .BYTE #%00100100
    .BYTE #%01011010
    .BYTE #%01000010
    .BYTE #%00100100
    .BYTE #%00111100
    .BYTE #%01111110
    .BYTE #%11010111
    .BYTE #%01111110
    .BYTE #%00111100

SpriteEnemieLine5_I:
    .BYTE #%11101110
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00111010
    .BYTE #%11111111
    .BYTE #%11001111
    .BYTE #%11001111
    .BYTE #%01111110
    .BYTE #%00111100

SpriteEnemieLine6_I:
    .BYTE #%00001001
    .BYTE #%00010001
    .BYTE #%00010001
    .BYTE #%00100010
    .BYTE #%01000010
    .BYTE #%01000010
    .BYTE #%11111111
    .BYTE #%11011011
    .BYTE #%01111110
    .BYTE #%00011000

;   Entrypoint Declaration
    ORG $FFFA

    .WORD BootGame ; NMI
    .WORD BootGame ; EntryPoint
    .WORD 0        ; IRQ/BRK
END
