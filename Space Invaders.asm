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
FLOOR_SCAN     = (KERNEL_SCANLINE-25)
PLAYER_LEN     = 10
ALIEN_LEN      = 10
PLAYER_SCAN    = (FLOOR_SCAN-PLAYER_LEN)
SCORE_SCAN     = 5
MOVE_SPEED     = 1
LEFT_LIMIT_PL  = 33
RIGHT_LIMIT_PL = 122
ENEMY_DIST     = KERNEL_SCANLINE-192
DEF_DIST       = PLAYER_SCAN-28
ALIENS_LIMIT   = PLAYER_SCAN-18*5-15
LEFT_LIMIT_AL  = 22
TAPS           = $B8
SEED           = 13

;===================================================================
;===================================================================
;           VARIABLES RAM ($0080-$00FF)(128B RAM)

    SEG.U   VARIABLES
    ORG     $80

RANDOM_NUMBER   ds  1   ; Random Number :)

FRAME_COUNT     ds  1   ; Frame Count
SCANLINE_COUNT  ds  1   ; Temp for Ajust Number of Scanline After Aliens

PLAYER_POS      ds  1   ; Player X-pos
PLAYER_GRP      ds  2   ; Player PTR Grp

MSL_POS         ds  3   ; Player, Alien1, Alien2 X-pos
MSL_SCAN        ds  3   ; Player, Alien1, Alien2 Y-pos
MSL_ATT         ds  1   ; Missels Status
;      MSL_ATT DECODER
;   Bit            Status
;
;    7          Player Missile
;    6         Alien 1º Missile
;    5         Alien 2º Missile
;
;   0-2      Current Missile Draw   P:2,A1:1,A2:0

ALIENS_POS      ds  1   ; Alien X-pos (First Line and Column)
ALIENS_SCAN     ds  1   ; Alien Y-pos
ALIENS_SPEED    ds  1   ; Frames Need to Move Aliens
ALIENS_MASK     ds  1   ; Select Reverse or Original (Right or Left) Aliens Sprites

SPRITE_HEIGHT   ds  1   ; Count for Draw Aliens

ALIENS_ATT      ds  7   ; Alien Status, 7º Pos is TEMP
COLUMNS_ALIVE   ds  1   ; Alien Columns with at Least One Alive
TEMP_ROTATION   ds  1   ; Used to Rotations and Discover Blank Columns

ALIENS_LINES    ds  12  ; Alien PTR GRP
ALIENS_TEMP     ds  2   ; Temp PTR GRP Aliens

ALIENS_DELAY    ds  2   ; PTR for Time ajust for X-pos Aliens
ALIENS_NUM      ds  1   ; Number of Lines of Aliens Alive
ALIENS_COUNT    ds  1   ; Number of Current Lines of Aliens

RIGHT_LIMIT_AL  ds  1   ; Max Right Position for Aliens Sprite
SCAN_LIMIT_AL   ds  1   ; Max vertical Position for Aliens Sprite
DIRECTION_AL    ds  1   ; Direction of Aliens (0:Left; 1:Right)

DEFENSE_POS     ds  1   ; Base Defense X-Pos
DEFENSE_GRP     ds  27  ; Base Defense Shape

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
    LDA #SEED           ; Default Seed with INTIM returns NULL (zero)
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
    LDX #0
    LDA ALIENS_POS
    JSR AjustDelayAliens
    LDX #1
    LDA ALIENS_POS
    CLC
    ADC #16
    JSR AjustDelayAliens
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
    STA ALIENS_SPEED
    STA ALIENS_MASK
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
    LDA #42
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
;   Set Direction and Position of Aliens Sprites
    JSR CheckBoardLimitsAliens
    JSR MoveSpritesAliens
;   Set Based Aliens GRP
    LDA ALIENS_MASK
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
;   Set First Line of Aliens
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
;   Increment Counter Frame
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
;   Check Right Move Available
    LDA PLAYER_POS
    CMP #(RIGHT_LIMIT_PL-5)
    BCS NoMove
    CLC
    ADC #MOVE_SPEED
    STA PLAYER_POS
    JMP NoMove
LeftMove:
;   Check Left Move Available
    LDA PLAYER_POS
    CMP #(LEFT_LIMIT_PL+1)
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
;   Clear Buffers of Moves
    STA HMCLR
;   Out VBlank (Magic Starts Here !!)
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
;   Set Color, Active 3 Copies Medium and Vertical Delay Enable
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
JmpDelay:       ; Aliens Position  /  Relative Byte Code Jump
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
;   Consume if Line Used Long JmpDelay (Adjust Cycle Synchronization)
    LDA ALIENS_DELAY
    SEC
    SBC #<JmpDelay
    SBC #14
    NOP
    BCC NotUseLineAlign
    .BYTE $EA,$EA,$EA
    CMP $80
NotUseLineAlign:
;   Check End of Aliens Lines
    LDX ALIENS_NUM
    BEQ ExitAliens
    LDX ALIENS_COUNT
    LDA ALIENS_ATT-1,X
    STA ALIENS_ATT+6
;   Set Next Line Sprites (Pointer Arithmetic)
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
VertSpaceAliensLoop:    ; Consume Unused Scanlines Between Aliens Lines
    INY
    DEX
    STA WSYNC
    BPL VertSpaceAliensLoop
;   Prepare for Another Loop
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
;   Jump if Aliens Land (No Print Player)
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
;   Jump if Aliens "Destroy" Base Defense
    CPY #DEF_DIST-3
    BPL DefenseIsGone
;   Prepare GRP0 for 3 Copies Medium for Base Defense Drawn
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
;   Save in stack Y value, use Y for Indirect Address
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
;   Missele 0 Color
    LDA #RIGHT_LIMIT_COLOR
    STA COLUP0
;   Missele 1 Color
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
;   Wait hot Scanlines Over
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
;   Where X Represents Values ​for Player X (X Equal 0 or 1)
;   Where A Contains the Color Clock Value of the Object on the Screen (Column Position of Object)
;
;   Function with Extreme Precision of Time Consumption and Triggering of Object Position Recorders, 
; with the Same Value as Register A.
SetHorizPos:        ; CPU Execution Cost and Accumulated Color Clocks (Minimum and Maximum cases After the Loop)
	STA WSYNC	    ; 0 (0)
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

;FUNCTION AjustDelayAliens(None)
;   Adjusts the Jump Address of the 'ALIENS_DELAY' Variable
; Taking the Position of the Object on the Screen as a Criterion, 
; This Serves to Keep the Swap Time Between GRP0 and GRP1 Viable, Keeping the Positioning Aligned
;
AjustDelayAliens:
;   Default Set of High and Low Addresses
    LDA #<DrawEnemies
    STA ALIENS_DELAY
    LDA #>DrawEnemies
    STA ALIENS_DELAY+1
    LDA ALIENS_POS
    SEC
    SBC #31
    BCC OutAjust
;   Balance as Needed
AjustDelayLoop:
    TAX
    LDA ALIENS_DELAY
    SBC #2
    STA ALIENS_DELAY
    BCS AjustDelayCarry
    DEC ALIENS_DELAY+1
    SEC
AjustDelayCarry:    ; Check Carry From Low to High Addresses
    TXA
    SBC #9
    BCS AjustDelayLoop
OutAjust:
    RTS

;FUNCTION MoveSpritesAliens
;   This Function Will Detect the Passage of Time Through Frame Counting, 
; Making the Aliens Move Horizontally and Treating the Vertical Descent 
; When It Reaches the Side Limits
;
MoveSpritesAliens:
    LDA FRAME_COUNT
    AND ALIENS_SPEED
    CMP ALIENS_MASK
    BEQ SetOut
    STA ALIENS_MASK  
;   Sprite Move
    LDA DIRECTION_AL
    BNE AliensMoveForward
;   AliensMoveBackward
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
;   
ReverseDirection: ; Treats vertical displacement of aliens
    LDA ALIENS_SCAN
    CLC
    ADC #10
    STA ALIENS_SCAN
;   Vertical Limit Check (End of Game, Collision with Player/Ground)
    CMP SCAN_LIMIT_AL
    BCC NoEndGame
;   Collision with player/ground
;   Landing adjustments (no bugs and flick scan on landing)
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
;   Stop moving Sprites (Move Speed ​​Zero)
    LDA #0
    STA ALIENS_SPEED
NoEndGame:
;   Aliens direction swap
    LDA DIRECTION_AL
    EOR #$FF
    STA DIRECTION_AL
    RTS

;FUNCTION CheckBoardLimitsAliens
;   This Function Will Check Empty Side Columns, 
; If You Think It Will Adjust The Aliens' Movement Limits,
; Always Keeping The First Column And Line As Relative To The Movement
;
CheckBoardLimitsAliens:
    LDX #4
    LDA ALIENS_ATT+5
CheckColumnsLoop:           ; Check Empty Columns
    ORA ALIENS_ATT,X
    DEX
    BPL CheckColumnsLoop
;   Checks Change From Previous State (Some Column Was killed in This Time Interval)
    CMP COLUMNS_ALIVE
    BEQ SetVerticalLimit    ; If There are no Changes, Branch !
;   Backup to Memory
    STA COLUMNS_ALIVE
    STA TEMP_ROTATION
;   Standard Limit of Aliens to the Right
    LDA #49
    STA RIGHT_LIMIT_AL
;   Increases Right Limit (without the presence of Columns in Right)
    LDX #5
BoardRightLimitLoop:
    ROR TEMP_ROTATION
    BCS CheckLeftLimit
    ADC #16
    STA RIGHT_LIMIT_AL
    DEX
    BPL BoardRightLimitLoop
;   Prepare to Check Columns to the Left
CheckLeftLimit:
    LDA COLUMNS_ALIVE
    ROL
    ROL 
    ROL
    STA TEMP_ROTATION
;   If First Column Exists, Branch !
    BCS SetVerticalLimit
;   Move Aliens From Each Column Until There is at Least 1 Alien in the First Column (Column Existence Condition)
    LDY #4
BoardLeftLimitLoop:
    LDX #5
ShiftAliens:
    ROL ALIENS_ATT,X
    DEX
    BPL ShiftAliens
;   After Displacement Reposition the Aliens (Same Absolute Position on the Screen)
    LDA ALIENS_POS
    CLC
    ADC #16
    STA ALIENS_POS
;   Continue Until the Column Exists
    ROL TEMP_ROTATION
    BCS SetVerticalLimit
    DEY
    BPL BoardLeftLimitLoop
;   Set the Vertical Limit (Maximum Descent of the First Row, Which is the Highest on the Screen)
SetVerticalLimit:
;   Set Default Base Hit Y-Ground (Relative to first line)
    LDA #ALIENS_LIMIT
    STA SCAN_LIMIT_AL
;   Reset Alien Line Numbers
    LDA #5
    STA ALIENS_NUM
    STA ALIENS_COUNT
;   Loop to Check Empty Lines Below
    LDX #0
CheckUnderLimit:
    LDA ALIENS_ATT,X    ; From the Lowest on the Screen to the Highest
    BNE NoUnderChange
;   If it does Not Exist, it Increases the Maximum Line Descent Limit
    LDA SCAN_LIMIT_AL
    CLC
    ADC #18
    STA SCAN_LIMIT_AL
    DEC ALIENS_NUM      ; Decrement the Count of "Live" Lines
    INX
    CPX #6              ; Repeat for All Lines
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
