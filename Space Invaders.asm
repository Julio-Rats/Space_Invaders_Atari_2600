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
SYSTEM_TV = "NTSC"  ; (ONLY NTSC FOR NOW)

;===================================================================
;                         NTSC 60 FPS
    IF SYSTEM_TV == "NTSC"

;   Number of Hot Scanlines
KERNEL_SCANLINE     = 192
;   Timers
VBLANK_TIMER        = 51
FRAME_TIMER         = 249
OVERSCAN_TIMER      = 54
;   Color Hex NTSC
BACK_COLOR          = $00
PF_COLOR            = $06
FLOOR_COLOR         = $E2
PLAYER_COLOR        = $C4
LEFT_SCORE_COLOR    = $C4
RIGHT_SCORE_COLOR   = $F6
LEFT_LIMIT_COLOR    = $F6
RIGHT_LIMIT_COLOR   = $C4
ENEMY_COLOR         = $16
DEFENSE_COLOR       = $34

;===================================================================
;                      OTHERS
    ELSE
        ECHO "TV SYSTEM NOT SUPPORTED!"
    ENDIF
;===================================================================

;===================================================================
;                       Global Constants
;===================================================================
GROUND_SCAN     = KERNEL_SCANLINE-8

;   Size and Lengths
PLAYER_LEN      = 10
ALIEN_LEN       = 10
DEFENSE_LEN     = 9
ALIENS_LINES    = 6
ALIENS_INTER    = 8

;   Start Positions
PLAYER_START    = 75
ALIENS_START    = 22
DEFENSE_START   = 42

;   Scanline Position
PLAYER_SCAN     = [GROUND_SCAN-PLAYER_LEN-1]
ENEMY_DIST      = [PLAYER_SCAN-152]
DEF_SCAN        = [PLAYER_SCAN-27]

;   Speeds
PLAYER_SPEED    = 1
ALIENS_X_SPEED  = 1
ALIENS_Y_SPEED  = ALIEN_LEN
ALS_FRAME_MOVE  = %00100000
MSL_SPEED       = 2

;   Limits
PLAYER_L_LIMIT  = 33
PLAYER_R_LIMIT  = 123
ALIENS_L_LIMIT  = 22
ALIENS_DR_LIMIT = 49
ALIENS_DY_LIMIT = [PLAYER_SCAN-(ALIEN_LEN+ALIENS_INTER)*ALIENS_LINES+4]

;   Random Number Utils
TAPS            = $B8
SEED            = 13

;===================================================================
;===================================================================
;           VARIABLES RAM ($0080-$00FF)(128B RAM)

    SEG.U   VARIABLES
    ORG     $0080

RANDOM_NUMBER   ds  1   ; Random Number :)

FRAME_COUNT     ds  1   ; Frame Count
SCANLINE_COUNT  ds  1   ; Temp for Ajust Number of Scanline Passed

PLAYER_POS      ds  1   ; Player X-pos

SCORE_VALUE     ds  4   ; Left and Rigth Score Value (1 Byte for 2 Digits, 2 Bytes for each side)
SCORE_INDEX     ds  8   ; Index to Indirect PTR, Relative to Number0 + Score_value*5 (one for each digit)

MSL_POS         ds  3   ; Player, Alien1, Alien2 X-pos
MSL_SCAN        ds  3   ; Player, Alien1, Alien2 Y-pos
MSL_STAT        ds  1   ; Missile Status
;      MSL_STAT DECODER
;   Bit            Status
;
;    7          Player Missile
;    6         Alien 1º Missile
;    5         Alien 2º Missile
;
;   0-1      Current Missile Draw   bit 0: Player MSL -- bit 1: Aliens MSL
MSL_CURRSCAN    ds  1   ; Missile Current Scanline Delay
MSL_NEXTHMV     ds  1   ; Missile Current Fine Tuning (HMOVE) - Only used in Aliens Missile Frame
MSL_NEXTPOS     ds  1   ; Relative Distance to Next Missile (below) - Only used in Aliens Missile Frame

ALIENS_POS      ds  1   ; Alien X-pos (First Line and Collumn)
ALIENS_SCAN     ds  1   ; Alien Y-pos
ALIENS_SPEED    ds  1   ; Frames Need to Move Aliens
ALIENS_MASK     ds  1   ; Direction of Aliens (b7=0: Right else left); Select Aliens Sprites (b6=0: Normal else Reverse)

ALIENS_STAT     ds  6   ; Alien Status (bit 0 to 5, Alive Alien := 1)
COLLUMNS_ALIVE  ds  1   ; Collumns of Living Aliens in the Previous Frame
ALIENS_NUM      ds  1   ; Used to count how many lines are left to draw
ALIENS_COUNT    ds  1   ; Number of Current Lines of Aliens

ALIENS_CURR     ds  2   ; Get Current PTR GRP Alien
ALIENS_BLAST    ds  1   ; 3 MSB indicates which line is in burst, 3 LSB means which Alien

ALIENS_DELAY    ds  2   ; PTR for Time ajust for X-pos Aliens

ALIENS_R_LIMIT  ds  1   ; Max Right Position for Aliens Sprite
ALIENS_Y_LIMIT  ds  1   ; Max vertical Position for Aliens Sprite

DEFENSE_POS     ds  1   ; Base Defense X-Pos
DEFENSE_SHAPE   ds  27  ; Base Defense Shape

POINTER_GRP     ds 12   ; Dedicated memory for indirect pointers for building graphs (shapes)
;   --- No longer used ---
; PLAYER_GRP      ds  2   ; Player PTR GRP
; SCORE_GRP       ds  2   ; PTR to $FF04 --> decremented for each line of the score (counter and pointer :p)
; ALIENS_GRP      ds  12  ; Aliens PTR GRP
;   --- End ---
TEMP_SCORE_ATT  ds  1   ; Used to Rotations ATT Aliens and Socre, Generic Temporary

;=============================================================================================
;=============================================================================================
;                           CODE SEGMENT

    SEG     CODE
    ORG     $F000     ; Start of "Cart Area" (See Atari Memory Map)

;=============================================================================================
;                           CPU ENTRYPOINT
;=============================================================================================
BootGame:
    SEI
    CLD
;   Clear Memory
    LDX #0
    TXA
    TXS
ClearMemory:
    DEX
    PHA
    BNE ClearMemory
    PHA

;   Get Seed for random numbers generation
    LDA INTIM
    BNE NoNULLSeed      ; Check Null Seed (zero)
    LDA #SEED           ; Default Seed with INTIM returns NULL (zero)
NoNULLSeed:
    STA RANDOM_NUMBER

    JSR NewRound

;   Start VBlank period
    LDA #%10000010      ; Starting VBlank
    STA VBLANK

;=============================================================================================
;                              NEW FRAME CYCLE
;=============================================================================================
StartFrame:
;   Sync with TV
    LDA #%00001110      ; Vertical sync is signaled by VSYNC's bit 1...

VsyncWait:
    STA WSYNC           ; (WSYNC write => wait for end of scanline)
    STA VSYNC
    LSR
    BNE VsyncWait

;   Set time for vertical blank period
    LDA #VBLANK_TIMER   ; Timing VBlank Scanlines
    STA TIM64T

;   Decrement Counter Frame
    DEC FRAME_COUNT

;=============================================================================================
;=============================================================================================
;                                VBLANK CODE AREA
;=============================================================================================
;=============================================================================================
;   Reset Back Color
    LDA #BACK_COLOR
    STA COLUBK
    LDA #PF_COLOR
    STA COLUPF

;   Prepare Color for Score
    LDA #LEFT_SCORE_COLOR
    STA COLUP0
    LDA #RIGHT_SCORE_COLOR
    STA COLUP1
    LDA #$02
    STA CTRLPF

;   Vertical Delay for Aliens
    LDA #1
    STA VDELP0
    STA VDELP1

;   Tree Copies - Medium Dist (Aliens Used)
    LDA #$06
    STA NUSIZ0
    STA NUSIZ1

;   Preparing pointer and counter for Score
    LDA #$04
    STA POINTER_GRP
;   Set high address Score GRP PTR
    LDA #>Number0
    STA POINTER_GRP+1

;   Calculates relative indexes for Scores
    JSR SetIndexScore

;   Move Aliens
    JSR CheckLogicAliens
    JSR MoveSpritesAliens

;   Set X-Pos of Aliens
    LDA ALIENS_POS
    LDX #0
    JSR SetHorizPos
    LDA ALIENS_POS
    CLC
    ADC #16
    INX
    JSR SetHorizPos

;   Possible Reset MSL
    LDA #$FF
    STA MSL_CURRSCAN

;   Toggle MSL Frame
    LDA MSL_STAT
    EOR #%00000001
    STA MSL_STAT

;   Set Current Missile
    LDA MSL_STAT
    AND #%11100000
    BEQ NoCurrMSL
    BIT MSL_STAT
    BPL NoPlayerMSL
    LDA MSL_STAT
    AND #%00000001
    BEQ NoPlayerMSL
;   Player MSL
    LDA MSL_SCAN
    STA MSL_CURRSCAN
    LDA #0
    STA MSL_NEXTPOS
    STA MSL_NEXTHMV
NoPlayerMSL:
    LDA MSL_STAT
    AND #%00000001
    BNE NoCurrMSL
;   JMP NoCurrMSL
NoCurrMSL:


;   Move Acty Misseles
    BIT MSL_STAT
    BPL AliensMissile1
    LDA MSL_SCAN
;   Check MSL Top Collision
    CMP #[MSL_SPEED+1]
    BCS PMSL
;   Stop Draw
    LDA MSL_STAT
    AND #%01111111
    STA MSL_STAT
    JMP AliensMissile1
PMSL:
;   Move MSL
    LDX #0
    LDA #[-MSL_SPEED]
    JSR MoveMSL
AliensMissile1:
    BIT MSL_STAT
    BVC AliensMissile2
;   Check MSL Ground Collision
    LDA MSL_SCAN+1
    CMP #[PLAYER_SCAN+PLAYER_LEN+1]
    BCC A1MSL
;   Stop Draw
    LDA MSL_STAT
    AND #%10111111
    STA MSL_STAT
    JMP AliensMissile2
A1MSL:
;   Move MSL
    LDX #1
    LDA #MSL_SPEED
    JSR MoveMSL
AliensMissile2:
    LDA MSL_STAT
    AND #%00100000
    BEQ NoMoveMSL
;   Check MSL Ground Collision
    LDA MSL_SCAN+2
    CMP #[PLAYER_SCAN+PLAYER_LEN+1]
    BCC A2MSL
;   Stop Draw
    LDA MSL_STAT
    AND #%11011111
    STA MSL_STAT
    JMP NoMoveMSL
A2MSL:
;   Move MSL
    LDX #2
    LDA #MSL_SPEED
    JSR MoveMSL
NoMoveMSL:

;   No Alien in Blast TEMPORARY
    LDA #$F0
    STA ALIENS_BLAST


;=============================================================================================
;                              INPUT CONTROL PROCESSING AREA
;=============================================================================================
;   Input Codes

;   Check End Game
    LDA ALIENS_SPEED
    BEQ NoMove
;   Check Shot in Progress
    LDA MSL_STAT
    BMI GetMove
;   Buttom Press
    BIT INPT4
    BMI GetMove
    LDA PLAYER_POS
    CLC
    ADC #$04
    STA MSL_POS
    LDX #$04
    JSR SetHorizPos
    LDA #[PLAYER_SCAN-20]
    STA MSL_SCAN
    LDA #%10000000
    ORA MSL_STAT
    STA MSL_STAT
GetMove:
;   Get Directional Controls
    BIT SWCHA
    BPL RightMove
    BVC LeftMove
    JMP NoMove
RightMove:
;   Check Right Move Available
    LDA PLAYER_POS
    CMP #[PLAYER_R_LIMIT-5]
    BCS NoMove
    CLC
    ADC #PLAYER_SPEED
    STA PLAYER_POS
    JMP NoMove
LeftMove:
;   Check Left Move Available
    LDA PLAYER_POS
    CMP #[PLAYER_L_LIMIT+1]
    BCC NoMove
    SEC
    SBC #PLAYER_SPEED
    STA PLAYER_POS
NoMove:
;    Hard Reset
    LDA SWCHB
    AND #1
    BNE NoPressReset
    JSR ResetGame
NoPressReset:

;=============================================================================================
;                              WAIT FOR VBLANK FINISH
;=============================================================================================
;   Wait Rest of Existing VBlank (Async Clock)
WaitVblankEnd:
    LDA INTIM
    BNE WaitVblankEnd

;   Register Y for Count Hot Scanlines
    TAY ; A:=0 -> Y
    STA SCANLINE_COUNT
    STA WSYNC

;   Apply Moves in Buffers
    STA HMOVE

;   Clear Collisions (New Frame)
    STA CXCLR

;   Start Timer Counter
    LDA #FRAME_TIMER
    STA TIM64T

;   VBlank and Disable Latches Inputs
    LDA #%10000000
;   Out VBlank (Magic Starts Here !!)
    STA VBLANK  ; Stop VBlank

;=============================================================================================
;=============================================================================================
;=============================================================================================
;                                        KERNEL
;=============================================================================================
;=============================================================================================
;=============================================================================================
;
;                         PRINT SCREEN MOMENT (HOT SCANLINES)
;
;   Start Visible Scanlines
;   Kernel Code

;   --- Wait and Drawn Score Points ---
    LDX #8
LoopSyncScore:
    DEX
    BNE LoopSyncScore
    NOP
    NOP
LoopDrawScore:
;   Draw Score
;  PF0 | PF1 | PF2      PF0 | PF1 | PF2
;   X  | _ _ |  Y        W  | _ _ |  Z
;-------------------------------
    LDY SCORE_INDEX+4
    LDA (POINTER_GRP),Y     ; X
    STA PF0
    LDA #0
    STA PF1

    LDY SCORE_INDEX+1
    LDA (POINTER_GRP),Y     ; Y
    LSR
    LSR
    LSR
    LSR
    STA PF2

; --> Half Screen Input Data

    LDY SCORE_INDEX+6
    LDA (POINTER_GRP),Y     ; W
    STA PF0

    LDY SCORE_INDEX+3
    LDA (POINTER_GRP),Y     ; Z
    LSR
    LSR
    LSR
    LSR
    STA PF2

;   Draw Score
;  PF0 | PF1 | PF2      PF0 | PF1 | PF2
;      | X Y |              | W Z |
;-------------------------------
    LDY SCORE_INDEX
    LDA (POINTER_GRP),Y     ; Y
    AND #$0F
    STA TEMP_SCORE_ATT
    LDY SCORE_INDEX+5
    LDA (POINTER_GRP),Y     ; X
    ASL
    ASL
    ASL
    ASL
    ORA TEMP_SCORE_ATT
    STA PF1
    LDA #0
    STA PF0
    STA PF2

; --> Half Screen Input Data

    LDY SCORE_INDEX+2
    LDA (POINTER_GRP),Y     ; Z
    AND #$0F
    STA TEMP_SCORE_ATT
    LDY SCORE_INDEX+7
    LDA (POINTER_GRP),Y     ; W
    ASL
    ASL
    ASL
    ASL
;   Forced absolute mode to spend one more cycle, ensuring loop synchronization with the scanline
    ORA.W TEMP_SCORE_ATT
    STA PF1

    DEC POINTER_GRP
;   If the BPL changes pages, the forced absolute mode in ORA must be removed.
    BPL LoopDrawScore
;-------------------------------

    LDA #0
    STA PF1
;   Clear Buffers of Moves, made in VBlank
    STA HMCLR
;   Set Color to Aliens
    STA CTRLPF          ; A == 0
    LDA #ENEMY_COLOR
    STA COLUP0
    STA COLUP1
    DEC MSL_CURRSCAN
    DEC MSL_CURRSCAN

;   Adjusts pointers used by score
;   Set Based Aliens GRP
    BIT ALIENS_MASK
    BVS InverseSprite
    LDA #>SpriteEnemieLine1
    LDY #<SpriteEnemieLine1
    JMP LoadTempSprite
InverseSprite:
    LDA #>SpriteEnemieLine1_I
    LDY #<SpriteEnemieLine1_I
LoadTempSprite:
    STA ALIENS_CURR+1
    STY ALIENS_CURR

;   Set First Line of Aliens
    LDA ALIENS_STAT+5
    STA TEMP_SCORE_ATT
    LDX #11
LoopSetAliensL1Grp:
    ROR TEMP_SCORE_ATT
    BCS AlienL1Alive
    LDA #>SpritEmpty
    LDY #<SpritEmpty
    JMP SetAlienL1
AlienL1Alive:
    LDA ALIENS_CURR+1
    LDY ALIENS_CURR
SetAlienL1:
    STA POINTER_GRP,X
    DEX
    STY POINTER_GRP,X
    DEX
    BPL LoopSetAliensL1Grp

;   Scanlines used in Score
    LDY #14

;   --- Wait for Draw Aliens ---
WaitEnemies:
    INY
    JSR TryDrawMSL
    STA WSYNC
    CPY ALIENS_SCAN
    BCC WaitEnemies

;------------------- Main loop for drawing Aliens -------------------
;   SIZE increment for the Loop below not to present borrow page in Branch (temporary)
    ROL $80

    LDX #10
LoopRelativeDelayAliens:
    DEX
    BNE LoopRelativeDelayAliens
;   Delay 5 Cycles
    ; dec $2D
    STY SCANLINE_COUNT
    LDY #[ALIEN_LEN-1]
    JMP (ALIENS_DELAY)      ; Indirect Jump
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
    CMP $80
;   Odd Scanline
    LDA (POINTER_GRP+10),Y      ; Alien 6
    TAX
    LDA (POINTER_GRP),Y         ; Alien 1
    STA GRP0                    ;   GRP0A == Alien 1
    LDA (POINTER_GRP+2),Y       ; Alien 2
    STA GRP1                    ;   GRP0A/GRP0B == Alien 1  ;  GRP1A == Alien 2
    LDA (POINTER_GRP+4),Y       ; Alien 3
    STA GRP0                    ;   GRP0A == Alien 3/GRP0B == Alien 1  ;  GRP1A/GPR1B == Alien 2
    LDA (POINTER_GRP+6),Y       ; Alien 4
    STA GRP1                    ;   GRP0A/GRP0B == Alien 3  ;  GRP1A == Alien 4/GPR1B == Alien 2
    LDA (POINTER_GRP+8),Y       ; Alien 5
    STA GRP0                    ;   GRP0A == Alien 5/GRP0B == Alien 3  ;  GRP1A/GPR1B == Alien 4
    STX GRP1                    ;   GRP0A/GRP0B == Alien 5  ;  GRP1A == Alien 6/GPR1B == Alien 4
    STX GRP0                    ;   GRP0A == Alien 6/GRP0B == Alien 5  ;  GRP1A/GPR1B == Alien 6

;   Delay 11 Cycles
    PHA
    PLA
    NOP
    NOP
    DEC MSL_CURRSCAN
    DEC MSL_CURRSCAN

;   Even Scanline
    DEY
    LDA (POINTER_GRP+10),Y      ; Alien 6
    TAX
    LDA (POINTER_GRP),Y         ; Alien 1
    STA GRP0                    ;   GRP0A == Alien 1
    LDA (POINTER_GRP+2),Y       ; Alien 2
    STA GRP1                    ;   GRP0A/GRP0B == Alien 1  ;  GRP1A == Alien 2
    LDA (POINTER_GRP+4),Y       ; Alien 3
    STA GRP0                    ;   GRP0A == Alien 3/GRP0B == Alien 1  ;  GRP1A/GPR1B == Alien 2
    LDA (POINTER_GRP+6),Y       ; Alien 4
    STA GRP1                    ;   GRP0A/GRP0B == Alien 3  ;  GRP1A == Alien 4/GPR1B == Alien 2
    LDA (POINTER_GRP+8),Y       ; Alien 5
    STA GRP0                    ;   GRP0A == Alien 5/GRP0B == Alien 3  ;  GRP1A/GPR1B == Alien 4
    STX GRP1                    ;   GRP0A/GRP0B == Alien 5  ;  GRP1A == Alien 6/GPR1B == Alien 4
    STX GRP0                    ;   GRP0A == Alien 6/GRP0B == Alien 5  ;  GRP1A/GPR1B == Alien 6

;   Check the Missile draw
    LDA MSL_CURRSCAN
    CMP #8
    BCC TryDrawMSL1
    LDA #0
    BCS TryDrawMSL2
TryDrawMSL1:
    NOP
    LDA #2
TryDrawMSL2:
    STA ENABL

    DEY
    BPL DrawEnemies

;   Stop Drawn Aliens
    LDA #0
    STA GRP0
    STA GRP1
    STA GRP0

;   Check End of Aliens Lines
    DEC ALIENS_NUM
    BEQ ExitAliens
    DEC ALIENS_COUNT
    LDX ALIENS_COUNT
    LDA ALIENS_STAT-1,X
    STA TEMP_SCORE_ATT
;   Set Next Line Sprites (Pointer Arithmetic)
    LDA ALIENS_CURR
    CLC
    ADC #ALIEN_LEN
    STA ALIENS_CURR

    DEC MSL_CURRSCAN

;   Set Aliens Sprite
    LDX #11
LoopSetAliensGrp:
    ROR TEMP_SCORE_ATT
    BCS AlienAlive      ; 2/3
    LDA #>SpritEmpty    ; 2 (2)
    LDY #<SpritEmpty    ; 2 (4)
    JMP SetAlien        ; 3 (6)
AlienAlive: ; 3
    LDA ALIENS_CURR+1   ; 3 (3)
    LDY ALIENS_CURR     ; 3 (6)
SetAlien:   ; 9
    STA POINTER_GRP,X
    DEX
    STY POINTER_GRP,X
    DEX
    BPL LoopSetAliensGrp
;   Restore Y-Count Scanline
    LDA #[ALIEN_LEN+4]
    CLC
    ADC SCANLINE_COUNT
    TAY

    DEC MSL_CURRSCAN
    DEC MSL_CURRSCAN

    LDX #[ALIENS_INTER-4]
;   Adjustment in Scanline Count (too much delay consumes a Scanline naturally)
    LDA ALIENS_POS
    CMP #$55
    BCC LoopInterAliens
    DEX                         ; Spend one scan line less
    INY                         ; Increments the Scanline counter
LoopInterAliens:                ; Consume Unused Scanlines Between Aliens Lines
    JSR TryDrawMSL
    INY
    DEX
    STA WSYNC
    BNE LoopInterAliens

    DEC MSL_CURRSCAN

;   Prepare for Another Loop
    LDX #10                      ; Delay in LoopRelativeDelayAliens
    JMP LoopRelativeDelayAliens

;----------------------------- Exit Aliens -----------------------------

ExitAliens:
    LDA SCANLINE_COUNT
    CLC
    ADC #ALIEN_LEN
    STA SCANLINE_COUNT
    TAY


;   Jump if Aliens Land (No Print Player)
    CPY #PLAYER_SCAN-4
    BCC PlayerAlive

;   Player Is Dead
    LDA #$10
    STA NUSIZ0
    STA NUSIZ1
    STA WSYNC
;   Delay to print the shot limits in the right and left position
    LDX #2
PlayerDeadAjustScan:
    DEX
    BNE PlayerDeadAjustScan
    NOP
    JMP PlayerDeadJmp


;   Play Alive
PlayerAlive:
    LDA #0
    STA VDELP0
    STA VDELP1
;   Jump if Aliens "Destroy" Base Defense
    CPY #[DEF_SCAN-3]
;   Defense Is Destroyed
    BMI DefenseNotIsGone
    ; JSR TryDrawMSL
;   8 cycles Delay / 24 Color Clocks
    LDA ($80,X)     ; Delay
    NOP             ; Delay
    INY
NoScanConsume:
    JMP DefenseIsGone

DefenseNotIsGone:
;   8 cycles Delay / 24 Color Clocks
    LDA ($80,X)     ; Delay
    NOP             ; Delay

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

;   Wait for time to draw the 3 defensive barriers
WaitDefense:
    INY
    JSR TryDrawMSL
    STA WSYNC
    STA HMCLR
    CPY #[DEF_SCAN-1]
    BCS WaitDefense2
    INY
    JSR TryDrawOrChangeMSL
    STA WSYNC
    STA HMOVE
    CPY #[DEF_SCAN-1]
    BCC WaitDefense
WaitDefense2:

;   Saving the current Scanline count
    INY
    STY SCANLINE_COUNT

;   Delay for GRP exchange at correct time
    LDY #13
LoopRelativeDelayDefense:
    DEY
    BNE LoopRelativeDelayDefense
    NOP

;------------------- Main loop for drawing Defense -------------------
;   Time-synchronized loop to draw defenses
    LDX #0
LoopDrawDefense:
    DEC MSL_CURRSCAN
    LDA MSL_CURRSCAN
    CMP #8
    BCC TryDrawMSL3
    LDA #0
    BCS TryDrawMSL4
TryDrawMSL3:
    NOP
    LDA #2
TryDrawMSL4:
    STA ENABL

;   Draw Defenses
    LDA DEFENSE_SHAPE,X
    STA GRP0
;   Delay 4 Cycles
    NOP
    NOP
    LDA [DEFENSE_SHAPE+DEFENSE_LEN],X
    STA GRP0
;   Delay 4 Cycles
    NOP
    NOP
    LDA [DEFENSE_SHAPE+2*DEFENSE_LEN],X
    STA GRP0

    LDY #4
LoopDelayDefense:
    DEY
    BNE LoopDelayDefense
;   Delay 6 Cycles
    NOP
    NOP
    NOP

    DEC MSL_CURRSCAN
    LDA MSL_CURRSCAN
    CMP #8
    BCC TryDrawMSL5
    LDA #0
    BCS TryDrawMSL6
TryDrawMSL5:
    NOP
    LDA #2
TryDrawMSL6:
    STA ENABL

    LDA DEFENSE_SHAPE,X
    STA GRP0
;   Delay 4 Cycles
    NOP
    NOP
    LDA DEFENSE_SHAPE+DEFENSE_LEN,X
    STA GRP0
;   Delay 4 Cycles
    NOP
    NOP
    LDA [DEFENSE_SHAPE+2*DEFENSE_LEN],X
    STA GRP0
;   Delay 10 Cycles
    ROL $80
    ROR $80
    INC SCANLINE_COUNT
    INC SCANLINE_COUNT
    INX
    CPX #9
    BNE LoopDrawDefense

;------------------- Exit Defense -------------------

    LDA #0
    STA GRP0
    LDY SCANLINE_COUNT
    INY

;   After Defense or Aliens (def destroyed)
DefenseIsGone:
;   Player GRP set
    LDA #<SpritePlayer
    STA POINTER_GRP
    LDA #>SpritePlayer
    STA POINTER_GRP+1
;   Set Player Pos
    LDA PLAYER_POS
    LDX #0
    JSR SetHorizPos

WaitPlayer:
    JSR TryDrawOrChangeMSL
    INY
    STA WSYNC
    STA HMOVE
    CPY #[PLAYER_SCAN-3]
    BCS WaitPlayer2
    JSR TryDrawMSL
    INY
    STA WSYNC
    STA HMCLR
    CPY #[PLAYER_SCAN-3]
    BCC WaitPlayer
WaitPlayer2:
    INY
    STA WSYNC
    LDA #0
    STA ENABL
    STA HMOVE
    INY
    STA WSYNC
    STA HMCLR
;   Start Draw Player
;   Missele/Player Sets
    LDA #$10
    STA NUSIZ0
    STA NUSIZ1
;   Set Player Color
    LDA #PLAYER_COLOR
    STA COLUP0
;   Save in Y Value in X, Y need for Indirect Address
    TYA
    TAX

;------------------- Main loop for drawing Player -------------------
    LDY #[PLAYER_LEN-1]
DrawPlayer:
    INX
    STA WSYNC                   ; --> Sync Wsync
    LDA (POINTER_GRP),Y
    STA GRP0
    DEY
    BPL DrawPlayer

;------------------- Exit Player -------------------

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
    NOP
    STA RESM0                   ; --> Need Sync
;   Fine Limits Position
    LDA #$C0
    STA HMM0
    LDA #$F0
    STA HMM1
;   Delay M1 Limits Position
    LDX #3
DelayM1:
    DEX
    BNE DelayM1
    NOP
    STA RESM1                   ; --> Need Sync
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
    STA ENABL
    STA ENAM0
    STA ENAM1
    STA HMCLR
;
;=============================================================================================
;=============================================================================================
;                                 OVERSCAN CODE AREA
;=============================================================================================
;=============================================================================================
;   Set Direction and Position of Aliens Sprites
    JSR CheckLogicAliens

;   Overscan Code
    JSR RandNumber  ; Temp, only tests

;=============================================================================================
;                              COLLISION PROCESSING AREA
;=============================================================================================

;   Collision Code
;   P0 and Ball Collided. There are 3 cases, Defense, Alien, Player
    BIT CXP0FB
    BVS Collision
;   P1 and Ball Collided, only aliens use P1
    BIT CXP1FB
    BVS Collision
    JMP NoCollision
Collision:
    JSR BallCollision
NoCollision:

;   Wait hot Scanlines Over
ScanlineEnd:
    LDA INTIM
    BNE ScanlineEnd

;=============================================================================================
;=============================================================================================
;=============================================================================================
;                                   END OF KERNEL
;=============================================================================================
;=============================================================================================
;=============================================================================================
Overscan:
    STA WSYNC

    LDA #%11000010          ; "Turn Off Cathodic Ray"
;   Starts VBlank and Latches Inputs
    STA VBLANK

    LDA #OVERSCAN_TIMER     ; Timing OverScanlines
    STA TIM8T

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
;=============================================================================================
;                               FUNCTION DECLARATION
;=============================================================================================
;=============================================================================================
;=============================================================================================
;   Functions Codes

; FUNCTION RandNumber (None):
;   Get next random number
;   Based in Linear-feedback Shift Register
;
RandNumber:
    LDA RANDOM_NUMBER
    LSR
    BCC NoEOR
    EOR #TAPS
NoEOR:
    STA RANDOM_NUMBER
    RTS


;FUNCTION SetHorizPos (A,X)
;   Where X Represents Values ​for Object (0: Player 0, 1: Player 1, 2: Missile 0, 3: Missile 1, 4: Ball)
;   Where A Contains the Color Clock Value of the Object on the Screen (Collumn Position of Object)
;
;   Function with Extreme Precision of Time Consumption and Triggering of Object Position Recorders,
; with the Same Value as Register A.
;
SetHorizPos:        ; CPU Execution Cost and Accumulated Color Clocks (Minimum and Maximum cases After the Loop)
    STA WSYNC       ;  -
    CMP $80         ; 3 (9)
    SEC             ; 2 (15)
LoopDivision: ; Each loop consumes 5 cycles, the last loop consumes 4. The minimum consumption is 4 cycles and the maximum is 59 cycles
    SBC #15         ; 2
;   This Branch cannot be Borrowed Page, as it is in sync with machine cycles to work with precise timing.
    BCS LoopDivision; 2/3
    EOR #7          ; 2 (27/177)
    ASL             ; 2 (33/183)
    ASL             ; 2 (39/189)
    ASL             ; 2 (45/195)
    ASL             ; 2 (51/201)
;   Fast Pos calc P = (A//15)*15+6
    STA RESP0,X     ; 4 (57/207) --> (69/219) --> +5 --> (74/224) --> -68 --> (6/156)
;   Fast Motion calc M = P - A
    STA HMP0,X
    RTS


;FUNCTION BallCollision (None):
;   Collision between Player 0 and Ball,
;    possibilities: aliens, Defense, player hit
;
BallCollision:
    BIT MSL_STAT
    BPL AlienShoot
    LDA #$80
    EOR MSL_STAT
    STA MSL_STAT
;   Alien or Defense Hit
;   Rotate Alien collided to delete
    LDA #%11011111
    STA TEMP_SCORE_ATT
;   Detect which collumn collided
    LDA ALIENS_POS
    CLC
    ADC #16
    LDX #5
CheckCollumnCollision:
    CMP MSL_POS
    BCS CollumnDetected
    ADC #16
;   Next Alien to check
    SEC
    ROR TEMP_SCORE_ATT
;   Last one collided
    DEX
    BNE CheckCollumnCollision

CollumnDetected:
;   Check which line collided
    LDX #[ALIENS_LINES-1]
    LDA ALIENS_SCAN
    CLC
    ADC #[ALIEN_LEN-8]    ; 8 is the Max Length of the Missile
CheckLineCollision:
    CMP MSL_SCAN
    BCS LineDetected
    ADC #[ALIEN_LEN+ALIENS_INTER]
    DEX
    JMP CheckLineCollision

;   X := Line Collided
LineDetected:
;   Check Ball Scanline, Make sure there is Defense Collision
    LDA MSL_SCAN
    CMP #$88
    BCC AlienCollision
;   Check if Aliens have already destroyed the Defense
    LDA ALIENS_SCAN
    LDY ALIENS_NUM
LoopAliensSumScan:
    CLC
    ADC #ALIEN_LEN+ALIENS_INTER
    DEY
    BNE LoopAliensSumScan
    SEC
    SBC #ALIENS_INTER
    CMP #$8F
    BCS AlienCollision
    JMP DefenseBallCollision ; WARNING !! Use Trick Change JSR to JMP
;   Any code entered here or below will not be executed!
    ; RTS   ; RTS Implicit above

AlienCollision:
;   Remove hit Alien
    LDA TEMP_SCORE_ATT
    AND ALIENS_STAT,X
    STA ALIENS_STAT,X
;   Increase Score
    SED
    LDA SCORE_VALUE
LoopSumPoints:
    CLC
    ADC #5
    BCC NoCarryScore
;   Increases the 2 most significant Score counters
    STA SCORE_VALUE
    LDA SCORE_VALUE+2
;   DO NOT use INC, it does not operate in decimal mode going from 99 to 9A instead of 00
    ADC #0              ; Carry always 1
    STA SCORE_VALUE+2
    LDA SCORE_VALUE
NoCarryScore:
    DEX
    BPL LoopSumPoints
;   Save the New Score
    STA SCORE_VALUE
    CLD
OutCollision:
    RTS

AlienShoot:
    ; Aliens hit Defense or Player
    RTS


;FUNCTION DefenseBallCollision (None):
;   Aliens or Player hit defense Structure
DefenseBallCollision:

    RTS


;FUNCTION ResetGame (None):
;   Restart the Game or Prepare for a New Round
ResetGame:
    LDA SCORE_VALUE+2
    CMP SCORE_VALUE+3
    BEQ ScoreEqual
    BCC NoHighScore
    BNE NewHighScore    ;   Better than JMP, always Branch, max +127 Bytes
;   high Score Equal
ScoreEqual:
    LDA SCORE_VALUE+1
    CMP SCORE_VALUE
    BCS NoHighScore
NewHighScore:
    LDA SCORE_VALUE
    STA SCORE_VALUE+1
    LDA SCORE_VALUE+2
    STA SCORE_VALUE+3
NoHighScore:
;   Reset Score
    LDA #0
    STA MSL_STAT
    STA SCORE_VALUE
    STA SCORE_VALUE+2

NewRound:
;   Board Limits of Aliens
    LDA #ALIENS_DR_LIMIT
    STA ALIENS_R_LIMIT

;   ---- Set Limits and Aliens CFG ----
;   Speed Move Aliens set
    LDA #ALS_FRAME_MOVE
    STA ALIENS_SPEED
    STA FRAME_COUNT

;   Move Direction (Left, Right)
    LDA #$0
    STA ALIENS_MASK

;   Set Aliens Alive
    LDA #%00111111
    STA COLLUMNS_ALIVE
    LDX #ALIENS_LINES
LoopSetAttEnemies:
    STA ALIENS_STAT-1,X
    DEX
    BNE LoopSetAttEnemies

;   ---- Set Positions ----
;   Set Player X-pos
    LDA #PLAYER_START
    STA PLAYER_POS

;   Set Aliens X-Pos
    LDA #ALIENS_START
    STA ALIENS_POS
    LDX #0
    JSR AjustDelayAliens
    LDA ALIENS_POS
    CLC
    ADC #16
    INX
    JSR AjustDelayAliens

;   Set Base Defense X-Pos
    LDA #DEFENSE_START
    STA DEFENSE_POS

;   Set Aliens Y-Pos
    LDA #ENEMY_DIST
    STA ALIENS_SCAN

;   --- Set Base Shape ---
    LDX #[DEFENSE_LEN-1]
LoopLoadDefense:
    LDA SpriteDefense,X
    STA DEFENSE_SHAPE,X
    STA [DEFENSE_SHAPE+DEFENSE_LEN],X
    STA [DEFENSE_SHAPE+2*DEFENSE_LEN],X
    DEX
    BPL LoopLoadDefense

    RTS


;FUNCTION AjustDelayAliens (None):
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
    BCC OutAjust    ; Branch if [A < 31]
;   Balance as Needed
LoopAjustDelay:
    TAX
    LDA ALIENS_DELAY
    SBC #2
    STA ALIENS_DELAY
    BCS NoPageBorrow
    DEC ALIENS_DELAY+1
    SEC

NoPageBorrow:    ; Check Carry From Low to High Addresses
    TXA
    SBC #9
    BCS LoopAjustDelay

OutAjust:
    RTS


;FUNCTION CheckLogicAliens (None):
;   This Function Will Check Empty Side Collumns,
; If You Think It Will Adjust The Aliens' Movement Limits,
; Always Keeping The First Collumn And Line As Relative To The Movement
;
CheckLogicAliens:
    LDX #4
    LDA ALIENS_STAT+5
LoopCheckCollumns:           ; Check Empty Collumns
    ORA ALIENS_STAT,X
    DEX
    BPL LoopCheckCollumns
;   Checks Change From Previous State (Some Collumn Was killed in This Time Interval)
    CMP COLLUMNS_ALIVE
    BEQ SetVerticalLimit    ; If There are no Changes, Branch !
;   Backup to Memory
    STA COLLUMNS_ALIVE
    CMP #0
    BNE NoResetAliens
    JMP NewRound            ; WARNING !! Use Trick Change JSR to JMP
;   Any code entered here or below will not be executed!
NoResetAliens:
;   Prepare to Check Collumns to the Left
    ASL
    ASL
    ASL
    STA TEMP_SCORE_ATT
;   If First Collumn Exists, Branch !
    BCS CheckRightLimit
;   Move Aliens From Each Collumn Until There is at Least 1 Alien in the First Collumn (Collumn Existence Condition)
    LDA #0
RepositionAliveAliens:
    LDX #5
ShiftAliens:
    ASL ALIENS_STAT,X
    DEX
    BPL ShiftAliens
;   After Displacement Reposition the Aliens (Same Absolute Position on the Screen)
    CLC
    ADC #16
;   Continue Until the Collumn Exists
    ROL TEMP_SCORE_ATT
    BCC RepositionAliveAliens

;   Set final offset
    CLC
    ADC ALIENS_POS
    STA ALIENS_POS
    JSR AjustDelayAliens ; Ajust delay for new aliens position

;   Make a new update COLLUMNS_ALIVE
    LDX #4
    LDA ALIENS_STAT+5
LoopCheckNewCollumns:           ; Check Empty Collumns
    ORA ALIENS_STAT,X
    DEX
    BPL LoopCheckNewCollumns
    STA COLLUMNS_ALIVE

CheckRightLimit:
;   Prepare to Check Collumns to the Right
    LDA COLLUMNS_ALIVE   ;Backup to Memory Update Collumns
    AND #%00111111
    STA TEMP_SCORE_ATT
;   Standard Limit of Aliens to the Right
    LDA #[ALIENS_DR_LIMIT-16] ; -16 because it will always add up at least once
    CLC
;   Increases Right Limit (without the presence of Collumns in Right)
LoopBoardRightLimit:
    ADC #16
    ROR TEMP_SCORE_ATT
    BCC LoopBoardRightLimit
    STA ALIENS_R_LIMIT

;   Set the Vertical Limit (Maximum Descent of the First Row, Which is the Highest on the Screen)
SetVerticalLimit:
;   Reset Alien Line Numbers
    LDA #ALIENS_LINES
    STA ALIENS_NUM
    STA ALIENS_COUNT
;   Set Default Base Hit Y-Ground (Relative to first line)
    LDA #ALIENS_DY_LIMIT
;   Loop to Check Empty Lines Below
    LDX #0
CheckUnderLimit:
    LDY ALIENS_STAT,X    ; From the Lowest on the Screen to the Highest
    BNE NoUnderChange
;   If it does Not Exist, it Increases the Maximum Line Descent Limit
    CLC
    ADC #[ALIEN_LEN+ALIENS_INTER]
    DEC ALIENS_NUM      ; Decrement the Count of "Live" Lines
    INX
    CPX #6              ; Repeat for All Lines
    BNE CheckUnderLimit
NoUnderChange:
    STA ALIENS_Y_LIMIT
    RTS


;FUNCTION MoveSpritesAliens (None):
;   This Function Will Detect the Passage of Time Through Frame Counting,
; Making the Aliens Move Horizontally and Treating the Vertical Descent
; When It Reaches the Side Limits
;
MoveSpritesAliens:
;   Check End Game
    LDA ALIENS_SPEED
    BEQ SetOut
;   Check Time to Move
    LDX FRAME_COUNT
    BNE SetOut
;   Reset Count
    STA FRAME_COUNT
;   Change Orientation Alien
    LDA ALIENS_MASK
    EOR #$40
    STA ALIENS_MASK
;   Sprite Move
    LDA ALIENS_POS
    CLC
    BIT ALIENS_MASK
    BMI AliensMoveBackward
;   AliensMoveForward
    ADC #ALIENS_X_SPEED
    CMP ALIENS_R_LIMIT      ; Check Max Right limit
    BCS ReverseDirection
    JMP PosMove

AliensMoveBackward:
    ADC #[-ALIENS_X_SPEED]
    CMP #ALIENS_L_LIMIT
    BCC ReverseDirection    ; Check Max Left limit
PosMove:
    STA ALIENS_POS          ; Set Move
;   Set Delay Time for draw aliens
    JMP AjustDelayAliens    ; WARNING !! Use Trick Change JSR to JMP
;   Any code entered here or below will not be executed!
SetOut:
;   WARNING if need code here !!
    RTS

;   Handles collisions with edges, Make reverse motion
ReverseDirection:   ; Treats vertical displacement of aliens
    LDA ALIENS_SCAN
    CLC
    ADC #ALIENS_Y_SPEED
    STA ALIENS_SCAN
;   Vertical Limit Check (End of Game, Collision with Player/Ground)
    CMP ALIENS_Y_LIMIT
    BCC NoEndGame
;   Collision with player/ground
;   Landing adjustments (no bugs and flick scan on landing)
    LDA #[GROUND_SCAN-3]
    LDX ALIENS_NUM
    SEC
LoopAjustEndScan:
    SBC #[ALIEN_LEN+ALIENS_INTER]
    DEX
    BNE LoopAjustEndScan
    CLC
    ADC #ALIEN_LEN

AjustEndScan:
    STA ALIENS_SCAN
;   Stop moving Sprites
    LDA #0
    STA ALIENS_SPEED

NoEndGame:
;   Aliens direction swap
    LDA ALIENS_MASK
    EOR #$80
    STA ALIENS_MASK
    RTS


;FUNCTION SetIndexScore (None):
;   This function calculates the Index related
; to the Number0 graph, to be able to access
; them and draw them on the Score screen
;
SetIndexScore:
    LDX #3
    LDY #6
LoopIndexScore:
    LDA SCORE_VALUE,X
;   Multiply by 5, being times 4 added to 1
    AND #$F0
    LSR
    LSR
;   Number multiplied by 4
    STA TEMP_SCORE_ATT
    LSR
    LSR
;   Adds it to itself, totaling multiplied by 5
    ; CLC   Always clear
    ADC TEMP_SCORE_ATT
    ADC #<Number0
    STA SCORE_INDEX,Y

    LDA SCORE_VALUE,X
    AND #$0F
;   Save himself
    STA TEMP_SCORE_ATT
    ASL
    ASL
;   Adds with multiplied by 4, totaling multiplied by 5
    ; CLC   Always clear
    ADC TEMP_SCORE_ATT
    ADC #<Number0
    STA SCORE_INDEX+1,Y

    DEY
    DEY
    DEX
    BPL LoopIndexScore
    RTS


;FUNCTION MoveMSL(A,X):
;   This function moves missiles towards players and aliens
; use the X input to change the current missile and
; use the A offset value to move
;
MoveMSL:
    CLC
    ADC MSL_SCAN,X
    STA MSL_SCAN,X
    RTS


;FUNCTION TryDrawMSL (None):
;   This Function activates/deactivates
; the missile (ball), based on the relative
; position of the missile in relation to the screen.
;
TryDrawMSL:
    DEC MSL_CURRSCAN
    LDA MSL_CURRSCAN
    CMP #8
    LDA #2
    BCC DrawMSL
    LSR
DrawMSL:
    STA ENABL
OutTryMSL:
    RTS

TryDrawOrChangeMSL:
    DEC MSL_CURRSCAN
    LDA MSL_CURRSCAN
    CMP #8
    LDA #2
    BCC DrawMSL2
    LSR
DrawMSL2:
    STA ENABL

    LDA MSL_CURRSCAN
    BPL NoNextMSL
    LDA MSL_NEXTPOS
    BEQ NoNextMSL
    CLC
    ADC MSL_NEXTPOS
    STA MSL_CURRSCAN
    LDA MSL_NEXTHMV
    STA HMBL
    LDA #0
    STA MSL_NEXTPOS
NoNextMSL:
    RTS


;=============================================================================================
;                             DATA DECLARATION
;=============================================================================================
;   Numbers Sprite for Score
    SEG     DATA
    ORG     $FF33

Number0:
    .BYTE #%11100111
    .BYTE #%10100101
    .BYTE #%10100101
    .BYTE #%10100101
    .BYTE #%11100111

Number1:
    .BYTE #%11100111
    .BYTE #%01000010
    .BYTE #%01000010
    .BYTE #%01100110
    .BYTE #%01000010

Number2:
    .BYTE #%11100111
    .BYTE #%00100100
    .BYTE #%11100111
    .BYTE #%10000001
    .BYTE #%11100111

Number3:
    .BYTE #%11100111
    .BYTE #%10000001
    .BYTE #%11100111
    .BYTE #%10000001
    .BYTE #%11100111

Number4:
    .BYTE #%10000001
    .BYTE #%10000001
    .BYTE #%11100111
    .BYTE #%10100101
    .BYTE #%10100101

Number5:
    .BYTE #%11100111
    .BYTE #%10000001
    .BYTE #%11100111
    .BYTE #%00100100
    .BYTE #%11100111

Number6:
    .BYTE #%11100111
    .BYTE #%10100101
    .BYTE #%11100111
    .BYTE #%00100100
    .BYTE #%00100100

Number7:
    .BYTE #%10000001
    .BYTE #%10000001
    .BYTE #%10000001
    .BYTE #%10000001
    .BYTE #%11100111

Number8:
    .BYTE #%11100111
    .BYTE #%10100101
    .BYTE #%11100111
    .BYTE #%10100101
    .BYTE #%11100111

Number9:
    .BYTE #%10000001
    .BYTE #%10000001
    .BYTE #%11100111
    .BYTE #%10100101
    .BYTE #%11100111

;   Player, Aliens and Defense Sprites
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
    .BYTE #%11111110
    .BYTE #%11111110
    .BYTE #%01111100
    .BYTE #%11111110
    .BYTE #%00111000
    .BYTE #%00111000
    .BYTE #%01111100
    .BYTE #%00111000
    .BYTE #%00111000
    .BYTE #%00010000

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

;   Interrupt Vector Table
    ORG $FFFA

    .WORD BootGame ; NMI
    .WORD BootGame ; EntryPoint
    .WORD BootGame ; IRQ/BRK
END
