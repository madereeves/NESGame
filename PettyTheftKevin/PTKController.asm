  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring


;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0

gamestate  .rs 1  ; .rs 1 means reserve one byte of space
playerY   .rs 1  ;
playerX   .rs 1  ;
playerFuture .rs 1
enemyY    .rs 1
enemyX    .rs 1
buttons1   .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; player 2 gamepad buttons, one bit per button
scoreOnes     .rs 1  ; byte for each digit in the decimal score
scoreTens     .rs 1
scoreHundreds .rs 1

collision     .rs 1

bgPointerLo .rs 1
bgPointerHi .rs 1


;; DECLARE SOME CONSTANTS HERE
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $05  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen

RIGHTWALL      = $F4  ; when ball reaches one of these, do something
TOPWALL        = $20
BOTTOMWALL     = $D0
LEFTWALL       = $04


PADDLE2X       = $F0

PLAYERSPEED    = $05
ENEMYSPEED     = $01

;;;;;;;;;;;;;;;;;;




  .bank 0
  .org $C000

vblankwait:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait
  RTS

RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

  JSR vblankwait

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem

  JSR vblankwait

LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down

LoadBackground:
  LDA $2002             ; Load the PPU status register to reset the PPU latch

  LDA #$20
  STA $2006             ; Write the high byte of the PPU Background Nametable 0 Address
  LDA #$00
  STA $2006             ; Write the low byte of the PPU Background Nametable 0 Address
                        ; $2000 - $23FF = Background Nametable 0 (960 Bytes of Tiles + 64 Bytes of Attributes)
                        ; $2400 - $27FF = Background Nametable 1
                        ; $2800 - $2BFF = Background Nametable 2
                        ; $2C00 - $2FFF = Background Nametable 3

  LDA #$00
  STA bgPointerLo       ; Write the low byte of the Background Nametable Address to the Background Address Pointer
  LDA #HIGH(background1) ; Use the HIGH() function to load the high byte of the Background Nametable Address
  STA bgPointerHi       ; Write the high byte of the Background Nametable Address to the Background Address Pointer

  LDX #$04              ; Set the number of Background Nametable pages (256 bytes each)
  LDY #$00              ; Set the page size (256 bytes)
NEXT:
  LDA [bgPointerLo], Y  ; Load the 16-bit bgPointer for the current Background Nametable index
  STA $2007             ; Send the current byte to the PPU
  INY
  BNE NEXT
NEXBLK:
  INC bgPointerHi
  DEX
  BMI DONE
  BNE NEXT
  LDY #$00
  BNE NEXT
DONE:





;;;Set some initial ball stats
  LDA #$80
  STA enemyY
  LDA #$80
  STA enemyX

  ;;;;Initial Player stats
  LDA #$50
  STA playerY
  LDA #$50
  STA playerX

  ;;;collision
  LDA #$00
  STA collision


  ;;;Set initial score value
  LDA #$00
  STA scoreOnes
  STA scoreTens
  STA scoreHundreds

  LDA #STATEPLAYING ; The Starting Game State
  STA gamestate

  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop, waiting for NMI



NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  JSR DrawScore

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005

  ;;;all graphics updates done by here, run game engine


  JSR ReadController1  ;;get the current button data for player 1
  JSR ReadController2  ;;get the current button data for player 2

GameEngine:
  LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle ; show the title screen

  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver ; show the game over screen

  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying ; start the game
GameEngineDone:

  JSR UpdateSprites ; set the sprite positions

  RTI             ; return from interrupt




;;;;;;;;

EngineTitle:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load game screen
  ;;  set starting paddle/ball position
  ;;  go to Playing State
  ;;  turn screen on

  JMP GameEngineDone

;;;;;;;;;

EngineGameOver:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load title screen
  ;;  go to Title State
  ;;  turn screen on
  JMP GameEngineDone

;;;;;;;;;;;

EnginePlaying:

;;;;Player Stuff

MovePlayerUp:
  ;;if up button pressed
  ;;  if paddle top > top wall
  ;;    move paddle top and bottom up
  LDA $0204
  CMP #TOPWALL
  BCC MovePlayerUpDone

  ;LDA playerY
  ;CLC
  ;ADC #$01
  ;STA playerFuture
  ;JSR CheckCollision
  ;LDA collision
  ;CMP #$01
  ;BEQ MovePlayerUpDone
  ;LDX #$00
  ;LDA background, x
  ;CMP #$00
  ;BEQ MovePlayerUpDone

  LDA buttons1
  AND #%00001000
  BEQ MovePlayerUpDone
  LDA playerY
  SEC
  SBC #PLAYERSPEED
  STA playerY
MovePlayerUpDone:

MovePlayerDown:
  ;;if down button pressed
  ;;  if paddle bottom < bottom wallr
  ;;    move paddle top and bottom down
  LDA $020C
  CMP #BOTTOMWALL
  BCS MovePlayerDownDone

  LDA buttons1
  AND #%00000100
  BEQ MovePlayerDownDone

  LDA playerY
  CLC
  ADC #PLAYERSPEED
  STA playerY
MovePlayerDownDone:

MovePlayerRight:
  LDA $020B
  CMP #RIGHTWALL
  BCS MovePlayerRightDone

  LDA buttons1
  AND #%00000001
  BEQ MovePlayerRightDone

  LDA playerX
  CLC
  ADC #PLAYERSPEED
  STA playerX
MovePlayerRightDone:

MovePlayerLeft:
  LDA $0203
  CMP #LEFTWALL
  BCC MovePlayerLeftDone

  LDA buttons1
  AND #%00000010
  BEQ MovePlayerLeftDone
  LDA playerX
  SEC
  SBC #PLAYERSPEED
  STA playerX
MovePlayerLeftDone:

;;;;;AI Enemy
MoveAIHorz:
  LDA enemyX
  CMP playerX
  BCC MoveAIRight

  LDA enemyX
  SEC
  SBC #$01
  STA enemyX
  JMP MoveAIHorzDone
MoveAIRight:
  LDA enemyX
  CLC
  ADC #$01
  STA enemyX
MoveAIHorzDone:

MoveAIVert:
  LDA enemyY
  CMP playerY
  BCC MoveAIDown

  LDA enemyY
  SEC
  SBC #$01
  STA enemyY
  JMP MoveAIVertDone
MoveAIDown:
  LDA enemyY
  CLC
  ADC #$01
  STA enemyY
MoveAIVertDone:


  JMP GameEngineDone

UpdateSprites:
    ;;Kevin

  LDA playerY
  STA $0200

  LDA #$C3
  STA $0201

  LDA #$01
  STA $0202

  LDA playerX
  STA $0203

  LDA playerY
  CLC
  ADC #$08
  STA $0204

  LDA #$D3
  STA $0205

  LDA #$01
  STA $0206

  LDA playerX
  STA $0207

  LDA playerY
  STA $0208

  LDA #$C4
  STA $0209

  LDA #$01
  STA $020A

  LDA playerX
  CLC
  ADC #$08
  STA $020B

  LDA playerY
  CLC
  ADC #$08
  STA $020C

  LDA #$D4
  STA $020D

  LDA #$01
  STA $020E

  LDA playerX
  CLC
  ADC #$08
  STA $020F

;;;Enemy
  LDA enemyY
  STA $0210

  LDA #$C5
  STA $0211

  LDA #$01
  STA $0212

  LDA enemyX
  STA $0213

  LDA enemyY
  CLC
  ADC #$08
  STA $0214

  LDA #$D5
  STA $0215

  LDA #$01
  STA $0216

  LDA enemyX
  STA $0217

  LDA enemyY
  STA $0218

  LDA #$C6
  STA $0219

  LDA #$01
  STA $021A

  LDA enemyX
  CLC
  ADC #$08
  STA $021B

  LDA enemyY
  CLC
  ADC #$08
  STA $021C

  LDA #$D6
  STA $021D

  LDA #$01
  STA $021E

  LDA enemyX
  CLC
  ADC #$08
  STA $021F

;CheckCollision:
  ;LDA playerFuture
  ;;Wall One
  ;CMP #$10
  ;BCC WallOneDone
  ;LDA #$01
  ;STA collision
;WallOneDone:
  ;RTS

  DrawScore:
    LDA $2002
    LDA #$20
    STA $2006
    LDA #$20
    STA $2006          ; start drawing the score at PPU $2020

    LDA scoreHundreds  ; get first digit
  ;  CLC
  ;  ADC #$30           ; add ascii offset  (this is UNUSED because the tiles for digits start at 0)
    STA $2007          ; draw to background
    LDA scoreTens      ; next digit
  ;  CLC
  ;  ADC #$30           ; add ascii offset
    STA $2007
    LDA scoreOnes      ; last digit
  ;  CLC
  ;  ADC #$30           ; add ascii offset
    STA $2007
    RTS


  IncrementScore:
  IncOnes:
    LDA scoreOnes      ; load the lowest digit of the number
    CLC
    ADC #$01           ; add one
    STA scoreOnes
    CMP #$0A           ; check if it overflowed, now equals 10
    BNE IncDone        ; if there was no overflow, all done
  IncTens:
    LDA #$00
    STA scoreOnes      ; wrap digit to 0
    LDA scoreTens      ; load the next digit
    CLC
    ADC #$01           ; add one, the carry from previous digit
    STA scoreTens
    CMP #$0A           ; check if it overflowed, now equals 10
    BNE IncDone        ; if there was no overflow, all done
  IncHundreds:
    LDA #$00
    STA scoreTens      ; wrap digit to 0
    LDA scoreHundreds  ; load the next digit
    CLC
    ADC #$01           ; add one, the carry from previous digit
    STA scoreHundreds
  IncDone:
    RTS

ReadController1:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController1Loop:
  LDA $4016
  LSR A
  ROL buttons1
  DEX
  BNE ReadController1Loop
  RTS

ReadController2:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController2Loop:
  LDA $4017
  LSR A
  ROL buttons2
  DEX
  BNE ReadController2Loop
  RTS





;;;;;;;;;;;;;;



  .bank 1
  .org $E000

background1:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$56,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 1
  ;;Above this is not seen on NTSC screen
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 2
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 3
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 4
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 5
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 6
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 7
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 8
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 9
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 10
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 11
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 12
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 13
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 14
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 15
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 16
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 17
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 18
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 19
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 20
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 21
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 22
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 23
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 24
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 25
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 26
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 27
  .db $55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56,$55,$56 ; Row 28
  .db $53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54,$53,$54 ; Row 29
  ;;Below this is not seen on NTSC screen
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$56,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 30

; The 64 Byte Attributes table holds the palette selections for the Background Nametable.
; Each byte affects a 4x4 tile array on screen (32x32 pixels)
  ; Each byte is divided into four 2x2 tile quadrants, each of which can have a different palette
  ; Each quadrant consists of 2 bits, controlling which of the four background palettes the quadrant (2x2 tiles) will use
  ; %00 = Palette 1, %01 = Palette 2, %10 = Palette 3, %11 = Palette 4
  ; Reading each byte from left to right, the quadrant order is Bottom Right, Bottom Left, Top Right, Top Left
; The Attributes table is stored after the tile data in the Nametable, starting at PPU address $23C0
attributes:
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100

palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $0C,$17,$28,$39,  $0C,$17,$28,$39,  $0C,$17,$28,$39,  $0C,$17,$28,$39   ;;sprite palette

sprites:
     ;vert tile attr horiz
  .db $80, $01, $00, $80   ;sprite 0

  .db $10, $02, $00, $88   ;sprite 1
  .db $18, $02, $00, $80   ;sprite 2
  .db $88, $02, $00, $88   ;sprite 3
  .db $88, $02, $00, $88

  .db $88, $02, $00, $88   ;sprite 4
  .db $88, $02, $00, $88   ;sprite 5
  .db $88, $02, $00, $88   ;sprite 6
  .db $10, $04, $00, $88   ;sprite 7

  .db $88, $05, $00, $88   ;sprite 8
  .db $88, $06, $00, $88   ;sprite 9
  .db $88, $07, $00, $88   ;sprite 10
  .db $88, $05, $00, $88   ;sprite 11



  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial


;;;;;;;;;;;;;;


  .bank 2
  .org $0000
  .incbin "Kevin.chr"   ;includes 8KB graphics file from SMB1
