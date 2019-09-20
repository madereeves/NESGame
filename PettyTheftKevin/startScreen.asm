; iNES Header
  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM-128, no bank swapping
  .inesmir 1   ; background mirroring

; Variable Reservations
  .rsset $0000

bgPointerLo   .rs 1 ; Low byte of pointer address to load new background nametable
bgPointerHi   .rs 1 ; High byte of pointer address to load new background nametable

gamestate     .rs 1  ; .rs 1 means reserve one byte of space
ballx         .rs 1  ; ball horizontal position
bally         .rs 1  ; ball vertical position
ballup        .rs 1  ; 1 = ball moving up
balldown      .rs 1  ; 1 = ball moving down
ballleft      .rs 1  ; 1 = ball moving left
ballright     .rs 1  ; 1 = ball moving right
ballspeedx    .rs 1  ; ball horizontal speed per frame
ballspeedy    .rs 1  ; ball vertical speed per frame
paddle1ytop   .rs 1  ; player 1 paddle top vertical position
paddle2ytop   .rs 1  ; player 2 paddle top vertical position
buttons1      .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2      .rs 1  ; player 2 gamepad buttons, one bit per button
; score1        .rs 1  ; player 1 score, 0-15
; score2        .rs 1  ; player 2 score, 0-15
scoreOnes     .rs 1  ; byte for each digit in the decimal score
scoreTens     .rs 1
scoreHundreds .rs 1

playery       .rs 1
playerx       .rs 1

; Constant Declarations
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $01  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen

RIGHTWALL      = $F4  ; when ball reaches one of these, do something
TOPWALL        = $20
BOTTOMWALL     = $E0
LEFTWALL       = $04

PADDLE1X       = $08  ; horizontal position for paddles, doesnt move
PADDLE2X       = $F0

; Banks = 8KB each
  ; On NROM-128, the 16KB PRG code (2x 8KB PRG Banks) at $C000 is duplicated at $8000
  ; On NROM-256, the 32KB PRG code (4x 8KB PRG Banks) start at $8000 and fill the whole space
  ; On most other mappers, the 16KB PRG code at $C000 is fixed, and the 16KB PRG code at $8000 is switchable

; BANK 0 = First 8KB of PRG ROM

  .bank 0
  .org $C000

; RESET is called at first boot or after a system reset
RESET:
  SEI                   ; Disable IRQs
  CLD                   ; Clear Decimal Mode, which is not available on the RP2A03
  LDX #$40
  STX $4017             ; Disable APU frame IRQ
  LDX #$FF
  TXS                   ; Set up the Stack
  INX                   ; Reset X register to 0
  STX $2000             ; Disable NMI
  STX $2001             ; Disable PPU rendering
  STX $4010             ; Disable APU DMC IRQs

  JSR vblankwait

ClearMem:
  LDA #$00              ; This routine uses Indirect Addressing to Store A at ($NNNN + X)
  STA $0000, x          ; Set $0000+X ($0000 - $00FF) to #$00
  STA $0100, x          ; Set $0100+X ($0100 - $01FF) to #$00
  STA $0300, x          ; Set $0300+X ($0300 - $03FF) to #$00
  STA $0400, x          ; Set $0400+X ($0400 - $04FF) to #$00
  STA $0500, x          ; Set $0500+X ($0500 - $05FF) to #$00
  STA $0600, x          ; Set $0600+X ($0600 - $06FF) to #$00
  STA $0700, x          ; Set $0700+X ($0700 - $07FF) to #$00
  LDA #$FE
  STA $0200, x          ; Set $0200+X ($0200 - $02FF = Sprite RAM values) to $#FE
  INX
  BNE ClearMem          ; Starting at #$00, ClearMem will loop 256 times

  JSR vblankwait

LoadPalettes:
  LDA $2002             ; Load the PPU status register to reset the PPU latch

  LDA #$3F
  STA $2006             ; Write the high byte of the PPU Palette RAM Address
  LDA #$00
  STA $2006             ; Write the low byte of the PPU Palette RAM Address
                        ; $3F00 - $3F0F = Background Palettes
                        ; $3F10 - $3F1F = Sprite Palettes
  LDX #$00              ; Reset the X Register Counter
LoadPalettesLoop:
  LDA palette, x        ; Load a byte from the palette table, where x is the table index
  STA $2007             ; Send the byte to the PPU
  INX
  CPX #$20              ; 32Bytes of Palette Values total (4 Palettes of 4 Colors, x2)
  BNE LoadPalettesLoop

LoadBackgroundTitle:
  JSR BackgroundPPULatch

  LDA #$00
  STA bgPointerLo       ; Write the low byte of the Background Nametable Address to the Background Address Pointer
  LDA #HIGH(backgroundTitle) ; Use the HIGH() function to load the high byte of the Background Nametable Address
  STA bgPointerHi       ; Write the high byte of the Background Nametable Address to the Background Address Pointer

  LDX #$04              ; Set the number of Background Nametable pages (256 bytes each)
  LDY #$00              ; Set the page size (256 bytes)
  JSR BackgroundSwitch



; Load initial ball position and direction
  LDA #$01              ; Ball starts out moving down and right
  STA balldown
  STA ballright
  LDA #$00              ; Ball is not moving up and left
  STA ballup
  STA ballleft

  LDA #$50
  STA bally             ; Set initial Ball Y position

  LDA #$50
  STA ballx             ; Set initial Ball X position

  LDA #$02              ; Set initial ball speed in pixels per frame
  STA ballspeedx
  STA ballspeedy

  LDA #$50
  STA playery
  STA playerx

  LDA #STATETITLE
  STA gamestate

  JSR vblankwait
  JSR EnableRendering

Forever:
  JMP Forever           ; Updates are finished, wait for next NMI

NMI:
  LDA #$00
  STA $2003             ; Tell the PPU the low byte of the CPU's Sprite RAM address
  LDA #$02
  STA $4014             ; Tell the PPU the high byte of the CPU's Sprite RAM address and start the Sprite transfer to the PPU OAM

;  JSR DrawScore

  JSR EnableRendering

  ; JSR ReadController1
  ; JSR ReadController2

GameEngine:
  LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle

  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver    ;show the game over screen

  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying     ;start the game
GameEngineDone:
  RTI

; GAME ENGINES
EngineTitle:
  ; JSR DisableRendering        ;here
  ; JSR BackgroundPPULatch
  ;
  ; LDA #$00
  ; STA bgPointerLo
  ; LDA #HIGH(backgroundTitlenostart)
  ; STA bgPointerHi
  ;
  ; LDX #$04
  ; LDY #$00
  ;
  ; JSR BackgroundSwitch
  ;
  ; LDA #STATEPLAYING
  ; STA gamestate
  ;
  ; JSR vblankwait
  ; JSR EnableRendering    ;here

  LDA buttons1
  AND #%00010000
  BEQ EngineTitleDone
  JSR DisableRendering
  JSR BackgroundPPULatch

  LDA #$00
  STA bgPointerLo
  LDA #HIGH(backgroundPlaying)
  STA bgPointerHi

  LDX #$04
  LDY #$00

  JSR BackgroundSwitch

  LDA #STATEPLAYING
  STA gamestate

  JSR vblankwait
  JSR EnableRendering

EngineTitleDone:
  JMP GameEngineDone

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EngineGameOver:
  JMP GameEngineDone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EnginePlaying:
CheckScore:
  LDA scoreOnes
  CMP #$04
  BNE CheckScoreDone

  JSR DisableRendering
  JSR BackgroundPPULatch

  LDA #$00
  STA bgPointerLo       ; Write the low byte of the Background Nametable Address to the Background Address Pointer
  LDA #HIGH(backgroundTitle) ; Use the HIGH() function to load the high byte of the Background Nametable Address
  STA bgPointerHi       ; Write the high byte of the Background Nametable Address to the Background Address Pointer

  LDX #$04              ; Set the number of Background Nametable pages (256 bytes each)
  LDY #$00              ; Set the page size (256 bytes)

  JSR BackgroundSwitch

  LDA #STATEGAMEOVER
  STA gamestate

  JSR vblankwait
  JSR EnableBackgroundOnly
  JMP GameEngineDone
CheckScoreDone:

; MoveBallRight:
;   LDA ballright
;   BEQ MoveBallRightDone   ;;if ballright=0, skip this section
;
;   LDA ballx
;   CLC
;   ADC ballspeedx        ;;ballx position = ballx + ballspeedx
;   STA ballx
;
;   LDA ballx
;   CMP #RIGHTWALL
;   BCC MoveBallRightDone      ;;if ball x < right wall, still on screen, skip next section
;   LDA #$00
;   STA ballright
;   LDA #$01
;   STA ballleft         ;;bounce, ball now moving left

;  JSR IncrementScore
  ;;in real game, give point to player 1, reset ball
MoveBallRightDone:


; MoveBallLeft:
;   LDA ballleft
;   BEQ MoveBallLeftDone   ;;if ballleft=0, skip this section
;
;   LDA ballx
;   SEC
;   SBC ballspeedx        ;;ballx position = ballx - ballspeedx
;   STA ballx
;
;   LDA ballx
;   CMP #LEFTWALL
;   BCS MoveBallLeftDone      ;;if ball x > left wall, still on screen, skip next section
;   LDA #$01
;   STA ballright
;   LDA #$00
;   STA ballleft         ;;bounce, ball now moving right

;  JSR IncrementScore
  ;;in real game, give point to player 2, reset ball
; MoveBallLeftDone:
;
; MoveBallUp:
;   LDA ballup
;   BEQ MoveBallUpDone
;
;   LDA bally
;   SEC
;   SBC ballspeedy
;   STA bally
;
;   LDA bally
;   CMP #TOPWALL
;   BCS MoveBallUpDone
;   LDA #$01
;   STA balldown
;   LDA #$00
;   STA ballup
; MoveBallUpDone:
;
; MoveBallDown:
;   LDA balldown
;   BEQ MoveBallDownDone
;
;   LDA bally
;   CLC
;   ADC ballspeedy
;   STA bally
;
;   LDA bally
;   CMP #BOTTOMWALL
;   BCC MoveBallDownDone
;   LDA #$00
;   STA balldown
;   LDA #$01
;   STA ballup
; MoveBallDownDone:
;
; MovePlayerUp:
;   LDA buttons1
;   AND #%00001000
;   BEQ MovePlayerUpDone
;   LDA playery
;   SEC
;   SBC #$01
;   STA playery
; MovePlayerUpDone:
;
; MovePlayerDown:
;   LDA buttons1
;   AND #%00000100
;   BEQ MovePlayerDownDone
;   LDA playery
;   CLC
;   ADC #$01
;   STA playery
; MovePlayerDownDone:
;
; MovePlayerLeft:
;   LDA buttons1
;   AND #%00000010
;   BEQ MovePlayerLeftDone
;   LDA playerx
;   SEC
;   SBC #$01
;   STA playerx
; MovePlayerLeftDone:
;
; MovePlayerRight:
;   LDA buttons1
;   AND #%00000001
;   BEQ MovePlayerRightDone
;   LDA playerx
;   CLC
;   ADC #$01
;   STA playerx
; MovePlayerRightDone:
;   JSR UpdatePlayerSprite
;   JSR UpdateBallSprite
;   JMP GameEngineDone

; SUBROUTINES

vblankwait:
  BIT $2002
  BPL vblankwait
  RTS

EnableRendering:
  ; #%VPHBSINN
  ; V = NMI Enable, P = PPU Master/Slave, H = Sprite Height (8x8 or 8x16), B = Background Tile Select
  ; S = Sprite Tile Select, I = Increment Mode, NN = Nametable Select
  LDA #%10010000        ; Enable NMI, Use Background from Pattern Table 1, Use Sprites from Pattern Table 0, Use Nametable 0
  STA $2000             ; Send settings to PPUCTRL

  ; #%BGRsbMmG
  ; B = Blue Emphasis, G = Green Emphasis, R = Red Emphasis, s = Sprite Enable
  ; b = Background Enable, M = Sprite Left Column Enable, m = Background Left Column Enable, G = Greyscale Mode
  LDA #%00011110        ; Enable Sprites, Enable Background, Left Side of Screen is Visible
  STA $2001             ; Send settings to PPUMASK

  LDA #$00
  STA $2005             ; Write the Scroll X Position
  STA $2005             ; Write the Scroll Y Position

  RTS

EnableBackgroundOnly:
  ; #%BGRsbMmG
  ; B = Blue Emphasis, G = Green Emphasis, R = Red Emphasis, s = Sprite Enable
  ; b = Background Enable, M = Sprite Left Column Enable, m = Background Left Column Enable, G = Greyscale Mode
  LDA #%00001010        ; Enable Sprites, Enable Background, Left Side of Screen is Visible
  STA $2001             ; Send settings to PPUMASK

DisableRendering:
  ; #%BGRsbMmG
  ; B = Blue Emphasis, G = Green Emphasis, R = Red Emphasis, s = Sprite Enable
  ; b = Background Enable, M = Sprite Left Column Enable, m = Background Left Column Enable, G = Greyscale Mode
  LDA #%00000000        ; Disable Sprites, Enable Background, Left Side of Screen is Visible
  STA $2001             ; Send settings to PPUMASK

  RTS

BackgroundPPULatch:
  LDA $2002             ; Load the PPU status register to reset the PPU latch

  LDA #$20
  STA $2006             ; Write the high byte of the PPU Background Nametable 0 Address
  LDA #$00
  STA $2006             ; Write the low byte of the PPU Background Nametable 0 Address
                        ; $2000 - $23FF = Background Nametable 0 (960 Bytes of Tiles + 64 Bytes of Attributes)
                        ; $2400 - $27FF = Background Nametable 1
                        ; $2800 - $2BFF = Background Nametable 2
                        ; $2C00 - $2FFF = Background Nametable 3
  RTS

BackgroundSwitch:
NEXTBYTE:
  LDA [bgPointerLo], Y  ; Load the 16-bit bgPointer for the current Background Nametable index
  STA $2007             ; Send the current byte to the PPU
  INY
  BNE NEXTBYTE
NEXTPAGE:
  INC bgPointerHi       ; Increment the high byte of the 16-bit background pointer to move to the next page
  DEX
  BMI DONE              ; If we've cleared all pages, exit the subroutine
  BNE NEXTBYTE          ; If we haven't reached the final page, start the next page
  LDY #$00              ; Reset the byte counter for the final page
  BNE NEXTBYTE          ; If we're on the final page, keep looping through the remaining bytes
DONE:
  RTS

; ReadController1:
;   LDA #$01
;   STA $4016             ; Send the high signal for the controller latch
;   LDA #$00
;   STA $4016             ; Send the low signal for the controller latch, start reading both controllers
;   LDX #$08              ; Set the X Register Counter for 8 Buttons
; ReadController1Loop:
;   LDA $4016             ; Load each button from the Controller 1 I/O Register
;   LSR A                 ; Shift the button data to the Carry Flag
;   ROL buttons1          ; Rotate the button data from the Carry Flag into the buttons1 variable
;   DEX                   ; Continue to the next button
;   BNE ReadController1Loop
;   RTS

; ReadController2:
;   LDX #$08              ; Latch has already been started by the ReadController1 subroutine
; ReadController2Loop:
;   LDA $4017             ; Load each button from the Controller 2 I/O Register
;   LSR A                 ; Shift the button data to the Carry Flag
;   ROL buttons2          ; Rotate the button data from the Carry Flag into the buttons1 variable
;   DEX                   ; Continue to the next button
;   BNE ReadController2Loop
;   RTS

; UpdatePlayerSprite:
;   ; Update Sprite Y position
;   LDA playery
;   STA $0204
;   STA $0208
;   CLC
;   ADC #$08
;   STA $020C
;   STA $0210
;
;   ; Update Sprite Tile
;   LDA #$32
;   STA $0205
;   CLC
;   ADC #$01
;   STA $0209
;   ADC #$01
;   STA $020D
;   ADC #$01
;   STA $0211
;
;   ; Update Sprite Palette
;   LDA #$00
;   STA $0206
;   STA $020A
;   STA $020E
;   STA $0212

  ; Update Sprite X position
  ; LDA playerx
  ; STA $0207
  ; STA $020F
  ; CLC
  ; ADC #$08
  ; STA $020B
  ; STA $0213
  ;
  ; RTS

; UpdateBallSprite:
;   LDA bally
;   STA $0200
;   LDA #$75
;   STA $0201
;   LDA #$03
;   STA $0202
;   LDA ballx
;   STA $0203
;   RTS

; IncrementScore:
; IncOnes:
;   LDA scoreOnes      ; load the lowest digit of the number
;   CLC
;   ADC #$01           ; add one
;   STA scoreOnes
;   CMP #$0A           ; check if it overflowed, now equals 10
;   BNE IncDone        ; if there was no overflow, all done
; IncTens:
;   LDA #$00
;   STA scoreOnes      ; wrap digit to 0
;   LDA scoreTens      ; load the next digit
;   CLC
;   ADC #$01           ; add one, the carry from previous digit
;   STA scoreTens
;   CMP #$0A           ; check if it overflowed, now equals 10
;   BNE IncDone        ; if there was no overflow, all done
; IncHundreds:
;   LDA #$00
;   STA scoreTens      ; wrap digit to 0
;   LDA scoreHundreds  ; load the next digit
;   CLC
;   ADC #$01           ; add one, the carry from previous digit
;   STA scoreHundreds
; IncDone:
;   RTS
;
; DrawScore:
;   LDA #$20
;   STA $2006             ; Write the high byte of the PPU Background Nametable 0 Address
;   LDA #$30
;   STA $2006             ; Write the low byte of the PPU Background Nametable 0 Address
;
;   LDA scoreHundreds
;   STA $2007
;   LDA scoreTens
;   STA $2007
;   LDA scoreOnes
;   STA $2007
;
;   RTS

; BANK 1 = Second 8KB of PRG ROM, holding the graphic tile data and the interrupt vectors

  .bank 1
  .org $E000             ; Align the Background Nametable so that the low byte of the Table Address is $00

; Each table uses .db for "Define constant bytes" to store a series of constants in memory, preceded by a label

; The Background Nametable is a single screen, 32 Tiles wide and 30 Tiles high, for a total of 960 Bytes
; Each byte is a hex tile number corresponding to the CHR-ROM background tile data
backgroundPlaying:
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
backgroundPlayingAttributes:
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100
  .db %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100, %11100100

backgroundTitle:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 1
  ;;Above this is not seen on NTSC screen
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$37,$38,$39,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 3
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$48,$49,$24,$30,$31,$32,$33,$34,$35,$36,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$57,$58,$59,$24,$40,$41,$42,$43,$44,$45,$46,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 5
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$67,$68,$69,$24,$50,$51,$52,$53,$54,$55,$56,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 6
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$77,$78,$79,$24,$60,$61,$62,$63,$64,$65,$66,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 7
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$70,$71,$72,$73,$74,$75,$76,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 8
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$80,$81,$82,$83,$84,$85,$86,$87,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 9
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$90,$91,$92,$93,$94,$95,$96,$37,$38,$39,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 10
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$A0,$A1,$A2,$A3,$A4,$A5,$A6,$47,$48,$49,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 11
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$57,$58,$59,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 12
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$67,$68,$69,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 13
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$77,$78,$79,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 14
  .db $24,$24,$24,$24,$24,$24,$24,$19,$0E,$1D,$1D,$22,$24,$1D,$11,$0E,$0f,$1D,$24,$14,$0E,$1F,$12,$17,$24,$D5,$24,$24,$24,$24,$24,$24 ; Row 15
  .db $24,$24,$24,$24,$24,$24,$24,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 16
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$0A,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 17
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 18
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$10,$0A,$16,$0E,$24,$18,$0F,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 19
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 20
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$1B,$18,$0C,$14,$24,$1D,$11,$12,$0E,$1F,$0E,$1B,$22,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 21
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 22
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 23
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 25
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$BE,$C0,$B4,$C1,$C1,$24,$C1,$C2,$B0,$C0,$C2,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 26
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 27
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 28
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 29
  ;;Below this is not seen on NTSC screen
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$56,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 30

backgroundTitlenostart:
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 1
    ;;Above this is not seen on NTSC screen
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 2
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$37,$38,$39,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 3
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$47,$48,$49,$24,$30,$31,$32,$33,$34,$35,$36,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 4
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$57,$58,$59,$24,$40,$41,$42,$43,$44,$45,$46,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 5
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$67,$68,$69,$24,$50,$51,$52,$53,$54,$55,$56,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 6
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$77,$78,$79,$24,$60,$61,$62,$63,$64,$65,$66,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 7
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$70,$71,$72,$73,$74,$75,$76,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 8
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$80,$81,$82,$83,$84,$85,$86,$87,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 9
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$90,$91,$92,$93,$94,$95,$96,$37,$38,$39,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 10
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$A0,$A1,$A2,$A3,$A4,$A5,$A6,$47,$48,$49,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 11
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$57,$58,$59,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 12
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$67,$68,$69,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 13
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$77,$78,$79,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 14
    .db $24,$24,$24,$24,$24,$24,$24,$19,$0E,$1D,$1D,$22,$24,$1D,$11,$0E,$0f,$1D,$24,$14,$0E,$1F,$12,$17,$24,$D5,$24,$24,$24,$24,$24,$24 ; Row 15
    .db $24,$24,$24,$24,$24,$24,$24,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 16
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$0A,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 17
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 18
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$10,$0A,$16,$0E,$24,$18,$0F,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 19
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 20
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$1B,$18,$0C,$14,$24,$1D,$11,$12,$0E,$1F,$0E,$1B,$22,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 21
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 22
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 23
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 24
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 25
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 26
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 27
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 28
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 29
    ;;Below this is not seen on NTSC screen
    .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$56,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 ; Row 30
backgroundTitleAttributes:
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

palette:
      ;Blue             Fuschia           Orange          Green
  .db $20,$17,$3F,$07, $20,$17,$3F,$07, $20,$17,$3F,$07, $20,$17,$3F,$07  ; Background Palettes
  .db $20,$17,$3F,$07, $20,$17,$3F,$07, $20,$17,$3F,$07, $20,$17,$3F,$07  ; Sprite Palettes

  ; Write the Interrupt Vectors starting at $FFFA
    ; Each .dw (Data Word) is 2 Bytes long, and references a label to jump to in the code when the interrupt occurs
  ; $FFFA-$FFFB = NMI vector
  ; $FFFC-$FFFD = Reset vector
  ; $FFFE-$FFFF = IRQ/BRK vector
  .org $FFFA
  .dw NMI               ; Non-Maskable Interrupt, happens at start of each frame
  .dw RESET             ; Occurs at first boot or reset
  .dw 0                 ; External IRQ, not currently used

; BANK 2 = 8KB of CHR ROM

  .bank 2
  .org $0000            ; CHR ROM has separate memory from above PRG ROM, stored in PPU
                        ; $0000 - $0FFF = Pattern Table 0 (4KB Sprite Tiles)
                        ; $1000 - $1FFF = Pattern Table 1 (4KB Background Tiles)
  .incbin "mario.chr"   ; include external 8KB tile set
