;100 doors algorithmin z80 assembly language
;created by hurray banana Feb 2021
;0 is door closed 1 is door open
;tab size=8
;PAC-MAN Hardware programming
;developed using SJASM assembler and Visual Studio 2015
	device zxspectrum48				;allows savebin to work for creating the binaries - at the end of the file
							;instead of using OUTPUT before each org - just slightly easier to manage

;TODO rise and fall code for process top message code
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants defined here
; in general when declaring constants and symbolic addresses use a colon at end
; when using constants and symbolic addresses drop the colon, this allows you to search for declarations
; by putting the colon at the end of the symbol name, if you omitt it you will find all the references (which might be loads)
; labels starting with . are local labels and can be re-used in other modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NOT_WANTED		equ 0				;directive constant to mark blocks as don't assemble

RST_38H:		equ $ff				;constant for RST 38H instruction for setting vsync interrupt
irq_enable:		equ $5000			;irq enable location in memory map, set 1 to enable 0 to disable
watchdog:		equ $50c0			;watchdog - if not reset every few frames cpu resets - forcing a re-boot known as the watchdog barking
flip_screen_addr:	equ $5003			;if value 0 then normal tilemap, 1 forces flipped tilemap (does not affect sprites)
hardware_io_area:	equ $5000			;hardware mapped memory i/o base

tile_ram_play_area:	equ $4040			;main tilemap area starts here (top right)-  addr increase vertically down from top - top right tile
tile_ram_play_area_left:equ $43a0			;main tilemap area top left tile
tile_ram_top_area:	equ $43c0			;top two rows above tilemap area- addr increase horizontally left from right 
tile_ram_play_area_last:equ $43bf			;last address of main player area (bottom left corner)
tile_ram_top_vis:	equ $43c2			;first two and last two tiles not visible in each of 2 rows
tile_ram_top_vis_line1:	equ $43dd			;top left of visible top row sub to move to next column to right
tile_ram_top_vis_line2:	equ $43fd			;second text row at top
tile_ram_start:		equ $4000			;base address of tile ram
pallete_ram_start:	equ $4400			;base address of pallete ram
pallete_ram_main:	equ $4440			;main video area pallete start (after first two rows)
tile_ram_bot_area:	equ $4000			;bottom two rows below tilemap area- addr increase horizontally left from right
tile_ram_bot_vis:	equ $4002			;first two and last two tiles not visible in each of 2 rows
tile_ram_bot_vis_line1:	equ $401d			;top left of visible bottom row sub to move to next column to right
tile_ram_bot_vis_line2:	equ $403d			;top left of last border row
user_ram_start:		equ $4c00			;start of user allocated RAM
ram_top:		equ $4ff0			;this is where the stack needs to start at
sprite_reg:		equ $4ff2			;sprite registers start here first byte is number bits 7-2, bit 1 is x flip, bit 0 is y flip
							;second byte lower nibble is pallete to use for sprite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; colour pallete constants for pen 3 colour
; these are colours based on pen 3
; pen 1 is Red ink in my images ff0000
; pen 2 is Blue ink in my images 0000ff
; pen 3 is Green ink in my images 00ff00
; pen 0 - transparent (or black) is white ffffff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
black:			equ 0;		(hidden)
red:			equ 1;		(blinky)
pink:			equ 3;		(pinky)
cyan:			equ 5;		(inky)
orange:			equ 7;		(clyde)
yellow:			equ 9;		(pac-man)
grey:			equ $f;		(off-white)
blue:			equ $10;	(scared ghost)
brown:			equ $15;	(poo ghost)
pen1block		equ $21;	(whites of eyes)
pen2block		equ $22;	(blue pupils)
pen3block		equ $23;	(ghost/pac body)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; data to be held in ram defined here                                 ;
; allocation of bytes for ram area assembler calculates			      ;
; addresses based on org statement and user_ram_start ($4c00)		  ;
; these are in affect our variables									  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; db define byte (8 bit value) creates space and sets value
; dw define word (16 bit value) creates space and sets value
; ds declares space for a number of bytes (often specified using a calculation for the assembler)
; defm defines a byte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

			org user_ram_start		;allocate this data at user_ram_start $4c00
newdoors:		ds 101				;100 doors - remember to load doors -1 to start so we move to the first door with a 1, ensured its page aligned

waitvsync:		db 0				;if 1 then interrupt is finished and main loop can continue 
frame_counter:		db 255				;value that increments every frame for stable animation and frame counts - doesn't reset just wraps so mods work consistently
frame_number:		db 0				;holds a frame number in lsB and second counter in msB 60 to 0 resets to 60 when gets to zero 
seconds_counter:	db 0				;quick ref to seconds part
full_tile:		db 0				;tile to use by bitmap renderer
save_reg:		db 0				;temp storage for an 8 bit register
save_sp:		dw 0				;storage area for stack when messing about with it
joystick_state:		db 0				;holds previous state in upper nibble and current state in lower nibble  IN1 $5000
start_state:		db 0				;holds current state of input byte IN1 - player 2 joystick and start buttons $5040
flip_screen:		db 0				;holds status of flip screen (as i think the location $5003 is write only - it always reads back as ff)
msg_one_pos:		dw 0				;start addr of top text
msg_bot_addr:		dw 0				;holds index of in bottom_list text to display at bottom of screen
bot_counter:		db 0				;counter for bottom scrolling message display
msg_top_addr		ds 2
mode:			db 0				;what mode is the system in 1 - title screen, 2 is game, 4 is end of game
game_time:		db 0				;holds the game time
flipx:			equ 1				;x flip bit for sprites
flipy:			equ 0				;y flip bit for sprites

display_base:		ds 1				;value for display jumpt table 0 means denary display, 2 means hex
colour_cycle_count	ds 1				;seconds counter for colour cycling
ghost_colour		ds 1				;pallete for ghost
speed:			db 1				;frame interval between interations - need to think about this and make it iteration steps instead - gets complicated
stepcount		ds 1				;how many frames have we counted so far
doorstep:		ds 2				;single byte as doors are page alinged so only L needs to change (must set to 255 before a loop starts to be processed)
door_running:		equ doorstep			;use value in e to check running state of algorithm
ascii_slide		ds 1				;holds the offset to the correct character set (0,1,2,3) which displays sinking version of the ascii sets 0,$30,$60,$90
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Code starts here
; when Z80 becomes active the PC will hold address 0
; address zero points to the first byte in ROM
; we place a jump instruction here so we can execute our code
; that will initialise the hardware and setup initial data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
        	org  0					;tell assembler to assemble from address 0
		jp start_initialise			;jump to initialisation code

		org $38					;assemble from $38 (where the vblank interrupt will jump to)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vertical blank interrupt routine
; interrupt generated when screen has finished drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
vblank:		di					;disable any interrupts (so we don't get interrupted)
		push	af				;save accumulator, flags and registers we are going to destroy
		push	ix						 
		push	hl						
		push	de						
		push	bc						
		
		xor	a				;clear accumulator
		ld	(irq_enable),a			;disable ints on the board
		ld	(watchdog),a			;reset the watchdog
		
		;increment frame counter			
		ld a,(frame_counter)			;get current value
		inc a					;increment 
		ld (frame_counter),a			;and store

		;sort out frame counter
		ld hl,(frame_number)			;get 16 bit frame counter - l holds frame, h holds seconds
		dec l					;reduce number of frames by 1
		jp p,dont_reset_frame_counter		;only reset if it goes negative

		ld l,$3b				;load back up with 59
		inc h					;add one to seconds

dont_reset_frame_counter:
		ld (frame_number),hl			;store new counter

		; main vblank routines
		call input_manager			;update input states
		call scrolling_message			;top line of scrolling text	
vbend:		
		xor a					;clear a
		ld	(watchdog),a			;and reset the watchdog
		inc a					;enable interrupts again
		ld (irq_enable),a			;enable ints on the board
		pop	bc				;restore registers in reverse order of push
		pop	de
		pop	hl
		pop	ix
		pop	af
		ei					;enable interrupts again
		reti					;return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reset frame number to start second counting from 0 frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
reset_framenumber:
		ld a,$39				;set up frame number for 0 so seconds are reset
		ld (frame_number),a			;reset frame number
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; input manager packages the input data from the two io ports
; so it can be used to track button/joystick state in a nice format
;
; keeps stack of current and previous state of inputs
; so we can test whether they are just pressed, held or released
; input manager constants 0 is down 1 is up
; mask status byte first using these
; for joystick_state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
inp_chk_up:		equ $11				;mask for looking at up (bits 4 and 0)
inp_chk_left:		equ $22				;mask for looking at left (bits 5 and 1)
inp_chk_right:		equ $44				;mask for looking at right (bits 6 and 2)
inp_chk_down:		equ $88				;mask for looking at down (bits 7 and 3)
;for start_state
inp_chk_p1st:		equ $50				;mask for looking at p1 start (bits 6 and 4)
inp_chk_p2st:		equ $a0				;mask for looking at p2 start (bits 7 and 5)

;comparison constants for player 1 and 2 start buttons
inp_p1_presd:		equ $40				;check for p1 start pressed bit 6 previous 1 -up bit 4 current down 0 )
inp_p2_presd:		equ $80				;check for p2 start pressed

inp_p1_relsd:		equ $10				;check for p1 start released bit 6 previous 0 -down bit 4 current up 1 )
inp_p2_relsd:		equ $20				;check for p2 start released

;comparison constants to check wether just pressed or released
inp_up_presd:		equ $10				;check for up pressed
inp_left_presd:		equ $20				;check for left pressed
inp_right_presd:	equ $40				;check for right pressed
inp_down_presd:		equ $80				;check for down pressed

inp_up_relsd:		equ $01				;check for up released
inp_left_relsd:		equ $02				;check for left released
inp_right_relsd:	equ $04				;check for right released
inp_down_relsd:		equ $08				;check for down released

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; manages processing of previous and current states, for pressed and released behaviour checking
; as well as allowing standard up and down checking
; gets new state and packs this into a byte with the previous state in the upper nibble
; uses a byte for joystick direction and a separate byte for the start buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
input_manager:
		;bit 0 - 3 up,left,right,down
		ld a,(joystick_state)			;get joystick state
		add a,a
		add a,a
		add a,a
		add a,a					;shift lower nibble to upper nibble to move to previous state location
		and $f0					;clear lower nibble
		ld b,a					;store temporarily
		ld a,($5000)				;get IN0
		and $0f					;clear upper nibble
		or b					;combine current state with previous state PPPP CCCC
		ld (joystick_state),a			;and store new previous and current state

		;bit 5 is 1p start, bit 6 is 2pstart
		;do start button states - PPCC xxxx upper hi bits of upper nibble contain previous state, low bits of upper nibble contain current state
		ld a,(start_state)			;get start button state
		add a,a
		add a,a					;shift current two bits to previous position
		and $c0					;clear lower 2 bits of upper nibble - and the lower nibble
		ld b,a					;store temporaily
		ld a,($5040)				;get IN1
		rra					;shift two bits to low bits of upper nibble
		and $30					;clear all bits other than 4 and 5
		or b					;combine previous with current PP CC XXXX
		ld (start_state),a			;store previous and new states
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; THIS IS THE ENTRY POINT FROM THE JUMP AT ADDRESS 0;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sets up z80 interrupt mode and clears all RAM		;
; then initialises code to display screen			;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
start_initialise:
		di					;disable ints on the CPU

		ld	sp,ram_top			;set up a new stack at the top of ram
		im	1				;set interrupt mode 1
		ld	a,RST_38H			;$ff is RST 38H instruction opcode
		out	(0),a				;set up the interrupt vector through port 0

	 	xor	a				;clear accumulator quickly 4 t-states rather than lda a,0 which is 7
	 	ld	(watchdog),a			;reset the watchdog
	
 	   	ld	hl,user_ram_start		;addr to copy from
 	   	ld	de,user_ram_start + 1		;addr to copy to
 	   	ld	bc,$3ef				;gonna loop this many times (size of RAM 1007 bytes)
 	   	ld	(hl),a				;clear (hl), which then gets copied through to de
 	   	ldir					;copy from (hl) to (de) and increment hl and de, decrementing bc, keep doing until bc = 0
 	   
		ld  a,$0f				;tile number of empty tile
 	   	ld	hl,tile_ram_start		;clear video ram
 	   	ld	de,tile_ram_start + 1
 	   	ld	bc,$3ff				;1k of tile ram
 	   	ld	(hl),a				;clear (hl), which then gets copied through to de
 	   	ldir					;copy from hl to de, decrementing bc, keep doing until bc = 0
 	   	
		ld  a,$7				;pallete 1
 	   	ld	hl,pallete_ram_start		;set all colour ram to palette 1
 	   	ld	de,pallete_ram_start + 1
 	   	ld	bc,$3ff				;1k of palette ram
 	   	ld	(hl),a				;clear (hl), which then gets copied through to de
 	   	ldir					;copy from hl to de, decrementing bc, keep doing until bc = 0

 	   	xor a					;quickly clear accumulator
 	   	ld	hl,sprite_reg			;clear sprite ram
 	   	ld	b,16				;16 bytes need clearing
clear_spr_ram:	ld	(hl),a				;store (zero)
		inc	hl				;move to next address
 	   	djnz clear_spr_ram			;decrement b and branch if not zero yet
 	   	
		ld	hl,hardware_io_area		;clear hardware mapped memory from $5000 to $5007
		ld	b,8				;do 8 bytes worth
clear_loop1:	ld	(hl),a				;store a at location
		inc	hl				;increment to next location
		djnz clear_loop1			;loop until b is zero

		xor a
		ld (speed),a				;set initial speed

							;setup scrolling message in init as we want it to continue unaffected when changing modes
		ld hl,top_mess				;store start position for top scrolling text
		ld (msg_one_pos),hl			;address of first character

		ld a,1
		ld (ascii_slide),a			;force offset to be $30 - slightly sunk

		xor	a				;clear a
		ld	(watchdog),a			;reset the watchdog, to stop it barking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end of hardware initialisation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; enable interrupts and start main loop
		ld a,1
		ld (irq_enable),a			;enable ints on the board by writing 1 to this address
		ei					;enable ints on the CPU

		jp start_title_mode			;setup title screen

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; logic to be performed every frame - main logic of code
; needs to check to see what mode game is in first so it can run
; the correct game loop function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
title_mode:		equ $1
game_mode:		equ $2

mainloop:		
		ld a,(mode)				;get mode
		cp title_mode				;is it title mode
		jp z,title_logic			;yes
		cp game_mode				;is it game mode
		jp z,game_logic				;yes
wait:
		halt					;finished processing let's wait for vsync
		jp mainloop				;perform logic again

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sets up title screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
start_title_mode:
		ld a,title_mode				;pick title mode
		ld (mode),a				;and store

		;setup scrolling message - not here as we want this to not reset between modes 
		;ld hl,top_mess				;store start position for top scrolling text
		;ld (msg_one_pos),hl			;address of first character

		ld hl,tile_ram_top_vis_line1		;address of top left row of top border
		set 2,h					;force to pallete ram
		ld a,grey				;colour pallete for white (pen 3)
		ld b,$1c				;number of tiles to fill (28 entire columns)
		call fill_border_area_row		;set colour for entire top row

		ld hl,tile_ram_top_vis_line2		;address of top left row of top border
		set 2,h					;force to pallete ram
		ld a,cyan				;colour pallete
		ld b,$1c				;number of tiles to fill (28 entire columns)
		call fill_border_area_row		;set colour for entire top row
		
		ld hl,tile_ram_top_vis_line2		;address of top left row of top border
		ld a,$20				;empty tile for now
		ld b,$1c				;number of tiles to fill (28 entire columns)
		call fill_border_area_row		;set colour for entire top row

		;setup bottom changing message
		ld hl,bottom_list			;point to last item in the list for bottom text drawing routine so we cycle round straight away
		ld (msg_bot_addr),hl			;store in lookup
		ld hl,tile_ram_bot_vis_line1		;address of top left row of top border
		set 2,h					;force to pallete ram
		ld a,yellow				;colour pallete for white (pen 3)
		ld b,$1c				;number of tiles to fill (28 entire columns)
		call fill_border_area_row		;set colour for entire top row

		;clear any data on second to last line
		ld de,msg_blank				;store text addr for instruction message
		ld hl,tile_ram_bot_vis_line1		;vram address for first of line of tiles
		call borders_blit_text_only		;write text to border

		;clear any data on last line
		ld de,msg_blank				;store text addr for instruction message
		ld hl,tile_ram_bot_vis_line2		;vram address for first of line of tiles
		call borders_blit_text_only		;write text to border

		;fill pallete ram for main screen area
		ld a,blue				;blue pallete for screen
		call fill_screen_area_colour		;fill entire screen with palette 

		call draw_bitmap_for_title		;show bitmap

		;set colour bands for bitmap
		ld hl,$47a0				;get palette fill start point
		ld b,13					;width
		ld c,6					;height
		ld a,red
		call tileram_rectangle
		ld hl,$4706				;get palette fill start point
		ld b,23					;width
		ld c,6					;height
		ld a,cyan
		call tileram_rectangle
		ld hl,$47ba				;get palette fill start point
		ld b,28					;width
		ld c,6					;height
		ld a,yellow
		call tileram_rectangle

		jp wait					;finished setup goto wait section

draw_bitmap_for_title:
		ld a,(display_base)			;what number base are we currently using
		and a
		jr z,decfortitle			;its denary
		ld hl,title_screen_column_data_hex	;get hex version of bitmap
		jr now_draw_title
decfortitle:	ld hl,title_screen_column_data		;load hl with address of title screen data
now_draw_title:	ld a,$40				;full block	
		jp draw_pixel_map_to_screen		;call code to render title screen data

;**************************************************
; sets up initial values for a clean run at the doors
; picks up step, mask and start of door array
; this could be improved performance wise
; by just incrementing l (as doors array is page aligned)
; but gets messy because of ALU and A favouritism
; so leaveing as 16 bit addition - speed is good enough
;**************************************************
initialise_doors:
		xor a					;clear a register
		ld hl,newdoors;doors			;point hl at addr of 2nd element of doors array
		ld (hl),a				;clear byte 
		ld de,newdoors+1			;point de at addr of first element of doors array
		ld bc,100				;counter for ldir
		ldir					;copy value at addr in hl to addr in de, then inc both hl and de
							;decrement bc at repeat until bc = 0
		inc a
		ld (doorstep),a				;store 1 in step
		ret

;**************************************************
; start of single loop entry
; picks up step, mask and start of door array
;**************************************************
door_mask:	equ 1
do_door_run:	ld a,(doorstep)				;get step
		cp 101
		ret z					;if 101 then no runs left
		ld e,a					;place in e
		xor a					
		ld c,door_mask				;set Toggle mask
		ld hl,newdoors				;load start of doors array (we always skip first byte because step starts at 1 ignore 1st byte)
    
nextdoor:	ld a,l					;instead of add hl,de
		add e					;as door data is page aligned so H remains constant
		ld l,a

		ld a,100
		sub l					;check we have a legal door position			
		jr c,nextrun				;bigger than 100 so gone off end
		
		ld a,c					;load mask
		xor (hl)				;toggle door
		ld (hl),a				;write door back
		
		jr nextdoor				;move onto to next door
    
nextrun:	inc e					;increment step value
		ld a,e					;copy to a as only accumulator can be written to RAM
		ld (doorstep),a				;write new step value back ready for next run
		ret	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; performs logic for title mode
; DOORS: put title screen here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
title_logic:
		call check_base_title			;see if they pushed up
		ld a,(start_state)			;get state
		and inp_chk_p1st			;examine p1 start
		cp inp_p1_presd				;check for pressed 
		jp z,start_game				;0 means pressed so start game

		ld c,3					;3 seconds delay
		call animate_ghost_mouth		;attempt to do animation of mouth
		
		;call process_top_message_text		;top line of changing text
		call bot_display_upd
		jp wait					;finished setup goto wait section

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; check for up to change base display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
check_base_title:
		ld a,(joystick_state)			;get joystick state
		and inp_chk_up				;examine up
		cp inp_up_presd				;check if pressed
		ret nz					;it's non zero so isn't pressed
		ld a,(display_base)			;get current base
		xor 1					;toggle bit 0
		ld (display_base),a			;write bit
		jp draw_bitmap_for_title		;update column display


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Animates the mouth of the ghost
; c holds number of seconds before animation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ghost_mask:		equ $60				;toggle tiles for animation between $40 and $20
ghost_top_mouth:	equ $41d5									
ghost_bot_mouth:	equ $41d6
ghost_mouth_len:	equ 12
animate_ghost_mouth:
		;Animate ghost mouth
		ld a,(frame_number)			;get frame number
		and a					;see if it is zero
		ret nz					;only animate every second
		ld a,(seconds_counter)			;check seconds elapsed
		and c
		ret nz			
		
		ld c,ghost_mask
		ld hl,ghost_top_mouth
		ld b,ghost_mouth_len
		call xor_screen_area_row

		ld hl,ghost_bot_mouth
		ld b,ghost_mouth_len
		call xor_screen_area_row		;mask still ok in c

		;call animate_ghost_skirt		;for testing
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Animates the skirt of the ghost
; c holds number of seconds before animation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mk:			equ ghost_mask
ghost_skirt_xor_top:	db	0,0,mk,0,mk,0,mk,mk,0,mk,0,mk,255
ghost_skirt_xor_bot:	db	mk,mk,mk,0,mk,mk,mk,mk,mk,mk,0,mk,mk,mk,255
ghost_top_skirt:	equ $41f8			;2 in from edge (5 xor blocks) 2[1], 4[1], 6[2], 9[1], 11[1]									
ghost_bot_skirt:	equ $41f9			;starts at edge (3 xor blocks) 0[3], 5[6], 12[3] 

animate_ghost_skirt:
		ld a,(frame_number)			;get frame number
		and c					;see if it is zero
		ret nz					;only animate every second

		ld de,ghost_skirt_xor_bot
		ld hl,ghost_bot_skirt
		call main_xor_blit_text_only;

		ld de,ghost_skirt_xor_top
		ld hl,ghost_top_skirt
		call main_xor_blit_text_only;
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sets up game screen
; clears screen 
; DOORS: this is where the initialisation code for
; a new clear set of doors needs to go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
start_game:

		xor a
		ld (stepcount),a			;reset step count

		ld a,game_mode				;pick game mode
		ld (mode),a				;and store

		ld a,red
		
		;blue pallete for screen
		call fill_screen_area_colour		;fill entire screen with palette 16

		ld a,1					;set initial ghost colour for game screen (blinky)
		ld (ghost_colour),a				
		call cycle_ghost_colour_immediate	;don't wait for timer or cycle
		xor a
		ld (colour_cycle_count),a		;reset timer

		ld hl,game_screen_column_data		;load hl with address of title screen data
		ld a,$40				;solid block
		call draw_pixel_map_to_screen		;call code to render title screen data

		call draw_ghost_eyes			;draw ghost eyes in large format

		;clear old messages
		ld de,msg_blank				;store text addr for instruction message
		ld hl,tile_ram_top_vis_line2		;vram address for 2nd line of tiles
		call borders_blit_text_only		;write text to border
		ld de,msg_blank				;store text addr for instruction message
		ld hl,tile_ram_bot_vis_line1		;vram address for first of bottom line of tiles
		call borders_blit_text_only		;write text to border


		;show debug text
		ld de,msg_debug				;store text addr for instruction message
		ld hl,tile_ram_bot_vis_line1		;vram address for first of bottom line of tiles
		ld c,cyan				;colour (yellow)
		call borders_blit_text			;write text to border

		;set static instruction message on bottom line
		ld de,msg_quit_game			;store text addr for instruction message
		ld hl,tile_ram_bot_vis_line2		;vram address for first of bottom line of tiles
		ld c,yellow				;colour (yellow)
		call borders_blit_text			;write text to border


		ld a,$20				;set game counter to 20 (in bcd)
		ld (game_time),a			;and store

		call reset_framenumber			;set frame number to 0 so we start at a whole second

		call draw_hurray_banana_text

		call initialise_doors			;run the raw code routine
		call set_door_palletes			;set colour for each alternate line
		call render_doors			;show basic door structure
		call render_door_labels			;render labels for doors

		jp wait					;finished setup goto wait section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; renders the incrementing palettes for each row of doors
; in the 10x10 door grid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
set_door_palletes:
		ld c,10					;10 rows
		ld de,-32				;value to move right one tile column (32 high)
		ld hl,door_toprow:			;hl points at door tile ram
		set 2,h					;move to palette ram
		ld a,1					;start with pallete 1
next_col_row:			
		ld b,10					;ten columns per row
		push hl					;save current position
.row_copy:
		ld (hl),a				;set tile
		add hl,de				;next column to the right
		djnz .row_copy				;dec b and do another column if one left

		pop hl					;retrieve start of last row
		inc hl					;move down a row
		add 2					;move to next palette
		cp 11					;check for palette 11
		jr nz,pal_ok				
		ld a,21					;skip to pallete 21
pal_ok:
		dec c					;count off rows
		jr nz,next_col_row			;do another if we have one left

		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; renders the doors array to rows on the system
; 10x10 door grid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
door_toprow:		equ $436e
render_doors:
		ld c,10					;10 rows
		ld de,-32				;value to move right one tile column (32 high)
		ld ix,newdoors+1			;ix points at door list (skipping 0 item)
		ld hl,door_toprow:			;hl points at door tile ram
next_row_copy:	
		ld b,10					;ten columns per row
		push hl					;save current position
row_copy:
		ld a,(ix+0)				;get door
		ld (hl),a				;set tile
		inc ix					;next door
		add hl,de				;next column to the right
		djnz row_copy				;dec b and do another column if one left

		pop hl					;retrieve start of last row
		inc hl					;move down a row

		dec c					;count off rows
		jr nz,next_row_copy			;do another if we have one left

		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set the door number labels for rows and columns
; allowing people to reference the door information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
door_label_top_corner:	equ door_toprow - 1 + 32	;1 left and 1 up from door one (this is blank)
render_door_labels:
		ld hl,door_label_top_corner +32 + 1	;do rows first
		ld a,48					;start at 0 (ascii 48 - tiles set for ascii)
		ld b,10					;10 rows
label_rows:	ld (hl),a
		inc a
		inc hl					;move to next row
		djnz label_rows	

show_column_labels:					;this label for when denary hex display toggled
		ld hl,door_label_top_corner -32 - 1	;do columns step right one column (32 bytes)
		ld de,-32				;column skip value
		ld a,(display_base)
		add 9					;9 if denary (display_base == 0, write 10 after), 10 if hex (display_base == 1)
		ld b,a					;b is counter for djnz					
		ld a,49					;start at one (ascii 49 - tiles set for ascii)
label_columns:	ld (hl),a
		inc a
		add hl,de				;move right to next column
		djnz label_columns
		ld a,$20				;empty tile
		ld (hl),a				;blank the zero from denary
		
		ld a,(display_base)			;check again for denary
		and a
		ret nz					;if hex display then quit now
		
		ld a,49					;now put (49, 48 in ascii) 10 on final column for denary
		ld (hl),a
		add hl,de
		dec a
		ld (hl),a
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; performs logic for game mode
; DOORS: this is where the frame logic needs to go
; needs to launch game over when doors iteration finished
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
game_logic:
		call quit_to_title			;see if we want to get back to title screen
		call player_controls			;speed change requests
		call process_top_message_text		;top line of changing text

		ld c,$0f				;number of frames before animation
		call animate_ghost_skirt		;attempt to do animation of skirt

		ld hl,stepcount
		inc (hl)				;increase the framecounting

		ld a,(speed)				;get current speed value
		and a					;check for zero,as this is manual iteration mode (joystick up or down)
		jr z,nodoorloop
		sub (hl)				;have we reached speed value
		jr nz,nodoorloop

		xor a					;reset framecounter
		ld (hl),a

		ld a,(door_running)
		cp 101
		call nz,do_door_run			;if still runs to do, do them
		call render_doors
nodoorloop:
		ld c,3					;tell it to cycle every second
		call cycle_ghost_colour


		call debug_output			;some debug values for player positions
		jp wait					;finished setup goto wait section


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; draw tiles to put in the ghost eyes
; pen 1 is white of eyes pen1block
; pen 2 is blue eye pen2block
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic text blitting routine for normal areas of screen (28x32 main playfield)
; text strings MUST be zero terminated
; on entry
; de points to text string
; hl points to address in vram to print at
; can do this as we don't have mess about with pallette ram
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
eyewhite4:		db	pen1block,pen1block,pen1block,pen1block,pen3block,pen3block,pen1block,pen1block,pen1block,pen1block,0
eyepupil:		db	pen2block,pen2block,pen1block,pen1block,pen3block,pen3block,pen2block,pen2block,pen1block,pen1block,0
eyewhitepair:		db	pen3block,pen1block,pen1block,pen3block,pen3block,pen3block,pen3block,pen1block,pen1block,0
ghost_lt_eye:		equ $41cf			;left top of the ghost eye rectangle
ghost_eye_line1:	equ ghost_lt_eye + 0		;
ghost_eye_line2:	equ ghost_lt_eye + 1		;one row down
ghost_eye_line3:	equ ghost_lt_eye + 2		;one further row down
ghost_eye_line4:	equ ghost_lt_eye + 3		;one further row down
ghost_eye_line5:	equ ghost_lt_eye + 4		;one further row down

draw_ghost_eyes:
		ld hl,ghost_eye_line1
		ld de,eyewhitepair
		call main_blit_text_only

		ld hl,ghost_eye_line2
		ld de,eyepupil
		call main_blit_text_only

		ld hl,ghost_eye_line3
		ld de,eyepupil
		call main_blit_text_only

		ld hl,ghost_eye_line4
		ld de,eyewhite4
		call main_blit_text_only

		ld hl,ghost_eye_line5
		ld de,eyewhitepair
		call main_blit_text_only
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; checks to see if it's time to cycle the colour set for the ghost
; c holds seconds to check before chainging
; call cycle_ghost_colour_immediate to skip checking
; and using specific colour in a, without cycling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cycle_ghost_colour:
ghostP_top_left:	equ $45ec			;top left corner of colour palette tile ram							
ghost_height:		equ 14
ghost_width:		equ 14
;1,3,5,7 ghost_colour
		ld a,(frame_number)			;get frame number
		and a					;see if it is zero
		ret nz					;only animate every second
		ld a,(colour_cycle_count)		;get seconds tally
		inc a					;advance
		ld (colour_cycle_count),a		;store again
		cp c
		ret nz					;time not up
		xor a
		ld (colour_cycle_count),a		;reset counter
		ld a,(ghost_colour)
		add 2					;slip even number palettes
		cp 9					;limit to 4 ghost colours
		jr nz,.save
		ld a,1					;set back to blinky
.save:		ld (ghost_colour),a			;save for next loops update
cycle_ghost_colour_immediate:		
		ld hl,ghostP_top_left			;get palette fill start point
		ld b,ghost_width
		ld c,ghost_height
		call tileram_rectangle

		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Checks for left right changes of speed
; can only do one of the joystick directions so state ok in b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
player_controls:
		ld hl,speed
		ld a,(joystick_state)			;get joystick state
		ld b,a					;and save in b
		and inp_chk_left			;examine left
		cp inp_left_presd			;check for pressed 
		jp nz,.right				;no then check right

		ld a,(speed)				;get current speed 
		sub 1
		jr c,.down				;don't save if negative

.store_speed:	ld (speed),a
		xor a					;clear a
		ld (stepcount),a			;reset step count
		jr .down				;can't press left and right at same time using a joystick

.right:		ld a,b					;get state again
		and inp_chk_right			;examine right
		cp inp_right_presd			;check if pressed
		jr nz,.down				;it's non zero so isn't pressed
		ld a,(speed)
		add 1
		cp 60
		jr c,.store_speed			;save if < 60
		
		;manual iteration
.down:		ld a,(speed)				;get current speed value
		and a					;check for zero,as this is manual iteration mode (joystick up or down)
		jr nz,.up				;don't even bother checking
		ld a,b					;get state again
		and inp_chk_down			;examine down
		cp inp_down_presd			;check if pressed
		jr nz,.up				;it's non zero so isn't pressed
		call do_door_run			;have to render as main loop will think no iterations have happened	
		call render_doors
.up:
		ld a,b					;get state again
		and inp_chk_up				;examine up
		cp inp_up_presd				;check if pressed
		ret nz					;it's non zero so isn't pressed
		ld a,(display_base)			;get current base
		xor 1					;toggle bit 0
		ld (display_base),a			;write bit
		call show_column_labels			;update column display
		call display100instruction		;re-draw to get thew 100 bit
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; checks to see if player pressed p2 start so they can get
; quickly back to title screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
quit_to_title:
		ld a,(start_state)			;get state
		and inp_chk_p2st			;examine p2 start
		cp inp_p2_presd				;check for pressed 
		ret nz					;not 0 means didn't press so return
		pop bc					;remove return address from stack as we are aborting the call
		jp start_title_mode			;go to start_title_routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; checks to see if whole second has passed
; if so reduces time, if it's zero goes to game over
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
time_tick:
		ld a,(frame_number)			;get frame number
		and a					;see if it is zero
		
		ret nz					;if not then second is not up yet

		ld a,(game_time)			;get game time
		dec a					;reduce by one
		daa					;correct for bcd
		ld (game_time),a			;store new time
		
		ret nz					;if not zero then stop now
							;fall through to game over setup
		pop bc					;remove return address from stack as we are aborting the call

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; shows debug output
; uses jp to run hex routine so we can use the ret in the
; show hex routine to return to the caller of debug_ouput
; quicker and saves some bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
loop_start_loc:		equ $4017	
speed_start_loc:	equ $400b	
debug_output:
		ld hl,fake_return
		push hl					;create return address as can't call (ix)
		
		ld a,(display_base)
		and a
		jr nz,do_hex
		ld ix,show_dec
		jr start_output

do_hex:		ld ix,show_hex_right_one		;version that decrements hl first
start_output:
		ld hl,loop_start_loc			;output text from here
		ld bc,(doorstep)			;which iteration are we on
		dec c					;show last rendered
		jp (ix);				;output value
fake_return:		
		ld hl,speed_start_loc			;output text from here
		ld bc,(speed)				;how fast
		jp (ix)					;output value

;jump table for base display		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; displays a portion of text from a message at the top of the screen
; updates every 8 frames
; address of start position is loaded into hl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
scrolling_message:
		ld a,(frame_counter)			;get frame counter see if it's modable
		and $7					;mask all but low bits - scroll every 8 frames
		ret nz					;don't update text unless mod 8 is 0

		ld hl,(msg_one_pos)			;get text addr of first character to show
		inc hl					;move to next character
		ld a,(hl)				;load first char to see if it's zero
		and a					;is it zero?
		jp nz,.render_scrolling_message		;if not then display text
		ld hl,top_mess				;if it is then reset to start position for top text

.render_scrolling_message:
		ld (msg_one_pos),hl			;store text addr for next time
		ex de,hl				;swap hl into de so character address is in de
		ld	hl,tile_ram_top_vis_line1	;vram address to display to
		;ld	c,$3f				;colour (white)
		call borders_blit_text_only		;render text without setting colour pallete
		ret

bot_display_upd:	
		ld hl,(frame_number)			;get frame number see if it's 60
		ld a,l					;check frame number
		and a					;to see if it's zero
		ret nz					;don't update if not frame 0
		
		ld a,(bot_mess_seconds)			;get seconds interval for message change
		and h					;and with number of seconds gone by
		ret nz					;only update if on a 2 second interval, jump onto top display text
		
		ld hl,(msg_bot_addr)			;get addr of message to display
		ld e,(hl)				;remember lo-byte first in memory
		inc hl					;move to hi-byte
		ld d,(hl)				;place in d
		inc hl					;point to next addr in array of addresses
		ld a,d					;load hi-byte into for or'ing with the lo-byte
		or e					;see if address is zero (if we or we can test full zero easy)
		jp nz,process_text_bot_text		;if it isn't display the message

		ld hl,bottom_list			;reset to first item and re-load into de
		ld e,(hl)				;remember lo-byte first in memory
		inc hl					;move to hi-byte
		ld d,(hl)				;place in d
		inc hl					;point to next addr in array of addresses

process_text_bot_text:
		ld (msg_bot_addr),hl			;store next message to display
		ld	hl,tile_ram_bot_vis_line1	;tile address to start writing at
		ld	c,$15				;palette 21 (third colour tan)
		call borders_blit_text_only		;write text to border area
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;routine to update text displays that change
; TODO add counter code to move up and down through offsets
; fixed speed of this maybe every 8/16 frames
; on new message string set offset to 0, then render text
; increment offset every 8/16 frames stop when offset is 4 set increment to $ff
; then decrement every 8/16 frames (maybe need a further byte to hold inc/dec value)
; when 0 change message string at set increment to 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
process_top_message_text: 
		ld hl,(frame_number)			;get frame number see if it's 60
		ld a,l					;check frame counter
		and a					;to see if it is zero
		ret nz					;don't update if not frame 0
		ld a,h					;get second counter
		and a					;make sure zero flag possible
		ret z					;get out if seconds 0
		ld a,(top_mess_seconds)			;get seconds timer bits to look at for message
		and h					;look at these bits in h, if they are zero change message
		ret nz					;don't update if not zero result

		ld hl,(msg_top_addr)			;get addr of message to display
		ld e,(hl)				;remember lo-byte first in memory
		inc hl					;move to hi-byte
		ld d,(hl)				;place in d
		inc hl					;point to next addr in array of addresses
		ld a,d					;load hi-byte into for or'ing with the lo-byte
		or e					;see if address is zero (if we or we can test full zero easy)
		jp nz,process_text_top_message_text	;if it isn't display the message

		ld hl,top_list				;reset to first item and re-load
		ld e,(hl)				;remember lo-byte first in memory
		inc hl					;move to hi-byte
		ld d,(hl)				;place in d
		inc hl					;point to next addr in array of addresses

process_text_top_message_text:
		ld (msg_top_addr),hl			;store next message to display
		ld hl,tile_ram_top_vis_line2		;draw on second line at top
		ld c,$00;$30				;offset (testing purposes)
		call borders_blit_text_only_offset	;write text to border area
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; renders a rectangle portion of screen with same value
; hl - addr of top left corner
; a - holds value to tile ram $40xx for tile $44xx for pallete
; b - holds number of columns
; c - holds number of rows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
tileram_rectangle:
		ld de,-32				;value to move right one tile column (32 high)
next_pal_row_fill:	
		push bc					;save b
		push hl					;save current position
.row_copy:
		ld (hl),a				;set tile
		add hl,de				;next column to the right
		djnz .row_copy				;dec b and do another column if one left

		pop hl					;retrieve start of last row
		inc hl					;move down a row

		pop bc					;get b back
		dec c					;count off rows
		jr nz,next_pal_row_fill			;do another if we have one left

		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; takes a byte and outputs the dec value to top or bottom rows only
; hl holds address of vram to place data (XXX format)
; c holds value to output
; the stock digits and the 6 hex additional digits
; this version uses a continuous tile set from 48 on
; needs writing still and its slow to implement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
show_dec:
		ld a,c
start_loop:	
		ld c,0
sub100:	
		sub 100
		jr c,got100s
		inc c
		jr sub100
got100s:
		add 100					;take back positive before last fatal subtraction
		ld b,a					;save temp
		ld a,48
		add c					;make ascii
		ld (hl),a				;output 100's
		dec hl					;move right
		;do 10's
		ld c,0
		ld a,b
sub10s:
		sub 10
		jr c,got10s
		inc c
		jr sub10s
got10s:
		add 10					;take back positive before last fatal subtraction
		ld b,a
		ld a,48
		add c
		ld (hl),a				;output 10's
		dec hl					;move right
		ld a,48
		add b					;get units
		ld (hl),a				;output units
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; takes a byte and outputs the hex value to top or bottom rows only
; hl holds address of vram to place data
; c holds value to output
; the stock digits and the 6 hex additional digits
; this version uses a continuous tile set from 48 on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
show_hex_right_one:					;for overwriting the extra char generated by denary display
		ld a,'X'
		ld (hl),a
		dec hl
show_hex:
		ld b,2					;do this twice
.nibbleout:
		xor a					;clear carry
		ld a,c					;get byte to output
		rlca
		rlca
		rlca
		rlca					;rotate 4 times to swap nibbles
		ld c,a					;store temp for next time
		and $f					;mask 
		add $30					;add ascii for 0 is 48
.output:
		ld (hl),a				;write tile value to vram
		dec hl					;move to next column on the right for lower nibble
		djnz .nibbleout				;decrement b, loop if it is not zero
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic text blitting routine for normal areas of screen (28x32 main playfield)
; text strings MUST be zero terminated
; on entry
; de points to text string
; hl points to address in vram to print at
; c = palette colour of tile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main_blit_text_and_colour:	
		ld	a,(de)				;get the byte at de
		inc	de				;increment the text pointer
		or	a				;test for string terminator 
		ret z					;return if we've finished this string
		
		ld	(hl),a				;write the byte to vram
		set	2,h				;point hl at colour ram by setting bit 2 of h
		ld	a,c				;get colour byte into a
		ld	(hl),a				;write the byte to colour ram
		ld	a,l				;low byte of vram/colour address into A
		sub	32				;subtract 32 to move across the screen by 1 char position
		ld	l,a				;store back to l
		res	2,h				;point hl back at vram by clearing bit 2 of h
		ld	a,h				;handle the carry from the SUB instruction (if there was one)
		sbc	a,0				;subtract zero and the carry flag from a
		ld	h,a				;store back to d
		jp	main_blit_text_and_colour	;loop until we read a zero terminating byte
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; draw my name and date and insttructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this bit here so we can call it dynamically on base change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
display100instruction:
		ld c,yellow
		ld hl,$43a1
		ld a,(display_base)			;what number base
		and a					;zero is denary
		jr nz,hexof100
		ld de,doors_100_copy
		jr draw100
hexof100:	ld de,doors_100_copy_hex
draw100:	call main_blit_text_and_colour
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
draw_hurray_banana_text:
		call display100instruction

		ld c,blue
		ld hl,$43a2
		ld de,developed_by_copy
		call main_blit_text_and_colour

		ld c,red
		ld hl,$43a3
		ld de,hurray_banana_copy
		call main_blit_text_and_colour

		ld c,cyan
		ld hl,$43a4
		ld de,date_and_year_copy
		call main_blit_text_and_colour

		ld c,orange
		ld hl,$43a6
		ld de,start1_instruction
		call main_blit_text_and_colour

		ld c,pink
		ld hl,$43a7
		ld de,start2_instruction
		call main_blit_text_and_colour

		ld c,grey
		ld hl,$43a8
		ld de,start3_instruction
		call main_blit_text_and_colour

		ld c,brown
		ld hl,$43a9
		ld de,start4_instruction
		call main_blit_text_and_colour
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic text blitting routine for normal areas of screen (28x32 main playfield)
; text strings MUST be zero terminated
; on entry
; de points to text string
; hl points to address in vram to print at
; can do this as we don't have mess about with pallette ram
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main_blit_text_only:	
		push bc					;save bc as we are gonna nuke it
		ld bc,32				;column skip amount
.char_loop:
		ld a,(de)				;get the byte at de
		inc de					;increment the text pointer
		or a					;test for string terminator 
		jp z,.finish				;return if we've finished this string
			
		ld (hl),a				;write the byte to vram
		sbc hl,bc				;move to next column
		jp .char_loop				;loop until we read a zero terminating byte
.finish:
		pop bc					;retrieve bc
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generic text blitting routine for normal areas of screen (28x32 main playfield)
; xor of the string with the tiles in tile ram
; text strings MUST be ff terminated, zero is used to not xor
; on entry
; de points to xor string
; hl points to address in vram to print at
; can do this as we don't have mess about with pallette ram
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main_xor_blit_text_only:	
		push bc					;save bc as we are gonna nuke it
		ld bc,32				;column skip amount
.char_loop:
		ld a,(de)				;get the byte at de
		inc de
		cp 255					;check for xor terminator
		jp z,.finish				;return if we've finished this string
		
		xor (hl)				;string value in a
		ld (hl),a				;write the byte to vram
		sbc hl,bc				;move to next column

		jp .char_loop				;loop until we read a zero terminating byte
.finish:
		pop bc					;retrieve bc
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; writes text to the top or bottom two rows
; on entry
; de points to text string
; hl points to address in vram to print at
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
borders_blit_text:	
		ld b,$1d				;maximum number of chars to draw in a line (28)
.blit_loop:
		ld a,(de)				;get the byte at HL
		inc de					;increment the text pointer
		or a					;test for string terminator (0)
		ret z					;return if we've finished this string
		
		dec b					;check maximum characters has not been met
		ret z					;return if we have

		ld (hl),a				;write the byte to vram
		set 2,h					;point DE at colour ram by setting bit 2 of D
		ld a,c					;get colour byte into A
		ld (hl),a				;write the byte to colour ram
		res 2,h					;point DE back at vram by clearing bit 2 of D
		dec hl					;move to next column
		jp .blit_loop				;loop until we read a zero terminating byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; writes text to the top or bottom two rows
; hl contains vram address to write to
; de holds address of zero terminated string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
borders_blit_text_only:	
		ld b,$1d				;maximum number of chars to draw in a line (28)
.blit_loop:
		ld a,(de)				;get the byte at HL
		inc de					;increment the text pointer
		or a					;test for string terminator (0)
		ret z					;return if we've finished this string
		
		dec b					;check maximum characters has not been met
		ret z					;return if we have
		
		ld (hl),a				;write the byte to vram
		dec hl					;move to next column
		jp .blit_loop				;loop until we read a zero terminating byte		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; writes text to the top or bottom two rows
; hl contains vram address to write to
; de holds address of zero terminated string
; c holds offset to apply to a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
borders_blit_text_only_offset:	
		ld b,$1d				;maximum number of chars to draw in a line (28)
.blit_loop:
		ld a,(de)				;get the byte at HL
		inc de					;increment the text pointer
		or a					;test for string terminator (0)
		ret z					;return if we've finished this string
		
		dec b					;check maximum characters has not been met
		ret z					;return if we have
		
		cp 32					;space is space
		jr z,.space_skip_offset
		add c					;add tile offset first
.space_skip_offset:
		ld (hl),a				;write the byte to vram

		dec hl					;move to next column
		jp .blit_loop				;loop until we read a zero terminating byte	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fills an entire row in either the top or
; bottom border with the value in a
; a contains tile/pallete number to write
; b contains number of tiles to fill
; hl contains tile ram or palette
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fill_border_area_row:
.write 		ld (hl),a				;store value at current address
		dec hl					;move right one column
		djnz .write				;loop back if we have more to do
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; xor's an entire row in either the top or
; bottom border with the a value
; b contains number of tiles to fill
; c contains value to xor
; hl contains tile ram or palette
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
xor_border_area_row:
.write 			
		ld a,c					;load xor mask
		xor (hl)				;xor with current tile/pallete addr contents
		ld (hl),a				;write new value
		dec hl					;move right one column
		djnz .write				;loop back if we have more to do
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fills the entire screen pallete ram with given pallete number
; a contains pen colour to fill tiles with
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fill_screen_area_colour:
		ld hl,pallete_ram_main			;colour ram
		ld de,pallete_ram_main + 1		;one on from hl so we effectively drag the value at hl through vram
		ld bc,$37f				;all but one address needed as we manually set the first one below
		ld (hl),a				;write first value to hl
		ldir					;copy (hl) -> (de), inc hl, inc de, dec bc, loop until bc = 0
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a contains tile number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fill_screen_area_working_slow:
		ld hl,tile_ram_play_area		;tile ram
		ld de,tile_ram_play_area + 1		;one on from hl so we effectively drag the value at hl through vram
		ld bc,$37f				;all but one address needed as we manually set the first one below
		ld (hl),a				;write first value to hl
		ldir					;copy (hl) -> (de), inc hl, inc de, dec bc, loop until bc = 0
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fills a row with given tile/pallete
; hl holds start address to fill
; a holds the tile/palette colour to use
; b holds number of tiles/palettes to draw
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fill_screen_area_row:
		ld de,-32				;skip between columns 32 bytes down to move right
.write_loop:	
		ld (hl),a				;store tile/pallete value 
		add hl,de				;move right (back 32 bytes)
		djnz .write_loop			;have we done the correct number of writes?
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; xor's a row with given value - useful for flashing tiles
; hl holds start address to fill
; b holds number of tiles/palettes to draw
; c holds value to xor with tile/pallete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
xor_screen_area_row:
		ld de,-32				;skip between columns 32 bytes down to move right
.write_loop:	
		ld a,c					;load c into
		xor (hl)				;and xor value at hl with a
		ld (hl),a				;store tile/pallete value 
		add hl,de				;move right (back 16 bytes)
		djnz .write_loop			;have we done the correct number of writes?
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; takes a wasteful raw tilemap and copies to tile ram
; hl contains addr of raw tilemap data
; map in this format needs a byte per tile and therefore takes up 896 bytes of rom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
draw_raw_map_to_screen:
		ld de,tile_ram_play_area		;main draw area of screen
		ld bc,$380				;copy 896 bytes raw
		ldir					;(hl) -> (de) inc hl, inc de, dec bc, stop if bc = 0	
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sp contains address +1 to push 2 tiles into
; it's quicker to use stack pointer to push 2 bytes than using (hl) and increment
; de gets destroyed
; b contains full block to use
; a contains bitmap for tiles
; 2 leftmost bits are dropped into carry for testing
; to be used as a block of 4 macro calls to process an entire byte (8 rows)
; need 4 of these blocks to complete an entire column
; 
; Macro's allow us to create unrolled loops, this processes two bits of data
; they have to be populated with local labels as the code is substituted by the assembler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
tile_empty_block:		equ $20
tile_full_block:		equ $40

		MACRO DECODE_AND_PUSH_TWO_TILES												
		add a					;get msb into carry						
		jp c,.fullfirst				;is it a block (1)						
		ld d,tile_empty_block			;no so set empty tile					
		jp .next				;look at next bit						
.fullfirst:	ld d,b					;set first to a full tile				

.next:		add a					;get msb into carry for msb				
		jp c,.fullnext				;is it a block (1)						
		ld e,tile_empty_block			;no sp set set 2nd tile to empty tile	
		jp .pushnow				;go and perform the push				
.fullnext:	ld e,b					;set second tile to full tile			
.pushnow:	push de					;store the tiles on screen				
	ENDM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this draws a column at a time in reverse (bottom to top) to take advantage of stack pushes
; takes a bitmap encoded pixel map and renders to screen in solid tiles
; on entry hl holds address of pixel map
; a holds tile to use in place of block
; $0e is a full block
; $7f is hash and $8f is checker pattern
; uses the macro above 4 times - to process a full byte of data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
draw_pixel_map_to_screen:
		ld (save_sp),sp				;save stack pointer as we are going to use it to store to tile ram
		ld (full_tile),a			;save full tile
		ld sp,tile_ram_play_area_last + 1	;set stack pointer to end of main video area + 1 - as push will pre decrement the address
		ld b,28 * 4				;28 columns of data to process 4 bytes per column
.process_byte:	
		ld a,b					;copy a to b to move to ram
		ld (save_reg),a				;temporarily store b
		ld a,(full_tile)			;get full tile
		ld b,a					;and put in b
		
		ld a,(hl)				;load bitmap for next eight blocks from map
		inc hl					;move to next bitmap ready for next loop
							;use 4 copies of the macro to process entire byte
		DECODE_AND_PUSH_TWO_TILES		;process bits 7 and 6
		DECODE_AND_PUSH_TWO_TILES		;process bits 5 and 4
		DECODE_AND_PUSH_TWO_TILES		;process bits 3 and 2
		DECODE_AND_PUSH_TWO_TILES		;process bits 1 and 0
		
		ld a,(save_reg)				;get saved value
		ld b,a					;restore b
		djnz .process_byte			;loop to process next byte of map if b is not zero
		ld sp,(save_sp)				;restore stack pointer
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data starts here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;align to start of second 4k rom .6f
        	org  $1000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; message data for display during render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

doors_100_copy: defm "1OO_DOORS",0
doors_100_copy_hex: defm "X64 DOORS",0
developed_by_copy: defm "DEVELOPED AND DESIGNED BY",0
hurray_banana_copy: defm "HURRAY BANANA",0
date_and_year_copy: defm "MARCH 2021",0
start1_instruction: defm "LEFT DEC DELAY",0
start2_instruction: defm "RIGHT INC DELAY",0
start3_instruction: defm "DOWN ADVANCE IF DELAY 0",0
start4_instruction: defm "UP TOGGLE HEX DENARY",0

		ORG $2000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; some messages to display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				;0123456789012345678901234567 - for alignment purposes
msg_quit_game:		defm    " 2P START  TO QUIT TO TITLE ",0
msg_debug:			defm	"LOOP       DELAY            ",0
msg_scoring:		defm	"1 UP       TIME     HI SCORE",0
msg_game_over:		defm	"  GAME    OVER  ",0
msg_gameover_blank:	defm	"                ",0
msg_blank:		defm	"                            ",0 ;use this label to also refer to a blank message
top_mess_seconds	db 3
machine_code:
MC_A:			DEFM	"HERE IS THE MACHINE LANGUAGE",0
MC_B:			DEFM	"AND ASSEMBLY LANGUAGE OF THE",0
MC_C:			DEFM	"100 DOORS Z80 IMPLEMENTATION",0
MC_00:			DEFM	"019B AF XOR A               ",0
MC_01:			DEFM	"019C 21 00 4C LD HL,$4C00   ",0
MC_02:			DEFM	"019F 77 LD (HL),A           ",0
MC_03:			DEFM	"01A0 11 01 4C LD DE,$4C01   ",0
MC_04:			DEFM	"01A3 01 64 00 LD BC,$64     ",0
MC_05:			DEFM	"01A6 ED B0 LDIR             ",0
MC_06:			DEFM	"01A8 3C INC A               ",0
MC_07:			DEFM	"01A9 32 7C 4C LD ($4C7C),A  ",0
MC_08:			DEFM	"01AC C9 RET                 ",0
MC_09:			DEFM	"01AD 3A 7C 4C LD A,($4C7C)  ",0
MC_10:			DEFM	"01B0 FE 65 CP $65           ",0
MC_11:			DEFM	"01B2 C8 RET Z               ",0
MC_12:			DEFM	"01B3 5F LD E,A              ",0
MC_13:			DEFM	"01B4 AF XOR A               ",0
MC_14:			DEFM	"01B5 0E 01 LD C,$1          ",0
MC_15:			DEFM	"01B7 21 00 4C LD HL,$4C00   ",0
MC_16:			DEFM	"01BA 7D LD A,L              ",0
MC_17:			DEFM	"01BB 83 ADD E               ",0
MC_18:			DEFM	"01BC 6F LD L,A              ",0
MC_19:			DEFM	"01BD 3E 64 LD A,$64         ",0
MC_20:			DEFM	"01BF 95 SUB L               ",0
MC_21:			DEFM	"01C0 38 05 JR C,$05         ",0
MC_22:			DEFM	"01C2 79 LD A,C              ",0
MC_23:			DEFM	"01C3 AE XOR (HL)            ",0
MC_24:			DEFM	"01C4 77 LD (HL),A           ",0
MC_25:			DEFM	"01C5 18 F3 JR $F3           ",0
MC_26:			DEFM	"01C7 1C INC E               ",0
MC_27:			DEFM	"01C8 7B LD A,E              ",0
MC_28:			DEFM	"01C9 32 7C 4C LD ($4C7C),A  ",0
MC_29:			DEFM	"01CC C9 RET                 ",0
;MC_00:			DEFM	"019B AF       XOR A         ",0
;MC_01:			DEFM	"019C 21 00 4C LD HL,$4C00   ",0
;MC_02:			DEFM	"019F 77       LD (HL),A     ",0
;MC_03:			DEFM	"01A0 11 01 4C LD DE,$4C01   ",0
;MC_04:			DEFM	"01A3 01 64 00 LD BC,$64     ",0
;MC_05:			DEFM	"01A6 ED B0    LDIR          ",0
;MC_06:			DEFM	"01A8 3C       INC A         ",0
;MC_07:			DEFM	"01A9 32 7C 4C LD ($4C7C),A  ",0
;MC_08:			DEFM	"01AC C9       RET           ",0
;MC_09:			DEFM	"01AD 3A 7C 4C LD A,($4C7C)  ",0
;MC_10:			DEFM	"01B0 FE 65    CP $65        ",0
;MC_11:			DEFM	"01B2 C8       RET Z         ",0
;MC_12:			DEFM	"01B3 5F       LD E,A        ",0
;MC_13:			DEFM	"01B4 AF       XOR A         ",0
;MC_14:			DEFM	"01B5 0E 01    LD C,$1       ",0
;MC_15:			DEFM	"01B7 21 00 4C LD HL,$4C00   ",0
;MC_16:			DEFM	"01BA 7D       LD A,L        ",0
;MC_17:			DEFM	"01BB 83       ADD E         ",0
;MC_18:			DEFM	"01BC 6F       LD L,A        ",0
;MC_19:			DEFM	"01BD 3E 64    LD A,$64      ",0
;MC_20:			DEFM	"01BF 95       SUB L         ",0
;MC_21:			DEFM	"01C0 38 05    JR C,$05      ",0
;MC_22:			DEFM	"01C2 79       LD A,C        ",0
;MC_23:			DEFM	"01C3 AE       XOR (HL)      ",0
;MC_24:			DEFM	"01C4 77       LD (HL),A     ",0
;MC_25:			DEFM	"01C5 18 F3    JR $F3        ",0
;MC_26:			DEFM	"01C7 1C       INC E         ",0
;MC_27:			DEFM	"01C8 7B       LD A,E        ",0
;MC_28:			DEFM	"01C9 32 7C 4C LD ($4C7C),A  ",0
;MC_29:			DEFM	"01CC C9       RET           ",0

;offsets for slide out/in of ascii character set
;ff used to indicate draw empty string msg_blank
slide_offsets:		db	$ff,$c0,$90,$30,$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this is the addressess of the messages to display (above), 
; the code looks for an address of zero to restart the list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
top_list:		defw	MC_A,MC_B,MC_C,MC_00,MC_01,MC_02,MC_03,MC_04
			defw	MC_05,MC_06,MC_07,MC_08,MC_09,MC_10,MC_11,MC_12
			defw	MC_13,MC_14,MC_15,MC_16,MC_17,MC_18,MC_19,MC_20
			defw	MC_21,MC_22,MC_23,MC_24,MC_25,MC_26,MC_27,MC_28
			defw	MC_29,0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; data for the top line scrolling message
; terminated with a zero
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
top_mess:			
			defm	"                         100 DOORS PAC MAN         "
			defm	"   A VERSION OF THE COMPUTATIONAL THINKING EXERCISE     "
			defm	"     IMPLEMENTED IN Z80 ASSEMBLY LANGUAGE      "
			defm	"  AND TARGETTING MIDWAY PAC MAN HARDWARE    "
			defm	"   THIS WILL RUN ON AN ACTUAL PAC MAN ARCADE MACHINE    "
			defm	"  IT USES THE JOYSTICK AND PLAYER 1 AND PLAYER 2 START BUTTONS TO CONTROL IT....."
			defm	"     THE ANIMATION EFFECTS ON THE GHOST MOUTH AND ITS FEET IS ACHIEVED BY XORING "
			defm	"VALUES TO THE TILE MEMORY WHICH GIVES US A TOGGLE EFFECT     "
			defm	" THE SOURCE IS AROUND 1700 LINES OF CODE ALTHOUGH THE CORE ALGORITHM FOR 100 DOORS "
			defm	"ONLY OCCUPIES 40 BYTES OF ROM          WITHOUT OPTIMISATION "
			defm	"           CREATED BY HURRAY BANANA FEB 2021 TO MARCH 2021       "
			defm	0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a list of message width of the screen (28 columns)
; to display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bot_mess_seconds:	defm 0	; change every second
bot_mess_1:		defm	"  PRESS 1P START TO BEGIN   ",0
bot_mess_2:		defm	"BY HURRAY BANANA  MARCH 2021",0
			;	 0123456789012345678901234567- for alignment purposes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this is the addressess of the messages to display (above), 
; the code looks for an address of zero to restart the list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bottom_list:		defw	bot_mess_1,msg_blank,bot_mess_2,msg_blank,0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;column ordered data for generating the title screen
;bitmap for title screen - each bit is a solid tile when rendered
;top row is left hand column working from bottom to top
;full screen 28 rows of 4 bytes (32 bits for each of the 32 rows of the main screen area)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
title_screen_column_data:
		defm $FC,$00,$00,$22
		defm $24,$00,$00,$3F
		defm $24,$00,$00,$20
		defm $18,$00,$00,$1E
		defm $40,$00,$00,$21
		defm $A8,$00,$0F,$E1
		defm $A8,$00,$08,$5E
		defm $F0,$00,$08,$40
		defm $00,$00,$07,$9E
		defm $70,$00,$00,$21
		defm $88,$00,$07,$21
		defm $88,$00,$08,$9E
		defm $00,$00,$08,$80
		defm $F0,$00,$07,$00
		defm $09,$FC,$00,$00
		defm $33,$BF,$87,$00
		defm $0B,$DF,$C8,$80
		defm $F1,$DF,$E8,$80
		defm $00,$B9,$E7,$00
		defm $41,$B9,$F0,$00
		defm $AB,$DF,$FF,$00
		defm $AB,$DF,$F0,$80
		defm $F1,$B9,$F0,$80
		defm $00,$B9,$E0,$00
		defm $F9,$DF,$E9,$00
		defm $0B,$DF,$CA,$80
		defm $0B,$BF,$8A,$80
		defm $F1,$FC,$04,$80
game_screen_column_data:
		defm $FC,$00,$00,$00
		defm $24,$00,$00,$00
		defm $24,$00,$00,$00
		defm $18,$00,$00,$00
		defm $40,$00,$00,$00
		defm $A8,$00,$00,$00
		defm $A8,$00,$00,$00
		defm $F0,$00,$00,$00
		defm $00,$00,$00,$00
		defm $70,$00,$00,$00
		defm $88,$00,$00,$00
		defm $88,$00,$00,$00
		defm $00,$00,$00,$00
		defm $F0,$00,$00,$00
		defm $09,$FC,$00,$00
		defm $33,$F8,$80,$00
		defm $0B,$F0,$40,$00
		defm $F1,$F0,$60,$00
		defm $00,$F8,$E0,$00
		defm $41,$FF,$F0,$00
		defm $AB,$FF,$F0,$00
		defm $AB,$F8,$F0,$00
		defm $F1,$F0,$70,$00
		defm $00,$F0,$60,$00
		defm $F9,$F8,$E0,$00
		defm $0B,$FF,$C0,$00
		defm $0B,$FF,$80,$00
		defm $F1,$FC,$00,$00
title_screen_column_data_hex:
		defm $FC,$00,$00,$34;$28 <- made X taller
		defm $24,$00,$00,$08;$10
		defm $24,$00,$00,$34;$28
		defm $18,$00,$00,$00
		defm $40,$00,$00,$1E
		defm $A8,$00,$0F,$E5
		defm $A8,$00,$08,$65
		defm $F0,$00,$08,$58
		defm $00,$00,$07,$80
		defm $70,$00,$00,$0C
		defm $88,$00,$07,$0A
		defm $88,$00,$08,$89
		defm $00,$00,$08,$BF
		defm $F0,$00,$07,$00
		defm $09,$FC,$00,$00
		defm $33,$BF,$87,$00
		defm $0B,$DF,$C8,$80
		defm $F1,$DF,$E8,$80
		defm $00,$B9,$E7,$00
		defm $41,$B9,$F0,$00
		defm $AB,$DF,$FF,$00
		defm $AB,$DF,$F0,$80
		defm $F1,$B9,$F0,$88
		defm $00,$B9,$E0,$13
		defm $F9,$DF,$E9,$10
		defm $0B,$DF,$CA,$93
		defm $0B,$BF,$8A,$88
		defm $F1,$FC,$04,$80 ;<-- added smiley in top right of screen


;nice and simple way of specifiying how to split the binary up. Has to be hard coded to file locations
;does an automatic ovewrite of files - needs to be at end of assembler file I think.
;save locations for data for each rom
	savebin "D:\OneDrive\Coding general\arcade coding\0mycode\AllPacmanCodingMame\roms\pacman\pacman.6e",$0000, $1000
	savebin "D:\OneDrive\Coding general\arcade coding\0mycode\AllPacmanCodingMame\roms\pacman\pacman.6f",$1000, $1000
	savebin "D:\OneDrive\Coding general\arcade coding\0mycode\AllPacmanCodingMame\roms\pacman\pacman.6h",$2000, $1000
	savebin "D:\OneDrive\Coding general\arcade coding\0mycode\AllPacmanCodingMame\roms\pacman\pacman.6j",$3000, $1000

;;try and auto launch mame debugger after assembly - does not like spaces in path names
	IF ((_ERRORS + _WARNINGS) = 0)
        SHELLEXEC "D:\OneDrive\Coding general\arcade coding\AllPacmanCoding\AllPacManCoding\100Doors\CopyDoorsRoms.bat"; + $(SolnDir)
        SHELLEXEC "D:\OneDrive\Coding general\arcade coding\AllPacmanCoding\AllPacManCoding\launchMame.bat"

    ENDIF
	
