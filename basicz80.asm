;100 doors algorithmin z80 assembly language
;created by hurray banana Feb 2021
;0 is door closed $ff is door open
;tab size=8


initialise:
	xor a			;clear a register
    	ld hl,doors		;point hl at addr of 1st element of doors array
    	ld (hl),a		;clear byte 
    	ld de,doors+1		;point de at addr of 2nd element of doors array
    	ld bc,100		;counter for ldir
	ldir			;copy value at addr in hl to addr in de, then inc both hl and de
    				;decrement bc at repeat until bc = 0

	ld hl,doors		;point hl at addr of 2nd element of doors array
    	ld (hl),a		;and wasted door

    	ld de,1			;step value (initially 1)
	ld c,1			;set Toggle mask
    	dec a			;set a to $ff (255, it was 0 before)
    
startrun:
    	ld hl,doors		;load start of doors array (we ignore 1st byte)
    
nextdoor:
    	add hl,de		;increment to next door
	ld a,100
    	sub l			;check we have a legal door position			
    	jr c,nextrun		;bigger than 100 so gone off end
    
    	ld a,c			;load mask
    	xor (hl)		;toggle door
    	ld (hl),a		;write door back
    
    	jr nextdoor		;move onto to next door
    
nextrun:
	inc e			;increment step value
    
   	ld a,101		;check step to see if outside doors
    	sub e			
    	jr nz,startrun		;if not zero (not 101) go again from start

    	halt			;wait for interrupt

doors:	.ds 101			;declare space for 101 bytes
