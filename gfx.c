/*=========================================================================

GFX - a small graphics library 

Copyright (C) 2004  Rafael de Oliveira Jannone

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Contact the author:
	by e-mail : rafael AT jannone DOT org
	homepage  : http://jannone.org/gfxlib
	ICQ UIN   : 10115284

See the License at http://www.gnu.org/copyleft/lesser.txt

=========================================================================*/

// GFX.C : main library functions

/* === WARNING ==

	This is a work-in-progress, meaning that most of this code is unstable
	and it's subject to future changes.  Also, most of it is very hackish,
	not properly cleaned up nor tested.

   === WARNING == */

#include "gfx.h"

// do a interslot call to BIOS

#ifdef __SDCC

void relocate_callbios_from_rom_to_ram();

void relocate_callbios_from_rom_to_ram() __naked {
__asm

callbios_ram_1      .equ #callbios_rom_1 - #callbios_rom_start + #callbios
callbios_rom_length .equ #callbios_rom_end - #callbios_rom_start

  push af
  push bc
  push de
  push hl
  ld hl, #callbios_rom_start
  ld de, #callbios
  ld bc, #callbios_rom_length
  ldir
  pop hl
  pop de
  pop bc
  pop af
  ret
  
callbios_rom_start:
  ld      (callbios_ram_1), ix
  rst     #0x30
callbios_rom_0:
  .db    0
callbios_rom_1:
  .dw    0
  ret
callbios_rom_end:
  
.area   _DATA
callbios:
  .ds #callbios_rom_length
.area   _CODE           
__endasm;
}

const u_char st_dir[]={
        0x00, // 0
        0x01, // 1
        0x03, // 2
        0x02, // 3
        0x06, // 4
        0x04, // 5
        0x0C, // 6
        0x08, // 7
        0x09  // 8
};

#else 

#asm

psect text

;callbios: ld (callbios0), ix
;	defb 0CDh
;callbios0:
;	defw 0000h
;	ret

callbios:
        ld      (callbios1), ix
        rst     030h
callbios0:
        defb    0
callbios1:
        defw    0
        ret

psect data
global _st_dir
_st_dir:
	defb 0000B ; 0
	defb 0001B ; 1
	defb 0011B ; 2
	defb 0010B ; 3
	defb 0110B ; 4
	defb 0100B ; 5
	defb 1100B ; 6
	defb 1000B ; 7
	defb 1001B ; 8

#endasm

#endif
                           
static int rnd = 0;

void seed_rnd(int seed) {
	rnd = seed;
}

u_char get_rnd() {
        rnd = ((rnd + 1) & 0x0FFF);
        return *(u_char*)rnd;
}

void init() {
#ifdef __SDCC
  // TODO: delimitare con #ifdef ROM ?
  relocate_callbios_from_rom_to_ram();
  
#else

#asm
	;in a, (0A8h)
	;and 0FCh
	;out (0A8h), a
#endasm

#endif           
}

/*

// this is not currently compiling... must investigate

void *set_sprite(u_char, void*);
void *put_sprite(u_char, int, int, u_char, u_char);

void _init_sprites() {
	u_char m = get_vdp(1);
	if (m & sprite_large) {
		set_sprite = set_sprite_16;
		put_sprite = put_sprite_16;
	} else {
		set_sprite = set_sprite_8;
		put_sprite = put_sprite_8;
	}
}
*/
#define _init_sprites()

void set_vdp(u_char reg, u_char value)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm

	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp
  
	f_wrtvdp .equ #0x0047

	push bc

	ld c, 4(ix)
	ld b, 5(ix)

	ld ix, #f_wrtvdp
	call callbios

	pop bc

	pop ix           ;epilogue
	ret

__endasm;
#else
#asm
	f_wrtvdp equ 047h

	push bc
	push ix

	ld c, (ix+6)
	ld b, (ix+8)

	ld ix, f_wrtvdp
	call callbios
	
	pop ix
	pop bc
#endasm
#endif          
}

u_char get_vdp(u_char reg) {
	return *(u_char*)(0xF3DF + reg);
}

void set_mode(u_int mode)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm

	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp
  
	push bc
	ld c, 4(ix)
	ld b, 5(ix)
	push bc
	pop ix
	call callbios
	pop bc

	pop ix           ;epilogue
	ret
                            
__endasm;
#else
#asm
	push bc
	push ix
	ld c, (ix+6)
	ld b, (ix+7)
	push bc
	pop ix
	call callbios
	pop ix
	pop bc
#endasm
#endif          
	_init_sprites();
}

void set_mangled_mode() {
	set_mode(mode_1);
	set_mode(0x7E);
	vwrite((void*)0x1BBF, 0x0800, 0x800);	
	vwrite((void*)0x1BBF, 0x1000, 0x800);	
	fill(MODE2_ATTR, 0xF0, 0x17FF);
	fill(0xFF8, 0xFF, 8);
	fill(0x17F8, 0xFF, 8);

	_init_sprites();
}

void set_color(u_char front, u_char back, u_char border) {
	*(u_char*)0xf3e9 = front;
	*(u_char*)0xf3ea = back;
	*(u_char*)0xf3eb = border;
#ifdef __SDCC
__asm
	f_chgclr .equ #0x0062

	push ix
	ld ix, #f_chgclr
	call callbios
	pop ix	
__endasm;
#else
#asm
	f_chgclr equ 062h

	push ix
	ld ix, f_chgclr
	call callbios
	pop ix	
#endasm
#endif          
}

void cls()
{
#ifdef __SDCC
__asm
        push ix

        f_cls .equ #0x00C3

        push af
	push bc
	push de  
	push hl

	ld ix, #f_cls
	call callbios

	pop hl
	pop de
	pop bc
        pop af

	pop ix           ;epilogue
	ret
                            
__endasm;
#else
#asm
        f_cls equ 0C3h

	push hl
	push de
	push bc
	push ix

	ld ix, f_cls
	call callbios
	pop ix
	pop bc
	pop de
	pop hl
#endasm
#endif          
}

void fill(u_int addr, u_char value, u_int count)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm
	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp

        f_filvrm .equ #0x0056

        push af
	push bc
	push hl

	ld l, 4(ix)
	ld h, 5(ix)
	ld c, 7(ix)
	ld b, 8(ix)
	ld a, 6(ix)

	ld ix, #f_filvrm
	call callbios

	pop bc
	pop hl
        pop af

	pop ix           ;epilogue
	ret
                            
__endasm;
#else
#asm
	f_filvrm equ 056h

	push hl
	push bc
	push ix

	ld l, (ix+6)
	ld h, (ix+7)
	ld c, (ix+10)
	ld b, (ix+11)
	ld a, (ix+8)

	ld ix, f_filvrm
	call callbios
	pop ix
	pop bc
	pop hl
#endasm
#endif          
}

void vpoke(u_int addr, u_char value)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm
	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp

	f_wrtvrm .equ #0x004D

	;push hl
	;push ix

	;ld l, (ix+6)
	;ld h, (ix+7)
	;ld a, (ix+8)
	
	;ld ix, f_wrtvrm
	;call callbios

	;pop ix
	;pop hl

	p_vdp_data .equ #0x98
	p_vdp_cmd  .equ #0x99

	; enter vdp address pointer

	ld a, 4(ix)
	di
	out (p_vdp_cmd), a
	ld a, 5(ix)
	and #0x3f
	or  #0x40
	ei
	out (p_vdp_cmd), a

	; enter data

	ld a, 6(ix)
	out (p_vdp_data), a

	pop ix           ;epilogue
	ret
		
__endasm;
#else
#asm
	f_wrtvrm .equ 0x004D

	;push hl
	;push ix

	;ld l, (ix+6)
	;ld h, (ix+7)
	;ld a, (ix+8)
	
	;ld ix, f_wrtvrm
	;call callbios

	;pop ix
	;pop hl

	p_vdp_data .equ 0x098
	p_vdp_cmd  .equ 0x099

	; enter vdp address pointer

	ld a, (ix+6)
	di
	out (p_vdp_cmd), a
	ld a, (ix+7)
	and 00111111B
	or  01000000B
	ei
	out (p_vdp_cmd), a

	; enter data

	ld a, (ix+8)
	out (p_vdp_data), a
		
#endasm
#endif            
}

u_char vpeek(u_int addr)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm
	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp

	f_rdvrm .equ 0x004A

	;push hl
	;push ix
	;ld l, 6(ix)
	;ld h, 7(ix)
	;ld ix, f_rdvrm
	;call callbios
	;pop ix
	;pop hl
	;ld l, a

	; enter vdp address pointer

	ld a, 4(ix)
	di
	out (p_vdp_cmd), a
	ld a, 5(ix)
	and #0x3f
	ei
	out (p_vdp_cmd), a

	; read data

	in a, (p_vdp_data)
	ld l, a

	pop ix           ;epilogue
	ret

__endasm;
#else
#asm
	f_rdvrm equ 04Ah

	;push hl
	;push ix
	;ld l, (ix+6)
	;ld h, (ix+7)
	;ld ix, f_rdvrm
	;call callbios
	;pop ix
	;pop hl
	;ld l, a

	; enter vdp address pointer

	ld a, (ix+6)
	di
	out (p_vdp_cmd), a
	ld a, (ix+7)
	and 00111111B
	ei
	out (p_vdp_cmd), a

	; read data

	in a, (p_vdp_data)
	ld l, a
#endasm
#endif            
}

void vmerge(u_int addr, u_char value)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm
	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp

	;f_wrtvrm .equ 0x004D
	;f_rdvrm .equ 0x0004A

	;push hl
	;push ix

	;ld l, 6(ix)
	;ld h, 7(ix)
	;ld ix, f_rdvrm
	;call callbios

	;pop ix
	;push ix

	;or 8(ix)
	;ld ix, f_wrtvrm
	;call callbios

	;pop ix
	;pop hl

	; enter vdp address pointer

	ld l, 4(ix)
	ld h, 5(ix)
	ld c, #p_vdp_cmd
	ld b, c

	di
	out (c), l
	ld a, h
	and #0x3f
	ei
	out (c), a

	; read data

	ld c, #p_vdp_data
	in h, (c)
	ld c, b

	; enter same address

	di
	out (c), l
	or  #0x40
	ei
	out (c), a

	ld a, 6(ix)
	out (p_vdp_data), a

	pop ix           ;epilogue
	ret

__endasm;
#else
#asm
	;f_wrtvrm equ 04Dh
	;f_rdvrm equ 04Ah

	;push hl
	;push ix

	;ld l, (ix+6)
	;ld h, (ix+7)
	;ld ix, f_rdvrm
	;call callbios

	;pop ix
	;push ix

	;or (ix+8)
	;ld ix, f_wrtvrm
	;call callbios

	;pop ix
	;pop hl

	; enter vdp address pointer

	ld l, (ix+6)
	ld h, (ix+7)
	ld c, p_vdp_cmd
	ld b, c

	di
	out (c), l
	ld a, h
	and 00111111B
	ei
	out (c), a

	; read data

	ld c, p_vdp_data
	in h, (c)
	ld c, b

	; enter same address

	di
	out (c), l
	or  01000000B
	ei
	out (c), a

	ld a, (ix+8)
	out (p_vdp_data), a
#endasm
#endif            
}

void vwrite(void *source, u_int dest, u_int count)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm

	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp
  
	f_ldirvm .equ 0x0005C

	push hl
	push bc
	push de
	push ix

	ld l, 4(ix)
	ld h, 5(ix)
	ld e, 6(ix)
	ld d, 7(ix)
	ld c, 8(ix)
	ld b, 9(ix)

	ld ix, #f_ldirvm
	call callbios

	pop ix
	pop de
	pop bc
	pop hl

	pop ix           ;epilogue
	ret

__endasm;
#else
#asm
	f_ldirvm equ 05Ch

	push hl
	push bc
	push de
	push ix

	ld l, (ix+6)
	ld h, (ix+7)
	ld e, (ix+8)
	ld d, (ix+9)
	ld c, (ix+10)
	ld b, (ix+11)

	ld ix, f_ldirvm
	call callbios

	pop ix
	pop de
	pop bc
	pop hl
#endasm
#endif          
}

void vread(u_int source, void* dest, u_int count)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm

	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp
  
	f_ldirmv .equ 0x0056

	push hl
	push bc
	push de
	push ix

	ld l, 4(ix)
	ld h, 5(ix)
	ld e, 6(ix)
	ld d, 7(ix)
	ld c, 8(ix)
	ld b, 9(ix)

	ld ix, #f_ldirmv
	call callbios

	pop ix
	pop de
	pop bc
	pop hl

	pop ix           ;epilogue
	ret

__endasm;
#else
#asm
	f_ldirmv equ 056h

	push hl
	push bc
	push de
	push ix

	ld l, (ix+6)
	ld h, (ix+7)
	ld e, (ix+8)
	ld d, (ix+9)
	ld c, (ix+10)
	ld b, (ix+11)

	ld ix, f_ldirmv
	call callbios

	pop ix
	pop de
	pop bc
	pop hl
#endasm
#endif          
}

void locate(u_char x, u_char y)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm
	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp
  
	f_posit .equ 0x00C6
	
	push hl
	push ix

	ld h, 4(ix)
	ld l, 5(ix) ; --- TODO: controllare se va 6(ix) o 5(ix)
	
	ld ix, #f_posit
	call callbios

	pop ix
	pop hl

	pop ix           ;epilogue
	ret

__endasm;
#else
#asm
	f_posit equ 0C6h
	
	push hl
	push ix

	ld h, 6(ix)
	ld l, 8(ix)
	
	ld ix, f_posit
	call callbios

	pop ix
	pop hl
#endasm
#endif          
}

void pset(int x, int y) {
	vmerge(map_pixel(x,y), map_subpixel(x));
}

void set_char_form(char c, void* form, u_char place) {
	u_int addr = c;
	addr <<= 3;
	if (place & place_1) vwrite(form, addr, 8);
	if (place & place_2) vwrite(form, (256 * 8) + addr, 8);
	if (place & place_3) vwrite(form, (256 * 8 * 2) + addr, 8);
}

void set_char_attr(char c, void *attr, u_char place) {
	u_int addr = c;
	addr <<= 3;
	addr += MODE2_ATTR;

	if (place & place_1) vwrite(attr, addr, 8);
	if (place & place_2) vwrite(attr, (256 * 8) + addr, 8);
	if (place & place_3) vwrite(attr, (256 * 8 * 2) + addr, 8);
}

void set_char_color(char c, u_char color, u_char place) {
	u_int addr = c;
	addr <<= 3;
	addr += MODE2_ATTR;

	if (place & place_1) fill(addr, color, 8);
	if (place & place_2) fill((256 * 8) + addr, color, 8);
	if (place & place_3) fill((256 * 8 * 2) + addr, color, 8);
}

void set_char(char c, void* form, void *attr, u_char color, u_char place) {
	set_char_form(c, form, place);
	if (attr)
		set_char_attr(c, attr, place);
	else
		set_char_color(c, color, place);
}

void fill_v(u_int addr, u_char value, u_char count) {
	u_char diff;

	diff = addr & 7;
	if (diff) {
		diff = 8 - diff;
		if (diff > count)
			diff = count;
		fill(addr, value, diff);
		addr = (addr & ~(7)) + 256;
		count -= diff;
	}

	diff = count >> 3;
	while (diff--) {
		fill(addr, value, 8);
		addr += 256;
		count -= 8;	
	}

	if (count > 0)
		fill(addr, value, count);

}

u_char get_stick(u_char id)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm

	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp

	f_gtstck .equ 0x00D5

	push hl
	push de
	push bc
	push ix

	ld a, 4(ix)
	ld ix, #f_gtstck
	call callbios

	pop ix
	pop bc
	pop de
	pop hl
	
	ld l, a

	pop ix           ;epilogue
	ret

__endasm;
#else
#asm
	f_gtstck equ 0D5h

	push hl
	push de
	push bc
	push ix

	ld a, (ix+6)
	ld ix, f_gtstck
	call callbios

	pop ix
	pop bc
	pop de
	pop hl
	
	ld l, a
#endasm
#endif          
}

bool get_trigger(u_char id)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm

	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp

	f_gttrig .equ 0x00D8

	push ix
	ld a, 4(ix)

	ld ix, #f_gttrig
	call callbios
	pop ix
	ld l, a

	pop ix           ;epilogue
	ret

__endasm;
#else
#asm
	f_gttrig equ 0D8h

	push ix
	ld a, (ix+6)

	ld ix, f_gttrig
	call callbios
	pop ix
	ld l, a	
#endasm
#endif          
}

void set_sprite_mode(u_char mode) {
	u_char m = get_vdp(1);
	set_vdp(1, (m & 0xFC) | mode);

	_init_sprites();
}

void set_sprite_8(u_char handle, void* data) {
	vwrite(data, 14336 + (handle << 3), 8);
}

void set_sprite_16(u_char handle, void* data) {
	vwrite(data, 14336 + (handle << 5), 32);
}

void put_sprite_8(u_char id, int x, int y, u_char handle, u_char color) {
	sprite_t sp;
	if (x < 0) {
		x += 32;
		color |= 128;
	}
	sp.y = y - 1;
	sp.x = x;
	sp.handle = handle;
	sp.color = color;
	vwrite(&sp, 6912 + (id << 2), 4);
}

void put_sprite_16(u_char id, int x, int y, u_char handle, u_char color) {
	sprite_t sp;
	if (x < 0) {
		x += 32;
		color |= 128;
	}
	sp.y = y - 1;
	sp.x = x;
	sp.handle = (handle << 2);
	sp.color = color;
	vwrite(&sp, 6912 + (id << 2), 4);
}

void blit_ram_vram(u_char* source, u_int dest, u_char w, u_char h, int sjmp, int djmp) {
	while (h--) {
		vwrite(source, dest, w);
		source += sjmp;
		dest += djmp;		
	}
}

void blit_fill_vram(u_int dest, u_char value, u_char w, u_char h, int djmp) {
	while (h--) {
		fill(dest, value, w);
		dest += djmp;		
	}
}

/* unfinished
void blit_ram_vram(surface_t *source, surface_t *dest, rect_t *from, rect_t *to) {

	int s_jmp, d_jmp, h;
	u_char* s_addr;
	u_int   d_addr;

	s_jmp = source->width - from->width;
	d_jmp = dest->width - from->width;
	h = from->height;

	s_addr = 0; // ?
	d_addr = 0; // ?
}

void blit(surface_t *source, surface_t *dest, rect_t *from, rect_t *to) {
	// one can always dream :)	
}

*/

void psg_init() {
#ifdef __SDCC
__asm
	f_gicini .equ 0x0090

	push ix
	ld ix, #f_gicini
	call callbios
	pop ix
__endasm;
#else
#asm
	f_gicini equ 090h

	push ix
	ld ix, f_gicini
	call callbios
	pop ix
#endasm
#endif          
}


void psg_set(u_char reg, u_char value)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC  
/*
	WRTPSG (0093H)		*1
	Function:  writes data in the PSG register
	Input:     A for PSG register number, E for data
	Output:    none
	Registers: none

	RDPSG (0096H)		*1
	Function:  reads the PSG register value
	Input:     A for PSG register number
	Output:    A for the value which was read
	Registers: none
*/
__asm
	push	ix  ; prologue
	ld	ix, #0x0000
	add	ix, sp

	f_wrtpsg .equ 0x0093
	f_rdpsg  .equ 0x0096

        push af
	push de
	ld a, 4(ix)          
        cp #0x07
        jr nz, 00001$ ; if not reg 7 then perform the usual wrtpsg call ...
          
	push ix
	ld ix, #f_rdpsg ; ... else mask bits 6 and 7 of register 7
	call callbios
	pop ix

        and #0xc0
	ld d, a ; save bits 6 and 7 in reg d
	ld a, 5(ix)
	and #0x3f
	or d
	ld e, a ; e <- { 4(ix) masked by d }
	ld a, #0x07
	jr 00002$                     
                        
00001$:	ld e, 5(ix)
00002$:	push ix
	ld ix, #f_wrtpsg
	call callbios
	pop ix

	pop de
	pop af

	pop ix           ;epilogue
	ret

__endasm;
#else
#asm
	f_wrtpsg equ 093h
	f_rdpsg  equ 096h

	push de
	push ix
	ld a, (ix + 8)
	ld e, a
	ld a, (ix + 6)
	ld ix, f_wrtpsg
	call callbios
	pop ix
	pop de
#endasm
#endif          
}

u_char psg_get(u_char reg)
#ifdef __SDCC
  __naked
#endif
{
#ifdef __SDCC
__asm
	push	ix  	; prologue
	ld	ix,#0
	add	ix,sp
  
	f_rdpsg .equ 0x0096

	ld a, 4(ix)
	push ix
	ld ix, #f_rdpsg
	call callbios
	pop ix

        ld h, #0x00
        ld l, a

	pop ix           ;epilogue
	ret
__endasm;
#else
#asm
	f_rdpsg equ 096h

	push ix
	ld a, (ix + 6)
	ld ix, f_rdpsg
	call callbios
	pop ix
#endasm
#endif          
}

void psg_tone(u_char channel, int period) {
	channel <<= 1;
	psg_set(channel, period & 255);
	psg_set(channel + 1, period >> 8);
}

void psg_noise(u_char period) {
	psg_set(6, period & 31);
}

void psg_volume(u_char channel, u_char volume) {
	psg_set(channel + 8, volume & 15);
}

void psg_envelope(u_char waveform, int period, u_char channels) {
	psg_set(13, waveform);
	psg_set(11, period & 255);
	psg_set(12, period >> 8);
	if (channels & 1)
		psg_set(8, 16);
	if (channels & 2)
		psg_set(9, 16);
	if (channels & 4)
		psg_set(10, 16);
	// FIXME: perhaps we should mute all others?
}

void psg_channels(u_char tone_channels, u_char noise_channels) {
	psg_set(7, (tone_channels << 3) | noise_channels);
}

u_char psg_noise_channels() {
	return psg_get(7) & 7;
}

u_char psg_tone_channels() {
	return (psg_get(7) >> 3) & 7;
}

void psg_init_tone_table(int tones[128]) {
	// this is not the most precise method of calculation
	// but it is fast!
	// also, the accumulated error is within a safe margin
	int c;
	double n = 8.1757989156;
	double s = 1.0594630943;
	tones[0] = psgT(n);
	for (c = 1; c < 128; c++) {
		n *= s;
		tones[c] = psgT(n);
	}
}

// TODO: reg 7 masking

