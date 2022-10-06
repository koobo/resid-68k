;APS00000000000000000000000000000000000000000000000000000000000000000000000000000000

;  ---------------------------------------------------------------------------
;  This is a MC680x0 assembler port of reSID, a MOS6581 SID emulator
;  engine, copyright (C) 2004 Dag Lem <resid@nimrod.no>
;
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software
;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;  ---------------------------------------------------------------------------


    incdir  include:
    include resid-68k.i

    * Launch and run a few cycles
sid_main:     
    lea     Sid,a0
    jsr	    sid_constructor
    moveq   #1,d0
    lea     Sid,a0
    jsr     sid_enable_filter
    moveq   #1,d0
    lea     Sid,a0
    jsr     sid_enable_external_filter

    bra     .pokeOceanLoaderV3

    * select voice
    move    #0*7,d7
    bsr     .pokeSound

    * Request bytes, convert to cycles
    lea     Sid,a0
    ;move.l  #200,d0
    ;mulu.l  sid_cycles_per_sample(a0),d0
    ;bvs     .overflow
    ;swap    d0
    ;ext.l   d0

    lea     Sid,a0
    lea     output,a1
    move.l  #100000,d0  * cycles (upper limit)
 ;   move.l  #1000,d1 * buffer limit
    move.l  #1000,d1   * get this many bytes
    jsr     sid_clock_fast
.overflow
    rts

	
.pokeSound
 REM
    * filter cutoff low
    move.b  #0,d0
    move.b  #$15,d1
    lea     Sid,a0
    jsr     sid_write

    * filter cutoff hi
    move.b  #1,d0
    move.b  #$16,d1
    lea     Sid,a0
    jsr     sid_write

    * filter resonance and routing
    * 0..3: enable filter for voice 1,2,3
    * 4..7: resonance
    move.b  #%111+%111<<4,d0
    move.b  #$17,d1
    lea     Sid,a0
    jsr     sid_write
 EREM
    * filter mode and main volume control
    * 0..3: main volume
    * 4: low pass
    * 5: band pass
    * 6: high pass
    * 7: mute voice 3
    move.b  #$f+%000<<4,d0
    move.b  #24,d1
    lea     Sid,a0
    jsr     sid_write

;A = 2^(1/12)
;F0 = 7454 note A4
;N = -9
;NF = F0*A^N
;FH = NF/256
;FL = NF-256*FH
.F = 4432
    * voice 1: freq lo
    move.b  #.F&$ff,d0
    move.b  #0,d1
    lea     Sid,a0
    add.b   d7,d1
    jsr     sid_write

    * voice 1: freq hi
    move.b  #.F>>8,d0
    move.b  #1,d1
    lea     Sid,a0
    add.b   d7,d1
    jsr     sid_write

  * voice 1: pw lo
    move.b  #0,d0
    move.b  #2,d1
    add.b   d7,d1
    lea     Sid,a0
    jsr     sid_write

    * voice 1: pw hi
    move.b  #1,d0
    move.b  #3,d1
    add.b   d7,d1
    lea     Sid,a0
    jsr     sid_write
    
    * voice 1: attack=5, decay=3
    move.b  #13*16+5,d0
    move.b  #5,d1
    lea     Sid,a0
    add.b   d7,d1
    jsr     sid_write

    * voice 1: sustain=15, release=10
    move.b  #12*16+10,d0
    move.b  #6,d1
    lea     Sid,a0
    add.b   d7,d1
    jsr     sid_write

    * global volume full
    move.b  #15,d0
    move.b  #24,d1
    lea     Sid,a0
    jsr     sid_write

    * voice 1 control
    * Set triangle waveform, set gate bit
    * 0 gate
    * 1: sync with voice 3
    * 2: ring with voice 3
    * 3: test
    * 4: triangle
    * 5: sawtooth
    * 6: pulse
    * 7: noise
    move.b  #1<<6+1,d0
    move.b  #4,d1
    lea     Sid,a0
    add.b   d7,d1
    jsr     sid_write
    rts

.pokeRelease
    * voice 1: Set triangle waveform, clear gate bit
    move.b  #16+0,d0
    move.b  #4,d1
    lea     Sid,a0
    add.b   d7,d1
    jsr     sid_write
    rts

.pokeOceanLoaderV3
;          0000 0008 0000 0000 0000 181f 0000 17f2  ................
;000000c0: 0000 1208 0000 0e16 0000 0f0d 0000 1000  ................
;000000d0: 0000 1108 0000 1301 0000 14a9 0000 1221  ...............!
;000000e0: 0002 1220 0006 1208 0006 0e16 0006 0f0d  ... ............
;000000f0: 0006 1000 0006 1108 0006 1301 0006 14a9  ................
;00000100: 0006 1221 0008 1220 000c 1208 000c 0e16  ...!... ........
;00000110: 000c 0f0d 000c 1000 000c 1108 000c 1301  

    move.b  #$1f,d0
    move.b  #$18,d1
    bsr     .poke
    move.b  #$f2,d0
    move.b  #$17,d1
    bsr     .poke
    move.b  #$08,d0
    move.b  #$12,d1
    bsr     .poke
    move.b  #$16,d0
    move.b  #$0e,d1
    bsr     .poke
    move.b  #$0d,d0
    move.b  #$0f,d1
    bsr     .poke
    move.b  #$00,d0
    move.b  #$10,d1
    bsr     .poke
    move.b  #$08,d0
    move.b  #$11,d1
    bsr     .poke
    move.b  #$01,d0
    move.b  #$13,d1
    bsr     .poke
    move.b  #$a9,d0
    move.b  #$14,d1
    bsr     .poke
    move.b  #$21,d0     * gate on
    move.b  #$12,d1
    bsr     .poke

    move.l  #500,d1
    bsr     .getD1Bytes

    move.b  #$20,d0     * gate off
    move.b  #$12,d1
    bsr     .poke

    move.l  #500,d1
    bsr     .getD1Bytes
    rts

.poke
    lea     Sid,a0
    add.b   d7,d1
    jsr     sid_write
    rts

.getD1Bytes
    lea     Sid,a0
    lea     output,a1
    move.l  #100000,d0  * cycles (upper limit)
 ;   move.l  #1000,d1 * buffer limit
;    move.l  #1000,d1   * get this many bytes
    jsr     sid_clock_fast
    rts

output	ds.b	1000

    section code,code

******************************************************************************
*
* Wave
*
******************************************************************************

* in:
*   a0 = object
wave_constructor:
    move.l  a0,wave_sync_source(a0)
    moveq   #CHIP_MODEL_MOS6581,d0
    bsr     wave_set_chip_model
    bsr     wave_reset
    rts

* in:
*   a0 = object
*   a1 = wave pointer
wave_set_sync_source
    move.l  a1,wave_sync_source(a0)
    move.l  a0,wave_sync_dest(a1)
    rts

* in:
*   a0 = object
*   d0 = chip model
wave_set_chip_model
    cmp.b   #CHIP_MODEL_MOS6581,d0
    bne.b   .1
    move.l  #wave6581__ST,wave_wave__ST(a0)
    move.l  #wave6581_P_T,wave_wave_P_T(a0) 
    move.l  #wave6581_PS_,wave_wave_PS_(a0) 
    move.l  #wave6581_PST,wave_wave_PST(a0)
    bra     .x 
.1
    move.l  #wave8580__ST,wave_wave__ST(a0)
    move.l  #wave8580_P_T,wave_wave_P_T(a0) 
    move.l  #wave8580_PS_,wave_wave_PS_(a0) 
    move.l  #wave8580_PST,wave_wave_PST(a0)
.x
    rts


* in:
*   a0 = object
*   d0 = freq_lo
wave_writeFREQ_LO:
    move    wave_freq(a0),d1
    and     #$ff00,d1
    or.b    d0,d1
    move    d1,wave_freq(a0)
    rts


* in:
*   a0 = object
*   d0 = freq_hi
wave_writeFREQ_HI:
    lsl     #8,d0
    move    wave_freq(a0),d1
    and     #$00ff,d1
    or      d0,d1
    move    d1,wave_freq(a0)
    rts


* in:
*   a0 = object
*   d0 = pw_lo
wave_writePW_LO:
    move    wave_pw(a0),d1
    and     #$0f00,d1
    or.b    d0,d1
    move    d1,wave_pw(a0)
    rts

* in:
*   a0 = object
*   d0 = pw_hi
wave_writePW_HI:
    lsl     #8,d0
    and     #$0f00,d0
    move    wave_pw(a0),d1
    and     #$00ff,d1
    or      d0,d1
    move    d1,wave_pw(a0)
    rts

* in:
*   a0 = object
*   d0 = control
wave_writeCONTROL_REG:
    move.b  d0,d1
    lsr.b   #4,d1
    move.b  d1,wave_waveform(a0)

    moveq   #$04,d1
    and.b  d0,d1
    move.b  d1,wave_ring_mod(a0)

    moveq   #$02,d1
    and.b  d0,d1
    move.b  d1,wave_sync(a0)

    and.b   #$08,d0
    beq     .noTestNext
    clr.l   wave_accumulator(a0)
    clr.l   wave_shift_register(a0)
    bra     .1
.noTestNext
    tst.b   wave_test(a0)
    beq.b   .1
    move.l  #$7ffff8,wave_shift_register(a0)
.1
    move.b  d0,wave_test(a0)
    rts

* in:
*   a0 = object
* out:
*   d0 = output
wave_readOSC:
    bsr     wave_output
    lsr     #4,d0
    rts

* in:
*   a0 = object
wave_reset
    clr.l   wave_accumulator(a0)
    move.l  #$7ffff8,wave_shift_register(a0)
    clr.w   wave_freq(a0)
    clr.w   wave_pw(a0)
    clr.b   wave_test(a0)
    clr.b   wave_ring_mod(a0)
    clr.b   wave_sync(a0)
    clr.b   wave_msb_rising(a0)
    rts


* in:
*   a0 = object
*   d0 = cycle_count delta_t
* uses:
*   d0-d6,a0
wave_clock:
    tst.b   wave_test(a0)
    beq     .go
    rts
.go

    * d1 = accumulator_prev
    move.l  wave_accumulator(a0),d1
    move.l  d1,d2

    * d3 = delta_accumulator
    move    wave_freq(a0),d3
    mulu.w  d0,d3
  
    * d2 = accumulator
    add.l   d3,d2
    and.l   #$ffffff,d2
    move.l  d2,wave_accumulator(a0)

    clr.b   wave_msb_rising(a0)
    btst    #23,d1      * previous MSB
    bne     .noMsb
    btst    #23,d2      * current MSB
    beq     .noMsb
    st      wave_msb_rising(a0)
.noMsb

    move.l  #$100000,d1

    * d0 = delta_t
    * d1 = shift period
    * d2 = accumulator
    * d3 = delta_accumulator

.loop
    tst.l   d3
    beq.b   .break

    cmp.l   d1,d3
    bhs.b   .continue

    * shift_period = delta_accumulator
    move.l  d3,d1

    cmp.l   #$80000,d1 
    bhi     .else
    move.l  d2,d4
    sub.l   d1,d4
    and.l   #$80000,d4
    bne     .break
    ;move.l  d2,d4
    ;and.l   #$80000,d4
    btst    #19,d2
    beq     .break
    bra     .continue
.else
    move.l  d2,d4
    sub.l   d1,d4
    and.l   #$80000,d4
    beq     .continue
    ;move.l  d2,d4
    ;and.l   #$80000,d4
    btst    #19,d2
    beq     .break

.continue
    * Shift the noise/random register.  
    * bit0
    move.l  wave_shift_register(a0),d4
    move.l  d4,d6
    clr.w   d4
    swap    d4
    move.w  d4,d5
    lsr.w   #6,d4
    lsr.w   #1,d5
    eor.w   d4,d5
    and     #$1,d5

    add.l   d6,d6
    and.l   #$7fffff,d6
    or.b    d5,d6
    move.l  d6,wave_shift_register(a0)


    sub.l   d1,d3
    bra     .loop

.break
    rts


* in:
*   a0 = object
* uses:
*   d0,d1,a0,a1,a2
wave_synchronize:
    tst.b   wave_msb_rising(a0)
    beq.b   .x
    move.l  wave_sync_dest(a0),a1
    tst.b   wave_sync(a1)
    beq.b   .x

    tst.b   wave_sync(a0)
    beq     .y
    move.l  wave_sync_source(a0),a2
    tst.b   wave_msb_rising(a2)
    bne     .x
.y
    clr.l   wave_accumulator(a1)
.x
    rts

* in:
*   a0 = object
* out:
*   d0 = Waveform output 12 bits
* uses: 
*   d0,d1,d2,a0,a1
wave_output:
    moveq   #0,d0
    move.b  wave_waveform(a0),d0
    move.w  .tab(pc,d0.w*2),d0
    jmp     .tab(pc,d0.w)

.tab
    dc.w     wave_output____-.tab
    dc.w     wave_output___T-.tab
    dc.w     wave_output__S_-.tab
    dc.w     wave_output__ST-.tab
    dc.w     wave_output_P__-.tab
    dc.w     wave_output_P_T-.tab
    dc.w     wave_output_PS_-.tab
    dc.w     wave_output_PST-.tab
    dc.w     wave_outputN___-.tab
    dc.w     wave_outputN__T-.tab
    dc.w     wave_outputN_S_-.tab
    dc.w     wave_outputN_ST-.tab
    dc.w     wave_outputNP__-.tab
    dc.w     wave_outputNP_T-.tab
    dc.w     wave_outputNPS_-.tab
    dc.w     wave_outputNPST-.tab
 

wave_output____:
    moveq   #0,d0
    rts
wave_output___T:
    move.l  wave_accumulator(a0),d0
    move.l  d0,d1
    tst.b   wave_ring_mod(a0)
    beq.b   .noRingMod
    move.l  wave_sync_source(a0),a1
    move.l  wave_accumulator(a1),d2
    eor.l   d2,d1
.noRingMod
    and.l   #$800000,d1
    beq     .noMsb
    not.l   d0
.noMsb
    lsr.l   #8,d0
    lsr.w   #3,d0
    and     #$0fff,d0
    rts
wave_output__S_:
    move.l  wave_accumulator(a0),d0
    lsr.l   #8,d0
    lsr.w   #4,d0
    rts
wave_output__ST:
    bsr     wave_output__S_
    moveq   #0,d1
    move.b  ([(wave_wave__ST).w,a0],d0.w),d1
    lsl.w   #4,d1
    move.w  d1,d0
    rts
wave_output_P__:
    tst.b   wave_test(a0)
    bne.b   .do
    move.l  wave_accumulator(a0),d0
    lsr.l   #8,d0
    lsr.w   #4,d0
    cmp     wave_pw(a0),d0
    bhs     .do
    moveq   #0,d0
    rts
.do
    move    #$0fff,d0
    rts
wave_output_P_T:
    bsr     wave_output___T
    move    d0,d1
    lsr.w   #1,d1
    moveq   #0,d2
    move.b  ([(wave_wave_P_T).w,a0],d1.w),d2
    lsl     #4,d2
    bsr     wave_output_P__
    and     d2,d0
    rts
wave_output_PS_:
    bsr     wave_output__S_
    moveq   #0,d1
    move.b  ([(wave_wave_PS_).w,a0],d0.w),d1
    lsl.w   #4,d1
    bsr     wave_output_P__
    and     d1,d0
    rts
wave_output_PST:
    bsr     wave_output__S_
    moveq   #0,d1
    move.b  ([(wave_wave_PST).w,a0],d0.w),d1
    lsl.w   #4,d1
    bsr     wave_output_P__
    and     d1,d0
    rts
wave_outputN___:
    moveq   #0,d0
    move.l  wave_shift_register(a0),d1

    move.l  d1,d2
    and.l   #$400000,d2
    lsr.l   #8,d2
    lsr.w   #3,d2
    or.w    d2,d0

    move.l  d1,d2
    and.l   #$100000,d2
    lsr.l   #8,d2
    lsr.w   #2,d2
    or.w    d2,d0

    move.l  d1,d2
    and.l   #$010000,d2
    lsr.l   #7,d2
    or.w    d2,d0

    move.w  d1,d2
    and.w   #$002000,d2
    lsr.w   #5,d2
    or.w    d2,d0

    move.w  d1,d2
    and.w   #$000800,d2
    lsr.w   #4,d2
    or.w    d2,d0

    move.w  d1,d2
    and.w   #$000080,d2
    lsr.w   #1,d2
    or.w    d2,d0

    move.w  d1,d2
    and.w   #$000010,d2
    lsl.w   #1,d2
    or.w    d2,d0

    and.w   #$000004,d1
    lsl.w   #1,d1
    or.w    d1,d0
    rts

wave_outputN__T:
wave_outputN_S_:
wave_outputN_ST:
wave_outputNP__:
wave_outputNP_T:
wave_outputNPS_:
wave_outputNPST:
    moveq   #0,d0
    rts


******************************************************************************
*
* Voice
*
******************************************************************************

* in:
*    a0 = object
voice_constructor:
    push    a0
    move.l  voice_wave(a0),a0
    bsr     wave_constructor
    move.l  (sp),a0
    move.l  voice_envelope(a0),a0
    bsr     envelope_constructor
    pop     a0

    moveq   #CHIP_MODEL_MOS6581,d0
    bsr     voice_set_chip_model
    rts

* in:
*    a0 = object
*    d0 = chip model
voice_set_chip_model:
    push    a0
    move.l  voice_wave(a0),a0
    bsr     wave_set_chip_model
    pop     a0

    cmp.b    #CHIP_MODEL_MOS6581,d0
    bne.b    .1
    move.w   #$380,voice_wave_zero(a0)
    move.l   #$800*$ff,voice_voice_DC(a0)
    bra      .2
.1
    move.w   #$800,voice_wave_zero(a0)
    move.l   #0,voice_voice_DC(a0)
.2
    rts

* in:
*    a0 = object
*    a1 = voice pointer
voice_set_sync_source:
    move.l  voice_wave(a0),a0
    move.l  voice_wave(a1),a1
    bsr     wave_set_sync_source
    rts

* in:
*    a0 = object
*    d0 = reg8 control
voice_writeCONTROL_REG:
    pushm   d0/a0
    move.l  voice_wave(a0),a0
    bsr     wave_writeCONTROL_REG
    popm    d0/a0
    move.l  voice_envelope(a0),a0
    bsr     envelope_writeCONTROL_REG
    rts


* in:
*    a0 = object
voice_reset:
    push    a0
    move.l  voice_wave(a0),a0
    bsr     wave_reset
    pop     a0
    move.l  voice_envelope(a0),a0
    bsr     envelope_reset
    rts

* in:
*    a0 = object
* out:
*    d0 = Amplitude modulated waveform output.
*         Ideal range [-2048*255, 2047*255].
* uses:
*    d0-d3,a0,a1,(a5)
voice_output:
    push    a5
    move.l  a0,a5
    move.l  voice_envelope(a5),a0
    bsr     envelope_output
    * d0 = 8-bit
    move.w  d0,d3
    move.l  voice_wave(a5),a0
    bsr     wave_output
    * d0 = 12-bit
    sub.w   voice_wave_zero(a5),d0
    muls.w  d3,d0
    * d0 = 20-bit
    add.l   voice_voice_DC(a5),d0
    pop     a5
    rts


******************************************************************************
*
* Envelope
*
******************************************************************************

* in:
*    a0 = object
envelope_constructor:
    bsr     envelope_reset
    rts

* in:
*    a0 = object
envelope_reset:
    clr.b   envelope_attack(a0)
    clr.b   envelope_decay(a0)
    clr.b   envelope_sustain(a0)
    clr.b   envelope_release(a0)
    clr.b   envelope_gate(a0)
    clr.w   envelope_rate_counter(a0)
    clr.b   envelope_exponential_counter(a0)
    move.b  #1,envelope_exponential_counter_period(a0)
    move.b  #envelope_state_RELEASE,envelope_state(a0)
    lea     envelope_rate_counter_period(pc),a1
    move    (a1),envelope_rate_period(a0) * offset at "release", ie. zero here
    st      envelope_hold_zero(a0)
    rts

* in:
*    a0 = object
*    d0 = control
* uses:
*    d0,d1,d2,a0,a1
envelope_writeCONTROL_REG:
    * gate_next
    and.b   #$01,d0
    beq.b   .1
    tst.b   envelope_gate(a0)
    bne.b   .1
    move.b  #envelope_state_ATTACK,envelope_state(a0)
    moveq   #0,d1
    move.b  envelope_attack(a0),d1
    lea     envelope_rate_counter_period(pc),a1
    move.w  (a1,d1.w*2),envelope_rate_period(a0)
    clr.b   envelope_hold_zero(a0)
    bra     .2
.1
    tst.b   d0
    bne     .2
    tst.b   envelope_gate(a0)
    beq.b   .2
    move.b  #envelope_state_RELEASE,envelope_state(a0)
    moveq   #0,d1
    move.b  envelope_release(a0),d1
    lea     envelope_rate_counter_period(pc),a1
    move.w  (a1,d1.w*2),envelope_rate_period(a0)
.2
    move.b  d0,envelope_gate(a0)
    rts


* in:
*    a0 = object
*    d0 = attack_decay
envelope_writeATTACK_DECAY:
    moveq   #0,d1
    move.b  d0,d1
    lsr.b   #4,d1
    move.b  d1,envelope_attack(a0)
    and     #$f,d0
    move.b  d0,envelope_decay(a0)

    cmp.b   #envelope_state_ATTACK,envelope_state(a0)
    bne.b   .1
    lea     envelope_rate_counter_period(pc),a1
    move    (a1,d1.w*2),envelope_rate_period(a0)
    rts
.1  
    cmp.b   #envelope_state_DECAY_SUSTAIN,envelope_state(a0)
    bne.b   .2
    lea     envelope_rate_counter_period(pc),a1
    move    (a1,d0.w*2),envelope_rate_period(a0)
.2
    rts


* in:
*    a0 = object
*    d0 = sustain_release
envelope_writeSUSTAIN_RELEASE:
    move.b  d0,d1
    lsr.b   #4,d1
    move.b  d1,envelope_sustain(a0)
    and     #$f,d0
    move.b  d0,envelope_release(a0)
    cmp.b   #envelope_state_RELEASE,envelope_state(a0)
    bne.b   .1
    lea     envelope_rate_counter_period(pc),a1
    move    (a1,d0.w*2),envelope_rate_period(a0)
.1
    rts

* in:
*    a0 = object
envelope_readENV:
    ; ... fall through

* in:
*    a0 = object
* out:
*    d0 = 8-bit envelope output
* uses:
*    d0, a0
envelope_output:
    moveq   #0,d0
    move.b  envelope_counter(a0),d0
    rts


* in:
*   a0 = object
*   d0 = cycle_count delta_t
* uses:
*   d0,d1,d2,a0,a1
envelope_clock:
    * d1 = rate step
    move.w  envelope_rate_period(a0),d1
    sub.w   envelope_rate_counter(a0),d1
    bgt     .overZero
    add.w   #$7fff,d1
.overZero

.loop
    tst.w   d0
    beq     .x

    cmp.w   d1,d0
    bhs     .2
    move.w  envelope_rate_counter(a0),d2
    add.w   d0,d2
    bpl.b   .3
    addq.w  #1,d2
    and.w   #$7fff,d2
.3
    move.w  d2,envelope_rate_counter(a0)  
    rts
.2

    clr.w   envelope_rate_counter(a0)
    sub.w   d1,d0

    cmp.b   #envelope_state_ATTACK,envelope_state(a0)
    beq     .4
    addq.b  #1,envelope_exponential_counter(a0)
    move.b  envelope_exponential_counter_period(a0),d2
    cmp.b   envelope_exponential_counter(a0),d2
    bne     .continueLoop
.4
    clr.b   envelope_exponential_counter(a0)
    tst.b   envelope_hold_zero(a0)
    bne     .continueLoop

    ; switch #1
    cmp.b   #envelope_state_ATTACK,envelope_state(a0)
    bne     .notAttack
    addq.b  #1,envelope_counter(a0)
    ;and.b   #$ff,envelope_counter(a0) this is a no-op
    cmp.b   #$ff,envelope_counter(a0)
    bne     .break1
    move.b  #envelope_state_DECAY_SUSTAIN,envelope_state(a0)
    moveq   #0,d2
    move.b  envelope_decay(a0),d2
    lea     envelope_rate_counter_period(pc),a1
    move.w  (a1,d2.w*2),envelope_rate_period(a0)
    bra     .break1
.notAttack
    cmp.b   #envelope_state_DECAY_SUSTAIN,envelope_state(a0)
    bne     .notDS
    moveq   #0,d2
    move.b  envelope_sustain(a0),d2
    lea     envelope_sustain_level(pc),a1
    move.b  (a1,d2.w),d2
    cmp.b   envelope_counter(a0),d2
    beq     .break1
    subq.b  #1,envelope_counter(a0)
    bra     .break1
.notDS
    ; No need to check envelope_state(a0), 
    ; the remaining one is RELEASE.
    subq.b  #1,envelope_counter(a0)
    and.b   #$ff,envelope_counter(a0)
.break1

    ; switch #2
    move.b  envelope_counter(a0),d2
    cmp.b   #$ff,d2
    bne.b   .n1
    move.b  #1,envelope_exponential_counter_period(a0)
    bra     .break2
.n1
    cmp.b   #$5d,d2
    bne.b   .n2
    move.b  #2,envelope_exponential_counter_period(a0)
    bra     .break2
.n2
    cmp.b   #$36,d2
    bne.b   .n3
    move.b  #4,envelope_exponential_counter_period(a0)
    bra     .break2
.n3
    cmp.b   #$1a,d2
    bne.b   .n4
    move.b  #8,envelope_exponential_counter_period(a0)
    bra     .break2
.n4
    cmp.b   #$0e,d2
    bne.b   .n5
    move.b  #16,envelope_exponential_counter_period(a0)
    bra     .break2
.n5
    cmp.b   #$06,d2
    bne.b   .n6
    move.b  #30,envelope_exponential_counter_period(a0)
    bra     .break2
.n6
    cmp.b   #$00,d2
    bne.b   .n7
    move.b  #1,envelope_exponential_counter_period(a0)
    st      envelope_hold_zero(a0)
.n7
.break2


.continueLoop
    move.w  envelope_rate_period(a0),d1
    bra     .loop
.x

    rts

envelope_sustain_level:
  dc.b $00
  dc.b $11
  dc.b $22
  dc.b $33
  dc.b $44
  dc.b $55
  dc.b $66
  dc.b $77
  dc.b $88
  dc.b $99
  dc.b $aa
  dc.b $bb
  dc.b $cc
  dc.b $dd
  dc.b $ee
  dc.b $ff

envelope_rate_counter_period:
  dc.w      9
  dc.w     32
  dc.w     63
  dc.w     95
  dc.w    149
  dc.w    220
  dc.w    267
  dc.w    313
  dc.w    392
  dc.w    977
  dc.w   1954
  dc.w   3126
  dc.w   3907
  dc.w  11720
  dc.w  19532
  dc.w  31251


******************************************************************************
*
* Filter
*
******************************************************************************

* in:
*    a0 = object
filter_constructor:
    clr.w   filter_fc(a0)
    clr.b   filter_res(a0)
    clr.b   filter_filt(a0)
    clr.b   filter_voice3off(a0)
    clr.b   filter_hp_bp_lp(a0)
    clr.b   filter_vol(a0)
    clr.l   filter_Vhp(a0)
    clr.l   filter_Vbp(a0)
    clr.l   filter_Vlp(a0)
    clr.l   filter_Vnf(a0)

    * Precalc constants for filter_set_w0
    fmove.s #3.1415926535897932385,fp0  * pi
    fmove   fp0,fp1
    fmul.s  #2*16000*1.048576,fp0
    fmul.s  #2*4000*1.048576,fp1
    fmove.l fp0,filter_w0_max_1(a0)
    fmove.l fp1,filter_w0_max_dt(a0)

    moveq   #1,d0
    bsr     filter_enable_filter

    moveq   #CHIP_MODEL_MOS6581,d0
    bsr     filter_set_chip_model
    rts

* in:
*    a0 = object
*    d0 = true to enable, false to disable
filter_enable_filter
    move.b  d0,filter_enabled(a0)
    rts

* in:
*    a0 = object
*    d0 = chip model
filter_set_chip_model
    cmp.b   #CHIP_MODEL_MOS6581,d0
    bne     .1

    move.l  #(-$fff*$ff/18)>>7,filter_mixer_DC(a0)
    move.l  #filter_f0_6581,filter_f0(a0)
    bra     .2
.1
    clr.l   filter_mixer_DC(a0)
    move.l  #filter_f0_8580,filter_f0(a0)
.2
    bsr     filter_set_w0
    bsr     filter_set_Q
    rts


* in:
*    a0 = object
filter_reset:
    clr.w   filter_fc(a0)
    clr.b   filter_res(a0)
    clr.b   filter_filt(a0)
    clr.b   filter_voice3off(a0)
    clr.b   filter_hp_bp_lp(a0)
    clr.b   filter_vol(a0)
    clr.l   filter_Vhp(a0)
    clr.l   filter_Vbp(a0)
    clr.l   filter_Vlp(a0)
    clr.l   filter_Vnf(a0)
    bsr     filter_set_w0
    bsr     filter_set_Q
    rts

* in:
*    a0 = object
*    d0 = fc_lo
* uses:
*    d0,d1,d2,a0,a1
filter_writeFC_LO:
    move    filter_fc(a0),d1
    and     #$7f8,d1
    and     #7,d0
    or      d0,d1
    move    d1,filter_fc(a0)
    bsr     filter_set_w0
    rts


* in:
*    a0 = object
*    d0 = fc_hi
*    d0,d1,d2,a0,a1
filter_writeFC_HI:
    lsl.w   #3,d0
    and     #$7f8,d0
    moveq   #7,d1
    and     filter_fc(a0),d1
    or      d1,d0
    move    d0,filter_fc(a0)
    bsr     filter_set_w0
    rts
    
    
* in:
*    a0 = object
*    d0 = res_filt
* uses:
*    d0,d1,a0,fp0
filter_writeRES_FILT:
    move.b  d0,d1
    lsr.b   #4,d1
    move.b  d1,filter_res(a0)
    and     #$f,d0
    move.b  d0,filter_filt(a0)
    bsr     filter_set_Q
    rts
    

* in:
*    a0 = object
*    d0 = mode_vol
filter_writeMODE_VOL:
    tst.b   d0
    smi     filter_voice3off(a0)

    move.b  d0,d1
    lsr.b   #4,d1
    and     #7,d1
    move.b  d1,filter_hp_bp_lp(a0)

    and     #$f,d0
    move.b  d0,filter_vol(a0)
    rts
    
* in:
*    a0 = object
* uses:
*    d0,d1,a0,fp0
filter_set_w0:
    move.w  filter_fc(a0),d0
    fmove.w ([(filter_f0).w,a0],d0.w*2),fp0
    fmul.s  #2*3.1415926535897932385*1.048576,fp0
    fmove.l fp0,d0
    move.l  d0,filter_w0(a0)
    * d0 = w0

    move.l  d0,filter_w0_ceil_1(a0)
    move.l  filter_w0_max_1(a0),d1
    cmp.l   d1,d0
    bls.b   .1
    move.l  d1,filter_w0_ceil_1(a0)
.1
    move.l  d0,filter_w0_ceil_dt(a0)
    move.l  filter_w0_max_dt(a0),d1
    cmp.l   d1,d0
    bls.b   .2
    move.l  d1,filter_w0_ceil_dt(a0)
.2
    rts

    
* in:
*    a0 = object
filter_set_Q:
    fmove.b filter_res(a0),fp0
    fdiv.w  #$f,fp0
    fadd.s  #0.707,fp0
    fmove.s #1024,fp1
    fdiv    fp0,fp1
    fmove.l fp1,filter_1024_div_Q(a0)
    rts


* in:
*    a0 = object
*    d0 = cycle_count delta_t
*    d1 = voice1 sample
*    d2 = voice2 sample
*    d3 = voice3 sample
*    d4 = ext_in sample
* uses:
*    d0-d5,a0
filter_clock:
    lsr.l   #7,d1
    lsr.l   #7,d2
    lsr.l   #7,d4

    tst.b   filter_voice3off(a0)
    beq.b   .1
    moveq   #4,d5
    and.b   filter_filt(a0),d5
    bne.b   .1
    clr.l   d3
    bra     .2
.1
    lsr.l   #7,d3
.2

    tst.b   filter_enabled(a0)
    bne.b   .3
    add.l   d4,d1
    add.l   d3,d1
    add.l   d2,d1
    move.l  d1,filter_Vnf(a0)
    clr.l   filter_Vhp(a0)    
    clr.l   filter_Vbp(a0)    
    clr.l   filter_Vlp(a0)    
    rts
.3
    moveq   #0,d5
    move.b  filter_filt(a0),d5
    move.w  .tab(pc,d5.w*2),d5
    jmp     .tab(pc,d5)
    
.tab    
    dc.w    .f0-.tab
    dc.w    .f1-.tab
    dc.w    .f2-.tab
    dc.w    .f3-.tab
    dc.w    .f4-.tab
    dc.w    .f5-.tab
    dc.w    .f6-.tab
    dc.w    .f7-.tab
    dc.w    .f8-.tab
    dc.w    .f9-.tab
    dc.w    .fa-.tab
    dc.w    .fb-.tab
    dc.w    .fc-.tab
    dc.w    .fd-.tab
    dc.w    .fe-.tab
    dc.w    .ff-.tab

.f0
    clr.l   d5
    add.l   d4,d1
    add.l   d3,d1
    add.l   d2,d1
    move.l  d1,filter_Vnf(a0)
    bra     .break
.f1
    move.l  d1,d5
    add.l   d4,d2
    add.l   d3,d2
    move.l  d2,filter_Vnf(a0)
    bra     .break
.f2
    move.l  d2,d5
    add.l   d4,d1
    add.l   d3,d1
    move.l  d1,filter_Vnf(a0)
    bra     .break
.f3
    move.l  d2,d5
    add.l   d4,d1
    add.l   d3,d1
    move.l  d1,filter_Vnf(a0)
    bra     .break
.f4
    move.l  d3,d5
    add.l   d4,d1
    add.l   d2,d1
    move.l  d1,filter_Vnf(a0)
    bra     .break
.f5
    move.l  d1,d5
    add.l   d3,d5
    add.l   d4,d2
    move.l  d2,filter_Vnf(a0)
    bra     .break
.f6
    move.l  d2,d5
    add.l   d3,d5
    add.l   d4,d1
    move.l  d1,filter_Vnf(a0)
    bra     .break
.f7
    move.l  d1,d5
    add.l   d2,d5
    add.l   d3,d5
    move.l  d4,filter_Vnf(a0)
    bra     .break
.f8
    move.l  d4,d5
    add.l   d3,d1
    add.l   d2,d1
    move.l  d1,filter_Vnf(a0)
    bra     .break
.f9
    move.l  d4,d5
    add.l   d1,d5
    add.l   d3,d2
    move.l  d2,filter_Vnf(a0)
    bra     .break
.fa
    move.l  d4,d5
    add.l   d2,d5
    add.l   d3,d1
    move.l  d1,filter_Vnf(a0)
    bra     .break
.fb
    move.l  d4,d5
    add.l   d1,d5
    add.l   d2,d5
    move.l  d3,filter_Vnf(a0)
    bra     .break
.fc
    move.l  d4,d5
    add.l   d3,d5
    add.l   d2,d1
    move.l  d1,filter_Vnf(a0)
    bra     .break
.fd
    move.l  d4,d5
    add.l   d3,d5
    add.l   d1,d5
    move.l  d2,filter_Vnf(a0)
    bra     .break
.fe
    move.l  d4,d5
    add.l   d3,d5
    add.l   d2,d5
    move.l  d1,filter_Vnf(a0)
    bra     .break
.ff
    move.l  d4,d5
    add.l   d3,d5
    add.l   d2,d5
    add.l   d1,d5
    clr.l   filter_Vnf(a0)
;    bra     .break

.break
    moveq   #8,d1
    * d5 = Vi
    * d0 = delta_t
    * d1 = delta_t_flt

.loop
    tst.l   d0
    beq     .x
    cmp.l   d1,d0
    bhs.b   .4
    move.l  d0,d1
.4
    * d2,d3 = w0_delta_t
    move.l  filter_w0_ceil_dt(a0),d2
    muls.l  d1,d2
    asr.l   #6,d2
    move.l  d2,d3

    muls.l  filter_Vhp(a0),d2
    asr.l   #8,d2
    asr.l   #6,d2
    sub.l   d2,filter_Vbp(a0)

    muls.l  filter_Vbp(a0),d3
    asr.l   #8,d3
    asr.l   #6,d3
    sub.l   d3,filter_Vlp(a0)

    move.l  filter_Vbp(a0),d2
    muls.l  filter_1024_div_Q(a0),d2
    asr.l   #8,d2
    asr.l   #2,d2

    sub.l   filter_Vlp(a0),d2
    sub.l   d5,d2
    move.l  d2,filter_Vhp(a0)

    sub.l   d1,d0
    bra     .loop
.x
    rts

* in:
*    a0 = object
* out:
*    d0 = filter output 20 bits
* uses:
*    d0,d1,a0
filter_output:
    tst.b   filter_enabled(a0)
    bne.b   .1
    moveq   #0,d1
    move.b  filter_vol(a0),d1
    move.l  filter_Vnf(a0),d0
    add.l   filter_mixer_DC(a0),d0
    muls.l  d1,d0
    rts
.1  
    moveq   #0,d0
    move.b  filter_hp_bp_lp(a0),d0
    move.w  .tab(pc,d0.w*2),d0
    jmp     .tab(pc,d0)

.tab
    dc.w    .f0-.tab
    dc.w    .f1-.tab
    dc.w    .f2-.tab
    dc.w    .f3-.tab
    dc.w    .f4-.tab
    dc.w    .f5-.tab
    dc.w    .f6-.tab
    dc.w    .f7-.tab

.f0 
    clr.l   d0
    bra     .break
.f1
    move.l  filter_Vlp(a0),d0
    bra     .break
.f2
    move.l  filter_Vbp(a0),d0
    bra     .break
.f3
    move.l  filter_Vlp(a0),d0
    add.l   filter_Vbp(a0),d0
    bra     .break
.f4
    move.l  filter_Vhp(a0),d0
    bra     .break
.f5
    move.l  filter_Vlp(a0),d0
    add.l   filter_Vhp(a0),d0
    bra     .break
.f6
    move.l  filter_Vbp(a0),d0
    add.l   filter_Vhp(a0),d0
    bra     .break
.f7
    move.l  filter_Vlp(a0),d0
    add.l   filter_Vbp(a0),d0
    add.l   filter_Vhp(a0),d0
    ;bra     .break
    
.break
    add.l   filter_Vnf(a0),d0
    add.l   filter_mixer_DC(a0),d0
    moveq   #0,d1
    move.b  filter_vol(a0),d1
    muls.l  d1,d0
    rts


******************************************************************************
*
* External filter
*
******************************************************************************

* in:
*    a0 = object
extfilter_constructor:
    bsr     extfilter_reset
    moveq   #1,d0
    bsr     extfilter_enable_filter
    moveq   #CHIP_MODEL_MOS6581,d0
    bsr     extfilter_set_chip_model
    move.l  #104858,extfilter_w0lp(a0)
    move.l  #105,extfilter_w0hp(a0)
    rts

* in:
*    a0 = object
*    d0 = true to enable, false to disable
extfilter_enable_filter:
    move.b  d0,extfilter_enabled(a0)
    rts

* in:
*    a0 = object
extfilter_reset:
    clr.l   extfilter_Vlp(a0)
    clr.l   extfilter_Vhp(a0)
    clr.l   extfilter_Vo(a0)
    rts

* in:
*   a0 = object
*   d0 = chip model
extfilter_set_chip_model
    cmp.b   #CHIP_MODEL_MOS6581,d0
    bne.b   .1
    move.l  #(((($800-$380)+$800)*$ff*3-$fff*$ff/18)>>7)*$f,extfilter_mixer_DC(a0)
    rts
.1
    clr.l   extfilter_mixer_DC(a0)
    rts

* in:
*   a0 = object
*   d0 = cycle_count delta_t
*   d1 = sample Vi
* uses:
*   d0-d5,a0
extfilter_clock:
    tst.b   extfilter_enabled(a0)
    bne     .1
    clr.l   extfilter_Vlp(a0)
    clr.l   extfilter_Vhp(a0)
    sub.l   extfilter_mixer_DC(a0),d1
    move.l  d1,extfilter_Vo(a0)
    rts
.1
    * d2 = delta_t_flt
    moveq   #8,d2
    * d6 = Vi
    move.l  d1,d6
.loop
    tst.l   d0
    beq     .x
    cmp.l   d2,d0
    bhs.b   .2
    move.l  d0,d2
.2
    move.l  d6,d1
    sub.l   extfilter_Vlp(a0),d1 * Vi-Vlp
    move.l  extfilter_w0lp(a0),d3
    muls.l  d2,d3
    asr.l   #8,d3
    muls.l  d1,d3
    asr.l   #8,d3
    asr.l   #4,d3
    * d3 = dVlp

    move.l  extfilter_w0hp(a0),d4
    muls.l  d2,d4
    move.l  extfilter_Vlp(a0),d5
    sub.l   extfilter_Vhp(a0),d5
    muls.l  d5,d4
    swap    d4
    asr.w   #4,d4
    ext.l   d4
    * d4 = dVhp

    move.l  extfilter_Vlp(a0),d5
    sub.l   extfilter_Vhp(a0),d5
    move.l  d5,extfilter_Vo(a0)

    add.l   d3,extfilter_Vlp(a0)
    add.l   d4,extfilter_Vhp(a0)

    sub.l   d2,d0
    bra     .loop
.x
    rts

* in:
*   a0 = object
* out:
*   d0 = audio output 20 bits
* uses:
*   d0, a0
extfilter_output:
    move.l  extfilter_Vo(a0),d0
    rts


***********************************************************************************
*
* SID
*
******************************************************************************

* in:
*   a0 = object
sid_constructor:

    * Set object pointers
    lea     Voice1,a1
    lea     Voice2,a2
    lea     Voice3,a3
    move.l  #Envelope1,voice_envelope(a1)
    move.l  #Envelope2,voice_envelope(a2)
    move.l  #Envelope3,voice_envelope(a3)
    move.l  #Wave1,voice_wave(a1)
    move.l  #Wave2,voice_wave(a2)
    move.l  #Wave3,voice_wave(a3)
    move.l  a1,sid_voice1(a0)
    move.l  a2,sid_voice2(a0)
    move.l  a3,sid_voice3(a0)
    move.l  #Filter,sid_filter(a0)
    move.l  #ExtFilter,sid_extfilt(a0)

    * ... and construct with defaults
    push    a0
    lea     Voice1,a0
    bsr     voice_constructor
    lea     Voice2,a0
    bsr     voice_constructor
    lea     Voice3,a0
    bsr     voice_constructor
    lea     Filter,a0
    bsr     filter_constructor
    lea     ExtFilter,a0
    bsr     extfilter_constructor
    pop     a0

    clr.b   sid_bus_value(a0)
    clr.l   sid_bus_value_ttl(a0)

    move.l  #985248,d0
    moveq   #SAMPLING_METHOD_SAMPLE_FAST,d1
    move.l  #44100/2,d2
    bsr     sid_set_sampling_parameters

    lea     Voice1,a0
    lea     Voice3,a1
    bsr     voice_set_sync_source

    lea     Voice2,a0
    lea     Voice1,a1
    bsr     voice_set_sync_source

    lea     Voice3,a0
    lea     Voice2,a1
    bsr     voice_set_sync_source
    rts

* in:
*   a0 = object
*   d0 = true to enable, false to not
sid_enable_filter:
    move.l  sid_filter(a0),a0
    bsr     filter_enable_filter
    rts

* in:
*   a0 = object
*   d0 = true to enable, false to not
sid_enable_external_filter:
    move.l  sid_extfilt(a0),a0
    bsr     extfilter_enable_filter
    rts


* in:
*   a0 = object
*   d0 = chip model
sid_set_chip_model:
    move.l  a0,a5
    move.l  sid_voice1(a5),a0
    bsr     voice_set_chip_model
    move.l  sid_voice2(a5),a0
    bsr     voice_set_chip_model
    move.l  sid_voice3(a5),a0
    bsr     voice_set_chip_model
    move.l  sid_filter(a5),a0
    bsr     filter_set_chip_model
    move.l  sid_extfilt(a5),a0
    bsr     extfilter_set_chip_model
    rts

* in:
*   a0 = object
sid_reset:
    move.l  a0,a5
    clr.b   sid_bus_value(a5)
    clr.l   sid_bus_value_ttl(a5)

    move.l  sid_voice1(a5),a0
    bsr     voice_reset
    move.l  sid_voice2(a5),a0
    bsr     voice_reset
    move.l  sid_voice3(a5),a0
    bsr     voice_reset
    move.l  sid_filter(a5),a0
    bsr     filter_reset
    move.l  sid_extfilt(a5),a0
    bsr     extfilter_reset
    rts

* in:
*   a0 = object
* out:
*   d0 = 8-bit output
* uses: 
*   d0,a0
sid_output8:
.range = 1<<8
.half  = .range>>1
    move.l  sid_extfilt(a0),a0
    bsr     extfilter_output
    divs.w  #(((4095*255>>7)*3*15*2)/.range),d0

    * clamp to [-128..127]
    cmp     #.half,d0
    blt     .1
    moveq   #.half-1,d0
    rts
.1
    cmp     #-.half,d0
    bge     .2
    moveq   #-.half,d0
.2
    rts

* in:
*   a0 = object
* out:
*   d0 = value
*   d1 = SID register offset
* uses:
*   d0,d1,d2,a0,a1
sid_write
    ;move.b  d1,sid_bus_value(a0)
    ;move.l  #$2000,sid_bus_value_ttl(a0)
    and     #$1f,d1
    move.w  .tab(pc,d1.w*2),d1
    jmp     .tab(pc,d1)

.tab    
    dc.w    .w00-.tab
    dc.w    .w01-.tab
    dc.w    .w02-.tab
    dc.w    .w03-.tab
    dc.w    .w04-.tab
    dc.w    .w05-.tab
    dc.w    .w06-.tab
    dc.w    .w07-.tab
    dc.w    .w08-.tab
    dc.w    .w09-.tab
    dc.w    .w0a-.tab
    dc.w    .w0b-.tab
    dc.w    .w0c-.tab
    dc.w    .w0d-.tab
    dc.w    .w0e-.tab
    dc.w    .w0f-.tab
    dc.w    .w10-.tab
    dc.w    .w11-.tab
    dc.w    .w12-.tab
    dc.w    .w13-.tab
    dc.w    .w14-.tab
    dc.w    .w15-.tab
    dc.w    .w16-.tab
    dc.w    .w17-.tab
    dc.w    .w18-.tab
    dc.w    .w19-.tab
    dc.w    .w1a-.tab
    dc.w    .w1b-.tab
    dc.w    .w1c-.tab
    dc.w    .w1d-.tab
    dc.w    .w1e-.tab
    dc.w    .w1f-.tab

.w19
.w1a
.w1b
.w1c
.w1d
.w1e
.w1f
    rts

.w00:
    move.l  ([sid_voice1.w,a0],voice_wave.w),a0
    bra     wave_writeFREQ_LO
.w01:
    move.l  ([sid_voice1.w,a0],voice_wave.w),a0
    bra     wave_writeFREQ_HI
.w02:
    move.l  ([sid_voice1.w,a0],voice_wave.w),a0
    bra     wave_writePW_LO
.w03:
    move.l  ([sid_voice1.w,a0],voice_wave.w),a0
    bra     wave_writePW_HI
.w04:
    move.l  sid_voice1(a0),a0
    bra     voice_writeCONTROL_REG
.w05:
    move.l  ([sid_voice1.w,a0],voice_envelope.w),a0
    bra     envelope_writeATTACK_DECAY
.w06:
    move.l  ([sid_voice1.w,a0],voice_envelope.w),a0
    bra     envelope_writeSUSTAIN_RELEASE
.w07:
    move.l  ([sid_voice2.w,a0],voice_wave.w),a0
    bra     wave_writeFREQ_LO
.w08:
    move.l  ([sid_voice2.w,a0],voice_wave.w),a0
    bra     wave_writeFREQ_HI
.w09:
    move.l  ([sid_voice2.w,a0],voice_wave.w),a0
    bra     wave_writePW_LO
.w0a:
    move.l  ([sid_voice2.w,a0],voice_wave.w),a0
    bra     wave_writePW_HI
.w0b:
    move.l  sid_voice2(a0),a0
    bra     voice_writeCONTROL_REG
.w0c:
    move.l  ([sid_voice2.w,a0],voice_envelope.w),a0
    bra     envelope_writeATTACK_DECAY
.w0d:
    move.l  ([sid_voice2.w,a0],voice_envelope.w),a0
    bra     envelope_writeSUSTAIN_RELEASE
.w0e:
    move.l  ([sid_voice3.w,a0],voice_wave.w),a0
    bra     wave_writeFREQ_LO
.w0f:
    move.l  ([sid_voice3.w,a0],voice_wave.w),a0
    bra     wave_writeFREQ_HI
.w10:
    move.l  ([sid_voice3.w,a0],voice_wave.w),a0
    bra     wave_writePW_LO
.w11:
    move.l  ([sid_voice3.w,a0],voice_wave.w),a0
    bra     wave_writePW_HI
.w12:
    move.l  sid_voice3(a0),a0
    bra     voice_writeCONTROL_REG
.w13:
    move.l  ([sid_voice3.w,a0],voice_envelope.w),a0
    bra     envelope_writeATTACK_DECAY
.w14:
    move.l  ([sid_voice3.w,a0],voice_envelope.w),a0
    bra     envelope_writeSUSTAIN_RELEASE
.w15:
    move.l  sid_filter(a0),a0
    bra     filter_writeFC_LO
.w16:
    move.l  sid_filter(a0),a0
    bra     filter_writeFC_HI
.w17:
    move.l  sid_filter(a0),a0
    bra     filter_writeRES_FILT
.w18:
    move.l  sid_filter(a0),a0
    bra     filter_writeMODE_VOL

* in:
*   a0 = object
*   d0 = clock_frequency
*   d1 = sampling method
*   d2 = sample freq
* out:
*   d0 = true if ok, false with a bad param combo
sid_set_sampling_parameters:
    cmp.b   #SAMPLING_METHOD_SAMPLE_FAST,d1
    bne     .fail

    move.l  d0,sid_clock_frequency(a0)
    move.b  d1,sid_sampling_method(a0)

    fmove.l d0,fp0
    fdiv.l  d2,fp0      
    move.l  #1<<FIXP_SHIFT,d3
    fmul.l  d3,fp0
    fadd.s  #0.5,fp0
    fmove.l fp0,sid_cycles_per_sample(a0)

    clr.l   sid_sample_offset(a0)
  
    moveq   #1,d0
    rts
.fail
    moveq   #0,d0
    rts

* in:
*   a0 = object
*   d0 = cycle_count delta_t
* uses:
*    d0-d7,a0,a1,a2,a3,a5
sid_clock:
    tst.l   d0
    bgt     .1
    rts
.1
    move.l  a0,a5

; not needed?
;    sub.l   d0,sid_bus_value_ttl(a5)
;    bpl.b   .2
;    clr.b   sid_bus_value(a5)
;    clr.l   sid_bus_value_ttl(a5)
;.2

    move.l  d0,d7
    push    d7      * save delta_t

    move.l  ([sid_voice1.w,a5],voice_envelope.w),a0
    bsr     envelope_clock    
 
    move.l  ([sid_voice2.w,a5],voice_envelope.w),a0
    move.l  d7,d0
    bsr     envelope_clock
 
    move.l  ([sid_voice3.w,a5],voice_envelope.w),a0
    move.l  d7,d0
    bsr     envelope_clock

    * d7 = delta_t_osc
.loop
    tst.l   d7
    beq     .loopExit
    
    * a3 = delta_t_min
    move.l  d7,a3

    * cycle checks
    move.l  ([sid_voice1.w,a5],voice_wave.w),a0
    bsr     .cycleCheck
    move.l  ([sid_voice2.w,a5],voice_wave.w),a0
    bsr     .cycleCheck
    move.l  ([sid_voice3.w,a5],voice_wave.w),a0
    bsr     .cycleCheck
    bra     .continue

.cycleCheck
    tst.w   wave_freq(a0)
    beq     .cx
    move.l  wave_sync_dest(a0),a1
    tst.b   wave_sync(a1)
    beq     .cx

    move.l  wave_accumulator(a0),d1
    move.l  #$1000000,d2
    btst    #23,d1      * andi.l #$800000 
    bne.b   .a
    move.l  #$800000,d2
.a  sub.l   d1,d2
    * d2 = delta_accumulator
    moveq   #0,d3
    move    wave_freq(a0),d3
    divul.l d3,d3:d2
    * d2 = delta_t_next = delta_accumulator / freq
    * d3 = remainder (ie. modulo)
    tst.l   d3
    beq.b   .b
    addq.l  #1,d2
.b
    cmp.l   d2,a3
    bge     .cx
    move.l  d2,a3
.cx    
    rts

.continue

    ; clock oscillators
    move.l  ([sid_voice1.w,a5],voice_wave.w),a0
    move.l  a3,d0
    bsr     wave_clock
  
    move.l  ([sid_voice2.w,a5],voice_wave.w),a0
    move.l  a3,d0
    bsr     wave_clock
  
    move.l  ([sid_voice3.w,a5],voice_wave.w),a0
    move.l  a3,d0
    bsr     wave_clock

    ; synchronize oscillators
    move.l  ([sid_voice1.w,a5],voice_wave.w),a0
    bsr     wave_synchronize

    move.l  ([sid_voice2.w,a5],voice_wave.w),a0
    bsr     wave_synchronize
    
    move.l  ([sid_voice3.w,a5],voice_wave.w),a0
    bsr     wave_synchronize

    sub.l   a3,d7
    bra     .loop

.loopExit

    ; Clock filter
    move.l  sid_voice3(a5),a0
    bsr     voice_output
    push    d0
    move.l  sid_voice2(a5),a0
    bsr     voice_output
    push    d0
    move.l  sid_voice1(a5),a0
    bsr     voice_output
    move.l  d0,d1
    popm    d2/d3
    moveq   #0,d4   * ext_in

    pop     d7      * restore delta_t
    move.l  d7,d0
    move.l  sid_filter(a5),a0
    bsr     filter_clock

    move.l  sid_filter(a5),a0
    bsr     filter_output

    ; Clock external filter
    move.l  d0,d1
    move.l  d7,d0
    move.l  sid_extfilt(a5),a0
    bsr     extfilter_clock

    rts


* in:
*   a0 = object
*   a1 = output byte buffer pointer
*   d0 = cycle_count delta_t, max cycles
*   d1 = bytes to get
sid_clock_fast:
    move.l  a0,a5
    * d3 = s
    moveq   #0,d3
.loop
    * d5 = next_sample_offset
    move.l  sid_sample_offset(a5),d5
    add.l   sid_cycles_per_sample(a5),d5
    add.l   #1<<(FIXP_SHIFT-1),d5

    * d2 = delta_t_sample
    move.l  d5,d2
    swap    d2
    ext.l   d2      * >>FIXP_SHIFT
   
    * if (delta_t_sample > delta_t)
    cmp.l   d0,d2
    bgt     .break
    * buffer overflow check
    * if (s >= n) 
    ;cmp.l   d1,d3
    ;bge     .x     

    pushm   d0-d5/a1/a5
    move.l  d2,d0
    move.l  a5,a0
    bsr     sid_clock
    popm    d0-d5/a1/a5

    sub.l   d2,d0
    move.l  d5,d6
    and.l   #FIXP_MASK,d6
    sub.l   #1<<(FIXP_SHIFT-1),d6
    move.l  d6,sid_sample_offset(a5)

    push    d0      * stash delta_t
    move.l  a5,a0
    bsr     sid_output8
    * store one byte at d3
    move.b  d0,(a1,d3.l)
    addq.l  #1,d3
    pop     d0
    * All bytes generated?
    cmp.l   d3,d1
    beq     .x
    bra     .loop

.break

    move.l  a5,a0
    pushm   d0/d3/a5
    bsr     sid_clock
    popm    d0/d3/a5

    swap    d0
    clr.w   d0      * delta_t<<FIXP_SHIFT
    sub.l   d0,sid_sample_offset(a5)
.x
    * bytes written
    move.l  d3,d0
    rts

    section reSID_bss,bss_p

Sid         ds.b sid_SIZEOF
Voice1      ds.b voice_SIZEOF
Voice2      ds.b voice_SIZEOF
Voice3      ds.b voice_SIZEOF
Wave1       ds.b wave_SIZEOF
Wave2       ds.b wave_SIZEOF
Wave3       ds.b wave_SIZEOF
Envelope1   ds.b envelope_SIZEOF
Envelope2   ds.b envelope_SIZEOF
Envelope3   ds.b envelope_SIZEOF
Filter      ds.b filter_SIZEOF
ExtFilter   ds.b extfilter_SIZEOF

	section	reSID_data,data

    ; Precalculated filter tables from resid.
    ; labels: filter_f0_6581, filter_f0_8580
    include "filter_f0_data.s"

wave6581__ST:    incbin "wave6581__ST.dat"
wave6581_P_T:    incbin "wave6581_P_T.dat"
wave6581_PS_:    incbin "wave6581_PS_.dat"
wave6581_PST:    incbin "wave6581_PST.dat"
wave8580__ST:    incbin "wave8580__ST.dat"
wave8580_P_T:    incbin "wave8580_P_T.dat"
wave8580_PS_:    incbin "wave8580_PS_.dat"
wave8580_PST:    incbin "wave8580_PST.dat"

