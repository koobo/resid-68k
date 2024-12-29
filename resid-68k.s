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

; Ported on October 2022 by K-P Koljonen <kpk@iki.fi>

pushm  macro
       ifc        "\1","all"
       movem.l    d0-a6,-(sp)
       else
       movem.l    \1,-(sp)
       endc
       endm

popm   macro
       ifc        "\1","all"
       movem.l    (sp)+,d0-a6
       else
       movem.l    (sp)+,\1
       endc
       endm

push   macro
       move.l     \1,-(sp)
       endm

pop    macro
       move.l     (sp)+,\1
       endm

  ifnd __VASM
    incdir include:
  endif

  ifd __VASM
    ; Disable "move.l to moveq" optimization, it may add "swap" which is
    ; pOEP-only
    opt o4-
  endif

    include resid-68k.i

RANGE8           = 1<<8
HALF8            = RANGE8>>1
RANGE16          = 1<<16
HALF16           = RANGE16>>1



CLAMP8 macro
    cmp.w   #HALF8,\1
    blt.b   .\@1
    moveq   #HALF8-1,\1
    bra.b   .\@2
.\@1
    cmp.w   #-HALF8,\1
    bge.b   .\@2
    moveq   #-HALF8,\1
.\@2
    endm

CLAMP16 macro
    cmp.l   #HALF16,\1
    blt.b   .\@1
    move.w  #HALF16-1,\1
    bra.b   .\@2
.\@1
    cmp.l   #-HALF16,\1
    bge.b   .\@2
    move.w  #-HALF16,\1
.\@2
    endm


 ifnd COUNTERS
COUNTERS = 0
 endif

 ifne COUNTERS
    printt "COUNTERS enabled"
C_start
C_CLK1   dc.l    0,0
        dc.l    "CLK1"
C_CLK2   dc.l    0,0
        dc.l    "CLK2"
C_CLK3   dc.l    0,0
        dc.l    "CLK3"
C_CLK4   dc.l    0,0
        dc.l    "CLK4"
C_CLK5   dc.l    0,0
        dc.l    "CLK5"
C_WAV1   dc.l    0,0
        dc.l    "WAV1"
C_WAV2   dc.l    0,0
        dc.l    "WAV2"
C_WAV3   dc.l    0,0
        dc.l    "WAV3"
C_WAV4   dc.l    0,0
        dc.l    "WAV4"
C_WO1    dc.l    0,0
        dc.l    "WO1 "
C_WO2    dc.l    0,0
        dc.l    "WO2 "
C_WO3    dc.l    0,0
        dc.l    "WO3 "
C_WO4    dc.l    0,0
        dc.l    "WO4 "
C_WO5    dc.l    0,0
        dc.l    "WO5 "
C_WO6    dc.l    0,0
        dc.l    "WO6 "
C_WO7    dc.l    0,0
        dc.l    "WO7 "
C_WO8    dc.l    0,0
        dc.l    "WO8 "
C_WO9    dc.l    0,0
        dc.l    "WO9 "
C_W10    dc.l    0,0
        dc.l    "W10 "
C_W11    dc.l    0,0
        dc.l    "W11 "
C_W12    dc.l    0,0
        dc.l    "W12 "
C_W13    dc.l    0,0
        dc.l    "W13 "
C_W14    dc.l    0,0
        dc.l    "W14 "
C_W15    dc.l    0,0
        dc.l    "W15 "
C_W16    dc.l    0,0
        dc.l    "W16 "
C_W17    dc.l    0,0
        dc.l    "W17 "
C_W18    dc.l    0,0
        dc.l    "W18 "
C_W19    dc.l    0,0
        dc.l    "W19 "
C_W20    dc.l    0,0
        dc.l    "W20 "
C_EFLT1  dc.l    0,0
        dc.l    "EFL1"
C_EFLT2  dc.l    0,0
        dc.l    "EFL2"
C_EFLT3  dc.l    0,0
        dc.l    "EFL3"
C_EFLT4  dc.l    0,0
        dc.l    "EFL4"
C_EFLT5 dc.l    0,0
        dc.l    "EFL5"
C_EFLT6 dc.l    0,0
        dc.l    "EFL6"
C_FLT1   dc.l    0,0
        dc.l    "FLT1"
C_FLT2   dc.l    0,0
        dc.l    "FLT2"
C_FLT3   dc.l    0,0
        dc.l    "FLT3"
C_FLT4  dc.l    0,0
        dc.l    "FLT4"
C_FLT5  dc.l    0,0
        dc.l    "FLT5"
C_FLT6  dc.l    0,0
        dc.l    "FLT6"
C_ENV1   dc.l    0,0
        dc.l    "EN01"
C_ENV2   dc.l    0,0
        dc.l    "EN02"
C_ENV3   dc.l    0,0
        dc.l    "EN03"
C_ENV4   dc.l    0,0
        dc.l    "EN04"
C_ENV5   dc.l    0,0
        dc.l    "EN05"
C_ENV6   dc.l    0,0
        dc.l    "EN06"
C_ENV7   dc.l    0,0
        dc.l    "EN07"
C_ENV8   dc.l    0,0
        dc.l    "EN08"
C_ENV9   dc.l    0,0
        dc.l    "EN09"
C_ENV10  dc.l    0,0
        dc.l    "EN10"
C_ENV11  dc.l    0,0
        dc.l    "EN11"
C_ENV12  dc.l    0,0
        dc.l    "EN12"
C_ENV13  dc.l    0,0
        dc.l    "EN13"
C_ENV14  dc.l    0,0
        dc.l    "EN14"
C_ENV15  dc.l    0,0
        dc.l    "EN15"
C_ENV16  dc.l    0,0
        dc.l    "EN16"
C_ENV17  dc.l    0,0
        dc.l    "EN17"
C_ENV18  dc.l    0,0
        dc.l    "EN18"
C_ENV19  dc.l    0,0
        dc.l    "EN19"
C_ENV20  dc.l    0,0
        dc.l    "EN20"
C_ENV21  dc.l    0,0
        dc.l    "EN21"
C_ENV22  dc.l    0,0
        dc.l    "EN22"
C_ENV23  dc.l    0,0
        dc.l    "EN23"
C_ENV24  dc.l    0,0
        dc.l    "EN24"
C_ENV25  dc.l    0,0
        dc.l    "EN25"
C_ENV26  dc.l    0,0
        dc.l    "EN26"
C_ENV27  dc.l    0,0
        dc.l    "EN27"
C_ENV28  dc.l    0,0
        dc.l    "EN28"
C_ENV29  dc.l    0,0
        dc.l    "EN29"
C_ENV30  dc.l    0,0
        dc.l    "EN30"
C_ENV31  dc.l    0,0
        dc.l    "EN31"
C_ENV32  dc.l    0,0
        dc.l    "EN32"
C_end

reset_counters:
    lea C_start,a0
    moveq    #(C_end-C_start)/12-1,d0
.l  
    clr.l   (a0)+
    clr.l   (a0)+
    add     #4,a0
    dbf d0,.l
    rts

sid_get_counters:
    lea     C_start,a2
    moveq   #(C_end-C_start)/12-1,d3
    rts


counter_1:
    dc.l    0,1

  endif ; COUNTERS

COUNT macro
    if COUNTERS
    addq.l  #1,\1+4
    endif
    endm
    




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
    rts
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
    move.b  d0,wave_freq+1(a0)
    rts


* in:
*   a0 = object
*   d0 = freq_hi
wave_writeFREQ_HI:
    move.b  d0,wave_freq+0(a0)
    rts


* in:
*   a0 = object
*   d0 = pw_lo
wave_writePW_LO:
    move.b  d0,wave_pw+1(a0)
    rts

* in:
*   a0 = object
*   d0 = pw_hi
wave_writePW_HI:
    and.b   #$f,d0
    move.b  d0,wave_pw+0(a0)
    rts

* in:
*   a0 = object
*   d0 = control
wave_writeCONTROL_REG:
    moveq   #0,d1
    move.b  d0,d1
    lsr.b   #4,d1

    ; Store wave generator routine address
    ;move.w  d1,wave_waveform(a0)
    lea     wave_output\.tab(pc),a1
 
    moveq   #$04,d2
    and.b   d0,d2
    move.b  d2,wave_ring_mod(a0)

    beq.b   .noRm
    * Select a ring mod variant of the table
    add     #16,d1
.noRm
    add.w   (a1,d1.w*2),a1
    move.l  a1,wave_get_output(a0)

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
    beq.b   .noTest
    * Test bit forces pulse out to be 0xfff
    move.w  #$fff,wave_test_mask(a0)
    rts
.noTest
    clr.w   wave_test_mask(a0)
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
    pea     wave_output____(pc)
    move.l  (sp)+,wave_get_output(a0)
    rts


* in:
*   a0 = object
*   a3 = cycle_count delta_t, preserved
* uses:
*   d1-d6,a0
WAVE_CLOCK_ macro
   ; tst.b   wave_test(a0)
   ; beq     .wcgo
   ; COUNT   C_WAV4
;
   ; jmp     (a2)

    tst.b   wave_test(a0)
    bne    .wcexit

.wcgo
    * calls: 3x

 REM ; option 1
    * d1 = accumulator_prev
    move.l  wave_accumulator(a0),d1

    * d3 = delta_accumulator
    move    wave_freq(a0),d3
    mulu.w  d0,d3
    
    clr.b   wave_msb_rising(a0)

    move.l  d1,d2
    * d2 = accumulator
    add.l   d3,d2
    and.l   #$ffffff,d2
    move.l  d2,wave_accumulator(a0)

    ;btst    #23,d1      * previous MSB
    and.l   #$800000,d1 * test previous MSB, pOEP|sOEP
    bne     .wcnoMsb
    btst    #23,d2      * test current MSB, pOEP-until-last
    beq     .wcnoMsb
    st      wave_msb_rising(a0)
.wcnoMsb
 EREM 
    ; --------------------------------- 
; option 2 - resid 1.0
    COUNT   C_WAV1

   * d3 = delta_accumulator
    ;move    wave_freq(a0),d3
    ;mulu.w  d0,d3
    ;move.w  a2,d3
    move.w  a3,d3
    mulu.w  wave_freq(a0),d3
   
    * d2 = accumulator_next
    move.l  wave_accumulator(a0),d2
    move.l  d2,d1
    add.l   d3,d2
    and.l   #$ffffff,d2

    * d1 = accumulator_bits_set
    not.l   d1
    and.l   d2,d1

    * accumulator = accumulator_next
    move.l  d2,wave_accumulator(a0)
 
    ; Check if MSB is set high
    and.l   #$800000,d1
    sne     wave_msb_rising(a0)

    ; --------------------------------- 

    move.l  #$100000,d1

    * d0 = delta_t
    * d1 = shift period
    * d2 = accumulator
    * d3 = delta_accumulator

.wcloop
    cmp.l   d1,d3
    bhs.b   .wccontinue

    COUNT   C_WAV2

    * calls: 3x

    * shift_period = delta_accumulator
    move.l  d3,d1
    move.l  d2,d4

    move.l  #$80000,d6  ; bit 19 mask 

    sub.l   d1,d4

 REM ; option 1
    cmp.l   #$80000,d1 
    bhi     .wcelse
    ;and.l   #$80000,d4
    btst    #19,d4   ; this seems to win over and.l here
    bne     .wcbreak
    btst    #19,d2
    beq     .wcbreak
    bra     .wccontinue
.wcelse
    and.l   #$80000,d4
    beq     .wccontinue
    btst    #19,d2
    beq     .wcbreak
 EREM
 ;REM ; option 2
 ;possibly faster since Bcc and std intruction may pair in certain
 ;situations
    cmp.l   d6,d1 
    bhi     .wcelse
    and.l   d6,d4
    bne     .wcbreak
    and.l   d2,d6
    beq     .wcbreak
    bra     .wccontinue
.wcelse
    and.l   d6,d4
    beq     .wccontinue
    and.l   d2,d6
    beq     .wcbreak
 ;EREM

.wccontinue
    COUNT   C_WAV3
    * Shift the noise/random register.  
    * bit0
 REM ; option 1
    move.l  wave_shift_register(a0),d4
    move.l  d4,d6
    swap    d4
    move.w  d4,d5
    lsr.w   #6,d4
    lsr.w   #1,d5
    eor.w   d4,d5
    add.l   d6,d6
    and     #$1,d5
    and.l   #$7fffff,d6
    or.b    d5,d6
    move.l  d6,wave_shift_register(a0)
 EREM
    ; option 2
    move.l  wave_shift_register(a0),d4
    move.l  d4,d5
    lsl.l   #5,d5  * move bit 17 to position 22
    moveq   #10,d6 * shift 
    eor.l   d4,d5
    and.l   #$3fffff,d4 * clear top bit
    lsl.l   d6,d5 * move bit 22 into X
    addx.l  d4,d4 * shift and OR bit 0 from X
    move.l  d4,wave_shift_register(a0)

    sub.l   d1,d3
    bne     .wcloop

.wcbreak
.wcexit
    endm
;    jmp     (a2)
    
;wave_clock:
;    WAVE_CLOCK_
;    jmp     (a2)

* in:
*   a0 = object
*   a2 = return address
*   d0 = cycle_count delta_t
* uses:
*   d0-d6,a0
;;wave_clock_single:
;;    tst.b   wave_test(a0)
;;    beq     .go
;;.x  jmp     (a2)
;;.go
;;    move    d0,d6
;;    subq    #1,d6
;;    bmi     .x
;;
;;    move.l  wave_accumulator(a0),d1
;;    moveq   #0,d2
;;    move.w  wave_freq(a0),d2
;;.loop
;;    move.l  d1,d3
;;    add.l   d2,d1
;;    and.l   #$ffffff,d1
;;    * d1 = acc next
;;    * d3 = acc 
;;
;;    not.l   d3
;;    and.l   d1,d3
;;    * d3 = accumulator_bits_Set
;;
;;    btst    #23,d3    
;;    sne     wave_msb_rising(a0)
;;
;;    btst    #19,d3
;;    beq     .a
;;
;;    ; clock shift register
;;    move.l  wave_shift_register(a0),d4
;;    move.l  d4,d5
;;    lsl.l   #5,d5 * move bit 17 to position 22
;;    eor.l   d4,d5
;;    moveq   #10,d3   
;;    and.l   #$3fffff,d4 * clear top bit
;;    lsl.l   d3,d5 * move bit 22 into X
;;    addx.l  d4,d4 * shift and or bit 0
;;    move.l  d4,wave_shift_register(a0)
;;.a
;;    dbf     d6,.loop
;;    move.l  d1,wave_accumulator(a0)
;;    jmp     (a2)
 
* in:
*   a0 = object
* uses:
*   d0,d1,a0,a1,a2
wave_synchronize:

WAVE_SYNC macro
    tst.b   wave_msb_rising(a0)
    beq.b   .\1x
    move.l  wave_sync_dest(a0),a1
    tst.b   wave_sync(a1)
    beq.b   .\1x

    tst.b   wave_sync(a0)
    beq     .\1y
    move.l  wave_sync_source(a0),a2
    tst.b   wave_msb_rising(a2)
    bne     .\1x
.\1y
    clr.l   wave_accumulator(a1)
.\1x
    endm

    WAVE_SYNC 1
    rts

* in:
*   a0 = object
*   a3 = return address
* out:
*   d0 = Waveform output 12 bits
* uses: 
*   d0-d4,a0,a1
wave_output:
;    
;    move.w  wave_waveform(a0),d0
;    move.w  .tab(pc,d0.w*2),d0
;    jmp     .tab(pc,d0.w)

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
.tabRingMod
    dc.w     wave_output____-.tab
    dc.w     wave_output___T_ring-.tab
    dc.w     wave_output__S_-.tab
    dc.w     wave_output__ST-.tab
    dc.w     wave_output_P__-.tab
    dc.w     wave_output_P_T_ring-.tab
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

wave_output___T:
    COUNT   C_WO1
    move.l  wave_accumulator(a0),d0
    move.l  d0,d1
  
    moveq   #11,d3 * shift
    and.l   #$800000,d1
    beq.b   .noMsb
    not.l   d0
    COUNT   C_WO2
.noMsb
    lsr.l   d3,d0
    and     #$0fff,d0
    jmp     (a3)

wave_output___T_ring:
    COUNT   C_WO3
    move.l  wave_accumulator(a0),d0
    move.l  d0,d1
 
    move.l  wave_sync_source(a0),a1
    moveq   #11,d3 * shift
    move.l  wave_accumulator(a1),d2
    eor.l   d2,d1

    and.l   #$800000,d1
    beq.b   .noMsb
    not.l   d0
    COUNT   C_WO4
.noMsb
    lsr.l   d3,d0
    and     #$0fff,d0
    jmp     (a3)



wave_output__S_:
    COUNT   C_WO5
    moveq   #12,d1
    move.l  wave_accumulator(a0),d0
    lsr.l   d1,d0
    jmp     (a3)


wave_output__ST:
    COUNT   C_WO6

    ; wave_output__S_ inlined
    moveq   #12,d2 * shift
    move.l  wave_accumulator(a0),d1
    moveq   #0,d0
    lsr.l   d2,d1
    move.l  wave_wave__ST(a0),a1
    move.b  (a1,d1.w),d0
    lsl.w   #4,d0
    jmp     (a3)

* Pulse
* The test bit holds the pulse waveform output at 0xfff
* out:
*   d0 = $000 or $fff
wave_output_P__:
    COUNT   C_WO7

    ;tst.b   wave_test(a0)
    ;bne.b   .do
    ;COUNT   C_WO5
    moveq   #12,d2 * shift
    move.l  wave_accumulator(a0),d0
    lsr.l   d2,d0
    cmp     wave_pw(a0),d0  * 1; 2+2
    bhs.b   .do             * 0/1/7; 6 taken, 4 not taken
    ;moveq   #0,d0           * 1; 2 
    move.w  wave_test_mask(a0),d0
    COUNT   C_WO8
    jmp     (a3)            
.do
    move    #$0fff,d0       * 1; 2,4?
    jmp     (a3)            

* 060: 2 or 9
* 030: 2+2, 6, 2 = 12

;    moveq   #12,d2 * shift
;    move.l  wave_accumulator(a0),d1
;    lsr.l   d2,d1
;    cmp     wave_pw(a0),d1 * 1, 2+2
;    shs     d0             * 1, 4
;    ext     d0             * 1, 4
;    lsr     #4,d0          * 1, 4
;    bra        wave_output_return

* 060: 3 or 4
* 030: 4+4+4+4 = 16


wave_output_P_T:
    COUNT   C_WO9

    ;------------------------ wave_output___T:
    move.l  wave_accumulator(a0),d0
    move.l  d0,d1

    moveq   #11+1,d3 * shift
    and.l   #$800000,d1
    beq.b   .noMsb
    COUNT   C_W10
    not.l   d0
.noMsb
    lsr.l   d3,d0
    and     #$0fff>>1,d0
    ;------------------------ wave_output___T:

    ;lsr.w   #1,d0
    move.l  wave_wave_P_T(a0),a1
    move.b  (a1,d0.w),d0
    lsl     #4,d0

    ;------------------------ wave_output___P:
    ;tst.b   wave_test(a0)
    ;bne.b   .do
    ;moveq   #12,d2 * shift
    move.l  wave_accumulator(a0),d1
    lsr.l   d3,d1
    cmp     wave_pw(a0),d1
    bhs.b   .do
    COUNT   C_W11
    ;moveq   #0,d0
    and.w   wave_test_mask(a0),d0 * mask is 0 if test bit is 0
    jmp     (a3)
.do
    ;------------------------ wave_output___P:

    and     #$0fff,d0
    jmp     (a3)


wave_output_P_T_ring:
    COUNT   C_W12

    ;------------------------ wave_output___T:
    move.l  wave_accumulator(a0),d0
    move.l  d0,d1
    
    move.l  wave_sync_source(a0),a1
    moveq   #11+1,d3 * shift
    move.l  wave_accumulator(a1),d2
    eor.l   d2,d1

    and.l   #$800000,d1
    beq.b   .noMsb
    not.l   d0
    COUNT   C_W13
.noMsb
    lsr.l   d3,d0
    and     #$0fff>>1,d0
    ;------------------------ wave_output___T:

    ;lsr.w   #1,d0
    move.l  wave_wave_P_T(a0),a1
    move.b  (a1,d0.w),d0
    lsl     #4,d0

    ;------------------------ wave_output___P:
    ;tst.b   wave_test(a0)
    ;bne.b   .do
    ;moveq   #12,d2 * shift
    move.l  wave_accumulator(a0),d1
    lsr.l   d3,d1
    cmp     wave_pw(a0),d1
    bhs.b   .do
    COUNT   C_W14
    ;moveq   #0,d0
    and.w   wave_test_mask(a0),d0 * mask is 0 if test bit is 0
    jmp     (a3)
.do
    ;------------------------ wave_output___P:

    and     #$0fff,d0
    jmp     (a3)


wave_output_PS_:
    COUNT   C_W15

    ;------------------------ wave_output___S:
    moveq   #12,d2 * shift
    move.l  wave_accumulator(a0),d0
    lsr.l   d2,d0
    move.l  wave_wave_PS_(a0),a1
    move.b  (a1,d0.w),d1
    lsl.w   #4,d1

    ;------------------------ wave_output___P:
    ;tst.b   wave_test(a0)
    ;bne.b   .do
    ;moveq   #12,d2 * shift
    ;move.l  wave_accumulator(a0),d0
    ;lsr.l   d2,d0
    cmp     wave_pw(a0),d0
    bhs.b   .do
    COUNT   C_W16
    ;moveq   #0,d0
    move.w  wave_test_mask(a0),d0 * mask is 0 if test bit is 0
    and.w   d1,d0
    jmp     (a3)
.do
    move    #$0fff,d0
    ;------------------------ wave_output___P:
    
    and     d1,d0
    jmp     (a3)

wave_output_PST:
    COUNT   C_W17

    ;------------------------ wave_output___S:
    moveq   #12,d2 * shift
    move.l  wave_accumulator(a0),d0  
    lsr.l   d2,d0
    move.l  wave_wave_PST(a0),a1
    move.b  (a1,d0.w),d1
    lsl.w   #4,d1

    ;------------------------ wave_output___P:
    ;tst.b   wave_test(a0)
    ;bne.b   .do
    ;moveq   #12,d2 * shift
    ;move.l  wave_accumulator(a0),d0
    ;lsr.l   d2,d0
    cmp     wave_pw(a0),d0
    bhs.b   .do
    COUNT   C_W18
    ;moveq   #0,d0
    move.w  wave_test_mask(a0),d0 * mask is 0 if test bit is 0
    and.w   d1,d0
    jmp     (a3)
.do
    move    #$0fff,d0
    ;------------------------ wave_output___P:
    
    and     d1,d0
    jmp     (a3)

wave_outputN___:
    COUNT   C_W19

    move.l  wave_shift_register(a0),d1

    move.l  d1,d2
    move.l  d1,d0
    and.l   #$100000,d2
    and.l   #$400000,d0
    lsr.l   #8,d2
    lsr.l   #8,d0
    lsr.w   #2,d2
    lsr.w   #3,d0
    ;or.w    d2,d0

    move.l  d1,d4
    or.w    d2,d0
    move.w  d1,d3
    and.l   #$010000,d4
    and.w   #$002000,d3
    lsr.l   #7,d4
    lsr.w   #5,d3
    or.w    d4,d0
    ;or.w    d3,d0

    move.w  d1,d2
    or.w    d3,d0
    move.w  d1,d4
    and.w   #$000800,d2
    and.w   #$000080,d4
    lsr.w   #4,d2
    lsr.w   #1,d4
    or.w    d2,d0
    ;or.w    d4,d0

    move.w  d1,d3
    or.w    d4,d0
    and.w   #$000004,d1
    and.w   #$000010,d3
    lsl.w   #2,d1
    lsl.w   #1,d3
    or.w    d1,d0
    or.w    d3,d0
    jmp     (a3)



wave_output____:
wave_outputN__T:
wave_outputN_S_:
wave_outputN_ST:
wave_outputNP__:
wave_outputNP_T:
wave_outputNPS_:
wave_outputNPST:
    COUNT   C_W20
    moveq   #0,d0
    jmp     (a3)

* in:
*    a0 = object
* Out:
*    d0 = oscillator output, 8-bit
wave_readOSC:
    move.l  wave_get_output(a0),a1
    lea     .return(pc),a3
    jmp     (a1)
.return
    lsr.w   #4,d0
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
    bra     wave_set_sync_source

* in:
*    a0 = object
*    d0 = reg8 control
voice_writeCONTROL_REG:
    pushm   d0/a0
    move.l  voice_wave(a0),a0
    bsr     wave_writeCONTROL_REG
    popm    d0/a0
    move.l  voice_envelope(a0),a0
    bra     envelope_writeCONTROL_REG
   

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
*    a0 = wave
*    a2 = voice
* out:
*    d0 = Amplitude modulated waveform output.
*         Ideal range [-2048*255, 2047*255].
* uses:
*    d0-d4,a0,a1,a2

 REM
VOICE_OUT macro
    ;move.l  voice_wave(a2),a0
    lea     .vo\1(pc),a3
    ;bra     wave_output
    * Shortcut:
    move.l  wave_get_output(a0),a1
    jmp     (a1)
.vo\1
    * d0 = 12-bit
    sub.w   voice_wave_zero(a2),d0
    * envelope_output inlined:
    move.l  voice_envelope(a2),a1
    muls.w  envelope_counterHi(a1),d0
    * d0 = 20-bit
    add.l   voice_voice_DC(a2),d0
    endm

voice_output:
    VOICE_OUT
    rts
 EREM


******************************************************************************
*
* Envelope
*
******************************************************************************

adsrTable:
    dc.l $007f
    dc.l $3000
    dc.l $1e00
    dc.l $0660
    dc.l $0182
    dc.l $5573
    dc.l $000e
    dc.l $3805
    dc.l $2424
    dc.l $2220
    dc.l $090c
    dc.l $0ecd
    dc.l $010e
    dc.l $23f7
    dc.l $5237
    dc.l $64a8

* in:
*    a0 = object
envelope_constructor:
    bsr     envelope_reset
    rts

* in:
*    a0 = object
envelope_reset:
    clr.l   envelope_pipeline(a0)
    clr.l   envelope_state_pipeline(a0)

    clr.b   envelope_attack(a0)
    clr.b   envelope_decay(a0)
    clr.b   envelope_sustain(a0)
    clr.b   envelope_release(a0)

    clr.b   envelope_gate(a0)
    st      envelope_resetLfsr(a0)
    
    clr.l   envelope_exponential_counter(a0)
    move.l  #1,envelope_exponential_counter_period(a0)
    clr.l   envelope_new_exponential_counter_period(a0)

    move.b  #envelope_state_RELEASE,envelope_state(a0)
    move.b  #envelope_state_RELEASE,envelope_state_next(a0)
    st      envelope_counter_enabled(a0)

    move.l  #$7fff,envelope_lfsr(a0)
    move.b  #$aa,envelope_counter(a0)
    clr.l   envelope_rate(a0)

    move.l  adsrTable+0(pc),envelope_rate(a0)
    rts

* in:
*    a0 = object
*    d0 = control
* uses:
*    d0,d1,d2,a0,a1
envelope_writeCONTROL_REG:
    * gate_next
    and.b   #$01,d0
    cmp.b   envelope_gate(a0),d0
    beq     .fi
    move.b  d0,envelope_gate(a0)
    beq     .release

; attack:
    move.b  #envelope_state_ATTACK,envelope_state_next(a0)
    move.l  #2,envelope_state_pipeline(a0)

    tst.b   envelope_resetLfsr(a0)
    beq     .t
    cmp.l   #2,envelope_exponential_pipeline(a0)
    bne    .else
.t
    moveq   #2,d0
    cmp.l   #1,envelope_exponential_counter_period(a0)
    beq     .11
    cmp.l   #2,envelope_exponential_pipeline(a0)
    beq     .11
    moveq   #4,d0
.11
    move.l  d0,envelope_pipeline(a0)  
    rts

.else
    cmp.l   #1,envelope_exponential_pipeline(a0)
    bne     .fi
    move.l  #3,envelope_state_pipeline(a0)
.fi
    rts

.release
    move.b  #envelope_state_RELEASE,envelope_state_next(a0)
    moveq   #3,d0
    tst.l   envelope_pipeline(a0)
    bne     .22
    moveq   #2,d0
.22
    move.l  d0,envelope_state_pipeline(a0)
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
    move.l  adsrTable(pc,d1.w*4),envelope_rate(a0)
    rts
.1  
    cmp.b   #envelope_state_DECAY_SUSTAIN,envelope_state(a0)
    bne.b   .2
    move.l  adsrTable(pc,d0.w*4),envelope_rate(a0)
.2
    rts


* in:
*    a0 = object
*    d0 = sustain_release
envelope_writeSUSTAIN_RELEASE:
    move.b  #$f0,d1
    and.b   d0,d1
    move.b  d1,d2
    lsr.b   #4,d2
    or.b    d2,d1
    move.b  d1,envelope_sustain(a0)

    and     #$f,d0
    move.b  d0,envelope_release(a0)

    cmp.b   #envelope_state_RELEASE,envelope_state(a0)
    bne.b   .1
    move.l  adsrTable(pc,d0.w*4),envelope_rate(a0)
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
    move.w  envelope_counterHi(a0),d0
    rts

* EXPERIMENTAL ENVELOPE CLOCK
* in:
*   a0 = object
*   d0 = cycle_count delta_t
* uses:
*   d0-d7,a0,a1,a2
* note:
*   call counts from frame $b of Advanced Chemistry (calls per output sample)
ENVELOPE_CLOCK_ macro 
  endm



* in:
*   a0 = object
envelope_clock_single:
    tst.l   envelope_new_exponential_counter_period(a0)
    beq     .1
        move.l  envelope_new_exponential_counter_period(a0),envelope_exponential_counter_period(a0)
        clr.l   envelope_new_exponential_counter_period(a0)
.1
    tst.l   envelope_state_pipeline(a0)
    beq     .2
        bsr     envelope_state_change
.2

    tst.l   envelope_pipeline(a0)
    beq     .elseif1
    subq.l  #1,envelope_pipeline(a0)
    bne     .elseif1
        tst.b   envelope_counter_enabled(a0)
        beq     .endif1
            cmp.b   #envelope_state_ATTACK,envelope_state(a0)
            bne     .elseif2
                addq.b  #1,envelope_counter(a0)
                cmp.b   #$ff,envelope_counter(a0)
                bne     .endif2
                    move.b  #envelope_state_DECAY_SUSTAIN,envelope_state_next(a0)
                    move.l  #3,envelope_state_pipeline(a0)
                    bra .endif2
.elseif2
            cmp.b   #envelope_state_DECAY_SUSTAIN,envelope_state(a0)
            beq     .if3
            cmp.b   #envelope_state_RELEASE,envelope_state(a0)
            bne     .endif2
.if3
                subq.b  #1,envelope_counter(a0)
                bne     .endif2
                    clr.b   envelope_counter_enabled(a0)
.endif2
        bsr     envelope_set_exponential_counter
        bra     .endif5
.endif1
        bra     .endif5
    ; -----------------------
.elseif1
    tst.l   envelope_exponential_pipeline(a0)
    beq     .elseif3
    subq.l  #1,envelope_exponential_pipeline(a0)
    bne     .elseif3
        clr.l   envelope_exponential_counter(a0)
        cmp.b   #envelope_state_RELEASE,envelope_state(a0)
        beq     .if4
        cmp.b   #envelope_state_DECAY_SUSTAIN,envelope_state(a0)
        bne     .endif4
        move.b  envelope_sustain(a0),d0
        cmp.b   envelope_counter(a0),d0
        beq     .endif4
.if4
            move.l  #1,envelope_pipeline(a0)
.endif4
    bra     .endif5
    ; -----------------------
.elseif3
    tst.b   envelope_resetLfsr(a0)
    beq     .endif5
        clr.b   envelope_resetLfsr(a0)
        move.l  #$7fff,envelope_lfsr(a0)
        cmp.b   #envelope_state_ATTACK,envelope_state(a0)
        bne     .elseif4
            clr.l   envelope_exponential_counter(a0)
            move.l  #2,envelope_pipeline(a0)
            bra     .endif5
.elseif4
        tst.b   envelope_counter_enabled(a0)
        beq     .endif5
        addq.l  #1,envelope_exponential_counter(a0)
        move.l  envelope_exponential_counter_period(a0),d0
        cmp.l   envelope_exponential_counter(a0),d0
        bne     .endif5 
            moveq   #1,d0
            cmp.l   #1,envelope_exponential_counter_period(a0)
            beq     .11
            moveq   #2,d0
.11
            move.l  d0,envelope_exponential_pipeline(a0)
.endif5
    ; -----------------------

    move.l  envelope_lfsr(a0),d0
    cmp.l   envelope_rate(a0),d0
    beq     .endif6
        move.l  d0,d1
        lsl.l   #8,d1
        lsl.l   #5,d1
        move.l  d1,d2
        add.l   d2,d2
        eor.l   d2,d1
        and.l   #$4000,d1
        lsr.l   #1,d0
        or.l    d1,d0
        move.l  d0,envelope_lfsr(a0)
        rts
.endif6
        st  envelope_resetLfsr(a0)
    rts

envelope_set_exponential_counter:
    move.b  envelope_counter(a0),d0
    cmp.b   #$ff,d0
    beq     .1
    cmp.b   #$00,d0
    beq     .1
    cmp.b   #$5d,d0
    beq     .2
    cmp.b   #$36,d0
    beq     .4
    cmp.b   #$1a,d0
    beq     .8
    cmp.b   #$0e,d0
    beq     .16
    cmp.b   #$06,d0
    beq     .30
    rts
.1
    move.l  #1,envelope_new_exponential_counter_period(a0)
    rts
.2
    move.l  #2,envelope_new_exponential_counter_period(a0)
    rts
.4
    move.l  #4,envelope_new_exponential_counter_period(a0)
    rts
.8
    move.l  #8,envelope_new_exponential_counter_period(a0)
    rts
.16
    move.l  #16,envelope_new_exponential_counter_period(a0)
    rts
.30
    move.l  #30,envelope_new_exponential_counter_period(a0)
    rts

envelope_state_change:
    subq.l  #1,envelope_state_pipeline(a0)
    cmp.b   #envelope_state_ATTACK,envelope_state_next(a0)
    beq     .attack
    cmp.b   #envelope_state_DECAY_SUSTAIN,envelope_state_next(a0)
    beq     .decay_sustain
    cmp.b   #envelope_state_RELEASE,envelope_state_next(a0)
    beq     .release
    rts
    
.attack
    cmp.l   #1,envelope_state_pipeline(a0)
    bne     .at1
        moveq   #0,d0
        move.b  envelope_decay(a0),d0
        move.l  adsrTable(pc,d0*4),envelope_rate(a0)
        rts
.at1
    cmp.l   #0,envelope_state_pipeline(a0)
    bne     .at2
        move.b  #envelope_state_ATTACK,envelope_state(a0)
        moveq   #0,d0
        move.b  envelope_attack(a0),d0
        move.l  adsrTable(pc,d0*4),envelope_rate(a0)
        st      envelope_counter_enabled(a0)
.at2
        rts

.decay_sustain
    cmp.l   #0,envelope_state_pipeline(a0)
    bne     .ds1
        move.b  #envelope_state_DECAY_SUSTAIN,envelope_state(a0)
        moveq   #0,d0
        move.b  envelope_decay(a0),d0
        move.l  adsrTable(pc,d0*4),envelope_rate(a0)
.ds1
    rts

.release    
    cmp.b   #envelope_state_ATTACK,envelope_state(a0)
    bne     .if1
    tst.l   envelope_state_pipeline(a0)
    beq     .endif1
.if1
    cmp.b   #envelope_state_DECAY_SUSTAIN,envelope_state(a0)
    bne     .endif2
    cmp.l   #1,envelope_state_pipeline(a0)
    bne     .endif2
.endif1
        move.b  #envelope_state_RELEASE,envelope_state(a0)
        moveq   #0,d0
        move.b  envelope_release(a0),d0
        move.l  adsrTable(pc,d0*4),envelope_rate(a0)
.endif2
    rts

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
    clr.w   filter_hp_bp_lp(a0)
    clr.b   filter_vol(a0)
    clr.l   filter_volScaled(a0)
    clr.l   filter_Vhp(a0)
    clr.l   filter_Vbp(a0)
    clr.l   filter_Vlp(a0)
    clr.l   filter_Vnf(a0)
    bsr     filter_reset_jumps

    * Precalc constants for filter_set_w0
    ;fmove.s #3.1415926535897932385,fp0  * pi
    ;fmove   fp0,fp1
    ;fmul.s  #2*16000*1.048576,fp0
    ;fmul.s  #2*4000*1.048576,fp1
    ;fmove.l fp0,filter_w0_max_1(a0)
    ;fmove.l fp1,filter_w0_max_dt(a0)
    ; w0_max_1:  105414.35706657827311530803
    ; w0_max_dt:  26353.58926664456827882701
    move.l  #105414,filter_w0_max_1(a0)
    move.l  #26354,filter_w0_max_dt(a0)
  
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
    beq     .disable
    rts

.disable
    pea     filter_clock\.disabled(pc)
    move.l  (sp)+,filter_filt_jump(a0)
    rts

* in:
*    a0 = object
*    d0 = chip model
filter_set_chip_model
    cmp.b   #CHIP_MODEL_MOS6581,d0
    bne     .1

    ; (-$fff*$ff/18)>>7 = -453.22265625
    move.l  #-453,filter_mixer_DC(a0)
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
    clr.w   filter_hp_bp_lp(a0)
    clr.b   filter_vol(a0)
    clr.l   filter_volScaled(a0)
    clr.l   filter_Vhp(a0)
    clr.l   filter_Vbp(a0)
    clr.l   filter_Vlp(a0)
    clr.l   filter_Vnf(a0)

    bsr     filter_reset_jumps
    bsr     filter_set_w0
    bsr     filter_set_Q
    rts

* in:
*    a0 = object
filter_reset_jumps:
    ; Initialize jumps to default
    lea     filter_clock\.tab(pc),a1
    add.w   (a1),a1
    move.l  a1,filter_filt_jump(a0)
    lea     filter_output\.tab(pc),a1
    add.w   (a1),a1
    move.l  a1,filter_hp_bp_lp_jump(a0)
    rts

* in:
*    a0 = object
*    d0 = fc_lo
* uses:
*    d0,d1,a0,a1
filter_writeFC_LO:
    move    filter_fc(a0),d1
    and     #$7f8,d1
    and     #7,d0
    or      d0,d1
    move    d1,filter_fc(a0)
    bra     filter_set_w0


* in:
*    a0 = object
*    d0 = fc_hi
* uses:
*    d0,d1,a0,a1
filter_writeFC_HI:
    lsl.w   #3,d0
    moveq   #7,d1
    and     #$7f8,d0
    and     filter_fc(a0),d1
    or      d1,d0
    move    d0,filter_fc(a0)
    bra     filter_set_w0
    
    
* in:
*    a0 = object
*    d0 = res_filt
* uses:
*    d0,d1,a0,a1
filter_writeRES_FILT:
    move.b  d0,d1
    lsr.b   #4,d1
    and     #$f,d0
    move.b  d1,filter_res(a0)
    move.b  d0,filter_filt(a0)

    tst.b   filter_enabled(a0)
    beq     .disabled

    lea     filter_clock\.tab(pc),a1
    add.w   (a1,d0.w*2),a1
.1
    move.l  a1,filter_filt_jump(a0)
    bra     filter_set_Q
    
.disabled
    lea     filter_clock\.disabled(pc),a1
    bra      .1
    
* in:
*    a0 = sid object
*    a1 = filter object
*    d0 = mode_vol
filter_writeMODE_VOL:
    move.b  d0,d1
    smi     filter_voice3off(a1)

    and     #$f,d0
    move.b  d0,filter_vol(a1)
    
    mulu    sid_volume(a0),d0
    lsr     #6,d0
    move.l  d0,filter_volScaled(a1)

    lsr.b   #4,d1
    and     #7,d1
    move.w  d1,filter_hp_bp_lp(a1)

    lea     filter_output\.tab(pc),a0
    add.w   (a0,d1.w*2),a0
    move.l  a0,filter_hp_bp_lp_jump(a1)
    rts

* in:
*    a0 = filter object
*    d0 = volume
filter_set_volume:
    moveq   #0,d1
    move.b  filter_vol(a0),d1
    mulu    d1,d0
    lsr     #6,d0
    move.l  d0,filter_volScaled(a0)
    rts

* in:
*    a0 = object
* uses:
*    d0,d1,a0,a1
filter_set_w0:

    ;move.w  filter_fc(a0),d0
    ;fmove.w ([(filter_f0).w,a0],d0.w*2),fp0
    ;fmul.s  #2*3.1415926535897932385*1.048576,fp0
    ;fmove.l fp0,d0

    move.w  filter_fc(a0),d0
    move.l  filter_f0(a0),a1
    move.l  (a1,d0.w*4),d0
    * d0 = w0
    
    move.l  d0,filter_w0_ceil_1(a0)
    move.l  d0,filter_w0_ceil_dt(a0)
    
    move.l  filter_w0_max_1(a0),d1
    cmp.l   d1,d0
    bls.b   .1
    move.l  d1,filter_w0_ceil_1(a0)
.1
    move.l  filter_w0_max_dt(a0),d1
    cmp.l   d1,d0
    bls.b   .2
    move.l  d1,filter_w0_ceil_dt(a0)
.2
    rts

* in:
*    a0 = object
* uses:
*    a0,d0
filter_set_Q:
;    fmove.b filter_res(a0),fp0
;    fdiv.w  #$f,fp0
;    fadd.s  #0.707,fp0
;    fmove.s #1024,fp1
;    fdiv    fp0,fp1
;    fmove.l fp1,filter_1024_div_Q(a0)

    ; Use a lookup table instead
    moveq   #0,d0
    move.b  filter_res(a0),d0
    move.l  (.tab,pc,d0.w*4),filter_1024_div_Q(a0)
    rts

    ; Precalculated values for each resonance parameter
    cnop    0,4
.tab
    dc.l    $5a8
    dc.l    $52c
    dc.l    $4c3
    dc.l    $469
    dc.l    $41c
    dc.l    $3d8
    dc.l    $39d
    dc.l    $368
    dc.l    $33a
    dc.l    $30f
    dc.l    $2e9
    dc.l    $2c7
    dc.l    $2a7
    dc.l    $28b
    dc.l    $270
    dc.l    $258



* in:
*    a0 = object
*    d0 = cycle_count delta_t
*    d1 = voice1 sample
*    d2 = voice2 sample
*    d3 = voice3 sample
* uses:
*    d0-d7,a0-a3
filter_clock:
    * calls: 1x

    asr.l   #7,d1
    asr.l   #7,d2
    asr.l   #7,d3

    tst.b   filter_voice3off(a0)
    beq.b   .1
    moveq   #$4,d5
    and.b   filter_filt(a0),d5
    bne.b   .1
    clr.l   d3
.1

 REM ;; filter disabled combined with below jmp
    tst.b   filter_enabled(a0)
    bne.b   .3
    add.l   d3,d1
    add.l   d2,d1
    move.l  d1,filter_Vnf(a0)
    clr.l   filter_Vhp(a0)    
    clr.l   filter_Vbp(a0)    
    clr.l   filter_Vlp(a0)    
    ;rts
    bra     filter_output
.3 
 EREM

 REM
    moveq   #0,d5                * 1
    move.b  filter_filt(a0),d5   * 1
    move.w  (.tab,pc,d5.w*2),d5  * 1
    jmp     .tab(pc,d5)          * 5
 EREM
    move.l  filter_filt_jump(a0),a1 * 1
    jmp     (a1)                    * 5

.disabled
    add.l   d3,d1
    add.l   d2,d1
    move.l  d1,filter_Vnf(a0)
    clr.l   filter_Vhp(a0)    
    clr.l   filter_Vbp(a0)    
    clr.l   filter_Vlp(a0)    
    ; filter_output part:
    move.l  filter_Vnf(a0),d0
    add.l   filter_mixer_DC(a0),d0
    muls.l  filter_volScaled(a0),d0
    bra     filter_output_return

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
    add.l   d3,d1
    clr.l   d5
    add.l   d2,d1
    move.l  d1,filter_Vnf(a0)
    bra     .break
.f1
    add.l   d3,d2
    move.l  d1,d5
    move.l  d2,filter_Vnf(a0)
    bra     .break
.f2
    add.l   d3,d1
    move.l  d2,d5
    move.l  d1,filter_Vnf(a0)
    bra     .break
.f3
    move.l  d1,d5
    move.l  d3,filter_Vnf(a0)
    add.l   d2,d5
    bra     .break
.f4
    add.l   d2,d1
    move.l  d3,d5
    move.l  d1,filter_Vnf(a0)
    bra     .break
.f5
    move.l  d1,d5
    move.l  d2,filter_Vnf(a0)
    add.l   d3,d5
    bra     .break
.f6
    move.l  d2,d5
    move.l  d1,filter_Vnf(a0)
    add.l   d3,d5
    bra     .break
.f7
    move.l  d1,d5
    add.l   d2,d5
    clr.l   filter_Vnf(a0)
    add.l   d3,d5
    bra     .break
.f8
    add.l   d3,d1
    add.l   d2,d1
    clr.l   d5
    move.l  d1,filter_Vnf(a0)
    bra     .break
.f9
    add.l   d3,d2
    move.l  d1,d5
    move.l  d2,filter_Vnf(a0)
    bra     .break
.fa
    add.l   d3,d1
    move.l  d2,d5
    move.l  d1,filter_Vnf(a0)
    bra     .break
.fb
    move.l  d1,d5
    move.l  d3,filter_Vnf(a0)
    add.l   d2,d5
    bra     .break
.fc
    add.l   d2,d1
    move.l  d3,d5
    move.l  d1,filter_Vnf(a0)
    bra     .break
.fd
    move.l  d3,d5
    move.l  d2,filter_Vnf(a0)
    add.l   d1,d5
    bra     .break
.fe
    move.l  d3,d5
    move.l  d1,filter_Vnf(a0)
    add.l   d2,d5
    bra     .break
.ff
    move.l  d3,d5
    add.l   d2,d5
    clr.l   filter_Vnf(a0)
    add.l   d1,d5

.break

     * w0_delta_t is dependent on delta_t_flt
    * calc initial value with delta_t_flt=8

    move.l  filter_Vhp(a0),d3
    moveq   #10,d1 * shift
    move.l  filter_Vbp(a0),d4
    move.l  d5,a2
    move.l  filter_Vlp(a0),a3
    moveq   #14,d5 * shift
    move.l  filter_1024_div_Q(a0),a1

    ; ---------------------------------
    cmp.w   #36,d0
    bne     .normal
    COUNT   C_FLT1

    * Special case for 36 cycles
    * Value for 8 cycles at a time
    move.l  filter_w0_ceil_dt(a0),d0
    asr.l   #3,d0 ; mul #8, asr #6
 rept 4
    move.l  d4,d7 * 0 sOEP
    muls.l  d0,d7 * 2 pOEP only
    move.l  d3,d6 * 1 pOEP
    asr.l   d5,d7 * 0 sOEP
    move.l  a1,d3 * 1 pOEP
    sub.l   d7,a3 * 0 sOEP
    muls.l  d0,d6 * 2 pOEP only
    muls.l  d4,d3 * 2 pOEP only
    asr.l   d1,d3 * 1 pOEP
    asr.l   d5,d6 * 0 sOEP
    sub.l   a3,d3 * 1 pOEP
    sub.l   d6,d4 * 0 sOEP
    sub.l   a2,d3 * 1 pOEP
 endr
    * 32 cycles done, 4 left
    * delta_t_flt changed, update w0_delta_t
    asr.l   #1,d0 ; mul #4, asr #6
    bra     .rest2
    ; ---------------------------------
.normal
    COUNT   C_FLT2

    * Cycles to run more than 8?
    * It is always > 0 here.
    subq.l  #8,d0
    ble     .rest

    * Enter 8 cycle loop
    * Value for 8 cycles at a time
    move.l  filter_w0_ceil_dt(a0),d2
    asr.l   #3,d2 ; mul #8, asr #6

    * d5 = shift
    * a2 = Vi
    * d0 = delta_t
    * d1 = delta_t_flt

; REM ;;;;;;;;; OPTION 1
;;.loop
;;    * calls: 6x
;;    COUNT   C_FLT1
;;
;;    cmp.l   d1,d0
;;    bhs.b   .4
;;    move.l  d0,d1
;;    * delta_t_flt changed, update w0_delta_t
;;    move.l  filter_w0_ceil_dt(a0),d2
;;    muls.l  d1,d2
;;    asr.l   #6,d2
;;    COUNT   C_FLT2
;;.4  

; Original:
;    * dVlp
;    move.l  d4,d7
;    muls.l  d2,d7
;    asr.l   d5,d7
;    * Vlp -= dVlp
;    sub.l   d7,a3
; 
;    * dVbp
;    move.l  d3,d7
;    muls.l  d2,d7
;    asr.l   d5,d7
;    * Vbp -= dVbp
;    sub.l   d7,d4
;
;    * Vhp  
;    move.l  a1,d3
;    muls.l  d4,d3
;    asr.l   #8,d3
;    asr.l   #2,d3
;
;    sub.l   a3,d3
;    sub.l   a2,d3

;;    * The above interleaved:
;;    * OPTION 1
;;    move.l  d4,d7 
;;    muls.l  d2,d7 * 2 pOEP only
;;    move.l  d3,d6 * 1 pOEP
;;    asr.l   d5,d7 * 0 sOEP
;;    move.l  a1,d3 * 1 pOEP
;;    muls.l  d2,d6 * 2 pOEP only
;;    sub.l   d7,a3 * 1 pOEP
;;    muls.l  d4,d3 * 2 pOEP only
;;    asr.l   d5,d6 * 1 pOEP
;;    asr.l   #8,d3 * 0 sOEP
;;    sub.l   d6,d4 * 1 pOEP
;;    asr.l   #2,d3 * 0 sOEP
;;    sub.l   a3,d3 * 1 pOEP
;;    sub.l   a2,d3 * 1 pOEP
;;    sub.l   d1,d0 * 0 sOEP
;;    * 13 cycles
;;
;;    * The above interleaved:
;;    * OPTION 2
;;    move.l  d4,d7 
;;    muls.l  d2,d7 * 2 pOEP only
;;    move.l  d3,d6 * 1 pOEP
;;    asr.l   d5,d7 * 0 sOEP
;;    move.l  a1,d3 * 1 pOEP
;;    sub.l   d7,a3 * 0 sOEP
;;    muls.l  d2,d6 * 2 pOEP only
;;    muls.l  d4,d3 * 2 pOEP only
;;    asr.l   #8,d3 * 1 pOEP
;;    asr.l   d5,d6 * 0 sOEP
;;    asr.l   #2,d3 * 1 pOEP
;;    sub.l   d6,d4 * 0 sOEP
;;    sub.l   a3,d3 * 1 pOEP
;;    sub.l   a2,d3 * 1 pOEP
;;    sub.l   d1,d0 * 0 sOEP
;;    * 12 cycles
;;
;;    bne     .loop
;;.x
 ;;;;;;;; OPTION 1 END

; REM ;;;;;;; OPTION 2
  
    * Start with 8 cycle steps

    COUNT   C_FLT3
.loop
    COUNT   C_FLT4
 
    * ---------------------------------
    * The above interleaved:
    * OPTION 2
    move.l  d4,d7 * 0 or 1
    muls.l  d2,d7 * 2 pOEP only
    move.l  d3,d6 * 1 pOEP
    asr.l   d5,d7 * 0 sOEP
    move.l  a1,d3 * 1 pOEP
    sub.l   d7,a3 * 0 sOEP
    muls.l  d2,d6 * 2 pOEP only
    muls.l  d4,d3 * 2 pOEP only
    asr.l   d1,d3 * 1 pOEP
    asr.l   d5,d6 * 0 sOEP
    sub.l   a3,d3 * 1 pOEP
    sub.l   d6,d4 * 0 sOEP
    sub.l   a2,d3 * 1 pOEP
    subq.l  #8,d0 * 0 sOEP, loop counter
    * 12/13 cycles
    bgt     .loop * 0/7 pOEP
    * ---------------------------------
.rest
    * Rest of the 1-8 cycles
    addq.l  #8,d0
    COUNT   C_FLT5

    * delta_t_flt changed, update w0_delta_t
    muls.l   filter_w0_ceil_dt(a0),d0
    asr.l   #6,d0
.rest2
    COUNT   C_FLT6
    * ---------------------------------
    * OPTION 3
    move.l  d4,d7 
    muls.l  d0,d7 * 2 pOEP only
    move.l  d3,d6 * 1 pOEP
    asr.l   d5,d7 * 0 sOEP
    move.l  a1,d3 * 1 pOEP
    sub.l   d7,a3 * 0 sOEP
    muls.l  d0,d6 * 2 pOEP only
    muls.l  d4,d3 * 2 pOEP only
    asr.l   d1,d3 * 1 pOEP
    asr.l   d5,d6 * 0 sOEP
    sub.l   a3,d3 * 1 pOEP
    sub.l   d6,d4 * 0 sOEP
    sub.l   a2,d3 * 1 pOEP
    * 11
    * ---------------------------------
    
.exit
;; EREM ;;;;;;;;; OPTION 2 END

    move.l  d4,filter_Vbp(a0)
    move.l  d3,filter_Vhp(a0)
    move.l  a3,filter_Vlp(a0)

    ;rts
    ; ... fall through to filter_output

* in:
*    a0 = object
*    d3 = Vhp from above
*    d4 = Vbp from above
*    a3 = Vlp from above
* out:
*    d0 = filter output 20 bits
* uses:
*    d0,d1,a0,a1
filter_output:
    * calls: 1x

    move.l  filter_Vnf(a0),d0
    add.l   filter_mixer_DC(a0),d0

 REM
    tst.b   filter_enabled(a0)
    bne.b   .1
    muls.l  filter_volScaled(a0),d0
    bra     filter_output_return
.1  
 EREM

 REM ; option 1
    move.w  filter_hp_bp_lp(a0),d1
    move.w  .tab(pc,d1.w*2),d1
    jmp     .tab(pc,d1)
 EREM
    move.l  filter_hp_bp_lp_jump(a0),a1
    jmp     (a1)

.tab
    dc.w    .f0-.tab
    dc.w    .f1-.tab
    dc.w    .f2-.tab
    dc.w    .f3-.tab
    dc.w    .f4-.tab
    dc.w    .f5-.tab
    dc.w    .f6-.tab
    dc.w    .f7-.tab
 
.f1
    add.l   a3,d0
.f0
    muls.l  filter_volScaled(a0),d0
    bra     filter_output_return
.f2
    add.l   d4,d0
    muls.l  filter_volScaled(a0),d0
    bra     filter_output_return
.f3
    add.l   a3,d0
    add.l   d4,d0
    muls.l  filter_volScaled(a0),d0
    bra     filter_output_return
.f4
    add.l   d3,d0
    muls.l  filter_volScaled(a0),d0
    bra     filter_output_return
.f5
    add.l   a3,d0
    add.l   d3,d0
    muls.l  filter_volScaled(a0),d0
    bra     filter_output_return
.f6
    add.l   d4,d0
    add.l   d3,d0
    muls.l  filter_volScaled(a0),d0
    bra     filter_output_return
.f7
    add.l   a3,d0
    add.l   d4,d0
    add.l   d3,d0
    muls.l  filter_volScaled(a0),d0
    bra     filter_output_return


******************************************************************************
*
* External filter
*
******************************************************************************

EXTFILTER_W0LP = 104858
EXTFILTER_W0HP = 105

* in:
*    a0 = object
extfilter_constructor:
    bsr     extfilter_reset
    moveq   #1,d0
    bsr     extfilter_enable_filter
    moveq   #CHIP_MODEL_MOS6581,d0
    bsr     extfilter_set_chip_model
    move.l  #EXTFILTER_W0LP,extfilter_w0lp(a0)
    move.l  #EXTFILTER_W0HP,extfilter_w0hp(a0)
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
    ; ((((0x800 - 0x380) + 0x800)*0xff*3 - 0xfff*0xff/18) >> 7)*0x0f
    ; = 280076.66015625 or 
    ; = 280065 
    move.l  #280065,extfilter_mixer_DC(a0)
    rts
.1
    clr.l   extfilter_mixer_DC(a0)
    rts

* in:
*   a0 = object
*   d0 = cycle_count delta_t
*   d1 = sample Vi
* Out:
*   d1 = extfilter_Vo
* uses:
*   d0-d7,a0,a1,a2,a3
extfilter_clock:
    tst.b   extfilter_enabled(a0)
    bne     .1
    clr.l   extfilter_Vlp(a0)
    clr.l   extfilter_Vhp(a0)
    sub.l   extfilter_mixer_DC(a0),d1
    move.l  d1,extfilter_Vo(a0)
    rts

.1
    move.l  d1,a1

    move.l  extfilter_Vlp(a0),d6
;    moveq   #8,d2
    moveq   #12,d2
    move.l  extfilter_Vhp(a0),a3
    moveq   #20,d1  * shift

    ; ---------------------------------
    cmp.w   #36,d0
    bne     .normal
    COUNT   C_EFLT1

    move.l  #(EXTFILTER_W0LP*8)>>8,d3 ; 3276
    move.l  #(EXTFILTER_W0HP)<<3,d4   ; 840
 rept 4
    move.l  d6,d7
    sub.l   a3,d7
    move.l  d7,a2
    muls.l  d4,d7 * 2 pOEP only
    move.l  a1,d5 * 1 pOEP 
    asr.l   d1,d7 * 0 sOEP
    sub.l   d6,d5 * 1 pOEP
    add.l   d7,a3 * 0 sOEP
    muls.l  d3,d5 * 2 pOEP only
    asr.l   d2,d5 * 1 pOEP
    add.l   d5,d6 * 1 pOEP   
 endr
    * 32 cycles done, 4 left
    asr.l   #1,d3 
    asr.l   #1,d4 
    bra     .rest2

.normal
    COUNT   C_EFLT2

    * Cycles to run more than 8?
    * It is always > 0 here.
    subq.l  #8,d0
    ble     .rest

;    move.l  extfilter_w0lp(a0),d3
;    move.l  extfilter_w0hp(a0),d4
;    asr.l   #5,d3 * mul 8, asr 8
;    asl.l   #3,d4 * mul 8
    move.l  #(EXTFILTER_W0LP*8)>>8,d3 ; 3276
    move.l  #(EXTFILTER_W0HP)<<3,d4   ; 840


    * d2 = delta_t_flt
    * a1 = Vi

;; REM ;;;;; OPTION 1    
;;.loop
;;    COUNT   C_EFLT1
;;
;;    * calls: 6x
;;
;;    cmp.l   d2,d0
;;    bhs.b   .2
;;    move.l  d0,d2
;;    * delta_t_flt changed
;;    move.l  extfilter_w0lp(a0),d3
;;    muls.l  d2,d3
;;    move.l  extfilter_w0hp(a0),d4
;;    muls.l  d2,d4
;;    asr.l   #8,d3
;;    COUNT   C_EFLT2
;;.2
;    ; d7 = Vi - Vlp
;    move.l  a4,d7
;    sub.l   a3,d7
;
;    * a2 = Vo = Vlp - Vhp
;    move.l  d7,a2
;
;    * d7 = dVhp
;    muls.l  d4,d7
;    asr.l   d1,d7
;    * Vhp += dVhp
;    add.l   d7,a3
;
;    * dVlp
;    move.l  a1,d5
;    sub.l   a4,d5
;    muls.l  d3,d5
;    asr.l   #8,d5
;    asr.l   #4,d5
;    * Vlp += dVlp
;    add.l   d5,a4

    * The above interleaved:
; REM ; option 1
;    move.l  a4,d7
;    sub.l   a3,d7
;    move.l  d7,a2
;    muls.l  d4,d7 * 2 pOEP only
;    move.l  a1,d5 * 1 pOEP 
;    asr.l   d1,d7 * 0 sOEP
;    sub.l   a4,d5 * 1 pOEP
;    muls.l  d3,d5 * 2 pOEP only
;    add.l   d7,a3 * 1 pOEP
;    asr.l   d6,d5 * 0 sOEP
;    add.l   d5,a4 * 1 pOEP
;    sub.l   d2,d0 * 0 sOEP
;    bne     .loop
; EREM

;; ; option 2 - similar speed but less regs
;;    move.l  d6,d7
;;    sub.l   a3,d7
;;    move.l  d7,a2
;;    muls.l  d4,d7 * 2 pOEP only
;;    move.l  a1,d5 * 1 pOEP 
;;    asr.l   d1,d7 * 0 sOEP
;;    sub.l   d6,d5 * 1 pOEP
;;    add.l   d7,a3 * 0 sOEP
;;    muls.l  d3,d5 * 2 pOEP only
;;    moveq   #12,d7 *1 pOEP
;;    asr.l   d7,d5 * 0 sOEP, move.l ,Rx
;;    add.l   d5,d6 * 1 pOEP
;;    sub.l   d2,d0 * 0 sOEP
;;    bne     .loop 
;;
;;.x
;; EREM ;;; OPTION 1 END

    ;;; OPTION 2 START
    COUNT   C_EFLT3
.loop
    COUNT   C_EFLT4
    move.l  d6,d7
    sub.l   a3,d7
    move.l  d7,a2
    muls.l  d4,d7 * 2 pOEP only
    move.l  a1,d5 * 1 pOEP 
    asr.l   d1,d7 * 0 sOEP
    sub.l   d6,d5 * 1 pOEP
    add.l   d7,a3 * 0 sOEP
    muls.l  d3,d5 * 2 pOEP only
    asr.l   d2,d5 * 1 pOEP
    add.l   d5,d6 * 1 pOEP    
    subq.l  #8,d0 * 0 sOEP, loop counter
    bgt     .loop * 1 pOEP

.rest
   * Rest of the 1-8 cycles
    addq.l  #8,d0

    COUNT   C_EFLT5
    ; ---------------------------------
    * delta_t_flt changed
    ;move.l  extfilter_w0lp(a0),d3
    ;muls.l  d0,d3
    ;move.l  extfilter_w0hp(a0),d4
    ;muls.l  d0,d4
    ;asr.l   #8,d3
    move.l  #(EXTFILTER_W0LP),d3
    moveq   #(EXTFILTER_W0HP),d4   
    muls.l  d0,d3
    muls.l  d0,d4
    asr.l   #8,d3
.rest2
    COUNT   C_EFLT6
    ; ---------------------------------
    move.l  d6,d7
    sub.l   a3,d7
    move.l  d7,a2
    muls.l  d4,d7 * 2 pOEP only
    move.l  a1,d5 * 1 pOEP 
    asr.l   d1,d7 * 0 sOEP
    sub.l   d6,d5 * 1 pOEP
    add.l   d7,a3 * 0 sOEP
    muls.l  d3,d5 * 2 pOEP only
    asr.l   d2,d5 * 1 pOEP
    move.l  a2,extfilter_Vo(a0) * 0 sOEP
    add.l   d5,d6 * 1 pOEP
    move.l  a3,extfilter_Vhp(a0) * 0 sOEP
    move.l  a2,d1  * d1 = output * 1 pOEP
    move.l  d6,extfilter_Vlp(a0) * 0 sOEP
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
    lea     resid_voice1(a0),a1
    lea     resid_voice2(a0),a2
    lea     resid_voice3(a0),a3

    pea     resid_envelope1(a0)
    move.l  (sp)+,voice_envelope(a1)
    pea     resid_envelope2(a0)
    move.l  (sp)+,voice_envelope(a2)
    pea     resid_envelope3(a0)
    move.l  (sp)+,voice_envelope(a3)

    pea     resid_wave1(a0)    
    move.l  (sp)+,voice_wave(a1)
    pea     resid_wave2(a0)    
    move.l  (sp)+,voice_wave(a2)
    pea     resid_wave3(a0)    
    move.l  (sp)+,voice_wave(a3)

    move.l  a1,sid_voice1(a0)
    move.l  a2,sid_voice2(a0)
    move.l  a3,sid_voice3(a0)

    pea     resid_filter(a0)
    move.l  (sp)+,sid_filter(a0)

    pea     resid_extfilter(a0)
    move.l  (sp)+,sid_extfilt(a0)

    * ... and construct with defaults
    push    a0
    move.l  a0,a4
    lea     resid_voice1(a4),a0
    bsr     voice_constructor
    lea     resid_voice2(a4),a0
    bsr     voice_constructor
    lea     resid_voice3(a4),a0
    bsr     voice_constructor
    lea     resid_filter(a4),a0
    bsr     filter_constructor
    lea     resid_extfilter(a4),a0
    bsr     extfilter_constructor
    pop     a0

    clr.b   sid_bus_value(a0)
    clr.l   sid_bus_value_ttl(a0)
    move    #$40,sid_volume(a0)

    move.l  #985248,d0
    moveq   #SAMPLING_METHOD_SAMPLE_FAST8,d1
    move.l  #44100/2,d2
    bsr     sid_set_sampling_parameters

    lea     resid_voice1(a4),a0
    lea     resid_voice3(a4),a1
    bsr     voice_set_sync_source

    lea     resid_voice2(a4),a0
    lea     resid_voice1(a4),a1
    bsr     voice_set_sync_source

    lea     resid_voice3(a4),a0
    lea     resid_voice2(a4),a1
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
    move.l  a0,a1
    move.l  sid_extfilt(a1),a0
    bsr     extfilter_enable_filter

    tst.b   d0
    bne.b   .enabled
    * Ext filter not enabled!
    * Hack around with the DC levels so that the signal stays centered
    * centered in the value range.
    
    clr.l   extfilter_mixer_DC(a0)
    move.l  sid_filter(a1),a0
    ;clr.l   filter_mixer_DC(a0)
  
    move.l  sid_voice1(a1),a0
    ;move.w  #$800,voice_wave_zero(a0)
    clr.l   voice_voice_DC(a0)

    move.l  sid_voice2(a1),a0
    ;move.w  #$800,voice_wave_zero(a0)
    clr.l   voice_voice_DC(a0)
    
    move.l  sid_voice3(a1),a0
    ;move.w  #$800,voice_wave_zero(a0)
    clr.l   voice_voice_DC(a0)

.enabled
    rts

* in:
*   a0 = object
*   d0 = volume 0..64
sid_set_volume:
    move    d0,sid_volume(a0)
    move.l  sid_filter(a0),a0
    bra     filter_set_volume

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
    move.l  sid_extfilt(a5),a0
    bsr     extfilter_set_chip_model
    move.l  sid_filter(a5),a0
    bsr     filter_set_chip_model   * destroys d0
    rts

* in:
*   a0 = object
sid_reset:
    move.l  a0,a5
    clr.b   sid_bus_value(a5)
    clr.l   sid_bus_value_ttl(a5)
    move    #$40,sid_volume(a5)
    clr.l   sid_sample_prev(a5)

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

 if COUNTERS
    bsr     reset_counters
 endif
    rts

* UNUSED
* in:
*   a0 = object
* out:
*   d0 = 8-bit output
* uses: 
*   d0,a0
 REM
sid_output8:
.range = 1<<8
.half  = .range>>1
.divisor = (((4095*255>>7)*3*15*2)/.range)
; this is 1439.6484375, round up

    move.l  sid_extfilt(a0),a0
    ;extfilter_output inlined
    move.l  extfilter_Vo(a0),d0
    divs.w  #1440,d0

    * clamp to [-128..127]
    CLAMP8  d0
    rts
 EREM

* UNUSED
* in:
*   a0 = object
* out:
*   d0 = 16-bit output
* uses: 
*   d0,a0
 REM
sid_output16:
.range = 1<<16
.half  = .range>>1
; Divisor is (4095*255>>7)*3*15*2/(1<<15) = 11.24725341796875
; Inverse with shift 10: (1/11.24725341796875)*(1<<10) = 91.0444

    moveq   #10,d1      * FP 10
    move.l  sid_extfilt(a0),a0
    ;extfilter_output inlined
    moveq   #91,d0
    muls.l   extfilter_Vo(a0),d0
    asr.l   d1,d0
    
    CLAMP16 d0
    rts
 EREM

* in:
*   a0 = object
*   d0 = byte value to write
*   d1 = SID register offset
* uses:
*   d0,d1,d2,a0,a1
sid_write:
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
    move.l  sid_voice1(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writeFREQ_LO
.w01:
    move.l  sid_voice1(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writeFREQ_HI
.w02:
    move.l  sid_voice1(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writePW_LO
.w03:
    move.l  sid_voice1(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writePW_HI
.w04:
    move.l  sid_voice1(a0),a0
    bra     voice_writeCONTROL_REG
.w05:
    move.l  sid_voice1(a0),a0
    move.l  voice_envelope(a0),a0
    bra     envelope_writeATTACK_DECAY
.w06:
    move.l  sid_voice1(a0),a0
    move.l  voice_envelope(a0),a0
    bra     envelope_writeSUSTAIN_RELEASE
.w07:
    move.l  sid_voice2(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writeFREQ_LO
.w08:
    move.l  sid_voice2(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writeFREQ_HI
.w09:
    move.l  sid_voice2(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writePW_LO
.w0a:
    move.l  sid_voice2(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writePW_HI
.w0b:
    move.l  sid_voice2(a0),a0
    bra     voice_writeCONTROL_REG
.w0c:
    move.l  sid_voice2(a0),a0
    move.l  voice_envelope(a0),a0
    bra     envelope_writeATTACK_DECAY
.w0d:
    move.l  sid_voice2(a0),a0
    move.l  voice_envelope(a0),a0
    bra     envelope_writeSUSTAIN_RELEASE
.w0e:
    move.l  sid_voice3(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writeFREQ_LO
.w0f:
    move.l  sid_voice3(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writeFREQ_HI
.w10:
    move.l  sid_voice3(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writePW_LO
.w11:
    move.l  sid_voice3(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_writePW_HI
.w12:
    move.l  sid_voice3(a0),a0
    bra     voice_writeCONTROL_REG
.w13:
    move.l  sid_voice3(a0),a0
    move.l  voice_envelope(a0),a0
    bra     envelope_writeATTACK_DECAY
.w14:
    move.l  sid_voice3(a0),a0
    move.l  voice_envelope(a0),a0
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
    move.l  sid_filter(a0),a1
    bra     filter_writeMODE_VOL


sid_set_sampling_method:   
    * Default scale factor to scale to final 16-bit sample range
    * output*scale/(1<<10)
    move.l  #91,sid_outputScale(a0)
    ; ---------------------------------
    lea     sid_clock_fast8(pc),a1
    cmp.b   #SAMPLING_METHOD_SAMPLE_FAST8,d1
    beq     .go

    lea     sid_clock_fast14(pc),a1
    cmp.b   #SAMPLING_METHOD_SAMPLE_FAST14,d1
    beq     .go

    lea     sid_clock_fast16(pc),a1
    cmp.b   #SAMPLING_METHOD_SAMPLE_FAST16,d1
    beq     .go

    ; ---------------------------------
    lea     sid_clock_interpolate14(pc),a1
    cmp.b   #SAMPLING_METHOD_INTERPOLATE14,d1
    beq     .go
    
    ; ---------------------------------
    lea     sid_clock_oversample14(pc),a1

    move.l  #2,sid_oversample(a0)
    move.l  #46,sid_outputScale(a0)
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE2x14,d1
    beq     .go

    move.l  #3,sid_oversample(a0)
    move.l  #31,sid_outputScale(a0)
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE3x14,d1
    beq     .go

    move.l  #4,sid_oversample(a0)
    move.l  #23,sid_outputScale(a0)
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE4x14,d1
    beq     .go

    ; ---------------------------------
    lea     sid_clock_oversample16(pc),a1

    move.l  #2,sid_oversample(a0)
    move.l  #46,sid_outputScale(a0)
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE2x16,d1
    beq     .go

    move.l  #3,sid_oversample(a0)
    move.l  #31,sid_outputScale(a0)
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE3x16,d1
    beq     .go

    move.l  #4,sid_oversample(a0)
    move.l  #23,sid_outputScale(a0)
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE4x16,d1
    beq     .go

    * Error: set Z
    or.b	#(1<<2),ccr
    rts
.go
    * Ok: clear Z
    and.b	#~(1<<2),ccr
    rts

* in:
*   a0 = object
*   d0 = clock_frequency
*   d1 = sampling method
*   d2 = sampling frequency in Hz
* out:
*   d0 = true if ok, false with a bad param combo
*   a1 = clock routine address based on the sampling method
sid_set_sampling_parameters:

    move.l  d0,sid_clock_frequency(a0)
    move.b  d1,sid_sampling_method(a0)
    clr.l   sid_sample_offset(a0)
    
    bsr     sid_set_sampling_method
    beq     .fail

    cmp.b   #SAMPLING_METHOD_OVERSAMPLE2x14,sid_sampling_method(a0)
    beq.b   .2
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE3x14,sid_sampling_method(a0)
    beq.b   .2
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE4x14,sid_sampling_method(a0)
    beq.b   .2
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE2x16,sid_sampling_method(a0)
    beq.b   .2
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE3x16,sid_sampling_method(a0)
    beq.b   .2
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE4x16,sid_sampling_method(a0)
    bne.b   .1
.2
    mulu.l  sid_oversample(a0),d2
.1

    ; Calculate cycles per sample as a fixed point value

    ;cycles_per_sample = clock_freq / sample_freq * (1 << FIXP_SHIFT) + 0.5);
    ;fmove.l d0,fp0
    ;fdiv.l  d2,fp0      
    ;move.l  #1<<FIXP_SHIFT,d3
    ;fmul.l  d3,fp0
    ;fadd.s  #0.5,fp0
    ;fmove.l fp0,sid_cycles_per_sample(a0)
   
    mulu.l  #(1<<FIXP_SHIFT)*10,d1:d0    
    divu.l  d2,d1:d0
    addq.l  #5,d0
    divu.l  #10,d0
    move.l  d0,sid_cycles_per_sample(a0)

    moveq   #1,d0
    rts
.fail
    moveq   #0,d0
    rts

* in:
*   a0 = object
*   d0 = clock_frequency
*   d1 = sampling method
*   d2 = Paula period value to determine the sampling frequency
* out:
*   d0 = true if ok, false with a bad param combo
*   a1 = clock routine address based on the sampling method
sid_set_sampling_parameters_paula:

    move.l  d0,sid_clock_frequency(a0)
    move.b  d1,sid_sampling_method(a0)
    clr.l   sid_sample_offset(a0)

    bsr     sid_set_sampling_method
    beq     .fail

    ; Calculate cycles per sample as a fixed point value

    ;cycles_per_sample = clock_freq / sample_freq * (1 << FIXP_SHIFT) + 0.5);

.PAL_CLOCK=3546895   

    * FP shift 10
    move.l  #.PAL_CLOCK<<10,d3
    divu.l  d2,d3
    * d3 = Playback frequency in Hz as 22.10 FP

    cmp.b   #SAMPLING_METHOD_OVERSAMPLE2x14,sid_sampling_method(a0)
    beq.b   .2
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE3x14,sid_sampling_method(a0)
    beq.b   .2
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE4x14,sid_sampling_method(a0)
    beq.b   .2
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE2x16,sid_sampling_method(a0)
    beq.b   .2
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE3x16,sid_sampling_method(a0)
    beq.b   .2
    cmp.b   #SAMPLING_METHOD_OVERSAMPLE4x16,sid_sampling_method(a0)
    bne.b   .1
.2
    mulu.l  sid_oversample(a0),d3
.1

    mulu.l  #10*(1<<FIXP_SHIFT)<<10,d1:d0    
    divu.l  d3,d1:d0
    addq.l  #5,d0
    divu.l  #10,d0
    move.l  d0,sid_cycles_per_sample(a0)

    moveq   #1,d0
    rts
.fail
    moveq   #0,d0
    rts

* In:
*   a0 = object
*   d0 = boost value to adjust final sample scaling, boost by factor x. Range 0..4
sid_set_output_boost: 
    cmp.l   #4,d0
    bls     .1
    moveq   #0,d0
.1
    move.l  d0,sid_outputBoost(a0)
    rts


* Out:
*   d6 = Boosted or unchanged output scale factor
sid_get_outputScale:
    move.l  sid_outputScale(a0),d6
    move.l  sid_outputBoost(a0),d7
    beq     .1
    mulu.w  d7,d6
.1
    rts

* in:
*   a5 = object
*   d0 = cycle_count delta_t
* out:
*   d1 = extfilter_Vo
* uses:
*   d0-d7,a0-a3
*   a5 preserved
* notes:
*   clock cycles per one call with different sampling modes
*   with the ~28 kHz base sampling rate:
*   - fast: cycles=36, remaining=31
*   - interpolate: 27 + 8
*   - oversample x2: 17/18
*   - oversample x3: 11/12
*   - oversample x4: 8/9
    
sid_clock:
    tst.l   d0
    bgt     .1
    COUNT   C_CLK5
    * No cycles to run, return previous output
    move.l  sid_extfilt(a5),a0
    move.l  extfilter_Vo(a0),d1
    rts
.1
 
    COUNT   C_CLK1
    * calls: 1x, once per output sample in fast

    move.l  d0,a3
    push    d0      * save delta_t

    ; ---------------------------------
    ; Run envelopes

    ;move.l  sid_voice1(a5),a0
    ;move.l  voice_envelope(a0),a0
    lea     resid_envelope1(a5),a0
    moveq   #3-1,d7
.envLoop
    move.l  a3,d0

.envl
    push    d0
    bsr     envelope_clock_single
    pop     d0
    subq    #1,d0
    bne     .envl

;    ENVELOPE_CLOCK_

    * assume envelope objects are stored one after another
    lea     envelope_SIZEOF(a0),a0
    dbf     d7,.envLoop

    ; ---------------------------------

    move.l  a3,d7
    * d7 = delta_t_osc = delta_t
.loop
    COUNT   C_CLK2
    * a3 = delta_t_min = delta_t_osc
    move.l  d7,a3

    ; Find minimum number of cycles to an oscillator accumulator MSB toggle.
    ; We have to clock on each MSB on / MSB off for hard sync to operate
    ; correctly.

    ;move.l  sid_voice1(a5),a0
    ;move.l  voice_wave(a0),a0
    lea     resid_wave1(a5),a0
    * assume wave objects are stored one after another
    * Loop three waves
    moveq   #3-1,d6
.cycleCheck
    * calls: 3x

    ; It is only necessary to clock on the MSB of an oscillator that is
    ; a sync source and has freq != 0.
    move.w  wave_freq(a0),d3
    beq     .cx
    move.l  wave_sync_dest(a0),a1
    tst.b   wave_sync(a1)
    beq     .cx

    COUNT   C_CLK3

    move.l  wave_accumulator(a0),d1 * max: $ffffff
    move.l  #$800000,d2
    btst    #23,d1      * andi.l #$800000 
    beq.b   .a
    add.l   d2,d2
.a  sub.l   d1,d2   * d2 = max 0x800000

    * d2 = delta_accumulator

    * The lowest note C-0 freq value is around $115. Lowest divisor so the 
    * quotient fits into 16 bits is $81. We can likely use 16-bit division
    * as it's <=22(1/0) where the 32-bit is 38(1/0) on 68060.
    * 27 and 44 on 68040.
    cmp.w   #$81,d3
    bhs     .shortDivOk
    
    COUNT   C_CLK4

    ; This is unlikely.
    ; Fallback to 32-bit div in case of an overflow
    ext.l   d3          * d3 = 1..$80
    divul.l d3,d3:d2
    * d2 = delta_t_next = delta_accumulator / freq
    * d3 = remainder (ie. modulo)
    tst.l   d3
    beq.b   .e
    addq.l  #1,d2
.e
    cmp.l   a3,d2
    bhs.b   .cx
    move.l  d2,a3
    bra.b   .cx

.shortDivOk
    divu.w  d3,d2
    * d2.w = delta_t_next = delta_accumulator / freq
    * Convert unsigned 16-bit quotient into unsigned 32-bit
    moveq   #0,d1
    move.w  d2,d1
    * Test if remainder is zero
    clr.w   d2
    tst.l   d2
    beq.b   .f
    addq.l  #1,d1
.f
    cmp.l   a3,d1
    bhs.b   .cx
    ; a3 = delta_t_min = delta_t_next
    move.l  d1,a3
.cx
    ; next wave
    lea     wave_SIZEOF(a0),a0
    dbf     d6,.cycleCheck

    ; a0 = wave3 + wave_SIZEOF

    ; ---------------------------------

    ; clock oscillators with delta_t_min
    ; in: a3 cycle count
    ; wave_clock does not clobber d0,a2,a3
    moveq   #3-1,d0
.wcLoop0
    lea     -wave_SIZEOF(a0),a0   
    WAVE_CLOCK_
    dbf     d0,.wcLoop0

    ; ---------------------------------

    * a0 = wave1
    ; synchronize oscillators
    WAVE_SYNC 1
    lea     wave_SIZEOF(a0),a0   
    WAVE_SYNC 2
    lea     wave_SIZEOF(a0),a0   
    WAVE_SYNC 3
    
    * delta_t_osc -= delta_t_min
    sub.l   a3,d7
    bne     .loop

    ; ---------------------------------
    * a0 = wave3

    * Get voice output

    * jmp+jmp = 5+5 = 10 cycles
    * jsr+rts = 5+7 = 12 cycles
    * jmp+bra = 5+3 = 5..8 cycles

    * ---- voice3 out
    lea     .wr1(pc),a3
    move.l  wave_get_output(a0),a1
    jmp     (a1)
.wr1:
    lea     -wave_SIZEOF(a0),a0
    sub.w   voice_wave_zero+resid_voice3(a5),d0
    muls.w  envelope_counterHi+resid_envelope3(a5),d0
    add.l   voice_voice_DC+resid_voice3(a5),d0
    lea     .wr2(pc),a3
    move.l  d0,d5
    * ---- voice2 out
    move.l  wave_get_output(a0),a1
    jmp     (a1)
.wr2:
    lea     -wave_SIZEOF(a0),a0
    sub.w   voice_wave_zero+resid_voice2(a5),d0
    muls.w  envelope_counterHi+resid_envelope2(a5),d0
    add.l   voice_voice_DC+resid_voice2(a5),d0
    lea     .wr3(pc),a3
    move.l  d0,d6
    * ---- voice1 out
    move.l  wave_get_output(a0),a1
    jmp     (a1)
.wr3:
    sub.w   voice_wave_zero+resid_voice1(a5),d0
    muls.w  envelope_counterHi+resid_envelope1(a5),d0
    add.l   voice_voice_DC+resid_voice1(a5),d0
    ; 30 cyc + return jmp 15
    ; = 45

 REM ;loopy


     move.l  sid_voice3(a5),a2
    moveq   #3-1,d5
    bra     .voiceOutLoop_
.voiceOutLoop
    move.l  d0,-(sp)
    lea     -wave_SIZEOF(a0),a0
    lea     -voice_SIZEOF(a2),a2
.voiceOutLoop_
    ; ---------------------------------
    ; VOICE OUT
    move.l  wave_get_output(a0),a1
    jmp     (a1)
wave_output_return:
    * d0 = 12-bit
    sub.w   voice_wave_zero(a2),d0
    * envelope_output inlined:
    move.l  voice_envelope(a2),a1
    muls.w  envelope_counterHi(a1),d0
    * d0 = 20-bit
    add.l   voice_voice_DC(a2),d0
    ; ---------------------------------
    dbf     d5,sid_clock\.voiceOutLoop
  EREM

 
 REM
    ; Clock filter
     ; Get input
    move.l  sid_voice3(a5),a2
    VOICE_OUT 3
    * Assume voice objects are stored one after another
    lea     -wave_SIZEOF(a0),a0
    lea     -voice_SIZEOF(a2),a2
    move.l  d0,d5
    VOICE_OUT 2
    lea     -wave_SIZEOF(a0),a0
    lea     -voice_SIZEOF(a2),a2
    move.l  d0,d6
    VOICE_OUT 1

 EREM


    ; 3 * 12 = 36
    ; + 3*jmp 
    ; = 51

    ; -----
    move.l  d5,d3   * voice3
    move.l  d6,d2   * voice2
    move.l  d0,d1   * voice1

    ;-------------------------------

    move.l  (sp),d0 * restore delta_t
    
    ;move.l  sid_filter(a5),a0  
    lea     resid_filter(a5),a0
    bra     filter_clock ; + filter_output
filter_output_return:
    ; Clock external filter
    move.l  d0,d1       * input for the filter
    ;move.l  sid_extfilt(a5),a0
    lea     resid_extfilter(a5),a0
    pop     d0          * restore delta_t
    bra     extfilter_clock
  


* Clock and get 16-bit samples corresponding on the number of cycles 
* in:
*   a0 = object
*   a1 = output buffer pointer 
*   d0 = cycles to run
*   d1 = buffer size limit in samples
* out:
*   d0 = samples written
* uses:
*   d0-a6
sid_clock_fast16:
    move.l  a0,a5
    * d3 = s
    moveq   #0,d3
    move.l  sid_sample_offset(a5),a4
    move.l  sid_cycles_per_sample(a5),a6

    bsr     sid_get_outputScale
    move.l  d6,-(sp)
.loop
    * d5 = next_sample_offset
    ;move.l  sid_sample_offset(a5),d5
    ;add.l   sid_cycles_per_sample(a5),d5
    move.l  a4,d5
    add.l   a6,d5
    add.l   #1<<(FIXP_SHIFT-1),d5

    * d2 = delta_t_sample
    moveq   #FIXP_SHIFT,d4
    move.l  d5,d2
    asr.l   d4,d2       * >>FIXP_SHIFT
   
       * Loop termination conditions:
    * buffer overflow check
    * if (s >= n) 
    cmp.l   d1,d3
    bge     .x     
    * if (delta_t_sample > delta_t)
    cmp.l   d0,d2
    bgt     .break
 
    * sample_offset = (next_sample_offset & FIXP_MASK) - (1 << (FIXP_SHIFT - 1));
    and.l   #FIXP_MASK,d5
    sub.l   #1<<(FIXP_SHIFT-1),d5
    move.l  d5,a4

    * delta_t -= delta_t_sample
    sub.l   d2,d0
    
    pushm   d0/d1/d3/a1 * 4 regs
    move.l  d2,d0
    bsr     sid_clock
    popm    d0/d1/d3/a1
    
    ; Inline output generation
    ;extfilter_output inlined
    move.l  sid_extfilt(a5),a0
    move.l  (sp),d6
    muls.l  extfilter_Vo(a0),d6
    moveq   #10,d4
    asr.l   d4,d6   * FP 10 shift
    CLAMP16  d6

    * store one sample d3
    move.w  d6,(a1,d3.l*2)    * buffer write
    addq.l  #1,d3
    bra     .loop
    
.break
    * run remaining d0 cycles
    pushm   d0/d3
    bsr     sid_clock
    popm    d0/d3

    swap    d0
    clr.w   d0      * delta_t<<FIXP_SHIFT
    sub.l   d0,a4
.x
    move.l  a4,sid_sample_offset(a5)
    addq.l  #4,sp
    * samples written
    move.l  d3,d0
    rts


* Clock and get 8-bit samples corresponding to the number of 
* given cycles, with volume scaling.
* in:
*   a0 = object
*   a1 = output buffer pointer 
*   d0 = cycles to run
*   d1 = buffer size limit in samples
* out:
*   d0 = samples written
* uses:
*   d0-a6
sid_clock_fast8:
    move.l  a0,a5
    * d3 = s
    moveq   #0,d3
    move.l  sid_sample_offset(a5),a4
    move.l  sid_cycles_per_sample(a5),a6

    bsr     sid_get_outputScale
    move.l  d6,-(sp)
.loop
    * cycle_count next_sample_offset = sample_offset 
    *                + cycles_per_sample
    *                + (1 << (FIXP_SHIFT - 1));
    move.l  a4,d5
    add.l   a6,d5
    add.l   #1<<(FIXP_SHIFT-1),d5
    * d5 = next_sample_offset

    * d2 = delta_t_sample
    moveq   #FIXP_SHIFT,d4
    move.l  d5,d2
    asr.l   d4,d2       * >>FIXP_SHIFT
   
    * Loop termination conditions:
    * buffer overflow check
    * if (s >= n) 
    cmp.l   d1,d3
    bge     .x     
    * if (delta_t_sample > delta_t)
    cmp.l   d0,d2
    bgt     .break
 
    * sample_offset = (next_sample_offset & FIXP_MASK) - (1 << (FIXP_SHIFT - 1));
    and.l   #FIXP_MASK,d5
    sub.l   #1<<(FIXP_SHIFT-1),d5
    move.l  d5,a4

    * delta_t -= delta_t_sample
    sub.l   d2,d0
    
    pushm   d0/d1/d3/a1 * 4 regs
    move.l  d2,d0
    bsr     sid_clock
    popm    d0/d1/d3/a1

    ; Inline output generation
    ;extfilter_output inlined
    move.l  sid_extfilt(a5),a0
    move.l  (sp),d6
    muls.l  extfilter_Vo(a0),d6
    moveq   #10+8,d4    * FP 10, 16->8    
    asr.l   d4,d6   * FP shift + 16->8 bit shift
    CLAMP8  d6

    * store one byte at d3
    move.b  d6,(a1,d3.l)    * chip write
    addq.l  #1,d3
    bra     .loop
    
.break
    * run remaining d0 cycles
    pushm   d0/d3
    bsr     sid_clock
    popm    d0/d3

    swap    d0
    clr.w   d0      * delta_t<<FIXP_SHIFT
    sub.l   d0,a4
.x
    move.l  a4,sid_sample_offset(a5)
  
    addq.l  #4,sp
    * samples written
    move.l  d3,d0
    rts


* Clock and get 14-bit samples corresponding to the number of 
* given cycles, with volume scaling.
* in:
*   a0 = object
*   a1 = output byte buffer pointer: high byte
*   a2 = output byte buffer pointer: low byte
*   d0 = cycles to run
*   d1 = buffer size limit in samples
* out:
*   d0 = samples written
* uses:
*   d0-a6
    cnop 0,4
sid_clock_fast14:
    move.l  a0,a5
    * a6 = s
    sub.l   a6,a6
    move.l  sid_sample_offset(a5),a4

    bsr     sid_get_outputScale
    move.l  d6,-(sp)
    bra     .go
.loop
 REM
    * d5 = next_sample_offset
    move.l  a4,d5
    add.l   sid_cycles_per_sample(a5),d5
    add.l   #1<<(FIXP_SHIFT-1),d5

    * d2 = delta_t_sample
    moveq   #FIXP_SHIFT,d4
    move.l  d5,d2
    asr.l   d4,d2       * >>FIXP_SHIFT
   
    * Loop termination conditions:
    * buffer overflow check
    * if (s >= n) 
;;;    cmp.l   d1,a6
;;;    bge     .x     
    * if (delta_t_sample > delta_t)
    cmp.l   d0,d2
    bgt     .break
 EREM
    and.l   #FIXP_MASK,d5
    sub.l   #1<<(FIXP_SHIFT-1),d5
    move.l  d5,a4
    sub.l   d2,d0

    pushm   d0/a1/a2 * 3 regs
    move.l  d2,d0
    bsr     sid_clock 
    * d1 = extfilter_Vo
    popm    d0/a1/a2

    ; Inline output generation
    ;move.l  sid_extfilt(a5),a0
    ;move.l  (sp),d6
    ;muls.l  extfilter_Vo(a0),d6
    muls.l  (sp),d1
    moveq   #10,d4  * FP 10
    asr.l   d4,d1   * FP shift
    CLAMP16 d1
    
    * store low 6 bits
    lsr.b   #2,d1
    move.b  d1,(a2,a6.l)     * chip write
    ror.w   #8,d1
    * store high 8 bits
    addq.l  #1,a6
    move.b  d1,-1(a1,a6.l)   * chip write, stall
.go
;    bra     .loop
    ; ---------------------------------
    move.l  a4,d5
    add.l   sid_cycles_per_sample(a5),d5
    add.l   #1<<(FIXP_SHIFT-1),d5

    * d2 = delta_t_sample
    moveq   #FIXP_SHIFT,d4
    move.l  d5,d2
    asr.l   d4,d2       * >>FIXP_SHIFT

    cmp.l   d0,d2
;    bgt     .break
    ble     .loop
    
.break
    * run remaining d0 cycles
    push    d0
    bsr     sid_clock
    pop     d0

    * delta_t<<FIXP_SHIFT
    moveq   #FIXP_SHIFT,d1
    lsl.l   d1,d0
    sub.l   d0,a4
.x
    move.l  a4,sid_sample_offset(a5)
  
    addq.l  #4,sp
    * samples written
    move.l  a6,d0
    rts

* A variant more suitable for the 030
 REM
sid_clock_fast14_030:
    move.l  a0,a5
    * d3 = s
    sub.l   a6,a6
    move.l  sid_sample_offset(a5),a4
.loop
    * d5 = next_sample_offset
    move.l  a4,d5
    add.l   sid_cycles_per_sample(a5),d5
    add.l   #1<<(FIXP_SHIFT-1),d5

    * d2 = delta_t_sample
    ;moveq   #FIXP_SHIFT,d4
    ;move.l  d5,d2
    ;asr.l   d4,d2       * >>FIXP_SHIFT
    
    swap    d5
    move    d5,d2

    * Loop termination conditions:
    * buffer overflow check
    * if (s >= n) 
    ;cmp.l   d1,a6
    ;bge     .x     
    
    * if (delta_t_sample > delta_t)
    cmp.w   d0,d5
    bgt     .break

    ;and.l   #FIXP_MASK,d5
    sub.w   d5,d0
    clr.w   d5
    swap    d5
    sub.l   #1<<(FIXP_SHIFT-1),d5
    move.l  d5,a4

    pushm   d0/d1/a1/a2 * 4 regs
    move.w  d2,d0
    * todo: make sid_clock not use .l for cycles
    bsr     sid_clock
    popm    d0/d1/a1/a2

;    * Mul by 96 and >> 10
    move.l  extfilter_Vo(a0),d4
    move.l  d4,d6
    add.l   d4,d6
    add.l   d4,d6
    asr.l   #5,d6   

    * store low 6 bits
    lsr.b   #2,d6
    move.b  d6,(a2,a6.l)     * chip write
    ror.w   #8,d6
    * store high 8 bits
    addq.l  #1,a6
    move.b  d6,-1(a1,a6.l)   * chip write, stall

    bra     .loop
    
.break
    * run remaining d0 cycles
    push    d0
    bsr     sid_clock
    pop     d0
    
    swap    d0
    clr.w   d0      * delta_t<<FIXP_SHIFT
    sub.l   d0,a4
.x
    move.l  a4,sid_sample_offset(a5)
  
    * samples written
    move.l  a6,d0
    rts
 EREM



* Clock by oversampling and averaging the samples.
* Seems to be effective in reducing the sampling noise.
* in:
*   a0 = object
*   a1 = output byte buffer pointer: high byte
*   a2 = output byte buffer pointer: low byte
*   d0 = cycles to run
*   d1 = buffer size limit in samples
* out:
*   d0 = samples written
* uses:
*   d0-a6
sid_clock_oversample14:
    
    move.l  a0,a5
    * d3 = s
    moveq   #0,d3

    bsr     sid_get_outputScale
    move.l  d6,-(sp)

    move.l  sid_sample_offset(a5),a4

    * Multiply cycles needed
    mulu.l  sid_oversample(a5),d0
.loop
    * inner loop count
    move.l  sid_oversample(a5),a6
    * sample data accumulator
    moveq   #0,d7
    pushm   d3/a1/a2
.innerLoop
    ; ---------------------------------
    * d5 = next_sample_offset
    move.l  a4,d5
    add.l   sid_cycles_per_sample(a5),d5
    add.l   #1<<(FIXP_SHIFT-1),d5

    * d2 = delta_t_sample
    moveq   #FIXP_SHIFT,d4
    move.l  d5,d2
    asr.l   d4,d2       * >>FIXP_SHIFT
   
;    * Loop termination conditions:
    * if (delta_t_sample > delta_t)
    cmp.l   d0,d2
    bgt     .popAndBreak

    and.l   #FIXP_MASK,d5
    sub.l   #1<<(FIXP_SHIFT-1),d5
    move.l  d5,a4
    sub.l   d2,d0

    pushm   d0/d7
    move.l  d2,d0
    bsr     sid_clock
    * d1 = extfilter_Vo
    popm    d0/d7
    
    ; Inline output generation
    ;;move.l  sid_extfilt(a5),a0
    ;;add.l   extfilter_Vo(a0),d7
    add.l   d1,d7   * accumulate samples
    ; ---------------------------------
    subq.w  #1,a6
    tst.w   a6
    bgt     .innerLoop
    popm    d3/a1/a2 
    ; ---------------------------------
    ; buffer overflow check
    ;;cmp.l   d1,d3
    ;;bge     .x     
    ; ---------------------------------
    muls.l  (sp),d7
    moveq   #10,d4  * FP 10
    asr.l   d4,d7   * FP shift
    CLAMP16 d7
    
    * store low 6 bits
    lsr.b   #2,d7
    move.b  d7,(a2,d3.l)     * chip write
    ror.w   #8,d7
    * store high 8 bits
    addq.l  #1,d3
    move.b  d7,-1(a1,d3.l)   * chip write, stall
    ; ---------------------------------
    bra     .loop

.popAndBreak
    popm    d3/a1/a2 

.break
    ; ---------------------------------
    * run remaining d0 cycles
    pushm   d0/d3
    bsr     sid_clock
    popm    d0/d3

    * delta_t<<FIXP_SHIFT
    moveq   #FIXP_SHIFT,d1
    lsl.l   d1,d0
    sub.l   d0,a4
.x
    move.l  a4,sid_sample_offset(a5)
    addq.l  #4,sp
    * samples written
    move.l  d3,d0
    rts


* Clock by oversampling and averaging the samples.
* IDENTITAL TO ABOVE, just not using two separate output buffers.
* Seems to be effective in reducing the sampling noise.
* in:
*   a0 = object
*   a1 = output buffer pointer
*   d0 = cycles to run
*   d1 = buffer size limit in samples
* out:
*   d0 = samples written
* uses:
*   d0-a6
sid_clock_oversample16:
    
    move.l  a0,a5
    * d3 = s
    moveq   #0,d3

    bsr     sid_get_outputScale
    move.l  d6,-(sp)

    move.l  sid_sample_offset(a5),a4

    * Multiply cycles needed
    mulu.l  sid_oversample(a5),d0
.loop
    * inner loop count
    move.l  sid_oversample(a5),a6
    * sample data accumulator
    moveq   #0,d7
    pushm   d1/d3/a1
.innerLoop
    ; ---------------------------------
    * d5 = next_sample_offset
    move.l  a4,d5
    add.l   sid_cycles_per_sample(a5),d5
    add.l   #1<<(FIXP_SHIFT-1),d5

    * d2 = delta_t_sample
    moveq   #FIXP_SHIFT,d4
    move.l  d5,d2
    asr.l   d4,d2       * >>FIXP_SHIFT
   
;    * Loop termination conditions:
    * if (delta_t_sample > delta_t)
    cmp.l   d0,d2
    bgt     .popAndBreak

    and.l   #FIXP_MASK,d5
    sub.l   #1<<(FIXP_SHIFT-1),d5
    move.l  d5,a4
    sub.l   d2,d0

    pushm   d0/d7
    move.l  d2,d0
    bsr     sid_clock
    popm    d0/d7
    
    ; Inline output generation
    move.l  sid_extfilt(a5),a0
    add.l   extfilter_Vo(a0),d7
    ; ---------------------------------
    subq.w  #1,a6
    tst.w   a6
    bgt     .innerLoop
    popm    d1/d3/a1
    ; ---------------------------------
    ; buffer overflow check
    cmp.l   d1,d3
    bge     .x     
    ; ---------------------------------
    muls.l  (sp),d7
    moveq   #10,d4  * FP 10
    asr.l   d4,d7   * FP shift
    CLAMP16 d7
    
    * store one sample d7
    move.w  d7,(a1,d3.l*2)    * buffer write
    addq.l  #1,d3
    ; ---------------------------------
    bra     .loop

.popAndBreak
    popm    d1/d3/a1

.break
    ; ---------------------------------
    * run remaining d0 cycles
    pushm   d0/d3
    bsr     sid_clock
    popm    d0/d3

    swap    d0
    clr.w   d0      * delta_t<<FIXP_SHIFT
    sub.l   d0,a4
.x
    move.l  a4,sid_sample_offset(a5)
    addq.l  #4,sp
    * samples written
    move.l  d3,d0
    rts





* Clock by interpolating between samples.
* EXPERIMENTAL, this is different than the reSID interpolate mode,
* which runs one clock cycle at a time.
* in:
*   a0 = object
*   a1 = output byte buffer pointer: high byte
*   a2 = output byte buffer pointer: low byte
*   d0 = cycle_count delta_t, max cycles
*   d1 = buffer limit
* out:
*   d0 = bytes got
* uses:
*   d0-a6
sid_clock_interpolate14:
    move.l  a0,a5
    * d3 = s
    moveq   #0,d3

    bsr     sid_get_outputScale
    move.l  d6,-(sp)

    move.l  sid_sample_offset(a5),a4
.loop
    * d5 = next_sample_offset
    move.l  a4,d5
    add.l   sid_cycles_per_sample(a5),d5
    
    * d2 = delta_t_sample
    moveq   #FIXP_SHIFT,d6
    move.l  d5,d2
    asr.l   d6,d2       * >>FIXP_SHIFT

    * Loop termination conditions:
    * buffer overflow check
    * if (s >= n) 
    cmp.l   d1,d3
    bge     .x     
    * if (delta_t_sample > delta_t)
    cmp.l   d0,d2
    bgt     .break

    * sample_offset = next_sample_offset & FIXP_MASK
    and.l   #FIXP_MASK,d5
    ;move.l  d5,sid_sample_offset(a5)
    move.l  d5,a4

    * delta_t -= delta_t_sample  
    sub.l   d2,d0

    * Divide required amount cycles into two portions,
    * run first part and grab sample,
    * run second part and grab sample
    move.l  d2,d6
    ; Leave 8 cycles for the 2nd part, it will align with the
    ; filter loops which loop in 8 cycle steps, a small speed up.
    subq.l  #8,d6
    ;lsr.l   #1,d6
    move.l  d2,a6
    sub.l   d6,a6

    pushm   d0/d1/d3/d5/a1/a2 * 6 regs

    move.l  d6,d0       * d6 cycles, first part
    bsr     sid_clock

    * Grab sample 
    move.l  sid_extfilt(a5),a0
    move.l  6*4(sp),d4  * access the previous top of the stack
    muls.l  extfilter_Vo(a0),d4
    moveq   #10,d6      * FP
    asr.l   d6,d4       * FP shift
    move.l  d4,sid_sample_prev(a5)

    move.l  a6,d0       * a6 cycles, second part
    bsr     sid_clock
   
  
    popm    d0/d1/d3/d5/a1/a2
    

    move.l  sid_extfilt(a5),a0
    move.l  (sp),d4
    muls.l  extfilter_Vo(a0),d4
    moveq   #10,d6      * FP
    asr.l   d6,d4       * FP shift

    * d4 = sample_now

    moveq   #16,d6 * >>FP_SHIFT
    move.l  d4,d7
    sub.l   sid_sample_prev(a5),d7
    muls.l  d5,d7
    asr.l   d6,d7
    add.l   sid_sample_prev(a5),d7
    * sample_prev = sample_now
    move.l  d4,sid_sample_prev(a5)

    CLAMP16 d7
   
    * store low 6 bits
    lsr.b   #2,d7
    move.b  d7,(a2,d3.l)     * chip write
    ror.w   #8,d7
    * store high 8 bits
    addq.l  #1,d3
    move.b  d7,-1(a1,d3.l)   * chip write, stall

    bra     .loop

.break
 REM * Option A, seems a bit superfluous
    * run delta_t-1 cycles if possible
    pushm   d0/d3

    subq.l  #1,d0
    bmi.b   .y

    push    d0
    bsr     sid_clock
    move.l  a5,a0
    ;bsr     sid_output16
    moveq   #10,d6      * FP
    move.l  sid_extfilt(a5),a0
    moveq   #91,d4
    muls.l  extfilter_Vo(a0),d4
    asr.l   d6,d4       * FP shift
    move.l  d4,sid_sample_prev(a5)    

    pop     d0
    beq.b   .y

    * run 1 cycles
    moveq   #1,d0    
    bsr     sid_clock
.y
    popm    d0/d3
 EREM
    * Option B
    * run remaining d0 cycles
    pushm   d0/d3
    bsr     sid_clock
    popm    d0/d3

    swap    d0
    clr.w   d0      * delta_t<<FIXP_SHIFT
    ;sub.l   d0,sid_sample_offset(a5)
    sub.l   d0,a4
.x
    move.l  a4,sid_sample_offset(a5)
    addq.l  #4,sp
    * bytes written
    move.l  d3,d0
    rts

* in:
*   a0 = object
* out:
*   d0 = Voice 3 envelope value
sid_readENV3:
    move.l  sid_voice3(a0),a0
    move.l  voice_envelope(a0),a0
    bra     envelope_readENV

* in:
*   a0 = object
* out:
*   d0 = Wave 3 oscillator value
sid_readOSC3:
    move.l  sid_voice3(a0),a0
    move.l  voice_wave(a0),a0
    bra     wave_readOSC


	section	reSID_data,data

    cnop    0,4
    ; Precalculated filter tables from reSID.
    ; labels: filter_f0_6581, filter_f0_8580
    include "filter_f0_data.s"

    ; Waveform data
wave6581__ST:    incbin "wave6581__ST.dat"
wave6581_P_T:    incbin "wave6581_P_T.dat"
wave6581_PS_:    incbin "wave6581_PS_.dat"
wave6581_PST:    incbin "wave6581_PST.dat"
wave8580__ST:    incbin "wave8580__ST.dat"
wave8580_P_T:    incbin "wave8580_P_T.dat"
wave8580_PS_:    incbin "wave8580_PS_.dat"
wave8580_PST:    incbin "wave8580_PST.dat"

