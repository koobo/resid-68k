
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

;    bra     .pokeOceanLoaderV3

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
    jsr     sid_clock_fast8
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
    move.b  #1<<6+1<<4+1,d0
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
    jsr     sid_clock_fast8
    rts

output	ds.b	1000

    section code,code

    include    "resid-68k.s"