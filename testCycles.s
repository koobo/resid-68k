;APS00000000000000000000000000000000000000000000000000000000000000000000000000000000

 ifnd __VASM
    incdir include:
 endif
    include exec/exec_lib.i
    include libraries/timer_lib.i
    include devices/timer.i
    include dos/dos_lib.i

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

* Period should be divisible by 64 for bug free 14-bit output
PAULA_PERIOD=128    
PAL_CLOCK=3546895
* Sampling frequency: PAL_CLOCK/PAULA_PERIOD=27710.1171875

* reSID update frequency 100 Hz:
* Samples per 1/100s = 277.10117
* Samples per 1/100s as 22.10 FP = 283751.59808
* SAMPLES_PER_FRAME = 283752

* reSID update frequency 200 Hz:
* Samples per 1/200s = 138.550585
* Samples per 1/200s as 22.10 FP = 141875.79904
SAMPLES_PER_FRAME = 141876

* Output buffer size 
* 100 Hz
*SAMPLE_BUFFER_SIZE = 277+1  * 277.101171875
* 200 Hz
SAMPLE_BUFFER_SIZE = 140     * 138.550585

    * Launch and run a few cycles
sid_main:     
    bsr     openTimer
    lea     Sid,a0
    jsr	    sid_constructor

    clr.l   result1
    clr.l   result2
    clr.l   result3

 rept 4
    moveq   #1,d0   * ext filter on
    moveq   #1,d1   * filter on
    bsr     measure
    add.l   d7,result1
 endr
 rept 4
    moveq   #0,d0   * ext filter off
    moveq   #1,d1   * filter on
    bsr     measure
    add.l   d7,result2
 endr
 rept 4
    moveq   #0,d0   * ext filter off
    moveq   #0,d1   * filter off
    bsr     measure
    add.l   d7,result3
 endr
    move.l  result1,d0
    lsr.l   #2,d0
    move.l  result2,d2
    lsr.l   #2,d2
    move.l  result3,d4
    lsr.l   #2,d4

 ifd __VASM
    bsr     print
 endif
    rts

result1 dc.l 0
result2 dc.l 0
result3 dc.l 0

measure:
    lea     Sid,a0
    push    d1
    jsr     sid_enable_external_filter
    lea     Sid,a0
    pop     d0
    jsr     sid_enable_filter

    lea     Sid,a0
    jsr	    sid_reset
 
    move.l  #985248,d0
    moveq   #SAMPLING_METHOD_OVERSAMPLE2x14,d1
    moveq   #SAMPLING_METHOD_OVERSAMPLE3x14,d1
    moveq   #SAMPLING_METHOD_OVERSAMPLE4x14,d1
    moveq   #SAMPLING_METHOD_INTERPOLATE14,d1
    moveq   #SAMPLING_METHOD_SAMPLE_FAST14,d1
    move.l  #PAULA_PERIOD,d2

    lea     Sid,a0
    jsr     sid_set_sampling_parameters_paula
    move.l  a1,a4


    lea     Sid,a0
    move.l  #SAMPLES_PER_FRAME,d0
    mulu.l  sid_cycles_per_sample(a0),d1:d0
    * Shift by 16 and 10 to get the FP to 
    * the correct position
    divu.l  #1<<(16+10),d1:d0
    move.l  d0,cyclesPerFrame
   
       move.l  4.w,a6
    jsr     _LVOForbid(a6)

    bsr     pokeSound2

    bsr    startMeasure

    * Loop count
    moveq   #1-1,d7
loop
    * output for high byte
    lea     output1,a1
    * output for low byte
    lea     output2,a2
    move.l  cyclesPerFrame,d0
    muls.l  #20,d0  * 20 * 5ms = 100 ms
    move.l  #10000,d1 * buffer limit
    lea     Sid,a0
    movem.l a4/d7,-(sp)
    jsr     (a4)    * call clock function
    movem.l (sp)+,d7/a4
    dbf     d7,loop

    bsr    stopMeasure
    move    $dff006,$dff180
    move.l  d0,d7

    move.l  4.w,a6
    jsr     _LVOPermit(a6)
    rts

cyclesPerFrame dc.l 0

.pokeSound
 ;REM
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
; EREM
    * filter mode and main volume control
    * 0..3: main volume
    * 4: low pass
    * 5: band pass
    * 6: high pass
    * 7: mute voice 3
    move.b  #$f+%001<<4,d0
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

	
    rts


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
    lea     output1,a1
    move.l  #10000,d0  * cycles (upper limit)
 ;   move.l  #1000,d1 * buffer limit
;    move.l  #1000,d1   * get this many bytes
    jsr     sid_clock_fast8
    rts



* Frame $b from "Advanced Chemistry"
pokeSound2:
    move.b  #$00,d0
    move.b  #$15,d1 * fc_lo
    bsr     .w
;   000b 1601 000b 17f1 
    move.b  #$01,d0
    move.b  #$16,d1 * fc_hi
    bsr     .w
    move.b  #$f1,d0 * res=f, filter voice 1
    move.b  #$17,d1 * res_filt
    bsr     .w
;00000420:   186f   0507   06a2   0200  ...o............
    move.b  #$6f,d0 * filter mode=6 (hp+bp), vol=f
    move.b  #$18,d1 * mode_vol
    bsr     .w
    move.b  #$07,d0 * attack=0, decay=7
    move.b  #$05,d1 * v1 attack decay
    bsr     .w
    move.b  #$a2,d0 * sustain=a, release=2
    move.b  #$06,d1 * v1 sustain release
    bsr     .w
    move.b  #$00,d0
    move.b  #$02,d1 * v1 pw lo
    bsr     .w
;00000430:   0387   0043   0103   0441  .......C.......A
    move.b  #$87,d0
    move.b  #$03,d1 * v1 pw hi
    bsr     .w
    move.b  #$43,d0
    move.b  #$00,d1 * v1 freq lo
    bsr     .w
    move.b  #$03,d0
    move.b  #$01,d1 * v1 freq hi
    bsr     .w
    move.b  #$41,d0 * gate, pulse
    move.b  #$04,d1 * v1 control
    bsr     .w
;00000440:   0c00   0d64   0900   0a84  .......d........
    move.b  #$00,d0 * attack=0, decay=0
    move.b  #$0c,d1 * v2 attack decay
    bsr     .w
    move.b  #$64,d0 * sustain=6, release=4
    move.b  #$0d,d1 * v2 sustain release
    bsr     .w
    move.b  #$00,d0
    move.b  #$09,d1 * v2 pw lo
    bsr     .w
    move.b  #$84,d0
    move.b  #$0a,d1 * v2 pw hi
    bsr     .w
;00000450:   0714   0827   0b41   1300  .......'...A....
    move.b  #$14,d0
    move.b  #$07,d1 * v2 freq lo
    bsr     .w
    move.b  #$27,d0
    move.b  #$08,d1 * v2 freq hi
    bsr     .w
    move.b  #$41,d0 * gate, pulse
    move.b  #$0b,d1 * v2 control
    bsr     .w
    move.b  #$00,d0 * attack=0, decay=0
    move.b  #$13,d1 * v3 attack decay
    bsr     .w
;00000460:   14c2   10a0   1180   0e8d  ................
    move.b  #$c2,d0 * sustain=c, release=2
    move.b  #$14,d1 * v3 sustain release
    bsr     .w
    move.b  #$a0,d0
    move.b  #$10,d1 * v3 pw lo
    bsr     .w
    move.b  #$80,d0
    move.b  #$11,d1 * v3 pw hi
    bsr     .w
    move.b  #$8d,d0
    move.b  #$0e,d1 * v3 freq lo
    bsr     .w
;00000470:   0f3a   1241 
    move.b  #$3a,d0
    move.b  #$0f,d1 * v3 freq hi
    bsr     .w
    move.b  #$41,d0 * gate, pulse
    move.b  #$12,d1 * v3 control 
    bsr     .w
    rts    

.w  lea Sid,a0
    bra sid_write

* In:
*   d0 = measurement
print:
    divu.w  #100,d0
    move.l  d0,d1
    ext.l   d0
    swap    d1
    ext.l   d1

    divu.w  #100,d2
    move.l  d2,d3
    ext.l   d2
    swap    d3
    ext.l   d3

    divu.w  #100,d4
    move.l  d4,d5
    ext.l   d4
    swap    d5
    ext.l   d5

    lea     result(pc),a0
    move.l  4.w,a6
    bsr     desmsg

    lea     desbuf,a0
    move.l  a0,a1
.f  tst.b   (a0)+
    bne     .f
    subq    #1,a0
    sub.l   a1,a0
    push    a0

    lea	dosname(pc),a1
	jsr     _LVOOldOpenLibrary(a6)
	move.l	d0,a6

    jsr     _LVOOutput(a6)
    move.l  d0,d1
    move.l  #desbuf,d2
    ;move.l  #128,d3
    pop     d3
    jsr     _LVOWrite(a6)

    move.l  a6,a1
    move.l  4.w,a6
    jsr     _LVOCloseLibrary(a6)
    rts

desmsg:
	movem.l	d0-d7/a0-a3/a6,-(sp)
	lea	desbuf(pc),a3
	move.l	sp,a1	
	lea	putc(pc),a2	
	move.l	4.w,a6
	jsr 	_LVORawDoFmt(a6)
	movem.l	(sp)+,d0-d7/a0-a3/a6
	rts
putc	move.b	d0,(a3)+	
	rts

desbuf  ds.b    256

result      dc.b    "Result: %ld.%02.2ld ms %ld.%02.2ld ms %ld.%02.2ld ms",10,0
dosname		dc.b	"dos.library",0
 even

***************************************************************************
*
* Performance measurement with timer.device
*
***************************************************************************

openTimer
	move.l	4.w,a0
	move	LIB_VERSION(a0),d0
	cmp	#36,d0
	blo.b	.x
	move.l	a0,a6

	lea	.timerDeviceName(pc),a0
	moveq	#UNIT_ECLOCK,d0
	moveq	#0,d1
	lea	timerRequest,a1
	jsr	_LVOOpenDevice(a6)		; d0=0 if success
	tst.l	d0
	seq	timerOpen
.x	rts

.timerDeviceName dc.b	"timer.device",0
	even

closeTimer
	tst.b	timerOpen
	beq.b	.x
	clr.b	timerOpen
	move.l	4.w,a6
	lea	timerRequest,a1
	jsr	_LVOCloseDevice(a6)
.x	rts

startMeasure
	tst.b	timerOpen
	beq.b	.x
	move.l	IO_DEVICE+timerRequest,a6
	lea	clockStart,a0
	jsr     _LVOReadEClock(a6)
.x	rts

; out: d0: difference in millisecs
stopMeasure
	tst.b	timerOpen
	bne.b	.x
	moveq	#-1,d0
	rts
.x
	move.l	IO_DEVICE+timerRequest,a6
	lea	clockEnd,a0
	jsr     _LVOReadEClock(a6)
    * D0 will be 709379 for PAL.
	move.l	d0,d2
	; d2 = ticks/s
	
	; Calculate diff between start and stop times
	; in 64-bits
	move.l	EV_HI+clockEnd,d0
	move.l	EV_LO+clockEnd,d1
	move.l	EV_HI+clockStart,d3
	sub.l	EV_LO+clockStart,d1
	subx.l	d3,d0
    mulu.l  #100000,d0:d1

	; Turn the diff into millisecs
	; Divide d0:d1 by d2
	divu.l  d2,d0:d1
    ; d0:d1 is now d0:d1/d2
	; take the lower 32-bits
	move.l	d1,d0
	rts

timerOpen               dc.w    1
timerRequest	        ds.b    IOTV_SIZE
clockStart              ds.b    EV_SIZE
clockEnd                ds.b    EV_SIZE


    include     "resid-68k.s"

    section bss,bss_p

Sid     ds.b    resid_SIZEOF


    section out,bss_c


output1	ds.b	10000
output2	ds.b	10000
