;APS00000015000000150000001500000015000000150000001500000015000000150000001500000015
    incdir  include:
    include exec/exec_lib.i
    include exec/tasks.i
    include dos/dos_lib.i
    include graphics/graphics_lib.i
    include hardware/custom.i
    include hardware/cia.i
    include hardware/dmabits.i
    include hardware/intbits.i
    include libraries/timer_lib.i
    include devices/timer.i
    
* Constants
PAL_CLOCK=3546895
SAMPLING_FREQ=27500
PAULA_PERIOD=(PAL_CLOCK+SAMPLING_FREQ/2)/SAMPLING_FREQ
SAMPLES_PER_FRAME set (SAMPLING_FREQ+25)/50
SAMPLES_PER_HALF_FRAME set (SAMPLING_FREQ+50)/400
; Must be even for Paula 
 ifne SAMPLES_PER_HALF_FRAME&1
SAMPLES_PER_HALF_FRAME set SAMPLES_PER_HALF_FRAME+1
 endif
 
* f=c/p
* fp=c
* p=c/f

main:
    bset    #1,$bfe001

    bsr     openTimer
    move.l  4.w,a6
    lea     DOSName,a1
    jsr     _LVOOldOpenLibrary(a6)
    move.l  d0,DOSBase
    lea     GFXName,a1
    jsr     _LVOOldOpenLibrary(a6)
    move.l  d0,GFXBase

    lea     Sid,a0
    jsr     sid_constructor

    move.l  #985248,d0
    moveq   #SAMPLING_METHOD_SAMPLE_FAST8,d1
    move.l  #SAMPLING_FREQ,d2
    lea     Sid,a0
    jsr     sid_set_sampling_parameters

    moveq   #1,d0
    lea     Sid,a0
    jsr     sid_enable_filter

    moveq   #1,d0
    lea     Sid,a0
    jsr     sid_enable_external_filter


    bsr     createReSIDWorkerTask

;;;;;;; DUMP TEST
; REM
    bsr     playDump  
    bra     .skip
; EREM
 
;;;;;;;; POKE TEST
    moveq   #7*2,d7
    bsr     pokeSound

    move    #1*50,d7
    bsr     wait

    moveq   #7*2,d7
    bsr     pokeRelease
.loop
    bsr     delay
    btst    #6,$bfe001
    bne     .loop
.skip
    bsr     stopReSIDWorkerTask    

    * Stop audio
    move    #$f,dmacon+$dff000
    move    #0,$dff0a8
    move    #0,$dff0b8
    move    #0,$dff0c8
    move    #0,$dff0d8

    bsr     closeTimer

    move.l  maxTime(pc),d0
    move.l  dumpPointer,d1
    sub.l   #regDump,d1
    rts

delay
    movem.l d0-a6,-(sp)
    move.l  GFXBase,a6
    jsr     _LVOWaitTOF(a6)
    movem.l (sp)+,d0-a6
    rts

* in:
*   d7 = 1/50 secs to wait
wait
.l
    bsr     delay
    subq    #1,d7
    bne     .l
    rts


createReSIDWorkerTask:
  
    movem.l d0-a6,-(sp)
    tst.b   workerStatus
    bne     .x

    lea     workerTaskStruct,a0
    move.b  #NT_TASK,LN_TYPE(a0)
    move.b  #-1,LN_PRI(a0)
    move.l  #.workerTaskName,LN_NAME(a0)
    lea     workerTaskStack,a1
    move.l  a1,TC_SPLOWER(a0)
    lea     4096(a1),a1
    move.l  a1,TC_SPUPPER(a0)
    move.l  a1,TC_SPREG(a0)

    move.l  a0,a1
    lea     workerEntry(pc),a2
    sub.l   a3,a3
    move.l  4.w,a6
    jsr     _LVOAddTask(a6)
    addq.b  #1,workerStatus
.x
    movem.l (sp)+,d0-a6
    rts

.workerTaskName
    dc.b    "reSID",0
    even

stopReSIDWorkerTask:
    
    movem.l d0-a6,-(sp)
    tst.b   workerStatus
    beq     .x
    move.b  #-1,workerStatus

    move.l  4.w,a6
    move.l  reSIDTask(pc),a1
    moveq   #0,d0
    move.b  reSIDExitSignal(pc),d1
    bset    d1,d0
    jsr     _LVOSignal(a6)

    lea     _DOSName(pc),a1
    jsr     _LVOOldOpenLibrary(a6)
    move.l  d0,a6
.loop
    tst.b   workerStatus
    beq     .y
    moveq   #1,d1
    jsr     _LVODelay(a6)
    bra     .loop
.y
    move.l  a6,a1
    move.l  4.w,a6
    jsr     _LVOCloseLibrary(a6)
.x 
    movem.l (sp)+,d0-a6
    rts

_DOSName
    dc.b    "dos.library",0
    even

* Playback task
workerEntry
    addq.b  #1,workerStatus

    move.l  4.w,a6
    sub.l   a1,a1
    jsr     _LVOFindTask(a6)
    move.l  d0,reSIDTask
    
    moveq   #-1,d0
    jsr     _LVOAllocSignal(a6)
    move.b  d0,reSIDAudioSignal
    moveq   #-1,d0
    jsr     _LVOAllocSignal(a6)
    move.b  d0,reSIDExitSignal

    lea     reSIDLevel4Intr1,a1
    moveq   #INTB_AUD0,d0		; Allocate Level 4
    jsr     _LVOSetIntVector(a6)
    move.l  d0,.oldVecAud0

    move.w  #INTF_AUD0,intena+$dff000
    move.w  #INTF_AUD0,intreq+$dff000
    move.w  #DMAF_AUD0!DMAF_AUD1!DMAF_AUD2!DMAF_AUD3,dmacon+$dff000

    * CH0 = high 8 bits - full volume
    * CH3 = low 6 bits  - volume 1
    * CH1 = high 8 bits - full volume
    * CH2 = low 6 bits  - volume 1
    move    #PAULA_PERIOD,$a6+$dff000
    move    #PAULA_PERIOD,$b6+$dff000
    move    #PAULA_PERIOD,$c6+$dff000
    move    #PAULA_PERIOD,$d6+$dff000
    ; TODO: hook up vol control
    move    #64,$a8+$dff000
    move    #1,$d8+$dff000
    move    #64,$b8+$dff000
    move    #1,$c8+$dff000


;    lea     buffer1,a2
;    lea     buffer2,a3
    movem.l outBuffer1(pc),d0/d1
    move.l  d0,$a0+$dff000 
    move.l  d1,$d0+$dff000 
    move.l  d0,$b0+$dff000 
    move.l  d1,$c0+$dff000 

    bsr     fillBufferA2
    move    d0,$a4+$dff000   * words
    move    d0,$d4+$dff000   * words
    move    d0,$b4+$dff000   * words
    move    d0,$c4+$dff000   * words

    bsr     dmawait
;    move    #DMAF_SETCLR!DMAF_AUD0!DMAF_AUD1!DMAF_AUD2!DMAF_AUD3,dmacon+$dff000
    move    #DMAF_SETCLR!DMAF_AUD0,dmacon+$dff000
    move.w  #INTF_SETCLR!INTF_AUD0,intena+$dff000

    ; buffer A now plays
    ; interrupt will be triggered soon to queue the next sample
    ; wait for the interrupt and queue buffer B
    ; fill buffer B
    ; after A has played, B will start
    ; interrupt will be triggered
    ; queue buffer A
    ; fill A
    ; ... etc

.loop
    move.l  4.w,a6
    moveq   #0,d0
    move.b  reSIDAudioSignal(pc),d1
    bset    d1,d0
    move.b  reSIDExitSignal(pc),d1
    bset    d1,d0
    jsr     _LVOWait(a6)

    tst.b   workerStatus
    bmi.b   .x

    bsr     reSIDLevel1Handler

    bra     .loop
.x

    move.w  #DMAF_AUD0!DMAF_AUD1!DMAF_AUD2!DMAF_AUD3,dmacon+$dff000
    move.w  #INTF_AUD0,intena+$dff000
    move.w  #INTF_AUD0,intreq+$dff000

    moveq   #INTB_AUD0,d0
    move.l  .oldVecAud0(pc),a1
    move.l  4.w,a6
    jsr     _LVOSetIntVector(a6)
    move.l  d0,.oldVecAud0
    
    move.b   reSIDAudioSignal(pc),d0
    jsr     _LVOFreeSignal(a6)
    move.b   reSIDExitSignal(pc),d0
    jsr     _LVOFreeSignal(a6)

    jsr     _LVOForbid(a6)
    clr.b   workerStatus
    rts

.oldVecAud0     dc.l    0

    ;0  = not running
    ;1  = running
    ;-1 = exiting
workerStatus        dc.b    0
    even

outBuffer1   dc.l    buffer1
outBuffer1b  dc.l    buffer1b
outBuffer2   dc.l    buffer2
outBuffer2b  dc.l    buffer2b

switchBuffers:
    movem.l outBuffer1(pc),d0/d1
    movem.l outBuffer2(pc),d2/d3
    movem.l d0/d1,outBuffer2
    movem.l d2/d3,outBuffer1
    rts

fillBufferA2:
    bsr     startMeasure

    lea     Sid,a0
    ;move.l  a2,a1           * target buffer
    movem.l outBuffer1(pc),a1/a2
    move.l  #100000,d0      * cycle limit, set high enough
    * bytes to get
    move.l  #SAMPLES_PER_HALF_FRAME,d1
    jsr     sid_clock_fast8
    * d0 = bytes received, make words
    lsr     #1,d0
    move.l  d0,d7
    bsr     stopMeasure
    cmp.l   maxTime(pc),d0
    blo.b   .1
    move.l  d0,maxTime
.1
    move.l  d7,d0
    rts

dmawait
	movem.l d0/d1,-(sp)
	moveq	#12-1,d1
.d	move.b	$dff006,d0
.k	cmp.b	$dff006,d0
	beq.b	.k
	dbf	d1,.d
	movem.l (sp)+,d0/d1
	rts

reSIDLevel4Intr1	
        dc.l	0		; Audio Interrupt
		dc.l	0
		dc.b	2
		dc.b	0
		dc.l	reSIDLevel4Name1
reSIDLevel4Intr1Data:
reSIDTask:	
    dc.l	0		            ;is_Data
	dc.l	reSIDLevel4Handler1	;is_Code

reSIDLevel4Name1
    dc.b    "reSID Audio",0
    even

reSIDLevel1Intr
      	dc.l	0		; Player (Software)
		dc.l	0
		dc.b	2
		dc.b	0
		dc.l    reSIDLevel4Name1
PlayIntrPSB	dc.l	0
		dc.l	reSIDLevel1Handler



* a0 = custom base
* a1 = is_data = task
* a6 = execbase
reSIDLevel4Handler1
    move.w  #INTF_AUD0,intreq(a0)
    move.b  reSIDAudioSignal(pc),d1
    moveq   #0,d0
    bset    d1,d0
    jmp     _LVOSignal(a6)
    ;lea     reSIDLevel1Intr(pc),a1
    ;jmp     _LVOCause(a6)

reSIDLevel1Handler
    movem.l d2-d7/a2-a4,-(sp)
    move    .bob,$dff180
    not	    .bob

    * Switch buffers and fill
    bsr     switchBuffers
    movem.l outBuffer1(pc),d0/d1
    move.l  d0,$a0+$dff000 
    move.l  d1,$d0+$dff000 
    move.l  d0,$b0+$dff000 
    move.l  d1,$c0+$dff000 

    bsr     fillBufferA2
    move    d0,$a4+$dff000   * words
    move    d0,$d4+$dff000   * words
    move    d0,$b4+$dff000   * words
    move    d0,$c4+$dff000   * words

    movem.l (sp)+,d2-d7/a2-a4
    moveq   #0,d0
    rts


.bob dc.w   $f0f

    
reSIDAudioSignal    dc.b    0
reSIDExitSignal    dc.b    0
maxTime             dc.l  0
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
	tst.b	timerOpen(pc)
	beq.b	.x
	move.l	IO_DEVICE+timerRequest(pc),a6
	lea	clockStart(pc),a0
	jsr     _LVOReadEClock(a6)
.x	rts

; out: d0: difference in millisecs
stopMeasure
	tst.b	timerOpen(pc)
	bne.b	.x
	moveq	#-1,d0
	rts
.x
	move.l	IO_DEVICE+timerRequest(pc),a6
	lea	clockEnd,a0
	jsr     _LVOReadEClock(a6)
    * D0 will be 709379 for PAL.
	move.l	d0,d2
	; d2 = ticks/s
	divu	#1000,d2
	; d2 = ticks/ms
	ext.l	d2
	
	; Calculate diff between start and stop times
	; in 64-bits
	move.l	EV_HI+clockEnd(pc),d0
	move.l	EV_LO+clockEnd(pc),d1
	move.l	EV_HI+clockStart(pc),d3
	sub.l	EV_LO+clockStart(pc),d1
	subx.l	d3,d0

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



* in:
*    d7 = offset: 0 = voice 1
*                 7 = voice 2
*                14 = voice 3
pokeSound
    * filter mode and main volume control
    * 0..3: main volume
    * 4: low pass
    * 5: band pass
    * 6: high pass
    * 7: mute voice 3
    move.b  #$1f,d0
    move.b  #24,d1
    lea     Sid,a0
    jsr     sid_write

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
 EREM

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
    add.b   d7,d1
    lea     Sid,a0
    jsr     sid_write

    * voice 1: freq hi
    move.b  #.F>>8,d0
    move.b  #1,d1
    add.b   d7,d1
    lea     Sid,a0
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
    add.b   d7,d1
    lea     Sid,a0
    jsr     sid_write

    * voice 1: sustain=15, release=10
    move.b  #12*16+10,d0
    move.b  #6,d1
    add.b   d7,d1
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

pokeRelease
    * voice 1 control
    * Set triangle waveform, clear gate bit
    move.b  #16+0,d0
    move.b  #4,d1
    lea     Sid,a0
    add.b   d7,d1
    jsr     sid_write
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
    jmp	sid_write

katakisVoice2Start:
    dc.w    $0000
    dc.b    $18,$1F * filter low pass, main volume = f
    dc.w    $0000
    dc.b    $0B,$08 * 2 ctrl 
    ; -------------
    dc.w    $0001
    dc.b    $0C,$06 * 2 ad
    dc.w    $0001
    dc.b    $0D,$08 * 2 sr
    dc.w    $0001
    dc.b    $09,$20 * 2 pl
    dc.w    $0001
    dc.b    $0A,$08 * 2 ph
    dc.w    $0001
    dc.b    $0B,$41 * 2 ctrl
    dc.w    $0001
    dc.b    $07,$A2 * 2 fl
    dc.w    $0001
    dc.b    $08,$0E * 2 fh
    ; -------------
    dc.w    $0002
    dc.b    $0C,$06 * 2 ad
    dc.w    $0002
    dc.b    $0D,$08 * 2 sr
    dc.w    $0002
    dc.b    $09,$20 * 2 pl
    dc.w    $0002
    dc.b    $0A,$08 * 2 ph
    dc.w    $0002
    dc.b    $0B,$41 * 2 ctrl
    dc.w    $0002
    dc.b    $07,$45 * 2 fl
    dc.w    $0002
    dc.b    $08,$1D * 2 fh
    ; -------------
    dc.w    $0003
    dc.b    $0C,$06 * 2 ad
    dc.w    $0003
    dc.b    $0D,$08 * 2 sr
    dc.w    $0003
    dc.b    $09,$20 * 2 pl
    dc.w    $0003
    dc.b    $0A,$08 * 2 ph
    dc.w    $0003
    dc.b    $0B,$41 * 2 ctrl
    dc.w    $0003
    dc.b    $07,$a2 * 2 fl
    dc.w    $0003
    dc.b    $08,$0e * 2 fh
    ; -------------
    dc.w    $0004
    dc.b    $0C,$06 * 2 ad
    dc.w    $0004
    dc.b    $0D,$08 * 2 sr
    dc.w    $0004
    dc.b    $09,$20 * 2 pl
    dc.w    $0004
    dc.b    $0A,$08 * 2 ph
    dc.w    $0004
    dc.b    $0B,$81 * 2 ctrl
    dc.w    $0004
    dc.b    $07,$e1 * 2 fl
    dc.w    $0004
    dc.b    $08,$24 * 2 fh
    ; -------------
    dc.w    $0005
    dc.b    $0C,$06 * 2 ad
    dc.w    $0005
    dc.b    $0D,$08 * 2 sr
    dc.w    $0005
    dc.b    $09,$20 * 2 pl
    dc.w    $0005
    dc.b    $0A,$08 * 2 ph
    dc.w    $0005
    dc.b    $0B,$81 * 2 ctrl
    dc.w    $0005
    dc.b    $07,$e1 * 2 fl
    dc.w    $0005
    dc.b    $08,$24 * 2 fh
    ; -------------
    dc.w    $0006
    dc.b    $0C,$06 * 2 ad
    dc.w    $0006
    dc.b    $0D,$08 * 2 sr
    dc.w    $0006
    dc.b    $09,$20 * 2 pl
    dc.w    $0006
    dc.b    $0A,$08 * 2 ph
    dc.w    $0006
    dc.b    $0B,$81 * 2 ctrl
    dc.w    $0006
    dc.b    $07,$e1 * 2 fl
    dc.w    $0006
    dc.b    $08,$24 * 2 fh
    ; -------------
    dc.w    $0007
    dc.b    $0C,$06 * 2 ad
    dc.w    $0007
    dc.b    $0D,$08 * 2 sr
    dc.w    $0007
    dc.b    $09,$20 * 2 pl
    dc.w    $0007
    dc.b    $0A,$08 * 2 ph
    dc.w    $0007
    dc.b    $0B,$81 * 2 ctrl
    dc.w    $0007
    dc.b    $07,$e1 * 2 fl
    dc.w    $0007
    dc.b    $08,$24 * 2 fh
    ; -------------
    dc.w    $0008
    dc.b    $0C,$06 * 2 ad
    dc.w    $0008
    dc.b    $0D,$08 * 2 sr
    dc.w    $0008
    dc.b    $09,$20 * 2 pl
    dc.w    $0008
    dc.b    $0A,$08 * 2 ph
    dc.w    $0008
    dc.b    $0B,$81 * 2 ctrl
    dc.w    $0008
    dc.b    $07,$e1 * 2 fl
    dc.w    $0008
    dc.b    $08,$24 * 2 fh
    ; -------------

katakisVoice2End:
    

katakisVoice3Start:
    dc.w    $0000
    dc.b    $18,$1F * filter low pass, main volume = f
    dc.w    $0000
    dc.b    $0B,$08 * 2 ctrl 
    ; -------------
    dc.w    $0001
    dc.b    $0C,$08 * 2 ad
    dc.w    $0001
    dc.b    $0D,$08 * 2 sr
    dc.w    $0001
    dc.b    $09,$40 * 2 pl
    dc.w    $0001
    dc.b    $0A,$08 * 2 ph
    dc.w    $0001
    dc.b    $0B,$41 * 2 ctrl
    dc.w    $0001
    dc.b    $07,$A2 * 2 fl
    dc.w    $0001
    dc.b    $08,$0E * 2 fh
    ; -------------
    dc.w    $0002
    dc.b    $0C,$08 * 2 ad
    dc.w    $0002
    dc.b    $0D,$08 * 2 sr
    dc.w    $0002
    dc.b    $09,$80 * 2 pl
    dc.w    $0002
    dc.b    $0A,$08 * 2 ph
    dc.w    $0002
    dc.b    $0B,$41 * 2 ctrl
    dc.w    $0002
    dc.b    $07,$45 * 2 fl
    dc.w    $0002
    dc.b    $08,$1D * 2 fh
    ; -------------
    dc.w    $0003
    dc.b    $0C,$08 * 2 ad
    dc.w    $0003
    dc.b    $0D,$08 * 2 sr
    dc.w    $0003
    dc.b    $09,$c0 * 2 pl
    dc.w    $0003
    dc.b    $0A,$08 * 2 ph
    dc.w    $0003
    dc.b    $0B,$41 * 2 ctrl
    dc.w    $0003
    dc.b    $07,$a2 * 2 fl
    dc.w    $0003
    dc.b    $08,$0e * 2 fh
    ; -------------
    dc.w    $0004
    dc.b    $0C,$08 * 2 ad
    dc.w    $0004
    dc.b    $0D,$08 * 2 sr
    dc.w    $0004
    dc.b    $09,$20 * 2 pl
    dc.w    $0004
    dc.b    $0A,$08 * 2 ph
    dc.w    $0004
    dc.b    $0B,$81 * 2 ctrl
    dc.w    $0004
    dc.b    $07,$e1 * 2 fl
    dc.w    $0004
    dc.b    $08,$24 * 2 fh
    ; -------------
    dc.w    $0005
    dc.b    $0C,$08 * 2 ad
    dc.w    $0005
    dc.b    $0D,$08 * 2 sr
    dc.w    $0005
    dc.b    $09,$20 * 2 pl
    dc.w    $0005
    dc.b    $0A,$08 * 2 ph
    dc.w    $0005
    dc.b    $0B,$81 * 2 ctrl
    dc.w    $0005
    dc.b    $07,$e1 * 2 fl
    dc.w    $0005
    dc.b    $08,$24 * 2 fh
    ; -------------
    dc.w    $0006
    dc.b    $0B,$81 * 2 ctrl
    ; -------------
    dc.w    $0007
    dc.b    $0B,$00 * 2 ctrl
    ; -------------
katakisVoice3End:

    



; one hihat hit, one snare hit, voice 3
katakisStart:
	DC.w	$0733
    dc.b    $00,$13 * attack, decay = 0
    dc.w    $0733
    dc.b    $A0,$14 * sustain level = a
	DC.w	$0733
    dc.b    $00,$10 * pulse lo = 0 
    dc.w    $0733
    dc.b    $00,$11 * pulse hi = 0
	DC.w	$0733
    dc.b    $00,$12 * control = 0
    dc.w    $0733
    dc.b    $A2,$0E * freq lo = a2
	DC.w	$0733
    dc.b    $0E,$0F * freq hi = e
    dc.w    $0733
    dc.b    $00,$15 * filter cut off lo = 0
	DC.w	$0733
    dc.b    $70,$16 * filter cut off hi = 70
    dc.w    $0734
    dc.b    $00,$13 * attack, decay = 0
	DC.w	$0734
    dc.b    $A0,$14 * sustain level = a
    dc.w    $0734
    dc.b    $00,$10 * pulse lo = 0 
	DC.w	$0734
    dc.b    $00,$11 * pulse hi = 0
    dc.w    $0734
    dc.b    $81,$12 * control = noise, gate 
	DC.w	$0734
    dc.b    $14,$0E * freq lo = e
    dc.w    $0734
    dc.b    $75,$0F * freq hi = f
	DC.w	$0734
    dc.b    $00,$15 * filter cut off lo = 0
    dc.w    $0734
    dc.b    $5A,$16 * filter cut off hi = $5a
	DC.w	$0734
    dc.b    $F1,$17 * filter resonance = f, voice 1
    dc.w    $0734
    dc.b    $1F,$18 * filter low pass, main volume = f
	DC.w	$0735
    dc.b    $00,$13 * attack, decay = 0
    dc.w    $0735
    dc.b    $A0,$14 * sustain level = a
	DC.w	$0735
    dc.b    $00,$10 * pulse lo = 0 
    dc.w    $0735
    dc.b    $00,$11 * pulse hi = 0
	DC.w	$0735
    dc.b    $00,$12 * control = 0
    dc.w    $0735
    dc.b    $45,$0E * freq lo = e
	DC.w	$0735
    dc.b    $1D,$0F * freq hi = f
    dc.w    $0735
    dc.b    $00,$15 * filter cut off lo = 0
	DC.w	$0735
    dc.b    $44,$16 * filter cut off hi = 44
    dc.w    $0736
    dc.b    $00,$13 * attack, decay = 0
	DC.w	$0736
    dc.b    $A0,$14 * sustain level = a
    dc.w    $0736
    dc.b    $00,$10 * pulse lo = 0 
	DC.w	$0736
    dc.b    $00,$11 * pulse hi = 0
    dc.w    $0736
    dc.b    $00,$12 * control = 0
	DC.w	$0736
    dc.b    $ED,$0E * freq lo = ed
    dc.w    $0736
    dc.b    $00,$15 * filter cut off lo = 0
	DC.w	$0736
    dc.b    $2E,$16 * filter cut off hi = 2e
    DC.w	$0737
    dc.b    $08,$12 * control = test bit
    dc.w    $0737
    dc.b    $F1,$17 * filter resonance = f, voice 1
	DC.w	$0737
    dc.b    $1F,$18 * filter low pass, main volume = f
    dc.w    $0738
    dc.b    $08,$13 * attack = 0, decay = 8
	DC.w	$0738
    dc.b    $08,$14 * sustain = 0, release = 8
    dc.w    $0738
    dc.b    $40,$10 * pulse lo = 40
	DC.w	$0738
    dc.b    $08,$11 * pulse hi = 8
    dc.w    $0738
    dc.b    $41,$12 * control = triangle, gate
	DC.w	$0738
    dc.b    $A2,$0E * freq lo = a2
    dc.w    $0738
    dc.b    $0E,$0F * freq hi = e
	DC.w	$0738
    dc.b    $00,$15 * filter cutoff lo = 0
    dc.w    $0738
    dc.b    $86,$16 * filter cutoff hi = 86
	DC.w	$0739
    dc.b    $08,$13 * attack = 0, decay = 3
    dc.w    $0739
    dc.b    $08,$14 * sustain = 0, release = 8
	DC.w	$0739
    dc.b    $80,$10 * pulse low = 80
    dc.w    $0739
    dc.b    $08,$11 * pulse high = 11
	DC.w	$0739
    dc.b    $41,$12 * control = triangle, gate
    dc.w    $0739
    dc.b    $45,$0E * freq lo = 45
	DC.w	$0739
    dc.b    $1D,$0F * freq hi = 1d
    dc.w    $0739
    dc.b    $00,$15 * filter cutoff lo = 0
	DC.w	$0739
    dc.b    $70,$16 * filter cutoff hi = 16
    dc.w    $073A
    dc.b    $08,$13 * attack = 0, decay = 3
katakisEnd:


playDump
    * volume
    move.b  #$f,d0
    move.b  #24,d1
    lea     Sid,a0
    jsr     sid_write
 



   lea     regDump,a5
    lea     regDumpEnd,a6
;
;    lea		katakisStart,a5
;    lea		katakisEnd,a6
;
;    lea     katakisVoice2Start,a5
;    lea     katakisVoice2End,a6
;
;
;    lea     katakisVoice3Start,a5
;    lea     katakisVoice3End,a6
;   

    * first snare hit on v2
    * OK on v2, v3
    lea     katakisV2regDump,a5
    lea     katakisV2regDumpEnd,a6

;    lea     katakisV3regDump,a5
;    lea     katakisV3regDumpEnd,a6

    * vblank timer, set start time
    ;moveq   #0,d7
    move     (a5),d7 
.loop
    move    #$f00,$dff180
    bsr     delay
    btst    #6,$bfe001
    beq     .x
   
.a
    cmp     (a5),d7
    bne     .s 
    move.b  2(a5),d1 * addr
    move.b  3(a5),d0 * data
    
;    sub.b   #7,d1       * move from voice 3 to 2

   ; exg	    d0,d1    * swapped with katakis dump!
;

    lea     Sid,a0
    jsr     sid_write
.1
    addq    #4,a5
    cmp.l   a6,a5
    bhs     .x
    bra     .a

.s
    addq.l  #1,d7
    cmp.l   a6,a5
    blo     .loop
.x
    move.l  a5,dumpPointer
    rts

dumpPointer	dc.l	0

DOSName     dc.b    "dos.library",0
GFXName     dc.b    "graphics.library",0
    even


;katakis v2 snare:
;-1:    ct=08                                      
;0:     ad=06,sr=08,pl=20,ph=08,ct=41,fl=A2,fh=0E         
;1:     ad=06,sr=08,pl=20,ph=08,ct=41,fl=45,fh=1D    
;2:     ad=06,sr=08,pl=20,ph=08,ct=41,fl=A2,fh=0E         
;3:     ad=06,sr=08,pl=20,ph=08,ct=81,fl=E1,fh=24         
;4:     ad=06,sr=08,pl=20,ph=08,ct=81,fl=E1,fh=24         
;5:     ad=06,sr=08,pl=20,ph=08,ct=81,fl=E1,fh=24         
;6:     ad=06,sr=08,pl=20,ph=08,ct=81,fl=E1,fh=24         
;7:     ad=06,sr=08,pl=20,ph=08,ct=81,fl=E1,fh=24         

;cl2=08 adsr2=0408 e2=00 cl3=08 adsr3=00A0 e3=00
;cl2=41 adsr2=0408 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=41 adsr2=0408 e2=38 cl3=00 adsr3=00A0 e3=00
;cl2=41 adsr2=0408 e2=17 cl3=81 adsr3=00A0 e3=2A
;cl2=81 adsr2=0408 e2=0A cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0408 e2=04 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0408 e2=02 cl3=08 adsr3=00A0 e3=00
;cl2=81 adsr2=0408 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0408 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0408 e2=00 cl3=81 adsr3=00A0 e3=2A
;cl2=81 adsr2=0408 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0408 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=08 adsr2=0408 e2=00 cl3=08 adsr3=00A0 e3=00 * v2 test bit
;cl2=41 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00 * v2 triangle, gate
;cl2=41 adsr2=0608 e2=3B cl3=00 adsr3=00A0 e3=00 * v2 triangle, sound start
;cl2=41 adsr2=0608 e2=29 cl3=81 adsr3=00A0 e3=2A * v2 triangle, v3 hihat
;cl2=81 adsr2=0608 e2=17 cl3=00 adsr3=00A0 e3=00 * v2 noise
;cl2=81 adsr2=0608 e2=0D cl3=00 adsr3=00A0 e3=00 * v2 noise
;cl2=81 adsr2=0608 e2=09 cl3=08 adsr3=00A0 e3=00 * v2 noise
;cl2=81 adsr2=0608 e2=05 cl3=00 adsr3=00A0 e3=00 * v2 noise
;cl2=81 adsr2=0608 e2=03 cl3=00 adsr3=00A0 e3=00 * v2 noise
;cl2=81 adsr2=0608 e2=02 cl3=81 adsr3=00A0 e3=2A * v2 noise
;cl2=81 adsr2=0608 e2=01 cl3=00 adsr3=00A0 e3=00 * v2 noise
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00 * v2 noise
;cl2=81 adsr2=0608 e2=00 cl3=08 adsr3=00A0 e3=00 * v2 sound end
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=81 adsr3=00A0 e3=2A
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;
katakisV2regDump
	DC.B	$02,$C0,$0B,$00 

    * default state: RELEASE
    DC.B    $02,$CE,$0C,$00	; 2ad=00
	DC.B	$02,$CE,$0D,$08 ; 2sr=08    * RELEASE: sets envelope_rate_period
    * two frames of release ramp down?
 ;DC.B    $02,$CE,$09,$20 ; 2pl=20
    ;DC.B	$02,$CE,$0A,$08 ; 2ph=08
    ;DC.B    $02,$CE,$0B,$81 ; 2cl=81 noise,gate
	;DC.B	$02,$CE,$07,$E1 ; 2fl=E1
    ;DC.B    $02,$CE,$08,$24 ; 2fh=24
    ; ---------------------
;	DC.B	$02,$CF,$0B,$08 ; 2cl=08 test
        ; ---------------------
    DC.B    $02,$D0,$0C,$06	; 2ad=06   attack 0   decay 6
    DC.B	$02,$D0,$0D,$08 ; 2sr=08  sustain 0 release 8
    DC.B    $02,$D0,$09,$20 ; 2pl=20
    DC.B	$02,$D0,$0A,$08 ; 2ph=08
    DC.B    $02,$D0,$0B,$41 ; 2cl=41 triangle,gate
	DC.B	$02,$D0,$07,$A2 ; 2fl=A2
    DC.B    $02,$D0,$08,$0E ; 2fh=0E
    ; ---------------------
	DC.B	$02,$D1,$0C,$06 ; 2ad=06
    DC.B    $02,$D1,$0D,$08 ; 2sr=08
	DC.B	$02,$D1,$09,$20 ; 2pl=20
    DC.B    $02,$D1,$0A,$08 ; 2ph=08
	DC.B	$02,$D1,$0B,$41 ; 2cl=41 triangle,gate
    DC.B    $02,$D1,$07,$45 ; 2fl=45
	DC.B	$02,$D1,$08,$1D ; 2fh=1D
    ; ---------------------
    DC.B    $02,$D2,$0C,$06 ; 2ad=06
	DC.B	$02,$D2,$0D,$08 ; 2sr=08
    DC.B    $02,$D2,$09,$20 ; 2pl=20
	DC.B	$02,$D2,$0A,$08 ; 2ph=08
    DC.B    $02,$D2,$0B,$41 ; 2cl=41 triangle,gate
	DC.B	$02,$D2,$07,$A2 ; 2fl=A2
    DC.B    $02,$D2,$08,$0E ; 2fh=0E
    ; ---------------------
	DC.B	$02,$D3,$0C,$06 ; 2ad=06
    DC.B    $02,$D3,$0D,$08 ; 2sr=08
	DC.B	$02,$D3,$09,$20 ; 2pl=20
    DC.B    $02,$D3,$0A,$08 ; 2ph=08
	DC.B	$02,$D3,$0B,$81 ; 2cl=81 noise,gate
    DC.B    $02,$D3,$07,$E1 ; 2fl=E1
	DC.B	$02,$D3,$08,$24 ; 2fh=24
    ; ---------------------
    DC.B    $02,$D4,$0C,$06 ; 2ad=06
	DC.B	$02,$D4,$0D,$08 ; 2sr=08
    DC.B    $02,$D4,$09,$20 ; 2pl=20
	DC.B	$02,$D4,$0A,$08 ; 2ph=08
    DC.B    $02,$D4,$0B,$81 ; 2cl=81 noise,gate
	DC.B	$02,$D4,$07,$E1 ; 2fl=E1
    DC.B    $02,$D4,$08,$24 ; 2fh=24
    ; ---------------------
	DC.B	$02,$D5,$0C,$06 ; 2ad=06
    DC.B    $02,$D5,$0D,$08 ; 2sr=08
	DC.B	$02,$D5,$09,$20 ; 2pl=20
    DC.B    $02,$D5,$0A,$08 ; 2ph=08
	DC.B	$02,$D5,$0B,$81 ; 2cl=81 noise,gate
    DC.B    $02,$D5,$07,$E1 ; 2fl=E1
	DC.B	$02,$D5,$08,$24 ; 2fh=24
    ; ---------------------
    DC.B    $02,$D6,$0C,$06 ; 2ad=06
	DC.B	$02,$D6,$0D,$08 ; 2sr=08
    DC.B    $02,$D6,$09,$20 ; 2pl=20
	DC.B	$02,$D6,$0A,$08 ; 2ph=08
    DC.B    $02,$D6,$0B,$81 ; 2cl=81 noise,gate
	DC.B	$02,$D6,$07,$E1 ; 2fl=E1
    DC.B    $02,$D6,$08,$24 ; 2fh=24
    ; ---------------------
	DC.B	$02,$D7,$0C,$06 ; 2ad=06
    DC.B    $02,$D7,$0D,$08 ; 2sr=08
	DC.B	$02,$D7,$09,$20 ; 2pl=20
    DC.B    $02,$D7,$0A,$08 ; 2ph=08
	DC.B	$02,$D7,$0B,$81 ; 2cl=81 noise,gate
    DC.B    $02,$D7,$07,$E1 ; 2fl=E1
	DC.B	$02,$D7,$08,$24 ; 2fh=24
    ; ---------------------
katakisV2regDumpEnd

;katakis v3 snare:
;-1:    ct=08                                      
;0:     ad=08,sr=08,pl=40,ph=08,ct=41,fl=A2,fh=0E         
;1:     ad=08,sr=08,pl=80,ph=08,ct=41,fl=45,fh=1D         
;2:     ad=08,sr=08,pl=c0,ph=08,ct=41,fl=A2,fh=0E         
;3:     ad=08,sr=08,pl=00,ph=09,ct=81,fl=e1,fh=24         
;4:     ad=08,sr=08,pl=40,ph=09,ct=81,fl=e1,fh=24         
;5:     ct=81                                      
;6:     ct=00

;cl2=81 adsr2=0608 e2=00 cl3=81 adsr3=00A0 e3=2A
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=08 adsr3=00A0 e3=00 * v3 test, gate
;cl2=81 adsr2=0608 e2=00 cl3=41 adsr3=0808 e3=34 * v3 triangle, gate, sound start
;cl2=81 adsr2=0608 e2=00 cl3=41 adsr3=0808 e3=28 * v3 triangle, gate 
;cl2=81 adsr2=0608 e2=00 cl3=41 adsr3=0808 e3=1B * v3 triangle, gate => 3 frames of triangle
;cl2=81 adsr2=0608 e2=00 cl3=81 adsr3=0808 e3=13 * v3 noise, gate 
;cl2=81 adsr2=0608 e2=00 cl3=81 adsr3=0808 e3=0D * v3 noise, gate 
;cl2=81 adsr2=0608 e2=00 cl3=08 adsr3=0808 e3=0A * v3 test, sound stop
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=0A
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=81 adsr3=00A0 e3=2A
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=08 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=81 adsr3=00A0 e3=2A
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=08 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=81 adsr3=00A0 e3=2A
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=08 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=81 adsr3=00A0 e3=2A
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00
;cl2=81 adsr2=0608 e2=00 cl3=00 adsr3=00A0 e3=00

katakisV3regDump
;    incbin  katakis_v3.regdump
    DC.B    $07,$35,$13,$00
	DC.B	$07,$35,$14,$A0
    DC.B    $07,$35,$10,$00
	DC.B	$07,$35,$11,$00
    DC.B    $07,$35,$12,$00     * 3cl=clear gate
	DC.B	$07,$35,$0E,$45
    DC.B    $07,$35,$0F,$1D
    ;----------------------
;	DC.B	$07,$36,$13,$00 * 3ad=00
;    DC.B    $07,$36,$14,$A0 * 3sr=A0
	DC.B	$07,$36,$13,$00 * 3ad=00  *  attack 0   decay 0
    DC.B    $07,$36,$14,$A0 * 3sr=A0  * sustain A release 0
 	DC.B	$07,$36,$10,$00 * 3pl=00
    DC.B    $07,$36,$11,$00 * 3ph=00
	DC.B	$07,$36,$12,$00 * 3cl=00  * 3cl=clear gate -> release 0 -> env3=0
    DC.B    $07,$36,$0E,$ED * 3fl=ED
	DC.B	$07,$36,$0F,$15 * 3hf=15
    ;----------------------
    DC.B    $07,$37,$12,$08 * 3cl=test, clear gate
    ;----------------------
	DC.B	$07,$38,$13,$08 * 3ad=08
    DC.B    $07,$38,$14,$08 * 3sr=08
	DC.B	$07,$38,$10,$40 * 3pl=40
    DC.B    $07,$38,$11,$08 * 3ph=08
	DC.B	$07,$38,$12,$41 * 3cl=triangle, gate
    DC.B    $07,$38,$0E,$A2 * 3fl=A2
	DC.B	$07,$38,$0F,$0E * 3fl=0E
    ;----------------------
    DC.B    $07,$39,$13,$08 * 3ad=08
	DC.B	$07,$39,$14,$08 * 3sr=08
    DC.B    $07,$39,$10,$80 * 3pl=80
	DC.B	$07,$39,$11,$08 * 3ph=08
    DC.B    $07,$39,$12,$41 * 3cl=triangle, gate
	DC.B	$07,$39,$0E,$45 * 3fl=45
    DC.B    $07,$39,$0F,$1D * 3fh=1D
    ;----------------------
	DC.B	$07,$3A,$13,$08 * 3ad=08
    DC.B    $07,$3A,$14,$08 * 3sr=08
	DC.B	$07,$3A,$10,$C0 * 3pl=C0
    DC.B    $07,$3A,$11,$08 * 3ph=08
	DC.B	$07,$3A,$12,$41 * 3cl=triangle, gate
    DC.B    $07,$3A,$0E,$A2 * 3fl=A2
	DC.B	$07,$3A,$0F,$0E * 3fh=0e
    ;----------------------
    DC.B    $07,$3B,$13,$08 * 3ad=08
	DC.B	$07,$3B,$14,$08 * 3sr=08
    DC.B    $07,$3B,$10,$00 * 3pl=00
	DC.B	$07,$3B,$11,$09 * 3ph=09
    DC.B    $07,$3B,$12,$81 * 3cl=noise, gate
	DC.B	$07,$3B,$0E,$E1 * 3fl=E1
    DC.B    $07,$3B,$0F,$24 * 3fh=24
    ;----------------------
	DC.B	$07,$3C,$13,$08 * 3ad=08
    DC.B    $07,$3C,$14,$08 * 3sr=08
	DC.B	$07,$3C,$10,$40 * 3pl=40
    DC.B    $07,$3C,$11,$09 * 3ph=09
	DC.B	$07,$3C,$12,$81 * 3cl=noise, gate
    DC.B    $07,$3C,$0E,$E1 * 3fl=E1
	DC.B	$07,$3C,$0F,$24 * 3fh=24
    ;----------------------
    DC.B    $07,$3D,$12,$08 * 3cl=test
    ;----------------------
	DC.B	$07,$3E,$13,$00 * 3ad=00
    DC.B    $07,$3E,$14,$A0 * 3sr=A0
	DC.B	$07,$3E,$10,$00 * 3pl=00
    DC.B    $07,$3E,$11,$00 * 3ph=00
	DC.B	$07,$3E,$12,$00 * 3cl=00
    DC.B    $07,$3E,$0E,$A2 * 3fl=A2
	DC.B	$07,$3E,$0F,$0E * 3fh=0E
    ;----------------------
    DC.B    $07,$3F,$13,$00 * 3ad=00
	DC.B	$07,$3F,$14,$A0 * 3sr=A0
    DC.B    $07,$3F,$10,$00 * 3pl=00
	DC.B	$07,$3F,$11,$00 * 3ph=00
    DC.B    $07,$3F,$12,$00 * 3cl=00
	DC.B	$07,$3F,$0E,$A2 * 3fl=A2
    DC.B    $07,$3F,$0F,$0E * 3fh=0E
    ;----------------------
;	DC.B	$07,$40,$13,$00 * 3ad=00
;    DC.B    $07,$40,$14,$A0 * 3sr=A0
;	DC.B	$07,$40,$10,$00 * 3pl=00
;    DC.B    $07,$40,$11,$00 * 3ph=00
;	DC.B	$07,$40,$12,$81 * 3cl=noise, gate
;    DC.B    $07,$40,$0E,$14
;	DC.B	$07,$40,$0F,$75
    ;----------------------
    DC.B    $07,$41,$13,$00 * 3ad=00
	DC.B	$07,$41,$14,$A0 * 3sr=A0
    DC.B    $07,$41,$10,$00 * 3pl=00
	DC.B	$07,$41,$11,$00 * 3ph=00
    DC.B    $07,$41,$12,$00 * 3cl=00
	DC.B	$07,$41,$0E,$45
    DC.B    $07,$41,$0F,$1D
    ;----------------------
	DC.B	$07,$42,$13,$00 * 3ad=00
    DC.B    $07,$42,$14,$A0 * 3sr=A0
	DC.B	$07,$42,$10,$00 * 3pl=00
    DC.B    $07,$42,$11,$00 * 3ph=00
	DC.B	$07,$42,$12,$00 * 3cl=00
    DC.B    $07,$42,$0E,$ED
	DC.B	$07,$42,$0F,$15
    ;----------------------
katakisV3regDumpEnd

regDump
;    incbin  clubstyle.regdump2
    incbin  advanced_chemistry.regdump
regDumpEnd

     include "resid-68k.s"


    section bss1,bss

Sid                 ds.b    resid_SIZEOF
DOSBase             ds.l    1
GFXBase             ds.l    1
workerTaskStack     ds.b    4096
workerTaskStruct    ds.b    TC_SIZE

    section bss2,bss_c

buffer1  ds.b	10000
buffer2  ds.b	10000
buffer1b  ds.b	10000
buffer2b  ds.b	10000



