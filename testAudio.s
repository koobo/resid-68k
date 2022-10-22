;APS00000015000000150000001500000015000000150000001500000015000000150000001500000015
    incdir  include:
    include exec/exec_lib.i
    include exec/tasks.i
    include dos/dos_lib.i
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
SAMPLES_PER_HALF_FRAME set (SAMPLING_FREQ+50)/100
; Must be even for Paula 
 ifne SAMPLES_PER_HALF_FRAME&1
SAMPLES_PER_HALF_FRAME set SAMPLES_PER_HALF_FRAME+1
 endif
 
* f=c/p
* fp=c
* p=c/f

main:
    bsr     openTimer
    move.l  4.w,a6
    lea     DOSName,a1
    jsr     _LVOOldOpenLibrary(a6)
    move.l  d0,DOSBase

    lea     Sid,a0
    jsr     sid_constructor

    move.l  #985248,d0
    moveq   #SAMPLING_METHOD_SAMPLE_FAST,d1
    move.l  #SAMPLING_FREQ,d2
    lea     Sid,a0
    jsr     sid_set_sampling_parameters

    moveq   #0,d0
    lea     Sid,a0
    jsr     sid_enable_filter

    moveq   #0,d0
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
    rts

delay
    movem.l d0-a6,-(sp)
    move.l  DOSBase,a6
    moveq   #1,d1
    jsr     _LVODelay(a6)
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
    bra sid_write

playDump
    lea     regDump+$000,a5
    lea     regDumpEnd,a6

    * vblank timer
    move	 #$1080,d7
    moveq   #0,d7
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

    lea     Sid,a0
    jsr     sid_write
    
    addq    #4,a5
    cmp.l   a6,a5
    bhs     .x
    bra     .a

.s

    addq.l  #1,d7
    cmp.l   a6,a5
    blo     .loop
.x

    rts


DOSName     dc.b    "dos.library",0
    even

regDump
;    incbin  clubstyle.regdump2
    incbin  advanced_chemistry.regdump
regDumpEnd

     include "resid-68k.s"


    section bss1,bss

DOSBase             ds.l    1
workerTaskStack     ds.b    4096
workerTaskStruct    ds.b    TC_SIZE
    section bss2,bss_c

buffer1  ds.b	10000
buffer2  ds.b	10000
buffer1b  ds.b	10000
buffer2b  ds.b	10000



