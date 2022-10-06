;APS00000015000000150000001500000015000000150000001500000015000000150000001500000015
    incdir  include:
    include exec/exec_lib.i
    include exec/tasks.i
    include dos/dos_lib.i
    include hardware/custom.i
    include hardware/cia.i
    include hardware/dmabits.i
    include hardware/intbits.i

* Constants
PAL_CLOCK=3546895
SAMPLING_FREQ=44100/2
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

    moveq   #1,d0
    lea     Sid,a0
    jsr     sid_enable_filter

    moveq   #1,d0
    lea     Sid,a0
    jsr     sid_enable_external_filter


    bsr     createReSIDWorkerTask
 
  ;  move    #1*50,d7
  ;  bsr     wait
    
 ;    bra      .skip
 ;   bra     workerEntry

  ;  bsr     playDump  
  ;  move    #1*50,d7
  ;  bsr     wait
  ;  bra     .skip

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
    move.w  #DMAF_AUD0!DMAF_AUD1,dmacon+$dff000

    move    #PAULA_PERIOD,$a6+$dff000
    move    #PAULA_PERIOD,$b6+$dff000
    ; TODO: hook up vol control
    move    #64,$a8+$dff000
    move    #64,$b8+$dff000

    lea     buffer1,a2
    lea     buffer2,a3
    move.l  a2,$a0+$dff000 
    move.l  a2,$b0+$dff000 

    bsr     fillBufferA2
    move    d0,$a4+$dff000   * words
    move    d0,$b4+$dff000   * words

    bsr     dmawait
    move    #DMAF_SETCLR!DMAF_AUD0!DMAF_AUD1,dmacon+$dff000
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

    move.l  4.w,a6
.loop
    moveq   #0,d0
    move.b  reSIDAudioSignal(pc),d1
    bset    d1,d0
    move.b  reSIDExitSignal(pc),d1
    bset    d1,d0
    jsr     _LVOWait(a6)

    tst.b   workerStatus
    bmi.b   .x

    move    .bob,$dff180
    not	    .bob

    * Switch buffers and fill
    exg     a2,a3
    move.l  a2,$a0+$dff000 
    move.l  a2,$b0+$dff000 
    bsr     fillBufferA2
    move    d0,$a4+$dff000   * words
    move    d0,$b4+$dff000   * words

    bra     .loop
.x
    move.w  #INTF_AUD0,intena+$dff000
    move.w  #INTF_AUD0,intena+$dff000
    moveq	#INTB_AUD0,d0
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

.bob dc.w   $f0f

    ;0  = not running
    ;1  = running
    ;-1 = exiting
workerStatus        dc.b    0
    even

fillBufferA2:
    lea     Sid,a0
    move.l  a2,a1           * target buffer
    move.l  #100000,d0      * cycle limit, set high enough
    * bytes to get
    move.l  #SAMPLES_PER_HALF_FRAME,d1
    movem.l a2/a3,-(sp)
    jsr     sid_clock_fast
    movem.l (sp)+,a2/a3
    * d0 = bytes received, make words
    lsr     #1,d0
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

* a0 = custom base
* a1 = is_data = task
* a6 = execbase
reSIDLevel4Handler1
    move.w  #INTF_AUD0,intreq(a0)
    move.b  reSIDAudioSignal(pc),d1
    moveq   #0,d0
    bset    d1,d0
    jmp     _LVOSignal(a6)
    
reSIDAudioSignal    dc.b    0
reSIDExitSignal    dc.b    0
    even

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
    move.b  #1<<6+1,d0
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


playDump
    lea     regDump+$b0,a5
    lea     regDumpEnd,a6
    moveq   #0,d7
.loop
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
    * Actual start at $b0
    incbin  oceanloader3.dump
regDumpEnd

    include "resid-68k.s"


    section bss1,bss

DOSBase             ds.l    1
workerTaskStack     ds.b    4096
workerTaskStruct    ds.b    TC_SIZE
    section bss2,bss_c

buffer1  ds.b	10000
buffer2  ds.b	10000

