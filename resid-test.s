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

    moveq   #0,d0
    lea     Sid,a0
    jsr     sid_enable_filter

    moveq   #1,d0
    lea     Sid,a0
    jsr     sid_enable_external_filter


    bsr     createWorkerTask
;    bsr     workerEntry

    bsr     pokeSound
    move    #2*50,d7
    bsr     wait
    bsr     pokeRelease
.loop
    bsr     delay
    btst    #6,$bfe001
    bne     .loop

    bsr     stopWorkerTask    

    * Stop audio
    move    #$f,dmacon+$dff000
    move    #0,$dff0a8
    move    #0,$dff0b8
    move    #0,$dff0c8
    move    #0,$dff0d8
    rts

createWorkerTask:
    lea     workerTaskStruct,a0
    move.b  #NT_TASK,LN_TYPE(a0)
    move.b  #-50,LN_PRI(a0)
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
    rts

.workerTaskName
    dc.b    "Wrkr",0
    even

stopWorkerTask
    tst.b   workerStatus
    beq     .x
    move.b  #-1,workerStatus
.loop
    tst.b   workerStatus
    beq     .x
    bsr     delay
    bra     .loop
.x 
    rts

delay
    move.l  DOSBase,a6
    moveq   #1,d1
    jsr     _LVODelay(a6)
    rts

* in:
*   d7 = 1/50 secs to wait
wait
.l
    bsr     delay
    subq    #1,d7
    bne     .l
    rts

workerEntry
    move.b  #1,workerStatus

    move.w  #INTF_AUD0!INTF_AUD1!INTF_AUD2!INTF_AUD3,intena+$dff000
    move.w  #INTF_AUD0!INTF_AUD1!INTF_AUD2!INTF_AUD3,intreq+$dff000
    move.w  #DMAF_AUD0!DMAF_AUD1!DMAF_AUD2!DMAF_AUD3,dmacon+$dff000
    move    #PAULA_PERIOD,$a6+$dff000
    move    #64,$a8+$dff000

    lea     buffer1,a2
    lea     buffer2,a3
    move.l  a2,$a0+$dff000 

    bsr     fillBufferA2
    move    d0,$a4+$dff000   * words

    bsr     dmawait
    move    #DMAF_SETCLR!DMAF_AUD0,dmacon+$dff000

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
    move.w  intreqr+$dff000,d0
    btst    #INTB_AUD0,d0
    beq.b   .1

    move    bob,$dff180
    not	    bob

    * Switch buffers and fill
    exg     a2,a3
    move.l  a2,$a0+$dff000 
    bsr     fillBufferA2
    move    d0,$a4+$dff000   * words
    
    move.w  #INTF_AUD0,intreq+$dff000
.1
    tst.b   workerStatus
    bpl     .loop
    clr.b   workerStatus
    rts

bob	dc	$0f0

pokeSound
    * filter lo
 REM
    move.b  #0,d0
    move.b  #$15,d1
    lea     Sid,a0
    jsr     sid_write

    * filter hi
    move.b  #1,d0
    move.b  #$16,d1
    lea     Sid,a0
    jsr     sid_write

    * filter resonance
    move.b  #5+3<<4,d0
    move.b  #$17,d1
    lea     Sid,a0
    jsr     sid_write

    * Poke full volume; filter mode: lp
    move.b  #15,d0
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
    lea     Sid,a0
    jsr     sid_write

    * voice 1: freq hi
    move.b  #.F>>8,d0
    move.b  #1,d1
    lea     Sid,a0
    jsr     sid_write

    * voice 1: attack=5, decay=3
    move.b  #13*16+5,d0
    move.b  #5,d1
    lea     Sid,a0
    jsr     sid_write

    * voice 1: sustain=15, release=10
    move.b  #12*16+10,d0
    move.b  #6,d1
    lea     Sid,a0
    jsr     sid_write

    * global volume full
    move.b  #15,d0
    move.b  #24,d1
    lea     Sid,a0
    jsr     sid_write

    * voice 1: Set triangle waveform, set gate bit
    move.b  #16+1,d0
    move.b  #4,d1
    lea     Sid,a0
    jsr     sid_write
    rts

pokeRelease
    * voice 1: Set triangle waveform, clear gate bit
    move.b  #16+0,d0
    move.b  #4,d1
    lea     Sid,a0
    jsr     sid_write
    rts

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

DOSName     dc.b    "dos.library",0
    even

    include "resid-68k.s"

    section bss1,bss

DOSBase             ds.l    1
workerTaskStack     ds.b    4096
workerTaskStruct    ds.b    TC_SIZE
    ;0  = not running
    ;1  = running
    ;-1 = exiting
workerStatus        ds.b    1

    section bss2,bss_c

buffer1  ds.b	10000
buffer2  ds.b	10000

