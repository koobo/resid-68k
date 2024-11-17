INCLUDE = -I$(HOME)/A/Asm/Include 
VASM=~/Prj/vbcc/bin/vasmm68k_mot
VASM_FLAGS := -Fhunkexe -kick1hunks -quiet -m68060 -nosym $(INCLUDE)

RESID    := resid-68k.i resid-68k.s

DATE := `git log -1 --format=%aI | head -c16 | sed s/://`

all: testAudio testCycles

clean:
	rm testAudio testCycles

testAudio: testAudio.s $(RESID)
	$(VASM) $< -o $@  $(VASM_FLAGS)

testCycles: testCycles.s $(RESID)
	$(VASM) $< -o $@  $(VASM_FLAGS)


tc: testCycles
	cp $< tc-$(DATE)
