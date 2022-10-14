INCLUDE = -I$(HOME)/A/Asm/Include 
VASM ?= vasmm68k_mot
VASM_FLAGS := -Fhunkexe -kick1hunks -quiet -m68020 -nosym $(INCLUDE)

RESID    := resid-68k.i resid-68k.s

all: testAudio testCycles

clean:
	rm testAudio testCycles

testAudio: testAudio.s $(RESID)
	$(VASM) $< -o $@  $(VASM_FLAGS)

testCycles: testCycles.s $(RESID)
	$(VASM) $< -o $@  $(VASM_FLAGS)
