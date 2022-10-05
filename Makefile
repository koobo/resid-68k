INCLUDE = -I$(HOME)/A/Asm/Include 
VASM ?= vasmm68k_mot
VASM_FLAGS := -Fhunkexe -kick1hunks -quiet -m68030 -m68881 -nosym $(INCLUDE)

SOURCE   := resid-test.s resid-68k.s
INCLUDES := resid-68k.i
TARGET   := main

.PHONY: all clean

all: $(TARGET)

clean:
	rm $(TARGET)

$(TARGET) : $(SOURCE) $(INCLUDES) Makefile
	$(VASM) $< -o $@  $(VASM_FLAGS)
