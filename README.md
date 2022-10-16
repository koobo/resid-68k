Motorola 68020 port of reSID version 0.16.

Only the "SAMPLE_FAST" method is provided: delta clocking picking nearest sample.
This is the lightest in terms of CPU use. It will also sometimes produce sampling noise. 
The other methods are likely too heavy for the 68060/50MHz.

The code is optimized for 68060. It uses a lot of multiplications and tries
to avoid divisions. Instructions have been arranged for superscalar execution.