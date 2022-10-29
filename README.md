Motorola 68020 port of reSID version 0.16.

There are a few choices for output functions. Each are given
output buffer address and amount of SID cycles to run.

- fast8: 8-bit samples, the nearest SID samples are used.
- fast14: Samples for 14-bit Paula output. The nearest SID samples are used.
- fast16: 16-bit samples. The nearest SID samples are used.
- interpolate14: SID is sampled in shorter intervals and the samples
                 are interpolated. This is a heavier than the fast methods.
                 Samples for 14-bit Paula output.
- oversample2x14: SID is sampled twice the sampling frequency and
                  the samples are averaged. This is heavier than 
                  the fast methods.
                  Samples for 14-bit Paula output.
- oversample3x14: Sample three times, heavier than above.
- oversample4x14: Sample four times, heavier than above.

The fast modes produce sampling noise depending on the tune that is being
played. The interpolate and oversample modes should reduce the noise.

A 68060 at 50MHz can manage the fast modes at CPU load of 50-80% depending
on the tune that is being played.

The code is optimized for 68060. It uses a lot of multiplications and tries
to avoid divisions. Instructions have been arranged for superscalar execution.
