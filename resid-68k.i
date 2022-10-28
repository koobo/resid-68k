;  ---------------------------------------------------------------------------
;  This is a MC680x0 assembler port of reSID, a MOS6581 SID emulator
;  engine, copyright (C) 2004 Dag Lem <resid@nimrod.no>
;
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software
;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;  ---------------------------------------------------------------------------

           IFND       RESID68K_I
RESID68K_I  SET        1

           include    "exec/types.i"

FIXP_SHIFT = 16
FIXP_MASK = $ffff

    ENUM
    EITEM   CHIP_MODEL_MOS6581
    EITEM   CHIP_MODEL_MOS8580

    ENUM
    ; Output bytes
    EITEM   SAMPLING_METHOD_SAMPLE_FAST8
    ; Output two bytes suitable for 14-bit Paula output
    EITEM   SAMPLING_METHOD_SAMPLE_FAST14
    EITEM   SAMPLING_METHOD_INTERPOLATE14
    EITEM   SAMPLING_METHOD_OVERSAMPLE2x14
    EITEM   SAMPLING_METHOD_OVERSAMPLE4x14
    EITEM   SAMPLING_METHOD_SAMPLE_RESAMPLE_INTERPOLATE
    EITEM   SAMPLING_METHOD_SAMPLE_RESAMPLE_FAST
    
    ENUM
    EITEM envelope_state_ATTACK
    EITEM envelope_state_DECAY_SUSTAIN
    EITEM envelope_state_RELEASE

    STRUCTURE   voice,0
        APTR    voice_wave
        APTR    voice_envelope
        ULONG   voice_voice_DC
        UWORD   voice_wave_zero
        UWORD   voice_pad
    LABEL voice_SIZEOF ; = 16

    STRUCTURE   wave,0
        ULONG   wave_accumulator    * 24-bit
        ULONG   wave_shift_register * 24-bit
        APTR    wave_sync_source
        APTR    wave_sync_dest
        APTR    wave_wave__ST       * Pointers to sample data for waveform combinations
        APTR    wave_wave_P_T
        APTR    wave_wave_PS_
        APTR    wave_wave_PST
        UBYTE   wave_msb_rising     * Tells whether the accumulator MSB was set high on this cycle.
        UBYTE   wave_test           * the remaining control register bits
        UBYTE   wave_ring_mod
        UBYTE   wave_sync
        UWORD   wave_freq
        UWORD   wave_pw
        UWORD   wave_waveform       * Control register right-shifted 4 bits for table lookup
        UWORD   wave_pad
    LABEL wave_SIZEOF ; = 44

    STRUCTURE   envelope,0
        ULONG   envelope_rate_counter
        ULONG   envelope_rate_period
        UBYTE   envelope_counterHi
        UBYTE   envelope_counter
        UBYTE   envelope_pad
        UBYTE   envelope_exponential_counter
        UBYTE   envelope_exponential_counter_period
        UBYTE   envelope_hold_zero
        UBYTE   envelope_attack
        UBYTE   envelope_decay
        UBYTE   envelope_sustain
        UBYTE   envelope_release
        UBYTE   envelope_gate
        UBYTE   envelope_state
    LABEL envelope_SIZEOF ; = 20

    STRUCTURE   filter,0
        UBYTE   filter_enabled
        UBYTE   filter_res
        UBYTE   filter_filt
        UBYTE   filter_voice3off
        UBYTE   filter_vol
        UBYTE   filter_pad1
        UBYTE   filter_pad2
        UBYTE   filter_pad3
        UWORD   filter_hp_bp_lp
        UWORD   filter_fc
        ULONG   filter_mixer_DC
        ULONG   filter_Vhp
        ULONG   filter_Vbp
        ULONG   filter_Vlp
        ULONG   filter_Vnf
        ULONG   filter_w0_ceil_1
        ULONG   filter_w0_ceil_dt
        ULONG   filter_1024_div_Q
        APTR    filter_f0
        ULONG   filter_w0_max_1
        ULONG   filter_w0_max_dt
    LABEL filter_SIZEOF ; = 56

    STRUCTURE   extfilter,0
        UBYTE   extfilter_enabled
        UBYTE   extfilter_pad1
        UBYTE   extfilter_pad2
        UBYTE   extfilter_pad3
        ULONG   extfilter_mixer_DC
        ULONG   extfilter_Vlp
        ULONG   extfilter_Vhp
        ULONG   extfilter_Vo
        ULONG   extfilter_w0lp
        ULONG   extfilter_w0hp
    LABEL extfilter_SIZEOF ; = 28

    STRUCTURE   sid,0
        APTR    sid_voice1
        APTR    sid_voice2
        APTR    sid_voice3
        APTR    sid_filter
        APTR    sid_extfilt
        ULONG   sid_bus_value_ttl
        ULONG   sid_clock_frequency
        ULONG   sid_cycles_per_sample   * 16.16 fixed point
        ULONG   sid_sample_offset
        APTR    sid_clock_routine
        UBYTE   sid_bus_value
        UBYTE   sid_sampling_method
        UWORD   sid_sample_prev
        UWORD   sid_volume
        UWORD   sid_oversampleShift
    LABEL sid_SIZEOF ; = 48

           ENDIF

           