# CHANGELOG for Pink

## 0.3.0 

New

* pink.processes
  * New namespace for creating syncronously-executed processes that
    conform to the pink control function convention.  (Similar to
    Common Music's processes and Chuck's Shreds.) 
  * process - creates a process state-machine control function
  * wait - waits upon a given time value, PinkSignal, or predicate
  * cue - creates a cue signal; can be checked with has-cued? and
    signalled with signal-cue; satsfies PinkSignal protocol
  * countdown-latch - creates a countdown-latch for coordinated
    signaling; initialized with number to count down; count-down
    decrements the latch count; latch-done? checks if latch is
    complete; satisfies PinkSignal protocol 
* pink.filters
  * biquad-lpf, biquad-hpf, biquad-bpf, biquad-notch, biquad-peaking,
    biquad-lowshelf, biquad-highshelf - Low Pass, High Pass, Band
    Pass, Notch, peaking, low shelf, and highelf filters based on the
    transposed direct form II (tdf2) form of biquad
* pink.oscillators
  * unirect - generates unipolar rectangular audio signal with given
    frequency and duty cycle. Works well as a gate signal for adsr140
    envelope.
* pink.util
  * hold-until - audio function that emits a given start value for
    duration, then emits end-value. duration is given in seconds.
    end-value may be a double or audio function; if the latter, the
    audio function will be called to process once the hold time is
    complete.
* pink.instruments.piano - translation of Scott Van Duyne's Piano
  Model from Common Lisp Music.  
* pink.envelopes
  * hold - simple envelope that holds a given value for a given duration.
    Will zero out after duration if duration ends mid-buffer, then return
    nil afterwards to signal completion.
* pink.control
  * chain - Creates a control function that chains together
    other control functions.  Executes first control-fn until
    completion, then the second, and so on.

Updated

* pink.io.midi
  * namespace redesigned for use with :as syntax when requiring [Issue
    #5, changes contributed by @triss]
  * find-device fixed to work on Windows [Issue # 3, fix contributed
    by @triss]

Fixed

* pink.noise
  * white-noise - paren typo caused noise value to be in range [0,2.0]
    instead of [-1.0,1.0]
* pink.engine
  * root node buffer sizes were not set correctly when using
    non-default *buffer-size* with engines


## 0.2.1

* removed use of Zach Tellman's primitive-math and added implementation of
  not== to pink.util based on his work

## 0.2.0

New 

* pink.oscillators
  * pulse - unipolar pulse generator 
* pink.delays
  * samp-delay - non-interpolating delay line with fixed delay-time in
    samples.
  * frac-delay,fdelay - interpolating (fractional) delay lines with fixed
    delay-time in samples/seconds.
  * delay-read,delay-readi - higher order function for creating indexed and
    interpolated delay line reader functions
* pink.filters
  * statevar - state-variable filter: returns multi-channel audio with
    high-pass, low-pass, band-pass, and band-reject versions of input signal
  * comb - feedback comb filter
  * combinv - feedforward comb filter
* pink.util
  * gen-recur - macro for generator recur statements that takes care of
    incrementing indx
  * get-channel - Audio Function that gets a channel of audio from a
    multi-channel generating audio function
  * with-signals - macro that destructures multi-channel audio function signal
    into separate single-channel audio function signals
  * merge-signals - function that merges output of two separate audio function
    signals ino a single stereo audio function signal
  * apply-stereo - destructures a stereo audio signal, applies func to each 
    channel, and merges back into a stereo audio function signal
* pink.effects.chorus
  * chorus - added stereo chorus effect
* pink.effects.reverb
  * freeverb - implementation of freeverb reverb processor
* pink.effects.ringmod
  * ringmod - Implementation of Julian Parker's digital model of a 
    diode-based ring modulator
* Updated to Clojure 1.7.0

Fixed

* pink.delays
  * adelay - calculation for delay time was off by buffer-size, reimplemented
    using new samp-delay
* pink.envelopes
  * adsr - fixed error when \*done\* and \*duration\* were nil 

## 0.1.0

* Initial Release
