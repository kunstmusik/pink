# CHANGELOG for Pink

All notable changes to this project will be documented in this file. This
change log follows the conventions of
[keepachangelog.com](http://keepachangelog.com/).

## [Unreleased] 

### Added 

* pink.live-code
  * New namespace for functions useful for live coding against the pink.simple
    engine. 

* pink.util
  * tau2pole, pole2tau - convert between tau (time in seconds) and pole values
    for exponential decay (ported from Faust)
  * limit1, limit - clamp value between min and max; limit1 operates as on a
    single value, limit operates as an block-based audio function

* pink.instruments.pluck
  * pluck - implementation of basic Karplus-Strong algorithm (Based on Plucked
    class from STK)

* pink.io.sound-file 
  * implemented new streaming wav file writing code  
  
* pink.engine 
  * rewrote engine-\>disk to use streaming wav writer
  * engine-\>buffer - renders an engine to double[]. Useful for building
    in-memory caches of pre-rendered score material. 

* pink.event
  * added user-settable event processing function to EventList and utility
    functions for setting event list processing to process incoming events as
    having relative (the default) or absolute times according to the cur-beat
    of the event list. Additional use-absolute-time-events! function added to
    pink.simple.  For more information, please see Issue #10 in the issue
    tracker. 

* pink.node
  * refactored and introduced new Node protocol
  * added control-node and audio-node utility functions that return reified
    Nodes that also implement IFn to adhere to control and audio function
    conventions (simplifies creation and use of nodes)
  * added mixer-node that uses mono-channel audio functions as inputs and
    applies its own gain and stereo panning; mixer-node responds to all Node
    protocol functions as well as GainNode and StereoMixerNode protocols 
    (set-gain!, set-pan!)
  * added gain-node that takes in stereo-channel audio functions as inputs
    and applies its own gain before stereo-channel output. Respondes to all
    Node and GainNode protocol functions.

* pink.config
  * added new \*beat\* config variable that has the current beat time of the
    event list.
  * added new \*engine\* config variable that points to the currently running
    engine.
  * documentation added for each config variable

* pink.filter
  * zdf-ladder - a Zero-delay feedback Moog Ladder filter (4-pole 24db/oct)
  * zdf-1pole - zero-delay feedback, 1-pole (6 dB/oct) filter
    (returns either low-pass, high-pass, or allpass signal)
  * zdf-2pole - zero-delay feedback, 2-pole (12 dB/oct) filter
    (returns low-pass, high-pass, band-pass, unity-gain band-pass, notch, 
    alldpass, or peak signals)
  * k35-lpf - 12 dB/oct lowpass filter (based on Korg 35 filter)
  * k35-hpf - 6 dB/cot highpass filter (based on Korg 35 filter) 
  * diode-filter - 24 dB/oct Diode Ladder filter (used in
    EMS VCS3 and TB-303)
  * lpf-18 - 3-pole (18 dB/oct) low pass filter with resonance and distortion

* pink.noise
  * dust - generate random impulses from 0.0 to +1.0
  * dust2 - generate random impulses from -1.0 to +1.0

* pink.effects.distortion
  * distort - normalized and non-normalized hyperbolic tangent distortion
  * distort1 - modified hyberbolic distortion with assymetric waveshaping

* pink.effects.reverb
  * freeverb - added optional stereo-spread parameter, enhanced documentation

### Changed

* pink.node - node messages (adds, removes, clears) are now stored in a single
  pre-allocated message ring buffer. This addresses processing messages in
  order (e.g., one can do a clear then push adds and this will work all within
  the same process pass) as well as reduces object allocations to zero at
  runtime. However, message buffer size must now be considered when creating a
  node as the message buffer does not support back pressure and message
  processing will go awry if more messages than buffer capacity are scheduled
  before the reader (i.e, the node) processes pending messages.  The default is
  512, but the user may provide a :max-messages keyword argument to any of the
  node creation functions in pink.node to increase the max capacity.

* pink.util
  * updated try-func to catch Throwable instead of Exception; fixes issue with
    assertion errors causing engine to die while live coding 
  * added constant folding to mul and sum. These functions may now return 
    numbers in addition to audio functions. This allows for further 
    folding for mul/sum nested expressions but could possibly break older code. 
    (The risk of breakage is assumed minimal, but please raise an issue if you
    do become affected.)

* pink.oscillators
  * pulse - added optional amplitude argument


## [0.3.0] - 2016-05-24

### Added 

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

### Changed 

* pink.io.midi
  * namespace redesigned for use with :as syntax when requiring [Issue
    #5, changes contributed by @triss]
  * find-device fixed to work on Windows [Issue # 3, fix contributed
    by @triss]

### Fixed

* pink.noise
  * white-noise - paren typo caused noise value to be in range [0,2.0]
    instead of [-1.0,1.0]
* pink.engine
  * root node buffer sizes were not set correctly when using
    non-default *buffer-size* with engines


## [0.2.1] - 2015-09-07 

### Changed 
* removed use of Zach Tellman's primitive-math and added implementation of
  not== to pink.util based on his work

## [0.2.0] - 2015-07-24

### Added 

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

### Fixed

* pink.delays
  * adelay - calculation for delay time was off by buffer-size, reimplemented
    using new samp-delay
* pink.envelopes
  * adsr - fixed error when \*done\* and \*duration\* were nil 

## 0.1.0 - 2015-05-08

* Initial Release



[Unreleased]: https://github.com/kunstmusik/pink/compare/0.3.0...HEAD
[0.2.0]: https://github.com/kunstmusik/pink/compare/0.1.0...0.2.0
[0.3.0]: https://github.com/kunstmusik/pink/compare/0.2.0...0.3.0
