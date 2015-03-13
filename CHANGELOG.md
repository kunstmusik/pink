# CHANGELOG for Pink

## 0.2.0

New 

* pink.oscillators
  * pulse - pulse generator
* pink.delays
  * samp-delay - non-interpolating delay line with fixed delay-time in
    samples.
  * frac-delay,fdelay - interpolating (fractional) delay lines with fixed
    delay-time in samples/seconds.
* pink.util
  * gen-recur - macro for generator recur statements that takes care of
    incrementing indx

Fixed

* pink.delays
  * adelay - calculation for delay time was off by buffer-size, reimplemented
    using new samp-delay
* pink.envelopes
  * adsr - fixed error when \*done\* and \*duration\* were nil 

## 0.1.0

* Initial Release
