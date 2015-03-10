# CHANGELOG for Pink

## 0.2.0

New 

* pink.oscillators
  * pulse - pulse generator
* pink.delays
  * samp-delay - non-interpolating delay line with fixed delay-time in
    samples.

Fixed

* pink.delays
  * adelay - calculation for delay time was off by buffer-size, reimplemented
    using new samp-delay

## 0.1.0

* Initial Release
