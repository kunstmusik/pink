# compose

A Clojure library designed to ... well, that part is up to you.

## Premise for Design

The following are principles of design for this library:

1. The functions of the library shall be designed for maximal reuse
2. A function that is useful for one area of the system might very well be useful on another level
3. The user shall be provided with a simple and stable API that will seek to cover general use cases for the library. This top-level API shall be designed as an encoding of a best practice for a given use case, but will not presume to be a generalized solution for all use cases.
4. The user shall be able to create alternate systems using the lower-level, full API of functions. The assumption is that the user is working towards a use case that is not accounted for by the top-level API. It is hoped then that the use case be published and the user's own system design be submitted to become part of the top-level API, given that the use case may be one that others will come across.


## Usage

FIXME


## Notes

* Systems Features to Consider
 * Visualization using Incanter
 * GUI Design
 * Convention over Configuration
 * Inversion of Control (IOC) and Dependency Injection (DI)

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
