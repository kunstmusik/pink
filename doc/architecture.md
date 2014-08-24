# Architecture 

The following are running notes on the design of Pink. These will eventually be rewritten into proper documentation.

## Premise for Design

The following are principles of design for this library:

1. The functions of the library shall be designed for maximal reuse
2. A function that is useful for one area of the system might very well be useful on another level
3. The user shall be provided with a simple API that will seek to cover general use cases for the library. This top-level API shall be designed as an encoding of a best practice for a given use case, but will not presume to be a generalized solution for all use cases.
4. The user shall be able to create alternate systems using the lower-level, full API of functions. The assumption is that the user is working towards a use case that is not accounted for by the top-level API. It is hoped then that the use case be published and the user's own system design be submitted to become part of the top-level API, given that the use case may be one that others will come across.
5. The system should be based on common abstractions (i.e. functions, closures) and seek to introduce as few new abstractions as possible.

## Buffers

* Audio Functions return buffers of audio while they are active, or nil if they are done processing. 
* Buffers may contain a single channel, in which case they are a double-array equal in size to the \*block-size\*
* Buffer may be multi-channel, in which case they are arrays of double-arrays (equivalent to Java double[][]). Each channel is held in a discrete array.
* The global out-buffer that is used by the engine is a double-array that contains space for x number of channels, defined by the engine's nchnl value.  This single array will have the values of channels interleaved into frames. For example, for an Engine with (= nchnls 2), the buffer contains (\* 2 \*block-size\*) size. The data values would alternate with the first value from channel 0, then the first value of channel 1, then the second value of channel 0, and so on.  
* For multi-channel signals, users should use discrete audio channels in their audio code.  
