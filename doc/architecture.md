# Architecture 

The following are running notes on the design of Pink. These will eventually be rewritten into proper documentation.

## Buffers

* Audio Functions return buffers of audio while they are active, or nil if they are done processing. 
* Buffers may contain a single channel, in which case they are a double-array equal in size to the \*block-size\*
* Buffer may be multi-channel, in which case they are arrays of double-arrays (equivalent to Java double[][]). Each channel is held in a discrete array.
* The global out-buffer that is used by the engine is a double-array that contains space for x number of channels, defined by the engine's nchnl value.  This single array will have the values of channels interleaved into frames. For example, for an Engine with (= nchnls 2), the buffer contains (\* 2 \*block-size\*) size. The data values would alternate with the first value from channel 0, then the first value of channel 1, then the second value of channel 0, and so on.  
* For multi-channel signals, users should use discrete audio channels in their audio code.  
