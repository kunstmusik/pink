# Architecture 

The design of Pink is motivated by the concept of being a Minimal Engine. It is the goal in Pink's design to have a limited set of primitive constructs that allow for supporting the widest variety of musical use cases.  
## Processing Model

Pink uses a block-based processing model (rather than a single-sample model).  It will process x number of samples at a time, where x is configured by the user using the :buffer-size option when creating an engine.  The default setting for engines is a buffer-size of 64 samples and 44100 sampling rate. 

Single-sample processing models allow for the highest temporal resolution for processing of events and control functions.  However, they also have a higher processing cost.  Using block-based processing allows for amortizing costs for each signal-processing unit generator, as they will generate x number of samples in a loop, using local variables for the duration of that loop, and do a store/restore of state only once per block. However, events will only be processed after every block, so timing may skew, and since some signal processing graphs require single-sample delays, special handling is required. 

Pink follows similar tradeoffs as found in Csound.  The user can work with one block size while rendering in realtime, but choose to use a smaller block-size (i.e. block-size = 1) for better event resolution when rendering to disk. Also, like Csound, parts of a signal processing graph can be set to process with smaller block-sizes.  For example, a portion of a graph can be run at block-size = 1, while the rest of the engine may run at block-size = 64.  

### Time

Pink's time counter begins at 0 and is incremented once per processing of its signal-processing graph. Time then is counted in number of blocks run since start.  As the user has access to the current block-size and sample-rate, the user can make calculations to figure out how much time has passed in clock time (i.e. number of seconds = (/ (\* time-counter block-size) sample-rate) ). Pink also has a system for tempo, allowing the user to set event start and duration times in numbers of beats, rather than in seconds.  By default, the tempo for the engine is set to 60 beats per minute, such that time values for events is processed as seconds.

### Synchronization 

For music systems, synchronization with an engine is an important concern. This allows for events to be timed together with the sound that is being produced, as well as allow control functions to operate together with the time of the engine.  To achieve this, event and control processing must be done in the thread of the engine.  Using a separate thread may be useful for other music system design purposes, but it should not be used when synchronization in time is required. 

Pink allows for two constructs for synchronization: events and control-functions.  Events are one time operations that are scheduled with the engine and will be fired when their start times are met.  Control-functions are continuous operations that will run until they return a false value. Event processing and control-function calling is done once per rendering of the audio graph.  Events and control-functions have access to the engine's current time counter and are processed in sync with the audio-graph.

## Main Thread

<img src="architecture.png"/>

Pink uses a single-threaded design. The main thread in Pink is responsible for running the engine. Each time through the loop, the engine has three main tasks: running any events that are scheduled to run at the current engine time, processing the control function graphs, and processing the audio function graph. When processing the audio graph, the engine is responsible for taking the returned audio samples and writing that to the soundcard or to disk.  

Additionally, the engine may respond to two messages, one for clearing the engine and one for stopping the engine. 


## Audio Functions

## Control Functions

## Events

============

The following are older running notes on the design of Pink. These will eventually be rewritten into proper documentation.

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
