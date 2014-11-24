# Terminology 

## Audio Engine 

* Buffer - array of doubles, equal to one \*buffer-size\* in length.  The audio engine processes one \*buffer-size\* at a time. A buffer is data for a single channel of audio.

* Frame - interleaved data for all channels for one sample's length. 

* audio-function - Zero-arg function that returns one-to-many buffers of audio data (depending on channels), or returns nil if the generation and processing of audio is complete for the function. These functions are called once per advancement of engine's time, equal to the \*buffer-size\* / \*sr\*.  

* control-function - Zero-arg function that returns true or false to signal whether the function is done processing or not. These functions are used for side-effects and are to model concurrent processes that are synchronized with the engine's time. Side-effects should be limited to adding and removing new audio or control functions, or adding new events to the engine.  Control Functions are called continuously until they return false.  

* event - Events are messages sent to a node or engine. They are conceived strictly as a delayed function application, without any knowledge of what they will do.  They contain a start time, a function to call, and args to use with the function. Events are used for side effects. Side-effects should be limited to adding and removing new audio or control functions, or adding new events to the engine. The event results in a one-time function call.  

* node - Nodes in pink are points in a graph that allow for dynamically adding and removing sub-graphs of functions. When used in the audio graph, they run current audio functions and sum output from child audio-functions, as well as discard any audio-function that returns a nil. When used as part of a control graph, they run any current function it contains and removes any that signal false. Nodes have their own message queue for scheduling and processing pending adds and removes.

* Unit Generator - Composable units of signal processing. These are used to create directed acyclic graphs of signal processing, for example, to build up instruments  effects. See [ugen](ugen.md) for more information.  

* Engine Time - The current value of time by the engine, measured as the number of blocks or number of samples since the engine's start, depending on the processing model of the engine. Pink uses a block-based model, so time is measured in number of blocks; sample time can be derived from block time (block number * block size) for sample-accurate operations.  For musical operations to be in sync with generated sound, they must be done in sync with engine time. If an operation is done in accordance with another clock (i.e. using a separate timer in another thread), it can not be in sync with the engine time. 
