# Terminology 

## Audio Engine 

* Buffer - array of doubles, equal to one \*buffer-size\* in length.  The audio engine processes one \*buffer-size\* at a time. A buffer is data for a single channel of audio.

* Frame - interleaved data for all channels for one sample's length. 

* audio-function - Zero-arg function that returns one-to-many buffers of audio data (depending on channels), or returns nil if the generation and processing of audio is complete for the function. These functions are called once per advancement of engine's time, equal to the \*buffer-size\* / \*sr\*.  

* control-function - Zero-arg function that returns true or false to signal whether the function is done processing or not. These functions are scheduled and used like audio-functions but are not part of the audio signal graph.   

* event - Events are messages sent to a node or engine.  They contain a start time, a function to call, and args to use with the function. 

* node - Functions as a node in a tree of an audio signal processing graph. Aggregates output from child audio-functions. Has its own message queue for scheduling and processing events.

