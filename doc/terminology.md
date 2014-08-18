# Terminology 

## Audio Engine 

* Buffer - array of doubles, equal to one \*buffer-size\* in length.  The audio engine processes one \*buffer-size\* at a time. A buffer is data for a single channel of audio.

* Frame - interleaved data for all channels for one sample's length. 

* Audio Function - Function that returns one-to-many buffers of audio data (depending on channels), or returns nil if the generation and processing of audio is complete for the function. These functions are called once per advancement of engine's time, equal to the \*buffer-size\* / \*sr\*.  


