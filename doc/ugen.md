# Unit Generators

Unit Generators are composable units of signal processing. The term comes from Max Matthews and was introduced in Music 3. In a 2011 [interview with Geeta Dayal](http://blog.frieze.com/max-mathews), Matthews says:

"MUSIC 3 was my big breakthrough, because it was what was called a block diagram compiler, so that we could have little blocks of code that could do various things. One was a generalized oscillator … other blocks were filters, and mixers, and noise generators."

Th system of Unit Generators has had a great influence on computer music software since its introduction. The design of Unit Generators differs depending on the context of the system they're in.  Some have added capabilities such as the ability to accept messages, exposure of variables for runtime modification, and reinitialization.  The design of Unit Generators in Pink is simpler, as the features that those capabilities are used to support can be supported by other means.  (This is largely due to the use of higher-order programming that is available in Lisps and Clojure.) 

## Lifecycle of Unit Generators

The following section discusses the basic lifecycle of a unit-generator. The following defines general aspects of unit generators that are largely shared by systems using unit generators, regardless of programming language. 

* Allocation
* Initialization
* Performance
* Deallocation

The following is largely based on previous work in [1](http://kunstmusik.com/2014/09/23/extending-aura-with-csound-opcodes/).

### Allocation

Memory allocation is generally carefully handled in audio systems.  Approaches include implementing custom real-time memory allocators and/or garbage collectors, as well as reuse via pooling. In Pink, allocation is done per unit generator as object allocation on the JVM is very cheap.  


### Initialization

An initialization pass for a Unit Generator is done to pre-configure state variables and to pre-calculate constants that will be used during each performance call. Memory may also be allocated at this time and must be handled with care, especially if the unit generator is allocated while on the audio main thread. The general advice is to not allocate any memory while on the main thread, but this is usually in reference to programming in C/C++ and using malloc/free directly.  In Pink, we rely on the JVM and Garbage Collector, so we can largely relax concerns over allocating any sub-objects/memory.


### Performance

At performance, a unit generator is responsbile for generating n number of samples for a given time t.  The number of samples and how time is measured depends on the system. For example, in a single-sample processing model, time would be measured in number of samples since the start of the engine, and the number of samples to produce would be 1.  For block-based systems, time would be mesaured in number of blocks since the start of the engine, and the number of samples to produce may be something like 64 (depends on how the user or developer has configured the system). 

For block-based systems, a unit generator will largely follow the following pattern:

1. Read state values from the previous pass into local variables
2. Process and generate samples up to the block-size, using a loop.  Using stack and local variables improves performance over the duration of the loop calculation.  
3. When block-size number of samples has been generated, the current local state is then written into state. 4. The return values are returned to the caller. 

In Pink and graph-based audio systems, a check is generally done between steps 1 and 2 to ensure that dependent signals are still valid and if not, to short-circuit to signal done-ness. Also to note, most state that is saved and loaded from a unit generator's memory is done strictly to preserve state between calls.  This pattern for unit generators can then be considered as an implementation of coroutines/generators.  

### Deallocation

Memory for a unit generator is freed, generally when an audio sub-graph is expired/done (i.e. when a note ends).  What happens at deallocation time is dependent on what memory system is implemented.  It could mean a call to free(), a marking that memory is garbage, or a decrement of reference count. Here, Pink follows JVM standard practice and the unit generator will be freed when it no longer has any references to it from live objects. 


## Pink Unit Generators

The following discusses the implementation of Pink Unit Generators using higher order functions.  

...

[1](http://blog.frieze.com/max-mathews)
