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

In Pink, Unit Generators are implemented as higher order functions.  In general, Pink Unit Generators follow the same coding practices as found in unit generators in other systems, such as Csound and SuperCollider. The use of higher order functions follows closely to what is done in those systems, offering the same lifecycle with which to design UGens.  

The basic shape of a Pink Unit generator is:

```clojure
(defn some-ugen
  [arg0 arg1 arg2]
  (let [x (some-calculation arg1)
        out (create-buffer)]
    (fn []
      (do-processing x arg0 arg1 out)
      out))
```

In Pink, a Unit Generator is built as function that returns a function.  The outer function is where allocation and initialization is done, and the returned function is used for performance.  At performance time, the audio function will return either a buffer of audio or nil.  Returning nil signifies that the audio function is done processing.  

In the let-block above, on can see where initialization work is generally done. Code there will be used to pre-calculate some constants as well create state variables for use between calls to the audio function. The return function will close over both the arguments to the outer function as well as the let-bindings. 

A Unit Generator in Pink must be sure to check that if any Unit Generators it depends on is done (returns nil).  If a nil is found, the unit generator must short-circuit and return nil itself.  In turn, an audio processing node will in turn check if a sub-graph is done and if so, remove that sub-graph to prevent further processing.
  
To note, most Unit Generators yield stateful functions. State is generally used only for storing and restoring values that are used in the processing loop, and are scoped only to the function which closes over it.  This state should therefore not be allowed to escape its scope and thus shared outside of the function. Because the state is privately scoped, it is safe to use by the function.  (This follows the same logic as to how transient collections are safe.)

### Generator Macro

Because Unit Generator code has a number of common requirements, the generator macro was developed to ease writing of unit generators.  The generator macro has the following shape, using four parts:

```clojure
(generator
  [cur-state state] ; 1. State pairs 
  [sig sig-fn] ; 2. Signal in sig function
  (let [some-value (calculation cur-state sig)] ; 3. Calculation for current sample
    (aset out int-index some-value)
    (recur (unchecked-inc indx) (update cur-state)))
  (yield out)) ; 4. Value to yield
```

The above reads as "For the cur-state in state, and sig in the result of calling sig-fn, process with loop until \*buffer-size\*, and yield out".  The expanded macro would create a function that will:

1. Restore cur-state from the last value of stat
2. Call sig-fn and assign the value to a temporary value. If the value is nil, immediately short-circuit and return nil.  
3. In a loop, using the values from the state and signals sections (sections 1 and 2), call the section 3 body until indx is >= \*buffer-size\*. 
4. When index is >= \*buffer-size\*, save cur-state to state, then return out.

The above generator would macroexpand out to the following:

```clojure
(let* [state1890 (double-array 1 state) 
       buffer-size1891 (clojure.core/long pink.config/*buffer-size*)] 
   (fn* 
     ([] 
     (let* [buffer1889 (sig-fn)] 
       (if buffer1889 
         (do (loop* [indx 0 cur-state (clojure.core/aget state1890 0)] 
           (if (clojure.core/< indx buffer-size1891) 
             (let* [int-indx (clojure.core/int indx) 
                    sig (aget buffer1 889 indx)] 
               (let* [some-value (calculation cur-state sig)] 
                 (aset out int-index some-value) 
                 (recur (unchecked-inc indx) (update cur-state)))) 
             (do (clojure.core/aset state1890 0 cur-state) out)))))))))
```

It is recommended to use the generator macro where possible, as it can 
lead to easier to read as well as safer code (due to its handling of state). 
However, the generator macro does not currently work for all unit generator 
use cases, as some Unit Generators require more logic per-sample than the
above gives (i.e. envelope generators).  


### Example: Phasor

The following is the source for the Phasor unit generator:

```clojure
(defn phasor 
  "Phasor with fixed frequency and starting phase"
  [^double freq ^double phase]
  (let [phase-incr ^double (/ freq (double *sr*))
        out ^doubles (create-buffer)]
    (generator 
      [cur-phase phase]
      []
      (do
        (aset out int-indx cur-phase)
        (recur (unchecked-inc indx) (rem (+ phase-incr cur-phase) 1.0)))
      (yield out))))
```

The phasor will, given a frequency and starting phase, return a function that will generate audio signals from 0.0 to 1.0 over and over again, repeating at the given frequency and offset by the given phase. 

The phasor uses the generator macro to simplify unit generator writing. Note it does not use the signals section of the macro (section 2). Some unit generators that use the generator macro may not use section 1 or section 2.

From here, it is best to study the existing unit generators to see how the generator macro is used and cases where it is not used.
