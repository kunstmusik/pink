# Processes 

## Introduction

Processes are a way to create Pink control functions using code that "feels" like writing multi-threaded code. The process macro from pink.processes is used to create the process and is most often used in conjunction with the wait function.  The wait function pauses the operation of the process for the given time in seconds, until a signal is received, or until a predicate indicates that the wait condition is no longer valid.  

Processes, like Chuck's Shreds, are run synchronously with the engine.  However, unlike Chuck, Pink's processes are run at block rate rather than sample rate, due to the differences in engine designs.  (Pink users may set \*buffer-size\* to 1 to operate at sample rate.) The calculations for Pink's wait times are, however, sample accurate, with deterministic jitter to the size of the buffer. 

Processes, like any control function, may be added directly to a Pink engine or scheduled via event to process sometime within the future.  Since processes are synchronously processed, users may create control code that works both in realtime as well as ahead-of-time (i.e., when rendering to disk).  

## Example

The following code creates a process that loops 32 times.  Each iteration will create a random pitch, call perf-piano twice using that pitch at a major 5th apart, then wait 0.25 seconds until the next iteration of the loop.  

```clojure 
(add-pre-cfunc
  (process
    (loop [a 0]
      (when (< a 32)
        (let [pitch (+ 60 (* 12 (Math/random)))]
          (perf-piano 0 1 0.15 pitch) 
          (perf-piano 0 1 0.15 (+ 7 pitch))) 
        (wait 0.25)
        (recur (inc a))
        ))))
```

The example shows the add-pre-cfunc function used to add the process as a control function to the global pink.simple engine.  (The code is taken from src/demo/pink/demo/processes.clj).  

## Wait Types

The wait function is designed to operate using one of three argument types. The first is a numeric value in seconds and is calculated according the sample rate of the engine. Users wanting to use other time values, such as beats, should convert their values accordingly.  

The second argument type is a PinkSignal.  The pink.processes namespace provides two options that satisfy the PinkSignal protocol: cues, used for one-time signaling from one source to many processes, and latches, used for a single listener awaiting upon signals from multiple processes. These two signal types all for interprocess notification and can be used to implement aleatoric processes (i.e., Lutoslawski 'ad libitum' writing).  An example of this is found in the source code for music-examples.processes, found within the music-examples project, where a conductor and multiple performance processes cue and signal each other to determine when new material should be performed. 

The third argument type is a predicate.  Users may supply an arbitrary predicate function.  The process will wait as long as the predicate returns truthy values (i.e., true) and the wait will complete when the predicate returns a falsey value (i.e., false, nil).  


## Implementation Notes

The pink process macro utilizes the ioc\_macros from [core.async](https://github.com/clojure/core.async/) to transform user code into a state machine. The state machine and its execution is wrapped into a function that operates according to the Pink control function convention of returning true if the process is still running, and false if it is complete. 

Many thanks to Timothy Baldridge and the rest of the core.async contributors for creating a wonderfully extensible library that could be reused for the purposes of programs like Pink.  
