# Processes 

## Introduction

Processes are a way to create Pink control functions using code that "feels" like writing multi-threaded code. The process macro from pink.processes is used to create the process and is most often used in conjunction with the wait function.  The wait function pauses the operation of the process for the given time in seconds.  

Processes, like Chuck's Shreds, are run synchronously with the engine.  However, unlike Chuck, Pink's processes are run at block rate rather than sample rate, due to the differences in engine designs.  (Pink users may set \*buffer-size\* to 1 to operate at sample rate.) The calculations for Pink's wait times are, however, sample accurate, with deterministic jitter to the size of the buffer. 

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

## Implementation Notes

The pink process macro utilizes the ioc\_macros from [core.async](https://github.com/clojure/core.async/) to transfrom user code into a state machine. The state machine and its execution is wrapped into a function that operates according to the pink control function convention of returning true if the process is still running, and false if it is complete. 

Many thanks to Timothy Baldridge and the rest of the core.async contributors for creating a wonderful library that could be reused for the purposes of programs like Pink.  
