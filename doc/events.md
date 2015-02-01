# Events

## Introduction

In Pink, events are considered timed applications of functions. An event is fired by calling a given function at a given time with given arguments. The event function in pink.events has the following arguments:

```clojure 
f start & args
```

For example, the following:

```clojure
(event horn 0.0 0.4 440.0)
```

would create an event that calls horn at time 0.0 with arguments 0.4 and 440.0. However, the event processor in Pink is designed only to apply the function, and has no knowledge of what the function does, and does not in and of itself do anything with the results of applying the function. (This may change to check for return values as success/failure; this part of the design is not yet resolved.)  

## Event Processing

Because the event processor does not concern itself with what the function does, the general responsibility of the action's meaning is inverted from other systems.  For example, in a MIDI processor, the processor would look at incoming data and decide based on the initial byte whether to start a new note, or modify some internal state. As a result, there is a fixed set of possible event actions.  To expand the kinds of events, one has to modify what kinds of messages the MIDI processor is able to understand, as well as change what information is in the event message.

Instead, Pink events rely on the message creator to determine what the action will be.  The event processor is only concerned with applying a function at a given time and nothing more. For example, given a MIDI note on message with note number 64 and velocity 127, the MIDI processor might read the message, determine that the channel maps to synthesizer-a, create a new instance of synthesizer-a, then add it to the engine's list of active audio-functions.  

In Pink, the responsibility is reversed. Instead of creating a message that maps to an action, the user embeds the action into the event itself. To achieve the previous example, a Pink event would have an f arg of engine-add-afunc. The args to the event would be what would be necessary to create an instance of synthesizer-a.  When the event is processed, the processor would fire the engine-add-afunc function, using the synthesizer-a instance that is created from the arguments given. 

Because the user engine user is in control of what happens at the given time, the core engine code can remain very small and simple, while at the same time being extremely expressive.  Pink provides the very basic mechanisms of events as well as convenience functions for commonly used actions. However, the user is not limited to any pre-determined notion of what can be done by an event, and is free to customize their events as they wish. 

## Higher Order Events

Events in Pink are higher order events, meaning that event arguments may themselves be functions. This capability at the event-level provides the same benefits as passing functions to functions does in higher-order functions.  On a musical level, this allows for more flexible designs of audio-functions as well as greater reuse.

For example, a violin is a string instrument.  It is often used by bowing it with a violin bow.  Performers can vary the speed and pressure of a bow while performing. Performers may also use other techniques, such as plucking the string, hitting the string with the back of the bow, and so on.  In all of these cases, the instrument itself has not changed, but rather the input into the instrument.  

In Pink, because an event is able to take in other functions, one can design an audio-function to take in arguments and pull values during the processing of the audio-function.  For example, rather than pass in a value for pitch, such as a frequency of 440 hz, one can pass in an audio function that will give time-varying values. This allows for an audio-function acting as an instrument to be re-used to perform any variety of ways the user likes. It also allows for the user to build up a library of audio-functions specifically for control and reuse them between instruments. 

### Special Event Notation

One problem that occurs with higher-order events is if a set of events was fixed and a user wanted to replay that set of events, the function instances that were used as arguments in the event may have already been used.  For example:

```clojure
(event horn 0.0 0.5 (env [0.0 440 0.5 880]))
```

In this event, and env unit-generator is used to vary the pitch from 440Hz to 880hz over a 0.5 second period.  On the first time an event was called, that env instance would be already created when horn was called. Everything would render fine that first time, but if the event was reused, the env instance would have been in a state where it had already rendered for 0.5 seconds.  

To mitigate this scenario, Pink uses a special apply!\*! operator.  If any IDeref values are given as arguments, Pink will first deref the value before applying the function.  For example, if pitch was an atom that held the value 440, the following:

```clojure
(event horn 0.0 0.5 @pitch)
```

Would always render a horn with pitch 440, even if the user reset! pitch to another value. With Pink's events, if you pass in just the IDeref:

```clojure
(event horn 0.0 0.5 pitch)
```

The value of pitch will be derefed before applying horn each time that event is run.  To solve the problem about the horn above, you can use the !\*! function which will wrap the given code in an IDeref.  So the following:

```clojure
(event horn 0.0 0.5 (!\*! env [0.0 440 0.5 880]))
```

Will always call (env [0.0 440 0.5 880]) and call the horn function with that each time that event is fired.


As a consequence of using apply!\*!, if you want to pass in an atom and want that atom itself to be passed in to the event function, you can use the !r! operator to wrap your atom. (!r! reads as a "reference argument".) For example:

```clojure
(def tempo (atom 60.0)
(event perf-func 0.0 0.5 (!r! tempo)
```

In general, if one is using higher-order events, it is likely one will want to use the !\*! function. The use of !r! will most likely come into play when doing temporal recursion with events (where an event performs some action, then schedules another event calling the same function).  In that scenario, it is useful to pass in some kind of reference like a tempo atom or done atom, such that while performing one can affect the recursive event stream. 
