# Events

## Introduction

In Pink, events are considered timed applications of functions. An event is fired by calling a given function at a given time with given arguments. The event function in pink.events has the following arguments:

> f start & args

For example, the following:

> (event horn 0.0 0.4 440.0)

would create an event that calls horn at time 0.0 with arguments 0.4 and 440.0. However, the event processor in Pink is designed only to apply the function, and has no knowledge of what the function does, and does not in and of itself do anything with the results of applying the function. (This may change to check for return values as success/failure; this part of the design is not yet resolved.)  

## Event Processing

Because the event processor does not concern itself with what the function does, the general responsibility of the action's meaning is inverted from other systems.  For example, in a MIDI processor, the processor would look at incoming data and decide based on the initial byte whether to start a new note, or modify some internal state. As a result, there is a fixed set of possible event actions.  To expand the kinds of events, one has to modify what kinds of messages the MIDI processor is able to understand, as well as change what information is in the event message.

Instead, Pink events rely on the message creator to determine what the action will be.  The event processor then is only concerned with applying a function at a given time and nothing more. So for example, given a MIDI note on message with note number 64 and velocity 127, the MIDI processor might read the message, determine that the channel maps to synthesizer-a, create a new instance of synthesizer-a, then add it to the engine's list of active audio-functions.  

In Pink, the responsibility is reversed. Instead of create a message that maps to an action, the user embeds the action into the event itself. To achieve the previous example, a Pink event would have an f arg of engine-add-afunc. The args to the event would be what would be necessary to create an instance of synthesizer-a.  When the event is processed, the processor would fire the engine-add-afunc function, using the synthesizer-a instance that is created from the arguments given. 

Because the user engine user is in control of what happens at the given time, the core engine code can remain very small and simple, while at the same time being extremely expressive.  Pink provides the very basics mechanism of events as well as convenience functions for commonly used actions. However, the user is not limited to any pre-determined notion of what can be done by an event, and is free to customize their events as they wish. 

## Higher Order Events

Events in Pink are higher order events, meaning that event arguments may themselves be functions. Functions are passed in both as the primary function to be called at the designated time, but also as arguments to that primary function. (...)

