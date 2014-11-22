# Unit Generators

Unit Generators are composable units of signal processing. The term comes from Max Matthews and was introduced in Music 3. In a 2011 interview with Geeta Dayal[1], Matthews says:

"MUSIC 3 was my big breakthrough, because it was what was called a block diagram compiler, so that we could have little blocks of code that could do various things. One was a generalized oscillator … other blocks were filters, and mixers, and noise generators."

Th system of Unit Generators has had a great influence on computer music software since its introduction. The design of Unit Generators differs depending on the context of the system they're in.  Some have added capabilities such as the ability to accept messages, exposure of variables for runtime modification, and reinitialization.  The design of Unit Generators in Pink is simpler, as the features that those capabilities are used to support can be supported by other means.  (This is largely due to the use of higher-order programming that is available in Lisps and Clojure.) 

## Lifecycle of Unit Generators

* Allocation
* Initialization
* Performance
* Deallocation

### Allocation



### Initialization



### Performance



### Deallocation

## Pink Unit Generators


[1](http://blog.frieze.com/max-mathews)
