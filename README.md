# Pink

A library for music systems development, written in Clojure.

## Introduction

This library provides the basis for developing music systems.  It is also designed so to scale to user needs, whether they are exploring and designing low-level signal processing algorithms, developing pre-written compositions, or creating interactive real-time systems. It offers a slim core engine designed to be highly customizable.   

Features include:

* 64-bit signal processing chain
* Functional Audio Signal Graph: Build up Audio Graphs using functional approach
* Clear Synchronization of time-aware functions with Control Functions
* Higher-order Events: Functions and events can be used as arguments to events 
* Use Clojure to extend the system: single language for all extension points to the system

For more information, please see the [Documentation](doc/intro.md).

## Installation

At the moment, installation requires cloning this repostory and installing it using 'lein install'.  (This instruction will be updated when a stable release is made available.)

## Mailing List

For questions, please consider joining the Pink Users mailing list [here](https://groups.google.com/forum/?hl=en#!forum/pink-users).

## Examples

Examples for using Pink are available in the [music-examples](http://github.com/kunstmusik/music-examples) project. 

## YourKit 

<img src="yourkit.png"/>

Many thanks to YourKit for granting an Open Source license.  Their software is exceptional for helping to diagnose memory and performance issues with Pink.

YourKit supports open source projects with its full-featured Java Profiler.YourKit, LLC is the creator of <a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and <a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>, innovative and intelligent tools for profiling Java and .NET applications.


## License

Copyright © 2014 Steven Yi 

Distributed under the Eclipse Public License, the same as Clojure.
