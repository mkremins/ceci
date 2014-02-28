# Clueless
Clueless is an attempt at creating a self-hosting ClojureScript compiler (i.e. a ClojureScript compiler written in ClojureScript and capable of compiling itself) from the ground up.

## Why?
There are three ways to run Clojure in the browser at the moment, and none of them are quite what I'm looking for.

* The [main ClojureScript codebase](http://github.com/clojure/clojurescript) is by far the most mature and versatile Clojure->JS compiler out there. Unfortunately, it isn't self-hosting (you need a JVM instance somewhere to run it), and it doesn't really offer any usable representation of namespaces, vars or macros at runtime.
* [kanaka](http://github.com/kanaka)'s initally-promising effort to build a [self-hosting fork](http://github.com/kanaka/clojurescript) of the main ClojureScript codebase seems to have ground to a complete halt, and it's starting to show definite signs of bitrot.
* [Gozala](http://github.com/Gozala)'s [wisp](http://github.com/Gozala/wisp) – another attempt at building a self-hosting flavor of Clojure for the browser from the ground up – changes too much of the core Clojure behavior to be entirely to my liking, and has a focus on producing human-readable JavaScript as output that I neither need nor want.

## Yeah, but... why?
Clojure is the sanest programming language I've ever worked with; JavaScript is the most versatile. A proper combination of the two, with the complete Clojure environment available at runtime, would be an excellent platform for building extensible, intelligent, cross-platform tools with human-friendly and easily hackable UIs.

## You still haven't answered the question. Why?
Because I'm too clueless to realize that I've taken on an impossible task.
