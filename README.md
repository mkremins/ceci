# Ceci

> Ceci n'est pas une ClojureScript.

Ceci is a self-hosting ClojureScript compiler. It turns Clojure code into JavaScript and is capable of compiling itself.

Wherever possible, Ceci aims to output JS that's fully compatible with that of the [original ClojureScript compiler](http://github.com/clojure/clojurescript). However, there are a few differences:

* Ceci expects macros to be written in ClojureScript, not JVM Clojure. You can include macros in your ordinary source files; Ceci doesn't mind.
* Ceci gives you access to `eval` at runtime.
* Ceci doesn't run emitted JavaScript through the Google Closure Compiler for dead code elimination, minification and the like. As a result, Ceci-emitted JS is usually "heavier" than the equivalent output of cljsc.
