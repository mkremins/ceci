# Ceci

Ceci is a self-hosting ClojureScript compiler. It turns Clojure code into JavaScript and will eventually be capable of compiling itself.

Differences from [`cljsc`](https://github.com/clojure/clojurescript):

* Ceci has "real" macros, defined inside ordinary source files (rather than segregated from the rest of your code) and transparently usable via ordinary `:require`.
* Ceci gives you access to `eval` at runtime.
* Ceci doesn't use the Google Closure Compiler for minification and dead code elimination. As a result, Ceci-emitted JS is usually "heavier" than the output of `cljsc` for the same code.

## License

Copyright © 2014 Max Kreminski.

Distributed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html), just like Clojure(Script).
