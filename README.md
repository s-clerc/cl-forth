# Cl-Forth

This is an implementation of the [FORTH Standard: Core](http://forth-standard.org/standard/core/INVERT) in Common Lisp. It's primary aim is to familiarise myself with implementing a programming language.

Currently I have got most of the standard maths words working, function definitions working, conditional working and while loops working.

I'm working on implementing `do `...`loop`(`+`)s right now.

There also isn't any parsing, everything is parsed as a list so `: plus-three 3 + ;` needs to be written as `\: plus-three 3 + \;` and then passed to the `run` function.

## Commentary 

The implementation is characterised by the use of a macro which allows for many common words to be implemented in the same notation as in the documents, so in the docs it might say (n1 n2 -- n3) for the `+` word and the implementation would be `(n1 n2 -- (+ n1 n2))`.

In general many anaphoric macros are used for ease of implementation, although I'm a bit concerned now about the mantainability because of how many variables are anaphoric. Were I to implement it again, I would still include `with-state` but I would not include any macro which assumed the name of an argument (e.g. assuming the `control` variable existed and represented the control stack).

## Author

* Sam de Clerc (jaimelelait@icloud.com)

## Copyright

Copyright (c) 2020 Sam de Clerc (jaimelelait@icloud.com)

## License

Licensed under the GPLv3 (or later) License.
