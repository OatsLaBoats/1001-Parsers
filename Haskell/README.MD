The fact that there is no proper record type is annoying

The lsp is way to eager to give suggestions even from modules that are not imported.

lazyness do be going crazy doe

Shadowing is not well implemented and can often cause bugs

monads are dope

when you are dealing with a situation where you need to accumulate errors it becomes really weird to work with
the language since it has no loops and mutation. Its no crazy and definetly possible. but for sure a little annoying.
This mostly came up during the analyzer where I often need to accumulate errors alongside building a new stmt list.
In the parser for example this is not a problem because errors there don't occumulate so we can just use the Either monad.

There is a lot that I don't know, it almost feels like an endless abyss of knowledge but honetly its pretty fun.