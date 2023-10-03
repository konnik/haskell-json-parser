# Haskell JSON parser (from scratch)

This is a very simple recursive decent parser for JSON I hacked together in Haskell in an afteroon just for the fun of it.

The developer experience of using Haskell for this kind of programming task is second to none, at least if you have a basic grasp of type classes like Functor, Applicative, Alternative and Monad.

I have not made any attempt to make this parser perform well, as a matter of fact I have not even benchmarked it, so it probably is pretty slow. 

And as you can tell the error reporting can be improved a lot. Right now you don't get any information on why a parse failed. 

This is the third JSON parser I've coded (the others where made in Kotlin and Clojure) and
this one was by far the most fun to do. Haskell really is a beautiful language.

Happy parsing!!!
