Design Pattern Templates in C++11
=================================

This project provides template implementations of common [Design
Patterns][gof-wiki] in C++11.  This is by no means a comprehensive list of
patterns, but as I find myself implementing these patterns, I'd like a single
repository to keep them all.

This is a re-invention of a previous project of mine based on C++98, which is no
longer available online.  Because I'm tired of implementing the same patterns
time and time again, I've decided to just open source my next implementation of
each under an MIT license that is available to everyone.  Current and future
employers win because I am not spending time implementing these again, and if I
find bugs I will push updates here.  Users win because they can just re-use what
I have.  I win because I don't have to write these patterns yet another time.
In short, everyone wins.

##Implemented Patterns

1. [Observer Pattern][observer-manual]. The Observer Pattern provides a
   mechanism by which observers can register to receive notifications from an
   observable object when certain events occur.  This implementation is based
   loosely on the Gang of Four description of this pattern.  More information
   is available on [wikipedia][observer-wiki].

More patterns will be added later, as I use them.

[gof-wiki]: http://en.wikipedia.org/wiki/Design_Patterns_(book)
[observer-manual]: https://github.com/nanolith/patterncpp/blob/master/docs/Observer.md
[observer-wiki]: http://en.wikipedia.org/wiki/Observer_pattern
