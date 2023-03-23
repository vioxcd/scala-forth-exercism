# Very Simple Subset of Forth

This is my solution to [Scala's Forth exercism track](https://exercism.org/tracks/scala/exercises/forth)

The task is to *"implement an evaluator for a very simple subset of Forth"*

It's a really fun exercise, as this is probably my first time implementing a programming language-related things, using a language I'm not entirely familiar with, and in a paradigm I'm still trying to grasp (both OO and FP).

Anyway, I learned a little bit of Forth and several things about programming:

- How the structure of code is guided by tests. The tests in this exercise are lovely and well-crafted! It made me alter the structure of my code from time to time to adjust
- Putting the state inside a class which holds the state in private property and exposes only public methods for altering the state (although it's imperfect because I expose my stack to the outside world)
- Traits and hiding implementation details. I was having a hard time dealing with the `ForthEvaluatorState` trait, and only later figured out something along the lines of "when *passing an argument* with certain trait to a function, it means the argument are *constrained* in what it can do within the function; on the other hand, when *returning* a trait from a function, it only need to *satisfy the traits specification*, and is *free* to do/put whatever in the class/object returned." I think this is very powerful, as I can return my `State` class knowing full-well it doesn't matter what other properties or methods it has because what's going to get "called" is what's specified in the traits `ForthEvaluatorState`

Caveat: I don't use the `Definition` class that is given in the `ForthEvaluator.scala` file. Not sure how I could make it work with that

Other references I found useful:

- [Easy Forth](https://skilldrick.github.io/easyforth/)
- [Learn X in Y Minutes: Forth](https://learnxinyminutes.com/docs/forth/)
- [Scala Specialization Notes](https://github.com/xiaoyunyang/coursera-scala-specialization). I took a peek at how "functional programming but with `var`" is implemented
- Scala's [docs](https://docs.scala-lang.org/scala3/book/introduction.html), of course
