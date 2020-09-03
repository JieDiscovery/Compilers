# Programming Language Technology

This is a repo of labs during the course DAT151 Programming Language Technology at [Chalmers](https://www.chalmers.se/en/Pages/default.aspx) which aimes to give understanding of how programming languages are designed, documented, and implemented. In this repo, you will have a deeper view about *Compilers* and *Interpreters* work step by step.

# Introduction

This repo mainly contains four parts:
* Paser
* TypeChecker and Interpreter
* Code Generation
* Functional Language Interpreter

## [1. Parser](http://www.cse.chalmers.se/edu/year/2019/course/DAT151_Programming_Language_Technology/laborations/lab1/index.html)
In this part, we wrote a parser for a fragment "C--" of the C++ programming language which could return an abstract syntax tree at success, and report an error with a line number at failure.

Test suite is given in this sub-repo so that you could compile the test program.

## [2. TypeChecker and Interpreter](http://www.cse.chalmers.se/edu/year/2019/course/DAT151_Programming_Language_Technology/laborations/lab2/index.html)
The part contains how to write a type checker and an interpreter for the fragment "C--" of the C++ programming language in [part 1](/Part I.Paser). The type checker could check the program and send it to the interpreter at success. The interpreter could run the program and correctly perform all its input and output actions. At type checking failure, a type error should be reported.

Test suite is offered as well.

## [3. Code Generation](http://www.cse.chalmers.se/edu/year/2019/course/DAT151/laborations/lab3/index.html)
The part is about how to build a code generator from a fragment C-- of the C/C++ programming language to JVM, Java Virtual Machine which could produce Java class files and can be run in the Java bytecode interpreter so that they correctly perform all their input and output actions.

## [4. Functional Language Interpreter](http://www.cse.chalmers.se/edu/year/2019/course/DAT151/laborations/lab4/index.html)
The objective of this part is to present how to write an interpreter for a small, untyped functional programming language which is a tiny subset of Haskell. The interpreter could walk through programs and print out the value of the main function.


## Copyright declaration

Basic structure of code in this repo is provided by the teaching group of course [DAT151](http://www.cse.chalmers.se/edu/year/2019/course/DAT151/) at Chalmers, there will be an identifier in each file to present their copyrights.



