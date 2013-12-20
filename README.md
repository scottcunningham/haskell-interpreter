

In this project you will extend the programming language interpreter that we have developed in class to incorporate some new features. You should start with the version of the interpreter attached to this page.
The project has several parts; you should complete as many as you can. Submit a separate Haskell source file (or files) for each part.

1. (5 marks). Add an input keyword to the programming language that will allow the user to enter an integer value (naturally, it should generate an exception in the interpreted program if the input parse fails -- you can use Haskell's read function to do the parsing)

2. (5 marks). Provide a primitive interpreter for the language that reads a source file and executes the program contained in it. You can use automatically derived read instances to do the parsing, and no special error messages are needed if a parse fails. Running the program should produce all the programs output on stdout and a dump of the final variable store on stderr.

3. (15 marks). Extend the interpreter so that there is a new Fork construct that starts a second interpreter. Model this on the While statement that is currently in the language so that your new construct contains a block of code that will form the new thread of execution (use Haskell's ForkIO to implement this). Variable access should be atomic in the interpreter; you should not bother adding any locking concepts in the language for this part, because the next part will take care of that.

4. (20 marks). Now add the notion of a "critical section" to the program by introducing a new keyword "synchronized", which contains a block. The block should have exclusive access to all the variables read and written in that section. Your interpreter will have to analyse the parse tree of the block and find which variables are referenced. You should use Haskell's STM to implement this (this may require some changes to the design of the variable store).

5. (5 marks). Finally, you can add some static analysis to the interpreter to report on errors before the program is run. Add one of the following, or something you decide on yourself (note: this section is potentially very time consuming if you decide to do it very thoroughly -- note how many marks are available for it, and adjust your effort accordingly!)
    - Add a check for uninitialised variable references, and report to the user when the analyser spots a code path in the program that might be reading from a variable before it is written to
    - Have the interpreter check for unreachable code (i.e. an "else" that could never be reached because the corresponding condition is a tautology
    - Have the interpreter check for unused variables (variables which have been assigned to but never read)