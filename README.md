# Lambda Calculus Interpreter

This is a Lambda Calculus Interpreter written in Haskell.

Lambda Calculus is a formal system for expressing computation based on
functions and applications. For more information, please visit
[Wikipedia](https://en.wikipedia.org/wiki/Lambda_calculus).

For help and information on rules for this specific interpreter, read `help.txt`
or type `:h` in the REPL.

Although the parser is entirely finished and well-functioning, the reduction
algorithm still may have some flaws. Most lambda-terms will work,but there may
still be some flaws in the alpha-renaming system. I am currently reading a paper
on an optimal lambda calculus reduction algorithm in order to fix that
uncertainty.
