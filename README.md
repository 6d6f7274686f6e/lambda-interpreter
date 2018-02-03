# Lambda Calculus Interpreter

This is a Lambda Calculus Interpreter written in Haskell.

Lambda Calculus is a formal system for expressing computation based on
functions and applications. For more information, please visit
[Wikipedia](https://en.wikipedia.org/wiki/Lambda_calculus).

Although the parser is entirely finished and well-functioning,
the reduction algorithm is by no way flawless. Most lambda-terms will work,
but anything including the Y combinator or other infinite loops will never
terminate, due to a flaw in the way Applications of two or more lambda-terms is
reduced. There may also be some flaws in the alpha-renaming system. I am
currently reading a paper on an optimal lambda calculus reduction algorithm in
order to fix that.
