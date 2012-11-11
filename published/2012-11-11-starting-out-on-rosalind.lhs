## Starting Out on Rosalind

[Rosalind][1] is a new project which is trying to teach bio-informatics.  It aims to do so by requiring that the student solve simple problems in the style of Project Euler, or the Google Code Jams.  Being a horrific slacker, I'll probably not keep up.

Still, the first given problem is simple enough that it shouldn't take long, so let's have a crack.

[1]: http://rosalind.info/

### The Problem

Given the string:

> exampleNucleotides = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"

Count the number of times each unique character appears, and present the counts in the order A C G T, such that for ``exampleNucleotides``, we get:

> exampleOutput = "20 12 17 21"

### The Solution

Haskell as a language encourages robust, correct code.  The type system is easy to use, and lets us protect ourselves from machine-gunning our own feet.  In the spirit of YAGNI though, for now, let's avoid using our greater powers, and just keep things simple.

While in the real world, we might need to deal with potentially corrupted input (for instance, not getting ``'A'``, ``'G'``, ``'C'`` or ``'T'``), or might need some internal form of representation for later reuse (``data Nucleotide = Adenine | Guanine | Cytosine | Thymine ``), we don't really need to worry about that now.

The basic solution to this is to fold across the input.  Folding consumes a list, one element at a time, using an accumulator function on each element.  The function takes two arguments: the last output of the function (the accumulated value) and the next list element.  In order to actually work, the fold function must also be presented with an initial value.

So, using ``foldl``, we can write something very straightforward:

> countNucleotides xs = foldl nucleotideCounter [0, 0, 0, 0] xs

Reminder: if we :t foldl in ghci, we can see what it takes:

```
Prelude> :t foldl
foldl :: (a -> b -> a) -> a -> [b] -> a
```

It takes 3 arguments: a function, an initial state of type `a`, and a list of `b`s.  The function takes an `a` and a `b` (the current state and a list element), and returns a new `a` (the next state).

Let's define our `nucleotideCounter`.  This is going to return a list of character counts, in the required order above: ``[A C G T]``:

> nucleotideCounter :: [Int] -> Char -> [Int]
> nucleotideCounter [a, c, g, t] 'A' = [a + 1, c, g, t]
> nucleotideCounter [a, c, g, t] 'C' = [a, c + 1, g, t]
> nucleotideCounter [a, c, g, t] 'G' = [a, c, g + 1, t]
> nucleotideCounter [a, c, g, t] 'T' = [a, c, g, t + 1]
> nucleotideCounter xs _ = xs

This handles the case where the character isn't A C G or T, but doesn't deal with lists of a different length.  Haskell will therefore throw a warning about our incomplete pattern match.  This is bad, but for such a simple problem where we have complete control over our input, doesn't really matter.

Now, let's make sure we can format the output according to the given example.  This is, again, ugly: something like ``intercalate`` from ``Data.List`` might be nicer way of expressing this, but in the interests of sticking to the prelude:

> formatOutput [a, g, c, t] = 
>   (show a) ++ " " ++ (show g) ++ " " ++ (show c) ++ " " ++ (show t)

And... let's check we're right:

> correct = formatOutput (countNucleotides exampleNucleotides) == exampleOutput 

If you fire this document up in GHCI, and inspect correct, you should see `True`.  This means we're ready to give this a try against real data!

### The Glue

Now, we're going to make this a Real Program(tm), by hooking up our main method.  It'll take our list of characters as input, and will print out the counts given above.

> main :: IO ()
> main = do
>   putStrLn "What are the characters?"
>   chars <- getLine
>   putStrLn $ formatOutput $ count chars
