## Quick Solutions for Rosalind

It's been a busy week.  Sadly, that's meant I've not managed to push through as much of [Rosalind][1] as I'd've liked to... but luckily, problems [2][2] and [3][3] are reasonably straightforward.

While I haven't made a fancy [Literate Haskell][4] program like last time, these can be solved as follows.

[1]: http://rosalind.info
[2]: http://rosalind.info/problems/rna/
[3]: http://rosalind.info/problems/revc/
[4]: http://www.haskell.org/haskellwiki/Literate_programming

### RNA

Nice and straightforward.  We just want to replace ``'T'`` with ``'U'``.  Hackily:

```haskell

flipT :: Char -> Char
flipT 'T' = 'U'
flipT x   = x

main :: IO()
main = interact $ map flipT
```

The above, once compiled, can simply be called with something like ``cat input | RNA`` in a bash shell.


### REVC

This one's pretty simple too.  All we need to do is reverse the input, and swap any paired characters.  So:

```
nucleotideComplement :: Char -> Char
nucleotideComplement 'A' = 'T'
nucleotideComplement 'T' = 'A'
nucleotideComplement 'G' = 'C'
nucleotideComplement 'C' = 'G'
nucleotideComplement 'C' = 'G'
-- the following lets us deal with the trailing newline in the input
nucleotideComplement x = x

main :: IO ()
main = interact $ reverse . map nucleotideComplement
```

As with the last one: this can be called with ``cat input | REVC``, assuming you've called it REVC.

### Warning: WIP!

I should note that I intend to come back to this entry and flesh it out a bit.  I'm trying to force myself to blog once a week, so am publishing stuff that's not quite finished with the intention of keeping myself on track.  Hopefully I'll get a chance to revise this shortly!
