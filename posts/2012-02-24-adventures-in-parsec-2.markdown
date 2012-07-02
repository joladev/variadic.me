---
title: Adventures in Parsec - Part 2
date: February 24, 2012
tags: haskell, parsec
description: Beginner level post on how to use Parsec to parse a log file, adding command line
---

This is part 2 of a series of simple Haskell tutorials. See the first one [here][part1].

Now that we are able to parse log lines, let's turn it into a simple command line tool. We'll be working with IO and do notation now, so make sure you atleast know the basics of it. If you're unsure, check out [Learn You A Haskell][lyahio].

Looking at our use case, let's assume we have a single uncompressed log file. The file consists of lines, and our parser takes lines. So let's run it down, we need to:

1. Read the file.
2. Split it into lines.
3. Parse each line.
4. Show the results!

Step one is handled by `readFile`, which is in Prelude (that means it is automatically imported into each Haskell file). `readFile` has the type __IO String__. What this means to us is that it can't leave the context of IO. If you are confused by this, please go read the chapter linked above!

~~~~~{.haskell}
main = do
    file <- readFile "logfile.txt"
~~~~~

The `main` function, or action, is the starting place of any Haskell program. We start by binding the result of `readFile` of "logfile.txt" to the name __file__. The type of file is now __String__, which is exactly the type that `lines` from Prelude takes! However, while `readFile` returns in the IO monad, `lines` does not, so we use a let binding instead of `<-`.

~~~~~{.haskell}
main = do
    file <- readFile "logfile.txt"
    let logLines = lines file
~~~~~

So far so good! If you try to compile the file now you will get an error, because you can't have a let binding at the end of a do block. We'll be fixing this in a moment. But before that, let's look at `parse` from Parsec. I wont put the type of `parse` here, it looks awful scary. Fortunately, it's still quite easy to use. The basic usage is:

~~~~~{.haskell}
parse line "(test)" testLine
~~~~~

So it takes a __Parser__ of something, a title __String__ and the __String__ to be parsed. This line will return an __Either ParseError String__. However, in our use case we have a __[String]__, so we need to `mapM` (the monadic version of `map`) the `parse` function over the list of log lines.

~~~~~{.haskell}
main = do
    file <- readFile "logfile.txt"
    let logLines = lines file
    result <- mapM (parse line "(test)") logLines
~~~~~

The type of result is __[Either ParseError String]__. To extract it, let's use `either`, also in Prelude. `either` takes two functions, and in the case of a left will apply the first function, in the case of a right will apply the second function. We will need to map this as well, as we have a list of eithers. __ParseError__ is printable, so let's just use `print` as the function to use in both cases.

~~~~~{.haskell}
main = do
    file <- readFile "logfile.txt"
    let logLines = lines file
    result <- mapM (parse line "(test)") logLines
    mapM_ (either print print) result
~~~~~

There we are! This will print out each parse result, whether an error or the __LogLine__ data type we created in the first part. 

Without even really trying we have done something that can be pretty hard in a lot of languages, this script will run in constant space! Through the magic of laziness, each line read will be parsed, printed and then freed to be garbage collected without having to wait for its friends. That said, this is not an efficient implementation, and in a later tutorial we will look at how to optimize this.

**Edit: this post was translated to Japanese by Hiroyuki Fudaba and available is [here][japanese].**

[part1]: http://FIX
[lyahio]: http://learnyouahaskell.com/input-and-output
[hoogle]: http://www.haskell.org/hoogle/
[japanese]: http://delihiros.hatenablog.jp/entry/2012/06/12/185344
