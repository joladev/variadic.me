---
title: Adventures in Parsec - Part 1
date: February 24, 2012
tags: haskell, parsec
description: Beginner level post on how to use Parsec to parse a log file
---

### Introduction

This will be the first in a series of little Haskell "tutorials". Together we will re-write an old script of mine. You probably need to know a little bit of Haskell, the more the better, but if you are already an avid Haskeller the series might be too basic. 

If you want to learn the basics of Haskell I can warmly recommend [Learn You A Haskell][lyah]. This book bears reading more than once! And then, to learn more about how to build stuff; [Real World Haskell][rwh] is an amazing book to move on with, filled to the brim with practical exercises and advice (and I've used it for reference heavily while originally writing this script).

### The Case Study

The [Apache Web Server][apache2] is probably the world's most common web server, and like most web servers, it logs each request it handles with some information. Although big IT companies may be very good at using such information, for most of the rest of us the logs just lie there growing. Let's see if we can't extract some interesting information from them! The log format we're working with is the [Apache combined log format][combinedlog]. Here is an example line:

    192.168.1.80 - - [18/Feb/2011:20:21:30 +0100] "GET / HTTP/1.0" 503 2682 "-" "-"
    
If your logfile is formatted differently you will need to make some adjustments. The fields are, in order: IP, ident, user, date, request, code, bytes, referrer, user-agent. To make sure we can get out any and all interesting information, we will parse each of these values and collect them in a data type. Let's define it now!

~~~~~{.haskell}
data LogLine = LogLine {
      getIP     :: String
    , getIdent  :: String
    , getUser   :: String
    , getDate   :: String
    , getReq    :: String
    , getStatus :: String
    , getBytes  :: String
    , getRef    :: String
    , getUA     :: String
} deriving (Ord, Show, Eq)
~~~~~

We use record syntax to get accessors for free. We also let the compiler derive instances for the Ord, Show and Eq typeclasses, for convenience.

The next step will be the actual parsing of a line from our log. When I originally wrote this script in Python, I parsed the log lines using a regex. It took a lot of time to write and I kept finding bugs in it for as long as I used it. But Haskell has something better, instead of regular expressions we will use the [parser combinators][parsercombinators] of the [Parsec library][parsec]. The idea is to combine smaller parsers into larger, letting you step by step define what things are. I think it's easiest to learn by example.

### Parsec

If we look at our example log line above, we note that each value is separated by a space. However, some values have spaces in them, these are surrounded by quotes or square brackets. So those are the three types of values we need to parse. Let's start by defining the easiest one, the plain value.

~~~~~{.haskell}
plainValue :: Parser String
plainValue = many1 (noneOf " \n")
~~~~~

The signature can be read as the result of plainValue will be a String in the Parser monad. One of the benefits of Parsec being a monad is that we get do-notation, which we will be using in the other parsers. Coming back to the plainValue parser, it starts by using `many1` which in the [documentation][parseccombdoc] is described as taking a parser and applying it one or more times and finally returning a list of the values. In our case, what we want to do is keep consuming characters until we hit a space, because the Apache log line values are space delimited. For this we use `noneOf`, which will take a list of characters and consume one character, as long as that character is not in the supplied list.

Combined, `many1 (noneOf " \n")` will keep consuming characters until it hits a space or a newline. Great!

Our next problem are the two cases of the bracketed value and the quoted value. Both of these can have spaces in them, so we will have to write parsers for these special cases.

~~~~~{.haskell}
bracketedValue :: Parser String
bracketedValue = do
    char '['
    content <- many (noneOf "]")
    char ']'
    return content

quotedValue :: Parser String
quotedValue = do
    char '"'
    content <- many (noneOf "\"")
    char '"'
    return content
~~~~~

Here we see parsers written in do notation. Not only are they quite clean and simple, but they really do describe the formatting of our log lines! Let's look at `bracketedValue`: it starts by trying to consume a left bracket character. After that we bind the result of `many (noneOf "]")` in the name content. Like the earlier `many1 (noneOf " \n")` this will keep consuming characters until it comes upon one from the supplied list. Finally we consume one last character, the ending bracket, and use return to put our result into the Parser monad.

Now that we have covered the possible ways our values can look, we need to use these parsers to define how to parse a whole line. Since we want to collect each result into the LogLine data-type, we need to make sure to bind the result of every value. Let's give it a try!

~~~~~{.haskell}
logLine :: Parser LogLine
logLine = do
    ip <- plainValue
    space -- parse and throw away a space
    ident <- plainValue
    space
    user <- plainValue
    space
    date <- bracketedValue
    space
    req <- quotedValue
    space
    status <- plainValue
    space
    bytes <- plainValue
    space
    ref <- quotedValue
    space
    ua <- quotedValue
    return $ LogLine ip ident user date req status bytes ref ua 
~~~~~

Although slightly verbose, it should be quite easy to interpret this parser. Believe it or not, we're done. This will parse well formed Apache combined log style lines. Let's try it out on our example!

~~~~~{.haskell}
testLine = "192.168.1.80 - - [18/Feb/2011:20:21:30 +0100] 
	   \"GET / HTTP/1.0\" 503 2682 \"-\" \"-\""

main = case parse logLine "(test)" testLine of
            Left err  -> print err
            Right res -> print res
~~~~~

`parse` extracts the result of the parser on the input from the parsec monad and either gives us an error or the result. And the result should look something like this

~~~~~{.haskell}
LogLine { getIP     = "192.168.1.80"
        , getIdent  = "-"
        , getUser   = "-"
        , getDate   = "18/Feb/2011:20:21:30 +0100"
        , getReq    = "GET / HTTP/1.0"
        , getStatus = "503"
        , getBytes  = "2682"
        , getRef    = "-"
        , getUA     = "-"
        }
~~~~~

The example code file is available [here][examplecode].

**Edit: this post was translated to Japanese by Hiroyuki Fudaba and available is [here][japanese].**

[lyah]: http://learnyouahaskell.com/
[rwh]: http://book.realworldhaskell.org/read/
[apache2]: http://httpd.apache.org/
[combinedlog]: http://httpd.apache.org/docs/2.2/logs.html#combined
[parsercombinators]: http://en.wikipedia.org/wiki/Parser_combinator
[parsec]: http://hackage.haskell.org/package/parsec
[parseccombdoc]: http://hackage.haskell.org/packages/archive/parsec/3.1.1/doc/html/Text-Parsec-Combinator.html
[examplecode]: https://gist.github.com/1899836
[japanese]: http://delihiros.hatenablog.jp/entry/2012/06/12/174635
