---
title: Adventures in Parsec - Making it fast with attoparsec
date: February 25, 2012
tags: parsec, haskell, attoparsec
---

### A year ago and today

As I previously mentioned I originally wrote those posts on simple use of Parsec over a year ago. When I finally got around to making my blog, I figured it wouldn't hurt to post them. I was impressed with the positive reaction, especially considering how specific my target audience is. You need to be comfortable with Haskell, but you haven't really used it to build anything yet. I was there once, and I found it hard to take that next step. I also got a lot of requests for examples of parsing more complex grammar, this may be something I'll do if I happen across a good subject.

Anyway, I originally aimed to make a longer series, but none of this is very fresh in memory. I dug up one of later versions of this script, and I figured I could write it up, for those interested!

### The need for speed

Log files tend to grow to huge proportions. In fact, the one I originally experimented with was over 2GB in size, more than enough to bring down the laptop I was working on at the time if I wasn't careful. A lot of times it did. Parsec is not intended to be fast as much as it is easy to use and read.

__Enter [attoparsec][attoparsec].__

Attoparsec really was designed instead to be fast, while still being usable. Written and maintained by [Bryan O'Sullivan][bos], the package is used just about everywhere (eg. [Snap Framework][snap], [Yesod Framework][yesod]). Attoparsec parsers look slightly different than Parsec's, so first I'm going to redefine the parsers I showed in [part 1][part1].

Step one is our LogLine data type. This time around we're going to be using ByteStrings, which are more efficient but a little less convenient than regular Strings.

~~~~~{.haskell}
data LogLine = LogLine {
    getIP     :: S.ByteString,
    getIdent  :: S.ByteString,
    getUser   :: S.ByteString,
    getDate   :: S.ByteString,
    getReq    :: S.ByteString,
    getStatus :: S.ByteString,
    getBytes  :: S.ByteString,
    getRef    :: S.ByteString,
    getUA     :: S.ByteString
} deriving (Ord, Show, Eq)
~~~~~

Next step is defining the type of elements a log line can consist of. Like I said, this is going to look a little bit different from the previous version. Although I did look at the attoparsec `char` parser combinator, and I found that manually defining them using `satisfy` actually resulted in a noticeable performance improvement (admittedly, anything is noticeable parsing a 2GB log file on a laptop). Still, defining them this way is quite simple.

~~~~~{.haskell}
quote, lbrack, rbrack, space :: Parser Char
quote  = satisfy (== '\"')
lbrack = satisfy (== '[')
rbrack = satisfy (== ']')
space  = satisfy (== ' ')
~~~~~

And here's our `val`, `quotedValue` and `bracketedValue`:

~~~~~{.haskell}
plainValue :: Parser S.ByteString
plainValue = takeTill (== ' ')

quotedValue :: Parser S.ByteString
quotedValue = do
    quote
    res <- takeTill (== '\"')
    quote
    return res

bracketedValue :: Parser S.ByteString
bracketedValue = do
    lbrack
    res <- takeTill (== ']')
    rbrack
    return res
~~~~~

We see some differences. `plainValue = many1 (noneOf " \n")` has turned into `plainValue = takeTill (== ' ')`. The documentation for attoparsec recommends using `takeWhile`/`takeTill` instead of `many1` for performance reasons. Previously we assumed a certain format for our parser. My personal use case changed, however, and I adjusted my script to also handle logs without the last two elements. To do this I broke out the parsing of the last two parts, and as you will see later, I used the parser `option` to handle the optional nature of these elements.

~~~~~{.haskell}
combined :: Parser (S.ByteString,S.ByteString)
combined = do
    space
    path <- quotedValue
    space
    ua <- quotedValue
    return (path,ua)
~~~~~

And so here it is, the log line parser that handles both the "standard" and combined Apache log formats. Before we get too excited I should point out that log formatting for Apache web server is done in a configuration file, and can very well be different for each machine you look at (depending on the sysadmin's preferences)!

~~~~~{.haskell}
line :: Parser LogLine
line = do
    ip <- plainValue
    space
    identity <- plainValue
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
    (path,ua) <- option ("","") combined
    return $ LogLine ip identity user date req status bytes path ua
~~~~~

[attoparsec]: http://hackage.haskell.org/package/attoparsec-0.10.1.1
[bos]: http://www.serpentine.com/blog/
[snap]: http://snapframework.com/
[yesod]: http://www.yesodweb.com/
[part1]: http://variadic.me/posts/2012-02-24-adventures-in-parsec.html