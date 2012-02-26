---
title: Adventures in Parsec - Making it fast with attoparsec
date: February 25, 2012
tags: parsec, haskell, attoparsec
description: Improving previous examples of Parsec using attoparsec and bytestrings
---

### A year ago and today

As I previously mentioned I originally wrote those posts on simple use of Parsec over a year ago. When I finally got around to making my blog, I figured it wouldn't hurt to post them. I was impressed with the positive reaction, especially considering how specific my target audience is. You need to be comfortable with Haskell, but you haven't really used it to build anything yet. I was there once, and I found it hard to take that next step. I also got a lot of requests for examples of parsing more complex grammar, this may be something I'll do if I happen across a good subject.

Anyway, I originally aimed to make a longer series, but none of this is very fresh in memory. I dug up one of the later versions of this script, and I figured I could write it up, for those interested! The full script is linked to at the bottom of the post.

### The need for speed

Log files tend to grow to huge proportions. In fact, the one I originally experimented with was over 2GB in size, more than enough to bring down the laptop I was working on at the time if I wasn't careful. A lot of times it did. Furthermore, Parsec is not intended to be fast as much as it is easy to use and read.

__Enter [attoparsec][attoparsec].__

Attoparsec really was designed instead to be fast, while still being usable. Written and maintained by [Bryan O'Sullivan][bos], the package is used just about everywhere (eg. [Snap Framework][snap], [Yesod Framework][yesod]). Attoparsec parsers look slightly different than Parsec's, so first I'm going to redefine the parsers I showed in [part 1][part1].

Step one is our `LogLine` data type. This time around we're going to be using ByteStrings, which are more efficient but a little less convenient than regular Strings.

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

Next step is defining the type of elements a log line can consist of. Like I said, this is going to look a little bit different from the previous version. Although I did look at the attoparsec `char` parser combinator, after some experimentation I found that manually defining them using `satisfy` actually resulted in a noticeable performance improvement (admittedly, anything is noticeable parsing a 2GB log file on a laptop). Still, defining them this way is quite simple.

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

`option` will leave the path and ua empty strings if the `combined` parser fails. Alright, so far so good. But the fun is just about to start!

### Using the parser

Let's say we'd like to see the top 10 most common IPs to request something from our web server (make up your own reason). Arguably then we could write a faster parser that only recovers the IPs, but the benefit of doing it this way is we can easily extend our script to show other interesting information, based on other things than IPs (such as most common referrer, total number of bytes transmitted according to log etc). What we want to end up with is something like this:

	1: "12.13.14.15", 11503
	2: "65.213.321.123", 10586
	3: "71.225.142.44", 9563
	4: "223.28.111.143", 8075
	5: "89.955.133.69", 8062
	6: "87.212.53.129", 4998
	7: "13.229.11.142", 4645
	8: "88.223.95.189", 3885
	9: "12.629.125.116", 3655
	10: "118.38.22.448", 3484

Rank, IP, number of occurances. To get there, first we need a function that takes a bunch of loglines and gives us a data structure with each IP and the number of times it occured. To me this sounds like a perfect job for the map datastructure. Of the common implementations, Data.HashMap is among the most efficient, and because too much laziness can be bad for you, we're using the strict implementation of it.

~~~~~{.haskell}
-- I include the imports here so you can see what the qualifiers in front of functions stand 
-- for

import qualified Data.Attoparsec.Lazy as AL
import qualified Data.ByteString.Char8 as S
import qualified Data.HashMap.Strict as M

countIPs :: [L.ByteString] -> [(S.ByteString,Int)]
countIPs = M.toList . foldl count M.empty
    where
        count acc l = case AL.maybeResult $ AL.parse line l of
            Just x  -> M.insertWith (+) (S.copy (getIP x)) 1 acc
            Nothing -> acc
~~~~~

This function is actually pretty simple, but that fact may be partly obscured by the transformation of lazy to strict bytestrings (not to mention all those qualifiers being pretty unattractive). It's a left fold, takes a list of lazy bytestrings and returns an [associative list][assoclist] of strict bytestrings and ints. The big thing to notice here is the copying of the bytestring using `S.copy`. When creating a map that includes a part of a bytestring loaded into memory, the whole bytestring will be kept in memory for as long as the map is! That is, even if I take out the IP from the log line, the whole line is kept in memory. In fact, the script would originally not run in constant space, and no amount of strictness would change that! Copying the bytestring is an expensive operation, but frees up the log line to be garbage collected.

Ok, if you've followed me this far you can probably figure this last part out. It's just the good old reading file, applying function, printing result.

~~~~~{.haskell}
main :: IO ()
main = do
  [cmd,path] <- getArgs
  dispatch cmd path

-- Looks up command in the list of actions, calls corresponding action.
dispatch :: Command -> FilePath -> IO ()
dispatch cmd path = action path
    where
        action = fromMaybe err (lookup cmd actions)
        err    = \_ -> putStrLn $ "Error: " ++ cmd ++ " is not a valid command."
                    
-- Associative list of commands and actions.
actions :: [(Command, FilePath -> IO ())]
actions = [("ips", mapToTopList countIPs)
	   -- add your own actions here
	  ]

-- Helper that turns a map into a top list, based on the second value.
mapToTopList :: ([L.ByteString] -> [(S.ByteString, Int)]) -> FilePath -> IO ()
mapToTopList f p = do
    file <- liftM L.lines $ L.readFile p
    let mostPopular (_,a) (_,b) = compare b a
        m = f file
    mapM_ putStrLn . zipWith pretty [1..] . take 10 . sortBy mostPopular $ m

-- Helper for printing the top list.
pretty :: Show a => Int -> (a, Int) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n
~~~~~

The full code is available [here][codeexample], including one action that counts the total number of bytes transmitted in requests in the log file.

I also implemented this script using the map reduce module from [Real World Haskell][rwh], if there's interest I'll show that too. For questions or feedback, please email me at <erik@variadic.me>.

[attoparsec]: http://hackage.haskell.org/package/attoparsec-0.10.1.1
[bos]: http://www.serpentine.com/blog/
[snap]: http://snapframework.com/
[yesod]: http://www.yesodweb.com/
[part1]: /posts/2012-02-24-adventures-in-parsec.html
[assoclist]: http://neilmitchell.blogspot.com/2008/01/associative-lists.html
[codeexample]: https://gist.github.com/1908213
[rwh]: http://book.realworldhaskell.org/