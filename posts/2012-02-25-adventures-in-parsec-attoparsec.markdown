---
title: Adventures in Parsec - Making it fast with attoparsec
date: February 25, 2012
tags: parsec, haskell, attoparsec
---

### A year ago and today

As I previously mentioned I originally wrote those posts on simple use of Parsec over a year ago. When I finally got around to making my blog, I figured it wouldn't hurt to post them. I was impressed with the positive reaction, especially considering how specific my target audience is. You need to be comfortable with Haskell, but you haven't really used it to build anything yet. I was there once, and I found it hard to take that next step. I also got a lot of requests for examples of parsing more complex grammar, this may be something I'll do if I happen across a good subject. ß

Anyway, I originally aimed to make a longer series, but none of this is very fresh in memory. I dug up one of later versions of this script, and I figured I could write it up, for those interested!

### The need for speed

Log files tend to grow to huge proportions. In fact, the one I originally experimented with was over 2GB in size, more than enough to bring down the laptop I was working on at the time if I wasn't careful. A lot of times it did. Parsec is not intended to be fast, as much as it is easy to use and read.

__Enter [attoparsec][attoparsec].__

Attoparsec was designed instead to be fast, while still being usable. Written and maintained by [Bryan O'Sullivan][bos], the package is used just about everywhere (eg. [Snap Framework][snap], [Yesod Framework][yesod]). Attoparsec parsers look slightly different than Parsec's, so first I'm going to redefine the parsers I showed in [part 1][part1].

[attoparsec]: http://hackage.haskell.org/package/attoparsec-0.10.1.1
[bos]: http://www.serpentine.com/blog/