---
title: Flattening Callbacks Using Promises
date: Aug 19, 2013
tags: javascript, promises, jquery
description: Flatten callbacks using promises and gain exception management and readability.
---

Lately I've been working on some async heavy client side code and saw it as a great opportunity to get acquainted with Promises. The project I work on already has jQuery which includes an implementation and so it became the obvious choice.

The simplest and initially most convincing use case is that of flattening deeply nested callback "stairs".

~~~~~{.javascript}
getUsername(function (username) {
  getSubscribedCategories(username, function (categories) {
    getSuggestedArticles(categories, function (articles) {
      console.log("Great success!");
      console.log(articles);
    });
  });
});
~~~~~

Before we get into it, is this even a problem? I'd have to argue yes. First of all, error management is just about impossible since none of the asynchronous methods ever throw an exception, only the callbacks have exceptions, and that's only after the original context is out of scope. Catching these exceptions is nigh on impossible without leaking much of the abstractions. Callbacks break the natural exception bubbling concept. As I write this article a post on the dangers of callbacks is featuring on Hacker News, I suggest reading [it for more information][callbackgoto].

To solve this problem, much of the async code in NodeJS libraries instead use a primary error parameter on callbacks.

~~~~~{.javascript}
getUsername(function (error, username) {
  if (error) return handleError(error); // defined somewhere else
  getSubscribedCategories(error, username, function (error, categories) {
    if (error) return handleError(error);
    getSuggestedArticles(error, categories, function (error, articles) {
      if (error) return handleError(error);
      console.log("Great success!");
      console.log(articles);
    });
  });
});
~~~~~

This only complicates the code, adding manual error management in three distinct places. Let's see what it would look like if the functions, instead of taking callbacks, would return promises.

~~~~~{.javascript}
getUsername()
.then(function (username) {
  return getSubscribedCategories(username);
}, handleError)
.then(function (categories) {
  return getSuggestedArticles(categories);
}, handleError)
.then(function (articles) {
  console.log("Great success!");
  console.log(articles);
}, handleError);
~~~~~

Take a moment to look at that. If you're new to deferred objects and promises, you're probably not convinced. It takes some getting used to, but . Right off the bat you can see there is no longer an error parameter, and no need to check for it. Instead we hand `handleError` as a secondary callback to `then`. We have succesfully removed a significant source of distraction in the code. In the traditional NodeJS style code above we had to mix error handling into our function specific code. Even worse, we had to write this over and over again! `if (error)` does not belong there. Especially since the function that takes the callback has already done error management and passed us the potential failure, forcing us to write error checking again, violating "Don't Repeat Yourself".

But there are still improvements to be made! The sharp eyed will notice that several of our anonymous functions add no actual value. We can rewrite it to be even simpler.

~~~~~{.javascript}
getUsername()
.then(getSubscribedCategories))
.then(getSuggestedArticles))
.then(function (articles) {
    console.log("Great success!");
    console.log(articles);
}, handleError);
~~~~~

Apart from dropping the anonymous functions, we now only pass the error handling in the last call to `then`. This is not a typo. One of the greatest things about promises is that they recover the effect of exception bubbling (or something quite like it). In the case of jQuery deferreds, at least the rejected promise will bubble and reject subsequent promises. This essentially means that if the first promise is rejected, __all following promises are rejected too__. Not only that, they are rejected with the original reason. Errors are thus preserved all the way to the end of the chain.

## Conclusion

As you can see, promises are a reasonably simple thing to use. We went from an unsightly mess of callbacks with no error management to a resonably flat and surprisingly readable error managed alternative. Reading it out loud actually explains it reasonably well: get the username, then get the subscribed categories, then get the suggested articles and then print them. If something goes wrong, handle it. We've written something that mimics synchronous code, which makes it much easier to reason about its behavior.

jQuery's implementation of promises is not the ideal example of the [Promises/A+][promises] specification but usable nonetheless. If you are interested in alternatives, take a look at Kris Kowal's [Q.js][qjs], which does handle exceptions.

__As always, if you have any thoughts, ideas or comments, please don't hesitate to email me at <erik@variadic.me>.__

[callbackgoto]: http://tirania.org/blog/archive/2013/Aug-15.html
[promises]: http://promises-aplus.github.io/promises-spec/
[whenimplement]: https://github.com/jquery/jquery/blob/master/src/deferred.js#L96
[qjs]: https://github.com/kriskowal/q
