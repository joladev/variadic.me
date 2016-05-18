---
title: Taking Promises Apart
date: Aug 22, 2013
tags: javascript, promises
description: Building your own Promises/A implementation for educational purposes.
---
Fueled by my recent interest in both jQuery's and Q.js' implementations of the Promises concept I set out to make one on my own, albeit a lot simpler. In fact, I limited myself to only the basic idea of the original [Promises/A][promisesa] specification. In summary it only states that a promise is something that has a method `then` which take two callbacks, one to be evaluated on success and the other on failure, and returns a promise which, as each call to `then` returns a promise, will allow chaining of functions and or further promises. It also implies that eventually it will resolve or reject the promise and evaluate callbacks depending on which. In other words, something that is a promise acts like this:

~~~~~{.javascript}
promiseReturningFunction()
.then(alsoReturnsPromise)
.then(function (value) {
  // a regular function used to chain
  // will automatically be wrapped in a promise
  return value;
})
.then(console.log);
~~~~~

But that's just half of it, because the promise can also fail and failures work differently. In order to preserve standard __error__ behavior we have to automatically reject all subsequent chained promises, with the original error message. If an exception happens in `promiseReturningFunction` we want it to travel all the way down the chain, and without evaluating any of the other callbacks in the chain. Chaining functions implies that they each rely on the success of the previous one, or we would not need to chain them in the first place.

When you think about it, that's all you need to implement the basic promises functionality. Nowhere close to the usability of [Q.js][qjs] or others, but still. Promises.

__Note that the [Promises/A][promisesap] has been superceeded by [Promises/A+][promisesap] which clarifies and extends the vague and confusing original specification.__

## Rolling our own

This is not a serious attempt at building a real competitor to the incumbents, this is just for educational purposes. If you just want to take a look at the code, here's a [gist][deferjs].

Ok, let's flesh this out!

Unintuitively, although the concept is called Promises, the main object is commonly referred to as the _deferred object_. In jQuery, you create a deferred object using `$.Deferred()`, in Q.js using `Q.defer()` and in AngularJS `$q.defer()`. Microsoft tried their hand at promises in the [WinJS implementation][winjs] distributed with Metro (or Modern UI) apps, but got absolutely confused and ended up with `new WinJS.Promise(init, oncancel)`.

We forego namespacing and simply create a function called `defer`.

~~~~~{.javascript}
var defer = function () {
  // our implementation
};
~~~~~
 
Thinking back to our definition of a promise, we need to be able to `reject` or `resolve` it. Essentially, when our deferred computation has completed or failed, we evaluate the appropriate handlers with the result of the deferred computation. We also need to be able to pass a `promise`, allowing calling code to assign handlers for success and failure. Think of the deferred object/promise as a reference to a computation that will, at some point, finish. And in order to do something once it finishes, whether success or failure, we need to assign it work to do on completion. This means we need `then`.

~~~~~{.javascript}
var defer = function () {

  // our implementation
  
  var promise = {
    then: then
  };

  return {
    resolve: resolve,
    reject: reject,
    promise: promise,
    then: then
  };
};
~~~~~

A deferred object can be resolved or rejected, and once it has reached one of those states, it can also hold a result value. It will also hold handlers acquired through `then`. So we need variables to represent this. For brevity, I will start omitting code we've already been through. To see the full implementation, take a look at the [gist][deferjs].

~~~~~{.javascript}
var resolution,
    value,
    handlers = {};
~~~~~

We are now missing `reject`, `resolve` and `then`. I found the last to be the most intuitive way forward. To write it however, we have to consider the first code snippet in this post. The parameters of `then` can take many forms. In our implementation we will handle three cases.

1. A function that returns a promise
2. A function that returns a value
3. Undefined

And each of those cases applies both to error and success handlers. What I will show you is a naive attempt at covering all the cases. Also, remember that even though the callback handed to `then` can be non-promises, in order to allow chaining `then` has to always return a promise.

~~~~~{.javascript}
var then = function (done, fail) {
  var deferred = defer();
  handlers.resolved = wrap(done, deferred);
  handlers.rejected = wrap(fail, deferred);
   
  fulfill();
   
  return deferred.promise;
};
~~~~~

I take special delight in the essentially recursive call to `defer`. As you can see, we actually use our own deferred object to bootstrap itself, in a fashion. Let me walk you through it. First we create a new deferred object to keep track of the state of the callback which is run by `then`. Secondly, we add done and fail to our handlers object, but "wrapped" with the deferred object. I will get into this shortly. The original deferred object might already be in a resolved or rejected state, so we call a function `fulfill` to optionally run the handlers. This will also be explained later.

Finally, we return a promise to the inner computation. 

Wrap is also an interesting JavaScript-esque piece of code. In order to resolve or reject our promise at some point in the future, using the handler, we pass the deferred object with it when we store it in the handlers object, through a technique called a [_closure_][closure].

~~~~~{.javascript}
var wrap = function (c, d) {
  return function () {
    c = c || function () {};

    if (resolution === 'rejected') {
      c(value);
      return d.reject(value);
    }

    var next;
    try {
      next = c(value);
    }
    catch (e) {
      return d.reject(e);
    }

    if (next) {
      if (next.then) {
        return next.then(function (resp) {
          return d.resolve(resp);
        }, function (err) {
          return d.reject(err);
        });
      }
    }

    return d.resolve(next);
  };
};
~~~~~

So `wrap` returns a new function which closes on the callback and deferred object. This gives us access to the correct promise object when we evaluate the handlers.

First up we make sure the handler exists. If it does not, we replace it with [_noop_][noop], a function which does nothing. Next, we make sure that, if our state is `'rejected'` we reject the promise which was returned earlier by `then`, with our value instead of the value returned by the handler in order to preserve the original error message. Of course, we still evaluate the error handler.

With that taken care of, we evaluate the handler inside a try catch, letting us grab any exception and causing a rejection of the promise.

Now that we have the result of the success handler in the `next` variable we really only have two cases left to handle, function returning value (or undefined) and function returning a promise. The first case is so simple I used it as an end case, or catch all. But in the case of a promise, as advertised by the object having a method `then`, we have to hook into it, once more using handlers. This is really the main reason why promises are so great. This is what makes them shine. The callbacks upon callbacks are hidden through this recursive evaluation. Each promise is hooked into the preceeding one and fully dependent on its final state. If the inner promise is resolved, we resolve. If not, we reject. And we do it once the inner computation is done.

We are just missing `resolve`, `reject` and `fulfill`. Their implementations are simple enough. The first two serve to update the state of the object. The last is just a convenience to evaluate the handlers if a resolution is reached.

~~~~~{.javascript}
var resolve = function (v) {
  value = v;
  resolution = 'resolved';
  fulfill();
  return this;
};

var reject = function (v) {
  value = v;
  resolution = 'rejected';
  fulfill();
  return this;
};

var fulfill = function () {
  if (resolution) {
    var handler = handlers[resolution];
    if (handler) {
      handler();
    }
  }
};
~~~~~

What strikes me is how elegant the idea is, even though the code shown here is not. The repeated checks for undefined and null are symptomatic of a bad abstraction (or lack of one!). But the idea was never to write a "real" implementation, rather just to explore the concept. Now that you've seen how it could be implemented, maybe you have a better understanding of what it is you are doing when you pass a promise.

I also can't help but reflect on how much easier it was to understand promises having spent some time with haskell. Promises solve the exact problem in JavaScript that monads do in haskell, they let you set up chains of computations that each depend on each other. It should be no surprise then that what we have made here really is a monad. In even more abstract terms, it's a value with a context passed through transformations. In a language like haskell though, implementing this kind of abstraction is made much more simple by the very design of the language, it was in some ways extended around monads (eg do notation, IO).

If you've gotten this far, I have to congratulate you. It is no small feat keeping your concentration through all that. If you are still hungry for more on Promises, do look at the implementation of [Q.js][qjs], it is surprisingly readable.

_Thank you for reading! I hope it helped you get a bit closer to truly grokking promises, as it did me. Please email me at <johanna@variadic.me> with comments, ideas or questions._

[qjs]: https://github.com/kriskowal/q
[promisesa]: http://wiki.commonjs.org/wiki/Promises/A
[promisesap]: http://promises-aplus.github.io/promises-spec/
[deferjs]: https://gist.github.com/yuhama/6241310
[winjs]: http://msdn.microsoft.com/en-us/library/windows/apps/br211867.aspx
[closure]: https://en.wikipedia.org/wiki/Closure_%28computer_science%29
[noop]: https://en.wikipedia.org/wiki/NOP