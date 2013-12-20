---
title: Bind, Call and Apply in JavaScript
date: Oct 22, 2013
tags: javascript
description: Exploring a JS snippet in order to understand bind, call and apply in JavaScript.
---

The other day, I saw this neat JS snippet in a [tweet].

~~~~~{.javascript}
var bind = Function.prototype.call.bind(Function.prototype.bind); // #fp
~~~~~

At first glance, I can guess what it does. It turns `x.y(z)` into `y(x,z)`. With a childlike glee I show it to my colleagues. They ask me what it does. I open my mouth to explain and... nothing comes out. I turn around and walk away. 

You see, most well written code will instantly tell you what it does. With some experience in functional JavaScript, and having read both Functional JavaScript and JavaScript Allongé (both amazing books), I had no problem reading it. But explain it (and why one would even care about doing it) to someone with no experience in functional programming?

I decided to work my way through it, with simple examples and lots of comments. This was the result.

~~~~~{.javascript}
// Set up a simple object to use as "context"
var context = { foo: "bar" };

// A function that uses a reference to a variable called "foo"
// on the "this" context.
function returnFoo () {
  return this.foo;
}

// This variable does not exist on scope, so is undefined.
returnFoo(); // => undefined

// But if we bind the function to the context.
var bound = returnFoo.bind(context);

// The name variable is now in scope.
bound(); // => "bar"

//
// That's what Function.prototype.bind does. Since returnFoo
// is a function, it inherits the function prototype.
//
// If you enjoyed that, keep reading. It just gets better.
//

// There are many ways of attaching a context to a function.
// Call and apply let you call a function with a given context.
returnFoo.call(context); // => bar
returnFoo.apply(context); // => bar

// Including adding the function to the object.
context.returnFoo = returnFoo;
context.returnFoo(); // => bar

//
// Now let's get freaky with it.
//

// Array.prototype has this sweet method called slice.
// You call it on an array, and it gives you a copy of
// the array from start index to end index (exclusive).
[1,2,3].slice(0,1); // => [1]

// So we grab slice and assign it to a local variable.
var slice = Array.prototype.slice;

// slice is now "unbound". As Array.prototype.slice usually
// acts on the context it is given, or "this", it will
// no longer work.
slice(0, 1); // => TypeError: can't convert undefined to object
slice([1,2,3], 0, 1); // => TypeError: ...

// But if we recall apply and call, they let us supply a context.
slice.call([1,2,3], 0, 1); // => [1]

// Apply works like call, but takes arguments as an array.
slice.apply([1,2,3], [0,1]); // => [1]

// It sure gets old using .call though. What if we bind it?
// That's right! Let's bind "call" to slice. Yes.
slice = Function.prototype.call.bind(Array.prototype.slice);

// Now slice uses the first argument as context.
slice([1,2,3], 0, 1); // => [1]

//
// Pretty cool huh? But I got one last thing for you.
//

// Let's put "bind" itself through the same process
// we did "slice".
var bind = Function.prototype.call.bind(Function.prototype.bind);

// Wrap your mind around that. Think about it.
// What does it do? We are flipping "call",
// returning a function that takes a function
// and a context and returning a fully bound function.

// Bringing back our original example.
var context = { foo: "bar" };
function returnFoo () {
  return this.foo;
}

// And using our new amazing "bind".
var amazing = bind(returnFoo, context);
amazing(); // => bar

// Reference: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/bind
~~~~~

## Notes

I used the [Mozilla Developer Network article on bind][mdn] almost exclusively when figuring this out. I am not sure why it is so low on Google's rankings, I've even taken to adding "mdn" whenever I search for HTML/CSS/JS documentation.

I have realised something about myself. My number one most effective technique for understanding complex ideas is attempting to explain them to someone else. My brain tends to just gloss over the complexitites of a concept, fooling me into thinking I get it. But when you explain something you have to really, really understand it. Through and through. 

I learned a lot from that tweet, I felt enlightened after working my way through it. I hope you did too! If you have any comments or ideas, please email me at <erik@variadic.me> or tweet me at [&#64;eakron][twitter].

[tweet]: https://twitter.com/littlecalculist/status/125413301965438976
[mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/bind
[twitter]: https://twitter.com/eakron
