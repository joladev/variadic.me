---
title: Maybe - The Bane of Null
date: October 25, 2012
tags: csharp, haskell
description: An alternative to null in computations that can fail.
---

__*This is the first part of two, second is found [here][maybe2].*__

We've all had it, the dreaded null reference exception. Using null to represent failed computations or a lack of an object reference is a norm in much of the programming world today. Java, C#, and many others use null even in system libraries. At the same time, there is a growing awareness of the very real cost of this practice. These languages are lauded for their type systems, allowing the compiler to infer many programmer mistakes at compile time. The null reference error is not one of these. It'll sneak in and wreak havoc, very often being difficult to track down, as the exception will most often not happen in the place where the null reference was given. It would be more than impractical to have the compiler make sure you check for null values or references in every single step of your code, for everything that is nullable. Especially when the developer might even be confident that his reference could never be null (in the end, all safety aside, we have to trust the developer).

Why does null exist in the first place? When it causes so much trouble, why is it there? According to the inventor of null, Tony Hoare, it was simply a mistake. During development of the first object oriented reference system (for ALGOL W), he gave into temptation and added a null value, to represent the lack of a reference, rather than developing a proper system to handle the lack of a result. Today he calls it his [Billion Dollar Mistake][tonyhoare].

### Are you telling me that there's an alternative?

In fact, there are several. There are a number of programming languages that have avoided this trap, most using a variation on the same theme. Instead of returning an object reference that might be null, they will return an object wrapping the reference. Some of these languages eschew the use of null altogether, others just recommend the programmer never to use it. In either case, in order to get his hands on the object, the developer has to first check if the wrapper object is a successful computation or not. Many of these languages include a special syntax, a combination of pattern matching and deconstruction of the object, to simplify this process. The variations are called many different things, ranging from Maybe to Option, including types that also implement error messages or other information.

Here is an example of this syntax in Haskell

~~~~~{.haskell}
printMaybe :: Maybe String -> IO ()
printMaybe input =  case input of
                         Just x  -> putStrLn x
                         Nothing -> putStrLn "I got nothing!"

main = do
    printMaybe $ Just "Hello, I just met you."
    printMaybe Nothing
~~~~~

Much of the syntax may be unintelligible for non-haskellers, but the important part is in printMaybe which uses "case" to check whether or not the input is a succesful computation. The same statement also grabs the value wrapped in the succesful computation and prints it out. If it was unsuccesful, it prints "I got nothing!".

### An example in C\#

Now, in languages like C# or Java we don't have this fancy pattern matching syntax, but we can still wrap values and this way require the caller to check the value. Basically, what we're doing is telling the caller that this computation might fail, using the type system.

Note, the following example is a proof of concept, not necessarily an indication of best practice. In C# one common solution is the TryParse technique used by eg the platform Dictionary implementation. It can be a bit obscure in its use, because of the `out` keyword, but avoids the creation of one extra object reference. It also should not be hard to port the example to Java.

~~~~~{.cs}
class Maybe <T>
{
    private T value;
    private bool success;

    public T Value { get { return value; } }
    public bool Success { get { return success; } }

    public Maybe()
    {
        this.success = false; // explicitly setting success to false
                               // this constructor represents failed computation
    }

    public Maybe(T value)
    {
        this.success = true; // represents succesful computation
        this.value = value;
    }
}

// An example of code using Maybe<T>

class Test
{
    public static Maybe<string> ThisWontFail()
    {
        return new Maybe<string>("I didn't fail!");
    }

    public static Maybe<string> ThisWillFail()
    {
        return new Maybe<string>(); // empty constructor represents failed computation
    }

    public static void Main (string[] args)
    {
        Maybe<string> a = ThisWontFail();

        if (a.Success)
        {
            Console.WriteLine(a.Value);
        }

        Maybe<string> b = ThisWillFail();

        if (b.Success) // this is not true
        {
            Console.WriteLine(b.Value);
            // this will never happen so we get no null reference exception
            // on the implicit call to ToString
        }
    }
}
~~~~~

The astute reader will of course notice that the user of this Maybe class could just ignore the Success property and get a null reference even with Maybe. The truth of the matter is that null is in the language and we can't prevent that, we can just try to avoid relying on it. __Returning a nullable reference is bad coding practice__. It requires the caller to always remember to check for null, even though the method itself gives no indication of this. Adding Maybe to the method signature can serve as a reminder, as it will force the caller to explicitly unwrap the result value.

### Taking another perspective

In reality, one could simulate the Maybe class by simply returning a value wrapped in a list of some sort. This would also require the user of the method to unwrap the value. What this lacks is a certain explicitness in your intentions. The Maybe object has one purpose and one purpose only, to represent results that may be failures. A list is used for many other things. The user is simply less likely to remember to check the length of a list before grabbing the first element (causing an index out of bounds exception, which is still easier to debug than a null reference!).

### Conclusion

Removing null from your API could severely reduce the number of hard to trace bugs, whatever solution you use, without needing added complexity. The `Maybe` data type as described here is only one possible implementation, but the idea behind it should be clear. Instead of sending a naked object reference, one should wrap it in an object containing information on whether or not one can expect the value to be null. An explicit requirement on the user of the code to unwrap the reference simply leads to better code.

For more information take a look at some of the languages that avoid null, like: [Scala][scala], [Rust][rust], [Haskell][haskell].

__*Continue on to part two here: [Adding an Error Message][maybe2].*__

_Don't hesitate to email me with questions or comments at <johanna@variadic.me>._

[maybe2]: http://variadic.me/posts/2012-10-26-maybe-adding-error-message.html
[tonyhoare]: http://en.wikipedia.org/wiki/Nullable_type
[rust]: http://www.rust-lang.org/
[scala]: http://www.scala-lang.org/
[haskell]: http://tryhaskell.org/
