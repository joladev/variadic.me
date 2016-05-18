---
title: Maybe - Adding an Error Message
date: October 26, 2012
tags: csharp
description: Rarely do we need only to know that something failed, without the why. Encoding an error message in a Maybe style wrapper is simple enough, but using some inheritance in C# we can make it pretty.
---

In the [previous post][maybe1] I proposed using a wrapper object to protect nullable values or references, however, the naive object I demonstrated only lets us know whether or or not our computation succeeded. In most cases, this is not enough to react properly. We need to know what went wrong. It is tempting to use built in exception management, but as a solution it is not very portable. Exceptions don't travel well between platforms (or sometimes, even within). Furthermore, it exposes internal details. If you catch SQLExceptions in your GUI code, you're locking yourself into whatever libraries generated these exceptions and thus violate the loose coupling principle.

Now, looking at the code example in the previous post, one could simply add a string property called error. After instantiating the wrapper object, one would simply set the property to whatever error message (or exception object) one would like. Playing around a bit with inheritance though, I've found a much prettier solution. No doubt it could be improved upon.

~~~~~{.cs}
abstract class Either<T>
{
    protected T value = default(T);
    protected bool success = false;
    protected string error = "";

    public T Value
    {
        get { return value; }
    }

    public string Error
    {
        get { return error; }
    }

    public bool Success
    {
        get { return success; }
    }
}

class Failure<T> : Either<T>
{
    public Failure(string error)
    {
        this.error = error;
    }
}

class Success<T> : Either<T>
{
    public Success(T value)
    {
        this.success = true;
        this.value = value;
    }
}
~~~~~

It might look overly complex, why the abstract class and inheritance? The easiest way to explain is by example. Say you only have the Either class, it would look something like the example below.

~~~~~{.cs}
class Either<T>
{
    protected T value = default(T);
    protected string error = "";
    protected bool success = false;

    public T Value
    {
        get { return value; }
    }

    public string Error
    {
        get { return error; }
    }

    public bool Success
    {
        get { return success; }
    }

    public Either(T value)
    {
        this.success = true;
        this.value = value;
    }

    public Either(string error)
    {
        this.error = error;
    }
}
~~~~~

Can you spot the bug? You might not even notice it in many use cases. The problem lies in the overloaded constructor. One takes a parameter of type T, as determined by the class, the other of type string. But what about when T is string? You would never reach the constructor that represents success.

Not only does the first example solve this problem, it also lends itself better to extension and even a generally more attractive look of the code. Using the example from the last post.

~~~~~{.cs}
class Program
{
    public static Either<string> ThisWontFail()
    {
        return new Success<string>("I didn't fail!");
        // The Success object is a subclass of Either
        // but we explicitly create Success to wrap
        // our value. This improves code readability
    }

    public static Either<string> ThisWillFail()
    {
        return new Failure<string>("This is an error message");
        // And we now explicitly create a Failure object
        // wrapping the error message.
    }

    public static void Main(string[] args)
    {
        Either<string> a = ThisWontFail();

        if (a.Success)
        {
            Console.WriteLine(a.Value);
        }

        Either<string> b = ThisWillFail();

        if (!b.Success) // flip it so we get to read the error
        {
            Console.WriteLine(b.Error);
        }
        Console.ReadKey();
    }
}
~~~~~

It is orders of magnitude more appealing to write `new Success<string>("WOHOO")` than it is `new Either<string>("WOHOO")`. The first is explicit in its purpose, the second is not.

This was the conclusion of my two part series on null and alternatives to it. I will be writing more on similar subjects in the future, so please subscribe to the feed, and I'll see you again!

_Please email me with questions or statements at <johanna@variadic.me>._

[maybe1]: http://variadic.me/posts/2012-10-25-maybe-bane-null.html