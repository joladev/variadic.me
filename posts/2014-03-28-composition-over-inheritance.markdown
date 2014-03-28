---
title: Composition Over Inheritance
date: March 28, 2014
tags: java, csharp, oop
description: Composition trumps classical inheritance
---

Classic Object Oriented Programming intro classes tend to begin with examples of inheritance. Although OOP has been something of a standard in commercial programming for a long time now (it was popularized by Java which was released 1995) very little progress has been made in the last 10 years. And even less in how OOP is taught.

Inheritance is introduced as __the__ solution to promote code reuse and organize your code. You are taught to begin each challenge, each code problem, by constructing an inheritance hierarchy (if you haven't experienced this, consider yourself lucky). If you are as unfortunate as I was, you will even be taught to begin each problem by constructing a UML model of it. A common example is the pervasive real life enterprise project of cataloguing animals and what sound they make. Since all animals make noises, it would be silly to rewrite the method "speak" for each class of animal. Instead we create a base class, `Animal`, and have the rest inherit from it.

~~~~~{.cs}
class Animal {
    private string noise;
    public void Speak() {
        Console.WriteLine(this.noise);
    }
}

class Cat : Animal {
    private string noise = "Meow";
}

class Dog : Animal {
    private string noise = "Woof";
}
~~~~~

`Speak` is only defined once, but can be used separately by each sub class. We have achieved glorious code reuse, rejoice! Our code will surely revolutionize the field of animal noise catalogization.

Apart from the absurdity of the example (it is surprisingly hard to find examples of real life use cases where __inheritance__ is truly useful and safe) what we have to realise is that we have already set ourselves up for failure. Inheritance forces a fundamentally unchangable requirement on all heirs. They _have to_ act like the base class. It is now impossible to add an animal that does not speak. What about crickets? They make a noise but they don't speak. Requirements change over time and good code will be able to adjust. Inheritance is saying that everything will always act this specific way. _All animals speak._ Eventually we realise this is wrong, we adjust our model, and say: _All animals make noises_. Except of course all the animals that don't. Nobody is perfect, and business requirements that never ever change are a blissful fantasy. We have to be able to adjust. Now, this example shows a limitation of inheritance, but not necessarily OOP.

## Composition

The goal of inheritance is composition. A way to structure your code so that multiple entities can share behavior. In fact, inheritance is just one limited way of achieving composition.

In C#, the `List` type is one of the most commonly used types. And rightfully so, many things in life show list-like behavior. An address book can be seen as a list of contacts. A person's children is a list. So ideally, we would want to reuse `List` to avoid repeating ourselves and get lots of nice free functionality. Except `List` is a sealed class in C#. This means you can't inherit from it. Why would they do this? Why can't I use inheritance to reuse this code? One reason commonly given is that `List` contains multiple performance optimizations and would not work as well if you mucked about with the internals. A flimsy reason perhaps.

A much more convincing reason, to me, is the fact that _nothing_ in life acts exactly like a list. By inheriting from `List`, you are saying that a person _is_ and _always will be_ a list. Your address book will always be a list. You have gone too far in your quest for code reuse. You have locked yourself into behavior you may not want. An address book is often ordered, but `List` does not guarantee this. In fact, an address book behaves a lot more like an ordered dictionary. Except when it doesn't.

Our goal is code reuse, but inheritance is not our only possible weapon. There are many ways of achieving composition without it.

### A better address book

Lets say we do want some of the `List` like behaviors, without limiting future changes too much. How would we go about this? In my experience, one of the best ways is internal state combined with interfaces. So, maybe the behavior we wanted to mimic from lists was `IEnumerable`. We also want to have a property called `Count`. For simplicity's sake each address is just a simple `string`.

`````{.cs}
public class AddressBook : IEnumerable
{
    private List<string> addresses = new List<string>();
    public int Count { get { return addresses.Count; } }

    public AddressBook(List<string> addresses)
    {
        addresses.ForEach(a => this.addresses.Add(a));
    }

    public IEnumerator GetEnumerator()
    {
        return addresses.GetEnumerator();
    }
}

class Program
{
    static void Main(string[] args)
    {
        var ab = new AddressBook(new List<string>() {"One", "Two"});
        Console.WriteLine(ab.Count);
        foreach (var a in ab)
        {
            Console.WriteLine(a);
        }
        Console.Read();
    }
}
`````

As you can see, we get syntactic sugar; we can write our foreach loop just like a regular `List`. From an end user point of view, it works just like if it had been done through inheritance. We've also got full code reuse, we had to write nothing ourselves. But we did not have to inherit. We have no extra behaviors that we may or may not want. We have left ourselves completely open to change. In fact, we could replace the internal `List` with `Dictionary` and the end user would never even notice a thing. __We have complete freedom to adapt to changing requirements.__

_Know why inheritance is pushed on unsuspecting students? Feeling chatty? Tweet me up at [&#64;eakron][eakron] or send me an email at <erik@variadic.me>._

[eakron]: https://twitter.com/eakron
