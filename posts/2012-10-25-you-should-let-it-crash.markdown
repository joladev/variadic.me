---
title: You should let it crash
date: October 23, 2012
tags: csharp, java
description: Contrary to intution, crashing is actually a good thing. Here's why you should never catch all.
---

Nobody likes crashes. It's irritating, disrupting to workflow, makes your application look shoddy and unfinished. But despite this, crashing is a good thing. In an ideal world, the developer would predict every single possible exception in his or her code base and put in contingencies, letting the application recover from each of them. We all know this is not feasible. No matter how much time is put into testing, some bugs will always get through. The issue at hand is what to do about this.

A tempting idea is to always follow a specific exception with a general one, catching any exceptions that we have not considered. This is a very, very bad idea.

When first exposed to exception management in a language like C# or Java, many of us resort to the lazy  solution of simply catching "Exception", the base class of all exceptions. We don't have the experience to figure out what exceptions can be expected. This way, we always know we'll catch the exception and our application will not crash. As we mature, we add specific exceptions with specific solutions, but some seem never to outgrow the need for the base class. A typical piece of try/catch code might look like this.

~~~~~{.cs}
try 
{
    // stuff
}
catch (SQLException e)
{
    // something went wrong in the database
    // let's fix it
}
catch (Exception e)
{
    // I'm not sure what went wrong
    // but I don't want my application
    // to crash because of it
}
~~~~~

The intention is good, you want to prevent crashes. And let's face it, crashes are annoying! And disastrous for a business; a frequently crashing application will drive away customers. But what do you put in the last catch clause? The same thing as you always put in a catch, right? The code to fix the problem. Except you don't know what went wrong.

That's the problem right there. You don't know the problem. You don't know what happened. So you can't fix it. Putting a catch all and some nonsense catch clause here will let your application continue in a corrupted state. You can never be sure it will behave correctly. You can't know if it will save the files or fire the missiles. Maybe it would be fine, but is this a risk you're willing to take? Let's say the application is Word and some extremely rare obscure bug happens, which prevents the save logic from functioning. Your catch all triggers and the user notices nothing. Now every time he clicks save, the application appears to be saving, but in reality it does nothing. The code that handles saving is not functioning. After hours of work he makes sure to save a final time, closes the application down and goes home. Put yourself in the shoes of this user. How would you react, when you get back to work in the morning.

Even worse, consider a banking web application. In order to determine who has access to what accounts, it includes an elaborate access control system. Every other full moon that occurs on a Tuesday with no rain, something gets stuck in the machinery and instead of erroring out, it will incorrectly give a session access to the wrong accounts. Because some hapless code monkey was too lazy to track down the specific exceptions that were possible, and instead wrote a catch all clause, figuring it would probably not matter.

It's naive to think that it would never happen to you, but the specific examples don't even matter. If an exception happens and you don't know how to fix it, your application state is corrupted. You should not allow it to continue running. Grab what debug information you can. Use it to update your app, make it better, try to catch more specific exceptions. But don't let your application go zombie.

Just let it crash.

_Please email me with questions or statements at <erik@variadic.me>._
