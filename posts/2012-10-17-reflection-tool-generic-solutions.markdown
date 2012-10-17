---
title: Reflection as a tool for generic solutions
date: October 17, 2012
tags: c#, reflection
description: Using reflection to transform specific solutions to generic ones.
---

### Generic solutions

In this article I will talk about two concepts: [reflection][reflection] and generic solutions. My goal is to explain reflection and why concepts like loose coupling and encapsulation are so important. The intended audience is not expert programmers, but need a basic understanding of object oriented programming. By generic solutions I mean a solution to a specific problem that can be transplanted into another use case with minimal changes. This requires extremely [loose coupling][loose], and is one of the main goals of object oriented programming. The basic process is one of disconnecting your code from the specific details of the case or problem that prompted you to write the code in the first place, in such a way that you or another person could use it to solve another problem with its own set of specific details, without having to rewrite it.

Reflection enables you to do this, by exposing meta data of objects at run time. Basically, your program can be "intelligent" enough to adjust to different situations and still produce a useful result. The less your code needs to know about the use case, the looser coupled it is from your code.

A lack of understanding of encapsulation, loose coupling and "generic solutions" is surprisingly prevalent in an industry where object orientation is a standard. I write this post mainly to help students at my department, but it should be of interest to any programmer.

### Our example use case, in Java

In a school project I did a couple of years back using Java we needed a simple piece of code that turned a list of [value objects][vo] into a String containing an HTML table, each get-method a column and each element in the list a row. The goal was turning something simple like a list of Student objects into an HTML table of students.

~~~~~{.java}
class Student {
    private String name;

    public Student(String name) {
        this.name = name;
    }

    public String getName() {
        return this.name;
    }
}
~~~~~

Taking a list of these students, we should return a table.

~~~~~{.html}
<table>
  <tr>
    <td>StudentName1</td>
  </tr>
</table>
~~~~~

What occurred to me was that I was not the only one who needed to do this. Each group working with this had their own case and their own value objects. I could've written an HTMLTablePrinter that assumed the input would be a list of Students, and since I ahead of time knew all the get-methods on a Student, I could just hard code them. But if I had the ability to introspect the value objects in the input at run time, I could create a printer that didn't care what the data type was, as long as it followed some simple rules.

### Figure out what you actually need to know

What all our value objects had in common was the basic Java methods inherited from the Object class, plus the get/set methods we had added ourselves. As long as I could assume that all attributes would have a corresponding method beginning with the letters 'get', I could match method names based on that, to find all attributes. The only bump in the road at this point is that one of the methods inherited from the Object class is called getClass, so we have to filter that one.

So in words, all we need to do is to grab one of the objects and look at its meta data, extract the interesting methods and then loop through the list invoking the list of methods each in turn. In the past I've been asked what use reflection has, because in a lot of examples the problem could've been solved without reflection at all. This problem can't be solved straightforwardly.

### Writing a naive non-generic solution in C\#

Although I originally "found" this problem while working in Java, I decided to write this article using C#, since it is what my original target audience (students at my department) uses. Ok, to start, we'll write a solution to our problem that is not portable, and tightly coupled to the use case. We'll create a basic class to work with, this is our "value object". In reality, it would be more appropriate to use [structs][struct] in C# for this purpose, but to keep it simple I've stayed with proper classes. However, I've chosen to use [properties][properties] instead of get/set methods, as these are much more prevalent in C# in this kind of class. Once we have a working solution, we will move on to improve it .

~~~~~{.cs}
class Student 
{
    public string Name { get; set; }
    public int NumberOfCouches { get; set; }

    public Student(name, noCouch) 
    {
        Name = name;
        NumberOfCouches = noCouch;
    }
}
~~~~~

So now we got students; for each student we keep track of their name and the number of couches they own (a most useful statistic). To create a string containing an HTML table representation of these objects, we'll create another class.

~~~~~{.cs}
class VOTable 
{
    public static string MakeVOTable(List<Student> students) 
    {
        StringBuilder str = new StringBuilder("<table>");

        foreach (Student s in students) 
        {
            str.append("<tr>");

            str.append("<td>");
            str.append(s.Name);
            str.append("</td>");
            
            str.append("<td>");
            str.append(s.NumberOfCouches);
            str.append("</td>");

            str.append("</tr>");
        }

        str.append("</table>");

        return str.ToString();
    }
}
~~~~~

This is a usable solution to the problem, generating an HTML table from our list of students. However, what happens if we decide to add another property to Student? We decide we also need to know what color of hair the student has. Apart from changing the Student class, we'd have to add several lines in our MakeVOTable method. If we forget to do this, the HairColor attribute simply wont render in the table. Subtle bugs are easily introduced this way.

Another problem is that we've hard coded the type of list that MakeVOTable accepts. We can only render lists of students, what if we wanted to use this snippet of code to make a table out of a Course object? We'd have to rewrite it and keep one version for each type of list we want to turn into a string.

In short, we know two things we shouldn't. We know the type of object that we will render, and we know the methods it has. But our case only really requires us to know one thing, __that our objects will have properties__.

### Time to reflect

We'll be using two concepts from reflection: the ability to get a list of properties at run time, and then secondly to invoke these properties on an object. In our use case we can expect all properties on our value object to be attributes.

~~~~~{.cs}
class VOTable 
{
    public static string MakeVOTable(List<object> vos) 
    {
    }
}
~~~~~

Our first improvement, the method signature for MakeVOTable no longer assumes the type parameter of List to be Student. Next we'll grab the list of properties, through some reflection magic. Remember to add `using System.Reflection;` to the top of the class file.

~~~~~{.cs}
class VOTable 
{
    public static string MakeVOTable(List<object> vos) 
    {
        Type t = vos.ElementAt(0).GetType(); // grab the type of the first element
        PropertyInfo[] properties = t.GetProperties();
    }
}
~~~~~

Lets step through this. At first we need to grab the `Type` object of . We declare a variable `t` of the type `Type`, and assign it the result of `GetType` on the first element of the list of objects. A keen eye will detect a possible angle of entry for bugs, a list of objects is not necessarily all of the same type, but for our example we will assume it is. In the second statement we get an array of properties and assign it to our variable `properties`.

Now that we have our properties, we simply need to iterate through the list of objects, invoking each property on each object and formatting the result. For simplicity's sake (although maybe not readability) we will stick to a StringBuilder and appending to it.

~~~~~{.cs}
class VOTable 
{
    public static string MakeVOTable(List<object> vos) 
    {
        Type t = vos.ElementAt(0).GetType();
        PropertyInfo[] properties = t.GetProperties();

        StringBuilder str = new StringBuilder();

        str.Append("<table>");
        foreach(object o in vos) 
        {
            str.Append("<tr>");
            foreach(PropertyInfo p in properties)
            {
                if (p.CanRead) // makes sure property has get method
                {
                    str.Append("<td>");

                    // Now we are ready to invoke the property on the object
                    object res = p.GetValue(o, null);
                    // o is the object we invoke p on
                    // null is our array of parameters

                    str.Append(res);

                    str.Append("</td>");
                }
            }
            str.Append("</tr>");
        }
        str.Append("</table>");

        return str.ToString(); // turn our StringBuilder into a string as we return
    }
}
~~~~~

And there it is. Our MakeVOTable can now take a list of any type of object that has properties and will return an HTML table from this. Don't hesitate to create a few different classes and trying it out on them. It's far from a robust piece of code, any number of bugs could be introduced, but as an example of the use of reflection, it's quite nice.

### Bonus credits

HTML tables can also take headers, and we can figure out the names of the properties by looking at the meta data on PropertyInfo. Let's add headers.

~~~~~{.cs}
class VOTable 
{
    public static string MakeVOTable(List<object> vos) 
    {
        Type t = vos.ElementAt(0).GetType();
        PropertyInfo[] properties = t.GetProperties();

        StringBuilder str = new StringBuilder();
        str.Append("<table>");

        str.Append("<tr>"); // table headers get their own row
        foreach(PropertyInfo p in properties) 
        {
            str.Append("<th>");
            str.Append(p.Name); // the name of the property
            str.Append("</th>");
        }
        str.Append("</tr>");

        foreach(object o in vos) 
        {
            str.Append("<tr>");
            foreach(PropertyInfo p in properties)
            {
                if (p.CanRead) // make sure property has get method
                {
                    str.Append("<td>");

                    // Now we are ready to invoke the property on the object
                    object res = p.GetValue(o,null);
                    // o is the object we invoke p on
                    // null is our array of parameters

                    str.Append(res);

                    str.Append("</td>");
                }
            }
            str.Append("</tr>");
        }
        str.Append("</table>");

        return str.ToString(); // turn our StringBuilder into a string as we return
    }
}
~~~~~

### Breaking it

As I mentioned it wouldn't be hard to break our code. Creating a `List<object>` with different data types would result in a crash, as the list of properties we grab off the first object would not all be available on all the rest of the objects. 

Another way is using an empty list. That would make our code crash on `Type t = vos.ElementAt(0).GetType();` with index out of range exception, as there is no element at index 0. Even simpler would be just giving null as the parameter to MakeVOTable, crashing it with argument is null exception on GetType.

There are solutions for each of these problems, but they are outside the scope of this article.

### Conclusion

Hopefully this example of transforming code to be more generic has been useful to you. I wrote this in C#, but it's possible in any language that enables introspection at run time. As long as the appropriate meta data is available, you can do some really nifty stuff to solve these kinds of problems.

Finding a generic solution often requires you to look at a problem in a very different, or at least more abstract, way. At first glance we solved our problem wonderfully with the first example code. But after further thought, we saw that we could easily rewrite our code so that it could handle a much greater set of situations. This piece of code is more loosely coupled to its problem area, it makes few asssumptions about the problem area (although it could be rewritten to make less), and could more effectively be applied to other code bases.

_Please email me with questions or statements at <erik@variadic.me>, or join the conversation on [Reddit][reddit] or [Hacker News][hackernews]._

[hackernews]: http://news.ycombinator.com/item?id=4664138
[reddit]: http://www.reddit.com/r/programming/comments/11ml74/reflection_as_a_tool_for_generic_solutions_in_c/
[properties]: http://msdn.microsoft.com/en-us/library/x9fsa0sw(v=vs.80).aspx
[loose]: http://en.wikipedia.org/wiki/Loose_coupling
[reflection]: http://en.wikipedia.org/wiki/Reflection_(computer_programming)
[vo]: http://en.wikipedia.org/wiki/Value_object
[struct]: http://msdn.microsoft.com/en-us/library/ah19swz4(v=vs.71).aspx
