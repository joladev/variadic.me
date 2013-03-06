---
title: Solving Problems Using Data Structures
date: TBD
tags: csharp, java
description: In this article I show how a data structure can be used to encapsulate implementation details and lead to nice clean code.
---

**_TL;DR_: In this article I show how a data structure can be used to encapsulate implementation details and lead to nice clean code.**

Recently I came across a problem that I know has been solved many times but I've never really looked at before. The goal was to be able to show the last 5 entries that had been accessed, ordered by access. This also assumes that each entry can only be shown once, even if accessed multiple times.

Broken down in steps, we need this functionality:
- When an entry is accessed, this entry should be taken to the top of the list.
- If the entry is already in the list, move it to the top.
- If a new entry is added to the list, and it has grown beyond 5, remove the least recently viewed entry.

This was for a web service, so my first thought was to just implement a new method that handled all that stuff and then persisted it as a list to the cache (database would be fine too, but this data is not critical). Just as soon as I had that idea, I got a bad feeling about it. If I really kept this data in a regular list, I'd have to handle uniqueness myself. And if I kept it in a Dictionary or Set, I'd have to handle order myself. That's when it hit me. _I finally have a legitimate reason to make my own data structure._

### The birth of the terribly named data structure

This is the basic jist of the implementation I ended up with. It lacks completeness, this is just the parts that are required for the use case. Efficiency was never a prime concern, there's no doubt plenty of room for optimization.

~~~~~{.cs}
namespace LruDictionary
{
    using System.Linq;
    using System.Collections.Generic;
 
    public class LruDictionary<T1, T2>
    {
        private readonly int maxSize = 5;
 
        private Dictionary<T1, T2> objects = new Dictionary<T1, T2>();
        private List<T1> order = new List<T1>();
 
        public List<T2> Values
        {
            get { return this.GetOrderedValues(); }
        }
 
        public List<T1> Keys
        {
            get { return order; }
        }
 
        public LruDictionary(int maxSize)
        : this()
        {
            this.maxSize = maxSize;
        }
        
        public LruDictionary()
        {
        }
 
        public void Add(T1 key, T2 value)
        {
            if (!objects.ContainsKey(key))
            {
                objects.Add(key, value);
            }
            if (order.Contains(key))
            {
                order.Remove(key);
            }
 
            order.Add(key);
 
            TrimExcess();
        }
 
        private void TrimExcess()
        {
            if (objects.Count > maxSize)
            {
                var key = order[0];
                order.RemoveAt(0);
                objects.Remove(key);
            }
        }
 
        private List<T2> GetOrderedValues()
        {
            var result = new List<T2>();
            for (int i = order.Count-1; i >= 0; i--)
            {
                result.Add(objects[order[i]]);
            }
            return result;
        } 
    }
}
~~~~~

The end result is that we've taken care of all the three main steps involved in keeping track of our recent entries, and done so without exposing any of the internal complexity. Using it is as simple as creating an instance and adding entries to it. 