---
title: Building the Wishlisted.org webapp in Clojure
date: March 29, 2012
tags: clojure, noir
description: Description of the design and architecture of wishlisted.org
---

__TL;DR__ I made [wishlisted.org][wishlisted] in Clojure. Here's the [source code][github].

### What is wishlisted.org?

I built this site to test myself and to try out a non-trivial example of a Clojure webapp, together with a very good [friend][stinaq]. I don't necessarily feel it will solve the world's problems, but I'm still gonna give you the pitch:

Organizing wishlists can be a logistical nightmare, you're lucky if nobody buys the same thing. Keeping track of everything on paper is a hassle, making sure everybody gets their copies is worse. Once printed they can't be updated. Some web based stores let you create wishlists on their site, which is convenient, but if you are using multiple stores well… Back to square one. It's time for one single place to put it all, with simple sharing and updating. Check out [wishlisted.org][wishlisted].

Something like that.

### Couldn't care less, what did you build it with?

Great question! I've been playing around with Clojure for a while, doing 4Clojure problems and I am currently reading [The Joy of Clojure][tjoc], a book that definitely goes beyond the absolute basics of Clojure. You can't read stuff like that without being sincerely inspired to make something.

I changed my mind on the design several times during development, the backend used to be Redis, I used different naming schemes, layout of project files etc. What I ended up with is what I felt was the most stable and elegant architecture for this particular project.

Wishlisted.org uses a [PostgreSQL][pgsql] database, all queries are written in Clojure using the [Korma][korma] library ([Chris Granger][ibdknox]). Using a DSL like this is idiomatic for Clojure, Lisp-family languages lend themselves very well to creating DSLs, to the point where the line between it and the actual language is blurred. In fact, the basic JDBC wrapper for Clojure is in itself so powerful one might not even look for alternatives!

The application itself is written using the micro framework [Noir][noir], by [Chris Granger][ibdknox], which is built on top of Compojure/Ring. Although easy to get started with, it makes relatively few assumptions on what your project should look like, and I feel like I made quite a bit of use of this fact. Having built my first few webapps in [Django][django], I'm most comfortable with putting routes (urls) in its own file. Noir doesn't enforce anything here, but in examples I've seen routing has been baked into the views themselves. The defpage macro itself encourages this, but lets me have it my way. Here's a tree view of the relevant parts of the project directory (to see the entire thing, check out the github repo [here][github]):

	├── README.md
	├── project.clj
	└── src
	    ├── log4j.xml
	    └── wishlistd
	        ├── cljs
	        │   └── main.cljs
	        ├── code.clj
	        ├── models
	        │   ├── init.clj
	        │   └── sql.clj
	        ├── server.clj
	        └── views
	            ├── routes.clj
	            └── wishlist.clj

Pretty straight forward, some things worth noting though. The log4j.xml file has settings to reduce the by default extreme logging that Korma does. The file init.clj sets up the database. Wishlist.clj holds the view and remotes.

### The horror of JavaScript

Really, I kid. JavaScript can be a good language, as long as you avoid large parts of it. My first draft of this site used pure JavaScript, heavily inspired by [John Crepezzi's][johncrepezzi] [hastebin][hastebin]. Basically, you just load the index page, there are no other routes. On load, JavaScript will figure out what to do using the current path. When you load a resource and display it, you update the path. Your webapp will feel responsive and the back button will still work. If you don't need persistance, you could just host this static web page, and the JavaScript, and do some really amazing things (there's plenty of work being done in giving useful alternatives when it comes to client side persistance, but so far it seems limited).

It was OK, but it had the same problem SQL did. It's not Clojure. Thankfully, someone already thought of this. ClojureScript lets you compile Clojure code into a monolithic JS file, using the Google Closure compiler. Here again I rely on the work of Chris Granger, utilising the ClojureScript libraries [fetch][fetch], [crate][crate], and [jayq][jayq]. Jayq is a JQuery wrapper for ClojureScript, crate brings [Hiccup][hiccup] templating and fetch (server and client side library) makes it really easy to quickly put together your client-server interactions.

Clicking new wishlist or inputting a specific URL will load that wishlist. When editing, the page will automatically save whenever focus leaves an input field. Unfortunately, this means it will not save if you leave the page without first leaving the input field. This, coupled with the issue of usability with the lack of a save button… Well, I added a save button. What I realised as I was adding the save button is that it doesn't actually need to do anything. Whatsoever. Clicking the save button will unfocus the input field and trigger the saving mechanism. Awesome.

Like I mentioned, fetch is both a client and server side library. It gives you convenient functions to remotely call server side functions from your client side ClojureScript. Maybe it is easiest to understand by example:

<pre class="sourceCode clojure"><code class="sourceCode clojure">(<span class="kw">defn</span><span class="fu"> read-wishlist </span>[code]
  (model/read-wishlist code))</code></pre>

That's the server side, and this is the client side.

<pre class="sourceCode clojure"><code class="sourceCode clojure">(<span class="kw">defn</span><span class="fu"> read-wishlist-rem </span>[code]
  (<span class="kw">letrem</span> [neu (read-wishlist code)]
    (<span class="kw">when-not</span> (<span class="kw">nil?</span> neu) 
      (show-wishlist! neu))))</code></pre>

The result is a naive but functioning AJAX heavy webapp, out of a relatively small amount of [code][cljs].

### Putting it all together

One of the most attractive tools in the Clojure ecosystem is [Leiningen][lein], a build tool based on Maven. It will scaffold a basic project structure, run your tests for you, run your application, compile it all into an "uberjar" and run a beautiful REPL, but most importantly, it will handle all your dependencies through the project.clj file.

I run the webapp on my Linode, behind an nginx server. Nginx serves all the static resources, CSS, JS, images and the index.html. On top of it all, it's fairly performant, and will load in under 500ms (Linode server in the UK, Pingdom server in the Netherlands), frequently as low as 250ms, including the initial AJAX request to load content.

### Wrap up

So we learned a lot. We got to work together on a project, often simultaneously, pushing and pulling commits. Having the project on github obviously made this a lot easier. I made my first non-trivial AJAX heavy webapp, I got to play around with libraries I had been reading about.

It also made Java more attractive to me. Clojure's principle of interop rather than rewriting everything means using Java's tools. Leiningen uses Maven, Noir uses the Jetty web server etc. 

Clojure makes Java fun.

_Join the conversation at [Reddit][reddit] or email me at <erik@variadic.me>._

[reddit]: http://www.reddit.com/r/Clojure/comments/rj0tb/building_the_wishlistedorg_webapp_in_clojure/
[ibdknox]: http://www.chris-granger.com/
[4clojure]: http://www.4clojure.com/
[hiccup]: https://github.com/weavejester/hiccup
[lein]: https://github.com/technomancy/leiningen
[cljs]: https://github.com/eakron/wishlistd/blob/develop/src/wishlistd/cljs/main.cljs
[jayq]: https://github.com/ibdknox/jayq
[crate]: https://github.com/ibdknox/crate
[fetch]: https://github.com/ibdknox/fetch
[hastebin]: http://hastebin.com/
[johncrepezzi]: http://www.seejohncode.com/
[github]: https://github.com/eakron/wishlistd
[django]: https://www.djangoproject.com/
[noir]: http://webnoir.org/
[korma]: http://sqlkorma.com/
[pgsql]: http://www.postgresql.org/
[stinaq]: http://stinaq.se/
[tjoc]: http://fogus.me/
[wishlisted]: http://wishlisted.org/
