---
title: Share state between controllers in AngularJS
date: Oct 15, 2013
tags: javascript, angularjs
description: Share state between controllers in AngularJS using a state service. 
---

_Edit: Got some nice input from Reddit and added a simplified version which covers most cases to the bottom of the article._

[AngularJS][angularjs] is one of the best frameworks I have ever come in contact with. It is easy to learn (although there is a small bump right at the start, figuring out the whole modular thing, leading many beginners to just create a single controller and putting everything in it). But you get over it fast enough, start breaking stuff up into contained cohesive units. And it's amazing. Suddenly, JavaScript, the language that has before seemed impossible to be given structure, has it. Instant enlightenment. [Dependency injection][di] is the single greatest invention by man. [Miško Hevery][misko] is a god.

You start punching out mad code. You set up services, filters, directives and a whole bunch of controllers. It's all gorgeous. AngularJS makes you write good code. Everything sort of naturally falls into place and even as the code base grows, it never gets unweildy. When you, in brief moments of insanity, try to work against the framework, it prompty punishes you. Not because it is cruel, but to protect you from making architectural mistakes. Monstrosities in unmaintainability. You chant the mantra: "testability, dependency injection, modularity", and figure it out. You didn't need to create a class in order to contain that piece of conversion logic, you needed a filter. Everything is right again.

Eventually you end up at the point where one controller needs to talk to another controller. And AngularJS fails you. Now really, this isn't a failure on AngularJS's part at all, but rather just a result of keeping all dependencies handled by a DI framework. Your controller has some state, and another controller also wants the same state. This could be a username, a config object, just about anything. Of course, now you see it. _You need a service._

## State Service

A naive implementation might look something like this. 

~~~~~{.javascript}
angular.module('services', [])
  .factory('state', function () {
    var state;

    return {
      state: state,
    };
  });
~~~~~

There's a huge problem with this, but in many cases you may not even notice it. A quick recap of your JavaScript 101. Objects and arrays are passed by reference. Values are not.

~~~~~{.javascript}
var state = { name: "one" };
var controller = { name: state.name };
state.name = "two";
controller.name; // => "one"
~~~~~

So updating the name property of this "state service" does not propagate changes to the scope of each controller. If your state happens to be an array or an object, and you stick to modifying it rather than reassigning it, this would still work (it is possible to implement the "state service" using a shared object instead, I've added an example of this at the bottom of the page). However, AngularJS's $scope is a bit more complicated than my silly example, and has some inner machinery that will not react as you expect. You see, in order to keep the DOM and the data on the scope in sync, also known as two way binding, it uses something called the [digest cycle][concepts]. A new digest cycle is only triggered if AngularJS can recognize a change being made on the scope. Since changes are made far, far away, it will be unable to do so. Bummer!

## Events

Luckily, we have events. In AngularJS, these are transmitted through the rootScope and bubble up through all scopes. Using `$scope.$on` we can attach listeners to these and react to changes in our state service!

So lets try again. In steps, what we want to do is this:

* Set an initial state and expose this
* Expose an updating function
* Let this function broadcast changes

~~~~~{.javascript}
angular.module('services', [])
  .factory('state', function ($rootScope) {
    'use strict';

    var state;

    var broadcast = function (state) {
      $rootScope.$broadcast('state.update', state);
    };

    var update = function (newState) {
      state = newState;
      broadcast(state);
    };
    
    return {
      update: update,
      state: state,
    };
  });
~~~~~

And on the controller side:

* Get the initial state
* Set up a listener on the change event
* Update the state when needed

~~~~~{.javascript}
angular.module('controllers', ['services'])
  .controller('MainCtrl', function ($scope, state) {
    $scope.state = state.state;
    
    $scope.$on('state.update', function (newState) {
      $scope.state = newState;
    });
    
    $scope.update = state.update;
  });
~~~~~

As a convenience, we could move the boilerplate of event attaching to the state manager. Something like this:

~~~~~{.javascript}
// in the service
var onUpdate = function ($scope, callback) {
  $scope.$on('state.update', function (newState) {
    callback(newState);
  });
};

// and in the controller
state.onUpdate($scope, function (newState) {
  $scope.state = newState;
};
~~~~~

## Finishing up

So there it is. The world has been saved. We can now share state between controllers in AngularJS. You can easily manage the logged in user, the user settings, or handle navigation with one controller, and the page content with another!

I put the finished service into a [gist for your viewing pleasure][gist]. But wait, there's more! Here's an implementation of this "pattern" I used recently with the tantalizingly sexy name of [Nsfw][nsfw]. Don't worry, it is set to __false__.

Please don't hesitate to throw me an email at <erik@variadic.me> or a tweet at [@eakron][twitter].

## Edit

I got some great feedback on Reddit and I am here adding a simplified version of the service which simply shares an object. By using `$watch` on this object, we can spot changes and react to these, or just simply binding it on the scope.

~~~~~{.javascript}
angular.module('services', [])
  .factory('state', function () {
    'use strict';

    var state = {};
    
    return {
      state: state,
    };
  });
~~~~~

Using it our controller looks more like this.

~~~~~{.javascript}
angular.module('controllers', ['services'])
  .controller('MainCtrl', function ($scope, state) {
    $scope.state = state.state;
  });
~~~~~

The "big" difference here will be in the markup, where you refer to your state, eg a property called username, as `state.username` rather than just `username`.

In most cases this is plenty! If you need to react to changes in the state object, you can `$watch`.

~~~~~{.javascript}
angular.module('controllers', ['services'])
  .controller('MainCtrl', function ($scope, state) {
    $scope.state = state.state;

    $scope.$watch('state', function (newVal, oldVal) {
      // your code here
    });
  });
~~~~~

[angularjs]: http://angularjs.org/
[di]: http://docs.angularjs.org/guide/di
[misko]: https://twitter.com/mhevery
[concepts]: http://docs.angularjs.org/guide/concepts
[gist]: https://gist.github.com/eakron/6989249
[nsfw]: https://gist.github.com/eakron/6989424
[twitter]: https://twitter.com/eakron
