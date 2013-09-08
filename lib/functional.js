/*
 * Author: Oliver Steele
 * Copyright: Copyright 2007 by Oliver Steele.  All rights reserved.
 * License: MIT License
 * Homepage: http://osteele.com/javascripts/functional
 * Source: http://osteele.com/javascripts/functional/functional.js
 * Changes: http://osteele.com/javascripts/functional/CHANGES
 * Created: 2007-07-11
 * Version: 1.0.2
 *
 *
 * This file defines some higher-order methods and functions for
 * functional and function-level programming.
 */

// rhino compatibility
typeof window == 'undefined' && (window = {});
root = typeof exports !== "undefined" && exports !== null ? exports : this;

// taken from to-function.js

/// ^ String lambdas

/**
 * Turns a string that contains a JavaScript expression into a
 * `Function` that returns the value of that expression.
 *
 * If the string contains a `->`, this separates the parameters from the body:
 * >> 'x -> x + 1'.lambda()(1) -> 2
 * >> 'x y -> x + 2*y'.lambda()(1, 2) -> 5
 * >> 'x, y -> x + 2*y'.lambda()(1, 2) -> 5
 *
 * Otherwise, if the string contains a `_`, this is the parameter:
 * >> '_ + 1'.lambda()(1) -> 2
 *
 * Otherwise if the string begins or ends with an operator or relation,
 * prepend or append a parameter.  (The documentation refers to this type
 * of string as a "section".)
 * >> '/2'.lambda()(4) -> 2
 * >> '2/'.lambda()(4) -> 0.5
 * >> '/'.lambda()(2,4) -> 0.5
 * Sections can end, but not begin with, `-`.  (This is to avoid interpreting
 * e.g. `-2*x` as a section).  On the other hand, a string that either begins
 * or ends with `/` is a section, so an expression that begins or ends with a
 * regular expression literal needs an explicit parameter.
 *
 * Otherwise, each variable name is an implicit parameter:
 * >> 'x + 1'.lambda()(1) -> 2
 * >> 'x + 2*y'.lambda()(1, 2) -> 5
 * >> 'y + 2*x'.lambda()(1, 2) -> 5
 *
 * Implicit parameter detection ignores strings literals, variable names that
 * start with capitals, and identifiers that precede `:` or follow `.`:
 * >> map('"im"+root', ["probable", "possible"]) -> ["improbable", "impossible"]
 * >> 'Math.cos(angle)'.lambda()(Math.PI) -> -1
 * >> 'point.x'.lambda()({x:1, y:2}) -> 1
 * >> '({x:1, y:2})[key]'.lambda()('x') -> 1
 *
 * Implicit parameter detection mistakenly looks inside regular expression
 * literals for variable names.  It also doesn't know to ignore JavaScript
 * keywords and bound variables.  (The only way you can get these last two is
 * with a function literal inside the string.  This is outside the intended use
 * case for string lambdas.)
 *
 * Use `_` (to define a unary function) or `->`, if the string contains anything
 * that looks like a free variable but shouldn't be used as a parameter, or
 * to specify parameters that are ordered differently from their first
 * occurrence in the string.
 *
 * Chain `->`s to create a function in uncurried form:
 * >> 'x -> y -> x + 2*y'.lambda()(1)(2) -> 5
 * >> 'x -> y -> z -> x + 2*y+3*z'.lambda()(1)(2)(3) -> 14
 *
 * `this` and `arguments` are special:
 * >> 'this'.call(1) -> 1
 * >> '[].slice.call(arguments, 0)'.call(null,1,2) -> [1, 2]
 */

String.prototype.lambda = function() {
  var params = [],
    expr = this,
    sections = expr.ECMAsplit(/\s*->\s*/m);
  if (sections.length > 1) {
    while (sections.length) {
      expr = sections.pop();
      params = sections.pop().split(/\s*,\s*|\s+/m);
      sections.length && sections.push('(function('+params+'){return ('+expr+')})');
    }
  } else if (expr.match(/\b_\b/)) {
    params = '_';
  } else {
    // test whether an operator appears on the left (or right), respectively
    var leftSection = expr.match(/^\s*(?:[+*\/%&|\^\.=<>]|!=)/m),
      rightSection = expr.match(/[+\-*\/%&|\^\.=<>!]\s*$/m);
    if (leftSection || rightSection) {
      if (leftSection) {
        params.push('$1');
        expr = '$1' + expr;
      }
      if (rightSection) {
        params.push('$2');
        expr = expr + '$2';
      }
    } else {
      // `replace` removes symbols that are capitalized, follow '.',
      // precede ':', are 'this' or 'arguments'; and also the insides of
      // strings (by a crude test).  `match` extracts the remaining
      // symbols.
      var vars = this.replace(/(?:\b[A-Z]|\.[a-zA-Z_$])[a-zA-Z_$\d]*|[a-zA-Z_$][a-zA-Z_$\d]*\s*:|this|arguments|'(?:[^'\\]|\\.)*'|"(?:[^"\\]|\\.)*"/g, '').match(/([a-z_$][a-z_$\d]*)/gi) || []; // '
      for (var i = 0, v; v = vars[i++]; )
        params.indexOf(v) >= 0 || params.push(v);
    }
  }
  return new Function(params, 'return (' + expr + ')');
}

/// Turn on caching for `string` -> `Function` conversion.
String.prototype.lambda.cache = function() {
  var proto = String.prototype,
    cache = {},
    uncached = proto.lambda,
    cached = function() {
      var key = '#' + this; // avoid hidden properties on Object.prototype
      return cache[key] || (cache[key] = uncached.call(this));
    };
  cached.cached = function(){};
  cached.uncache = function(){proto.lambda = uncached};
  proto.lambda = cached;
}

/**
 * ^^ Duck-Typing
 *
 * Strings support `call` and `apply`.  This duck-types them as
 * functions, to some callers.
 */

/**
 * Coerce the string to a function and then apply it.
 * >> 'x+1'.apply(null, [2]) -> 3
 * >> '/'.apply(null, [2, 4]) -> 0.5
 */
String.prototype.apply = function(thisArg, args) {
  return this.toFunction().apply(thisArg, args);
}

/**
 * Coerce the string to a function and then call it.
 * >> 'x+1'.call(null, 2) -> 3
 * >> '/'.call(null, 2, 4) -> 0.5
 */
String.prototype.call = function() {
  return this.toFunction().apply(arguments[0],
                   Array.prototype.slice.call(arguments, 1));
}

/// ^^ Coercion

/**
 * Returns a `Function` that perfoms the action described by this
 * string.  If the string contains a `return`, applies
 * `new Function` to it.  Otherwise, this function returns
 *  the result of `this.lambda()`.
 * >> '+1'.toFunction()(2) -> 3
 * >> 'return 1'.toFunction()(1) -> 1
 */
String.prototype.toFunction = function() {
  var body = this;
  if (body.match(/\breturn\b/))
    return new Function(this);
  return this.lambda();
}

/**
 * Returns this function.  `Function.toFunction` calls this.
 * >> '+1'.lambda().toFunction()(2) -> 3
 */
Function.prototype.toFunction = function() {
  return this;
}

/**
 * Coerces `fn` into a function if it is not already one,
 * by calling its `toFunction` method.
 * >> Function.toFunction(function() {return 1})() -> 1
 * >> Function.toFunction('+1')(2) -> 3
 *
 * `Function.toFunction` requires an argument that can be
 * coerced to a function.  A nullary version can be
 * constructed via `guard`:
 * >> Function.toFunction.guard()('1+') -> function()
 * >> Function.toFunction.guard()(null) -> null
 *
 * `Function.toFunction` doesn't coerce arbitrary values to functions.
 * It might seem convenient to treat
 * `Function.toFunction(value)` as though it were the
 * constant function that returned `value`, but it's rarely
 * useful and it hides errors.  Use `Functional.K(value)` instead,
 * or a lambda string when the value is a compile-time literal:
 * >> Functional.K('a string')() -> "a string"
 * >> Function.toFunction('"a string"')() -> "a string"
 */
Function.toFunction = function(value) {
  return value.toFunction();
}

// Utilities

// IE6 split is not ECMAScript-compliant.  This breaks '->1'.lambda().
// ECMAsplit is an ECMAScript-compliant `split`, although only for
// one argument.
String.prototype.ECMAsplit =
  // The test is from the ECMAScript reference.
  ('ab'.split(/a*/).length > 1
   ? String.prototype.split
   : function(separator, limit) {
     if (typeof limit != 'undefined')
       throw "ECMAsplit: limit is unimplemented";
     var result = this.split.apply(this, arguments),
       re = RegExp(separator),
       savedIndex = re.lastIndex,
       match = re.exec(this);
     if (match && match.index == 0)
       result.unshift('');
     // in case `separator` was already a RegExp:
     re.lastIndex = savedIndex;
     return result;
   });


/// `Functional` is the namespace for higher-order functions.
Functional = root.Functional || {};

/**
 * This function copies all the public functions in `Functional` except itself
 * into the global namespace.  If the optional argument $except$ is present,
 * functions named by its property names are not copied.
 * >> Functional.install()
 */
Functional.install = function(except) {
  var source = Functional,
    target = root;
  for (var name in source)
    name == 'install'
    || name.charAt(0) == '_'
    || except && name in except
    || {}[name] // work around Prototype
    || (target[name] = source[name]);
}

/// ^ Higher-order functions

/**
 * Returns a function that applies the last argument of this
 * function to its input, and the penultimate argument to the
 * result of the application, and so on.
 * == compose(f1, f2, f3..., fn)(args) == f1(f2(f3(...(fn(args...)))))
 * :: (a2 -> a1) (a3 -> a2)... (a... -> a_{n}) -> a... -> a1
 * >> compose('1+', '2*')(2) -> 5
 */
Functional.compose = function(/*fn...*/) {
  var fns = Functional.map(Function.toFunction, arguments),
    arglen = fns.length;
  return function() {
    for (var i = arglen; --i >= 0; )
      arguments = [fns[i].apply(this, arguments)];
    return arguments[0];
  }
}

/**
 * Same as `compose`, except applies the functions in argument-list order.
 * == sequence(f1, f2, f3..., fn)(args...) == fn(...(f3(f2(f1(args...)))))
 * :: (a... -> a1) (a1 -> a2) (a2 -> a3)... (a_{n-1} -> a_{n})  -> a... -> a_{n}
 * >> sequence('1+', '2*')(2) -> 6
 */
Functional.sequence = function(/*fn...*/) {
  var fns = Functional.map(Function.toFunction, arguments),
    arglen = fns.length;
  return function() {
    for (var i = 0; i < arglen; i++)
      arguments = [fns[i].apply(this, arguments)];
    return arguments[0];
  }
}

/**
 * Applies `fn` to each element of `sequence`.
 * == map(f, [x1, x2...]) = [f(x, 0), f(x2, 1), ...]
 * :: (a ix -> boolean) [a] -> [a]
 * >> map('1+', [1,2,3]) -> [2, 3, 4]
 *
 * If `object` is supplied, it is the object of the call.
 *
 * The fusion rule:
 * >> map('+1', map('*2', [1,2,3])) -> [3, 5, 7]
 * >> map(compose('+1', '*2'), [1,2,3]) -> [3, 5, 7]
 */
Functional.map = function(fn, sequence, object) {
  fn = Function.toFunction(fn);
  var len = sequence.length,
    result = new Array(len);
  for (var i = 0; i < len; i++)
    result[i] = fn.apply(object, [sequence[i], i]);
  return result;
}

/**
 * Applies `fn` to `init` and the first element of `sequence`,
 * and then to the result and the second element, and so on.
 * == reduce(f, init, [x0, x1, x2]) == f(f(f(init, x0), x1), x2)
 * :: (a b -> a) a [b] -> a
 * >> reduce('x y -> 2*x+y', 0, [1,0,1,0]) -> 10
 */
Functional.reduce = function(fn, init, sequence, object) {
  fn = Function.toFunction(fn);
  var len = sequence.length,
    result = init;
  for (var i = 0; i < len; i++)
    result = fn.apply(object, [result, sequence[i]]);
  return result;
}

/**
 * Returns a list of those elements $x$ of `sequence` such that
 * $fn(x)$ returns true.
 * :: (a -> boolean) [a] -> [a]
 * >> select('%2', [1,2,3,4]) -> [1, 3]
 */
Functional.select = function(fn, sequence, object) {
  fn = Function.toFunction(fn);
  var len = sequence.length,
    result = [];
  for (var i = 0; i < len; i++) {
    var x = sequence[i];
    fn.apply(object, [x, i]) && result.push(x);
  }
  return result;
}

/// A synonym for `select`.
Functional.filter = Functional.select;

/// A synonym for `reduce`.
Functional.foldl = Functional.reduce;

/**
 * Same as `foldl`, but applies the function from right to left.
 * == foldr(f, init, [x0, x1, x2]) == fn(x0, f(x1, f(x2, init)))
 * :: (a b -> b) b [a] -> b
 * >> foldr('x y -> 2*x+y', 100, [1,0,1,0]) -> 104
 */
Functional.foldr = function(fn, init, sequence, object) {
  fn = Function.toFunction(fn);
  var len = sequence.length,
    result = init;
  for (var i = len; --i >= 0; )
    result = fn.apply(object, [sequence[i], result]);
  return result;
}

/// ^^ Predicates

/**
 * Returns a function that returns `true` when all the arguments, applied
 * to the returned function's arguments, returns true.
 * == and(f1, f2...)(args...) == f1(args...) && f2(args...)...
 * :: [a -> boolean] a -> a
 * >> and('>1', '>2')(2) -> false
 * >> and('>1', '>2')(3) -> true
 * >> and('>1', 'error()')(1) -> false
 */
Functional.and = function(/*functions...*/) {
  var args = Functional.map(Function.toFunction, arguments),
    arglen = args.length;
  return function() {
    var value = true;
    for (var i = 0; i < arglen; i++)
      if (!(value = args[i].apply(this, arguments)))
        break;
    return value;
  }
}

/**
 * Returns a function that returns `true` when any argument, applied
 * to the returned function's arguments, returns true.
 * == or(f1, f2...)(args...) == f1(args...) || f2(args...)...
 * :: [a -> boolean] a -> a
 * >> or('>1', '>2')(1) -> false
 * >> or('>1', '>2')(2) -> true
 * >> or('>1', 'error()')(2) -> true
 */
Functional.or = function(/*functions...*/) {
  var args = Functional.map(Function.toFunction, arguments),
    arglen = args.length;
  return function() {
    var value = false;
    for (var i = 0; i < arglen; i++)
      if ((value = args[i].apply(this, arguments)))
        break;
    return value;
  }
}

/**
 * Returns true when $fn(x)$ returns true for some element $x$ of
 * `sequence`.  The returned function short-circuits.
 * == some(f, [x1, x2, x3, ...]) == f(x1) || f(x2) || f(x3)...
 * :: (a -> boolean) [a] -> boolean
 * >> some('>2', [1,2,3]) -> true
 * >> some('>10', [1,2,3]) -> false
 */
Functional.some = function(fn, sequence, object) {
  fn = Function.toFunction(fn);
  var len = sequence.length,
    value = false;
  for (var i = 0; i < len; i++)
    if ((value = fn.call(object, sequence[i])))
      break;
  return value;
}

/**
 * Returns true when $fn(x)$ returns true for every element $x$ of
 * `sequence`.  The returned function short-circuits.
 * == every(f, [x1, x2, x3, ...]) == f(x1) && f(x2) && f(x3)...
 * :: (a -> boolean) [a] -> boolean
 * >> every('<2', [1,2,3]) -> false
 * >> every('<10', [1,2,3]) -> true
 */
Functional.every = function(fn, sequence, object) {
  fn = Function.toFunction(fn);
  var len = sequence.length,
    value = true;
  for (var i = 0; i < len; i++)
    if (!(value = fn.call(object, sequence[i])))
      break;
  return value;
}

/**
 * Returns a function that returns `true` when $fn()$ returns false.
 * == f.not()(args...) == !f(args...)
 * :: (a -> boolean) -> (a -> boolean)
 * >> not(Functional.K(true))() -> false
 * >> not(Functional.K(false))() -> true
 */
Functional.not = function(fn) {
  fn = Function.toFunction(fn);
  return function() {
    return !fn.apply(null, arguments);
  }
}

/**
 * Returns a function that returns true when this function's arguments
 * applied to that functions are always the same.  The returned function
 * short-circuits.
 * == equal(f1, f2...)(args...) == f1(args...) == f2(args...)...
 * :: [a... -> b] -> a... -> b
 * >> equal()() -> true
 * >> equal(K(1))() -> true
 * >> equal(K(1), K(1))() -> true
 * >> equal(K(1), K(2))() -> false
 * >> equal(K(1), K(2), 'error()')() -> false
 */
Functional.equal = function(/*fn...*/) {
  var arglen = arguments.length,
    args = Functional.map(Function.toFunction, arguments);
  if (!arglen) return Functional.K(true);
  // if arglen == 1 it's also constant true, but
  // call it for effect.
  return function() {
    var value = args[0].apply(this, arguments);
    for (var i = 1; i < arglen; i++)
      if (value != args[i].apply(this, args))
        return false;
    return true;
  }
}


/// ^^ Utilities

/**
  * Returns its argument coerced to a function.
  * >> lambda('1+')(2) -> 3
  * >> lambda(function(n){return n+1})(2) -> 3
  */
Functional.lambda = function(object) {
  return object.toFunction();
}


/**
 * Returns a function that takes an object as an argument, and applies
 * `object`'s `methodName` method to `arguments`.
 * == invoke(name)(object, args...) == object[name](args...)
 * :: name args... -> object args2... -> object[name](args... args2...)
 * >> invoke('toString')(123) -> "123"
 */
Functional.invoke = function(methodName/*, arguments*/) {
  var args = Array.slice(arguments, 1);
  return function(object) {
    return object[methodName].apply(object, Array.slice(arguments, 1).concat(args));
  }
}

/**
 * Returns a function that takes an object, and returns the value of its
 * `name` property.  `pluck(name)` is equivalent to `'_.name'.lambda()`.
 * == pluck(name)(object) == object[name]
 * :: name -> object -> object[name]
 * >> pluck('length')("abc") -> 3
 */
Functional.pluck = function(name) {
  return function(object) {
    return object[name];
  }
}

/**
 * Returns a function that, while $pred(value)$ is true, applies `fn` to
 * $value$ to produce a new value, which is used as an input for the next round.
 * The returned function returns the first $value$ for which $pred(value)$
 * is false.
 * :: (a -> boolean) (a -> a) -> a
 * >> until('>10', '2*')(1) -> 16
 */
Functional.until = function(pred, fn) {
  fn = Function.toFunction(fn);
  pred = Function.toFunction(pred);
  return function(value) {
    while (!pred.call(null, value))
      value = fn.call(null, value);
    return value;
  }
}

/**
 * :: [a] [b]... -> [[a b]...]
 * == zip(a, b...) == [[a0, b0], [a1, b1], ...]
 * Did you know that `zip` can transpose a matrix?
 * >> zip.apply(null, [[1,2],[3,4]]) -> [[1, 3], [2, 4]]
 */
Functional.zip = function(/*args...*/) {
  var n = Math.min.apply(null, Functional.map('.length', arguments));
  var results = new Array(n);
  for (var i = 0; i < n; i++) {
    var key = String(i);
    results[key] = Functional.map(pluck(key), arguments);
  };
  return results;
}

Functional._startRecordingMethodChanges = function(object) {
  var initialMethods = {};
  for (var name in object)
    initialMethods[name] = object[name];
  return {getChangedMethods: function() {
    var changedMethods = {};
    for (var name in object)
    if (object[name] != initialMethods[name])
      changedMethods[name] = object[name];
    return changedMethods;
  }};
}

// For each method that this file defined on `Function.prototype`,
// define a function on `Functional` that delegates to it.
Functional._attachMethodDelegates = function(methods) {
  for (var name in methods)
    Functional[name] = Functional[name] || (function(name) {
      var fn = methods[name];
      return function(object) {
        return fn.apply(Function.toFunction(object), Array.slice(arguments, 1));
      }
    })(name);
}

// Record the current contents of `Function.prototype`, so that we
// can see what we've added later.
Functional.__initalFunctionState = Functional._startRecordingMethodChanges(Function.prototype);

/// ^ Higher-order methods

/// ^^ Partial function application

/**
 * Returns a bound method on `object`, optionally currying `args`.
 * == f.bind(obj, args...)(args2...) == f.apply(obj, [args..., args2...])
 */
// TSC: disabling this b/c it breaks some meteor code
// Function.prototype.bind = function(object/*, args...*/) {
//   var fn = this;
//   var args = Array.slice(arguments, 1);
//   return function() {
//     return fn.apply(object, args.concat(Array.slice(arguments, 0)));
//   }
// }

/**
 * Returns a function that applies the underlying function to `args`, and
 * ignores its own arguments.
 * :: (a... -> b) a... -> (... -> b)
 * == f.saturate(args...)(args2...) == f(args...)
 * >> Math.max.curry(1, 2)(3, 4) -> 4
 * >> Math.max.saturate(1, 2)(3, 4) -> 2
 * >> Math.max.curry(1, 2).saturate()(3, 4) -> 2
 */
Function.prototype.saturate = function(/*args*/) {
  var fn = this;
  var args = Array.slice(arguments, 0);
  return function() {
    return fn.apply(this, args);
  }
}

/**
 * Invoking the function returned by this function only passes `n`
 * arguments to the underlying function.  If the underlying function
 * is not saturated, the result is a function that passes all its
 * arguments to the underlying function.  (That is, `aritize` only
 * affects its immediate caller, and not subsequent calls.)
 * >> '[a,b]'.lambda()(1,2) -> [1, 2]
 * >> '[a,b]'.lambda().aritize(1)(1,2) -> [1, undefined]
 * >> '+'.lambda()(1,2)(3) -> error
 * >> '+'.lambda().ncurry(2).aritize(1)(1,2)(3) -> 4
 *
 * `aritize` is useful to remove optional arguments from a function that
 * is passed to a higher-order function that supplies *different* optional
 * arguments.
 *
 * For example, many implementations of `map` and other collection
 * functions, including those in this library, call the function argument
 *  with both the collection element
 * and its position.  This is convenient when expected, but can wreak
 * havoc when the function argument is a curried function that expects
 * a single argument from `map` and the remaining arguments from when
 * the result of `map` is applied.
 */
Function.prototype.aritize = function(n) {
  var fn = this;
  return function() {
    return fn.apply(this, Array.slice(arguments, 0, n));
  }
}

/**
 * Returns a function that, applied to an argument list $arg2$,
 * applies the underlying function to $args ++ arg2$.
 * :: (a... b... -> c) a... -> (b... -> c)
 * == f.curry(args1...)(args2...) == f(args1..., args2...)
 *
 * Note that, unlike in languages with true partial application such as Haskell,
 * `curry` and `uncurry` are not inverses.  This is a repercussion of the
 * fact that in JavaScript, unlike Haskell, a fully saturated function is
 * not equivalent to the value that it returns.  The definition of `curry`
 * here matches semantics that most people have used when implementing curry
 * for procedural languages.
 *
 * This implementation is adapted from
 * [http://www.coryhudson.com/blog/2007/03/10/javascript-currying-redux/].
 */
Function.prototype.curry = function(/*args...*/) {
  var fn = this;
  var args = Array.slice(arguments, 0);
  return function() {
    return fn.apply(this, args.concat(Array.slice(arguments, 0)));
  };
}

/*
 * Right curry.  Returns a function that, applied to an argument list $args2$,
 * applies the underlying function to $args2 + args$.
 * == f.curry(args1...)(args2...) == f(args2..., args1...)
 * :: (a... b... -> c) b... -> (a... -> c)
 */
Function.prototype.rcurry = function(/*args...*/) {
  var fn = this;
  var args = Array.slice(arguments, 0);
  return function() {
    return fn.apply(this, Array.slice(arguments, 0).concat(args));
  };
}

/**
 * Same as `curry`, except only applies the function when all
 * `n` arguments are saturated.
 */
Function.prototype.ncurry = function(n/*, args...*/) {
  var fn = this;
  var largs = Array.slice(arguments, 1);
  return function() {
    var args = largs.concat(Array.slice(arguments, 0));
    if (args.length < n) {
      args.unshift(n);
      return fn.ncurry.apply(fn, args);
    }
    return fn.apply(this, args);
  };
}

/**
 * Same as `rcurry`, except only applies the function when all
 * `n` arguments are saturated.
 */
Function.prototype.rncurry = function(n/*, args...*/) {
  var fn = this;
  var rargs = Array.slice(arguments, 1);
  return function() {
    var args = Array.slice(arguments, 0).concat(rargs);
    if (args.length < n) {
      args.unshift(n);
      return fn.rncurry.apply(fn, args);
    }
    return fn.apply(this, args);
  };
}

/**
 * `_` (underscore) is bound to a unique value for use in `partial`, below.
 * This is a global variable, but it's also a property of `Function` in case
 * you overwrite or bind over the global one.
 */
// _ = Function._ = {};

Function._ = {};

/**
 * Returns a function $f$ such that $f(args2)$ is equivalent to
 * the underlying function applied to a combination of $args$ and $args2$.
 *
 * `args` is a partially-specified argument: it's a list with "holes",
 * specified by the special value `_`.  It is combined with $args2$ as
 * follows:
 *
 * From left to right, each value in $args2$ fills in the leftmost
 * remaining hole in `args`.  Any remaining values
 * in $args2$ are appended to the result of the filling-in process
 * to produce the combined argument list.
 *
 * If the combined argument list contains any occurrences of `_`, the result
 * of the application of $f$ is another partial function.  Otherwise, the
 * result is the same as the result of applying the underlying function to
 * the combined argument list.
 */
Function.prototype.partial = function(/*args*/) {
  var fn = this;
  var _ = Function._;
  var args = Array.slice(arguments, 0);
  //substitution positions
  var subpos = [], value;
  for (var i = 0; i < arguments.length; i++)
    arguments[i] == _ && subpos.push(i);
  return function() {
    var specialized = args.concat(Array.slice(arguments, subpos.length));
    for (var i = 0; i < Math.min(subpos.length, arguments.length); i++)
      specialized[subpos[i]] = arguments[i];
    for (var i = 0; i < specialized.length; i++)
      if (specialized[i] == _)
        return fn.partial.apply(fn, specialized);
    return fn.apply(this, specialized);
  }
}

/// ^^ Combinators

/// ^^^ Combinator Functions

/**
 * The identity function: $x -> x$.
 * == I(x) == x
 * == I == 'x'.lambda()
 * :: a -> a
 * >> Functional.I(1) -> 1
 */
Functional.I = function(x) {return x};

/**
 * Returns a constant function that returns `x`.
 * == K(x)(y) == x
 * :: a -> b -> a
 * >> Functional.K(1)(2) -> 1
 */
Functional.K = function(x) {return function() {return x}};

/// A synonym for `Functional.I`
Functional.id = Functional.I;

/// A synonym for `Functional.K`
Functional.constfn = Functional.K;


/**
 * Returns a function that applies the first function to the
 * result of the second, but passes all its arguments too.
 * == S(f, g)(args...) == f(g(args...), args...)
 *
 * This is useful for composing functions when each needs access
 * to the arguments to the composed function.  For example,
 * the following function multiples its last two arguments,
 * and adds the first to that.
 * >> Function.S('+', '_ a b -> a*b')(2,3,4) -> 14
 *
 * Curry this to get a version that takes its arguments in
 * separate calls:
 * >> Function.S.curry('+')('_ a b -> a*b')(2,3,4) -> 14
 */
Function.S = function(f, g) {
  f = Function.toFunction(f);
  g = Function.toFunction(g);
  return function() {
    return f.apply(this, [g.apply(this, arguments)].concat(Array.slice(arguments, 0)));
  }
}

/// ^^^ Combinator methods

/**
 * Returns a function that swaps its first two arguments before
 * passing them to the underlying function.
 * == f.flip()(a, b, c...) == f(b, a, c...)
 * :: (a b c...) -> (b a c...)
 * >> ('a/b'.lambda()).flip()(1,2) -> 2
 *
 * For more general derangements, you can also use `prefilterSlice`
 * with a string lambda:
 * >> '100*a+10*b+c'.lambda().prefilterSlice('a b c -> [b, c, a]')(1,2,3) -> 231
 */
Function.prototype.flip = function() {
  var fn = this;
  return function() {
    var args = Array.slice(arguments, 0);
    args = args.slice(1,2).concat(args.slice(0,1)).concat(args.slice(2));
    return fn.apply(this, args);
  }
}

/**
 * Returns a function that applies the underlying function to its
 * first argument, and the result of that application to the remaining
 * arguments.
 * == f.uncurry(a, b...) == f(a)(b...)
 * :: (a -> b -> c) -> (a, b) -> c
 * >> 'a -> b -> a/b'.lambda().uncurry()(1,2) -> 0.5
 *
 * Note that `uncurry` is *not* the inverse of `curry`.
 */
Function.prototype.uncurry = function() {
  var fn = this;
  return function() {
    var f1 = fn.apply(this, Array.slice(arguments, 0, 1));
    return f1.apply(this, Array.slice(arguments, 1));
  }
}

/**
 * ^^ Filtering
 *
 * Filters intercept a value before it is passed to a function, and apply the
 * underlying function to the modified value.
 */

/**
 * `prefilterObject` returns a function that applies the underlying function
 * to the same arguments, but to an object that is the result of appyling
 * `filter` to the invocation object.
 * == fn.prefilterObject(filter).apply(object, args...) == fn.apply(filter(object), args...)
 * == fn.bind(object) == compose(fn.prefilterObject, Functional.K(object))
 * >> 'this'.lambda().prefilterObject('n+1').apply(1) -> 2
 */
Function.prototype.prefilterObject = function(filter) {
  filter = Function.toFunction(filter);
  var fn = this;
  return function() {
    return fn.apply(filter(this), arguments);
  }
}

/**
 * `prefilterAt` returns a function that applies the underlying function
 * to a copy of the arguments, where the `index`th argument has been
 * replaced by the value of `filter(argument[index])`.
 * == fn.prefilterAt(i, filter)(a1, a2, ..., a_{n}) == fn(a1, a2, ..., filter(a_{i}), ..., a_{n})
 * >> '[a,b,c]'.lambda().prefilterAt(1, '2*')(2,3,4) -> [2, 6, 4]
 */
Function.prototype.prefilterAt = function(index, filter) {
  filter = Function.toFunction(filter);
  var fn = this;
  return function() {
    var args = Array.slice(arguments, 0);
    args[index] = filter.call(this, args[index]);
    return fn.apply(this, args);
  }
}

/**
 * `prefilterSlice` returns a function that applies the underlying function
 * to a copy of the arguments, where the arguments `start` through
 * `end` have been replaced by the value of `filter(argument.slice(start,end))`,
 * which must return a list.
 * == fn.prefilterSlice(i0, i1, filter)(a1, a2, ..., a_{n}) == fn(a1, a2, ..., filter(args_{i0}, ..., args_{i1}), ..., a_{n})
 * >> '[a,b,c]'.lambda().prefilterSlice('[a+b]', 1, 3)(1,2,3,4) -> [1, 5, 4]
 * >> '[a,b]'.lambda().prefilterSlice('[a+b]', 1)(1,2,3) -> [1, 5]
 * >> '[a]'.lambda().prefilterSlice(compose('[_]', Math.max))(1,2,3) -> [3]
 */
Function.prototype.prefilterSlice = function(filter, start, end) {
  filter = Function.toFunction(filter);
  start = start || 0;
  var fn = this;
  return function() {
    var args = Array.slice(arguments, 0);
    var e = end < 0 ? args.length + end : end || args.length;
    args.splice.apply(args, [start, (e||args.length)-start].concat(filter.apply(this, args.slice(start, e))));
    return fn.apply(this, args);
  }
}

/// ^^ Method Composition

/**
 * `compose` returns a function that applies the underlying function
 * to the result of the application of `fn`.
 * == f.compose(g)(args...) == f(g(args...))
 * >> '1+'.lambda().compose('2*')(3) -> 7
 *
 * Note that, unlike `Functional.compose`, the `compose` method on
 * function only takes a single argument.
 * == Functional.compose(f, g) == f.compose(g)
 * == Functional.compose(f, g, h) == f.compose(g).compose(h)
 */
Function.prototype.compose = function(fn) {
  var self = this;
  fn = Function.toFunction(fn);
  return function() {
    return self.apply(this, [fn.apply(this, arguments)]);
  }
}

/**
 * `sequence` returns a function that applies the underlying function
 * to the result of the application of `fn`.
 * == f.sequence(g)(args...) == g(f(args...))
 * == f.sequence(g) == g.compose(f)
 * >> '1+'.lambda().sequence('2*')(3) -> 8
 *
 * Note that, unlike `Functional.compose`, the `sequence` method on
 * function only takes a single argument.
 * == Functional.sequence(f, g) == f.sequence(g)
 * == Functional.sequence(f, g, h) == f.sequence(g).sequence(h)
 */
Function.prototype.sequence = function(fn) {
  var self = this;
  fn = Function.toFunction(fn);
  return function() {
    return fn.apply(this, [self.apply(this, arguments)]);
  }
}

/**
 * Returns a function that is equivalent to the underlying function when
 * `guard` returns true, and otherwise is equivalent to the application
 * of `otherwise` to the same arguments.
 *
 * `guard` and `otherwise` default to `Functional.I`.  `guard` with
 * no arguments therefore returns a function that applies the
 * underlying function to its value only if the value is true,
 * and returns the value otherwise.
 * == f.guard(g, h)(args...) == f(args...), when g(args...) is true
 * == f.guard(g ,h)(args...) == h(args...), when g(args...) is false
 * >> '[_]'.lambda().guard()(1) -> [1]
 * >> '[_]'.lambda().guard()(null) -> null
 * >> '[_]'.lambda().guard(null, Functional.K('n/a'))(null) -> "n/a"
 * >> 'x+1'.lambda().guard('<10', Functional.K(null))(1) -> 2
 * >> 'x+1'.lambda().guard('<10', Functional.K(null))(10) -> null
 * >> '/'.lambda().guard('p q -> q', Functional.K('n/a'))(1, 2) -> 0.5
 * >> '/'.lambda().guard('p q -> q', Functional.K('n/a'))(1, 0) -> "n/a"
 * >> '/'.lambda().guard('p q -> q', '-> "n/a"')(1, 0) -> "n/a"
 */
Function.prototype.guard = function(guard, otherwise) {
  var fn = this;
  guard = Function.toFunction(guard || Functional.I);
  otherwise = Function.toFunction(otherwise || Functional.I);
  return function() {
    return (guard.apply(this, arguments) ? fn : otherwise).apply(this, arguments);
  }
}

/// ^^ Utilities

/**
 * Returns a function identical to this function except that
 * it prints its arguments on entry and its return value on exit.
 * This is useful for debugging function-level programs.
 */
Function.prototype.traced = function(name) {
  var self = this;
  name = name || self;
  return function() {
    root.console && console.info('[', name, 'apply(', this!=root && this, ',', arguments, ')');
    var result = self.apply(this, arguments);
    root.console && console.info(']', name, ' -> ', result);
    return result;
  }
}


/**
 * ^^ Function methods as functions
 *
 * In addition to the functions defined above, every method defined
 * on `Function` is also available as a function in `Functional`, that
 * coerces its first argument to a `Function` and applies
 * the remaining arguments to this.
 *
 * A few examples make this clearer:
 * == curry(fn, args...) == fn.curry(args...)
 * >> Functional.flip('a/b')(1, 2) -> 2
 * >> Functional.curry('a/b', 1)(2) -> 0.5

 * For each method that this file defined on Function.prototype,
 * define a function on Functional that delegates to it.
 */
Functional._attachMethodDelegates(Functional.__initalFunctionState.getChangedMethods());
delete Functional.__initalFunctionState;


// In case to-function.js isn't loaded.
Function.toFunction = Function.toFunction || Functional.K;

if (!Array.slice) { // mozilla already supports this
  Array.slice = (function(slice) {
    return function(object) {
      return slice.apply(object, slice.call(arguments, 1));
    };
  })(Array.prototype.slice);
}

Functional.autoCurry = function (fn, numArgs) {
  var fn = fn.toFunction();
  numArgs = numArgs || fn.length;
  var f = function () {
    if (arguments.length < numArgs) {
    return Functional.autoCurry(Functional.curry.apply(this, [fn].concat(Array.slice(arguments,0))),
      numArgs - arguments.length);
    } else {
    return fn.apply(this, arguments);
    }
  };
  f.toString = function() { return fn.toString(); };
  return f;
}

Function.prototype.autoCurry = function (n) {
  return Functional.autoCurry(this, n);
};

Function.prototype.p = Function.prototype.partial;


Functional.memoize = function (fn) {
  return function () {
    var args = Array.prototype.slice.call(arguments),
        hash = "",
        i = args.length;
    currentArg = null;
    while (i--) {
        currentArg = args[i];
        hash += (currentArg === Object(currentArg)) ?
        JSON.stringify(currentArg) : currentArg;
        fn.memoize || (fn.memoize = {});
    }
    return (hash in fn.memoize) ? fn.memoize[hash] :
    fn.memoize[hash] = fn.apply(this, args);
  };
}


Functional.orig = {};
Functional.orig.map     = Functional.map;
Functional.orig.reduce  = Functional.reduce;
Functional.orig.select  = Functional.select;
Functional.orig.guard   = Functional.guard;
Functional.orig.foldr   = Functional.foldr;
Functional.orig.some    = Functional.some;
Functional.orig.every   = Functional.every;
Functional.orig.until   = Functional.until;

// autoCurry-ize
Functional.map    = Functional.map.autoCurry(2);
Functional.reduce = Functional.reduce.autoCurry(3);
Functional.select = Functional.select.autoCurry(2);
Functional.guard  = Functional.guard.autoCurry(2);
Functional.foldr  = Functional.foldr.autoCurry(3);
Functional.some   = Functional.some.autoCurry(2);
Functional.every  = Functional.every.autoCurry(2);
Functional.until  = Functional.until.autoCurry();

// alias for coffeescript
Functional.and_   = Functional.and;
Functional.or_    = Functional.or;
Functional.until_ = Functional.until;

// adding in some other functionality
Functional.S = Function.S;
var F = Functional;
F._ = Function._;

// helpers
Functional.equals = function(x, y) {
  return x === y;
}.autoCurry();

Functional.isArray = function(obj) {
  return (obj && obj.constructor == Array);
}

Functional.isObj = function(obj) {
  return (typeof obj == "object" && !isArray(obj));
}

Functional.isNumber = function(n) {
  return !isNaN(parseFloat(n)) && isFinite(n);
}

Functional.nTimes = function(times, fun) {
  var result = [];
  for(var i=0;i<times;i++ ){ result = Functional.cons(fun(), result); }
  return result;
}.autoCurry();

Functional.log = function(what) {
  console.log(what);
  return what;
}

// Array
Functional.take = function(n, xs) {
  return xs.slice(0, n);
}.autoCurry();

Functional.drop = function(n, xs) {
  return xs.slice(n, xs.length);
}.autoCurry();

Functional.unshift = function(xs, x) {
  return x.concat(xs);
}.autoCurry();

Functional.cons = function(x, xs) {
  return [x].concat(xs);
}.autoCurry();

Functional.concat = function(xs, ys) {
  return xs.concat(ys);
}.autoCurry();

Functional.first = function(xs) {
  return xs[0];
}

Functional.rest = function(xs) {
  return (typeof xs == "string") ? xs.substr(1, xs.length) : xs.slice(1, xs.length);
}

Functional.last = function(xs) {
  return xs[xs.length -1];
}

Functional.join = function(token, xs) {
  return xs.join(token);
}.autoCurry()

Functional.groupsOf = function(n, xs) {
  if(!xs.length) return [];
  return Functional.cons(take(n, xs), groupsOf(n, drop(n,xs)));
}.autoCurry()

Functional.zipWith = function(f, xs, ys) {
  f = f.toFunction();
  return xs.reduce(function(result, x, i){
    return result.concat(f(x, ys[i]));
  }, []);
}.autoCurry();

Functional.uniq = function(xs) {
  var result = [];
  for(var i=0;i<xs.length;i++ ) { if(result.indexOf(xs[i]) < 0) result.push(xs[i]); };
  return result;
}

Functional.uniqBy = function(fun, xs) {
  var result = [], len = xs.length, fun = fun.toFunction();
  for(var i=0;i<len;i++ ) {
    if(Functional.map(fun)(result).indexOf(fun(xs[i])) < 0) {
      result.push(xs[i]);
    }
  };
  return result;
}.autoCurry();

Functional.reverse = function(xs) {
  var mempty = (typeof xs == "string") ? "" : [];
  if(Functional.isArray(xs)) {
    //destructive
    return xs.reverse();
  } else {
    return Functional.reduce(function(x, acc){ return acc.concat(x); }, mempty, xs);
  }
}.autoCurry();

Functional.sort = function(xs) {
  //destructive
  is_num_array = reduce(andand, true, map(isNumber,xs));
  num_sort_func = function(a,b) {return a - b;};
  return is_num_array ? xs.sort(num_sort_func) : xs.sort();
}

Functional.element = function(arr, x) {
  return arr.indexOf(x) >= 0;
}.autoCurry();

Functional.flatten = Functional.reduce(function(a,b){return a.concat(b);}, []);

Functional.sortBy = function(fun, xs) {
  var _sortBy = function(iterator, xs, context) {
    return map('.value', map(function(value, index) {
      return {
        value: value,
        criteria: iterator.call(context, value, index)
      };
    }, xs).sort(function(left, right) {
      var a = left.criteria, b = right.criteria;
      return a < b ? -1 : a > b ? 1 : 0;
    }));
  }
  var f = fun.toFunction();
  return _sortBy(f, xs);
}.autoCurry();

Functional.groupBy = function(fun, xs) {
  var f = fun.toFunction();
  var _makeHash = function(obj, x) {
    var val = f(x);
    if(!obj[val]) obj[val] = [];
    obj[val].push(x);
    return obj;
  }

  return reduce(_makeHash, {}, xs);
}.autoCurry();

// String
Functional.strip = function(str) {
  return str.replace(/\s+/g, "");
}

Functional.split = function(token, xs) {
  return xs.split(token);
}.autoCurry();

Functional.test = function(expr, x) {
  return expr.test(x);
}.autoCurry();

Functional.match = function(expr, x) {
  return x.match(expr);
}.autoCurry();

Functional.replace = function(pattern, sub, str) {
  return str.replace(pattern, sub);
}.autoCurry();

// Conditional
Functional.when = function(pred, f) {
  return function() {
    if(pred.apply(this, arguments)) return f.apply(this, arguments);
  }
}.autoCurry();

Functional.ifelse = function(pred, f, g) {
  return function() {
    return pred.apply(this, arguments) ? f.apply(this, arguments) : g.apply(this, arguments);
  }
}.autoCurry();

Functional.negate = function(bool) {
  return !bool;
}

Functional.andand = function(x, y) {
  return x && y;
}.autoCurry();

Functional.oror = function(x, y) {
  return x || y;
}.autoCurry();

// Object
Functional.setVal = function(attribute, x, val) {
  x[attribute] = val;
  return val;
}.autoCurry();

Functional.getVal = function(attribute, x) {
  return function(){ return x[attribute]; };
}.autoCurry();

Functional.merge = function(x,y) {
  var target = {};
  for(property in x) target[property] = x[property];

  for(property in y) {
    if(Functional.isObj(y[property])) {
      merge(target[property], y[property]);
    } else {
      if(target && y) target[property] = y[property];
    }
  }
  return target;
}.autoCurry();

Functional.unionWith = function(f, x, y) {
  f = f.toFunction();
  var target = {};
  for(property in x){ if(x.hasOwnProperty(property)) target[property] = x[property]; }

  for(property in y) {
    if(y.hasOwnProperty(property)) {
        if(isObj(y[property].valueOf())) {
          unionWith(f, target[property], y[property]);
        } else {
          if(x[property]) {
            target[property] = f(x[property], y[property]);
          } else {
            target[property] = y[property];
          }
        }
      }
    }
  return target;
}.autoCurry();



// Math
Functional.random = function(i) {
  return Math.floor(Math.random()*i);
}

Functional.subtract = function(x,y){
  return y - x;
}.autoCurry();

Functional.sum = Functional.reduce('+', 0)
Functional.div = function(x,y){ return x / y; }
Functional.average = function(xs) {
  var zerolessArr = filter('!==0', xs);
  return Functional.div(sum(zerolessArr), zerolessArr.length);
}

// Other
Functional.repeat = function(arg, n) {
  return Functional.nTimes(n, Functional.id.curry(arg));
}.autoCurry();


// useful
Functional.chainAccess = function (obj, name, val) {
  if(val == null) { return obj[name]; }
  obj[name] = val;
  return this;
}


