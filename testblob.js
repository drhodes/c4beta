//     Underscore.js 1.4.4
//     http://underscorejs.org
//     (c) 2009-2013 Jeremy Ashkenas, DocumentCloud Inc.
//     Underscore may be freely distributed under the MIT license.

(function() {

  // Baseline setup
  // --------------

  // Establish the root object, `window` in the browser, or `global` on the server.
  var root = this;

  // Save the previous value of the `_` variable.
  var previousUnderscore = root._;

  // Establish the object that gets returned to break out of a loop iteration.
  var breaker = {};

  // Save bytes in the minified (but not gzipped) version:
  var ArrayProto = Array.prototype, ObjProto = Object.prototype, FuncProto = Function.prototype;

  // Create quick reference variables for speed access to core prototypes.
  var push             = ArrayProto.push,
      slice            = ArrayProto.slice,
      concat           = ArrayProto.concat,
      toString         = ObjProto.toString,
      hasOwnProperty   = ObjProto.hasOwnProperty;

  // All **ECMAScript 5** native function implementations that we hope to use
  // are declared here.
  var
    nativeForEach      = ArrayProto.forEach,
    nativeMap          = ArrayProto.map,
    nativeReduce       = ArrayProto.reduce,
    nativeReduceRight  = ArrayProto.reduceRight,
    nativeFilter       = ArrayProto.filter,
    nativeEvery        = ArrayProto.every,
    nativeSome         = ArrayProto.some,
    nativeIndexOf      = ArrayProto.indexOf,
    nativeLastIndexOf  = ArrayProto.lastIndexOf,
    nativeIsArray      = Array.isArray,
    nativeKeys         = Object.keys,
    nativeBind         = FuncProto.bind;

  // Create a safe reference to the Underscore object for use below.
  var _ = function(obj) {
    if (obj instanceof _) return obj;
    if (!(this instanceof _)) return new _(obj);
    this._wrapped = obj;
  };

  // Export the Underscore object for **Node.js**, with
  // backwards-compatibility for the old `require()` API. If we're in
  // the browser, add `_` as a global object via a string identifier,
  // for Closure Compiler "advanced" mode.
  if (typeof exports !== 'undefined') {
    if (typeof module !== 'undefined' && module.exports) {
      exports = module.exports = _;
    }
    exports._ = _;
  } else {
    root._ = _;
  }

  // Current version.
  _.VERSION = '1.4.4';

  // Collection Functions
  // --------------------

  // The cornerstone, an `each` implementation, aka `forEach`.
  // Handles objects with the built-in `forEach`, arrays, and raw objects.
  // Delegates to **ECMAScript 5**'s native `forEach` if available.
  var each = _.each = _.forEach = function(obj, iterator, context) {
    if (obj == null) return;
    if (nativeForEach && obj.forEach === nativeForEach) {
      obj.forEach(iterator, context);
    } else if (obj.length === +obj.length) {
      for (var i = 0, l = obj.length; i < l; i++) {
        if (iterator.call(context, obj[i], i, obj) === breaker) return;
      }
    } else {
      for (var key in obj) {
        if (_.has(obj, key)) {
          if (iterator.call(context, obj[key], key, obj) === breaker) return;
        }
      }
    }
  };

  // Return the results of applying the iterator to each element.
  // Delegates to **ECMAScript 5**'s native `map` if available.
  _.map = _.collect = function(obj, iterator, context) {
    var results = [];
    if (obj == null) return results;
    if (nativeMap && obj.map === nativeMap) return obj.map(iterator, context);
    each(obj, function(value, index, list) {
      results[results.length] = iterator.call(context, value, index, list);
    });
    return results;
  };

  var reduceError = 'Reduce of empty array with no initial value';

  // **Reduce** builds up a single result from a list of values, aka `inject`,
  // or `foldl`. Delegates to **ECMAScript 5**'s native `reduce` if available.
  _.reduce = _.foldl = _.inject = function(obj, iterator, memo, context) {
    var initial = arguments.length > 2;
    if (obj == null) obj = [];
    if (nativeReduce && obj.reduce === nativeReduce) {
      if (context) iterator = _.bind(iterator, context);
      return initial ? obj.reduce(iterator, memo) : obj.reduce(iterator);
    }
    each(obj, function(value, index, list) {
      if (!initial) {
        memo = value;
        initial = true;
      } else {
        memo = iterator.call(context, memo, value, index, list);
      }
    });
    if (!initial) throw new TypeError(reduceError);
    return memo;
  };

  // The right-associative version of reduce, also known as `foldr`.
  // Delegates to **ECMAScript 5**'s native `reduceRight` if available.
  _.reduceRight = _.foldr = function(obj, iterator, memo, context) {
    var initial = arguments.length > 2;
    if (obj == null) obj = [];
    if (nativeReduceRight && obj.reduceRight === nativeReduceRight) {
      if (context) iterator = _.bind(iterator, context);
      return initial ? obj.reduceRight(iterator, memo) : obj.reduceRight(iterator);
    }
    var length = obj.length;
    if (length !== +length) {
      var keys = _.keys(obj);
      length = keys.length;
    }
    each(obj, function(value, index, list) {
      index = keys ? keys[--length] : --length;
      if (!initial) {
        memo = obj[index];
        initial = true;
      } else {
        memo = iterator.call(context, memo, obj[index], index, list);
      }
    });
    if (!initial) throw new TypeError(reduceError);
    return memo;
  };

  // Return the first value which passes a truth test. Aliased as `detect`.
  _.find = _.detect = function(obj, iterator, context) {
    var result;
    any(obj, function(value, index, list) {
      if (iterator.call(context, value, index, list)) {
        result = value;
        return true;
      }
    });
    return result;
  };

  // Return all the elements that pass a truth test.
  // Delegates to **ECMAScript 5**'s native `filter` if available.
  // Aliased as `select`.
  _.filter = _.select = function(obj, iterator, context) {
    var results = [];
    if (obj == null) return results;
    if (nativeFilter && obj.filter === nativeFilter) return obj.filter(iterator, context);
    each(obj, function(value, index, list) {
      if (iterator.call(context, value, index, list)) results[results.length] = value;
    });
    return results;
  };

  // Return all the elements for which a truth test fails.
  _.reject = function(obj, iterator, context) {
    return _.filter(obj, function(value, index, list) {
      return !iterator.call(context, value, index, list);
    }, context);
  };

  // Determine whether all of the elements match a truth test.
  // Delegates to **ECMAScript 5**'s native `every` if available.
  // Aliased as `all`.
  _.every = _.all = function(obj, iterator, context) {
    iterator || (iterator = _.identity);
    var result = true;
    if (obj == null) return result;
    if (nativeEvery && obj.every === nativeEvery) return obj.every(iterator, context);
    each(obj, function(value, index, list) {
      if (!(result = result && iterator.call(context, value, index, list))) return breaker;
    });
    return !!result;
  };

  // Determine if at least one element in the object matches a truth test.
  // Delegates to **ECMAScript 5**'s native `some` if available.
  // Aliased as `any`.
  var any = _.some = _.any = function(obj, iterator, context) {
    iterator || (iterator = _.identity);
    var result = false;
    if (obj == null) return result;
    if (nativeSome && obj.some === nativeSome) return obj.some(iterator, context);
    each(obj, function(value, index, list) {
      if (result || (result = iterator.call(context, value, index, list))) return breaker;
    });
    return !!result;
  };

  // Determine if the array or object contains a given value (using `===`).
  // Aliased as `include`.
  _.contains = _.include = function(obj, target) {
    if (obj == null) return false;
    if (nativeIndexOf && obj.indexOf === nativeIndexOf) return obj.indexOf(target) != -1;
    return any(obj, function(value) {
      return value === target;
    });
  };

  // Invoke a method (with arguments) on every item in a collection.
  _.invoke = function(obj, method) {
    var args = slice.call(arguments, 2);
    var isFunc = _.isFunction(method);
    return _.map(obj, function(value) {
      return (isFunc ? method : value[method]).apply(value, args);
    });
  };

  // Convenience version of a common use case of `map`: fetching a property.
  _.pluck = function(obj, key) {
    return _.map(obj, function(value){ return value[key]; });
  };

  // Convenience version of a common use case of `filter`: selecting only objects
  // containing specific `key:value` pairs.
  _.where = function(obj, attrs, first) {
    if (_.isEmpty(attrs)) return first ? null : [];
    return _[first ? 'find' : 'filter'](obj, function(value) {
      for (var key in attrs) {
        if (attrs[key] !== value[key]) return false;
      }
      return true;
    });
  };

  // Convenience version of a common use case of `find`: getting the first object
  // containing specific `key:value` pairs.
  _.findWhere = function(obj, attrs) {
    return _.where(obj, attrs, true);
  };

  // Return the maximum element or (element-based computation).
  // Can't optimize arrays of integers longer than 65,535 elements.
  // See: https://bugs.webkit.org/show_bug.cgi?id=80797
  _.max = function(obj, iterator, context) {
    if (!iterator && _.isArray(obj) && obj[0] === +obj[0] && obj.length < 65535) {
      return Math.max.apply(Math, obj);
    }
    if (!iterator && _.isEmpty(obj)) return -Infinity;
    var result = {computed : -Infinity, value: -Infinity};
    each(obj, function(value, index, list) {
      var computed = iterator ? iterator.call(context, value, index, list) : value;
      computed >= result.computed && (result = {value : value, computed : computed});
    });
    return result.value;
  };

  // Return the minimum element (or element-based computation).
  _.min = function(obj, iterator, context) {
    if (!iterator && _.isArray(obj) && obj[0] === +obj[0] && obj.length < 65535) {
      return Math.min.apply(Math, obj);
    }
    if (!iterator && _.isEmpty(obj)) return Infinity;
    var result = {computed : Infinity, value: Infinity};
    each(obj, function(value, index, list) {
      var computed = iterator ? iterator.call(context, value, index, list) : value;
      computed < result.computed && (result = {value : value, computed : computed});
    });
    return result.value;
  };

  // Shuffle an array.
  _.shuffle = function(obj) {
    var rand;
    var index = 0;
    var shuffled = [];
    each(obj, function(value) {
      rand = _.random(index++);
      shuffled[index - 1] = shuffled[rand];
      shuffled[rand] = value;
    });
    return shuffled;
  };

  // An internal function to generate lookup iterators.
  var lookupIterator = function(value) {
    return _.isFunction(value) ? value : function(obj){ return obj[value]; };
  };

  // Sort the object's values by a criterion produced by an iterator.
  _.sortBy = function(obj, value, context) {
    var iterator = lookupIterator(value);
    return _.pluck(_.map(obj, function(value, index, list) {
      return {
        value : value,
        index : index,
        criteria : iterator.call(context, value, index, list)
      };
    }).sort(function(left, right) {
      var a = left.criteria;
      var b = right.criteria;
      if (a !== b) {
        if (a > b || a === void 0) return 1;
        if (a < b || b === void 0) return -1;
      }
      return left.index < right.index ? -1 : 1;
    }), 'value');
  };

  // An internal function used for aggregate "group by" operations.
  var group = function(obj, value, context, behavior) {
    var result = {};
    var iterator = lookupIterator(value || _.identity);
    each(obj, function(value, index) {
      var key = iterator.call(context, value, index, obj);
      behavior(result, key, value);
    });
    return result;
  };

  // Groups the object's values by a criterion. Pass either a string attribute
  // to group by, or a function that returns the criterion.
  _.groupBy = function(obj, value, context) {
    return group(obj, value, context, function(result, key, value) {
      (_.has(result, key) ? result[key] : (result[key] = [])).push(value);
    });
  };

  // Counts instances of an object that group by a certain criterion. Pass
  // either a string attribute to count by, or a function that returns the
  // criterion.
  _.countBy = function(obj, value, context) {
    return group(obj, value, context, function(result, key) {
      if (!_.has(result, key)) result[key] = 0;
      result[key]++;
    });
  };

  // Use a comparator function to figure out the smallest index at which
  // an object should be inserted so as to maintain order. Uses binary search.
  _.sortedIndex = function(array, obj, iterator, context) {
    iterator = iterator == null ? _.identity : lookupIterator(iterator);
    var value = iterator.call(context, obj);
    var low = 0, high = array.length;
    while (low < high) {
      var mid = (low + high) >>> 1;
      iterator.call(context, array[mid]) < value ? low = mid + 1 : high = mid;
    }
    return low;
  };

  // Safely convert anything iterable into a real, live array.
  _.toArray = function(obj) {
    if (!obj) return [];
    if (_.isArray(obj)) return slice.call(obj);
    if (obj.length === +obj.length) return _.map(obj, _.identity);
    return _.values(obj);
  };

  // Return the number of elements in an object.
  _.size = function(obj) {
    if (obj == null) return 0;
    return (obj.length === +obj.length) ? obj.length : _.keys(obj).length;
  };

  // Array Functions
  // ---------------

  // Get the first element of an array. Passing **n** will return the first N
  // values in the array. Aliased as `head` and `take`. The **guard** check
  // allows it to work with `_.map`.
  _.first = _.head = _.take = function(array, n, guard) {
    if (array == null) return void 0;
    return (n != null) && !guard ? slice.call(array, 0, n) : array[0];
  };

  // Returns everything but the last entry of the array. Especially useful on
  // the arguments object. Passing **n** will return all the values in
  // the array, excluding the last N. The **guard** check allows it to work with
  // `_.map`.
  _.initial = function(array, n, guard) {
    return slice.call(array, 0, array.length - ((n == null) || guard ? 1 : n));
  };

  // Get the last element of an array. Passing **n** will return the last N
  // values in the array. The **guard** check allows it to work with `_.map`.
  _.last = function(array, n, guard) {
    if (array == null) return void 0;
    if ((n != null) && !guard) {
      return slice.call(array, Math.max(array.length - n, 0));
    } else {
      return array[array.length - 1];
    }
  };

  // Returns everything but the first entry of the array. Aliased as `tail` and `drop`.
  // Especially useful on the arguments object. Passing an **n** will return
  // the rest N values in the array. The **guard**
  // check allows it to work with `_.map`.
  _.rest = _.tail = _.drop = function(array, n, guard) {
    return slice.call(array, (n == null) || guard ? 1 : n);
  };

  // Trim out all falsy values from an array.
  _.compact = function(array) {
    return _.filter(array, _.identity);
  };

  // Internal implementation of a recursive `flatten` function.
  var flatten = function(input, shallow, output) {
    each(input, function(value) {
      if (_.isArray(value)) {
        shallow ? push.apply(output, value) : flatten(value, shallow, output);
      } else {
        output.push(value);
      }
    });
    return output;
  };

  // Return a completely flattened version of an array.
  _.flatten = function(array, shallow) {
    return flatten(array, shallow, []);
  };

  // Return a version of the array that does not contain the specified value(s).
  _.without = function(array) {
    return _.difference(array, slice.call(arguments, 1));
  };

  // Produce a duplicate-free version of the array. If the array has already
  // been sorted, you have the option of using a faster algorithm.
  // Aliased as `unique`.
  _.uniq = _.unique = function(array, isSorted, iterator, context) {
    if (_.isFunction(isSorted)) {
      context = iterator;
      iterator = isSorted;
      isSorted = false;
    }
    var initial = iterator ? _.map(array, iterator, context) : array;
    var results = [];
    var seen = [];
    each(initial, function(value, index) {
      if (isSorted ? (!index || seen[seen.length - 1] !== value) : !_.contains(seen, value)) {
        seen.push(value);
        results.push(array[index]);
      }
    });
    return results;
  };

  // Produce an array that contains the union: each distinct element from all of
  // the passed-in arrays.
  _.union = function() {
    return _.uniq(concat.apply(ArrayProto, arguments));
  };

  // Produce an array that contains every item shared between all the
  // passed-in arrays.
  _.intersection = function(array) {
    var rest = slice.call(arguments, 1);
    return _.filter(_.uniq(array), function(item) {
      return _.every(rest, function(other) {
        return _.indexOf(other, item) >= 0;
      });
    });
  };

  // Take the difference between one array and a number of other arrays.
  // Only the elements present in just the first array will remain.
  _.difference = function(array) {
    var rest = concat.apply(ArrayProto, slice.call(arguments, 1));
    return _.filter(array, function(value){ return !_.contains(rest, value); });
  };

  // Zip together multiple lists into a single array -- elements that share
  // an index go together.
  _.zip = function() {
    var args = slice.call(arguments);
    var length = _.max(_.pluck(args, 'length'));
    var results = new Array(length);
    for (var i = 0; i < length; i++) {
      results[i] = _.pluck(args, "" + i);
    }
    return results;
  };

  // Converts lists into objects. Pass either a single array of `[key, value]`
  // pairs, or two parallel arrays of the same length -- one of keys, and one of
  // the corresponding values.
  _.object = function(list, values) {
    if (list == null) return {};
    var result = {};
    for (var i = 0, l = list.length; i < l; i++) {
      if (values) {
        result[list[i]] = values[i];
      } else {
        result[list[i][0]] = list[i][1];
      }
    }
    return result;
  };

  // If the browser doesn't supply us with indexOf (I'm looking at you, **MSIE**),
  // we need this function. Return the position of the first occurrence of an
  // item in an array, or -1 if the item is not included in the array.
  // Delegates to **ECMAScript 5**'s native `indexOf` if available.
  // If the array is large and already in sort order, pass `true`
  // for **isSorted** to use binary search.
  _.indexOf = function(array, item, isSorted) {
    if (array == null) return -1;
    var i = 0, l = array.length;
    if (isSorted) {
      if (typeof isSorted == 'number') {
        i = (isSorted < 0 ? Math.max(0, l + isSorted) : isSorted);
      } else {
        i = _.sortedIndex(array, item);
        return array[i] === item ? i : -1;
      }
    }
    if (nativeIndexOf && array.indexOf === nativeIndexOf) return array.indexOf(item, isSorted);
    for (; i < l; i++) if (array[i] === item) return i;
    return -1;
  };

  // Delegates to **ECMAScript 5**'s native `lastIndexOf` if available.
  _.lastIndexOf = function(array, item, from) {
    if (array == null) return -1;
    var hasIndex = from != null;
    if (nativeLastIndexOf && array.lastIndexOf === nativeLastIndexOf) {
      return hasIndex ? array.lastIndexOf(item, from) : array.lastIndexOf(item);
    }
    var i = (hasIndex ? from : array.length);
    while (i--) if (array[i] === item) return i;
    return -1;
  };

  // Generate an integer Array containing an arithmetic progression. A port of
  // the native Python `range()` function. See
  // [the Python documentation](http://docs.python.org/library/functions.html#range).
  _.range = function(start, stop, step) {
    if (arguments.length <= 1) {
      stop = start || 0;
      start = 0;
    }
    step = arguments[2] || 1;

    var len = Math.max(Math.ceil((stop - start) / step), 0);
    var idx = 0;
    var range = new Array(len);

    while(idx < len) {
      range[idx++] = start;
      start += step;
    }

    return range;
  };

  // Function (ahem) Functions
  // ------------------

  // Create a function bound to a given object (assigning `this`, and arguments,
  // optionally). Delegates to **ECMAScript 5**'s native `Function.bind` if
  // available.
  _.bind = function(func, context) {
    if (func.bind === nativeBind && nativeBind) return nativeBind.apply(func, slice.call(arguments, 1));
    var args = slice.call(arguments, 2);
    return function() {
      return func.apply(context, args.concat(slice.call(arguments)));
    };
  };

  // Partially apply a function by creating a version that has had some of its
  // arguments pre-filled, without changing its dynamic `this` context.
  _.partial = function(func) {
    var args = slice.call(arguments, 1);
    return function() {
      return func.apply(this, args.concat(slice.call(arguments)));
    };
  };

  // Bind all of an object's methods to that object. Useful for ensuring that
  // all callbacks defined on an object belong to it.
  _.bindAll = function(obj) {
    var funcs = slice.call(arguments, 1);
    if (funcs.length === 0) funcs = _.functions(obj);
    each(funcs, function(f) { obj[f] = _.bind(obj[f], obj); });
    return obj;
  };

  // Memoize an expensive function by storing its results.
  _.memoize = function(func, hasher) {
    var memo = {};
    hasher || (hasher = _.identity);
    return function() {
      var key = hasher.apply(this, arguments);
      return _.has(memo, key) ? memo[key] : (memo[key] = func.apply(this, arguments));
    };
  };

  // Delays a function for the given number of milliseconds, and then calls
  // it with the arguments supplied.
  _.delay = function(func, wait) {
    var args = slice.call(arguments, 2);
    return setTimeout(function(){ return func.apply(null, args); }, wait);
  };

  // Defers a function, scheduling it to run after the current call stack has
  // cleared.
  _.defer = function(func) {
    return _.delay.apply(_, [func, 1].concat(slice.call(arguments, 1)));
  };

  // Returns a function, that, when invoked, will only be triggered at most once
  // during a given window of time.
  _.throttle = function(func, wait) {
    var context, args, timeout, result;
    var previous = 0;
    var later = function() {
      previous = new Date;
      timeout = null;
      result = func.apply(context, args);
    };
    return function() {
      var now = new Date;
      var remaining = wait - (now - previous);
      context = this;
      args = arguments;
      if (remaining <= 0) {
        clearTimeout(timeout);
        timeout = null;
        previous = now;
        result = func.apply(context, args);
      } else if (!timeout) {
        timeout = setTimeout(later, remaining);
      }
      return result;
    };
  };

  // Returns a function, that, as long as it continues to be invoked, will not
  // be triggered. The function will be called after it stops being called for
  // N milliseconds. If `immediate` is passed, trigger the function on the
  // leading edge, instead of the trailing.
  _.debounce = function(func, wait, immediate) {
    var timeout, result;
    return function() {
      var context = this, args = arguments;
      var later = function() {
        timeout = null;
        if (!immediate) result = func.apply(context, args);
      };
      var callNow = immediate && !timeout;
      clearTimeout(timeout);
      timeout = setTimeout(later, wait);
      if (callNow) result = func.apply(context, args);
      return result;
    };
  };

  // Returns a function that will be executed at most one time, no matter how
  // often you call it. Useful for lazy initialization.
  _.once = function(func) {
    var ran = false, memo;
    return function() {
      if (ran) return memo;
      ran = true;
      memo = func.apply(this, arguments);
      func = null;
      return memo;
    };
  };

  // Returns the first function passed as an argument to the second,
  // allowing you to adjust arguments, run code before and after, and
  // conditionally execute the original function.
  _.wrap = function(func, wrapper) {
    return function() {
      var args = [func];
      push.apply(args, arguments);
      return wrapper.apply(this, args);
    };
  };

  // Returns a function that is the composition of a list of functions, each
  // consuming the return value of the function that follows.
  _.compose = function() {
    var funcs = arguments;
    return function() {
      var args = arguments;
      for (var i = funcs.length - 1; i >= 0; i--) {
        args = [funcs[i].apply(this, args)];
      }
      return args[0];
    };
  };

  // Returns a function that will only be executed after being called N times.
  _.after = function(times, func) {
    if (times <= 0) return func();
    return function() {
      if (--times < 1) {
        return func.apply(this, arguments);
      }
    };
  };

  // Object Functions
  // ----------------

  // Retrieve the names of an object's properties.
  // Delegates to **ECMAScript 5**'s native `Object.keys`
  _.keys = nativeKeys || function(obj) {
    if (obj !== Object(obj)) throw new TypeError('Invalid object');
    var keys = [];
    for (var key in obj) if (_.has(obj, key)) keys[keys.length] = key;
    return keys;
  };

  // Retrieve the values of an object's properties.
  _.values = function(obj) {
    var values = [];
    for (var key in obj) if (_.has(obj, key)) values.push(obj[key]);
    return values;
  };

  // Convert an object into a list of `[key, value]` pairs.
  _.pairs = function(obj) {
    var pairs = [];
    for (var key in obj) if (_.has(obj, key)) pairs.push([key, obj[key]]);
    return pairs;
  };

  // Invert the keys and values of an object. The values must be serializable.
  _.invert = function(obj) {
    var result = {};
    for (var key in obj) if (_.has(obj, key)) result[obj[key]] = key;
    return result;
  };

  // Return a sorted list of the function names available on the object.
  // Aliased as `methods`
  _.functions = _.methods = function(obj) {
    var names = [];
    for (var key in obj) {
      if (_.isFunction(obj[key])) names.push(key);
    }
    return names.sort();
  };

  // Extend a given object with all the properties in passed-in object(s).
  _.extend = function(obj) {
    each(slice.call(arguments, 1), function(source) {
      if (source) {
        for (var prop in source) {
          obj[prop] = source[prop];
        }
      }
    });
    return obj;
  };

  // Return a copy of the object only containing the whitelisted properties.
  _.pick = function(obj) {
    var copy = {};
    var keys = concat.apply(ArrayProto, slice.call(arguments, 1));
    each(keys, function(key) {
      if (key in obj) copy[key] = obj[key];
    });
    return copy;
  };

   // Return a copy of the object without the blacklisted properties.
  _.omit = function(obj) {
    var copy = {};
    var keys = concat.apply(ArrayProto, slice.call(arguments, 1));
    for (var key in obj) {
      if (!_.contains(keys, key)) copy[key] = obj[key];
    }
    return copy;
  };

  // Fill in a given object with default properties.
  _.defaults = function(obj) {
    each(slice.call(arguments, 1), function(source) {
      if (source) {
        for (var prop in source) {
          if (obj[prop] == null) obj[prop] = source[prop];
        }
      }
    });
    return obj;
  };

  // Create a (shallow-cloned) duplicate of an object.
  _.clone = function(obj) {
    if (!_.isObject(obj)) return obj;
    return _.isArray(obj) ? obj.slice() : _.extend({}, obj);
  };

  // Invokes interceptor with the obj, and then returns obj.
  // The primary purpose of this method is to "tap into" a method chain, in
  // order to perform operations on intermediate results within the chain.
  _.tap = function(obj, interceptor) {
    interceptor(obj);
    return obj;
  };

  // Internal recursive comparison function for `isEqual`.
  var eq = function(a, b, aStack, bStack) {
    // Identical objects are equal. `0 === -0`, but they aren't identical.
    // See the Harmony `egal` proposal: http://wiki.ecmascript.org/doku.php?id=harmony:egal.
    if (a === b) return a !== 0 || 1 / a == 1 / b;
    // A strict comparison is necessary because `null == undefined`.
    if (a == null || b == null) return a === b;
    // Unwrap any wrapped objects.
    if (a instanceof _) a = a._wrapped;
    if (b instanceof _) b = b._wrapped;
    // Compare `[[Class]]` names.
    var className = toString.call(a);
    if (className != toString.call(b)) return false;
    switch (className) {
      // Strings, numbers, dates, and booleans are compared by value.
      case '[object String]':
        // Primitives and their corresponding object wrappers are equivalent; thus, `"5"` is
        // equivalent to `new String("5")`.
        return a == String(b);
      case '[object Number]':
        // `NaN`s are equivalent, but non-reflexive. An `egal` comparison is performed for
        // other numeric values.
        return a != +a ? b != +b : (a == 0 ? 1 / a == 1 / b : a == +b);
      case '[object Date]':
      case '[object Boolean]':
        // Coerce dates and booleans to numeric primitive values. Dates are compared by their
        // millisecond representations. Note that invalid dates with millisecond representations
        // of `NaN` are not equivalent.
        return +a == +b;
      // RegExps are compared by their source patterns and flags.
      case '[object RegExp]':
        return a.source == b.source &&
               a.global == b.global &&
               a.multiline == b.multiline &&
               a.ignoreCase == b.ignoreCase;
    }
    if (typeof a != 'object' || typeof b != 'object') return false;
    // Assume equality for cyclic structures. The algorithm for detecting cyclic
    // structures is adapted from ES 5.1 section 15.12.3, abstract operation `JO`.
    var length = aStack.length;
    while (length--) {
      // Linear search. Performance is inversely proportional to the number of
      // unique nested structures.
      if (aStack[length] == a) return bStack[length] == b;
    }
    // Add the first object to the stack of traversed objects.
    aStack.push(a);
    bStack.push(b);
    var size = 0, result = true;
    // Recursively compare objects and arrays.
    if (className == '[object Array]') {
      // Compare array lengths to determine if a deep comparison is necessary.
      size = a.length;
      result = size == b.length;
      if (result) {
        // Deep compare the contents, ignoring non-numeric properties.
        while (size--) {
          if (!(result = eq(a[size], b[size], aStack, bStack))) break;
        }
      }
    } else {
      // Objects with different constructors are not equivalent, but `Object`s
      // from different frames are.
      var aCtor = a.constructor, bCtor = b.constructor;
      if (aCtor !== bCtor && !(_.isFunction(aCtor) && (aCtor instanceof aCtor) &&
                               _.isFunction(bCtor) && (bCtor instanceof bCtor))) {
        return false;
      }
      // Deep compare objects.
      for (var key in a) {
        if (_.has(a, key)) {
          // Count the expected number of properties.
          size++;
          // Deep compare each member.
          if (!(result = _.has(b, key) && eq(a[key], b[key], aStack, bStack))) break;
        }
      }
      // Ensure that both objects contain the same number of properties.
      if (result) {
        for (key in b) {
          if (_.has(b, key) && !(size--)) break;
        }
        result = !size;
      }
    }
    // Remove the first object from the stack of traversed objects.
    aStack.pop();
    bStack.pop();
    return result;
  };

  // Perform a deep comparison to check if two objects are equal.
  _.isEqual = function(a, b) {
    return eq(a, b, [], []);
  };

  // Is a given array, string, or object empty?
  // An "empty" object has no enumerable own-properties.
  _.isEmpty = function(obj) {
    if (obj == null) return true;
    if (_.isArray(obj) || _.isString(obj)) return obj.length === 0;
    for (var key in obj) if (_.has(obj, key)) return false;
    return true;
  };

  // Is a given value a DOM element?
  _.isElement = function(obj) {
    return !!(obj && obj.nodeType === 1);
  };

  // Is a given value an array?
  // Delegates to ECMA5's native Array.isArray
  _.isArray = nativeIsArray || function(obj) {
    return toString.call(obj) == '[object Array]';
  };

  // Is a given variable an object?
  _.isObject = function(obj) {
    return obj === Object(obj);
  };

  // Add some isType methods: isArguments, isFunction, isString, isNumber, isDate, isRegExp.
  each(['Arguments', 'Function', 'String', 'Number', 'Date', 'RegExp'], function(name) {
    _['is' + name] = function(obj) {
      return toString.call(obj) == '[object ' + name + ']';
    };
  });

  // Define a fallback version of the method in browsers (ahem, IE), where
  // there isn't any inspectable "Arguments" type.
  if (!_.isArguments(arguments)) {
    _.isArguments = function(obj) {
      return !!(obj && _.has(obj, 'callee'));
    };
  }

  // Optimize `isFunction` if appropriate.
  if (typeof (/./) !== 'function') {
    _.isFunction = function(obj) {
      return typeof obj === 'function';
    };
  }

  // Is a given object a finite number?
  _.isFinite = function(obj) {
    return isFinite(obj) && !isNaN(parseFloat(obj));
  };

  // Is the given value `NaN`? (NaN is the only number which does not equal itself).
  _.isNaN = function(obj) {
    return _.isNumber(obj) && obj != +obj;
  };

  // Is a given value a boolean?
  _.isBoolean = function(obj) {
    return obj === true || obj === false || toString.call(obj) == '[object Boolean]';
  };

  // Is a given value equal to null?
  _.isNull = function(obj) {
    return obj === null;
  };

  // Is a given variable undefined?
  _.isUndefined = function(obj) {
    return obj === void 0;
  };

  // Shortcut function for checking if an object has a given property directly
  // on itself (in other words, not on a prototype).
  _.has = function(obj, key) {
    return hasOwnProperty.call(obj, key);
  };

  // Utility Functions
  // -----------------

  // Run Underscore.js in *noConflict* mode, returning the `_` variable to its
  // previous owner. Returns a reference to the Underscore object.
  _.noConflict = function() {
    root._ = previousUnderscore;
    return this;
  };

  // Keep the identity function around for default iterators.
  _.identity = function(value) {
    return value;
  };

  // Run a function **n** times.
  _.times = function(n, iterator, context) {
    var accum = Array(n);
    for (var i = 0; i < n; i++) accum[i] = iterator.call(context, i);
    return accum;
  };

  // Return a random integer between min and max (inclusive).
  _.random = function(min, max) {
    if (max == null) {
      max = min;
      min = 0;
    }
    return min + Math.floor(Math.random() * (max - min + 1));
  };

  // List of HTML entities for escaping.
  var entityMap = {
    escape: {
      '&': '&amp;',
      '<': '&lt;',
      '>': '&gt;',
      '"': '&quot;',
      "'": '&#x27;',
      '/': '&#x2F;'
    }
  };
  entityMap.unescape = _.invert(entityMap.escape);

  // Regexes containing the keys and values listed immediately above.
  var entityRegexes = {
    escape:   new RegExp('[' + _.keys(entityMap.escape).join('') + ']', 'g'),
    unescape: new RegExp('(' + _.keys(entityMap.unescape).join('|') + ')', 'g')
  };

  // Functions for escaping and unescaping strings to/from HTML interpolation.
  _.each(['escape', 'unescape'], function(method) {
    _[method] = function(string) {
      if (string == null) return '';
      return ('' + string).replace(entityRegexes[method], function(match) {
        return entityMap[method][match];
      });
    };
  });

  // If the value of the named property is a function then invoke it;
  // otherwise, return it.
  _.result = function(object, property) {
    if (object == null) return null;
    var value = object[property];
    return _.isFunction(value) ? value.call(object) : value;
  };

  // Add your own custom functions to the Underscore object.
  _.mixin = function(obj) {
    each(_.functions(obj), function(name){
      var func = _[name] = obj[name];
      _.prototype[name] = function() {
        var args = [this._wrapped];
        push.apply(args, arguments);
        return result.call(this, func.apply(_, args));
      };
    });
  };

  // Generate a unique integer id (unique within the entire client session).
  // Useful for temporary DOM ids.
  var idCounter = 0;
  _.uniqueId = function(prefix) {
    var id = ++idCounter + '';
    return prefix ? prefix + id : id;
  };

  // By default, Underscore uses ERB-style template delimiters, change the
  // following template settings to use alternative delimiters.
  _.templateSettings = {
    evaluate    : /<%([\s\S]+?)%>/g,
    interpolate : /<%=([\s\S]+?)%>/g,
    escape      : /<%-([\s\S]+?)%>/g
  };

  // When customizing `templateSettings`, if you don't want to define an
  // interpolation, evaluation or escaping regex, we need one that is
  // guaranteed not to match.
  var noMatch = /(.)^/;

  // Certain characters need to be escaped so that they can be put into a
  // string literal.
  var escapes = {
    "'":      "'",
    '\\':     '\\',
    '\r':     'r',
    '\n':     'n',
    '\t':     't',
    '\u2028': 'u2028',
    '\u2029': 'u2029'
  };

  var escaper = /\\|'|\r|\n|\t|\u2028|\u2029/g;

  // JavaScript micro-templating, similar to John Resig's implementation.
  // Underscore templating handles arbitrary delimiters, preserves whitespace,
  // and correctly escapes quotes within interpolated code.
  _.template = function(text, data, settings) {
    var render;
    settings = _.defaults({}, settings, _.templateSettings);

    // Combine delimiters into one regular expression via alternation.
    var matcher = new RegExp([
      (settings.escape || noMatch).source,
      (settings.interpolate || noMatch).source,
      (settings.evaluate || noMatch).source
    ].join('|') + '|$', 'g');

    // Compile the template source, escaping string literals appropriately.
    var index = 0;
    var source = "__p+='";
    text.replace(matcher, function(match, escape, interpolate, evaluate, offset) {
      source += text.slice(index, offset)
        .replace(escaper, function(match) { return '\\' + escapes[match]; });

      if (escape) {
        source += "'+\n((__t=(" + escape + "))==null?'':_.escape(__t))+\n'";
      }
      if (interpolate) {
        source += "'+\n((__t=(" + interpolate + "))==null?'':__t)+\n'";
      }
      if (evaluate) {
        source += "';\n" + evaluate + "\n__p+='";
      }
      index = offset + match.length;
      return match;
    });
    source += "';\n";

    // If a variable is not specified, place data values in local scope.
    if (!settings.variable) source = 'with(obj||{}){\n' + source + '}\n';

    source = "var __t,__p='',__j=Array.prototype.join," +
      "print=function(){__p+=__j.call(arguments,'');};\n" +
      source + "return __p;\n";

    try {
      render = new Function(settings.variable || 'obj', '_', source);
    } catch (e) {
      e.source = source;
      throw e;
    }

    if (data) return render(data, _);
    var template = function(data) {
      return render.call(this, data, _);
    };

    // Provide the compiled function source as a convenience for precompilation.
    template.source = 'function(' + (settings.variable || 'obj') + '){\n' + source + '}';

    return template;
  };

  // Add a "chain" function, which will delegate to the wrapper.
  _.chain = function(obj) {
    return _(obj).chain();
  };

  // OOP
  // ---------------
  // If Underscore is called as a function, it returns a wrapped object that
  // can be used OO-style. This wrapper holds altered versions of all the
  // underscore functions. Wrapped objects may be chained.

  // Helper function to continue chaining intermediate results.
  var result = function(obj) {
    return this._chain ? _(obj).chain() : obj;
  };

  // Add all of the Underscore functions to the wrapper object.
  _.mixin(_);

  // Add all mutator Array functions to the wrapper.
  each(['pop', 'push', 'reverse', 'shift', 'sort', 'splice', 'unshift'], function(name) {
    var method = ArrayProto[name];
    _.prototype[name] = function() {
      var obj = this._wrapped;
      method.apply(obj, arguments);
      if ((name == 'shift' || name == 'splice') && obj.length === 0) delete obj[0];
      return result.call(this, obj);
    };
  });

  // Add all accessor Array functions to the wrapper.
  each(['concat', 'join', 'slice'], function(name) {
    var method = ArrayProto[name];
    _.prototype[name] = function() {
      return result.call(this, method.apply(this._wrapped, arguments));
    };
  });

  _.extend(_.prototype, {

    // Start chaining a wrapped Underscore object.
    chain: function() {
      this._chain = true;
      return this;
    },

    // Extracts the result from a wrapped and chained object.
    value: function() {
      return this._wrapped;
    }

  });

}).call(this);
//     Backbone.js 1.0.0

//     (c) 2010-2013 Jeremy Ashkenas, DocumentCloud Inc.
//     Backbone may be freely distributed under the MIT license.
//     For all details and documentation:
//     http://backbonejs.org

(function(){

  // Initial Setup
  // -------------

  // Save a reference to the global object (`window` in the browser, `exports`
  // on the server).
  var root = this;

  // Save the previous value of the `Backbone` variable, so that it can be
  // restored later on, if `noConflict` is used.
  var previousBackbone = root.Backbone;

  // Create local references to array methods we'll want to use later.
  var array = [];
  var push = array.push;
  var slice = array.slice;
  var splice = array.splice;

  // The top-level namespace. All public Backbone classes and modules will
  // be attached to this. Exported for both the browser and the server.
  var Backbone;
  if (typeof exports !== 'undefined') {
    Backbone = exports;
  } else {
    Backbone = root.Backbone = {};
  }

  // Current version of the library. Keep in sync with `package.json`.
  Backbone.VERSION = '1.0.0';

  // Require Underscore, if we're on the server, and it's not already present.
  var _ = root._;
  if (!_ && (typeof require !== 'undefined')) _ = require('underscore');

  // For Backbone's purposes, jQuery, Zepto, Ender, or My Library (kidding) owns
  // the `$` variable.
  Backbone.$ = root.jQuery || root.Zepto || root.ender || root.$;

  // Runs Backbone.js in *noConflict* mode, returning the `Backbone` variable
  // to its previous owner. Returns a reference to this Backbone object.
  Backbone.noConflict = function() {
    root.Backbone = previousBackbone;
    return this;
  };

  // Turn on `emulateHTTP` to support legacy HTTP servers. Setting this option
  // will fake `"PUT"` and `"DELETE"` requests via the `_method` parameter and
  // set a `X-Http-Method-Override` header.
  Backbone.emulateHTTP = false;

  // Turn on `emulateJSON` to support legacy servers that can't deal with direct
  // `application/json` requests ... will encode the body as
  // `application/x-www-form-urlencoded` instead and will send the model in a
  // form param named `model`.
  Backbone.emulateJSON = false;

  // Backbone.Events
  // ---------------

  // A module that can be mixed in to *any object* in order to provide it with
  // custom events. You may bind with `on` or remove with `off` callback
  // functions to an event; `trigger`-ing an event fires all callbacks in
  // succession.
  //
  //     var object = {};
  //     _.extend(object, Backbone.Events);
  //     object.on('expand', function(){ alert('expanded'); });
  //     object.trigger('expand');
  //
  var Events = Backbone.Events = {

    // Bind an event to a `callback` function. Passing `"all"` will bind
    // the callback to all events fired.
    on: function(name, callback, context) {
      if (!eventsApi(this, 'on', name, [callback, context]) || !callback) return this;
      this._events || (this._events = {});
      var events = this._events[name] || (this._events[name] = []);
      events.push({callback: callback, context: context, ctx: context || this});
      return this;
    },

    // Bind an event to only be triggered a single time. After the first time
    // the callback is invoked, it will be removed.
    once: function(name, callback, context) {
      if (!eventsApi(this, 'once', name, [callback, context]) || !callback) return this;
      var self = this;
      var once = _.once(function() {
        self.off(name, once);
        callback.apply(this, arguments);
      });
      once._callback = callback;
      return this.on(name, once, context);
    },

    // Remove one or many callbacks. If `context` is null, removes all
    // callbacks with that function. If `callback` is null, removes all
    // callbacks for the event. If `name` is null, removes all bound
    // callbacks for all events.
    off: function(name, callback, context) {
      var retain, ev, events, names, i, l, j, k;
      if (!this._events || !eventsApi(this, 'off', name, [callback, context])) return this;
      if (!name && !callback && !context) {
        this._events = {};
        return this;
      }

      names = name ? [name] : _.keys(this._events);
      for (i = 0, l = names.length; i < l; i++) {
        name = names[i];
        if (events = this._events[name]) {
          this._events[name] = retain = [];
          if (callback || context) {
            for (j = 0, k = events.length; j < k; j++) {
              ev = events[j];
              if ((callback && callback !== ev.callback && callback !== ev.callback._callback) ||
                  (context && context !== ev.context)) {
                retain.push(ev);
              }
            }
          }
          if (!retain.length) delete this._events[name];
        }
      }

      return this;
    },

    // Trigger one or many events, firing all bound callbacks. Callbacks are
    // passed the same arguments as `trigger` is, apart from the event name
    // (unless you're listening on `"all"`, which will cause your callback to
    // receive the true name of the event as the first argument).
    trigger: function(name) {
      if (!this._events) return this;
      var args = slice.call(arguments, 1);
      if (!eventsApi(this, 'trigger', name, args)) return this;
      var events = this._events[name];
      var allEvents = this._events.all;
      if (events) triggerEvents(events, args);
      if (allEvents) triggerEvents(allEvents, arguments);
      return this;
    },

    // Tell this object to stop listening to either specific events ... or
    // to every object it's currently listening to.
    stopListening: function(obj, name, callback) {
      var listeners = this._listeners;
      if (!listeners) return this;
      var deleteListener = !name && !callback;
      if (typeof name === 'object') callback = this;
      if (obj) (listeners = {})[obj._listenerId] = obj;
      for (var id in listeners) {
        listeners[id].off(name, callback, this);
        if (deleteListener) delete this._listeners[id];
      }
      return this;
    }

  };

  // Regular expression used to split event strings.
  var eventSplitter = /\s+/;

  // Implement fancy features of the Events API such as multiple event
  // names `"change blur"` and jQuery-style event maps `{change: action}`
  // in terms of the existing API.
  var eventsApi = function(obj, action, name, rest) {
    if (!name) return true;

    // Handle event maps.
    if (typeof name === 'object') {
      for (var key in name) {
        obj[action].apply(obj, [key, name[key]].concat(rest));
      }
      return false;
    }

    // Handle space separated event names.
    if (eventSplitter.test(name)) {
      var names = name.split(eventSplitter);
      for (var i = 0, l = names.length; i < l; i++) {
        obj[action].apply(obj, [names[i]].concat(rest));
      }
      return false;
    }

    return true;
  };

  // A difficult-to-believe, but optimized internal dispatch function for
  // triggering events. Tries to keep the usual cases speedy (most internal
  // Backbone events have 3 arguments).
  var triggerEvents = function(events, args) {
    var ev, i = -1, l = events.length, a1 = args[0], a2 = args[1], a3 = args[2];
    switch (args.length) {
      case 0: while (++i < l) (ev = events[i]).callback.call(ev.ctx); return;
      case 1: while (++i < l) (ev = events[i]).callback.call(ev.ctx, a1); return;
      case 2: while (++i < l) (ev = events[i]).callback.call(ev.ctx, a1, a2); return;
      case 3: while (++i < l) (ev = events[i]).callback.call(ev.ctx, a1, a2, a3); return;
      default: while (++i < l) (ev = events[i]).callback.apply(ev.ctx, args);
    }
  };

  var listenMethods = {listenTo: 'on', listenToOnce: 'once'};

  // Inversion-of-control versions of `on` and `once`. Tell *this* object to
  // listen to an event in another object ... keeping track of what it's
  // listening to.
  _.each(listenMethods, function(implementation, method) {
    Events[method] = function(obj, name, callback) {
      var listeners = this._listeners || (this._listeners = {});
      var id = obj._listenerId || (obj._listenerId = _.uniqueId('l'));
      listeners[id] = obj;
      if (typeof name === 'object') callback = this;
      obj[implementation](name, callback, this);
      return this;
    };
  });

  // Aliases for backwards compatibility.
  Events.bind   = Events.on;
  Events.unbind = Events.off;

  // Allow the `Backbone` object to serve as a global event bus, for folks who
  // want global "pubsub" in a convenient place.
  _.extend(Backbone, Events);

  // Backbone.Model
  // --------------

  // Backbone **Models** are the basic data object in the framework --
  // frequently representing a row in a table in a database on your server.
  // A discrete chunk of data and a bunch of useful, related methods for
  // performing computations and transformations on that data.

  // Create a new model with the specified attributes. A client id (`cid`)
  // is automatically generated and assigned for you.
  var Model = Backbone.Model = function(attributes, options) {
    var defaults;
    var attrs = attributes || {};
    options || (options = {});
    this.cid = _.uniqueId('c');
    this.attributes = {};
    _.extend(this, _.pick(options, modelOptions));
    if (options.parse) attrs = this.parse(attrs, options) || {};
    if (defaults = _.result(this, 'defaults')) {
      attrs = _.defaults({}, attrs, defaults);
    }
    this.set(attrs, options);
    this.changed = {};
    this.initialize.apply(this, arguments);
  };

  // A list of options to be attached directly to the model, if provided.
  var modelOptions = ['url', 'urlRoot', 'collection'];

  // Attach all inheritable methods to the Model prototype.
  _.extend(Model.prototype, Events, {

    // A hash of attributes whose current and previous value differ.
    changed: null,

    // The value returned during the last failed validation.
    validationError: null,

    // The default name for the JSON `id` attribute is `"id"`. MongoDB and
    // CouchDB users may want to set this to `"_id"`.
    idAttribute: 'id',

    // Initialize is an empty function by default. Override it with your own
    // initialization logic.
    initialize: function(){},

    // Return a copy of the model's `attributes` object.
    toJSON: function(options) {
      return _.clone(this.attributes);
    },

    // Proxy `Backbone.sync` by default -- but override this if you need
    // custom syncing semantics for *this* particular model.
    sync: function() {
      return Backbone.sync.apply(this, arguments);
    },

    // Get the value of an attribute.
    get: function(attr) {
      return this.attributes[attr];
    },

    // Get the HTML-escaped value of an attribute.
    escape: function(attr) {
      return _.escape(this.get(attr));
    },

    // Returns `true` if the attribute contains a value that is not null
    // or undefined.
    has: function(attr) {
      return this.get(attr) != null;
    },

    // Set a hash of model attributes on the object, firing `"change"`. This is
    // the core primitive operation of a model, updating the data and notifying
    // anyone who needs to know about the change in state. The heart of the beast.
    set: function(key, val, options) {
      var attr, attrs, unset, changes, silent, changing, prev, current;
      if (key == null) return this;

      // Handle both `"key", value` and `{key: value}` -style arguments.
      if (typeof key === 'object') {
        attrs = key;
        options = val;
      } else {
        (attrs = {})[key] = val;
      }

      options || (options = {});

      // Run validation.
      if (!this._validate(attrs, options)) return false;

      // Extract attributes and options.
      unset           = options.unset;
      silent          = options.silent;
      changes         = [];
      changing        = this._changing;
      this._changing  = true;

      if (!changing) {
        this._previousAttributes = _.clone(this.attributes);
        this.changed = {};
      }
      current = this.attributes, prev = this._previousAttributes;

      // Check for changes of `id`.
      if (this.idAttribute in attrs) this.id = attrs[this.idAttribute];

      // For each `set` attribute, update or delete the current value.
      for (attr in attrs) {
        val = attrs[attr];
        if (!_.isEqual(current[attr], val)) changes.push(attr);
        if (!_.isEqual(prev[attr], val)) {
          this.changed[attr] = val;
        } else {
          delete this.changed[attr];
        }
        unset ? delete current[attr] : current[attr] = val;
      }

      // Trigger all relevant attribute changes.
      if (!silent) {
        if (changes.length) this._pending = true;
        for (var i = 0, l = changes.length; i < l; i++) {
          this.trigger('change:' + changes[i], this, current[changes[i]], options);
        }
      }

      // You might be wondering why there's a `while` loop here. Changes can
      // be recursively nested within `"change"` events.
      if (changing) return this;
      if (!silent) {
        while (this._pending) {
          this._pending = false;
          this.trigger('change', this, options);
        }
      }
      this._pending = false;
      this._changing = false;
      return this;
    },

    // Remove an attribute from the model, firing `"change"`. `unset` is a noop
    // if the attribute doesn't exist.
    unset: function(attr, options) {
      return this.set(attr, void 0, _.extend({}, options, {unset: true}));
    },

    // Clear all attributes on the model, firing `"change"`.
    clear: function(options) {
      var attrs = {};
      for (var key in this.attributes) attrs[key] = void 0;
      return this.set(attrs, _.extend({}, options, {unset: true}));
    },

    // Determine if the model has changed since the last `"change"` event.
    // If you specify an attribute name, determine if that attribute has changed.
    hasChanged: function(attr) {
      if (attr == null) return !_.isEmpty(this.changed);
      return _.has(this.changed, attr);
    },

    // Return an object containing all the attributes that have changed, or
    // false if there are no changed attributes. Useful for determining what
    // parts of a view need to be updated and/or what attributes need to be
    // persisted to the server. Unset attributes will be set to undefined.
    // You can also pass an attributes object to diff against the model,
    // determining if there *would be* a change.
    changedAttributes: function(diff) {
      if (!diff) return this.hasChanged() ? _.clone(this.changed) : false;
      var val, changed = false;
      var old = this._changing ? this._previousAttributes : this.attributes;
      for (var attr in diff) {
        if (_.isEqual(old[attr], (val = diff[attr]))) continue;
        (changed || (changed = {}))[attr] = val;
      }
      return changed;
    },

    // Get the previous value of an attribute, recorded at the time the last
    // `"change"` event was fired.
    previous: function(attr) {
      if (attr == null || !this._previousAttributes) return null;
      return this._previousAttributes[attr];
    },

    // Get all of the attributes of the model at the time of the previous
    // `"change"` event.
    previousAttributes: function() {
      return _.clone(this._previousAttributes);
    },

    // Fetch the model from the server. If the server's representation of the
    // model differs from its current attributes, they will be overridden,
    // triggering a `"change"` event.
    fetch: function(options) {
      options = options ? _.clone(options) : {};
      if (options.parse === void 0) options.parse = true;
      var model = this;
      var success = options.success;
      options.success = function(resp) {
        if (!model.set(model.parse(resp, options), options)) return false;
        if (success) success(model, resp, options);
        model.trigger('sync', model, resp, options);
      };
      wrapError(this, options);
      return this.sync('read', this, options);
    },

    // Set a hash of model attributes, and sync the model to the server.
    // If the server returns an attributes hash that differs, the model's
    // state will be `set` again.
    save: function(key, val, options) {
      var attrs, method, xhr, attributes = this.attributes;

      // Handle both `"key", value` and `{key: value}` -style arguments.
      if (key == null || typeof key === 'object') {
        attrs = key;
        options = val;
      } else {
        (attrs = {})[key] = val;
      }

      // If we're not waiting and attributes exist, save acts as `set(attr).save(null, opts)`.
      if (attrs && (!options || !options.wait) && !this.set(attrs, options)) return false;

      options = _.extend({validate: true}, options);

      // Do not persist invalid models.
      if (!this._validate(attrs, options)) return false;

      // Set temporary attributes if `{wait: true}`.
      if (attrs && options.wait) {
        this.attributes = _.extend({}, attributes, attrs);
      }

      // After a successful server-side save, the client is (optionally)
      // updated with the server-side state.
      if (options.parse === void 0) options.parse = true;
      var model = this;
      var success = options.success;
      options.success = function(resp) {
        // Ensure attributes are restored during synchronous saves.
        model.attributes = attributes;
        var serverAttrs = model.parse(resp, options);
        if (options.wait) serverAttrs = _.extend(attrs || {}, serverAttrs);
        if (_.isObject(serverAttrs) && !model.set(serverAttrs, options)) {
          return false;
        }
        if (success) success(model, resp, options);
        model.trigger('sync', model, resp, options);
      };
      wrapError(this, options);

      method = this.isNew() ? 'create' : (options.patch ? 'patch' : 'update');
      if (method === 'patch') options.attrs = attrs;
      xhr = this.sync(method, this, options);

      // Restore attributes.
      if (attrs && options.wait) this.attributes = attributes;

      return xhr;
    },

    // Destroy this model on the server if it was already persisted.
    // Optimistically removes the model from its collection, if it has one.
    // If `wait: true` is passed, waits for the server to respond before removal.
    destroy: function(options) {
      options = options ? _.clone(options) : {};
      var model = this;
      var success = options.success;

      var destroy = function() {
        model.trigger('destroy', model, model.collection, options);
      };

      options.success = function(resp) {
        if (options.wait || model.isNew()) destroy();
        if (success) success(model, resp, options);
        if (!model.isNew()) model.trigger('sync', model, resp, options);
      };

      if (this.isNew()) {
        options.success();
        return false;
      }
      wrapError(this, options);

      var xhr = this.sync('delete', this, options);
      if (!options.wait) destroy();
      return xhr;
    },

    // Default URL for the model's representation on the server -- if you're
    // using Backbone's restful methods, override this to change the endpoint
    // that will be called.
    url: function() {
      var base = _.result(this, 'urlRoot') || _.result(this.collection, 'url') || urlError();
      if (this.isNew()) return base;
      return base + (base.charAt(base.length - 1) === '/' ? '' : '/') + encodeURIComponent(this.id);
    },

    // **parse** converts a response into the hash of attributes to be `set` on
    // the model. The default implementation is just to pass the response along.
    parse: function(resp, options) {
      return resp;
    },

    // Create a new model with identical attributes to this one.
    clone: function() {
      return new this.constructor(this.attributes);
    },

    // A model is new if it has never been saved to the server, and lacks an id.
    isNew: function() {
      return this.id == null;
    },

    // Check if the model is currently in a valid state.
    isValid: function(options) {
      return this._validate({}, _.extend(options || {}, { validate: true }));
    },

    // Run validation against the next complete set of model attributes,
    // returning `true` if all is well. Otherwise, fire an `"invalid"` event.
    _validate: function(attrs, options) {
      if (!options.validate || !this.validate) return true;
      attrs = _.extend({}, this.attributes, attrs);
      var error = this.validationError = this.validate(attrs, options) || null;
      if (!error) return true;
      this.trigger('invalid', this, error, _.extend(options || {}, {validationError: error}));
      return false;
    }

  });

  // Underscore methods that we want to implement on the Model.
  var modelMethods = ['keys', 'values', 'pairs', 'invert', 'pick', 'omit'];

  // Mix in each Underscore method as a proxy to `Model#attributes`.
  _.each(modelMethods, function(method) {
    Model.prototype[method] = function() {
      var args = slice.call(arguments);
      args.unshift(this.attributes);
      return _[method].apply(_, args);
    };
  });

  // Backbone.Collection
  // -------------------

  // If models tend to represent a single row of data, a Backbone Collection is
  // more analagous to a table full of data ... or a small slice or page of that
  // table, or a collection of rows that belong together for a particular reason
  // -- all of the messages in this particular folder, all of the documents
  // belonging to this particular author, and so on. Collections maintain
  // indexes of their models, both in order, and for lookup by `id`.

  // Create a new **Collection**, perhaps to contain a specific type of `model`.
  // If a `comparator` is specified, the Collection will maintain
  // its models in sort order, as they're added and removed.
  var Collection = Backbone.Collection = function(models, options) {
    options || (options = {});
    if (options.url) this.url = options.url;
    if (options.model) this.model = options.model;
    if (options.comparator !== void 0) this.comparator = options.comparator;
    this._reset();
    this.initialize.apply(this, arguments);
    if (models) this.reset(models, _.extend({silent: true}, options));
  };

  // Default options for `Collection#set`.
  var setOptions = {add: true, remove: true, merge: true};
  var addOptions = {add: true, merge: false, remove: false};

  // Define the Collection's inheritable methods.
  _.extend(Collection.prototype, Events, {

    // The default model for a collection is just a **Backbone.Model**.
    // This should be overridden in most cases.
    model: Model,

    // Initialize is an empty function by default. Override it with your own
    // initialization logic.
    initialize: function(){},

    // The JSON representation of a Collection is an array of the
    // models' attributes.
    toJSON: function(options) {
      return this.map(function(model){ return model.toJSON(options); });
    },

    // Proxy `Backbone.sync` by default.
    sync: function() {
      return Backbone.sync.apply(this, arguments);
    },

    // Add a model, or list of models to the set.
    add: function(models, options) {
      return this.set(models, _.defaults(options || {}, addOptions));
    },

    // Remove a model, or a list of models from the set.
    remove: function(models, options) {
      models = _.isArray(models) ? models.slice() : [models];
      options || (options = {});
      var i, l, index, model;
      for (i = 0, l = models.length; i < l; i++) {
        model = this.get(models[i]);
        if (!model) continue;
        delete this._byId[model.id];
        delete this._byId[model.cid];
        index = this.indexOf(model);
        this.models.splice(index, 1);
        this.length--;
        if (!options.silent) {
          options.index = index;
          model.trigger('remove', model, this, options);
        }
        this._removeReference(model);
      }
      return this;
    },

    // Update a collection by `set`-ing a new list of models, adding new ones,
    // removing models that are no longer present, and merging models that
    // already exist in the collection, as necessary. Similar to **Model#set**,
    // the core operation for updating the data contained by the collection.
    set: function(models, options) {
      options = _.defaults(options || {}, setOptions);
      if (options.parse) models = this.parse(models, options);
      if (!_.isArray(models)) models = models ? [models] : [];
      var i, l, model, attrs, existing, sort;
      var at = options.at;
      var sortable = this.comparator && (at == null) && options.sort !== false;
      var sortAttr = _.isString(this.comparator) ? this.comparator : null;
      var toAdd = [], toRemove = [], modelMap = {};

      // Turn bare objects into model references, and prevent invalid models
      // from being added.
      for (i = 0, l = models.length; i < l; i++) {
        if (!(model = this._prepareModel(models[i], options))) continue;

        // If a duplicate is found, prevent it from being added and
        // optionally merge it into the existing model.
        if (existing = this.get(model)) {
          if (options.remove) modelMap[existing.cid] = true;
          if (options.merge) {
            existing.set(model.attributes, options);
            if (sortable && !sort && existing.hasChanged(sortAttr)) sort = true;
          }

        // This is a new model, push it to the `toAdd` list.
        } else if (options.add) {
          toAdd.push(model);

          // Listen to added models' events, and index models for lookup by
          // `id` and by `cid`.
          model.on('all', this._onModelEvent, this);
          this._byId[model.cid] = model;
          if (model.id != null) this._byId[model.id] = model;
        }
      }

      // Remove nonexistent models if appropriate.
      if (options.remove) {
        for (i = 0, l = this.length; i < l; ++i) {
          if (!modelMap[(model = this.models[i]).cid]) toRemove.push(model);
        }
        if (toRemove.length) this.remove(toRemove, options);
      }

      // See if sorting is needed, update `length` and splice in new models.
      if (toAdd.length) {
        if (sortable) sort = true;
        this.length += toAdd.length;
        if (at != null) {
          splice.apply(this.models, [at, 0].concat(toAdd));
        } else {
          push.apply(this.models, toAdd);
        }
      }

      // Silently sort the collection if appropriate.
      if (sort) this.sort({silent: true});

      if (options.silent) return this;

      // Trigger `add` events.
      for (i = 0, l = toAdd.length; i < l; i++) {
        (model = toAdd[i]).trigger('add', model, this, options);
      }

      // Trigger `sort` if the collection was sorted.
      if (sort) this.trigger('sort', this, options);
      return this;
    },

    // When you have more items than you want to add or remove individually,
    // you can reset the entire set with a new list of models, without firing
    // any granular `add` or `remove` events. Fires `reset` when finished.
    // Useful for bulk operations and optimizations.
    reset: function(models, options) {
      options || (options = {});
      for (var i = 0, l = this.models.length; i < l; i++) {
        this._removeReference(this.models[i]);
      }
      options.previousModels = this.models;
      this._reset();
      this.add(models, _.extend({silent: true}, options));
      if (!options.silent) this.trigger('reset', this, options);
      return this;
    },

    // Add a model to the end of the collection.
    push: function(model, options) {
      model = this._prepareModel(model, options);
      this.add(model, _.extend({at: this.length}, options));
      return model;
    },

    // Remove a model from the end of the collection.
    pop: function(options) {
      var model = this.at(this.length - 1);
      this.remove(model, options);
      return model;
    },

    // Add a model to the beginning of the collection.
    unshift: function(model, options) {
      model = this._prepareModel(model, options);
      this.add(model, _.extend({at: 0}, options));
      return model;
    },

    // Remove a model from the beginning of the collection.
    shift: function(options) {
      var model = this.at(0);
      this.remove(model, options);
      return model;
    },

    // Slice out a sub-array of models from the collection.
    slice: function(begin, end) {
      return this.models.slice(begin, end);
    },

    // Get a model from the set by id.
    get: function(obj) {
      if (obj == null) return void 0;
      return this._byId[obj.id != null ? obj.id : obj.cid || obj];
    },

    // Get the model at the given index.
    at: function(index) {
      return this.models[index];
    },

    // Return models with matching attributes. Useful for simple cases of
    // `filter`.
    where: function(attrs, first) {
      if (_.isEmpty(attrs)) return first ? void 0 : [];
      return this[first ? 'find' : 'filter'](function(model) {
        for (var key in attrs) {
          if (attrs[key] !== model.get(key)) return false;
        }
        return true;
      });
    },

    // Return the first model with matching attributes. Useful for simple cases
    // of `find`.
    findWhere: function(attrs) {
      return this.where(attrs, true);
    },

    // Force the collection to re-sort itself. You don't need to call this under
    // normal circumstances, as the set will maintain sort order as each item
    // is added.
    sort: function(options) {
      if (!this.comparator) throw new Error('Cannot sort a set without a comparator');
      options || (options = {});

      // Run sort based on type of `comparator`.
      if (_.isString(this.comparator) || this.comparator.length === 1) {
        this.models = this.sortBy(this.comparator, this);
      } else {
        this.models.sort(_.bind(this.comparator, this));
      }

      if (!options.silent) this.trigger('sort', this, options);
      return this;
    },

    // Figure out the smallest index at which a model should be inserted so as
    // to maintain order.
    sortedIndex: function(model, value, context) {
      value || (value = this.comparator);
      var iterator = _.isFunction(value) ? value : function(model) {
        return model.get(value);
      };
      return _.sortedIndex(this.models, model, iterator, context);
    },

    // Pluck an attribute from each model in the collection.
    pluck: function(attr) {
      return _.invoke(this.models, 'get', attr);
    },

    // Fetch the default set of models for this collection, resetting the
    // collection when they arrive. If `reset: true` is passed, the response
    // data will be passed through the `reset` method instead of `set`.
    fetch: function(options) {
      options = options ? _.clone(options) : {};
      if (options.parse === void 0) options.parse = true;
      var success = options.success;
      var collection = this;
      options.success = function(resp) {
        var method = options.reset ? 'reset' : 'set';
        collection[method](resp, options);
        if (success) success(collection, resp, options);
        collection.trigger('sync', collection, resp, options);
      };
      wrapError(this, options);
      return this.sync('read', this, options);
    },

    // Create a new instance of a model in this collection. Add the model to the
    // collection immediately, unless `wait: true` is passed, in which case we
    // wait for the server to agree.
    create: function(model, options) {
      options = options ? _.clone(options) : {};
      if (!(model = this._prepareModel(model, options))) return false;
      if (!options.wait) this.add(model, options);
      var collection = this;
      var success = options.success;
      options.success = function(resp) {
        if (options.wait) collection.add(model, options);
        if (success) success(model, resp, options);
      };
      model.save(null, options);
      return model;
    },

    // **parse** converts a response into a list of models to be added to the
    // collection. The default implementation is just to pass it through.
    parse: function(resp, options) {
      return resp;
    },

    // Create a new collection with an identical list of models as this one.
    clone: function() {
      return new this.constructor(this.models);
    },

    // Private method to reset all internal state. Called when the collection
    // is first initialized or reset.
    _reset: function() {
      this.length = 0;
      this.models = [];
      this._byId  = {};
    },

    // Prepare a hash of attributes (or other model) to be added to this
    // collection.
    _prepareModel: function(attrs, options) {
      if (attrs instanceof Model) {
        if (!attrs.collection) attrs.collection = this;
        return attrs;
      }
      options || (options = {});
      options.collection = this;
      var model = new this.model(attrs, options);
      if (!model._validate(attrs, options)) {
        this.trigger('invalid', this, attrs, options);
        return false;
      }
      return model;
    },

    // Internal method to sever a model's ties to a collection.
    _removeReference: function(model) {
      if (this === model.collection) delete model.collection;
      model.off('all', this._onModelEvent, this);
    },

    // Internal method called every time a model in the set fires an event.
    // Sets need to update their indexes when models change ids. All other
    // events simply proxy through. "add" and "remove" events that originate
    // in other collections are ignored.
    _onModelEvent: function(event, model, collection, options) {
      if ((event === 'add' || event === 'remove') && collection !== this) return;
      if (event === 'destroy') this.remove(model, options);
      if (model && event === 'change:' + model.idAttribute) {
        delete this._byId[model.previous(model.idAttribute)];
        if (model.id != null) this._byId[model.id] = model;
      }
      this.trigger.apply(this, arguments);
    }

  });

  // Underscore methods that we want to implement on the Collection.
  // 90% of the core usefulness of Backbone Collections is actually implemented
  // right here:
  var methods = ['forEach', 'each', 'map', 'collect', 'reduce', 'foldl',
    'inject', 'reduceRight', 'foldr', 'find', 'detect', 'filter', 'select',
    'reject', 'every', 'all', 'some', 'any', 'include', 'contains', 'invoke',
    'max', 'min', 'toArray', 'size', 'first', 'head', 'take', 'initial', 'rest',
    'tail', 'drop', 'last', 'without', 'indexOf', 'shuffle', 'lastIndexOf',
    'isEmpty', 'chain'];

  // Mix in each Underscore method as a proxy to `Collection#models`.
  _.each(methods, function(method) {
    Collection.prototype[method] = function() {
      var args = slice.call(arguments);
      args.unshift(this.models);
      return _[method].apply(_, args);
    };
  });

  // Underscore methods that take a property name as an argument.
  var attributeMethods = ['groupBy', 'countBy', 'sortBy'];

  // Use attributes instead of properties.
  _.each(attributeMethods, function(method) {
    Collection.prototype[method] = function(value, context) {
      var iterator = _.isFunction(value) ? value : function(model) {
        return model.get(value);
      };
      return _[method](this.models, iterator, context);
    };
  });

  // Backbone.View
  // -------------

  // Backbone Views are almost more convention than they are actual code. A View
  // is simply a JavaScript object that represents a logical chunk of UI in the
  // DOM. This might be a single item, an entire list, a sidebar or panel, or
  // even the surrounding frame which wraps your whole app. Defining a chunk of
  // UI as a **View** allows you to define your DOM events declaratively, without
  // having to worry about render order ... and makes it easy for the view to
  // react to specific changes in the state of your models.

  // Creating a Backbone.View creates its initial element outside of the DOM,
  // if an existing element is not provided...
  var View = Backbone.View = function(options) {
    this.cid = _.uniqueId('view');
    this._configure(options || {});
    this._ensureElement();
    this.initialize.apply(this, arguments);
    this.delegateEvents();
  };

  // Cached regex to split keys for `delegate`.
  var delegateEventSplitter = /^(\S+)\s*(.*)$/;

  // List of view options to be merged as properties.
  var viewOptions = ['model', 'collection', 'el', 'id', 'attributes', 'className', 'tagName', 'events'];

  // Set up all inheritable **Backbone.View** properties and methods.
  _.extend(View.prototype, Events, {

    // The default `tagName` of a View's element is `"div"`.
    tagName: 'div',

    // jQuery delegate for element lookup, scoped to DOM elements within the
    // current view. This should be prefered to global lookups where possible.
    $: function(selector) {
      return this.$el.find(selector);
    },

    // Initialize is an empty function by default. Override it with your own
    // initialization logic.
    initialize: function(){},

    // **render** is the core function that your view should override, in order
    // to populate its element (`this.el`), with the appropriate HTML. The
    // convention is for **render** to always return `this`.
    render: function() {
      return this;
    },

    // Remove this view by taking the element out of the DOM, and removing any
    // applicable Backbone.Events listeners.
    remove: function() {
      this.$el.remove();
      this.stopListening();
      return this;
    },

    // Change the view's element (`this.el` property), including event
    // re-delegation.
    setElement: function(element, delegate) {
      if (this.$el) this.undelegateEvents();
      this.$el = element instanceof Backbone.$ ? element : Backbone.$(element);
      this.el = this.$el[0];
      if (delegate !== false) this.delegateEvents();
      return this;
    },

    // Set callbacks, where `this.events` is a hash of
    //
    // *{"event selector": "callback"}*
    //
    //     {
    //       'mousedown .title':  'edit',
    //       'click .button':     'save'
    //       'click .open':       function(e) { ... }
    //     }
    //
    // pairs. Callbacks will be bound to the view, with `this` set properly.
    // Uses event delegation for efficiency.
    // Omitting the selector binds the event to `this.el`.
    // This only works for delegate-able events: not `focus`, `blur`, and
    // not `change`, `submit`, and `reset` in Internet Explorer.
    delegateEvents: function(events) {
      if (!(events || (events = _.result(this, 'events')))) return this;
      this.undelegateEvents();
      for (var key in events) {
        var method = events[key];
        if (!_.isFunction(method)) method = this[events[key]];
        if (!method) continue;

        var match = key.match(delegateEventSplitter);
        var eventName = match[1], selector = match[2];
        method = _.bind(method, this);
        eventName += '.delegateEvents' + this.cid;
        if (selector === '') {
          this.$el.on(eventName, method);
        } else {
          this.$el.on(eventName, selector, method);
        }
      }
      return this;
    },

    // Clears all callbacks previously bound to the view with `delegateEvents`.
    // You usually don't need to use this, but may wish to if you have multiple
    // Backbone views attached to the same DOM element.
    undelegateEvents: function() {
      this.$el.off('.delegateEvents' + this.cid);
      return this;
    },

    // Performs the initial configuration of a View with a set of options.
    // Keys with special meaning *(e.g. model, collection, id, className)* are
    // attached directly to the view.  See `viewOptions` for an exhaustive
    // list.
    _configure: function(options) {
      if (this.options) options = _.extend({}, _.result(this, 'options'), options);
      _.extend(this, _.pick(options, viewOptions));
      this.options = options;
    },

    // Ensure that the View has a DOM element to render into.
    // If `this.el` is a string, pass it through `$()`, take the first
    // matching element, and re-assign it to `el`. Otherwise, create
    // an element from the `id`, `className` and `tagName` properties.
    _ensureElement: function() {
      if (!this.el) {
        var attrs = _.extend({}, _.result(this, 'attributes'));
        if (this.id) attrs.id = _.result(this, 'id');
        if (this.className) attrs['class'] = _.result(this, 'className');
        var $el = Backbone.$('<' + _.result(this, 'tagName') + '>').attr(attrs);
        this.setElement($el, false);
      } else {
        this.setElement(_.result(this, 'el'), false);
      }
    }

  });

  // Backbone.sync
  // -------------

  // Override this function to change the manner in which Backbone persists
  // models to the server. You will be passed the type of request, and the
  // model in question. By default, makes a RESTful Ajax request
  // to the model's `url()`. Some possible customizations could be:
  //
  // * Use `setTimeout` to batch rapid-fire updates into a single request.
  // * Send up the models as XML instead of JSON.
  // * Persist models via WebSockets instead of Ajax.
  //
  // Turn on `Backbone.emulateHTTP` in order to send `PUT` and `DELETE` requests
  // as `POST`, with a `_method` parameter containing the true HTTP method,
  // as well as all requests with the body as `application/x-www-form-urlencoded`
  // instead of `application/json` with the model in a param named `model`.
  // Useful when interfacing with server-side languages like **PHP** that make
  // it difficult to read the body of `PUT` requests.
  Backbone.sync = function(method, model, options) {
    var type = methodMap[method];

    // Default options, unless specified.
    _.defaults(options || (options = {}), {
      emulateHTTP: Backbone.emulateHTTP,
      emulateJSON: Backbone.emulateJSON
    });

    // Default JSON-request options.
    var params = {type: type, dataType: 'json'};

    // Ensure that we have a URL.
    if (!options.url) {
      params.url = _.result(model, 'url') || urlError();
    }

    // Ensure that we have the appropriate request data.
    if (options.data == null && model && (method === 'create' || method === 'update' || method === 'patch')) {
      params.contentType = 'application/json';
      params.data = JSON.stringify(options.attrs || model.toJSON(options));
    }

    // For older servers, emulate JSON by encoding the request into an HTML-form.
    if (options.emulateJSON) {
      params.contentType = 'application/x-www-form-urlencoded';
      params.data = params.data ? {model: params.data} : {};
    }

    // For older servers, emulate HTTP by mimicking the HTTP method with `_method`
    // And an `X-HTTP-Method-Override` header.
    if (options.emulateHTTP && (type === 'PUT' || type === 'DELETE' || type === 'PATCH')) {
      params.type = 'POST';
      if (options.emulateJSON) params.data._method = type;
      var beforeSend = options.beforeSend;
      options.beforeSend = function(xhr) {
        xhr.setRequestHeader('X-HTTP-Method-Override', type);
        if (beforeSend) return beforeSend.apply(this, arguments);
      };
    }

    // Don't process data on a non-GET request.
    if (params.type !== 'GET' && !options.emulateJSON) {
      params.processData = false;
    }

    // If we're sending a `PATCH` request, and we're in an old Internet Explorer
    // that still has ActiveX enabled by default, override jQuery to use that
    // for XHR instead. Remove this line when jQuery supports `PATCH` on IE8.
    if (params.type === 'PATCH' && window.ActiveXObject &&
          !(window.external && window.external.msActiveXFilteringEnabled)) {
      params.xhr = function() {
        return new ActiveXObject("Microsoft.XMLHTTP");
      };
    }

    // Make the request, allowing the user to override any Ajax options.
    var xhr = options.xhr = Backbone.ajax(_.extend(params, options));
    model.trigger('request', model, xhr, options);
    return xhr;
  };

  // Map from CRUD to HTTP for our default `Backbone.sync` implementation.
  var methodMap = {
    'create': 'POST',
    'update': 'PUT',
    'patch':  'PATCH',
    'delete': 'DELETE',
    'read':   'GET'
  };

  // Set the default implementation of `Backbone.ajax` to proxy through to `$`.
  // Override this if you'd like to use a different library.
  Backbone.ajax = function() {
    return Backbone.$.ajax.apply(Backbone.$, arguments);
  };

  // Backbone.Router
  // ---------------

  // Routers map faux-URLs to actions, and fire events when routes are
  // matched. Creating a new one sets its `routes` hash, if not set statically.
  var Router = Backbone.Router = function(options) {
    options || (options = {});
    if (options.routes) this.routes = options.routes;
    this._bindRoutes();
    this.initialize.apply(this, arguments);
  };

  // Cached regular expressions for matching named param parts and splatted
  // parts of route strings.
  var optionalParam = /\((.*?)\)/g;
  var namedParam    = /(\(\?)?:\w+/g;
  var splatParam    = /\*\w+/g;
  var escapeRegExp  = /[\-{}\[\]+?.,\\\^$|#\s]/g;

  // Set up all inheritable **Backbone.Router** properties and methods.
  _.extend(Router.prototype, Events, {

    // Initialize is an empty function by default. Override it with your own
    // initialization logic.
    initialize: function(){},

    // Manually bind a single named route to a callback. For example:
    //
    //     this.route('search/:query/p:num', 'search', function(query, num) {
    //       ...
    //     });
    //
    route: function(route, name, callback) {
      if (!_.isRegExp(route)) route = this._routeToRegExp(route);
      if (_.isFunction(name)) {
        callback = name;
        name = '';
      }
      if (!callback) callback = this[name];
      var router = this;
      Backbone.history.route(route, function(fragment) {
        var args = router._extractParameters(route, fragment);
        callback && callback.apply(router, args);
        router.trigger.apply(router, ['route:' + name].concat(args));
        router.trigger('route', name, args);
        Backbone.history.trigger('route', router, name, args);
      });
      return this;
    },

    // Simple proxy to `Backbone.history` to save a fragment into the history.
    navigate: function(fragment, options) {
      Backbone.history.navigate(fragment, options);
      return this;
    },

    // Bind all defined routes to `Backbone.history`. We have to reverse the
    // order of the routes here to support behavior where the most general
    // routes can be defined at the bottom of the route map.
    _bindRoutes: function() {
      if (!this.routes) return;
      this.routes = _.result(this, 'routes');
      var route, routes = _.keys(this.routes);
      while ((route = routes.pop()) != null) {
        this.route(route, this.routes[route]);
      }
    },

    // Convert a route string into a regular expression, suitable for matching
    // against the current location hash.
    _routeToRegExp: function(route) {
      route = route.replace(escapeRegExp, '\\$&')
                   .replace(optionalParam, '(?:$1)?')
                   .replace(namedParam, function(match, optional){
                     return optional ? match : '([^\/]+)';
                   })
                   .replace(splatParam, '(.*?)');
      return new RegExp('^' + route + '$');
    },

    // Given a route, and a URL fragment that it matches, return the array of
    // extracted decoded parameters. Empty or unmatched parameters will be
    // treated as `null` to normalize cross-browser behavior.
    _extractParameters: function(route, fragment) {
      var params = route.exec(fragment).slice(1);
      return _.map(params, function(param) {
        return param ? decodeURIComponent(param) : null;
      });
    }

  });

  // Backbone.History
  // ----------------

  // Handles cross-browser history management, based on either
  // [pushState](http://diveintohtml5.info/history.html) and real URLs, or
  // [onhashchange](https://developer.mozilla.org/en-US/docs/DOM/window.onhashchange)
  // and URL fragments. If the browser supports neither (old IE, natch),
  // falls back to polling.
  var History = Backbone.History = function() {
    this.handlers = [];
    _.bindAll(this, 'checkUrl');

    // Ensure that `History` can be used outside of the browser.
    if (typeof window !== 'undefined') {
      this.location = window.location;
      this.history = window.history;
    }
  };

  // Cached regex for stripping a leading hash/slash and trailing space.
  var routeStripper = /^[#\/]|\s+$/g;

  // Cached regex for stripping leading and trailing slashes.
  var rootStripper = /^\/+|\/+$/g;

  // Cached regex for detecting MSIE.
  var isExplorer = /msie [\w.]+/;

  // Cached regex for removing a trailing slash.
  var trailingSlash = /\/$/;

  // Has the history handling already been started?
  History.started = false;

  // Set up all inheritable **Backbone.History** properties and methods.
  _.extend(History.prototype, Events, {

    // The default interval to poll for hash changes, if necessary, is
    // twenty times a second.
    interval: 50,

    // Gets the true hash value. Cannot use location.hash directly due to bug
    // in Firefox where location.hash will always be decoded.
    getHash: function(window) {
      var match = (window || this).location.href.match(/#(.*)$/);
      return match ? match[1] : '';
    },

    // Get the cross-browser normalized URL fragment, either from the URL,
    // the hash, or the override.
    getFragment: function(fragment, forcePushState) {
      if (fragment == null) {
        if (this._hasPushState || !this._wantsHashChange || forcePushState) {
          fragment = this.location.pathname;
          var root = this.root.replace(trailingSlash, '');
          if (!fragment.indexOf(root)) fragment = fragment.substr(root.length);
        } else {
          fragment = this.getHash();
        }
      }
      return fragment.replace(routeStripper, '');
    },

    // Start the hash change handling, returning `true` if the current URL matches
    // an existing route, and `false` otherwise.
    start: function(options) {
      if (History.started) throw new Error("Backbone.history has already been started");
      History.started = true;

      // Figure out the initial configuration. Do we need an iframe?
      // Is pushState desired ... is it available?
      this.options          = _.extend({}, {root: '/'}, this.options, options);
      this.root             = this.options.root;
      this._wantsHashChange = this.options.hashChange !== false;
      this._wantsPushState  = !!this.options.pushState;
      this._hasPushState    = !!(this.options.pushState && this.history && this.history.pushState);
      var fragment          = this.getFragment();
      var docMode           = document.documentMode;
      var oldIE             = (isExplorer.exec(navigator.userAgent.toLowerCase()) && (!docMode || docMode <= 7));

      // Normalize root to always include a leading and trailing slash.
      this.root = ('/' + this.root + '/').replace(rootStripper, '/');

      if (oldIE && this._wantsHashChange) {
        this.iframe = Backbone.$('<iframe src="javascript:0" tabindex="-1" />').hide().appendTo('body')[0].contentWindow;
        this.navigate(fragment);
      }

      // Depending on whether we're using pushState or hashes, and whether
      // 'onhashchange' is supported, determine how we check the URL state.
      if (this._hasPushState) {
        Backbone.$(window).on('popstate', this.checkUrl);
      } else if (this._wantsHashChange && ('onhashchange' in window) && !oldIE) {
        Backbone.$(window).on('hashchange', this.checkUrl);
      } else if (this._wantsHashChange) {
        this._checkUrlInterval = setInterval(this.checkUrl, this.interval);
      }

      // Determine if we need to change the base url, for a pushState link
      // opened by a non-pushState browser.
      this.fragment = fragment;
      var loc = this.location;
      var atRoot = loc.pathname.replace(/[^\/]$/, '$&/') === this.root;

      // If we've started off with a route from a `pushState`-enabled browser,
      // but we're currently in a browser that doesn't support it...
      if (this._wantsHashChange && this._wantsPushState && !this._hasPushState && !atRoot) {
        this.fragment = this.getFragment(null, true);
        this.location.replace(this.root + this.location.search + '#' + this.fragment);
        // Return immediately as browser will do redirect to new url
        return true;

      // Or if we've started out with a hash-based route, but we're currently
      // in a browser where it could be `pushState`-based instead...
      } else if (this._wantsPushState && this._hasPushState && atRoot && loc.hash) {
        this.fragment = this.getHash().replace(routeStripper, '');
        this.history.replaceState({}, document.title, this.root + this.fragment + loc.search);
      }

      if (!this.options.silent) return this.loadUrl();
    },

    // Disable Backbone.history, perhaps temporarily. Not useful in a real app,
    // but possibly useful for unit testing Routers.
    stop: function() {
      Backbone.$(window).off('popstate', this.checkUrl).off('hashchange', this.checkUrl);
      clearInterval(this._checkUrlInterval);
      History.started = false;
    },

    // Add a route to be tested when the fragment changes. Routes added later
    // may override previous routes.
    route: function(route, callback) {
      this.handlers.unshift({route: route, callback: callback});
    },

    // Checks the current URL to see if it has changed, and if it has,
    // calls `loadUrl`, normalizing across the hidden iframe.
    checkUrl: function(e) {
      var current = this.getFragment();
      if (current === this.fragment && this.iframe) {
        current = this.getFragment(this.getHash(this.iframe));
      }
      if (current === this.fragment) return false;
      if (this.iframe) this.navigate(current);
      this.loadUrl() || this.loadUrl(this.getHash());
    },

    // Attempt to load the current URL fragment. If a route succeeds with a
    // match, returns `true`. If no defined routes matches the fragment,
    // returns `false`.
    loadUrl: function(fragmentOverride) {
      var fragment = this.fragment = this.getFragment(fragmentOverride);
      var matched = _.any(this.handlers, function(handler) {
        if (handler.route.test(fragment)) {
          handler.callback(fragment);
          return true;
        }
      });
      return matched;
    },

    // Save a fragment into the hash history, or replace the URL state if the
    // 'replace' option is passed. You are responsible for properly URL-encoding
    // the fragment in advance.
    //
    // The options object can contain `trigger: true` if you wish to have the
    // route callback be fired (not usually desirable), or `replace: true`, if
    // you wish to modify the current URL without adding an entry to the history.
    navigate: function(fragment, options) {
      if (!History.started) return false;
      if (!options || options === true) options = {trigger: options};
      fragment = this.getFragment(fragment || '');
      if (this.fragment === fragment) return;
      this.fragment = fragment;
      var url = this.root + fragment;

      // If pushState is available, we use it to set the fragment as a real URL.
      if (this._hasPushState) {
        this.history[options.replace ? 'replaceState' : 'pushState']({}, document.title, url);

      // If hash changes haven't been explicitly disabled, update the hash
      // fragment to store history.
      } else if (this._wantsHashChange) {
        this._updateHash(this.location, fragment, options.replace);
        if (this.iframe && (fragment !== this.getFragment(this.getHash(this.iframe)))) {
          // Opening and closing the iframe tricks IE7 and earlier to push a
          // history entry on hash-tag change.  When replace is true, we don't
          // want this.
          if(!options.replace) this.iframe.document.open().close();
          this._updateHash(this.iframe.location, fragment, options.replace);
        }

      // If you've told us that you explicitly don't want fallback hashchange-
      // based history, then `navigate` becomes a page refresh.
      } else {
        return this.location.assign(url);
      }
      if (options.trigger) this.loadUrl(fragment);
    },

    // Update the hash location, either replacing the current entry, or adding
    // a new one to the browser history.
    _updateHash: function(location, fragment, replace) {
      if (replace) {
        var href = location.href.replace(/(javascript:|#).*$/, '');
        location.replace(href + '#' + fragment);
      } else {
        // Some browsers require that `hash` contains a leading #.
        location.hash = '#' + fragment;
      }
    }

  });

  // Create the default Backbone.history.
  Backbone.history = new History;

  // Helpers
  // -------

  // Helper function to correctly set up the prototype chain, for subclasses.
  // Similar to `goog.inherits`, but uses a hash of prototype properties and
  // class properties to be extended.
  var extend = function(protoProps, staticProps) {
    var parent = this;
    var child;

    // The constructor function for the new subclass is either defined by you
    // (the "constructor" property in your `extend` definition), or defaulted
    // by us to simply call the parent's constructor.
    if (protoProps && _.has(protoProps, 'constructor')) {
      child = protoProps.constructor;
    } else {
      child = function(){ return parent.apply(this, arguments); };
    }

    // Add static properties to the constructor function, if supplied.
    _.extend(child, parent, staticProps);

    // Set the prototype chain to inherit from `parent`, without calling
    // `parent`'s constructor function.
    var Surrogate = function(){ this.constructor = child; };
    Surrogate.prototype = parent.prototype;
    child.prototype = new Surrogate;

    // Add prototype properties (instance properties) to the subclass,
    // if supplied.
    if (protoProps) _.extend(child.prototype, protoProps);

    // Set a convenience property in case the parent's prototype is needed
    // later.
    child.__super__ = parent.prototype;

    return child;
  };

  // Set up inheritance for the model, collection, router, view and history.
  Model.extend = Collection.extend = Router.extend = View.extend = History.extend = extend;

  // Throw an error when a URL is needed, and none is supplied.
  var urlError = function() {
    throw new Error('A "url" property or function must be specified');
  };

  // Wrap an optional error callback with a fallback error event.
  var wrapError = function (model, options) {
    var error = options.error;
    options.error = function(resp) {
      if (error) error(model, resp, options);
      model.trigger('error', model, resp, options);
    };
  };

}).call(this);
var _ = require('underscore');

var BSim = {};
BSim.Beta = function() {
    "use strict";
    var self = this;
    _.extend(this, Backbone.Events);

    var mMemory = new BSim.Beta.Memory(this); // TODO: it might make sense to use an Int32Array here.
    this.memory = mMemory;
    var mSourceMap = [];  // file & line number for source of each assembled byte
    var mRegisters = new Int32Array(32);
    var mRunning = false; // Only true when calling run(); not executeCycle().
    var mPC = 0x80000000;
    var mPendingInterrupts = 0; // Used to store any pending interrupt.
    var mCycleCount = 0;
    var mClockCounter = 0;
    var CYCLES_PER_TICK = 10000;
    // These need to be public for the instructions to look at.
    this.mMouseCoords = -1;
    this.mKeyboardInput = null;
    this.mServerInfo = [];

    // We use these in the 'running' state so we can batch DOM updates,
    // on the theory that changing object properties is cheap but changing DOM
    // nodes is expensive. Changes are thus cached in here until the end of the
    // quantum, then shipped off to anything that cares and cleared.
    // When not in run mode (i.e. mRunning is false and stepping through),
    // changes are signalled immediately and these are not used.
    var mChangedRegisters = {};
    var mChangedWords = {};

    // These track last reads/writes to registers and memory, used for highlighting them in the
    // UI.
    // Memory tracks the last five accesses; registers track whatever happened in the current cycle.
    var mLastReads = [];
    var mLastWrites = [];
    var mCurrentRegisterReads = [];
    var mCurrentRegisterWrites = [];

    // Used for 'step back'
    var mHistory = new Dequeue();
    var mCurrentStep = {};
    // The below two are an optimisation for Safari. While V8 and whatever Firefox uses can
    // optimise for the 'class' pattern formed by UndoStep, Safari cannot. This means that
    // accessing mCurrentStep.registers is extremely slow, even when mCurrentStep is an
    // instance of a recognisable class (UndoStep). To work around this, we instead use
    // variables to hold the frequently accessed members of mCurrentStep, then stuff them
    // in at the end of each cycle.
    var mCurrentStepRegisters = {};
    var mCurrentStepWords = [];

    // Information not strictly related to running the Beta, but needed in BSim
    var mBreakpoints = {};
    var mLabels = {};
    var mOptions = {};
    var mVerifier = null;
    var mTTYContent = '';
    this.mSources = [];  // list of {file: name, contents: ...}

    // Mostly exception stuff.
    var SUPERVISOR_BIT = 0x80000000;
    var XP = 30;
    var VEC_RESET = 0;
    var VEC_II = 4;
    var VEC_CLK = 8;
    var VEC_KBD = 12;
    var VEC_MOUSE = 16;

    var INTERRUPT_CLOCK = 0x02;
    var INTERRUPT_KEYBOARD = 0x04;
    var INTERRUPT_MOUSE = 0x08;

    var UndoStep = function(pc) {
        this.registers = {};
        this.words = {};
        this.pc = pc;
        this.tty = null;
    };

    var set_defaults = function() {
        mOptions = {
            clock: false,
            div: true,
            mul: true,
            kalways: false,
            tty: false,
            annotate: false
        };
    };
    set_defaults();

    this.setSources = function(sources) {
        this.mSources = sources;
    };

    this.loadBytes = function(bytes,source_map) {
        this.stop();
        this.reset();

        mMemory.loadBytes(bytes);
        mSourceMap = source_map;
        set_defaults();

        // Update the UI with our new program.
        this.trigger('resize:memory', bytes.length);
        // This trigger is redundant to the reset performed by this.reset()
        // this.trigger('change:bulk:register', _.object(_.range(32), mRegisters));
        var r = _.range(0, mMemory.size(), 4);
        this.trigger('change:bulk:word', _.object(r, _.map(r, function(i) { return mMemory.readWord(i); } )));

        this.clearBreakpoints();
        this.setLabels({});
        this.setPC(SUPERVISOR_BIT);
    };

    this.setOption = function(option, enabled) {
        mOptions[option] = enabled;
    };

    this.isOptionSet = function(option) {
        return !!mOptions[option];
    };

    // Takes a list of breakpoint addresses and replaces all current breakpoints with them.
    this.setBreakpoints = function(breakpoints) {
        mBreakpoints = _.object(_.map(breakpoints, function(b) { return [b, true]; }));
        this.trigger('add:bulk:breakpoint', breakpoints);
    };

    this.clearBreakpoints = function() {
        this.trigger('delete:bulk:breakpoint', _.map(_.keys(mBreakpoints), function(v) { return parseInt(v, 10); }));
        mBreakpoints = {};
    };

    this.addBreakpoint = function(breakpoint) {
        mBreakpoints[breakpoint] = true;
        this.trigger('add:breakpoint', breakpoint);
    };

    this.removeBreakpoint = function(breakpoint) {
        delete mBreakpoints[breakpoint];
        this.trigger('delete:breakpoint', breakpoint);
    };

    this.getBreakpoints = function() {
        return _.map(_.keys(mBreakpoints), function(v) { return parseInt(v, 10); });
    };

    this.setLabels = function(labels) {
        mLabels = _.invert(labels);
        this.trigger('change:bulk:labels', mLabels);
    };

    this.getLabel = function(address) {
        return mLabels[address & ~SUPERVISOR_BIT] || null;
    };

    this.setVerifier = function(verifier) {
        this.mVerifier = verifier;
    };

    this.verifier = function() {
        return this.mVerifier;
    };

    this.readWord = function(address, notify, fetch) {
        address &= (~SUPERVISOR_BIT) & 0xFFFFFFFC;
        if(notify) {
            if(!mRunning) {
                self.trigger('read:word', address);
            } else {
                mLastReads.push(address);
                if(mLastReads.length > 5) mLastReads.shift();
            }
        }

        return mMemory.readWordCached(address, fetch);
    };

    this.writeWord = function(address, value, notify) {
        value |= 0; // force to int.
        address &= ~SUPERVISOR_BIT;
        address &= 0xFFFFFFFC; // Force multiples of four.

        // Implement undo
         mCurrentStepWords.push([address, mMemory.readWord(address)]);

        mMemory.writeWordCached(address, value);

        if(!mRunning) this.trigger('change:word', address, value);
        if(notify) {
            if(!mRunning) {
                this.trigger('write:word', address);
            } else {
                mLastWrites.push(address);
                if(mLastWrites.length > 5) mLastWrites.shift();
            }
        }
        mChangedWords[address] = value;
    };

    this.readRegister = function(register) {
        if(register < 0 || register > 31) {
            throw new BSim.Beta.RuntimeError("Attempted to read invalid register r" + register);
        }
        if(register == 31) return 0;
        return mRegisters[register];
    };

    this.writeRegister = function(register, value) {
        value |= 0; // force to int.
        if(register == 31) return;

        if(register < 0 || register > 31) {
            throw new BSim.Beta.RuntimeError("Attempted to write invalid register r" + register);
        }

        // Implement undo
        if(!_.has(mCurrentStepRegisters, register))
            mCurrentStepRegisters[register] = mRegisters[register];

        mRegisters[register] = value;

        if(!mRunning) this.trigger('change:register', register, value);
        else mChangedRegisters[register] = value;
    };

    // This differs from readRegister in that it also logs the access.
    // It should be used when the machine would actually read from the
    // register.
    this.realReadRegister = function(register) {
        mCurrentRegisterReads.push(register);
        return self.readRegister(register);
    };

    this.realWriteRegister = function(register, value) {
        mCurrentRegisterWrites.push(register);
        return self.writeRegister(register, value);
    };

    this.setPC = function(address, allow_supervisor) {
        if(!(mPC & SUPERVISOR_BIT) && !allow_supervisor) address &= ~SUPERVISOR_BIT;
        if(this.isOptionSet('kalways')) address |= SUPERVISOR_BIT;
        mPC = address & 0xFFFFFFFC; // Only multiples of four are valid.

        if(!mRunning) this.trigger('change:pc', address);
    };

    this.getPC = function() {
        return mPC;
    };

    this.signExtend16 = function(value) {
        value &= 0xFFFF;
        if(value & 0x8000) {
            value = value - 0x10000;
        }
        return value;
    };

    this.decodeInstruction = function(instruction) {
        var opcode = (instruction >> 26) & 0x3F;
        var rc = (instruction >> 21) & 0x1F;
        var ra = (instruction >> 16) & 0x1F;
        var rb = (instruction >> 11) & 0x1F;
        var literal = this.signExtend16(instruction & 0xFFFF);

        return {
            opcode: opcode,
            ra: ra,
            rb: rb,
            rc: rc,
            literal: literal
        };
    };

    this.reset = function(no_update_memory) {
        this.setPC(SUPERVISOR_BIT | VEC_RESET, true);
        mRegisters = new Int32Array(32);
        mPendingInterrupts = 0;
        mCycleCount = 0;
        this.trigger('change:cycle_count',0);
        mClockCounter = 0;
        this.mServerInfo = [];
        if(!no_update_memory) mMemory.reset();
        this.mMouseCoords = -1;
        this.mKeyboardInput = null;
        mTTYContent = '';

        // Tell the world.
        this.trigger('text:clear');
        this.trigger('change:bulk:register', _.object(_.range(32), mRegisters));
        if(!no_update_memory) {
            var r = _.range(0, mMemory.size(), 4);
            this.trigger('change:bulk:word', _.object(r, _.map(r, function(v) { return mMemory.readWord(v); })));
        }
    };

    this.ttyOut = function(text) {
        mCurrentStep.tty = mTTYContent;
        mTTYContent += text;
        this.trigger('text:out', text);
    };

    this.ttyContent = function() {
        return mTTYContent;
    };

    this.inSupervisorMode = function() {
        return !!(mPC & SUPERVISOR_BIT);
    };

    // Called when any illegal instruction is executed.
    this.handleIllegalInstruction = function(decoded) {
        /*if(this.inSupervisorMode()) {
            // This is "implementation defined"; we whine on any tty and then halt.
            this.trigger("out:text", "\nIllegal operation while in supervisor mode! Halting.\n");
            return false;
        }*/
        this.writeRegister(XP, mPC);
        this.setPC(SUPERVISOR_BIT | VEC_II, true);
    };

    // Various interrupts.
    // These are split into two parts: one at the time that the interrupt actually occurs,
    // and another when the beta is able to service it (the next time it leaves supervisor mode).

    // Triggers a clock interrupt
    this.clockInterrupt = function() {
        if(this.isOptionSet('clock')) {
            mPendingInterrupts |= INTERRUPT_CLOCK;
        }
    };

    var doClockInterrupt = function() {
        self.writeRegister(XP, mPC+4);
        self.setPC(SUPERVISOR_BIT | VEC_CLK, true);
        mPendingInterrupts &= ~INTERRUPT_CLOCK;
    };

    // Keyboard interrupt
    this.keyboardInterrupt = function(character) {
        if(this.isOptionSet('tty')) {
            this.mKeyboardInput = character; // TODO: buffering?
            mPendingInterrupts |= INTERRUPT_KEYBOARD;
        }
    };

    var doKeyboardInterrupt = function() {
        self.writeRegister(XP, mPC+4);
        self.setPC(SUPERVISOR_BIT | VEC_KBD, true);
        mPendingInterrupts &= ~INTERRUPT_KEYBOARD;
    };

    // Mouse interrupt
    this.mouseInterrupt = function(x, y) {
        if(this.isOptionSet('tty')) {
            this.mMouseCoords = ((x & 0xFFFF) << 16) | (y & 0xFFFF);
            mPendingInterrupts |= INTERRUPT_MOUSE;
        }
    };

    var doMouseInterrupt = function() {
        self.writeRegister(XP, mPC+4);
        self.setPC(SUPERVISOR_BIT | VEC_MOUSE, true);
        mPendingInterrupts &= ~INTERRUPT_MOUSE;
    };

    this.getCycleCount = function() {
        return mCycleCount;
    };

    // Executes a single instruction (this is not a fancy beta).
    // Returns false if execution should halt; otherwise the return
    // value is undefined (but may well be 'undefined').
    // Use ===.
    this.executeCycle = function() {
        if(mBreakpoints[mPC & ~SUPERVISOR_BIT] === false) {
            mBreakpoints[mPC & ~SUPERVISOR_BIT] = true;
        }

        // Clean up records of read/written registers.
        mCurrentRegisterReads = [];
        mCurrentRegisterWrites = [];

        // Check if we should fire a clock exception first.
        if(++mClockCounter % CYCLES_PER_TICK === 0) {
            mClockCounter = 0;
            this.clockInterrupt();
        }
        // Execute interrupts.
        // TODO: Cleanup
        if(!this.inSupervisorMode()) {
            if(mPendingInterrupts & INTERRUPT_CLOCK) {
                doClockInterrupt();
                return;
            }
            if(mPendingInterrupts & INTERRUPT_KEYBOARD) {
                doKeyboardInterrupt();
                return;
            }
            if(mPendingInterrupts & INTERRUPT_MOUSE) {
                doMouseInterrupt();
                return;
            }
        }
        // Prepare undo
        mCurrentStep = new UndoStep(mPC);
        mCurrentStepWords = [];
        mCurrentStepRegisters = {};

        //cjt: move try up here so that instruction fetch errors are caught
        try {
            mCycleCount = (mCycleCount + 1) % 0x7FFFFFFF;
            if(!mRunning) this.trigger('change:cycle_count',mCycleCount);

            // Continue on with instructions as planned.
            var instruction = this.readWord(mPC, false, true);
            var old_pc = mPC;
            mPC += 4; // Increment this early so that we have the right reference for exceptions.
            if(instruction === 0) {
                mPC -= 4;
                return false;
            }
            var decoded = this.decodeInstruction(instruction);
            var op = BSim.Beta.Opcodes[decoded.opcode];
            if(!op) {
                // Illegal opcode.
                return this.handleIllegalInstruction(decoded);
            }
            if(op.privileged && !(mPC & SUPERVISOR_BIT)) {
                return this.handleIllegalInstruction(decoded);
            }
            if(!mRunning) this.trigger('change:pc', mPC);
            //cjt: previous location of try
            var ret = null;
            if(op.has_literal) {
                ret = op.exec.call(this, decoded.ra, decoded.literal, decoded.rc);
            } else {
                ret = op.exec.call(this, decoded.ra, decoded.rb, decoded.rc);
            }
            if(ret === false) {
                this.setPC(old_pc, true);
            }
            if(!mRunning) {
                this.trigger('read:register', mCurrentRegisterReads);
                this.trigger('write:register', mCurrentRegisterWrites);
            }
            mCurrentStep.registers = mCurrentStepRegisters;
            mCurrentStep.words = mCurrentStepWords;
            mHistory.push(mCurrentStep);
            if(mHistory.length() > 50) mHistory.shift();
            return ret;
        } catch(e) {
            if(e instanceof BSim.Beta.RuntimeError) {
                e.message += ' [PC = 0x'+BSim.Common.FormatWord(mPC)+']';
                this.trigger('error', e);
                this.setPC(old_pc, true);
                return false;
            } else {
                throw e;
            }
        }

        return false;
    };

    this.undoCycle = function() {
        if(!mHistory.length) return false;
        var step = mHistory.pop();
        _.each(step.registers, function(value, register) {
            self.writeRegister(register, value);
        });
        var done = {};
        _.each(step.words, function(tuple) {
            var address = tuple[0], value = tuple[1];
            if(done[address]) return;
            done[address] = true;
            mMemory.writeWord(address, value);
            this.trigger('change:word', address, value);
        });
        self.setPC(step.pc, true);
        if(step.tty) {
            mTTYContent = step.tty;
            self.trigger('text:replace', mTTYContent);
        }
        mCycleCount = (mCycleCount - 1) % 0x7FFFFFFF;
        if(!mRunning) this.trigger('change:cycle_count',mCycleCount);
        return true;
    };

    this.undoLength = function() {
        return mHistory.length();
    };

    // Runs the program to completion (if it terminates) or until stopped using
    // stop(). Yields to the UI every `quantum` emulated cycles.
    // When run using this function, the emulator does not issue standard change events,
    // instead issuing a change:all event after each quantum and when stopping
    // (even if nothing actually changed).
    // This function is non-blocking.
    this.run = function(quantum) {
        this.trigger('run:start');
        mRunning = true;
        _.defer(function run_inner() {
            var exception = null;
            // Bail out if we're not supposed to run any more.
            if(!mRunning) {
                self.trigger('run:stop');
                return;
            }
            var i = quantum;
            // Execute quantum cycles, then yield for the UI.
            while(i--) {
                // Check for a breakpoint
                var real_pc = mPC & ~SUPERVISOR_BIT;
                if(mBreakpoints[real_pc] === true) {
                    mBreakpoints[real_pc] = false;
                    mRunning = false;
                    break;
                }
                // This means we should terminate.
                if(self.executeCycle() === false) {
                    mRunning = false;
                    break;
                }
            }
            // Now relay all the changes that occurred during our quantum.
            self.trigger('change:bulk:word', mChangedWords);
            self.trigger('change:bulk:register', mChangedRegisters);
            self.trigger('read:bulk:word', mLastReads);
            self.trigger('write:bulk:word', mLastWrites);
            self.trigger('read:register', mCurrentRegisterReads);
            self.trigger('write:register', mCurrentRegisterWrites);
            self.trigger('change:pc', mPC);
            self.trigger('change:cycle_count', mCycleCount);
            mChangedRegisters = {};
            mChangedWords = {};
            mLastReads = [];
            mLastWrites = [];

            // Run again.
            _.defer(run_inner);
        });
    };

    this.stop = function() {
        mRunning = false;
    };

    this.isRunning = function() {
        return mRunning;
    };

    // Returns memory size in bytes.
    this.memorySize = function() {
        return mMemory.size();
    };

    this.getMemory = function() {
        return mMemory;
    };

    var oldHighlightObject;
    this.highlightSource = function(pc) {
        if ($('#editor').width() == 0 || pc >= mSourceMap.length) return;
        var source = mSourceMap[pc];
        if (source !== undefined) {
            if(oldHighlightObject) oldHighlightObject.clear();
            oldHighlightObject = editor.addLineClass(source.file, source.line-1, 'highlight-line');
            editor.showLine(source.file, source.line-1);  // make sure we can see it!
        }
    }

    return this;
};

BSim.Beta.RuntimeError = function(message) {
    this.message = message;
};
BSim.Beta.RuntimeError.prototype.toString = function() {
    return this.message;
};
BSim.Common = {
    RegisterName: function(reg) {
        var special = {
            27: 'BP',
            28: 'LP',
            29: 'SP',
            30: 'XP'
        };
        if(special[reg]) return special[reg];
        else return 'R' + reg;
    },
    FormatWord: function(value, length) {
        // Fix up negative numbers
        value = BSim.Common.FixUint(value|0);
        // Default to a full representation of the 32-bit value
        if(!length) length = 8;
        var s = value.toString(16);
        // Truncate, if necessary
        s = s.substr(Math.max(0, s.length - length), length);
        // Zero pad, if necessary
        while(s.length < length) s = "0" + s;
        return s;
    },
    FixUint: function(value) {
        if (value < 0) value = 0xFFFFFFFF + value + 1;
        return value;
    }
};
BSim.Beta.Opcodes = {};

/*
 * betaop() takes one argument that should completely define an opcode.
 * It is an object with the following fields:
 *
 * opcode: numeric opcode (mandatory)
 * name: mnemonic for the opcode (mandatory)
 * exec: function that implements the opcode.
 *       Although it is redundant, do include the opcode name as an identifier. It shows in the profiler
 *       an back traces; if omitted you just get either 'exec' or '(anonymous function)'.
 * disassemble: returns a string giving the disassembly of the instruction (optional)
 * privileged: true if the opcode can only be used in supervisor mode (optional; default false)
 * has_literal: true if the opcode takes a literal instead of a register (optional; defualt false)
 * paths: dictionary of signal values that the opcode should trigger. If omitted, default is determined by
 *        the value of has_literal and alufn must be provided.
 * alufn: Used when paths is omitted to provide a sane default. (mandatory iff paths is omitted)
 */

// Build up our opcode table.
(function() {
    // Gives a register its best name
    var name_register = BSim.Common.RegisterName;

    // The generic dissasembly functions
    var generic_disassemble = function(op, a, b, c) {
        return op.name + "(" + name_register(a) + ", " + name_register(b) + ", " + name_register(c) + ")";
    };

    var generic_disassemblec = function(op, a, literal, c) {
        return op.name + "(" + name_register(a) + ", " + literal + ", " + name_register(c) + ")";
    };

    var betaop = function(op) {
        // Unify shorthand.
        if(!op.privileged) op.privileged = false;
        if(!op.has_literal) op.has_literal = false;
        if(!op.disassemble) {
            // op.disassemble = function() {return '';};
            if(!op.has_literal) {
                op.disassemble = function(decoded) {
                    return generic_disassemble(op, decoded.ra, decoded.rb, decoded.rc);
                };
            } else {
                op.disassemble = function(decoded) {
                    return generic_disassemblec(op, decoded.ra, decoded.literal, decoded.rc);
                };
            }
        }
        if(!op.paths) {
            var alufn = op.alufn || null;
            if(op.has_literal) {
                op.paths = {
                    alufn: alufn,
                    werf: 1,
                    bsel: 1,
                    wdsel: 1,
                    wr: 0,
                    pcsel: 0,
                    asel: 0,
                    wasel: 0
                };
            } else {
                op.paths = {
                    alufn: alufn,
                    werf: 1,
                    bsel: 0,
                    wdsel: 1,
                    wr: 0,
                    ra2sel: 0,
                    pcsel: 0,
                    asel: 0,
                    wasel: 0
                };
            }
        }
        var signals = ['alufn','werf','bsel','wdsel','wr','ra2sel','pcsel','asel','wasel'];
        _.each(signals, function(signal) {
            if(op.paths[signal] === undefined) {
                op.paths[signal] = null;
            }
        });
        // Insert it into useful places.
        BSim.Beta.Opcodes[op.opcode] = op;
    };

    betaop({
        opcode: 0x20,
        name: 'ADD',
        alufn: '+',
        exec: function ADD(a, b, c) {
            this.realWriteRegister(c, this.realReadRegister(a) + this.realReadRegister(b));
        },
        disassemble: function(op) {
            if(op.rb == 31) return "MOVE(" + name_register(op.ra) + ", " + name_register(op.rc) + ")";
            else return "ADD(" + name_register(op.ra) + ", " + name_register(op.rb) + ", " + name_register(op.rc) + ")";
        }
    });

    betaop({
        opcode: 0x30,
        name: 'ADDC',
        has_literal: true,
        alufn: '+',
        exec: function ADDC(a, literal, c) {
            this.realWriteRegister(c, this.realReadRegister(a) + literal);
        },
        disassemble: function(op) {
            if(op.ra == 31) return "CMOVE(" + op.literal + ", " + name_register(op.rc) + ")";
            else return "ADDC(" + name_register(op.ra) + ", " + op.literal + ", " + name_register(op.rc) + ")";
        }
    });

    betaop({
        opcode: 0x28,
        name: 'AND',
        alufn: '&',
        exec: function AND(a, b, c) {
            this.realWriteRegister(c, this.realReadRegister(a) & this.realReadRegister(b));
        }
    });

    betaop({
        opcode: 0x38,
        name: 'ANDC',
        alufn: '&',
        has_literal: true,
        exec: function ANDC(a, literal, c) {
            this.realWriteRegister(c, this.realReadRegister(a) & literal);
        }
    });

    betaop({
        opcode: 0x1C,
        name: 'BEQ',
        paths: {
            werf: 1,
            wdsel: 0,
            wr: 0,
            pcsel: 'z',
            wasel: 0,
            z: true
        },
        has_literal: true,
        exec: function BEQ(a, literal, c) {
            var pc = this.getPC();
            if(this.realReadRegister(a) === 0) {
                this.setPC(pc + 4*literal, false);
            }
            this.realWriteRegister(c, pc);
        },
        disassemble: function(op, pc) {
            var target = ((op.literal * 4) + (pc + 4)) & ~0x80000000;
            var label = this.getLabel(target) || target;
            if(op.ra == 31) {
                if(op.rc == 31) {
                    return "BR(" + label + ")";
                } else {
                    return "BR(" + label + ", " + name_register(op.rc) + ")";
                }
            } else {
                return "BEQ(" + name_register(op.ra) + ", " + label + ", " + name_register(op.rc) + ")";
            }
        }
    });

    betaop({
        opcode: 0x1D,
        name: 'BNE',
        has_literal: true,
        paths: {
            werf: 1,
            wdsel: 0,
            wr: 0,
            pcsel: '~z',
            wasel: 0,
            z: true
        },
        exec: function BNE(a, literal, c) {
            var pc = this.getPC();
            if(this.realReadRegister(a) !== 0) {
                this.setPC(pc + 4*literal, false);
            }
            this.realWriteRegister(c, pc);
        },
        disassemble: function(op, pc) {
            var target = ((op.literal * 4) + (pc + 4)) & ~0x80000000;
            var label = this.getLabel(target) || target;
            return "BNE(" + name_register(op.ra) + ", " + label + ", " + name_register(op.rc) + ")";
        }
    });

    betaop({
        opcode: 0x24,
        name: 'CMPEQ',
        alufn: '=',
        exec: function CMPEQ(a, b, c) {
            this.realWriteRegister(c, this.realReadRegister(a) == this.realReadRegister(b));
        }
    });

    betaop({
        opcode: 0x34,
        name: 'CMPEQC',
        has_literal: true,
        alufn: '=',
        exec: function CMPEQC(a, literal, c) {
            this.realWriteRegister(c, this.realReadRegister(a) == literal);
        }
    });

    betaop({
        opcode: 0x26,
        name: 'CMPLE',
        alufn: '<=',
        exec: function CMPLE(a, b, c) {
            this.realWriteRegister(c, this.realReadRegister(a) <= this.realReadRegister(b));
        }
    });

    betaop({
        opcode:  0x36,
        name: 'CMPLEC',
        alufn: '<=',
        has_literal: true,
        exec: function CMPLEC(a, literal, c) {
            this.realWriteRegister(c, this.realReadRegister(a) <= literal);
        }
    });

    betaop({
        opcode: 0x25,
        name: 'CMPLT',
        alufn: '<',
        exec: function CMPLT(a, b, c) {
            this.realWriteRegister(c, this.realReadRegister(a) < this.realReadRegister(b));
        }
    });

    betaop({
        opcode: 0x35,
        name: 'CMPLTC',
        alufn: '<',
        has_literal: true,
        exec: function CMPLTC(a, literal, c) {
            this.realWriteRegister(c, this.realReadRegister(a) < literal);
        }
    });

    betaop({
        opcode: 0x23,
        name: 'DIV',
        alufn: '/',
        exec: function DIV(a, b, c) {
            if(!this.isOptionSet('div')) return this.handleIllegalInstruction();
            if(this.readRegister(b) === 0) {
                throw new BSim.Beta.RuntimeError("Division of " + this.readRegister(a) + " by zero");
            }
            this.realWriteRegister(c, (this.realReadRegister(a) / this.realReadRegister(b))|0);
        }
    });

    betaop({
        opcode: 0x33,
        name: 'DIVC',
        alufn: '/',
        has_literal: true,
        exec: function DIVC(a, literal, c) {
            if(!this.isOptionSet('div')) return this.handleIllegalInstruction();
            if(literal === 0) {
                throw new BSim.Beta.RuntimeError("Division of " + this.readRegister(a) + " by zero");
            }
            this.realWriteRegister(c, (this.realReadRegister(a) / literal)|0);
        }
    });

    betaop({
        opcode: 0x1B,
        name: 'JMP',
        paths: {
            werf: 1,
            wdsel: 0,
            wr: 0,
            pcsel: 2,
            wasel: 0
        },
        exec: function JMP(a, b, c) {
            this.realWriteRegister(c, this.getPC());
            this.setPC(this.realReadRegister(a));
        },
        disassemble: function(op) {
            if(op.rc == 31) return "JMP(" + name_register(op.ra) + ")";
            else return "JMP(" + name_register(op.ra) + ", " + name_register(op.rc) + ")";
        }
    });

    betaop({
        opcode: 0x18,
        name: 'LD',
        paths: {
            alufn: '+',
            werf: 1,
            bsel: 1,
            wdsel: 2,
            wr: 0,
            pcsel: 0,
            asel: 0,
            wasel: 0
        },
        has_literal: true,
        exec: function LD(a, literal, c) {
            this.realWriteRegister(c, this.readWord(this.realReadRegister(a) + literal, true));
        }
    });

    betaop({
        opcode: 0x1F,
        name: 'LDR',
        paths: {
            alufn: 'A',
            werf: 1,
            wdsel: 2,
            wr: 0,
            pcsel: 0,
            asel: 1,
            wasel: 0
        },
        has_literal: true,
        exec: function LDR(a, literal, c) {
            this.realWriteRegister(c, this.readWord(this.getPC() + 4*literal, true));
        },
        disassemble: function(op, pc) {
            var target = op.literal*4 + pc;
            var label = this.getLabel(target) || target;
            return "LDR(" + label + ", " + name_register(op.rc) + ")";
        }
    });

    betaop({
        opcode: 0x22,
        name: 'MUL',
        alufn: '*',
        exec: function MUL(a, b, c) {
            if(!this.isOptionSet('mul')) return this.handleIllegalInstruction();
            this.realWriteRegister(c, this.realReadRegister(a) * this.realReadRegister(b));
        }
    });

    betaop({
        opcode: 0x32,
        name: 'MULC',
        alufn: '*',
        has_literal: true,
        exec: function MULC(a, literal, c) {
            if(!this.isOptionSet('mul')) return this.handleIllegalInstruction();
            this.realWriteRegister(c, this.realReadRegister(a) * literal);
        }
    });

    betaop({
        opcode: 0x29,
        name: 'OR',
        alufn: '|',
        exec: function OR(a, b, c) {
            this.realWriteRegister(c, this.realReadRegister(a) | this.realReadRegister(b));
        }
    });

    betaop({
        opcode: 0x39,
        name: 'ORC',
        alufn: '|',
        has_literal: true,
        exec: function ORC(a, literal, c) {
            this.realWriteRegister(c, this.realReadRegister(a) | literal);
        }
    });

    betaop({
        opcode: 0x2C,
        name: 'SHL',
        alufn: '<<',
        exec: function SHL(a, b, c) {
            this.realWriteRegister(c, this.realReadRegister(a) << this.realReadRegister(b));
        }
    });

    betaop({
        opcode: 0x3C,
        name: 'SHLC',
        alufn: '<<',
        has_literal: true,
        exec: function SHLC(a, literal, c) {
            this.realWriteRegister(c, this.realReadRegister(a) << literal);
        }
    });

    betaop({
        opcode: 0x2D,
        name: 'SHR',
        alufn: '>>>',
        exec: function SHR(a, b, c) {
            this.realWriteRegister(c, this.realReadRegister(a) >>> this.realReadRegister(b));
        }
    });

    betaop({
        opcode: 0x3D,
        name: 'SHRC',
        alufn: '>>>',
        has_literal: true,
        exec: function SHRC(a, literal, c) {
            this.realWriteRegister(c, this.realReadRegister(a) >>> literal);
        }
    });

    betaop({
        opcode: 0x2E,
        name: 'SRA',
        alufn: '>>',
        exec: function SRA(a, b, c) {
            this.realWriteRegister(c, this.realReadRegister(a) >> this.realReadRegister(b));
        }
    });

    betaop({
        opcode: 0x3E,
        name: 'SRAC',
        alufn: '>>',
        has_literal: true,
        exec: function SRAC(a, literal, c) {
            this.realWriteRegister(c, this.realReadRegister(a) >> literal);
        }
    });

    betaop({
        opcode: 0x21,
        name: 'SUB',
        alufn: '-',
        exec: function SUB(a, b, c) {
            this.realWriteRegister(c, this.realReadRegister(a) - this.realReadRegister(b));
        }
    });

    betaop({
        opcode: 0x31,
        name: 'SUBC',
        alufn: '-',
        has_literal: true,
        exec: function SUBC(a, literal, c) {
            this.realWriteRegister(c, this.realReadRegister(a) - literal);
        }
    });

    betaop({
        opcode: 0x19,
        name: 'ST',
        paths: {
            alufn: '+',
            werf: 0,
            bsel: 1,
            wr: 1,
            ra2sel: 1,
            pcsel: 0,
            asel: 0
        },
        has_literal: true,
        exec: function ST(a, literal, c) {
            this.writeWord(this.realReadRegister(a) + literal, this.realReadRegister(c), true);
        },
        disassemble: function(decoded) {
                return "ST(" + name_register(decoded.rc) + ", " + decoded.literal + ", " + name_register(decoded.ra) + ")";
        }
    });

    betaop({
        opcode: 0x2A,
        name: 'XOR',
        alufn: '^',
        exec: function XOR(a, b, c) {
            this.realWriteRegister(c, this.realReadRegister(a) ^ this.realReadRegister(b));
        }
    });

    betaop({
        opcode: 0x3A,
        name: 'XORC',
        alufn: '^',
        has_literal: true,
        exec: function XORC(a, literal, c) {
            this.realWriteRegister(c, this.realReadRegister(a) ^ literal);
        }
    });

    betaop({
        opcode: 0x2B,
        name: 'XNOR',
        alufn: '~^',
        exec: function XNOR(a, b, c) {
            this.realWriteRegister(c, ~(this.realReadRegister(a) ^ this.realReadRegister(b)));
        }
    });

    betaop({
        opcode: 0x3B,
        name: 'XNORC',
        alufn: '~^',
        has_literal: true,
        exec: function XNORC(a, literal, c) {
            this.realWriteRegister(c, ~(this.realReadRegister(a) ^ literal));
        }
    });

    // Privileged instructions
    betaop({
        opcode: 0x00,
        name: 'PRIV_OP',
        has_literal: true,
        privileged: true,
        exec: function PRIV_OP(a, literal, c) {
            switch(literal) {
                case 0: // HALT
                    return false;
                case 1: // RDCHAR
                    if(!this.isOptionSet('tty')) return this.handleIllegalInstruction();
                    if(this.mKeyboardInput === null) {
                        this.setPC(this.getPC() - 4); // loop
                    } else {
                        if(this.mKeyboardInput == 13) this.mKeyboardInput = 10; // Use the expected newline.
                        this.realWriteRegister(0, this.mKeyboardInput);
                        this.mKeyboardInput = null;
                    }
                    break;
                case 2: // WRCHAR
                    if(!this.isOptionSet('tty')) return this.handleIllegalInstruction();
                    var chr = String.fromCharCode(this.realReadRegister(a));
                    this.ttyOut(chr);
                    break;
                case 3: // CYCLE
                    this.realWriteRegister(0, this.getCycleCount());
                    break;
                case 4: // TIME
                    this.realWriteRegister(0, Date.now());
                    break;
                case 5: // MOUSE
                    if(!this.isOptionSet('tty')) return this.handleIllegalInstruction();
                    this.realWriteRegister(0, this.mMouseCoords);
                    this.mMouseCoords = -1;
                    break;
                case 6: // RANDOM
                    this.realWriteRegister(c, _.random(0xFFFFFFFF));
                    break;
                case 7: // SEED
                    throw new BSim.Beta.RuntimeError("SEED() is unimplemented. To implement, you must provide your own RNG (Math.random is unseedable)");
                case 8: // SERVER
                    this.mServerInfo.push(this.realReadRegister(0));
                    break;
                default:
                    return this.handleIllegalInstruction();
            }
        },
        disassemble: function(op) {
            var ops = ["HALT()", "RDCHAR()", "WRCHAR()", "CYCLE()", "TIME()", "CLICK()", "RANDOM()", "SEED()", "SERVER()"];
            if(op.literal >= 0 && op.literal < ops.length) return ops[op.literal];
            else return "illop";
        }
    });
})();
BSim.Beta.Memory = function(mBeta) {
    var self = this;
    var mMemory = new Uint32Array(0);
    var mMemoryFlags = new Uint8Array(0);
    var mOriginalMemory = new Uint32Array(0);

    var LRU = 0;
    var FIFO = 1;
    var RANDOM = 2;
    var CYCLE = 3;

    // cache parameters
    var cache = false;      // is cache on?
    var totalWords = 64;     // total number of words in the cache
    var lineSize = 1;       // number of words/line (must be 2**N)
    var totalLines = 64;     // totalWords/lineSize
    var nWays = 1;              // number of lines/set
    var replacementStrategy = LRU;    // how to choose replacement line on miss
    var writeBack = true;          // use write back instead of write thru?
    var rWay;               // select which subcache will get replacement
    var readCycleCount = 10;     // latency for main memory read
    var writeCycleCount = 10;    // latency for main memory write

    var nLines;             // number of lines in each subcache
    var lineShift;          // shift/mask info to retrieve line #
    var lineMask;
    var tagShift;           // shift/mask info to retrieve tag
    var tagMask;
    
    // cache state -- one entry for each cache line
    var dirty = new Uint8Array(0);      // dirty bit for each line
    var valid = new Uint8Array(0);      // valid bit for each line
    var tag = new Uint32Array(0);       // tag for each line
    var age = new Uint32Array(0);       // pseudo-time since last use

    // cache statistics
    var cycles = 0;
    var fetchHits = 0;
    var fetchMisses = 0;
    var readHits = 0;
    var readMisses = 0;
    var writeHits = 0;
    var writeMisses = 0;
    var dirtyReplacements = 0;
    var validReplacements = 0;
    var totalReplacements = 0; 
    //var random;

    this.loadBytes = function(bytes) {
        var words = Math.ceil(bytes.length / 4);
        mMemory = new Uint32Array(words);
        mOriginalMemory = new Uint32Array(words);
        mMemoryFlags = new Uint8Array(words);
        for(var i = 0; i < bytes.length; i += 4) {
            mMemory[i/4] = (bytes[i+3] << 24) |
                           (bytes[i+2] << 16) |
                           (bytes[i+1] << 8)  |
                            bytes[i+0];
        }
        mOriginalMemory = new Uint32Array(mMemory);
    };

    // update DOM with cache statistics
    function update_cache_display() {
        if (cache) {
            var fetches = fetchHits + fetchMisses;
            var reads = readHits + readMisses;
            var writes = writeHits + writeMisses;
            var total = reads + writes + fetches;
            var hits = readHits + writeHits + fetchHits;
            var misses = readMisses + writeMisses + fetchMisses;

            $('#fetch-hits').text(fetchHits.toString());
            $('#read-hits').text(readHits.toString());
            $('#write-hits').text(writeHits.toString());
            $('#total-hits').text(hits.toString());

            $('#fetch-misses').text(fetchMisses.toString());
            $('#read-misses').text(readMisses.toString());
            $('#write-misses').text(writeMisses.toString());
            $('#total-misses').text(misses.toString());

            $('#fetch-total').text(fetches.toString());
            $('#read-total').text(reads.toString());
            $('#write-total').text(writes.toString());
            $('#total-total').text(total.toString());

            $('#fetch-ratio').text(fetches ? (fetchHits/fetches).toFixed(3) : ' ');
            $('#read-ratio').text(reads ? (readHits/reads).toFixed(3) : ' ');
            $('#write-ratio').text(writes ? (writeHits/writes).toFixed(3) : ' ');
            $('#total-ratio').text(total ? (hits/total).toFixed(3) : ' ');
        
            $('#total-cycles').text(cycles.toString());
        } else {
            $('.cache-span').text(' ');
        }
    }

    function cache_reset() {
        // cache statistics
        cycles = 0;
        fetchMisses = 0;
        readMisses = 0;
        writeMisses = 0;
        fetchHits = 0;
        readHits = 0;
        writeHits = 0;
        dirtyReplacements = 0;
        validReplacements = 0;
        totalReplacements = 0;
        //random.setSeed(0);              // restart pseudorandom sequence
        rWay = 0;                       // reset replacement pointer

        // cache state
        for (var i = 0; i < dirty.length; i += 1) {
            dirty[i] = 0;
            valid[i] = 0;
            tag[i] = 0;
            age[i] = 0;
        }

        update_cache_display();
    };

    this.reset = function() {
        mMemory = new Uint32Array(mOriginalMemory);
        cache_reset();
    };
    
    this.contents = function() {
        return mMemory;
    };

    function log2(n) {
        var log = 0;
        var v = 1;

        while (log < 32) {
            if (v >= n) break;
            v <<= 1;
            log += 1;
        }
        return log;
    }
            
    function mask(n) {
        var log = log2(n);
        if (log == 32) return 0xFFFFFFFF;
        else return (1 << log) - 1;
    }

    function process_cache_parameters() {
        if (cache) {
            dirty = new Uint8Array(totalLines);  // boolean
            valid = new Uint8Array(totalLines);  // boolean
            tag = new Uint32Array(totalLines);
            age = new Uint32Array(totalLines);

            nLines = totalLines / nWays;
            lineShift = log2(lineSize)+2;
            lineMask = mask(nLines);
            tagShift = lineShift + log2(nLines);
            tagMask = (1 << (32 - tagShift)) - 1;

            var ntagbits = 32 - tagShift;
            var nbits = 32*lineSize + ntagbits + 1 + (writeBack ? 1 : 0);
            var cost_sram = (nLines == 1) ? totalLines*nbits*50 :   // registers
                            totalLines*nbits*6 +        // ram bits
                            (tagShift - lineShift)*20 + // address buffers
                            nLines*20 +                 // address decode for each row
                            nbits*nWays*30;             // sense amp + output drivers
            var ncomparators = nWays*(32 - tagShift);
            var cost_comparators = ncomparators * 20;
            var nmuxes = nWays*32*(lineSize - 1);       // tree of 2:1 32-bit muxes
            var cost_muxes = nmuxes * 8;

            // update display
            var txt = '[31:'+tagShift.toString()+']';
            $('#tag-bits').text(txt);
            if (nLines > 1) txt = '['+(tagShift-1).toString()+':'+lineShift.toString()+']';
            else txt = 'N/A';
            $('#index-bits').text(txt);
            txt = '['+(lineShift-1).toString()+':0]';
            $('#offset-bits').text(txt);
            txt = nWays > 1 ? nWays.toString()+'x(' : '';
            txt += nLines.toString()+'x'+nbits.toString();
            if (nWays > 1) txt += ')';
            $('#mem-size').text(txt);
            txt = ncomparators.toString();
            $('#comparator-bits').text(txt);
            txt = nmuxes.toString();
            $('#mux-bits').text(txt);
            txt = cost_sram + cost_comparators + cost_muxes;
            $('#total-cost').text(txt);

            $('#total-words').prop('disabled',false);
            $('#line-size').prop('disabled',false);
            $('#associativity').prop('disabled',false);
            $('#replacement-strategy').prop('disabled',nWays == 1);
            $('#write-strategy').prop('disabled',false);
        } else {
            $('#total-words').prop('disabled',true);
            $('#line-size').prop('disabled',true);
            $('#associativity').prop('disabled',true);
            $('#replacement-strategy').prop('disabled',true);
            $('#write-strategy').prop('disabled',true);
        }

        cache_reset();
    };

    // choose replacement line according to current strategy
    function replace(aline,atag,makeDirty) {
        if (nWays > 1) {
            switch (replacementStrategy) {
            case LRU:
            case FIFO:
                {   var oldest = age[aline];
                    var index = aline + nLines;
                    rWay = 0;
                    for (var way = 1; way < nWays; way += 1) {
                        if (age[index] < oldest) {
                            rWay = way;
                            oldest = age[index];
                        }
                        index += nLines;
                    }
                }
                break;
            case RANDOM:
                //todo rWay = random.nextInt(nWays);
                break;
            case CYCLE:
                rWay = (rWay + 1) % nWays;
                break;
            }
        }

        // fill in correct line in chosen subcache
        aline += rWay * nLines;

        // update statistics
        totalReplacements += 1;
        if (valid[aline]) {
            validReplacements += 1;
            // writeback line if dirty
            if (dirty[aline]) {
                dirty[aline] = 0;
                dirtyReplacements += 1;
                cycles += writeCycleCount + lineSize - 1;
            }
        }

        // refill line with new data
        valid[aline] = 1;
        dirty[aline] = makeDirty ? 1 : 0;
        tag[aline] = atag;
        cycles += readCycleCount + lineSize - 1;
        age[aline] = cycles;
    }

    this.readWord = function(address) {
        var addr = address >> 2;
        if(addr < 0 || addr >= mMemory.length) {
            throw new BSim.Beta.RuntimeError("Attempted to read out of bounds address 0x" + BSim.Common.FormatWord(address));
        }

        return mMemory[addr];
    };

    this.readWordCached = function(address,fetch) {
        var v = this.readWord(address);

        if (cache) {
            cycles += 1;   // cache lookup takes one cycle

            // check the appropriate line of each subcache
            var aline = (address >> lineShift) & lineMask;
            var atag = (address >> tagShift) & tagMask;
            var index = aline;
            for (var way = 0; way < nWays; way += 1) {
                if (valid[index] && tag[index] == atag) {
                    // hit!
                    if (fetch) fetchHits += 1
                    else readHits += 1;
                    if (replacementStrategy == LRU) age[index] = cycles;
                    return v;
                }
                index += nLines;
            }

            // miss -- select replacement and refill
            replace(aline,atag,false);
        } else cycles += readCycleCount;

        if (fetch) fetchMisses += 1;
        else readMisses += 1;
        return v;
    };

    this.writeWord = function(address, value) {
        value |= 0; // force to int.
        var addr = address >> 2;
        if (addr < 0 || addr >= mMemory.length) {
            throw new BSim.Beta.RuntimeError("Attempted to write out of bounds address 0x" + BSim.Common.FormatWord(address));
        }
        if (mMemoryFlags[addr]) {
            throw new BSim.Beta.RuntimeError("Attempted write to protected memory at 0x" + BSim.Common.FormatWord(address));
        }

        mMemory[addr] = value;
    };

    this.writeWordCached = function(address, value) {
        this.writeWord(address,value);

        if (cache) {
            cycles += 1;   // cache lookup takes one cycle

            // check the appropriate line of each subcache
            var aline = (address >> lineShift) & lineMask;
            var atag = (address >> tagShift) & tagMask;
            var index = aline;
            for (var way = 0; way < nWays; way += 1) {
                if (valid[index] && tag[index] == atag) {
                    // hit!
                    writeHits += 1;
                    if (writeBack) dirty[index] = 1;
                    else cycles += writeCycleCount;
                    if (replacementStrategy == LRU) age[index] = cycles;
                    return;
                }
                index += nLines;
            }

            // miss -- select replacement and refill
            replace(aline,atag,writeBack);

            // write-through cache also write word to memory
            if (!writeBack) cycles += writeCycleCount;
        } else cycles += writeCycleCount;

        writeMisses += 1;
    };

    this.size = function() {
        return mMemory.length * 4;
    };

    this.setProtectedRegions = function(regions) {
        _.each(regions, function(region) {
            var start_word = region.start / 4;
            var end_word = region.end / 4;
            for(var i = start_word; i < end_word && i < mMemoryFlags.length; ++i) {
                mMemoryFlags[i] = true;
            }
        });
    };

    this.isProtected = function(address) {
        return !!mMemoryFlags[address >> 2];
    };

    // set up change event handlers for cache controls
    $('#cache-status').on('change',function (e) {
        cache = $(this).val() == 'on';
        process_cache_parameters();
    });
    $('#total-words').on('change',function (e) {
        totalWords = parseInt($(this).val());

        // lineSize <= totalWords
        lineSize = Math.min(totalWords,lineSize);
        $('select#line-size option').each(function () {
            var size = parseInt($(this).text());
            $(this).prop('disabled',size > totalWords);
            $(this).prop('selected',size == lineSize);
        });
        totalLines = totalWords/lineSize;

        // nWays <= totalLines
        if (nWays > totalLines) nWays = 1;
        $('select#associativity option').each(function () {
            var nw;
            switch ($(this).text()) {
            case 'direct mapped': nw = 1; break;
            case '2-way': nw = 2; break;
            case '4-way': nw = 4; break;
            case '8-way': nw = 8; break;
            case 'fully associative': nw = totalLines; break;
            }
            $(this).prop('disabled',nw > totalLines);
            $(this).prop('selected',nw == nWays);
        });

        if ($('#associativity').val() == 'fully associative') nWays = totalLines;

        process_cache_parameters();
    });
    $('#line-size').on('change',function (e) {
        lineSize = parseInt($(this).val());
        totalLines = totalWords/lineSize;

        // nWays <= totalLines
        if (nWays > totalLines) nWays = 1;
        $('select#associativity option').each(function () {
            var nw;
            switch ($(this).text()) {
            case 'direct mapped': nw = 1; break;
            case '2-way': nw = 2; break;
            case '4-way': nw = 4; break;
            case '8-way': nw = 8; break;
            case 'fully associative': nw = totalLines; break;
            }
            $(this).prop('disabled',nw > totalLines);
            $(this).prop('selected',nw == nWays);
        });

        if ($('#associativity').val() == 'fully associative') nWays = totalLines;

        process_cache_parameters();
    });
    $('#associativity').on('change',function (e) {
        switch ($(this).val()) {
        case 'direct mapped': nWays = 1; break;
        case '2-way': nWays = 2 ; break;
        case '4-way': nWays = 4 ; break;
        case '8-way': nWays = 8; break;
        case 'fully associative': nWays = totalLines; break;
        }
        process_cache_parameters();
    });
    $('#replacement-strategy').on('change',function (e) {
        switch ($(this).val()) {
        case 'LRU': replacementStrategy = LRU; break;
        case 'FIFO': replacementStrategy = FIFO; break;
        case 'Random': replacementStrategy = RANDOM; break;
        case 'Cycle': replacementStrategy = Cycle; break;
        }
        process_cache_parameters();
    });
    $('#write-strategy').on('change',function (e) {
        switch ($(this).val()) {
        case 'write-through': writeBack = false; break;
        case 'write-back': writeBack = true; break;
        }
        process_cache_parameters();
    });

    process_cache_parameters();  // initially use default values

    mBeta.on('read:register', update_cache_display);  // update cache stats 
};
// Provides a scrollable table view that doesn't try to render the entire data set in the DOM.
// container: DOM element to insert the table in
// width: table width in pixels
// height: table height in pixels
// row_height: height of each row in pixels
// column_count: number of columns in pixels.
// Note that height must be an exact multiple of row_height, and will be adjusted to the closest
// value if it isn't.
//
// The basic idea is that it creates a minimal table to fill the width, and overlays an element
// that has the height of the whole table. As that element scrolls, the table's contents are adjusted
// to match the expected content of the viewport
var BigTable = function(container, width, height, row_height, column_count) {
    var mContainer = $(container);
    var mRowHeight = row_height;
    var mDisplayRowCount = Math.floor(height / row_height);
    var mWidth = width;
    var mHeight = mDisplayRowCount * mRowHeight;
    var mColumnCount = column_count;
    var mEmptyData = [];

    var mVisibleStart = 0;
    var mData = [];
    var mTableRows = [];

    var mScroller = null;
    var mScrollContent = null;
    var mContent = null;

    var mBufferingDraws = 0;

    // Inserts a row of data at the end
    // data: array of cells for the row. Must have length equal to the table's column count.
    this.insertRow = function(data) {
        data.cls = '';
        mData.push(data);
        mScrollContent.css({height: mRowHeight * mData.length});
        redraw(mData.length - 1);
    };

    // Updates the given row with the given data. The row must exist.
    // data's length must be equal to the table's column count.
    this.updateRow = function(row, data) {
        if(!mData[row]) {
            return;
        }
        mData[row] = data;
        redraw(row);
    };

    // Update's the given column of the given row with the given data, which should be a string.
    this.updateCell = function(row, column, data) {
        if(!mData[row]) {
            return;
        }
        mData[row][column] = data;
        redraw(row);
    };

    // Empties and blanks the table
    this.empty = function() {
        mData = [];
        mVisibleStart = 0;
        redraw();
    };

    // Adds a class the given row. If the optional parameter redraw is true, redraws the row immediately.
    this.addRowClass = function(row, cls) {
        if(!mData[row]) return;
        if(!~mData[row].cls.indexOf(' ' + cls + ' ')) {
            mData[row].cls += ' ' + cls + ' ';
            redraw(row);
        }
    };

    // Removes a class from the given row. If the optional parameter redraw is true, redraws the row immediately.
    this.removeRowClass = function(row, cls) {
        if(!mData[row]) return;
        mData[row].cls = mData[row].cls.replace(' ' + cls + ' ', '');
        redraw(row);
    };

    // Attempts to centre the given row in the display.
    this.scrollTo = function(row, where) {
        if(!where) where = 'middle';
        var height = (row * mRowHeight);
        if(where == 'middle') height -= (mHeight / 2) - (mRowHeight);
        else if(where == 'bottom') height -= mHeight - mRowHeight;
        mScroller[0].scrollTop = height;
        handleScroll(height); // Manually do this to make sure we are locked on where we intend to be.
    };

    // Returns the number of (logical) rows in the table.
    this.rowCount = function() {
        return mData.length;
    };

    this.startBatch = function() {
        mBufferingDraws++;
    };

    this.endBatch = function() {
        mBufferingDraws--;
        if(!mBufferingDraws)
            redraw();
    };

    this.resize = function(height) {
        mDisplayRowCount = Math.floor(height / mRowHeight);
        mHeight = mDisplayRowCount * mRowHeight;
        mContent.css({height: mHeight});
        mScroller.css({height: mHeight});
        create_rows();
        redraw();
    };

    var create_rows = function() {
        // Set up our display rows.
        mContent.empty();
        mTableRows = [];
        for(var i = 0; i < mDisplayRowCount; ++i) {
            var row = $('<tr>').css({height: mRowHeight});
            var row_cells = [];
            for(var j = 0; j < mColumnCount; ++j) {
                var cell = $('<td>');
                row.append(cell);
                row_cells.push(cell[0]);
            }
            row_cells.row = row[0];
            mTableRows.push(row_cells);
            mContent.append(row);
        }
    };

    var redraw = function(row) {
        if(mBufferingDraws) return;
        // If we weren't given an argument, redraw everything visible.
        if(row === undefined) {
            for(var i = mVisibleStart; i < mVisibleStart + mDisplayRowCount; ++i) {
                redraw(i);
            }
            return;
        } else {
            if(row >= mVisibleStart && row < mVisibleStart + mDisplayRowCount) {
                var display_row = row - mVisibleStart;
                var data = mData[row] || mEmptyData;
                for (var j = data.length - 1; j >= 0; j--) {
                    mTableRows[display_row][j].textContent = data[j];
                }
                mTableRows[display_row].row.className = data.cls;
            }
            return;
        }
    };

    var handleScroll = function(height) {
        var top = (_.isNumber(height)) ? height : mScroller[0].scrollTop;
        if(top < 0) top = 0; // This can probably actually happen.
        var top_row = (top / mRowHeight)|0;
        // Don't do anything if we haven't actually moved.
        if(top_row != mVisibleStart) {
            mVisibleStart = top_row;
            redraw();
        }
    };

    var initialise = function() {
        mScroller = $('<div>').css({width: mWidth, height: mHeight, position: 'absolute', top: 0, left: 0, 'overflow-y': 'scroll'});
        mContent = $('<table>').css({width: mWidth, height: mHeight, position: 'absolute', top: 0, left: 0});
        mScrollContent = $('<div>');

        mContainer.css({position: 'relative'}).append(mContent, mScroller);
        mScroller.append(mScrollContent);
        mScroller.scroll(handleScroll);

        // Set up our handy mEmptyData
        for(var k = 0; k < mColumnCount; ++k) {
            mEmptyData.push('');
        }
        mEmptyData.cls = '';

        create_rows();
    };

    initialise();
};
BSim.RegfileView = function(container, beta) {
    // Build a table. Yes; I'm a terrible person.
    var mContainer = $(container);
    var mTable = $('<table class="regfile">');
    var mRegisterValueCells = new Array(32);
    var mBeta = beta;

    var update_register = function(register, value) {
        mRegisterValueCells[register].text(BSim.Common.FormatWord(value));
    };

    var bulk_update_registers = function(registers) {
        for(var register in registers) {
            update_register(register, registers[register]);
        }
    };

    var read_register = function(registers) {
        mTable.find('td').removeClass('last-read');
        _.each(registers, function(register) {
            mRegisterValueCells[register].addClass('last-read').prev('td').addClass('last-read');
        });
    };

    var write_register = function(registers) {
        mTable.find('td').removeClass('last-write');
        _.each(registers, function(register) {
            mRegisterValueCells[register].addClass('last-write').prev('td').addClass('last-write');
        });
    };

    var initialise = function() {
        // Build up our table of registers.
        for(var i = 0; i < 8; ++i) {
            var tr = $('<tr>');
            for(var j = 0; j < 4; ++j) {
                tr.append($('<td class="register">').text(BSim.Common.RegisterName((i + j*8))));
                var cell = $('<td class="value">').text(BSim.Common.FormatWord(0));
                mRegisterValueCells[i+j*8] = cell;
                tr.append(cell);
            }
            mTable.append(tr);
        }
        mContainer.append(mTable);

        // Set up callbacks to update appropriate parts of the UI (or all of it)
        mBeta.on('change:register', update_register);
        mBeta.on('change:bulk:register', bulk_update_registers);
        mBeta.on('read:register', read_register);
        mBeta.on('write:register', write_register);
    };

    initialise();
};
BSim.TTY = function(container, beta) {
    var mContainer = $(container);
    var mBeta = beta;
    var mPendingText = '';
    var mTextHolder = $('<pre class="tty-output" tabindex="1">');
    var mJustFocused = false;
    var mHasFocus = false;

    var initialise = function() {
        mContainer.append(mTextHolder);

        var text_holder = mTextHolder[0];

        var append_text = function() {
            text_holder.textContent += mPendingText;
            mPendingText = '';
            text_holder.scrollTop = text_holder.scrollHeight;
        };

        // Appending text a character at a time stutters if we have lots of text.
        // This instead batches the text up for 50ms intervals.
        var append_text_slowly = _.throttle(append_text, 50);

        var handle_new_text = function(text) {
            mPendingText += text;
            append_text_slowly();
        };

        var clear_text = function() {
            mPendingText = '';
            text_holder.textContent = '';
        };

        var replace_text = function(text) {
            mPendingText = '';
            text_holder.textContent = text;
        };

        mBeta.on('text:out', handle_new_text);
        mBeta.on('text:clear', clear_text);
        mBeta.on('text:replace', replace_text);

        mContainer.keypress(function(e) {
            beta.keyboardInterrupt(e.which);
        });

        mTextHolder.mousedown(function(e) {
            //console.log('click: '+mHasFocus.toString()+' '+mJustFocused.toString());
            if(!mHasFocus || mJustFocused) return; // Ignore clicks just as we're focused.
            var offset = mTextHolder.offset();
            var x = e.pageX - offset.left;
            var y = e.pageY - offset.top;
            if(x < 0) x = 0;
            if(y < 0) y = 0; // This is not impossible.
            beta.mouseInterrupt(x, y);
        });

        mTextHolder.focus(function(e) {
            //console.log('focus: '+mHasFocus.toString()+' '+mJustFocused.toString());
            // Ignore any click events that come in the immediate future.
            mJustFocused = true;
            mHasFocus = true;
            setTimeout(function() {
                mJustFocused = false;
            }, 100); // this timer is a hack, but I'm not sure we can do better given the order of events (focus then click)
        });

        mTextHolder.blur(function(e) {
            mHasFocus = false;
        });
    };

    initialise();
};
