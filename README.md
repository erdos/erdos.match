# erdos.match

A simple clojure library for pattern matching.

## Usage

Copy the match.clj file to your src/erdos folder.
Include the following in your code:
```clojure
(use '[erdos.match :only (match)])
```

## Examples

### simple value matching

When matching for simple values, the pattern will be compiled for a (case) expression.
```clojure
(match a
   1       :one,
   2       :two,
   "three" :three,
   ?a      (str "value: " ?a))
```

### vectors

It is possible to match vectors and lists by length.
```clojure
(match a
   []      0,
   [_]     1,
   [_ _]   2,
   [_ _ _] 3,
   [& _]   :many)
```

Or combining the previous two:
```clojure
(match a
   [:t _] :t,
   [_ :t] :t,
   [_ _]  :f)
```

You can also refer to previously matched values.
```clojure
(match a
   [?a ?a] (str ?a "=" ?a)
   [?a ?b] (str ?a "/=" ?b))
```

### for analyzing clojure code
Checking for symbols and lists is possible. Please note the different syntax for lists and vectors.
```clojure
(match a
   (if ?cond ?then ?else) (str "if-expression")
   (when ?cond & ?then)   (str "when-expr")
   _                      :unexpected)
```


## License

Copyright © 2014 Janos Erdos
