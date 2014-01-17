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
   1 :one,
   2 :two,
   "three" :three)
```

It is possible to match vectors and lists by length.
```clojure
(match a
   []      0
   [_]     1
   [_ _]   2
   [_ _ _] 3
   [& _]   :many)
```

Or combining the previous two:
```
(match a
   [:t _] :t
   [_ :t] :t
   [_ _]  :f)
```

## License

Copyright © 2014 Janos Erdos
