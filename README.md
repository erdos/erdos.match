# erdos.match

A simple clojure library for pattern matching.

Patterns will be simplified and compiled to clojure code.

## Usage

Copy the match.clj file to your src/erdos folder.
Include the following in your code:
```clojure
(use '[erdos.match :only (match)])
```

## Syntax

Usage of the (match) macro. The first argument is the value to be matched. The following arguments are patterns and bodies. The body for the first matching pattern will be executed.

The _ symbol can be used to match any object. Symbols starting with `?` sign will be used as names for capturing objects. (They can also be used for type matching, see examples.) The first occurence of the symbol is used for binding and all other occurences are for equality checking. Thus, you can check for repeating parts in your pattern.

All other symbols will be handled as concrete objects for matching.

The `[]` notation is for matching vectors and `()` is for matching lists. They are not interchangeable. The `&` sign is used for matching the remaining part of the sequence.

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

### Type matching

```clojure
(match "what is this?"
       ^Integer ?a :int
       ^String  ?a :string
       ^Long    ?a :long
       _ :unknown)
```
Cool eeh?

## License

Copyright © 2014 Janos Erdos
