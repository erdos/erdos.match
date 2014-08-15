# erdos.match

_A simple clojure library for pattern matching._

*problem:* With most pattern matching libraries, patterns do not look like real clojure data structures. This makes it difficult to write patterns that match to clojure expressions. With this library, your patterns look like clojure code and data structures with minimal modifications.

Patterns will be simplified and compiled to clojure code at compile time. Using this library, it is also possible to create multimethods with patterns as dispatch values.


## Usage

Copy the match.clj file to your src/erdos folder.
Include the following in your code:
```clojure
(require '[erdos.match :refer (match)])
```

## Syntax

The `(match)` macro is used for pattern matching. The first argument is the value to be matched. The following arguments are patterns and bodies. Only the branch for the first matching pattern will be executed. When no pattern is matched, nil value is returned.

**example**

```clojure
(match expression-to-match
  pattern-1   branch-1
  pattern-2   branch-2
  ;...
  pattern-n   branch-n)
```

### Constant values

Numbers, string, symbols and keywords are supported.

**example**

```clojure
(match a
  :first  :keyword
  second  :symbol
  "third" :string
  4       :number)
;; a=4 => :number, a='second => :symbol, etc..
```

### Vectors and lists

The `[]` notation is for matching vectors and `()` is for matching lists and seqs. They are not interchangeable. The `&` sign is used for matching the remaining part of the vector or list.

**example**

```clojure
(match a
  []    :empty-vect
  ()    :empty-list
  [_]   :vec1
  [& _] :vect-many
  (_)   :list-1
  (& _) :list-many)
;; a=[] => :empty-vect, a=(3) => :vec1, etc..
```

### Maps

Values in maps can be matched by keys. For example, pattern `{:a ?a}` matches `{:a 1}` object by binding value 1 to var ?a.

**example**

```clojure
(match a
  {:a ?a, :b ?b} (str "a=" ?a " b=" ?b)
  {:a _}         "only :a is given"
  {:b _}         "only :b is given"
  {}             "neither :a nor :b given"
  _              "it is not even a map.")
```

### Binding values

The `_` symbol can be used to match any object and null values. Symbols starting with `?` sign will be used as var names for capturing objects. (They can also be used for type matching, see examples.) The first occurence of the symbol is used for *binding* and all other occurences are for *equality checking*. Thus, you can check for repeating parts in your pattern.

### Type matching

You can add type matching using the `:tag` meta info on var names. For example, `^Integer ?i` matches an Integer value and binds it to var `?i`.

**example**

```clojure
(match "what is this?"
       ^Integer ?a :int
       ^String  ?a :string
       ^Long    ?a :long
       _ :unknown)
;; => :string
```

### Guard functions

You can also add guard functions using the `:guard` meta map key. For example, the pattern `^{:guard even?} ?i` will match even numbers. Please note, you can not use meta info on the `_` symbol. All other symbols will be handled as concrete objects for matching.


**example**

Match even numbers.

```clojure
(match 24
   ^{:guard even?} ?e :even
   _                  :odd)
;; => :even
```

### Optional matching

For optionally matching for items in lists and sequences, use the `:when` meta key with a function value. The `:guard` meta key will be ignored in this case.

**example**

```clojure
(match '(1 2 3)
   (1 ^{:when string? } ?s 2 3) true)
;; => true

(match '(1 "oneandhalf" 2 3)
   (1 ^{:when string? } ?s 2 3) ?s)
;; => "oneandhalf"

```

### Run-time and compile-time expressions

It is also possible to match for expressions calculated at compile time or at run time. For compile-time matching, use the `~` prefix. For example: `~true` matches for the value true, whereas `true` will match for the symbol 'true. For run-time matching, use the `@` prefix. You can also use the names of already matched var names in this expression.

Do you want to match for expressions computed in compile time?

```clojure
(match 10
    ~(+ 1 2 3)   :first
    ~(+ 1 2 3 4) :second
    _            :unknown)
;; => :second
```

Or the other way: match for expressions computed when matched for. Therefore, you can match for already matched variables.

```clojure
(match [true false]
  [?a @(not ?a)] :not-the-same
  _              :the-same)
;; => :not-the-same
```

### Matcher methods

Matcher methods are very similar to multimethods. First, you declare a matcher function with `defmatcher`. Second, you add match cases to the matcher using `addmatch`.

`(defmatcher m)` declares a new matcher named `m`.

`(addmatch m pat body)` defines a matching case for matcher named `m`.

## More examples

### vectors

Match the vector by the number of elements

```clojure
(match a
   []      0,
   [_]     1,
   [_ _]   2,
   [_ _ _] 3,
   [& _]   :many)
;; when a=[] => 0, a=[5] => 1, a=[4 5] => 2, etc..
```

Logical OR operator

```clojure
(match a
   [~nil ?a]   ?a,
   [~false ?a] ?a,
   [?a _]      ?a)
```

You can also refer to previously matched values.

```clojure
(match a
   [?a ?a] (str ?a "=" ?a)
   [?a ?b] (str ?a "/=" ?b))
;; when a=[1 1] => "1=1", a=[1 2]=>"1/=2", etc..
```

### for analyzing clojure code

Checking for symbols and lists is possible. Please note the different syntax for lists and vectors.
```clojure
(match a
   (if ?cond ?then ?else) (str "if-expression")
   (when ?cond & ?then)   (str "when-expr")
   _                      :unexpected)
```

### Pattern compiling

To check out the compiled pattern, use the `match-pattern` function or the `match-pattern*` macro.

For example, the following code..
```clojure
(match-pattern
   "asd"
   ^Integer ?i (str ?i "is and int")
   ^String ?s (str ?s "is a string")
   ^Long ?l   (str ?l "is a long"))
```
..will compile to something like this:
```clojure
(clojure.core/first
  (clojure.core/let [MC23818 "asd"]
     (clojure.core/or
        (if (clojure.core/instance? Integer MC23818)
           (clojure.core/let [?i MC23818]
              [(str ?i "is and int")]))
        (if (clojure.core/instance? String MC23818)
           (clojure.core/let [?s MC23818]
              [(str ?s "is a string")]))
        (if (clojure.core/instance? Long MC23818)
           (clojure.core/let [?l MC23818]
              [(str ?l "is a long")])))))
```

## License

**The MIT License**

Copyright © 2014 Janos Erdos

_Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:_

_The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software._

_THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE._
