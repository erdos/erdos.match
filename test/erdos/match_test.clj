(ns erdos.match-test
  ""
  (:use clojure.test
        erdos.match))

(deftest test-match
  (testing "simple numeric vals"
    (is (match 6, 6 true, _ false)
        (match 7, 1 false, 2 false, 6 true, 8 false, _ false)))
  (testing "symbols"
    (is (match 'a, a true, _ false)
        (match 'b, a false, b true, _ false)))
  (testing "logical and"
    (are [x y] (= y (match x, [:t :t] :t, [_ _] :f))
         [:t :t] :t, [:t :f] :f, [:f :f] :f, [:f :t] :f))
  (testing "tuple vectors"
    (are [x y] (= y (match x, [] :unit, [_] :single, [_ _] :double, [_ _ _] :triple))
         [] :unit, [1] :single, [1 2] :double, [1 2 3] :triple))
  (testing "deep vectors"
    (are [x y] (= y (match x, [] 1, [[]] 2, [[[]]] 3))
         [] 1, [[]] 2, [[[]]] 3))
  (testing "is-vector?"
    (are [x y] (= y (match x, [& _] :t, _ :f))
         [] :t, [1] :t, [1 2] :t, nil :f, "" :f, :asd :f))
  (testing "equality in vector"
    (are [x y] (= y (match x,
                           [[?a ?b] [?a ?b]] :both-match
                           [[?a _] [?a _]]   :first-match
                           [[_ ?b] [_ ?b]]   :second-match
                           _                 :no-match))
         [[1 2] [1 2]] :both-match,
         [[1 2] [1 3]] :first-match,
         [[2 1] [3 1]] :second-match,
         [[1 2] [3 4]] :no-match)))

(deftest test-siplify-sexp
  (testing "merge if's")
  (testing "do not change constats"
    (are [x] (= (simplify-sexp x) x)
         'asdf, nil, :asd, [], {}, "asdf"))
  (testing "simplify (or) forms"
    (are [x y] (= (simplify-sexp y) (simplify-sexp x))
         '(or 1 2 3)          '(or 1 2 3)
         '(or 1 (or 2 3))     '(or 1 2 3)
         '(or (or 1) (or 2))  '(or 1 2)
         '(or (or 1 2) 3)     '(or 1 2 3)))

  (testing "simplify (let) forms"
    (are [x y] (= (simplify-sexp x) (simplify-sexp y))
        `(let [a 1] :x) `(let [a 1] :x)
        `(let [a 1] (let [b 2] :x)) `(let [a 1 b 2] :x)
        `(let [a 1] (let [a 2] :x)) `(let [a 1 a 2] :x)
        `(let [a 1] (let [a (inc a)] :w)) `(let [a 1 a (inc a)] :w)
        ))
  (testing "simplify (if) forms to (cond)"))

(deftest test-match-type
  (testing "basic type matching"
    (are [r x] (= x (match r ^Long ?a :long, ^String ?a :string))
         "Dolorem" :string,
         12        :long,
         'sdf       nil)))

(deftest test-match-map
  (testing "basic match"
    (are [x y] (= x (match y {:a ?a} (str "a:" ?a), {:b ?b} (str "b:" ?b), _ :f))
         "b:1" {:b 1}
         "a:2" {:a 2}
         :f   {:c 3})))

(comment
   (match-pattern* "asd"
           ^Integer ?a :int
           ^String ?a :string
           ^Long ?a :long)

   (match {:a 1 :b 2 :c [1 2]}
          {:a ?a :b ?b :c [1 ?c]} [?a ?b ?c]
          _ :else)

  :OK)
