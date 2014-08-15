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

(def ^:private vec1 [1 2 3 4 5])

(deftest test-match-vec
  (are [-x] (match vec1 -x true)
       [1 2 3 4 5]
       [1 2 3 4 ?a] [1 2 ?a 4 5] [1 2 ?a 4 ?b]
       [1 2 3 4 _]   [1 2 _ 4 5] [_ _ _ _ _]
       [1 2 3 4 5 & _] [1 2 3 4 5 & ?a]
       [_ _ ?a ?b & ?cs]))

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

(defmatcher vagy)

(addmatch vagy _ false)
(addmatch vagy [true _] true)
(addmatch vagy [_ true] true)

(comment
  (match "asd" "a" 1 "asd2" 2 _ 3)

  )

(deftest test-seq-match
  (testing "basic"
    (is (match ()
               nil false,
               (1) false, (1 2) false
               () true, :f false, _ :OK)  )
    (is (match '(1)
               () false, (2) false, (1 2) false, (1) true, _ false)))

  (testing "variable seq length"
    (is (match '(1 2 3)
               (1) false
               (1 ?a) false
               (1 ?a 3 ?b) false
               (1 ?a ?b ?c) false
               (1 2 ?3) true))
    (is (match '(1 2 3)
               (1) false
               (1 & ?xs) true))))

(def seq1 '(1 2 3 4 5))

(deftest test-seq-varlen
  (are [-x] (match seq1 -x true)
       (1 2 3 4 5)
       (1 2 3 4 5 & ?xs)
       (1 2 3 4 5 & _)))

(deftest test-seq-optional
  (are [-x] (match seq1 -x true)
       (_ _ _ _ _)
       (?a ?b ?c ?d ?e)
       (?a ?b _ _ _)
       (1 2 3 4 ?a)
       (1 2 3 4 & ?a)
       (1 2 3 4 & _)
       (1 2 3 & ?bs)
       (1 2 3 ^{:when string?} ?a 4 5)
       (1 2 3 ^{:when number?} ?f 5)
       (1 2 ^{:when #{3}} _ 4 5)
       (1 2 ^{:when #{3}} ?a ^{:when #{3}} ?b 4 5)
       (1 2 ^{:when #{3}} _  ^{:when #{3}} _  4 5)))

(deftest test-seq-optional-not
  (are [-x] (match seq1 -x false _ true)
       (1 2 ^{:when #{3}} _ ^{:when #{4}} _ 4 5)))

(clojure.test/run-tests 'erdos.match-test)
