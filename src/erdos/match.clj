(ns ^{:author "Janos Erdos"}
  erdos.match
  "Small library for generating pattern matching code.")


(defn- reverse-series
  "Reverse list of opcodes and change :?= to := or :=="
  [series]
  (:res
   (reduce
    (fn [acc [code & args]]
      (if (= code :?=)
        (if (contains? (:syms acc) (first args))
          (assoc acc :res (cons `[:== ~@args]  (:res acc)))
          (assoc acc,
            :res (cons `[:= ~@args] (:res acc))
            :syms (conj (:syms acc) (first args))))
        (assoc acc :res (cons `[~code ~@args] (:res acc)))))
    {:res nil, :syms #{}} series)))


(defn- make-series
  "Analyze pattern object and produce a list of opcodes."
  [pat root-sym]
  (letfn [(-sym     [pat rsn]
            (cond
             (= '_ pat)         []
             (= \? (-> pat name first))
             (concat (if-let [t (-> pat meta :tag)]
                       [[:guard `(instance? ~t ~rsn)]])
                     (if-let [g (-> pat meta :guard)]
                       [[:guard `(~g ~rsn)]])
                     [[:?= (with-meta pat {}) rsn]])
             :otherwise [[:guard `(= '~pat ~rsn)]]))
          (-const   [pat rsn]   `([:guard (= ~pat ~rsn)]))
          (-seq-itm [rsn l2 i k]
            (if-not (= k '_) `([:= ~l2 (nth ~rsn ~i)] ~@(-match k l2))))
          (-handle-seq [pat rsn]
            (let [l2 (gensym "v")]
              (if (= '& (last (butlast pat)))
                (do
                  (assert (symbol? (last pat))
                          "Vararg name must be a symbol.")
                  (conj
                   (mapcat (partial -seq-itm rsn l2)
                           (range), (-> pat butlast butlast))
                   [:?= (last pat) `(nthrest ~rsn ~(-> pat count dec dec))]
                   [:guard `(>= (count ~rsn) ~(-> pat count dec dec))]))
                (conj
                 (mapcat (partial -seq-itm rsn l2)
                         (range), (seq pat))
                 [:guard `(= (count ~rsn) ~(count pat))]))))
          (-map [pat rsn]
            (let [l2 (gensym "m")]
                (cons [:guard `(map? ~rsn)]
                 (mapcat
                  (fn [[k v]] `[[:guard (contains? ~rsn ~k)]
                               [:= ~l2 (get ~rsn ~k)]
                               ~@(-match v l2)])
                  (seq pat)))))
          (-vec [pat rsn]
            `([:guard (vector? ~rsn)] ~@(-handle-seq pat rsn)))
          (-list [pat rsn]
            `([:guard (seq? ~rsn)] ~@(-handle-seq pat rsn)))
          (-deref [pat rsn]
            `([:== ~pat ~rsn]))
          (-match [pat rsn]
            (cond
             (vector? pat)  (-vec pat rsn)
             (keyword? pat) (-const pat rsn)
             (map? pat)     (-map pat rsn)
             (symbol? pat)  (-sym pat rsn)
             (number? pat)  (-const pat rsn)
             (char? pat)    (-const pat rsn)
             (string? pat)  (-const pat rsn)
             (and (seq? pat)
                  (= (first pat) 'clojure.core/unquote))
             (-const (-> pat second eval) rsn)
             (and (seq? pat)
                  (= (first pat) 'clojure.core/deref))
             (-deref (second pat) rsn)
             (seq? pat)    (-list pat rsn)
             :otherwise     (-> "Unexpected pattern: " (str pat (list? pat))
                                IllegalArgumentException. throw)))]
    (-match pat root-sym)))

;; process generated op code.
(defmulti ^:private opcode (fn [op & _] (first op)))
(defmethod opcode :=
  [[_ n v] & body]     `(let [~n ~v] ~@body))
(defmethod opcode :==
  ([[_ n v] then]      `(if (= ~n ~v) ~then))
  ([[_ n v] then else] `(if (= ~n ~v) ~then ~else)))
(defmethod opcode :guard
  [[_ e] then]         `(if ~e ~then))


(defn- compile-series
  "Compile ser list of opcodes to clj code."
  [ser body]
   (reduce (fn [acc op] (opcode op acc))
           body, (reverse-series ser)))


(defn- merge-sexp
  "Join expressions by merging similiar outer if-else branches."
  [a b]
  (or
   (if (empty? a) b)
   (if (empty? b) a)

   (when (and (= 'if (first a) (first b))
              (= (second a) (second b)))
     (assert (nil? (get a 3)))
     (assert (nil? (get b 3)))
     `(if ~(second a) ~(merge-sexp (nth a 2) (nth b 2))))

   (when (= 'clojure.core/or (first a) (first b))
     `(or ~@(concat (rest a) (rest b))))

   (when (= 'clojure.core/or (first a))
     `(or ~@(rest a) ~b))

   (when (= 'clojure.core/or (first b))
     `(or ~a ~@(rest b)))

   `(or ~a ~b)))


(defn- match0-pattern
  "Returns the generated code for the clauses"
  [value & clauses]
  (assert (-> clauses count even?))
  (let [vsym (gensym "MC")
        cls  (partition 2 clauses) ;; [pattern action]*
        cls  (map (fn [[p c]] [(make-series p vsym) c]) cls) ;; [pcode action]*
        cls  (map (fn [[p c]] (compile-series p [c])) cls)]
    `(first
      (let [~vsym ~value]
        ~(reduce merge-sexp cls)))))


(defmacro match0 [value & clauses]
  (apply match0-pattern value clauses))


(defn- simplify-sexp-item
  "Simplify a sexp, eg.: merge (if) forms, etc."
  [sexp]
  (match0 sexp
          (or & ?ops)
          `(or ~@(mapcat
                  #(match0 %,
                           (clojure.core/or & ?xs) ?xs,
                           (or & ?xs) ?xs, ?x [?x]) ?ops))

          (if (= ?a ?x1) ?a1 (if (= ?a ?x2) ?a2 ?a3))
          `(case ~?a, ~?x1 ~?a1, ~?x2 ~?a2, ~?a3)

          (if (= ?a ?x1) ?a1 (clojure.core/case ?a & ?as))
          `(case ~?a, ~?x1 ~?a1, ~@?as)

          (clojure.core/let [?k ?v]
            (clojure.core/let [& ?as] ?body))
          `(let [~?k ~?v ~@?as] ~?body)

          ?else ?else))


(defn simplify-sexp
  "Recursively simplify sexp."
  [sexp] (clojure.walk/postwalk simplify-sexp-item sexp))


(defn match-pattern
  "Produce clj code for pattern matching"
  [expr & clauses]
  (simplify-sexp
   (apply match0-pattern expr clauses)))


(defmacro match-pattern*
  "Macro form of match-pattern fn"
  [expr & clauses]
  `'~(apply match-pattern expr clauses))


(defmacro match
  "Use this macro for patterns matching."
  [expr & clauses]
  (apply match-pattern expr clauses))

:OK
