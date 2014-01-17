(ns ^{:author "Janos Erdos"}
  erdos.match
  "Small library for generating pattern matching code.")

(comment
  TODO
  map matching, set matching <?>
  map matching for pojo/beans
  vector matching for array access
  all object - type tag
  all object - guard fns
  lisp code simplification work in progress
)

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
  (letfn [(-sym [pat rsn]
            (cond
             (= '_ pat)                 []
             (= \? (-> pat name first))
             (concat (if-let [t (-> pat meta :tag)]
                       [[:guard `(instance? ~t ~rsn)]])
                     [[:?= (with-meta pat {}) rsn]])
             :otherwise [[:guard `(= '~pat ~rsn)]]))
          (-const [pat rsn]     [[:guard `(= ~pat ~rsn)]])
          (-vec-itm [rsn l2 i k]
            (if-not (= k '_)
              (cons [:=nth l2 rsn i] (-match k l2))))
          (-handle-seq [pat rsn]
            (let [l2 (gensym "v")]
              (if (= '& (last (butlast pat)))
                (do
                  (assert (symbol? (last pat))
                          "Vararg name must be a symbol.")
                  (conj
                   (mapcat (partial -vec-itm rsn l2)
                           (range), (-> pat butlast butlast))
                   [:?= (last pat) `(nthrest ~rsn ~(-> pat count dec dec))]
                   [:guard `(>= (count ~rsn) ~(-> pat count dec dec))]))
                (conj
                 (mapcat (partial -vec-itm rsn l2)
                         (range), (seq pat))
                 [:guard `(= (count ~rsn) ~(count pat))]))))
          (-vec [pat rsn]
            (conj (-handle-seq pat rsn)
                  [:guard `(vector? ~rsn)]))
          (-list [pat rsn]
            (conj (-handle-seq pat rsn)
                  [:guard `(seq? ~rsn)]))
          (-match [pat rsn]
            (cond
             (symbol? pat)  (-sym pat rsn)
             (number? pat)  (-const pat rsn)
             (char? pat)    (-const pat rsn)
             (keyword? pat) (-const pat rsn)
             (vector? pat)  (-vec pat rsn)
             (string? pat)  (-const pat rsn)
             (list? pat)    (-list pat rsn)
             ;(set? pat)
             ;(map? pat)
             ))]
    (-match pat root-sym)))

;; process generated op code.
(defmulti ^:private opcode (fn [op & _] (first op)))
(defmethod opcode :=
  [[_ n v] & body]     `(let [~n ~v] ~@body))
(defmethod opcode :==
  ([[_ n v] then]      `(if (= ~n ~v) ~then))
  ([[_ n v] then else] `(if (= ~n ~v) ~then ~else)))
(defmethod opcode :=nth
  [[_ n v i] & body]   `(let [~n (nth ~v ~i)] ~@body))
(defmethod opcode :guard
  [[_ e] then]         `(if ~e ~then))

(defn- compile-series
  "Compile ser list of opcodes to clj code."
  [ser body]
   (reduce
    (fn [acc op]
      (opcode op acc))
    body, (reverse-series ser)))

(defn merge-sexp
  "Join expressions by merging similiar outer if-else branches."
  [a b]
  (or
   (if (empty? a) b)
   (if (empty? b) a)

   (when (and (= 'if (first a) (first b))
              (= (second a) (second b)))
     (assert (nil? (get a 3)))
     (assert (nil? (get a 3)))
     `(if ~(second a) ~(merge-sexp (nth a 2) (nth b 2))))

   (when (= 'clojure.core/or (first a) (first b))
     `(or ~@(concat (rest a) (rest b))))

   (when (= 'clojure.core/or (first a))
     `(or ~@(rest a) ~b))
   (when (= 'clojure.core/or (first b))
     `(or ~a ~@(rest b)))

   `(or ~a ~b)))

(defn- match0*
  "Returns the generated code for the clauses"
  [value & clauses]
  (assert (-> clauses count even?))
  (let [vsym (gensym "MC")
        cls  (partition 2 clauses) ;; [pattern action]*
        cls  (map (fn [[p c]] [(make-series p vsym) c]) cls) ;; [pcode action]*
        cls  (map (fn [[p c]]  (compile-series p [c])) cls)
        ]
    `(first
      (let [~vsym ~value]
        ~(reduce merge-sexp cls)))))

(defmacro match0-
  "debug: macro form of match*"
  [value & clauses]
  `'~(apply match0* value clauses))

(defmacro match0 [value & clauses]
  (apply match0* value clauses))

(defn simplify-sexp
  "Simplify clojure code, eg.:
   merges if statements to case,
   nested or exprs to simple one,
   nested let's to simple one"
  [sexp]
  (clojure.walk/postwalk
   (fn [x]
     (match0 x
             ;; (or (or a b) (or c d)) -> (or a b c d)
             (or & ?ops)
             `(or ~@(mapcat #(match0 %, (clojure.core/or & ?xs) ?xs,
                                     (or & ?xs) ?xs, ?x [?x]) ?ops))

             (if (= ?a ?x1) ?a1 (if (= ?a ?x2) ?a2 ?a3))
             `(case ~?a, ~?x1 ~?a1, ~?x2 ~?a2, ~?a3)

             (if (= ?a ?x1) ?a1 (clojure.core/case ?a & ?as))
             `(case ~?a, ~?x1 ~?a1, ~@?as)

             (clojure.core/let [?k ?v]
               (clojure.core/let [& ?as] ?body))
             `(let [~?k ~?v ~@?as] ~?body)

             ?else ?else))
   sexp))

(defn match*
  "Produce clj code for pattern matching"
  [expr & clauses]
  (simplify-sexp (apply match0* expr clauses)))

(defmacro match-
  "Macro form of match* fn"
  [expr & clauses]
  `'~(apply match* expr clauses))

(defmacro match
  "Use this macro for patterns matching."
  [expr & clauses]
  (apply match* expr clauses))

:OK
