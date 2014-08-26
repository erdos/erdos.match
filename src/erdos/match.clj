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

;; opcode constructors

(defn- guard-pred [f & args]
  `[:guard (~f ~@args)])

(defn- guard= [& ops]
  (apply guard-pred '= ops))

(defn- let?= [sym val]
  (assert (symbol? sym))
  [:?= sym val])

(defn- let= [sym val]
  (assert (symbol? sym))
  [:= sym val])

(def -match nil)
(defmulti  -match (fn [pat rsn] (type pat)))

;; scalar types
(doseq [t [String Number Boolean
           nil clojure.lang.Keyword Character]]
  (defmethod -match t [pat rsn] [(guard= pat rsn)]))

(def symbol-prefix-guards
  {"str" string?
   "sym" symbol?
   "kwd" keyword?
   "int" integer?
   "num" number?
   "flo" float?
   "cha" char?
   "vec" vector?
   "map" map?
   "set" set?
   "seq" seq?})

;; symbol
(defmethod -match
  clojure.lang.Symbol [pat rsn]
  (cond
   (= '_ pat)
   ,  []
   (= \? (-> pat str first))
   ,  (let [g0 (if-let [t (-> pat meta :tag)]
                 (guard-pred 'instance? t rsn))
            g1 (if-let [g (-> pat meta :guard)]
                 (guard-pred g rsn))]
        (if-let [ns (.getNamespace ^clojure.lang.Symbol pat)]
          (if-let [g2 (get symbol-prefix-guards (subs ns 1))]
            (if (= "_" (name pat))
              [(guard-pred g2 rsn) g0 g1]
              [(guard-pred g2 rsn) g0 g1
               (let?= (symbol (str "?" (name pat))) rsn)])
            (->> ns
                 (str "No guard for symbol prefix: ")
                 (new IllegalArgumentException)
                 (throw)))
       [g0 g1 (let?= pat rsn)]))
   :otherwise
   ,  [(guard= `'~pat rsn)]))

;; map
(defmethod -match
  clojure.lang.IPersistentMap [pat rsn]
  (let [l2 (gensym "m")]
    (cons (guard-pred 'map? rsn)
          (mapcat
           (fn [[k v]]
             (list*
              (guard-pred 'contains? rsn k)
              (let= l2 `(get ~rsn ~k))
              (-match v l2)))
           (seq pat)))))

;; lists.
(defn handle-vec-itm [rsn l2 i k]
  (if-not (= k '_)
    (list* (let= l2 `(nth ~rsn ~i))
           (-match k l2))))


(defn handle-vec [pat rsn]
  (let [l2 (gensym "v")]
    (if (= '& (last (butlast pat)))
      (do
        (assert (symbol? (last pat))
                "Vararg name must be a symbol.")
        (conj
         (mapcat (partial handle-vec-itm rsn l2)
                 (range), (-> pat butlast butlast))
         (let?= (last pat) `(nthrest ~rsn ~(-> pat count dec dec)))
         (guard-pred '>= `(count ~rsn) (-> pat count dec dec))))
      (conj
       (mapcat (partial handle-vec-itm rsn l2)
               (range), (seq pat))
       (guard= `(count ~rsn) (count pat))))))


(defn handle-seq-itm
  [tmp seq-sym itm-sym p]
  (concat
   [(guard-pred 'seq seq-sym)
    (let= itm-sym `(first ~seq-sym))]
   (if (and (symbol? p)
            (-> p meta :when))
     [(let= tmp `( ~(-> p meta :when) ~itm-sym))
      (if (not= p '_)
        (let?= p `(if ~tmp ~itm-sym)))
      (let= seq-sym `(if ~tmp (next ~seq-sym) ~seq-sym))]
     (concat
      (-match p itm-sym)
      [(let=  seq-sym `(next ~seq-sym))]))))


(defn handle-seq
  "Creates code for sequential traversal."
  [pat rsn]
  (let [seq-sym (gensym "seq")
        ende?   (= '& (last (butlast pat)))
        ps      (mapcat (partial handle-seq-itm
                                 (gensym "tmp")
                                 seq-sym
                                 (gensym "itm"))
                        (if ende? (butlast (butlast pat)) pat))]
    (list*
     (let= seq-sym `(seq ~rsn))
     (if ende?
       (concat ps [(let?= (last pat) seq-sym)])
       (concat ps [(guard-pred 'nil? seq-sym)])))))


(defmethod -match
  clojure.lang.ISeq
  [pat rsn]
  (cond
   (= (first pat) 'clojure.core/unquote)
   ,,,[(guard= (-> pat second eval) rsn)]
   (= (first pat) 'clojure.core/deref)
   ,,,[(guard= (second pat) rsn)]
   :else
   ,,,(list*
       (guard-pred 'seq? rsn)
       (handle-seq pat rsn))))


;; vector
(defmethod -match
  clojure.lang.IPersistentVector
  [pat rsn]
  (list*
   (guard-pred 'vector? rsn)
   (handle-vec pat rsn)))

;; (-match '(?a ?b ?c d) 'jano)

(defn- make-series
  "Analyze pattern object and produce a list of opcodes."
  [pat root-sym]
  (letfn []
    (-match pat root-sym)))

;; process generated op code.
(defmulti ^:private opcode (fn [op & _] (first op)))
(defmethod opcode :=
  [[_ n v] & body]     `(let [~n ~v] ~@body))
(defmethod opcode :==
  ([[_ n v] then]      `(if (= ~n ~v) ~then))
  ([[_ n v] then else] `(if (= ~n ~v) ~then ~else)))
(defmethod opcode :guard
  ([[_ e] then]         `(if ~e ~then))
  ([[_ e] then else]    `(if ~e ~then ~else)))
(defmethod opcode nil
  ([_ then] then))


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


(defmacro defmatcher [name]
  `(def ~name
     (with-meta (constantly nil)
       {:doc "Automatically generated matcher fn."
        :matches ()})))


(defmacro addmatch
  [name clause & body]
  (let [body (if (next body)
               `(do ~@body) (first body))]
    `(let [m# (-> ~name meta
                  :matches
                  (conj '~body '~clause))
           f# (list 'fn '[x#]
                    (concat '(match x#) m#))]
       (alter-var-root
        (var ~name)
        (constantly
         (with-meta
           (eval f#)
           (assoc (meta ~name)
             :matches m#)))))))
:OK
