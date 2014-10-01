(ns erdos.match-neue)

(def ^:dynamic *conditionals*)
(def ^:dynamic *matched-symbols* #{})
(def ^:dynamic *root* (gensym "root"))

(defmacro tail []
  nil)


(defmulti match-itm type)

(defmethod match-itm
  clojure.lang.Symbol [s]
  (if (contains? *matched-symbols* s)
    `(if (= ~s ~*root*)
       ~(tail))
    (binding [*matched-symbols*
              (conj *matched-symbols* s)]
      `(let [~s ~*root*]
         ~(tail)))))

(doseq [s '(clojure.lang.Keyword, java.lang.Number, java.lang.Boolean)]
  (defmethod match-itm s [c]
    `(if (= ~c ~*root*)
       ~(tail))))

(defmethod match-itm
  clojure.lang.PersistentVector [v]
;;  - vector? ?
;;  - count = ?
 ;; dde

  )


(defn match-seq [s]
  (let [r *root*
        g (gensym)]
    ))

(defn match-seq [s]
  `(let [~*root* (next ~*root*)]
     (tail (match-itm (first s))
           (match-seq (rest s)))))

(defmethod match-itm
  clojure.lang.ISeq
  [s]
  `(if (seq? ~*root*)
     (tail (match-seq s))))
