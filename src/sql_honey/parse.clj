(ns sql-honey.parse
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [instaparse.core :as instaparse]
            [sql-honey.types :as types]))

(def grammar
  (delay (slurp (io/resource "sql_honey/grammar.bnf"))))

(def parser
  (delay (instaparse/parser @grammar)))

(defmulti transform
  {:arglists '([form])}
  first)

(defmethod transform :default
  [form]
  form)

(defmethod transform :function-call
  [[_ f & args]]
  (apply types/fn-call f args))

(defmethod transform :cast
  [[_ [_ x typename]]]
  (types/cast x typename))

(defmethod transform :add      [[_ x y]] [:+ x y])
(defmethod transform :subtract [[_ x y]] [:- x y])
(defmethod transform :divide   [[_ x y]] [:/ x y])
(defmethod transform :multiply [[_ x y]] [:* x y])

;; (defmethod transform :unquoted-unqualified-indentifier
;;   [[_ s]]
;;   (str/lower-case s))

;; (defmethod transform :quoted-unqualified-identifier
;;   [[_ s]]
;;   (quoted s))

(defmethod transform :identifier
  [[_ & parts]]
  (apply types/identifier parts))

(defmethod transform :integer
  [[_ s]]
  (Long/parseLong s))

(defmethod transform :string-literal
  [[_ s]]
  (types/literal s))

(defmethod transform :null
  [_]
  nil)

;; (remove-method transform :select-as)

(defmethod transform :select
  [[_ & args]]
  {:select (vec args)})

(defmethod transform :subselect-query
  [[_ & args]]
  (reduce merge args))

(defmethod transform :subselect
  [[_ query identifier]]
  [query identifier])

;; (defmethod transform :from-as
;;   [[_ table alias]]
;;   [table alias])

(defmethod transform :from
  [[_ & args]]
  {:from (vec args)})

;; (defmethod transform :join-source
;;   [[_ what alias]]
;;   (if alias
;;     [what alias]
;;     what))

(defmethod transform :join
  [[_ [join-type] what condition]]
  {join-type [[what condition]]})

(defmethod transform :joins
  [[_ & joins]]
  (reduce (partial merge-with concat) {} joins))

(defmethod transform :group-by
  [[_ & identifiers]]
  {:group-by (vec identifiers)})

(defmethod transform :order-by-subclause
  [[_ identifier [direction]]]
  (if direction
    [identifier direction]
    [identifier :asc]))

(defmethod transform :order-by
  [[_ & subclauses]]
  {:order-by (vec subclauses)})

(defmethod transform :equals
  [[_ x y]]
  [:= x y])

(defmethod transform :greater-than
  [[_ x y]]
  [:> x y])

(defmethod transform :less-than
  [[_ x y]]
  [:< x y])

(defmethod transform :greater-than-or-equal
  [[_ x y]]
  [:>= x y])

(defmethod transform :less-than-or-equal
  [[_ x y]]
  [:<= x y])

(defmethod transform :where
  [[_ tree]]
  {:where tree})

(defmethod transform :query
  [[_ & args]]
  (reduce merge args))

#_(defmethod transform :insert-into
  [[_ table & cols]]
  {:insert-into table, :columns cols})

;; (defmethod transform :row
;;   [[_ & args]]
;;   (vec args))

;; (defmethod transform :values
;;   [[_ & args]]
;;   {:values args})

(defn transform-all [x]
  (walk/postwalk
   (fn [form]
     (if (and (vector? form) (keyword? (first form)))
       (transform form)
       form))
   x))

(defn parse [s]
  (let [parsed (@parser s)]
    (when (instance? instaparse.gll.Failure parsed)
      (let [error-column (:column parsed)
            error-at     (str/join (take 20 (drop error-column (:text parsed))))]
        (throw (ex-info (format "Error parsing SQL at %s" (pr-str error-at))
                        (assoc parsed :error-at error-at)))))
    (transform-all parsed)))
