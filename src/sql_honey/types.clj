(ns sql-honey.types
  (:refer-clojure :exclude [cast])
  (:require [potemkin :as p]
            [pretty.core :as pretty]))

(p/defrecord+ FnCall [function args]
  pretty/PrettyPrintable
  (pretty [_]
    (list* (pretty/qualify-symbol-for-*ns* `fn-call) function args)))

(defn fn-call [function & args]
  (->FnCall function (vec args)))

(p/defrecord+ Identifier [parts]
  pretty/PrettyPrintable
  (pretty [_]
    (list* (pretty/qualify-symbol-for-*ns* `identifier) parts)))

(defn identifier [& parts]
  (->Identifier (vec parts)))

(p/defrecord+ Literal [s]
  pretty/PrettyPrintable
  (pretty [_]
    (list (pretty/qualify-symbol-for-*ns* `literal) s)))

(defn literal [s]
  (->Literal s))

(p/defrecord+ Cast [x typename]
  pretty/PrettyPrintable
  (pretty [_]
    (list (pretty/qualify-symbol-for-*ns* `cast) x typename)))

(defn cast [x typename]
  (->Cast x typename))

;; (p/defrecord+ Quoted [x]
;;   pretty/PrettyPrintable
;;   (pretty [_]
;;     (list (pretty/qualify-symbol-for-*ns* `quoted) x)))

;; (defn quoted [x]
;;   (->Quoted x))
