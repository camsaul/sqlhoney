(ns sql-honey.print
  (:require [clojure.string :as str]
            [sql-honey.parse :as parse]
            [sql-honey.types :as types])
  (:import [sql_honey.types Cast FnCall Identifier Literal]))

(comment types/keep-me)

(def ^:dynamic *indent* 0)

(defn indent-amount [increase]
  {:pre [((some-fn string? integer?) increase)]}
  (if (string? increase)
                                   (count increase)
                                   increase))

(defmacro with-indent [increase & body]
  `(binding [*indent* (+ *indent* (indent-amount ~increase))]
     ~@body))

(defn print-indent []
  (print (str/join (repeat *indent* " "))))

(def ^:dynamic *width* 120)

(defn print-newline-indent []
  (print "\n")
  (print-indent))

(defn dispatch-on-keyword-or-class [x]
  (if (and (vector? x)
           (keyword? (first x)))
    (first x)
    (class x)))

(defmulti print-compact
  {:arglists '([x])}
  dispatch-on-keyword-or-class)

;; TODO -- better name?
(defmulti print-spacious
  {:arglists '([x])}
  dispatch-on-keyword-or-class)

(defn acceptable-compact-representation? [s]
  (and (= (count (str/split-lines s)) 1)
       (< (+ *indent* (count s))
          *width*)))

(defn pretty-print [x]
  (try
    (let [dv (dispatch-on-keyword-or-class x)]
      (cond
        (not (get-method print-compact dv))
        (print-spacious x)

        (not (get-method print-spacious dv))
        (print-compact x)

        :else
        (let [s (with-out-str
                  (print-compact x))]
          (if (acceptable-compact-representation? s)
            (print s)
            (print-spacious x)))))
    (catch Throwable e
      (throw (ex-info (format "Error pretty printing %s: %s" (pr-str x) (ex-message e))
                      {:x x}
                      e)))))

(defmethod print-compact clojure.lang.Keyword
  [x]
  (print (name x)))

(defmethod print-compact Identifier
  [{:keys [parts]}]
  (print (str/join "." parts)))

(defmethod print-compact Literal
  [{:keys [s]}]
  (print "'")
  (print s)
  (print "'"))

(defmethod print-compact FnCall
  [{:keys [function args]}]
  (pretty-print function)
  (print "(")
  (when (seq args)
    (pretty-print (vec args)))
  (print ")"))

(defmethod print-compact Cast
  [{:keys [x typename]}]
  (print "cast(")
  (pretty-print x)
  (print " AS ")
  (pretty-print typename)
  (print ")"))

(defmethod print-compact clojure.lang.Fn
  [f]
  (f))

(defmethod print-compact clojure.lang.PersistentVector
  [[x & more]]
  (pretty-print x)
  (doseq [arg more]
    (print ", ")
    (pretty-print arg)))

(defmethod print-compact :default
  [x]
  (if (vector? x)
    (let [[first-arg & more] x]
      (print (pr-str first-arg))
      (doseq [arg more]
        (print " ")
        (pretty-print arg)))
    (print (pr-str x))))

(defmethod print-compact :as
  [[_ x y]]
  (pretty-print x)
  (print " AS ")
  (pretty-print y))

(defmethod print-compact :star
  [_]
  (print "*"))

(defmethod print-compact :jdbc-placeholder
  [_]
  (print "?"))

(defmethod print-compact :aliased-from
  [[_ source alias]]
  (pretty-print source)
  (print " ")
  (pretty-print alias))

(defmacro print-indented-children {:style/indent 1} [s & body]
  `(let [s# ~s]
     (print s#)
     (with-indent s#
       ~@body)))

(defn print-compact-operator [operator-str x y]
  (let [s (with-out-str
            (pretty-print x)
            (printf " %s " operator-str))]
    (print-indented-children s
      (pretty-print y))))

(defn print-spacious-operator [operator-str x y]
  (pretty-print x)
  (print-newline-indent)
  (print operator-str)
  (print-newline-indent)
  (pretty-print y))

(doseq [operator [:= :> :< :>= :<= :+ :- :* :/]]
  (defmethod print-compact operator
    [[_ x y]]
    (print-compact-operator (name operator) x y))

  (defmethod print-spacious operator
    [[_ x y]]
    (print-spacious-operator (name operator) x y)))

(defmethod print-compact :between
  [[_ x y z]]
  (pretty-print x)
  (print " BETWEEN ")
  (pretty-print y)
  (print " AND ")
  (pretty-print z))

(defmethod print-compact nil
  [_]
  (print "NULL"))

(defmethod print-compact :case
  [[_ & exprs]]
  (let [has-else?       (odd? (count exprs))
        [if-thens else] (if has-else?
                          [(butlast exprs) (last exprs)]
                          [exprs])]
    (println "if-thens:" if-thens)      ; NOCOMMIT
    (print-indented-children "CASE"
      (doseq [[condition expr] (partition 2 if-thens)]
        (print-indented-children " WHEN "
          (pretty-print condition))
        (print-indented-children " THEN "
          (pretty-print expr)))
      (when has-else?
        (print-indented-children " ELSE "
          (pretty-print else))))
    (print " END")))

(defmethod print-spacious :case
  [[_ & exprs]]
  (let [has-else?              (odd? (count exprs))
        [if-thens else]        (if has-else?
                                 [(butlast exprs) (last exprs)]
                                 [exprs])
        [first-if-then & more] (partition 2 if-thens)]
    (print-indented-children "CASE "
      (letfn [(print-if-then [[condition expr]]
                (print-indented-children "WHEN "
                  (pretty-print condition))
                (print-newline-indent)
                (print-indented-children "THEN "
                  (pretty-print expr)))]
        (print-if-then first-if-then)
        (doseq [if-then more]
          (print-newline-indent)
          (print-if-then more)))
      (when has-else?
        (print-newline-indent)
        (print-indented-children "ELSE "
          (pretty-print else))))
    (print-newline-indent)
    (print "END")))

(defmethod print-spacious :between
  [[_ x y z]]
  (pretty-print x)
  (print-newline-indent)
  (print "BETWEEN")
  (print-newline-indent)
  (pretty-print y)
  (print-newline-indent)
  (print "AND")
  (print-newline-indent)
  (pretty-print z))

(declare pretty-print-parsed)

(defn print-subquery [m]
  (print "(")
  (with-indent 2
    (pretty-print-parsed m))
  (print-newline-indent)
  (print ")"))

(defn subselect? [x]
  (and (map? x)
       (not (record? x))))

(defmethod print-compact :aliased-from
  [[_ from identifier]]
  (if (subselect? from)
    (print-subquery from)
    (pretty-print from))
  (print " ")
  (pretty-print identifier))

(defn print-compact-compound-condition
  [condition-str [first-condition & more]]
  (print "(")
  (with-indent 1
    (pretty-print first-condition)
    (doseq [condition more]
      (print-newline-indent)
      (print condition-str)
      (print-newline-indent)
      (pretty-print condition)))
  (print ")"))

(defmethod print-compact :and
  [[_ & conditions]]
  (print-compact-compound-condition "AND" conditions))

(defmethod print-compact :or
  [[_ & conditions]]
  (print-compact-compound-condition "OR" conditions))

(defmulti pretty-print-top-level
  {:arglist '([k v])}
  (fn [k _]
    k))

(defn print-indented-clause [clause [x & more]]
  (print-newline-indent)
  (print clause)
  (print " ")
  (with-indent (str clause " ")
    (pretty-print x)
    (doseq [arg more]
      (print ",")
      (print-newline-indent)
      (pretty-print arg))))

(defmethod pretty-print-top-level :default
  [k v]
  (if (sequential? v)
    (print-indented-clause (name k) v)
    (do
      (print-newline-indent)
      (pretty-print k)
      (print " ")
      (pretty-print v))))

(defmethod pretty-print-top-level :select
  [_ args]
  (print-indented-clause "SELECT" args))

(defmethod pretty-print-top-level :from
  [_ [first-arg & more]]
  (print-newline-indent)
  (print "FROM ")
  (with-indent 2
    (pretty-print first-arg)
    (doseq [arg more]
      (print ", ")
      (print-newline-indent)
      (pretty-print arg))))

(defmethod pretty-print-top-level :where
  [_ condition]
  (print-indented-clause "WHERE" [condition]))

(defmethod pretty-print-top-level :left-join
  [_ clauses]
  (doseq [[what condition] clauses]
    (print-newline-indent)
    (print "LEFT JOIN ")
    (with-indent 2
      (pretty-print what))
    (with-indent 7
      (print-newline-indent)
      (print "ON ")
      (pretty-print condition))))

(defmethod pretty-print-top-level :group-by
  [_ args]
  (print-indented-clause "GROUP BY" args))

(defmethod pretty-print-top-level :order-by
  [_ args]
  (print-indented-clause "ORDER BY" (for [[what direction] args]
                                      (fn []
                                        (pretty-print what)
                                        (print " ")
                                        (print (case direction
                                                 :asc  "ASC"
                                                 :desc "DESC"))))))

(defmethod pretty-print-top-level :limit
  [_ v]
  (print-newline-indent)
  (print "LIMIT ")
  (pretty-print v))

(defn pretty-print-parsed [parsed]
  {:pre [(map? parsed)]}
  (doseq [k [:select :from :left-join :inner-join :right-join :full-join :group-by :where :order-by :limit]]
    (when-let [v (get parsed k)]
      (pretty-print-top-level k v))))

(defn pretty-print-sql [s]
  (pretty-print-parsed (parse/parse s))
  (println))
