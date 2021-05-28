(ns sql-honey.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [sql-honey.core :as sql-honey]
            [sql-honey.print :as print]
            [sql-honey.types :as types]))

(defn- pretty-print [sql]
  (-> (with-out-str
        (sql-honey/pretty-print-sql sql))
      str/trim
      str/split-lines))

(deftest string-literal-test
  (is (= {:select [(types/literal "abc")]}
         (sql-honey/parse "SELECT 'abc'")))
  (is (= ["SELECT 'abc'"]
         (pretty-print "SELECT 'abc'")))
  (testing "escaped"
    (is (= {:select [(types/literal "ab''c")]}
           (sql-honey/parse "SELECT 'ab''c'")))
    (is (= ["SELECT 'ab''c'"]
           (pretty-print "SELECT 'ab''c'")))))

(deftest jdbc-placeholder-test
  (is (= {:select [[:star]]
          :from   [(types/identifier "table")]
          :where  [:= [:jdbc-placeholder] 100]}
         (sql-honey/parse "SELECT * FROM table WHERE ? = 100")))
  (is (= ["SELECT *"
          "FROM table"
          "WHERE ? = 100"]
         (pretty-print "SELECT * FROM table WHERE ? = 100"))))

(deftest function-call-no-args-test
  (is (= {:select [(types/fn-call (types/identifier "now"))]}
         (sql-honey/parse "SELECT now()")))
  (is (= ["SELECT now()"]
         (pretty-print "SELECT now()"))))

(deftest conditions-test
  (doseq [condition [">" "<" "=" ">=" "<="]]
    (testing condition
      (is (= {:select [[:star]]
              :from   [(types/identifier "TABLE")]
              :where  [(keyword condition) (types/identifier "x") 1]}
             (sql-honey/parse (format "SELECT * FROM TABLE WHERE x%s1" condition))))
      (is (= ["SELECT *"
              "FROM TABLE"
              (format "WHERE x %s 1" condition)]
             (pretty-print (format "SELECT * FROM TABLE WHERE x%s1" condition)))))))

(deftest between-test
  (is (= {:select [[:star]]
          :from   [(types/identifier "table")]
          :where  [:between (types/identifier "x") 1 2]}
         (sql-honey/parse "SELECT * FROM table WHERE x BETWEEN 1 AND 2")))
  (is (= ["SELECT *"
          "FROM table"
          "WHERE x BETWEEN 1 AND 2"]
         (pretty-print "SELECT * FROM table WHERE x BETWEEN 1 AND 2")))
  (testing "pretty print when >= print width"
    (binding [print/*width* 20]
      (is (= ["SELECT *"
              "FROM table"
              "WHERE x"
              "      BETWEEN"
              "      1"
              "      AND"
              "      2"]
             (pretty-print "SELECT * FROM table WHERE x BETWEEN 1 AND 2")))))
  ;; FIXME
  #_(is (= ["SELECT *"
            "FROM table"
            "WHERE x BETWEEN 1 AND (1 + 1)"]
           (pretty-print "SELECT * FROM table WHERE x BETWEEN 1 AND (1 + 1)"))))

(deftest compound-condition-test
  (doseq [[k s] {:or "OR"
                 :and "AND"}]
    (testing s
      (is (= {:select [[:star]]
              :from   [(types/identifier "table")]
              :where  [k [:= (types/identifier "x") 1] [:= (types/identifier "y") 2]]}
             (sql-honey/parse (format "SELECT * FROM table WHERE (x = 1 %s y = 2)" s))))
      (is (= ["SELECT *"
              "FROM table"
              "WHERE (x = 1"
              (str "       " s)
              "       y = 2)"]
             (pretty-print (format "SELECT * FROM table WHERE (x = 1 %s y = 2)" s))))))

  (is (= {:select [[:star]]
          :from [(types/identifier "table")]
          :where [:and
                  [:= (types/identifier "x") 1]
                  [:or
                   [:= (types/identifier "y") 2]
                   [:= (types/identifier "z") 3]]]}
         (sql-honey/parse "SELECT * FROM table WHERE (x = 1 AND (y = 2 OR z = 3))")))
  (is (= ["SELECT *"
          "FROM table"
          "WHERE (x = 1"
          "       AND"
          "       (y = 2"
          "        OR"
          "        z = 3))"]
         (pretty-print "SELECT * FROM table WHERE (x = 1 AND (y = 2 OR z = 3))"))))

(deftest group-by-test
  (is (= {:select   [[:star]]
          :from     [(types/identifier "ORDERS")]
          :group-by [(types/fn-call
                      (types/identifier "parsedatetime")
                      (types/fn-call (types/identifier "year") (types/identifier "source" "CREATED_AT"))
                      (types/literal "yyyy"))]}
         (sql-honey/parse "SELECT * FROM ORDERS GROUP BY parsedatetime(year(source.CREATED_AT), 'yyyy')")))
  (is (= ["SELECT *"
          "FROM ORDERS"
          "GROUP BY parsedatetime(year(source.CREATED_AT), 'yyyy')"]
         (pretty-print "SELECT * FROM ORDERS GROUP BY parsedatetime(year(source.CREATED_AT), 'yyyy')")))
  (is (= {:select   [[:star]],
          :from     [(types/identifier "table")]
          :where    [:= (types/identifier "x") 1]
          :group-by [(types/identifier "x")]}
         (sql-honey/parse "SELECT * FROM table WHERE x = 1 GROUP BY x")))
  (is (= ["SELECT *"
          "FROM table"
          "GROUP BY x"
          "WHERE x = 1"]
         (pretty-print "SELECT * FROM table WHERE x = 1 GROUP BY x"))))

(deftest nested-function-calls-test
  (is (= {:select [[:as
                    (types/fn-call
                     (types/identifier "parsedatetime")
                     (types/fn-call (types/identifier "year") (types/identifier "source" "CREATED_AT"))
                     (types/literal "yyyy"))
                    (types/identifier "CREATED_AT")]]}
         (sql-honey/parse "SELECT parsedatetime(year(source.CREATED_AT), 'yyyy') AS CREATED_AT")))
  (is (= {:select [(types/fn-call
                    (types/identifier "parsedatetime")
                    (types/fn-call (types/identifier "year") (types/fn-call (types/identifier "now")))
                    (types/literal "yyyy"))]}
         (sql-honey/parse "SELECT parsedatetime(year(now()), 'yyyy')")))
  (is (= ["SELECT parsedatetime(year(now()), 'yyyy')"]
         (pretty-print "SELECT parsedatetime(year(now()), 'yyyy')"))))

(deftest wrapped-condition-test
  (is (= {:select [[:star]]
          :where  [:= (types/identifier "x") (types/identifier "y")]}
         (sql-honey/parse "SELECT * WHERE (x = y)"))))

(deftest cast-test
  (is (= {:select [(types/cast -2 (types/identifier "long"))]}
         (sql-honey/parse "SELECT CAST(-2 AS long)")))
  #_(is (= {:select [(types/cast -2 (types/identifier "long"))]}
         (sql-honey/parse "SELECT CAST(-2 AS \"long\")")))
  (is (= ["SELECT cast(-2 AS long)"]
         (pretty-print "SELECT CAST(-2 AS long)"))))

(deftest order-by-test
  (is (= {:select [[:star]]
          :from [(types/identifier "table")]
          :order-by [[(types/fn-call
                       (types/identifier "parsedatetime")
                       (types/fn-call (types/identifier "year") (types/identifier "source" "CREATED_AT"))
                       (types/literal "yyyy"))
                      :asc]]}
         (sql-honey/parse "SELECT * FROM table ORDER BY parsedatetime(year(source.CREATED_AT), 'yyyy') ASC")))
  (is (= ["SELECT *"
          "FROM table"
          "ORDER BY parsedatetime(year(source.CREATED_AT), 'yyyy') ASC,"
          "         x DESC,"
          ;; TODO -- should we add `:asc` if it was implied but not explicit? Maybe make this an option.
          "         y ASC"]
         (pretty-print "SELECT * FROM table ORDER BY parsedatetime(year(source.CREATED_AT), 'yyyy') ASC, x DESC, y"))))

(deftest from-test
  ;; TODO -- not sure how to indent this
  (is (= ["SELECT *"
          "FROM ("
          "    SELECT *"
          "    FROM table"
          "  ) source, "
          "  abc def, "
          "  ("
          "    SELECT *"
          "  ) abc, "
          "  def"]
         (pretty-print "SELECT * FROM (SELECT * FROM table) source, abc def, (SELECT *) abc, def"))))

(deftest arithmetic-expression-test
  (is (= {:select [[:+ 1 2]]}
         (sql-honey/parse "SELECT 1 + 2")))
  (is (= {:select [[:+ 1 2]]}
         (sql-honey/parse "SELECT (1 + 2)")))
  (testing "order of operations"
    (is (= {:select [[:*
                      [:+ 1 2]
                      [:/
                       [:+ 3 4]
                       5]]]}
           (sql-honey/parse "SELECT 1 + 2 * 3 + 4 / 5")
           (sql-honey/parse "SELECT (1 + 2) * (3 + 4 / 5)")
           (sql-honey/parse "SELECT (1 + 2) * ((3 + 4) / 5)")))))

(deftest reserved-keywords-test
  (is (thrown?
       clojure.lang.ExceptionInfo
       (sql-honey/parse "SELECT JOIN")))
  (is (thrown?
       clojure.lang.ExceptionInfo
       (sql-honey/parse "SELECT JOIN ")))
  (is (= {:select [(types/identifier "JOINS")]}
         (sql-honey/parse "SELECT JOINS")))
  (is (= {:select [(types/identifier "JJOIN")]}
         (sql-honey/parse "SELECT JJOIN"))))

(deftest null-test
  (is (= {:select [nil]}
         (sql-honey/parse "SELECT NULL")))
  (is (= ["SELECT NULL"]
         (pretty-print "SELECT NULL"))))

(deftest select-as-test
  (is (= {:select [[:as
                    (types/identifier "x")
                    (types/identifier "y")]]}
         (sql-honey/parse "SELECT x AS y")))
  (is (= ["SELECT x AS y"]
         (pretty-print "SELECT x AS y"))))

(deftest case-test
  (is (= {:select [[:case
                    [:= (types/identifier "a") (types/identifier "b")]
                    (types/identifier "a")
                    (types/identifier "source" "count")]]}
         (sql-honey/parse "SELECT CASE WHEN a = b THEN a ELSE source.count END")))
  (is (= {:select [[:/
                    (types/cast (types/identifier "Q2" "count") (types/identifier "float"))
                    [:case
                     [:= (types/identifier "source" "count") 0] nil
                     (types/identifier "source" "count")]]]}
         (sql-honey/parse "SELECT (CAST(Q2.count AS float) / CASE WHEN source.count = 0 THEN NULL ELSE source.count END)")))
  (is (= ["SELECT CASE WHEN x = 1"
          "            THEN NULL"
          "            ELSE NULL"
          "       END"]
         (pretty-print "SELECT CASE WHEN x = 1 THEN NULL ELSE NULL END")))
  (is (= ["SELECT cast(Q2.count AS float)"
          "       /"
          "       CASE WHEN source.count = 0"
          "            THEN NULL"
          "            ELSE source.count"
          "       END"]
         (pretty-print "SELECT (CAST(Q2.count AS float) / CASE WHEN source.count = 0 THEN NULL ELSE source.count END)"))))
