(ns sql-honey.core
  (:require [potemkin :as p]
            [sql-honey.parse :as parse]
            [sql-honey.print :as print]))

(comment parse/keep-me print/keep-me)

(p/import-vars
 [parse
  parse]

 [print
  pretty-print-parsed
  pretty-print-sql])
