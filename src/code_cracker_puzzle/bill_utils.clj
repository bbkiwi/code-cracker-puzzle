(ns code-cracker-puzzle.bill-utils
    (:gen-class)
    (:require [clojure.repl :refer :all]))

(defn myvars [] (keys (ns-interns *ns*))) ; use (myvars) to see what's in *ns*


