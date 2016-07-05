(ns code-cracker-puzzle.core-improved-docs
    (:require [clojure.repl :refer :all]))

(defn empty?
  "Returns true if coll has no items or coll = nil
   - same as (not (seq coll)).
  Please use the idiom (seq x) rather than (not (empty? x))"
  {:added "1.0"
   :static true}
  [coll] (not (seq coll)))

(defmacro or
  "Evaluates exprs one at a time, from left to right. Returns the
  first logical true value and doesn't evaluate any of the other
  expressions, otherwise returns the value of the last expression.
  (or) returns nil."
  {:added "1.0"}
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if or# or# (or ~@next)))))

(comment; need doc to say what it works on as coll is not correct
  distinct
  nth)


