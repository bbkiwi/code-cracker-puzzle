(ns code-cracker-puzzle.bill-utils
    (:gen-class)
    (:require [clojure.repl :refer :all]))

(defn myvars [] (keys (ns-interns *ns*)))                   ; use (myvars) to see what's in *ns*
(defn constant-map
  "Returns a map with the all keys mapped to the same const "
  [keys const]
  (zipmap keys (repeat (count keys) const)))

(defn replace-kv
  "Given a map of replacement pairs and a coll, returns a
   seq with any elements with index = a key in smap replaced with the
   corresponding val in smap"
  [smap coll]
  (let [inds (range (count coll))
        indmap (zipmap inds coll)
        rmap (merge indmap smap)]
    (replace rmap inds)))

(defn indices-ignoring
  "returns indices in coll of values not satisfied by pred"
  [pred coll]
  (keep-indexed #(when (not (pred %2)) %1) coll))

(defn replace-indices
  "returns coll with specified indices replaced by corresponding entry of rvals"
  [indices coll rvals]
  (let [rdist (replace-kv (zipmap indices rvals) coll)]
     rdist))




