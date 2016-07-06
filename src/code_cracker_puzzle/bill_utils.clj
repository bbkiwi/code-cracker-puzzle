(ns code-cracker-puzzle.bill-utils
    (:gen-class)
    (:require [clojure.repl :refer :all]
              [criterium.core :refer [quick-bench]]))

(defn myvars [] (keys (ns-interns *ns*)))                   ; use (myvars) to see what's in *ns*

;(defn constant-map
;  "Returns a map with the all keys mapped to the same const "
;  [keys const]
;  (zipmap keys (repeat (count keys) const)))
;
;; modeled after zipmap code is very marginally faster than above
;(defn constant-map
;  "Returns a map with the all keys mapped to the same const "
;  [keys val]
;  (loop [map {}
;         ks (seq keys)]
;    (if ks
;      (recur (assoc map (first ks) val)
;             (next ks))
;      map)))

; fastest
(defn constant-map
  "Returns a map with the all keys mapped to the same const "
  [keys val]
  (reduce (fn [map key] (assoc map key val)) {} keys))


;; still  slower than zipmap
;(defn zipmap-fast
;  "Returns a map with the keys mapped to the corresponding vals."
;  [keys vals]
;  (let [nv (count vals)]
;    (reduce-kv (fn [map ind key] (if (< ind nv)
;                                   (assoc map key (get vals ind))
;                                   (reduced map)))
;               {} (vec keys))))
;



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

(defn indices-satifying
  "returns indices in coll of values satisfied by pred"
  [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn replace-indices
  "returns coll with specified indices replaced by corresponding entry of rvals"
  [indices coll rvals]
  (let [rdist (replace-kv (zipmap indices rvals) coll)]
     rdist))

(defn nth-indices
  ([coll inds]
   (map (partial nth coll) inds))
  ([coll inds not-found]
   (map #(nth coll % not-found) inds)))

;(defn prod-fn
;  "Returns the carteasean product of functions fns
;  e.g. ((prod-fn f1 f2) x) is the vector [(f1 x) (f2 x)]"
;  [& fns]
;  (fn [x] (reduce #(conj %1 (%2 x)) [] fns)))

(defn natural-evaluation-map
  "((natural-evaluation-map x) f) = (f x)"
  [x]
  (fn [f] (f x)))

;(defn prod-fn
;  "Returns the carteasean product of functions fns
;  e.g. ((prod-fn f1 f2) x) is lazy seq  ((f1 x) (f2 x))"
;  [& fns]
;  (fn [x] (map #(%1 x) fns)))

(defn prod-fn
  "Returns the carteasean product of functions fns
  e.g. ((prod-fn f1 f2) x) is lazy seq ((f1 x) (f2 x))"
  [& fns]
  (fn [x] (map (natural-evaluation-map x) fns)))

(defn my-or
  [& args]
  (reduce #(if %1 (reduced %1) %2) args))

(defn my-or-looping
  [& args]
  (loop [val (first args)
         res (rest args)]
    (cond
      val val
      (empty? res) val
      :else (recur (first res) (rest res)))))

(defn my-or-looping-rec
  [& args]
  (loop [val (first args)
         res (rest args)]
    (cond
      val val
      (empty? res) val
      :else (apply my-or-looping-rec res))))
(coll? nil)

;; fail (my-or-looping nil 2)
;(defn my-or-looping
;  [& args]
;  (loop [val (first args)
;         res (rest args)]
;    (cond
;      val val
;      res val
;      :else (recur (first res) (next res)))))

(defn my-or-recursion
  ([] nil)
  ([arg] arg)
  ([arg & args]
   (if arg
     arg
     (apply my-or-recursion  (first args) (rest args)))))


(defn my-or-recursion
  ([] nil)
  ([arg] arg)
  ([arg & args]
   (let [[farg & rargs] args]
     (if arg
       arg
       (apply my-or-recursion farg rargs)))))


(defn my-or-recur
  [& args]
  (let [[x & more] args]
    ;(println x more)
    (cond
      x x
      more (recur more)
      :else x)))

(defn my-or-recurp
  [& args]
  (let [[x & more] args]
    ;(println x more)
    (condp #(= (boolean %1) %2) true
      x x
      more (recur more)
      x)))

; same as above
(defn my-or-recurx
  [& args]
  (let [x (first args)
        more (next args)] ; (rest args) fails
    ;(println x more)
    (cond
      x x
      more (recur more)
      :else x)))

(defn pred-or
  "Returns a predicate that evaluates preds one at a time,
  from left to right. If any returns a logical true value,
  it returns that value and doesn't evaluate any of the other expressions,
  otherwise it returns nil   ((pred-or) arg) returns nil."
  [& preds]
  (fn [arg] (some #(%1 arg) preds)))

(defn pred-and
  "Returns a predicate that evaluates preds one at a time,
  from left to right. If any returns a logical false value,
  it returns false and doesn't evaluate any of the other expressions,
  otherwise it returns true  ((pred-and) arg) returns true."
  [& preds]
  (fn [arg] (every? #(%1 arg) preds)))

; slower version
;(defn pred-or
;  "Returns a predicate that evaluates preds one at a time,
;  from left to right. If any returns a logical true value,
;  it returns that value and doesn't evaluate any of the other expressions,
;  otherwise it returns the  value of the last expression
;  (which will be nil or false)
;  ((pred-or) arg) returns nil."
;  [& preds]
;  (fn [arg]
;    (reduce
;      (fn [res pred]
;        (let [pa (pred arg)]
;          (if pa (reduced pa) pa)))
;      nil
;      preds)))

