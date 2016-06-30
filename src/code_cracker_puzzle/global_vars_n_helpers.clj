(ns code-cracker-puzzle.global-vars-n-helpers
    (:gen-class)
    (:require [code-cracker-puzzle.bill-utils :refer :all]
              [clojure.string :as str]
              [clojure.set :as set]
              [clojure.walk]
              [clojure.repl :refer :all]))

(def dotmap (merge (constant-map (range 1 27) \-) (constant-map (range 27 500) \*) {0 (char 32)}))


;TODO faster if keep set of words so (in-dictionary? word) == (all-words-in-set word)
(defn in-dictionary?
  "Returns wordstring with blanks added either side (truthy) if in dic else nil (falsey)"
  [wordstring dic]
  ;(println regex-pat)
  (re-find (re-pattern (str " " wordstring " ")) dic))

(defn clean-letuse
  "Removes letters that are used in codecracker pattern from letter to
   be used. letuse = \"\" is short for all letters. Returns string"
  ([letuse]
   (clean-letuse letuse ""))
  ([letuse codecracker-pattern]
   (let [actual-letuse (if (= letuse "") "abcdefghijklmnopqrstuvwxyz" letuse)]
     (str/join
       "" (set/difference (set actual-letuse) (set codecracker-pattern))))))

(defn decode-to-vec
  "Decodes the clue (coll of  numbers) using assigned-letters-map to
  a vector of characters"
  [clue assigned-letters-map]
  (replace assigned-letters-map clue))

(defn decode
  "Decodes the clue (coll of  numbers) using assigned-letters-map to
  a string. Note if some numbers are not keys in the map they will turn to strings."
  [clue assigned-letters-map]
  (str/join "" (decode-to-vec clue assigned-letters-map)))

(defn encode
  "Encodes a string of letters using assigned-letters-map
  (or default {1 \\a, 2 \\b ...}  to a vector of numbers and
  characters (if they are not in values in the map)"
  ([word]
   (encode word (zipmap (range 1 27) "abcdefghijklmnopqrstuvwxyz")))
  ([word assigned-letters-map]
   (replace (set/map-invert assigned-letters-map) (vec word))))

(defn assigned-letters-map->assigned-letters-string
  [alm]
  (let [dmap (zipmap (range 1 27) "??????????????????????????")
        rmap (merge dmap alm)]
    (str/join "" (replace rmap (range 1 27)))))
