(ns code-cracker-puzzle.data-assembly
    (:gen-class)
    (:require [code-cracker-puzzle.bill-utils :refer :all]
              [clojure.string :as str]
              ;[clojure.set :as set]
              [clojure.edn :as edn]                         ;safe io
              [clojure.walk]
              [clojure.repl :refer :all]
              ;[io.aviso.ansi :as ioa]
              ;[io.aviso.columns :as col]
              [criterium.core :refer [quick-bench]]))


(def word-dic-words
  "string starting with blank consisting of words each followed by blank"
  ;(slurp "D:\\Bill\\My Documents\\UCmatlab\\CodeCracker\\WordLists\\2of4brifblanksep.txt")
  (slurp "resources/2of4brifblanksep.txt"))

;put individual letters in dictionary
(def word-dic (str " a b c d e f g h i j k l m n o p q r s t u v w x y z " word-dic-words))

; persistent vec of all words (note removal of any leading blanks from word-dic before splitting)
(def all-words-in-vec (str/split (str/replace-first word-dic #" *" "") #" +"))
; set of all words
(def all-words-in-set (set all-words-in-vec))
; a small sub set for testing use
(def some-words-in-set (set (take 400 all-words-in-set)))
; lazy seq of all words
(def all-words-in-lazy-seq (lazy-seq all-words-in-vec))
;(declare findall)
;(defn all-words-in-dic [] (findall #" +(\w*)(?= )" word-dic))

(def CCdata
  ;(slurp "D:\\Bill\\My Documents\\UCmatlab\\CodeCracker\\CCdata.txt")
  (slurp "resources/CCdata.txt"))

(defn transpose
  "own version of m/transpose [clojure.core.matrix :as m]"
  [row-vectors]
  (apply map (fn [& row] (vec row)) row-vectors))

; alternate read
; (with-open [rdr (clojure.java.io/reader "D:\\Bill\\My Documents\\UCmatlab\\CodeCracker\\CCdata.txt")]
;  (count (line-seq rdr)))
(import '(java.io BufferedReader StringReader))
(defn get-cc
  [numcc]
  (let [n1 (* (dec numcc) 15)
        n2 (+ n1 15)
        cc-date (nth (line-seq (BufferedReader. (StringReader. CCdata))) n1)
        assigned-letters (str/lower-case (nth (line-seq (BufferedReader. (StringReader. CCdata))) (inc n1)))
        row-strings (subvec (vec (line-seq (BufferedReader. (StringReader. CCdata)))) (+ n1 2) n2)
        row-vectors (map #(edn/read-string (str "[" %1 "]")) row-strings)
        col-vectors (transpose row-vectors)
        clues-in-vec (fn [v] (filter #(and (> (count %) 1) (not (contains? (set %) 0))) (partition-by zero? v)))
        horizontal-clues (apply concat (map clues-in-vec row-vectors))
        vertical-clues (apply concat (map clues-in-vec col-vectors))
        all-clues (concat horizontal-clues vertical-clues)
        em (apply hash-map (flatten (filter #(not= \space (val %)) (zipmap (range 1 27) assigned-letters))))]
    (println cc-date)
    (println assigned-letters)
    ;(println row-strings)
    ;(println row-vectors)
    ;(println col-vectors)
    ;(println clues-in-vec)
    ;(println horizontal-clues)
    ;(println vertical-clues)
    ;(edn/read-string (str  "[" (str/join " " row-strings) "]"))
    {:date         cc-date                                  ;string
     :encodemap    em                                       ; map
     :clues        all-clues}))                             ; lazy seq of cons


(defn free-grid
  "make clues for nxn grid of all free letters"
  [n]
  (let [nsq (* n n)
        rows (map vec (partition n (range 30 (+ 30 nsq))))
        cols (transpose rows)]
    (concat rows cols)))

(defn asym-grid
  "make clues for nxn grid of all free letters
  except first anti-diagonal which will prevent
  symmetric solutions"
  [n]
  (clojure.walk/prewalk-replace {31 1 (+ 30 n) n} (free-grid n)))


(defn free-cube
  "make clues for nxnxn cube of all free letters"
  [n]
  (let [nsq (* n n)
        ncube (* nsq n)
        in (range n)
        c1 (map vec (partition n (range 30 (+ 30 ncube))))
        cube (map vec (partition n c1))
        coor-fn (fn [x y z] (nth (nth (nth cube x) y) z))
        coor3 (for [x in y in] (vec (map #(coor-fn x y %) in)))
        coor2 (for [x in z in] (vec (map #(coor-fn x % z) in)))
        coor1 (for [y in z in] (vec (map #(coor-fn % y z) in)))]
    ;(println cube)
    (concat coor3 coor2 coor1)))

(defn asym-cube
  "make clues for nxnxn cube of all free letters
  except first anti-slice which will prevent
  symmetric solutions"
  [n]
  (clojure.walk/prewalk-replace {31 1 (+ 30 n) n (+ 30 (* n n)) (* n n)} (free-cube n)))

(defn diagsame-grid
  "make clues for nxn grid of letters constrained so same
  code when rowindex+colindex same"
  [n]
  (let [nsq (* n n)
        rows (vec (map vec (partition n (range nsq))))
        cols (transpose rows)
        mat (map (fn [r c] (vec (map #(+ %1 %2) r c))) rows cols)
        dv (distinct (flatten mat))
        mp (zipmap dv (range 30 (+ 30 (count dv))))]
    (clojure.walk/prewalk-replace mp mat)))

(defn sym-grid
  "make clues for nxn grid of letters constrained by transpose"
  [n]
  (let [nsq (* n n)
        rows (vec (map vec (partition n (range nsq))))
        cols (transpose rows)
        mat (map (fn [r c] (vec (map #(min %1 %2) r c))) rows cols)
        dv (distinct (flatten mat))
        mp (zipmap dv (range 30 (+ 30 (count dv))))]
    (println mat)
    (clojure.walk/prewalk-replace mp mat)))

