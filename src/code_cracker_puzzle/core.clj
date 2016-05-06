(ns code-cracker-puzzle.core
    (:gen-class)
    (:require [clojure.string :as str]
              [clojure.set :as set]
              [clojure.edn :as edn]                         ;safe io
              [clojure.core.matrix :as m]))


(defn myvars [] (keys (ns-interns *ns*))) ; use (myvars) to see what's in *ns*

(def word-dic
  (slurp "D:\\Bill\\My Documents\\UCmatlab\\CodeCracker\\WordLists\\2of4brifblanksep.txt"))

(def CCdata
  (slurp "D:\\Bill\\My Documents\\UCmatlab\\CodeCracker\\CCdata.txt"))

(defn findall
  "Uses regex-pat which has group 1 equal the words sought to search dictionary dic, a string of words
   separated by single blanks. Returns lazy sequence (possibly empty) of words from group 1.
   #\" (regexforword)(?= \"   "
  [regex-pat dic]
  ;(println regex-pat)
  (map #(nth % 1) (re-seq regex-pat dic)))

(defn in-dictionary?
  "Returns wordstring with blanks added either side (truthy) if in dic else nil (falsey)"
  [wordstring dic]
  ;(println regex-pat)
  (re-find (re-pattern (str " "  wordstring " ")) dic))

(defn clean-letuse
  "Removes letters that are used in codecracker pattern from letter to
   be used. letuse = \"\" is short for all letters. Returns string"
  ([letuse]
   (clean-letuse letuse ""))
  ([letuse codecracker-pattern]
   (let [actual-letuse (if (= letuse "") "abcdefghijklmnopqrstuvwxyz" letuse)]
     (str/join
       "" (set/difference (set actual-letuse) (set codecracker-pattern))))))

(defn letpat
  [letuse]
  (if
    (= letuse "abcdefghijklmnopqrstuvwxyz") #"(\w)"
    (str "([" letuse "])")))

(defn pat-for-new
  "Gives pattern for first use of letter n 1...8"
  [n letuse]
  (if
    (= n 1) (letpat letuse)
    (str "(?!\\" (str/join "|\\" (range 2 (inc n))) ")" (letpat letuse))))

(defn pat-for-old
  "Gives pattern for subsequent use of letter n 1...8"
  [n & letuse]                                              ; second param ignored but want to keep same arg list as pat-for-new
  (str "\\" (inc n)))

(defn char->num
  "Convert char \0 ... to number 0 ..."
  [ch]
  (- (int ch) (int \0)))

(defn pat-for-symbol
  "Gives pattern for each symbol in code cracker pattern"
  [front-pat char-from-pat letuse cleaned-letuse]
  (cond
    (= char-from-pat \0) (letpat letuse)
    (contains? (set "12345678") char-from-pat)
    (if (contains? (set front-pat) char-from-pat)
      (pat-for-old (char->num char-from-pat) cleaned-letuse)
      (pat-for-new (char->num char-from-pat) cleaned-letuse))
    :else (str char-from-pat)))


; maybe use reduce here
(defn code-cracker-to-regex-body
  "This is the recursive routine"
  [front-pat char-pat rest-pat letuse cleaned-letuse]
  (let [pfs (pat-for-symbol front-pat char-pat letuse cleaned-letuse)]
    (if (empty? rest-pat)
      pfs
      (str pfs
           (code-cracker-to-regex-body
             (str/join "" [front-pat char-pat])
             (first rest-pat)
             (str/join "" (rest rest-pat))
             letuse
             cleaned-letuse)))))

(defn code-cracker-pat-to-regexpat
  "Converts code cracker pattern string to regex. Assume letters assigned sequentially from 1 to 8
  0 is a free letter. Pattern starts with blank which is start of word in the dictionary.
  The whole word is group 1, then code crackers unknowns are assinge groups 2 up to 8.
  Example \"b12t\"  goes to
  #\" (b([abdefghijklmnopqrsuvwxyz])(?!\\2)([abdefghijklmnopqrsuvwxyz])t)(?= )\"
  Note no checks on validity of code cracker pattern done - code letters must start
  and 1, introduced consecutively."
  ;TODO might want code-cracker-pat to be vector and will need to fix
  [code-cracker-pat letuse]
  (let
    [front-pat ""
     char-pat (first code-cracker-pat)                      ; not work if code-cracker-pat is vec
     rest-pat (str/join "" (rest code-cracker-pat))]
    (re-pattern (str
                  " ("
                  (code-cracker-to-regex-body
                    front-pat
                    char-pat
                    rest-pat
                    (clean-letuse letuse)
                    (clean-letuse letuse code-cracker-pat))
                  ")(?= )"))))

;; version using maps
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
  "Encodes a string of letters using assigned-letters-map to
  a vector of numbers and characters (if they are not in values in the map)"
  ([word]
   (encode word (zipmap (range 1 27) "abcdefghijklmnopqrstuvwxyz")))
  ([word assigned-letters-map]
   (replace (set/map-invert assigned-letters-map) (vec word))))

(defn make-code-cracker-pat
  "Take decoded vector and convert."
  [decoded-vec]
  (let [distinct-numbers (distinct (filter number? decoded-vec))
        temp-map (zipmap distinct-numbers (range 1 (inc (count distinct-numbers))))]
    ;(println temp-map)
    (str/join "" (replace temp-map decoded-vec))))

(defn assigned-letters-string->assigned-letters-map
  [als]
  ; TODO why does this fail?
  ;(filter #(<= (int \a) (int (val %)) (int \z))
  ;                                 (zipmap (range 1 (inc (count als))) als))
  (apply hash-map (flatten (filter #(<= (int \a) (int (val %)) (int \z))
                                   (zipmap (range 1 (inc (count als))) als)))))

(defn assigned-letters-map->assigned-letters-string
  [alm]
  (let [dmap (zipmap (range 1 27) "??????????????????????????")
        rmap (merge dmap alm)]
    (str/join "" (replace rmap (range 1 27)))))

(defn update-assigned-letters-map
  [assigned-letters-map clue word-found]
  (merge assigned-letters-map (zipmap clue word-found)))


; version making lazy sequences representing tree with levels from each clue
(defn code-solver
  "Takes clue (a vector of code numbers) and a partial solution map assigned-letters-map
   Returns a lazy seq representing a tree with leaves the maps that correspond to
   each solution word."
  [clue assigned-letters-map]
  ;(println clue assigned-letters-map)
  (if
    (map? assigned-letters-map)   ; map or else lazy seq tree of maps
    (let [letuse (clean-letuse "" (assigned-letters-map->assigned-letters-string assigned-letters-map))
          decoded-vec (decode-to-vec clue assigned-letters-map)
          code-cracker-pat (make-code-cracker-pat decoded-vec)
          regexpat (code-cracker-pat-to-regexpat code-cracker-pat letuse)
          words-found (findall regexpat word-dic)]
      ;(println "Letters to Use " letuse)
      ;(println "Decoded Vector " decoded-vec)
      ;(println "Code Cracker Pattern " code-cracker-pat)
      ;(println "Regex pattern to search " regexpat)
      ;(println "Found " words-found)
      ; TODO only conj if non-empty updated map to add
      (reduce #(lazy-seq (conj %1 (update-assigned-letters-map assigned-letters-map clue %2))) (lazy-seq) words-found))
    (clojure.walk/postwalk #(if (map? %) (code-solver clue %) %) assigned-letters-map)))


; TODO this version takes clues from clue-set in order given
; however there is a ranking of clues for an assigned-letters-map
; it should be the same for each map in the coll of maps
; the best should be taken first, so need to reduce in that order
; and recalculate the ranking each time

(defn code-solver-from-clues
  [clue-set assigned-letters-map-coll]
  (reduce #(code-solver %2 %1) assigned-letters-map-coll clue-set))

;  score by number of different letters needed to complete less 1  over total length
;  zero if one letter left, neg if complete, always < 1
;  check all completed words and  assign positive test score of 1 if not in dictionary 2 if in dictionary
(defn score-clue
  [clue  decode-map]
  (->>
    (decode-to-vec clue decode-map)
    (filter number?)
    (distinct)
    (count)
    (dec)
    (* (/ (count clue)))))

(defn rank-n-sort-clues
  [clues decode-map]
  (->>
    clues
    (filter #(>= (score-clue % decode-map) 0))
    (sort-by #(score-clue % decode-map))))



; TODO check further, using simple rank, not using clue that gives unique
; not sure if get to words not in dictionary
(defn code-solver-using-best-clue
  [clues assigned-letters-map-coll]
  (let [leaves (filter map? (tree-seq (complement map?) identity  assigned-letters-map-coll))
        letmaptosort (first leaves)
        sortclues (rank-n-sort-clues clues letmaptosort)]
    ;(println letmaptosort)
    ;(println sortclues)
    (if (first sortclues)
      (code-solver-using-best-clue clues
                                   (code-solver
                                     (first sortclues)
                                     assigned-letters-map-coll))
      leaves)))

; Examples

; (def cc (get-cc 1))
; (def ans (code-solver-using-best-clue (:clues cc) (:encodemap cc)))
; (show-sol 0 ans (:clues cc))


; TODO make code-solver-using-best-clue
; rank clues, can from best to worse finding one with smallest number of matches,
; use it and recurse - terminate when all rankings are positive.

(defn brute-solver-from-4-clues
  [clues assigned-letters-map-tree]
  (->>
    assigned-letters-map-tree
    (code-solver (nth clues 0))
    (code-solver (nth clues 1))
    (code-solver (nth clues 2))
    (code-solver (nth clues 3))
    (tree-seq (complement map?) identity)   ;TODO Why is this flattening it out?
    (filter map?)))


(defn make-example
  "Breaks sentence into lower case words and encodes via 1 \\a, 2 \\b ...
   to get list of clues which are used to solve. The clues are decoded
   back to sentences for each solution found."
  [sentence]
  (let [clues (map encode (str/split (str/lower-case sentence) #" +"))
        solutions (code-solver-from-clues clues #{{}})
        solutions (tree-seq (complement map?) identity solutions)
        solutions (filter map? solutions)]
    (println clues)
    ;(println "Found " (count solutions) " solutions")
    (map (fn [s] (str/join " " (map #(decode %1 s) clues))) solutions)))


;(def tt (fn [v] (filter #(and (> (count %) 1) (not (contains? (set %) 0))) (partition-by zero? v))))

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
        col-vectors (m/transpose row-vectors)
        clues-in-vec (fn [v] (filter #(and (> (count %) 1) (not (contains? (set %) 0))) (partition-by zero? v)))
        horizontal-clues (apply concat (map clues-in-vec row-vectors))
        vertical-clues (apply concat (map clues-in-vec col-vectors))
        all-clues (concat horizontal-clues vertical-clues)
        ; TODO why does this fail? It doesnt give a map.
        ;em (filter #(not= \space (val %))(zipmap (range 1 27) assigned-letters))
        em (apply hash-map (flatten (filter #(not= \space (val %)) (zipmap (range 1 27) assigned-letters))))
        nasm (zipmap (range 1 27) "..........................")
        ;TODO why below causes no error but only wrong answers?
        ;pw (map #(str/join "" (decode % (merge nasm (:encodemap cc)))) (:clues cc))
        pw (map #(decode % (merge nasm em)) all-clues)]

    (println cc-date)
    (println assigned-letters)
    ;(println row-strings)
    ;(println row-vectors)
    ;(println col-vectors)
    ;(println clues-in-vec)
    ;(println horizontal-clues)
    ;(println vertical-clues)
    ;(edn/read-string (str  "[" (str/join " " row-strings) "]"))
    {:date         cc-date  ;string
     :encodemap    em  ; map
     :partialwords pw  ; lazy seq of strings
     :clues        all-clues}))  ; lazy seq of cons




;(make-example "pas pals clap sap lap slap claps pal")

;TODO finding have to constantly change type so can get commands to work!
;TODO finding editing constantly changing how parens go and muck up code
;TODO check clues are used in order given and can be a list
;TODO define order on clues related to assigned letters meaning more likely
;     to have fewer words satisfying
;TODO is there better way for responding to different types of arguments
;TODO Why does ESC put you back in edit window??
;TODO Why when added core.matrix to project dependency and ns here, it was not recognized until I restarted
;     Said it was resynchronizing Leiningen


(defn show-sol
  "n is which solution to view
   ans is coll of solutions which are maps,
   clues is coll of lists of code numbers"
  [n ans clues]
  (map (fn [s] (str/join " " (map #(decode %1 s) clues))) [(nth ans n)]))


; do some filtering out of empty maps
; use tree-seq to explore
(defn -main
  [numcc]
  (let [cc (get-cc numcc)]
    (code-solver-from-clues (:clues cc) (:encodemap cc))))    ; will be very slow


;(def ans (brute-solver-from-4-clues [[1 2 3 2 1],[4 5 6 4],[1 6 5], [1 5 6]] {} ))
;; this takes awhile
;(def clues [[1 2 3 2 1],[1 5 6],[6 7 8 9], [9 10 11 12]])
;(def ans (brute-solver-from-4-clues clues {}))
;(show-sol 75342 ans clues)   ;("madam mic cops stub")




; puzzle done in matlab program TestSolveCC
;(def cc (get-cc 48))
; clues that were used (gave unique matches) were in order
; - not all on board were needed
; (time (test-cc-48 1)) works slowly i.e. uses key clue to start
; (time (test-cc-48 5) much faster
; but (test-cc-48 0) overflows the stack
; TODO Why? I thought lazy-seq would prevent
(defn test-cc-48
  "uses the first n from good ones augments rest"
  [n]
  (let [cc (get-cc 48)
        good-order-clues
        [[13 17 14 8 10 17 17 5 3 ] [5 8 14 22 3 ] [5 11 8 10 3 5 15 ] [3 6 15 3 6]
         [11 6 15 6 16 5 17 ] [1 6 12 11 5 1 6 ] [6 22 23 6 1 3 ] [2 5 11 15 5 12 6]
         [6 7 5 2 8 3 ] [5 23 5 2 24 ] [3 9 10 11 8 3 ] [9 5 25 6 3 ] [4 11 14 15 12]
         [4 21 22 5 ] [22 5 21 20 6 ] [6 15 20 14 26 3 ] [18 14 2 24 6 26 3]
         [19 21 5 3 9 6 3]]
        n-good-clues (distinct (concat (take n good-order-clues) (:clues cc)))
        ans (code-solver-from-clues n-good-clues (:encodemap cc))
        maps-in-ans (filter map? (tree-seq (complement map?) identity ans))
        scores (map #(score-clue % (:encodemap cc)) n-good-clues)]
    (println cc)
    (println scores)
    (show-sol 0 maps-in-ans n-good-clues)))



; Examples - some work n 1 ... 48 other stack overflow
(defn solve-example-cc
  [n]
  (let [cc (get-cc n)
        ans (code-solver-using-best-clue (:clues cc) (:encodemap cc))]
    (println (count ans))
    (time (show-sol 0 ans (:clues cc)))))




; observation
; (apply distinct? (distinct x))  ;true

; comment with line comment should be toggle ...

;various docs say arguments can be coll when it is not correct
;often don't work for maps which are colls!
;e.g. distinct, nth ...

;(coll? {:a 1 :b 33})
;=> true
;(distinct {:a 1 :b 33})  ; error!
;(nth {:a 1 :b 33} 0)     ; error!