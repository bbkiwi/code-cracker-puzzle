(ns code-cracker-puzzle.core
    (:gen-class)
    (:require [clojure.string :as str]
              [clojure.set :as set]))

(def word-dic
  (slurp
    "D:\\Bill\\My Documents\\UCmatlab\\CodeCracker\\WordLists\\2of4brifblanksep.txt"))

(defn findall
  "Uses regex-pat which has group 1 the words sought to search dictionary dic: a string of words
   separated by single blanks. Returns list (possibly empty) of words from group 1."
  [regex-pat dic]
  ;(println regex-pat)
  (map #(nth % 1) (re-seq regex-pat dic)))

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
  (cond
    (= letuse "abcdefghijklmnopqrstuvwxyz") #"(\w)"
    :else (str "([" letuse "])")))

(defn pat-for-new
  "Gives pattern for first use of letter n 1...8"
  [n letuse]
  (cond
    (= n 1) (letpat letuse)
    :else (str "(?!\\" (str/join "|\\" (range 2 (inc n))) ")" (letpat letuse))))

(defn pat-for-old
  "Gives pattern for subsequent use of letter n 1...8"
  [n & letuse]  ; second param ignored but want to keep same arg list as pat-for-new
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
    :else
      (str char-from-pat)))

(defn code-cracker-to-regex-body
  "This is the recursive routine"
  [front-pat char-pat rest-pat letuse cleaned-letuse]
  (let [pfs (pat-for-symbol front-pat char-pat letuse cleaned-letuse)]
    (if (empty? rest-pat) pfs
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
     char-pat (first code-cracker-pat) ; not work if code-cracker-pat is vec
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

;; version 1
(defn decode1
  "Substitutes the letters in assigned-letters into clue (a coll of numbers 1 ... 26)"
  [clue assigned-letters]
  (map #(nth assigned-letters (dec %)) clue))

;; version 2 using maps
(defn decode
  "Substitues using assigned-letters-map to replace entries in clue
  (a coll of numbers)"
  [clue assigned-letters-map]
  (replace assigned-letters-map clue))

(defn encode
  "Substitues using assigned-letters-map to replace entries in clue
  (a coll of numbers)"
  ([word]
   (encode word  (zipmap (range 1 27) "abcdefghijklmnopqrstuvwxyz")))
  ([word assigned-letters-map]
   (replace (set/map-invert assigned-letters-map) (vec word))))

(defn make-code-cracker-pat
  "Take decoded vector and convert."
  [decoded-vec]
  (let [distinct-numbers (distinct (filter number? decoded-vec))
        temp-map (zipmap distinct-numbers (range 1 (inc (count distinct-numbers))))]
    ;(println temp-map)
    (str/join ""(replace temp-map decoded-vec))))

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
    (str/join ""(replace rmap (range 1 27)))))

(defn update-assigned-letters-map
  [assigned-letters-map clue word-found]
  (merge assigned-letters-map (zipmap clue word-found)))

(defn code-solver1
  "Takes clue (a vector of code numbers) and a partial solution map assigned-letters-map
   Returns a set of maps that correspond to each solution word."
  [clue assigned-letters-map]
  (let [letuse (clean-letuse "" (assigned-letters-map->assigned-letters-string assigned-letters-map))
        decoded-vec (decode clue assigned-letters-map)
        code-cracker-pat (make-code-cracker-pat decoded-vec)
        regexpat (code-cracker-pat-to-regexpat code-cracker-pat letuse)
        words-found (findall regexpat word-dic)]
    (println "Letters to Use " letuse)
    (println "Decoded Vector " decoded-vec)
    (println "Code Cracker Pattern " code-cracker-pat)
    (println "Regex pattern to search " regexpat)
    (println "Found " words-found)
    (reduce #(conj %1 (update-assigned-letters-map assigned-letters-map clue %2)) #{} words-found)))


(defn code-solver
  "Takes clue (a vector of code numbers) and a partial solution map assigned-letters-map
   Returns a set of maps that correspond to each solution word."
  [clue assigned-letters-map]
  ;(println clue assigned-letters-map)
  (cond
    (map? assigned-letters-map)
    (let [letuse (clean-letuse "" (assigned-letters-map->assigned-letters-string assigned-letters-map))
                decoded-vec (decode clue assigned-letters-map)
                code-cracker-pat (make-code-cracker-pat decoded-vec)
                regexpat (code-cracker-pat-to-regexpat code-cracker-pat letuse)
                words-found (findall regexpat word-dic)]
            ;(println "Letters to Use " letuse)
            ;(println "Decoded Vector " decoded-vec)
            ;(println "Code Cracker Pattern " code-cracker-pat)
            ;(println "Regex pattern to search " regexpat)
            ;(println "Found " words-found)
         (reduce #(conj %1 (update-assigned-letters-map assigned-letters-map clue %2)) #{} words-found))
    :else (reduce #(set/union %1 (code-solver clue %2)) #{} assigned-letters-map)))

(defn code-solver-from-clues
  [clue-set assigned-letters-map-coll]
  (reduce #(code-solver %2 %1) assigned-letters-map-coll clue-set))

(defn make-example
  "Breaks sentence into lower case words and encodes via 1 \\a, 2 \\b ...
   to get list of clues which are used to solve. The clues are decoded
   back to sentences for each solution found."
  [sentence]
  (let [clues (map encode (str/split (str/lower-case sentence) #" +"))
        solutions (code-solver-from-clues clues #{{}})]
    (println clues)
    (println "Found " (count solutions) " solutions")
    (map (fn [s] (str/join " "(map #(str/join "" (decode %1 s)) clues))) solutions)))


;(make-example "claps lap sap pas clap pal pals slap")
;TODO check clues are used in order given and can be a list
;TODO define order on clues related to assigned letters meaning more likely
;     to have fewer words satisfying
;TODO is there better way for responding to different types of arguments

(defn -main
  [clue-set initial-partial-sol-set]
  (println "Will Solve!"))


