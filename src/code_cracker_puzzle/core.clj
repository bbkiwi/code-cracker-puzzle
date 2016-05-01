(ns code-cracker-puzzle.core
    (:gen-class))

(def word-dic
  (slurp
    "D:\\Bill\\My Documents\\UCmatlab\\CodeCracker\\WordLists\\2of4brifblanksep.txt"))

(defn findall
  "Uses regex-pat which has group 1 the words sought to search dictionary dic: a string of words
   separated by single blanks. Returns list (possibly empty) of words from group 1."
  [regex-pat dic]
  (println regex-pat)
  (map #(nth % 1) (re-seq regex-pat dic)))

(defn clean-letuse
  "Removes letters that are used in codecracker pattern from letter to
   be used. letuse = \"\" is short for all letters. Returns string"
  ([letuse]
    (clean-letuse letuse ""))
  ([letuse codecracker-pattern]
    (let [actual-letuse (if (= letuse "") "abcdefghijklmnopqrstuvwxyz" letuse)]
      (clojure.string/join
        "" (clojure.set/difference (set actual-letuse) (set codecracker-pattern))))))

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
    :else (str "(?!\\" (clojure.string/join "|\\" (range 2 (inc n))) ")" (letpat letuse))))

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

(defn code-cracker-te-regex-body
  "This is the recursive routine"
  [front-pat char-pat rest-pat letuse cleaned-letuse]
  (let [pfs (pat-for-symbol front-pat char-pat letuse cleaned-letuse)]
    (if (empty? rest-pat) pfs
       (str pfs
            (code-cracker-te-regex-body
              (clojure.string/join "" [front-pat char-pat])
              (first rest-pat)
              (clojure.string/join "" (rest rest-pat))
              letuse
              cleaned-letuse)))))

(defn code-cracker-to-regexpat
  "Converts code cracker pattern to regex. Assume letters assigned sequentially from 1 to 8
  0 is a free letter. Pattern starts with blank which is start of word in the dictionary.
  The whole word is group 1, then code crackers unknowns are assinge groups 2 up to 8.
  Example \"b12t\"  goes to
  #\" (b([abdefghijklmnopqrsuvwxyz])(?!\\2)([abdefghijklmnopqrsuvwxyz])t)(?= )\"
  Note no checks on validity of code cracker pattern done - code letters must start
  and 1, introduced consecutively."
  [code-cracker-pat letuse]
  (let
    [front-pat ""
     char-pat (first code-cracker-pat)
     rest-pat (clojure.string/join "" (rest code-cracker-pat))]
    (re-pattern (str
          " ("
          (code-cracker-te-regex-body
              front-pat
              char-pat
              rest-pat
              (clean-letuse letuse)
              (clean-letuse letuse code-cracker-pat))
          ")(?= )" ))))

