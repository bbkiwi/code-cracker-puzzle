(ns code-cracker-puzzle.work
    (:gen-class)
    (:require [clojure.string :as str]
              [clojure.set :as set]
              [clojure.edn :as edn]                         ;safe io
              [clojure.core.matrix :as m]
              [clojure.walk]
              [clojure.repl :refer :all]
              [io.aviso.ansi :as ioa]
              [io.aviso.columns :as col]
              [criterium.core :refer [quick-bench]]))

; why do I need require clojure.repl here?
; ANS in project.clj ns desiginated by :main will get it.

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
; lazy seq of all words
(def all-words-in-lazy-seq (lazy-seq all-words-in-vec))
;(declare findall)
;(defn all-words-in-dic [] (findall #" +(\w*)(?= )" word-dic))


;TODO faster if keep set of words so (in-dictionary? word) == (all-words-in-set word)
(defn in-dictionary?
  "Returns wordstring with blanks added either side (truthy) if in dic else nil (falsey)"
  [wordstring dic]
  ;(println regex-pat)
  (re-find (re-pattern (str " "  wordstring " ")) dic))

(def CCdata
  ;(slurp "D:\\Bill\\My Documents\\UCmatlab\\CodeCracker\\CCdata.txt")
  (slurp "resources/CCdata.txt"))

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

;TODO remove restriction that a code can have at most 8 distinct code numbers
(defn make-code-cracker-pat
  "Take decoded vector and convert.
  The vector should consist of pos integers and characters"
  [decoded-vec]
  (let [distinct-range-numbers (distinct (filter number? decoded-vec))
        temp-map (zipmap distinct-range-numbers (range 1 (inc (count distinct-range-numbers))))]
    ;(println temp-map)
    (str/join "" (replace temp-map decoded-vec))))

(defn letpat
  [letuse]
  (if
    (= letuse "abcdefghijklmnopqrstuvwxyz") #"(\w)"
                                            (str "([" letuse "])")))
(defn letpat-free
  [letuse]
  (if
    (= letuse "abcdefghijklmnopqrstuvwxyz") #"\w"
                                            (str "[" letuse "]")))

(defn pat-for-new
  "Gives pattern for first use of letter n 1...8"
  [n letuse]
  (if
    (= n 1) (letpat letuse)
            (str "(?!\\" (str/join "|\\" (range 2 (inc n))) ")" (letpat letuse))))

(defn pat-for-old
  "Gives pattern for subsequent use of letter n 1...8"
  [n & letuse]  ; second param ignored but want to keep same arg list as pat-for-new
  (str "\\" (inc n)))

(defn char->num
  "Convert char \0 ... to number 0 ..."
  [ch]
  (- (int ch) (int \0)))

;TODO want different types of free letters
;  could be any letter, or any letter not assigned to code 1...27
;  or could be any letter that has been assigned to code 1...27 (or other ranges
;  or vowels or consonants ...
(defn pat-for-symbol
  "Gives pattern for each symbol in code cracker pattern"
  [front-pat char-from-pat letuse cleaned-letuse]
  (cond
    (= char-from-pat \0) (letpat-free (clean-letuse "")) ; any letter
    ;(= char-from-pat \0) (letpat-free letuse) ;   those not assigned
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
  "Converts code cracker pattern string to regex. Assume letters assigned sequentially from 1 to 8.
  1...27 correspond to a permutation of the alphabet, 0 or higher numbers are free to be any letter.
  Pattern starts with blank which is start of word in the dictionary.
  The whole word is group 1, then code crackers unknowns are assigned groups 2,...,9.
  Example \"b12t\"  goes to
  #\" +(b([adefghijklmnopqrsuvwxyz])(?!\\2)([adefghijklmnopqrsuvwxyz])t)(?= )\"
  Note no checks on validity of code cracker pattern done - code letters must start
  at 1, introduced consecutively."
  ;TODO might want code-cracker-pat to be vector and will need to fix
  [code-cracker-pat letuse]
  (let
    [front-pat ""
     char-pat (first code-cracker-pat)  ; not work if code-cracker-pat is vec
     rest-pat (str/join "" (rest code-cracker-pat))]
    (re-pattern (str
                  " +("
                  (code-cracker-to-regex-body
                    front-pat
                    char-pat
                    rest-pat
                    (clean-letuse letuse)
                    (clean-letuse letuse code-cracker-pat))
                  ")(?= )"))))

(defn findall
  "Uses regex-pat which has group 1 equal the words sought to search dictionary dic, a string of words
   separated by single blanks (also starting and ending with a blank. Returns lazy sequence (possibly empty) of words from group 1.
   #\" +(regexforword)(?= )\"   "
  [regex-pat dic]
  ;(println regex-pat)
  (map #(nth % 1) (re-seq regex-pat dic)))

(defn find-all-words
  "Takes clue (a vector of code numbers) and a partial solution map assigned-letters-map
   Returns a lazy seq  of each solution word."
  [clue assigned-letters-map]
  (let [letuse (clean-letuse "" (assigned-letters-map->assigned-letters-string assigned-letters-map))
        decoded-vec (decode-to-vec clue assigned-letters-map)
        code-cracker-pat (make-code-cracker-pat decoded-vec)
        regexpat (code-cracker-pat-to-regexpat code-cracker-pat letuse)]
    ;(println letuse "\n" code-cracker-pat "\n" regexpat)
    (findall regexpat word-dic)))

;  a clue is a vector of numbers 1..26 represent the alphabet permuted
;         so 2 and 21 will represent different letters
; TODO do I really want 0 to be used as a code?
;         numbers 0, and above 26 correspond to arbitrary letters so 29 can be any letter
;  ; Using masks to generate transducers for finding words
;  A partial decoded clue (pdc) is a vector of numbers and characters
;  e.g. [11 20 \a 11] will be used to find 4 letter words like
;  "blab" "dead" "rear" "roar" "seas" "spas" "teat" "that" "twat"
;        which have 3rd letter a, 1st and 4th letters the same.
;        Each match determines what code 11 and code 20 stand for.
;  Such pdc can generate masks which can select certain letters from
;     word lists - the masks have truthy entries for letters to be used
;     and falsey (nil) for letters to be rejected
;  e.g.
;   using value \a in the pdc produces
;     this mask (nil nil \a nil)
;     applying this mask to a word and checking if it is \a finds those with 3rd letter a
;   using value 11 gives (11 nil nil 11) applying to a word and checking if count is 1
;     finds those with 1st and 4th letters the same
;   using values [11 20] gives (11 20 nil 11] applying and checking if count is 2
;     finds those with letters corresponding to codes 11 and 12 distinct
;   using pdc as values give (11 20 \a 11) applying and checking if count is 4
;     finds those words with 4 letters
;   other masks can be used to test if
;     certain codes belong to letters in a specific set
;     certain sub seq of the clue are words
;TODO other examples
;

(comment
  ;(quick-bench (doall (applymask-1 [\c nil nil 2 3 nil 2] [1 2 3 4 5 6 7]))) ; mean : 6.888192 µs
  (defn applymask-1
    "mask is vec of truthy/nil. Select those elements of word corresponding to truthy
    if mask is shorter than word it is truncated, if longer padded with truthy
    word can be string, vec, list. Produces lazy-seq"
    [mask word]
    (keep-indexed #(when (nth mask %1 %2) %2) word))

  ; (quick-bench (doall (applymask-2 [\c nil nil 2 3 nil 2] [1 2 3 4 5 6 7]))) ; mean : 17.868987 µs
  (defn applymask-2
    "mask is vec of truthy/nil. Select those elements of word corresponding to truthy
   if mask is shorter than word it is truncated, if longer padded with truthy
   word can be string, vec, list. Produces lazy-seq"
    [mask word]
    (for [[m c] (map vector mask word) :when m] c)))

; fastest
;(quick-bench (applymask [\c nil nil 2 3 nil 2] [1 2 3 4 5 6 7])) ;mean : 3.912921 µs
(defn applymask
  "mask is vec of truthy/nil. Select those elements of word corresponding to truthy
  if mask is shorter than word it is truncated, if longer padded with truthy
  word can be string, vec, list. Produces persistent vector"
  [mask word]
  (reduce-kv #(if (nth mask %2 %3) (conj %1 %3) %1) [] (vec word)))

; variation that selects just the first truthy of mask
(defn applymask-till-match
  "mask is vec of truthy/nil. Select first element of word corresponding to truthy
  if mask is shorter than word it is truncated, if longer padded with truthy
  word can be string, vec, list. Produces persistent vector"
  [mask word]
  (reduce-kv #(if (nth mask %2 %3) (reduced (conj %1 %3)) %1) [] (vec word)))

(defn mask-from-values
  [pdc values]
  (let [set-of-values (if (coll? values) (set values) (conj #{} values))]
    ;(println set-of-values)
    (map set-of-values pdc)))

(defn mask-for-free-clues
  "will select all codes that are numbers > 26"
  [pdc]
  (let [fv (filter #(and (number? %) (> % 26)) pdc)]
    (mask-from-values pdc fv)))

(defn mask-for-constrained-clues
  "will select all codes that are numbers 1 to 26"
  [pdc]
  (let [cv (filter #(and (number? %) (> % 0) (< % 27)) pdc)]
    (mask-from-values pdc cv)))

;TODO use a positional mask
; (map #(and %1 %2) [nil 1 1 1 nil] [11 22 33 \t 11])
;; (nil 22 33 \t nil) applied and result checked if also specifies a word

(defn characteristic-masks-for-numbers
  "pdc is partial decoded clue which is a vector of
  characters and numbers. The  masks are the
  characteristic functions for each distinct number"
  [pdc]
  (let [dv (distinct (filter number? pdc))
        mf (partial mask-from-values pdc)]
    (map mf dv)))

(defn characteristic-masks-for-chars
  "pdc is partial decoded clue which is a vector of
  characters and numbers. The  masks are the
  characteristic functions for each distinct char"
  [pdc]
  (let [dv (distinct (filter char? pdc))
        mf (partial mask-from-values pdc)]
    (map mf dv)))


(defn count-filter-from-characteristic-mask
  "Used for checking that all letters with same code number are the same"
  [cm]
  (let [usethis? (fn [word] (->> word (applymask cm) set count (= 1)))]
    (filter usethis?)))

(defn comp-count-filter-from-characteristic-masks
  [cms]
  (apply comp (map count-filter-from-characteristic-mask cms)))

(defn value-filter-from-characteristic-mask
  "Used for checking specified letters are correct
  mask must have ony chars and nils"
  [cm]
  (let
    [spl (filter char? cm)
     usethis? (fn [word] (->>
                           word
                           (applymask cm)
                           (= spl)))]
    ;(println spl)
    (filter usethis?)))

(defn comp-value-filter-from-characteristic-masks
  [cms]
  (apply comp (map value-filter-from-characteristic-mask cms)))

(defn characteristic-mask-for-distinct
  [pdc]
  (let [values-that-should-be-distinct
        (distinct (filter #(or
                            (char? %)
                            (and (> % 0)
                                 (< % 27)))
                          pdc))]
    (mask-from-values pdc values-that-should-be-distinct)))

(defn distinct-letters-filter
  [pdc]
  (let [dmask (characteristic-mask-for-distinct pdc)
        ndistinct  (count (distinct (remove nil?  dmask)))
        usethis? (fn [word] (->> word (applymask dmask) set count (= ndistinct)))]
    (filter usethis?)))

(defn sub-word-filter
  "selects if subword given by mask is also a word"
  [smask]
  (let [usethis? (fn [word] (->> word (applymask smask) (str/join "")  all-words-in-set))]
    (filter usethis?)))

(defn letter-to-use-filter
  [smask lettouse]
  (let [usethis? (fn [word] (as-> word arg
                                  (applymask smask arg)
                                  (set arg)
                                  (set/difference arg (set lettouse))
                                  (empty? arg)))]
    (filter usethis?)))

;TODO must be a better way to code this
; might not need as can use applymask-till-match
(defn simple-masks-from-characteristic-masks
  "cms is list of characteristic masks
  produces list of simple masks.
  A simple mask has true at first occurence of the distinct code"
  [cms]
  (let [;cmf (fn [cm] (map #(= true %) (next (reductions #(if %1 "F" %2) false cm))))
        ;cmf (fn f [cm]
        ;      (let [fir (first cm)
        ;            res (rest cm)]
        ;        (if fir
        ;          (cons fir (repeat (count res) false))
        ;          (cons fir (f res)))))
        cmf (fn [cm]
              (let [sp (split-with false? cm)
                    fir (first sp)
                    lst (last sp)]
                (concat fir '(true) (repeat (dec (count lst)) false))))]
    (map cmf cms)))


(defn filter-from-simple-mask
  "Used for checking that all letters with a given code number are
  in the correct set of letters to use"
  [sm lettouse]
  (let [goodset (set lettouse)
        using-correct-letters? (fn [word] (->> word (applymask sm) goodset))]
    (filter using-correct-letters?)))

;TODO need to know lettouse for each different mask
(defn comp-filter-from-simple-masks
  [sms]
  (apply comp (map filter-from-simple-mask sms)))

;TODO need to make or of masks corresponding to 1...26
; gen filter that tests cardnality of applymask being same as number of trues in mask



;TODO using dictionary as set seems fastest
(defn filter-from-partial-decoded-clue
  "creates filter transducer"
  [pdc]
  (let [n (count pdc)]
    (comp
      ; TODO arrange filters with most restrictive first so will run fastest
      ; selects words of correct length
      (filter #(= n (count %)))
      ; selects words with correct letters associated with char in pdc
      (comp-value-filter-from-characteristic-masks
          (characteristic-masks-for-chars pdc))
      ; selects words with number codes well defined i.e. corresponding to unique letter
      (comp-count-filter-from-characteristic-masks
          (characteristic-masks-for-numbers pdc))
      ;TODO select words with number codes being in specified sets
      ;TODO select words with specified subseq in dictionary
      ; selects words with the correct number of distinct letters (chars and num 1 ..27
      (distinct-letters-filter pdc))))



(defn filteredlist
  "filters words (set or list but set is fastest) default is all-words-in-set"
  ([filter-transducer]
   (filteredlist filter-transducer all-words-in-set))
  ([filter-transducer words]
   (sequence filter-transducer words)))


(comment
  (filteredlist (filter-from-partial-decoded-clue [1 2 3 2 1]))
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [1 2 3 4 5])
                  (letter-to-use-filter [1  1 1 1 1] "ybcdfghjklmnpqrstvwxz")))
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [31 32 33 34 35])
                  (letter-to-use-filter [1  1 1 1 1] "ybcdfghjklmnpqrstvwxz")))
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [31 32 33 34 35])
                  (sub-word-filter [1 1 1 nil nil])
                  (sub-word-filter [nil 1 1 1 nil])
                  (sub-word-filter [nil nil 1 1 1])))
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [31 32 33 34 35])
                  (letter-to-use-filter [1 nil nil nil nil] "abc")
                  (letter-to-use-filter [nil nil nil nil 1]  "xyz")
                  (sub-word-filter [nil 1 1 1 nil])))

  (quick-bench (find-all-words [1 2 3 2 1] {})) ;22 ms
  (quick-bench (filteredlist (filter-from-partial-decoded-clue [1 2 3 2 1])))) ;4 ms


(defn sorted-single-letter-clues
  "Find all numbers occuring in a clue and how many clues it appears in
  Returns list of single number clues for these and the count"
  [cc]
  (let [clues (:clues cc)
        sortedpairs (sort
                      #(> (last %1) (last %2))
                      (remove
                        #(zero? (last %))
                        (map
                          (fn [n] [n (count (keep (fn [clue] ((set clue) n)) clues))])
                          (range 1 27))))
        singleclues (map #(seq [(first %)]) sortedpairs)
        numinothers (map #(last %) sortedpairs)]
    ;(println singleclues)
    ;(println numinothers)
    ;singleclues
    {:singleclues singleclues :numinothers numinothers}))

(defn letpat-filter
  [letuse]
  (if
    (= letuse "abcdefghijklmnopqrstuvwxyz") #"."
                                            (str "[" letuse "]")))

(defn partial-decoded-code-vec-to-regexpat-filter
  "Given partially decoded code cracker clue and new encoding map
  and letter to use produce the regex used in filtercode
  code vals 0 and >26 are treated as free"
  [partial-decoded-code-vec new-map letuse]
  (let [cleanletpat (letpat-filter(clean-letuse letuse (vals new-map)))
        freeletpat  (letpat-filter(clean-letuse ""))
        v1 (reduce (fn [res input]
                     (conj res
                           (cond
                             ((set "abcdefghijklmnopqrstuvwxyz") input) \.
                             (or (zero? input) (> input 26)) input
                             ((set res) input) \.
                             :else input)))
                   [] partial-decoded-code-vec)
        v2 (replace new-map v1)
        v3 (reduce (fn [res input]
                     (conj res
                           (cond
                             (number? input)(if (or (zero? input) (> input 26))
                                              freeletpat
                                              cleanletpat)
                             :else input)))
                   [] v2)
        v3 (re-pattern (str/join "" v3))]
    ;(println partial-decoded-code-vec new-map letuse)
    ;(println cleanletpat freeletpat v1 v2)
    v3))


;TODO  in filtercode wanted to use transducer but cant see how to use it to keep a lazy seq
; tried with filtercode producing (remove #(nil? (re-matches repat %))) called it ftran
; tried (transduce ftran (completing #(cons %2 %1)) [] (lazy-seq ["texxx" "abcde" "te" "atexj" "te;lk"]))
; but have found now way to keep a lazy sequence
;TODO the code below must be able to be shortened

(defn filtercode-using-regex
  ""
  [alm nlm otherclue otherwords]
  (let [repat (partial-decoded-code-vec-to-regexpat-filter (decode-to-vec otherclue alm) nlm "")]
    (filter #(re-matches repat %) otherwords)))

; using transducers
(defn filtercode
  ""
  [alm nlm otherclue otherwords]
  (let [freelet  (clean-letuse "")
        cleanlet (clean-letuse "" (vals nlm))
        diffm (apply dissoc nlm (keys alm))
        newchm (decode-to-vec otherclue diffm)
        pdc (decode-to-vec otherclue nlm)]
    (println cleanlet diffm newchm pdc)
    (filteredlist (comp
                    ;(filter-from-partial-decoded-clue pdc)
                    (comp-value-filter-from-characteristic-masks
                      (characteristic-masks-for-chars newchm))
                    (letter-to-use-filter (mask-for-constrained-clues pdc) cleanlet))
                  otherwords)))

(defn numinother-score
  [decodedclue oldnuminother]
  (if (or (> (count decodedclue) 1) (apply number? decodedclue)) oldnuminother 3))

(defn simple-scores
  "completed clues give 3 if in dictionary, 2 if not in dictionary,
   give 1 if no words match uncompleted clue
   or by ratio of  number of different letters needed to complete less 1
   over total length so zero if one letter left always < 1 if uncompleted"
  [decodedclue wordlist]
  (let [wordlistempty? (empty? (take 1 wordlist))
        numlettersleft (->>
                         decodedclue
                         (filter number?)
                         (distinct)
                         (count))]
    (if (zero? numlettersleft)
      (if wordlistempty? 2 3)
      (if wordlistempty?
        1
        (->>
          numlettersleft
          (dec)
          (* (/ (count decodedclue))))))))

;TODO is there an optimal maxcnt? with 4x4 empty grid example
;TODO 30 was much faster than 10 or 100000
(defn score-using-bounded-wordcount
  "completed clues give 3 if in dictionary, 2 if not in dictionary,
  give 1 if no words match uncompleted clue
  give 1 - 1/(number of words found upto maxcnt = 10)
  so denominator is min number of matches and 10
  in particular gives 0 if exactly 1 word matches uncompleted clue"
  [wordlist simplescore]
  (let [maxcnt 30
        boundedwordlistsize (count (take maxcnt wordlist))]
    (if (>= simplescore 1)
      simplescore
      (inc (- (/ boundedwordlistsize))))))

;TODO make these all be single concepts and then combine with and or etc
(defn non-completed-some-could-be-bad?
  [cc]
  ;(some #(< % 1) (:simplescores cc)))   ; this means some non-completed or 1 letter left but not completable
  (some #(<= % 1) (:wordcountscores cc)))                   ; allows non-completed-bad-clues

(defn non-completed-and-all-good?
  [cc]
  (and
    (some #(< % 1) (:simplescores cc))
    (every? #(and (not= % 1) (not= % 2)) (:wordcountscores cc)))) ;some not completed but no  bad clues either completed or not

(defn all-completed-and-all-good?
  [cc]
  (every? #(= % 3) (:wordcountscores cc)))                  ; all completed with good words

(defn all-completed?
  [cc]
  (every? #(> % 1) (:simplescores cc)))                     ; all completed

(defn all-completed-or-not-completable?
  [cc]
  (every? #(>= % 1) (:wordcountscores cc)))

(defn all-good?
  [cc]
  (every? #(or (< % 1) (= % 3)) (:wordcountscores cc)))     ; all good words

(defn set-remaining-keys
  "Sets values for keys :wordlists :partialwords :decodedclues :simplescores :wordcountscores"
  [cc]
  (when (not (:depth cc)) (println "Warning set-remaining-keys got cc with no depth. Setting up root?"))
  (let [dotmap (zipmap (range 1 27) (repeat 27 \.))
        decodedclue-vecs (map #(decode-to-vec % (:encodemap cc)) (:clues cc))
        partialwords (map #(str/join "" (replace dotmap %)) decodedclue-vecs)
        simplescores (map simple-scores decodedclue-vecs (:wordlists cc))
        wordcountscores (map score-using-bounded-wordcount (:wordlists cc) simplescores)
        numinothers (map numinother-score decodedclue-vecs (:numinothers cc))
        completed (if (every? #(> % 1) wordcountscores) "complete" "unfinished")
        allgood (if (every? #(or (< % 1) (= % 3)) wordcountscores) "all good" "some bad")]
    (merge cc
           {:partialwords    partialwords
            :decodedclues    decodedclue-vecs
            :simplescores    simplescores
            :wordcountscores wordcountscores
            :numinothers     numinothers
            :completed       completed
            :allgood         allgood})))

;TODO ? use merge to handle default params
(defn make-root
  "makes root from cc with :encodemap rootmap
  or (:encodemap cc) if exists else {}
  adds single letters as clues if asl true"
  [{cc :ccinfo rootmap :rootmap asl :addsingleletters}]
  (let [clues (:clues cc)
        rootmap (or rootmap (merge {}(:encodemap cc)))
        singleletterclues (if asl (sorted-single-letter-clues cc)
                                  {:singleclues '() :numinothers '()})
        numinothers (concat (map #(/ %)(singleletterclues :numinothers)) (repeat (count clues) 1))
        augmentedclues (concat  (singleletterclues :singleclues) clues)
        wordlists (map #(find-all-words % rootmap) augmentedclues)
        rmap (merge cc
                     {:clues augmentedclues
                      :numinothers numinothers
                      :encodemap  rootmap
                      :wordlists  wordlists
                      :depth      0})]
     ;(println clues)
     ;(println augmentedclues)
     (set-remaining-keys rmap)))

(defn update-assigned-letters-map
  [assigned-letters-map clue word-found]
  (merge assigned-letters-map (zipmap clue word-found)))


(defn make-child
  [cc clue word]
  (let [newmap (update-assigned-letters-map (:encodemap cc) clue word)
        ;newwords (map #(filtercode (:encodemap cc) newmap %1 %2) (:clues cc) (:wordlists cc))
        newwords (map #(filtercode-using-regex (:encodemap cc) newmap %1 %2) (:clues cc) (:wordlists cc))
        updatedcc (merge cc {:encodemap newmap,
                             :wordlists newwords
                             :parent    cc
                             :depth     (inc (:depth cc))
                             :clue      clue
                             :word      word})]
    ;(println (inc (:depth cc)) (:encodemap cc) clue word newmap)
    ;(println (map #(take maxn %) (:wordlists cc)))
    ;(println (map #(take maxn %) newwords)) ; if this is uncommented will
    (set-remaining-keys updatedcc)))

(defn children-from-clue
  "Make list of children from clue of given index using all words in its wordlist
  Note orderof list correponds to reverse order of words"
  [cc clue-index]
  (reduce #(conj %1 (make-child cc (nth (:clues cc) clue-index) %2)) (lazy-seq) (nth (:wordlists cc) clue-index)))

(defn children-from-clue-indicies
  "take list of indicies of non-completed-good-clues and combines all their children"
  [cc clue-indicies]
  ;(println clue-indicies (:depth cc) (:wordcountscores cc))
  (reduce #(concat %1 (lazy-seq (children-from-clue cc %2))) (lazy-seq) clue-indicies))

(defn children-from-top-clues
  "takes children of the kbest best clues wordcountscores.
  wordcountscores are sorted, so order of duplicates not determined.
  If kbest > 1 will get get duplicate solutions. e.g. using clue1 then clue2
  will produce same sols as using clue2 then clue1.
  e.g. to use this in a tree-seq call (partial children-from-top-clues 3)
  Note (partial children-from-top-clues 1) is slower than children-from-best-clue"
  [kbest cc]
  (let [cm (zipmap (range (count (cc :clues))) (cc :wordcountscores))
        ;scm (sort #(< (last %1) (last %2)) cm)
        scm (sort-by last cm)                               ; better than above
        sind (keys scm)]
    (children-from-clue-indicies cc (take kbest sind))))


(defn children-from-best-clue
  "takes children of the best index using key (:wordcountscores, :simplescores, ...)
  since 0 is smallest possible  scan indices keeping track of min found
  but stop if find 0"
  [key cc]
  (let [bind (first (reduce-kv
                       (fn [acc ind val]
                         (cond
                           (zero? val) (reduced [ind val])
                           (> (last acc) val) [ind val]
                           :else acc))
                       [nil 4]  ; 4 is bigger than any wordcountscores
                       (vec (cc key))))]
     (children-from-clue cc bind)))


(defn children-from-best-clue-using
  [key]
  (partial children-from-best-clue key))


(defn nice-print
  "cclist is list of cc found using children, looks at index n"
  [cclist n]
  (let [cc (nth cclist n)]
    (println (cc :encodemap))
    (map #(println %1 %2 %3 %4)
         (cc :simplescores)
         (cc :clues)
         (cc :decodedclues)
         (cc :wordcountscores))))


(defn make-example-for-work
  "Breaks sentence into lower case words and encodes via 1 \\a, 2 \\b ...
   to get list of clues which are used to solve.
   Produces root with {} encodemap"
  [sentence]
  (let [clues (map encode (str/split (str/lower-case sentence) #" +"))
        cc {:clues clues}]
    ;(println clues)
    (make-root {:ccinfo cc :rootmap {}})))

(defn make-example-from-clues
  [clues]
  (let [cc {:clues clues}]
    (make-root {:ccinfo cc :rootmap {}})))



(defn printcctable
  "prints table with each row being a the partial word, it's wordcount, simplescore and clue of cc
  colored blue for most recent clue used, yellow for completed word not in dictionary, red for non-completible partial word,
  green for completible partial word, black for completed word in dictionary"
  [cc]
  (let [word (:word cc)
        rows (map #(zipmap [:clue :partialword :wordcountscore :simplescore :numinother] [%1 %2 %3 %4 %5])
                  (:clues cc) (:partialwords cc) (:wordcountscores cc) (:simplescores cc) (:numinothers cc))
        rows (conj rows {:clue "clue" :partialword "part" :wordcountscore "wcount" :simplescore "simplesc" :numinother "numino"})
        select-n-color (fn [key row]
                         (let [wc (:wordcountscore row)
                               val (key row)]
                           (cond
                             (= word val) (ioa/bold-blue val) ; cant be resolved as defined by macro
                             (= wc 2) (ioa/bold-yellow val)
                             (string? wc) (ioa/bold-black val) ; for header line
                             (< wc 1) (ioa/bold-green val)
                             (= wc 1) (ioa/bold-red val)
                             :else (ioa/bold-black val))))]
    (col/write-rows
      *out*
      [(partial select-n-color :partialword) " " (partial select-n-color :wordcountscore) " "
       (partial select-n-color :simplescore) " " (partial select-n-color :numinother) " " [(partial select-n-color :clue) :left]]
      rows)))

(defn printinfoencoding
  [n cc]
  (let [em (cc :encodemap)
        nmax (inc (apply max (conj (keys em) 26)))]
    (println n (map #(% cc) [:depth :completed :allgood :clue :word]))
    (doall (map #(if (em %) (printf (str ioa/bold-blue-font "%3s" ioa/reset-font) %) (printf "%3s" %)) (range 1 nmax)))
    (println)
    (doall
      (map #(if (em %) (printf (str ioa/bold-blue-font "%3s" ioa/reset-font) ((cc :encodemap) %)) (printf "%3s" ".")) (range 1 nmax)))
    (println)))


(defn show-from-root
  [ans]
  (let [depth (:depth ans)
        gen (take (inc depth) (iterate #(:parent %) ans))]
    (doseq [n (range depth -1 -1)]
      (printinfoencoding (- depth n) (nth gen n))
      (printcctable (nth gen n)))))

(defn show-at-most-n
  "times and shows info about up to nmax solutions in ans"
  [ans nmax]
  (let [sols (take nmax ans)
        nshow (count sols)]
    (doseq [n (range nshow)]
      (printinfoencoding n (nth ans n))
      (printcctable (nth ans n)))))


;TODO clarify how to set up root and how to use tree-seq
(comment
  (def root (make-example-for-work "the quick brown fox jumps over the lazy level dog")) ; 96 solutions
  (def root (make-example-for-work "abcd bcde cdef  abcdef")) ; hone ones nest honest ... 12 solutions
  (def root (make-example-for-work "abcdefg bcdef cde")) ;tragedy raged age ...pirates irate rat ...phoneys honey one ..20 sols
  (def root (make-example-for-work "abcdefg fedcb")) ; deviant naive ..10 sols
  (def root (make-example-for-work "now is the time for all good men to come to the aid of their party")) ; lots of solutions
  (def root (make-example-for-work "Jived fox nymph grabs quick waltz")) ; might take long time to verify this
  (def root (make-example-for-work "abc cdef fghij jklmna")) ; lots of solutions 0 ... 5431
  (def root (make-example-for-work "abc defg hijkl mnopqr stu")) ; solutions 0 ...
  (def root (make-example-for-work "level"))                ;13 solutions
  (def root (make-example-for-work "level kayak"))          ; disjoint palendromes so will also have kayak level
  (def root (make-example-for-work "abc def ghi adg beh cfi")) ; 3x3 grid of distinct letters
  (def root (make-example-for-work "abcd efgh ijkl mnop aeim bfjn cgko dhlp")) ; 4x4 grid of distinct letters
  ; using fastest below found 2 solutions in just under 10 minutes!, one the transpose matrix of the other
  ; then (show-at-most-n ans 2)
  ;0 (7 complete all good [13 14 15 16] sumo (newt achy grip sumo nags ecru whim typo) (3 3 3 3 3 3 3 3))
  ;1 (7 complete all good [4 8 12 16] sumo (nags ecru whim typo newt achy grip sumo) (3 3 3 3 3 3 3 3))
  ; "Elapsed time: 64308.210487 msecs" (maxcnt = 30) in score-using-bounded-wordcount
  ; "Elapsed time: 478321.65516 msecs" (maxcnt = 100000)
  ;   61 sec more if ask for 3 sols
  ; but if (show-at-most ans 3) will find the two thus proves no other sols but time -
  ;"Elapsed time: 178874.157181 msecs" (maxcnt = 30)
  ;"Elapsed time: 547677.413145 msecs"       maxcnt = 10 in score-using-bounded-wordcount
  ;"Elapsed time: 483433.413157 msecs"       maxcnt = 100000

  (def root (make-root {:ccinfo root :rootmap {1 \n 4 \s 13 \t 16 \o}}))       ; with hint
  ;"Elapsed time: 60.3222 msecs" Liz solved this in a few hours)

  (def root (make-example-for-work "abcde fghij klmno pqrst uvwxy afkpu bglqv chmrw dinsx ejoty")) ; 5x5 grid of distinct letters
  ; nil "Elapsed time: 480560.109286 msecs" so no 5x5 of distinct letters verified in 8 minutes (maxcnt=10)

  ;fastest
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (children-from-best-clue-using :wordcountscores) root)))
  ;slower
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (partial children-from-top-clues 1) root)))
  (nice-print ans 0)                                        ;
  (show-at-most-n ans 10)
  (show-from-root (nth ans 0))                              ; show chain from root
  (str/join " " (:partialwords (nth ans 0)))                ; show just partialwords
  ; if want to examine complete tree traversal
  (def root (make-example-for-work "pas pals clap sap lap slap claps pal")) ; 4 sols big tree
  (def root (make-root {:ccinfo root :rootmap {16 \p 1 \a}}))                  ; 2 sols little tree
  (def ans (tree-seq non-completed-and-all-good? (children-from-best-clue-using :wordcountscores) root))
  (show-at-most-n ans 100))

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



(comment
  ; set up root using assigned encodemap
  (def ccnumber 43)                                         ;3 has bad word, 4 has no bad words, 43 has a few bad words
  (def root (make-root {:ccinfo (get-cc ccnumber)}))
  ; set up root with singleletter clues added
  (def root (make-root {:ccinfo (get-cc ccnumber) :addsingleletters true}))
  ; set up root overwritting assigned encodemap and using {}
  (def root (make-root {:ccinfo (get-cc ccnumber) :rootmap {}}))
  ; will solve code cracker even if has a few bad words but may find other partial sols with some badwords
  ;  43 with "root" encoding is interesting and will almost solve, but not with override {}
  (def ans (filter
             (complement non-completed-some-could-be-bad?)
             (tree-seq non-completed-some-could-be-bad? (children-from-best-clue-using :wordcountscores) root)))
  (def ans (filter
             all-completed-or-not-completable?
             (tree-seq non-completed-some-could-be-bad? (children-from-best-clue-using :wordcountscores) root)))
  ;fail for 43, work for 3
  (def ans (filter
             (complement non-completed-some-could-be-bad?)
             (tree-seq non-completed-some-could-be-bad? (children-from-best-clue-using :simplescores) root)))
  ;finds a few 'solutions'  for 43, 3
  (def ans (filter
             all-completed-or-not-completable?
             (tree-seq non-completed-some-could-be-bad? (children-from-best-clue-using :simplescores) root)))
  (def ans (filter
             all-completed?
             (tree-seq non-completed-some-could-be-bad? (children-from-best-clue-using :wordcountscores) root)))
  ; will not solve if has bad word and will find nothing
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (children-from-best-clue-using :wordcountscores) root)))
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (children-from-best-clue-using :simplescores) root)))
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (partial children-from-top-clues 1) root)))
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (children-from-best-clue-using :numinothers) root)))
  ; will not solve if has bad word and will stop with partial sol when get a bad uncomplete word
  (def ans (filter
             (complement non-completed-and-all-good?)
             (tree-seq non-completed-and-all-good? (children-from-best-clue-using :wordcountscores) root)))
  (show-at-most-n ans 1)
  (show-from-root (nth ans 0)))                             ; show chain from root

;TODO
(comment ; finding some sols to last clue that are not in dic! i.e. nen
  (def root (make-example-from-clues [[30 31 32] [33 31 34] [30 34 32] [32 34 32]]))
  (def root (make-example-from-clues [[27] [28]])) ; not allowing both to be same???
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (children-from-best-clue-using :wordcountscores) root))))



; parinfer bug?
(defn foo
  "try comment out dec with ; then add (println) after it"
  [x]
  (let
    [f (fn [y] (->>
                 x
                 (+ y)
                 dec))]
    (println x)
    f))

(defn test-sol
  [root]
  (let [ans (filter
              all-completed-and-all-good?
              (tree-seq non-completed-and-all-good? (children-from-best-clue-using :wordcountscores) root))]
    (show-at-most-n ans 10)))

(defn bench-test-sol
  [root]
  (let [ans (doall (filter
                     all-completed-and-all-good?
                     (tree-seq non-completed-and-all-good? (children-from-best-clue-using :wordcountscores) root)))]))


;TODO in make-child filtercode using reg-ex 71 ms, using transducers 232.ms
