(ns code-cracker-puzzle.work
    (:gen-class)
    (:require [code-cracker-puzzle.bill-utils :refer :all]
              [code-cracker-puzzle.data-assembly :refer :all]
              [code-cracker-puzzle.output-routines :refer :all]
              [clojure.string :as str]
              [clojure.set :as set]
              [clojure.walk]
              [clojure.repl :refer :all]
              [criterium.core :refer [quick-bench]]))

; why do I need require clojure.repl here?
; ANS in project.clj ns desiginated by :main will get it.


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

;TODO remove restriction that a code can have at most 8 distinct code numbers
(defn make-code-cracker-vector
  "Take decoded vector and convert.
  The vector should consist of pos integers and characters
  1 ... 26 correspond to a permutation of the alphabet
  code numbers > 26 go to F meaning free"
  [decoded-vec]
  (let [freemarked (map (fn [v] (if (and (number? v) (> v 26)) \F v)) decoded-vec)
        distinct-range-numbers (distinct (filter number? freemarked))
        temp-map (zipmap distinct-range-numbers (range 1 (inc (count distinct-range-numbers))))]
    ;(println decoded-vec freemarked  temp-map)
    (replace temp-map freemarked)))

(defn make-code-cracker-pat
  "Take decoded vector and convert to code-cracker-pat string
  The vector should consist of pos integers and characters
  1 ... 26 correspond to a permutation of the alphabet
  code numbers > 26 go to F meaning free"
  [decoded-vec]
  (str/join "" (make-code-cracker-vector decoded-vec)))

(defn letpat
  "part of regex to find group matching letuse"
  [letuse]
  (if
    (= letuse "abcdefghijklmnopqrstuvwxyz") #"(\w)"
                                            (str "([" letuse "])")))
(defn letpat-free
  "part of regex to find match to letuse"
  [letuse]
  (if
    (= letuse "abcdefghijklmnopqrstuvwxyz") #"\w"
                                            (str "[" letuse "]")))

(defn pat-for-new
  "Gives pattern for first use of letter n 1...8
  restricted as named groups only single digit"
  [n letuse]
  (if
    (= n 1) (letpat letuse)
            (str "(?!\\" (str/join "|\\" (range 2 (inc n))) ")" (letpat letuse))))

(defn pat-for-old
  "Gives pattern for subsequent use of letter n 1...8
     second param ignored but want to keep same arg list as pat-for-new"
  [n & letuse]
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
    (= char-from-pat \F) (letpat-free (clean-letuse ""))    ; any letter
    ;(= char-from-pat \F) (letpat-free letuse) ;   those not assigned
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
  F means free, lower case chars mean match that letter.
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
     char-pat (first code-cracker-pat)                      ; not work if code-cracker-pat is vec
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

;TODO Fails for clues with repeated free letters
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
;         numbers above 26 correspond to arbitrary letters so 29 can be any letter
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

(defn mask-for-char-clues
  "will select all codes that are chars"
  [pdc]
  (let [cv (filter char? pdc)]
    (mask-from-values pdc cv)))

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
        ndistinct (count (distinct (remove nil? dmask)))
        usethis? (fn [word] (->> word (applymask dmask) set count (= ndistinct)))]
    (filter usethis?)))

(defn sub-word-filter
  "selects if subword given by mask is also a word"
  [smask]
  (let [usethis? (fn [word] (->> word (applymask smask) (str/join "") all-words-in-set))]
    (filter usethis?)))

(defn contained-letters-clue-filter
  "clclue must be a clue using only numbers found within clue
  selects if the word specified by clclue is a word
  note: will always reject word  if clclue has a number NOT within clue"
  [clue clclue]
  (let [usethis? (fn [word] (->> clclue (replace (zipmap clue word)) (str/join "") all-words-in-set))]
    (filter usethis?)))

(defn contained-all-but-one-letters-clue-filter
  "otherclue must be a clue using only numbers found within clue plus one more
  selects word if partially decoded otherclue has matches"
  [clue otherclue]
  (let [usethis? (fn
                   [word]                                   ;word will be in wordlist associated with clue
                   (let [nmap (zipmap clue word)
                         pdc (replace nmap otherclue)
                         num (distinct (filter number? pdc)) ; will have one number
                         codenum (nth num 0)
                         newcharsfromconstrained (keep nmap (range 1 27)) ;chars added correponding to constrained letters
                         cleanlet (clean-letuse "" newcharsfromconstrained)
                         freelet (clean-letuse "")
                         letuse (if (> codenum 26) freelet cleanlet)
                         testset (reduce #(conj %1 (str/join "" (replace {codenum %2} pdc))) #{} letuse)]
                     (seq (set/intersection testset all-words-in-set))))]
    (filter usethis?)))

(comment
  (defn f
    [word]                                                  ;word will be in wordlist associated with clue
    (let [clue [1 2 3 4]
          otherclue [1 2 35 35]
          nmap (zipmap clue word)
          pdc (replace nmap otherclue)
          num (distinct (filter number? pdc))               ; will have one number
          codenum (nth num 0)
          newcharsfromconstrained (keep nmap (range 1 27))  ;chars added correponding to constrained letters
          cleanlet (clean-letuse "" newcharsfromconstrained)
          freelet (clean-letuse "")
          letuse (if (> codenum 26) freelet cleanlet)
          testset (reduce #(conj %1 (str/join "" (replace {codenum %2} pdc))) #{} letuse)]
      (println pdc num letuse testset))))

;TODO want intersecting clues filter as a more general case of sub-word-filter
;   e.g. [1 2 3 4 5] [4 3 2]   or [30 31 32 33 34] [30 32 34 34] [33 31 30] i.e have one long clue and others using some
;         of the same numbers
;   but maybe groups of clues like [1 2 3 4 5] [2 3 4 10 10]

(defn letter-to-use-filter
  [smask lettouse]
  (let [usethis? (fn [word] (as-> word arg
                                  (applymask smask arg)
                                  (set arg)
                                  (set/difference arg (set lettouse))
                                  (empty? arg)))]
    (filter usethis?)))

(defn letter-to-avoid-filter
  [smask lettoavoid]
  (let [usethis? (fn [word] (as-> word arg
                                  (applymask smask arg)
                                  (set arg)
                                  (set/intersection arg (set lettoavoid))
                                  (empty? arg)))]
    (filter usethis?)))


;TODO must be a better way to code this
; might not need as can use applymask-till-match

(defn simple-mask-from-characteristic-mask
  "returns mask up to  first truthy in cm padded by nil
  to be used with cm produced from for one number in clue "
  [cm]
  (let [sp (split-with (complement identity) cm)
        fir (first sp)
        lst (last sp)]
    (println sp fir lst)
    (concat fir [(first lst)] (repeat (dec (count lst)) nil))))


; (simple-masks-from-characteristic-masks (characteristic-masks-for-numbers [10 20 10 40 40]))
(defn simple-masks-from-characteristic-masks
  "cms is list of characteristic masks
  produces list of simple masks.
  cms produced by characteristic-masks-for-numbers
  A simple mask has true at first occurence of the distinct code"
  [cms]
  (map simple-mask-from-characteristic-mask cms))


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
      ;; selects words with correct letters associated with char in pdc
      ;(comp-value-filter-from-characteristic-masks
      ;    (characteristic-masks-for-chars pdc))
      ; selects words with correct letters associated with char in pdc.  faster  than above
      (value-filter-from-characteristic-mask (mask-from-values pdc (filter char? pdc)))
      ; selects words with number codes well defined i.e. corresponding to unique letter
      (comp-count-filter-from-characteristic-masks
        (characteristic-masks-for-numbers pdc))
      ;TODO select words with number codes being in specified sets
      ;TODO select words with specified subseq in dictionary
      ;TODO select words with other combinations of letters in clue being words
      ; selects words with the correct number of distinct letters (chars and num 1 ..27
      (distinct-letters-filter pdc))))



(defn filteredlist
  "filters words default is all-words-in-set
  words can be set or list, set is faster, but list maintains alphabetic order"
  ([filter-transducer]
    ;(filteredlist filter-transducer all-words-in-set)
   (filteredlist filter-transducer all-words-in-lazy-seq))
  ([filter-transducer words]
   (sequence filter-transducer words)))

;TODO very slow
; should use new-map like filtercode
; if pdc has only char contained-letters-clue-filter much faster
; if pdc has only one non char maybe try all possible values for that char to test
(defn other-clue-filter
  "to be applied to word list associated with clue
  selects word if partially decode otherclue has matches"
  [clue otherclue]
  (let [usethis? (fn
                   [word]                                   ;word will be in wordlist associated with clue
                   (let [nmap (zipmap clue word)
                         pdc (replace nmap otherclue)
                         filtr (filter-from-partial-decoded-clue pdc) ; filter only wordlist for otherclue
                         matches (filteredlist filtr)]
                     (seq matches)))]                       ;(seq empty) is nil=falsey (seq non-empty) is truthy
    (filter usethis?)))


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

(defn replace-duplicates
  [v r]
  (reduce-kv (fn [res ind input]
               (conj res
                     (cond
                       ((set (take ind v)) input) r
                       :else input)))
             [] (vec v)))

(defn constant-map
  [keys const]
  (zipmap keys (repeat (count keys) const)))


(defn partial-decoded-code-vec-to-regexpat-for-filter
  "Given partially decoded code cracker clue and new encoding map extending the one that
  partially decoded the clue
  and letuse to use produce an efficient regex used in filtercode-using-regex
  code vals > 26 are treated as free"
  [partial-decoded-code-vec new-map letuse]
  (let [mdot (constant-map partial-decoded-code-vec \.)
        nodup (replace-duplicates partial-decoded-code-vec \.)
        ;newchars (vals new-map)
        newcharsfromconstrained (keep new-map (range 1 27)) ;chars added correponding to constrained letters
        cleanletpat (letpat-filter (clean-letuse letuse newcharsfromconstrained))
        freeletpat \. ;(letpat-filter (clean-letuse ""))
        ;char-in-nodup (filter char? nodup)
        num-in-clue (filter number? partial-decoded-code-vec)
        free-in-clue (filter #(> % 26) num-in-clue)
        rmap (merge mdot  (constant-map num-in-clue cleanletpat) (constant-map free-in-clue freeletpat) new-map)
        v1 (replace rmap nodup)]
    ;(println nodup rmap v1)
    (re-pattern (str/join "" v1))))


;TODO the code below must be able to be shortened
; This is a bit slower than old version v0
(defn partial-decoded-code-vec-to-regexpat-for-filter-v1
  "Given partially decoded code cracker clue and new encoding map
  and letuse to use produce an efficient regex used in filtercode-using-regex
  code vals > 26 are treated as free"
  [partial-decoded-code-vec new-map letuse]
  (let [;newchars (vals new-map)
        newcharsfromconstrained (keep new-map (range 1 27)) ;chars added correponding to constrained letters
        cleanletpat (letpat-filter (clean-letuse letuse newcharsfromconstrained))
        freeletpat \. ;(letpat-filter (clean-letuse ""))
        rfn (fn [input]
              (cond
                (number? input)(if (> input 26)
                                 freeletpat
                                 cleanletpat)
                :else input))
        v1 ; replace chars and duplicates with .
        (reduce-kv (fn [res ind input]
                     (conj res
                        (cond
                          ((set "abcdefghijklmnopqrstuvwxyz") input) \.
                          ;(> input 26) input
                          ; if remove checking for duplicates marginally faster and using filter about same ???
                          ((set (take ind partial-decoded-code-vec)) input) \.
                          :else (rfn (new-map input input)))))
                   [] (vec partial-decoded-code-vec))
        v3 (re-pattern (str/join "" v1))]
    ;(println partial-decoded-code-vec new-map letuse)
    ;(println cleanletpat freeletpat v1)
    v3))


(defn partial-decoded-code-vec-to-regexpat-for-filter-v0
  "Given partially decoded code cracker clue and new encoding map
  and letuse to use produce an efficient regex used in filtercode-using-regex
  code vals > 26 are treated as free"
  [partial-decoded-code-vec new-map letuse]
  (let [newchars (vals new-map)
        newcharsfromconstrained (keep new-map (range 1 27)) ;chars added correponding to constrained letters
        cleanletpat (letpat-filter (clean-letuse letuse newcharsfromconstrained))
        freeletpat \. ;(letpat-filter (clean-letuse ""))
        v1 ; replace chars and duplicates with .
        (reduce (fn [res input]
                  (conj res
                        (cond
                          ((set "abcdefghijklmnopqrstuvwxyz") input) \.
                          ;(> input 26) input
                          ((set res) input) \.
                          :else input)))
                [] partial-decoded-code-vec)
        v2  ; replace new assigned codes with their letters
        (replace new-map v1)
        v3  ; replace number codes with free or clean pattern
        (reduce (fn [res input]
                  (conj res
                        (cond
                          (number? input) (if (> input 26)
                                            freeletpat
                                            cleanletpat)
                          :else input)))
                [] v2)
        v3 (re-pattern (str/join "" v3))]
    ;(println partial-decoded-code-vec new-map letuse)
    ;(println cleanletpat freeletpat v1 v2)
    v3))


(defn filtercode-using-regex
  "Assumming otherwords satisfy otherclue with assigned-letter-map alm
  filters out words that no longer satisfy otherclue using new letter map nlm"
  [alm nlm otherclue otherwords]
  (if (= alm nlm)
    otherwords
    (let [repat (partial-decoded-code-vec-to-regexpat-for-filter (decode-to-vec otherclue alm) nlm "")]
      ;(println repat)
      (filter #(re-matches repat %) otherwords))))

; TODO use helper fcn here
; basically same as partial-decoded-code-vec-to-regexpat-for-filter
(defn partial-decoded-code-vec-to-info-for-rel
  "Given partially decoded code cracker clue and new encoding map extending the one that
  partially decoded the clue
  and letuse to use produce an info needed
  code vals > 26 are treated as free"
  [partial-decoded-code-vec new-map letuse]
  (let [mdot (constant-map partial-decoded-code-vec \.)
        nodup (replace-duplicates partial-decoded-code-vec \.)
        ;newchars (vals new-map)
        newcharsfromconstrained (keep new-map (range 1 27)) ;chars added correponding to constrained letters
        cleanletpat (letpat-filter (clean-letuse letuse newcharsfromconstrained))
        freeletpat \. ;(letpat-filter (clean-letuse ""))
        ;char-in-nodup (filter char? nodup)
        num-in-clue (filter number? partial-decoded-code-vec)
        free-in-clue (filter #(> % 26) num-in-clue)
        rmap (merge mdot  (constant-map num-in-clue cleanletpat) (constant-map free-in-clue freeletpat) new-map)
        posmap (zipmap (range 1 (inc (count nodup))) (replace rmap nodup))]
    (apply dissoc posmap (filter #(= (posmap %) \.) (keys posmap)))))

(declare get-by-pos-char-map)
;TODO not finished will want otherwords to be a set and intersect with new letter at pos sets
;TODO or remove letter pos sets for those letter pos no longer available.
(defn filtercode-using-rels
  "Assumming otherwords satisfy otherclue with assigned-letter-map alm
  filters out words that no longer satisfy otherclue using new letter map nlm"
  [alm nlm otherclue otherwords]
  (if (= alm nlm)
    {}
    (let [posmap (partial-decoded-code-vec-to-info-for-rel (decode-to-vec otherclue alm) nlm "")
          posmap (merge posmap {(inc (count otherclue)) nil})]
      (println posmap)
      (get-by-pos-char-map posmap))))


; using transducers
; speed depends on type of otherwords but fastest when it is lazy-seq, however at least 100 times slower than regex
(defn filtercode
  "removes words from otherwords that no longer fit otherclue when the assigned letter map als is
  updated to a new letter map nlm"
  [alm nlm otherclue otherwords]
  (if (= alm nlm)
    otherwords
    (let [;freelet  (clean-letuse "")
          ;cleanlet (clean-letuse "" (vals nlm))
          diffm (apply dissoc nlm (keys alm))
          ;newchm (decode-to-vec otherclue diffm)
          newchars (vals diffm)
          newcharsfromconstrained (keep diffm (range 1 27)) ;chars added correponding to constrained letters
          pdc (decode-to-vec otherclue nlm)
          pdcbydiffm (decode-to-vec otherclue diffm)
          ;maskfornewchars (mask-from-values pdc newchars)
          maskfornewchars (mask-from-values pdcbydiffm newchars)
          maskforconst (mask-for-constrained-clues pdc)]
      ;(println cleanlet diffm newchm newchars pdc)
      ;(println maskfornewchars maskforconst)
      (filteredlist (comp
                      ;(filter-from-partial-decoded-clue pdc) ; not needed
                      ; selects words with correct letters associated with newchars in pdc
                      ;(value-filter-from-characteristic-mask (mask-for-char-clues pdc)) ; next fastest?
                      (value-filter-from-characteristic-mask maskfornewchars) ;fastest?
                      ;(comp-value-filter-from-characteristic-masks ;slowest
                      ;  (characteristic-masks-for-chars newchm))
                      ; selects words with constrained letters in correct set
                      ;(letter-to-use-filter maskforconst cleanlet)
                      ; selects words with constrained letters avoiding newchars should be faster
                      (letter-to-avoid-filter maskforconst newcharsfromconstrained))
                    otherwords))))

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
  "Sets values for keys :partialwords :decodedclues :simplescores :wordcountscores
   :numinothers :completed :allgood"
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
        rootmap (or rootmap (merge {} (:encodemap cc)))
        singleletterclues (if asl (sorted-single-letter-clues cc)
                                  {:singleclues '() :numinothers '()})
        numinothers (concat (map #(/ %) (singleletterclues :numinothers)) (repeat (count clues) 1))
        augmentedclues (concat (singleletterclues :singleclues) clues)
        pdaugmentedclues (map #(decode-to-vec % rootmap) augmentedclues)
        ;wordlists (map #(find-all-words % rootmap) augmentedclues)
        wordlists (doall (map #(filteredlist (filter-from-partial-decoded-clue %)) pdaugmentedclues))
        rmap (merge cc
                    {:clues       augmentedclues
                     :numinothers numinothers
                     :encodemap   rootmap
                     :wordlists   wordlists
                     :depth       0})]
    ;(println clues)
    ;(println augmentedclues)
    (set-remaining-keys rmap)))

(defn update-assigned-letters-map
  [assigned-letters-map clue word-found]
  (merge assigned-letters-map (zipmap clue word-found)))

;TODO refactor so wordlists etc. indexed by clue
;TODO may want to handle partial decoded clues that are all chars as special case
;     as wordlist can only be given clue (if it a word)

(defn make-child-rx
  [cc clue word]
  (let [newmap (update-assigned-letters-map (:encodemap cc) clue word)
        newwords (map #(filtercode-using-regex (:encodemap cc) newmap %1 %2) (:clues cc) (:wordlists cc))
        updatedcc (merge cc {:encodemap newmap,
                             :wordlists newwords
                             :parent    cc
                             :depth     (inc (:depth cc))
                             :clue      clue
                             :word      word})]
    ;(println (inc (:depth cc)) (:encodemap cc) clue word newmap)
    ;(println (map #(take maxn %) (:wordlists cc)))
    ;(println (map #(take maxn %) newwords))
    (set-remaining-keys updatedcc)))

(defn make-child-tr
  [cc clue word]
  (let [newmap (update-assigned-letters-map (:encodemap cc) clue word)
        newwords (map #(filtercode (:encodemap cc) newmap %1 %2) (:clues cc) (:wordlists cc))
        updatedcc (merge cc {:encodemap newmap,
                             :wordlists newwords
                             :parent    cc
                             :depth     (inc (:depth cc))
                             :clue      clue
                             :word      word})]
    ;(println (inc (:depth cc)) (:encodemap cc) clue word newmap)
    ;(println (map #(take maxn %) (:wordlists cc)))
    ;(println (map #(take maxn %) newwords))
    (set-remaining-keys updatedcc)))



(defn children-from-clue
  "Make list of children from clue of given index using all words in its wordlist
  Note orderof list correponds to reverse order of words"
  [make-child cc clue-index]
  (reduce #(conj %1 (make-child cc (nth (:clues cc) clue-index) %2)) (lazy-seq) (nth (:wordlists cc) clue-index)))

(defn children-from-clue-indicies
  "take list of indicies of non-completed-good-clues and combines all their children"
  [make-child cc clue-indicies]
  ;(println clue-indicies (:depth cc) (:wordcountscores cc))
  (reduce #(concat %1 (lazy-seq (children-from-clue make-child cc %2))) (lazy-seq) clue-indicies))

(defn children-from-top-clues
  "takes children of the kbest best clues wordcountscores.
  wordcountscores are sorted, so order of duplicates not determined.
  If kbest > 1 will get get duplicate solutions. e.g. using clue1 then clue2
  will produce same sols as using clue2 then clue1.
  e.g. to use this in a tree-seq call (partial children-from-top-clues 3)
  Note (partial children-from-top-clues 1) is slower than children-from-best-clue"
  [make-child kbest cc]
  (let [cm (zipmap (range (count (cc :clues))) (cc :wordcountscores))
        ;scm (sort #(< (last %1) (last %2)) cm)
        scm (sort-by last cm)                               ; better than above
        sind (keys scm)]
    (children-from-clue-indicies make-child cc (take kbest sind))))


(defn children-from-best-clue
  "takes children of the best index using key (:wordcountscores, :simplescores, ...)
  since 0 is smallest possible  scan indices keeping track of min found
  but stop if find 0. Assumes values of key sorted upon are all < 4"
  [make-child key cc]
  (let [bind (first (reduce-kv
                      (fn [acc ind val]
                        (cond
                          (zero? val) (reduced [ind val])
                          (> (last acc) val) [ind val]
                          :else acc))
                      [nil 4]                               ; 4 is bigger than any wordcountscores
                      (vec (cc key))))]
    (children-from-clue make-child cc bind)))


(defn children-from-best-clue-using
  "2-arity childeren-from-best clue make-child key
  1-arity  childeren-from-best clue make-child-rx key"
  ([make-child key]
   (partial children-from-best-clue make-child key))
  ([key]
   (partial children-from-best-clue make-child-rx key)))


; Relations - overhead to make these relations, but fast to lookup
(defn relation
  [f vs in-key out-key]
  (reduce (fn [rel inp] (conj rel {in-key inp out-key (f inp)})) #{} vs))

;TODO note the maps in the rel do not all have the same keys
(def word-letterpositions-rel
  "#{{:word \"dog\" 1 \\d 2 \\o 3 \\g 4 nil}..}"
  (reduce
    (fn [rel inp]
      (let [lenw (count inp)]
        (conj rel
              (merge
                {:word inp}
                (zipmap (range 1 (inc lenw)) (map (partial get inp) (range 0 lenw)))
                {(inc lenw) nil}))))
    #{}
    all-words-in-set))

(def position-indices
  (let [pos (range 1 22)]
    (zipmap
      pos
      (for [i pos] (set/index word-letterpositions-rel [i])))))

;; First attempt which produces list of words
;(defn get-by-pos-char
;  [pos char]
;  (map :word (get (position-indices pos) {pos char}))) ;produces lazy-seq
;; in this routine need to convert to sets which is time costly to can intersect
;(defn get-by-pos-char-map
;  [pcmap]
;  (let [matchsets (map (fn [k] (set (get-by-pos-char k (pcmap k)))) (keys pcmap))]
;    (apply set/intersection matchsets)))

;Now this keeps result as set
(defn get-by-pos-char
  [pos char]
  ;(map :word (get (position-indices pos) {pos char}) ; turns into list so need to convert back to set
  (get (position-indices pos) {pos char}))

;Interset then extract the words is much faster but intersection depends on size of sets
;TODO map :word or not? no difference in timing
(defn get-by-pos-char-map
  [pcmap]
  (let [matchsets (map (fn [k] (get-by-pos-char k (pcmap k))) (keys pcmap))]
    ;(map :word (apply set/intersection matchsets))
    (apply set/intersection matchsets)))


;TODO here the index is made each time, but if index is made once for
;   those desired positions fast to look up for different chars
;(defn get-by-pos-char-map
;  "pcmap is map of letter position (1 to 21) to desired char or nil
;  finds all words with specified char in those position
;  e.g. {1 \\d 3 \\g 5 nil} finds words of 4 letters with first
;  letter an d and third a g"
;  [pcmap]
;  (map :word (get (set/index word-letterpositions-rel (keys pcmap)) pcmap)))


(def word-letters-rel (relation set all-words-in-set :word :letters))
(def by-letters (set/index word-letters-rel [:letters]))
(defn get-by-letters
  [letters]
  (map :word (get by-letters {:letters (set letters)})))

; anagrams
(def word-sortedletters-rel (relation sort all-words-in-set :word :sortedletters))
(def by-sortedletters (set/index word-sortedletters-rel [:sortedletters]))
(defn get-by-sortedletters
  [sortedletters]
  (map :word (get by-sortedletters {:sortedletters (seq sortedletters)})))


(def word-codecracker-rel (relation (comp make-code-cracker-vector encode) all-words-in-set :word :ccvec))
(def by-ccvec (set/index word-codecracker-rel [:ccvec]))
(defn get-by-ccvec
  [ccvec]
  (map :word (get by-ccvec {:ccvec ccvec})))

(def vcstr (partial replace (merge (zipmap "aeiou" (repeat 5 \V)) (zipmap "bcdfghjklmnpqrstvwxy" (repeat \C)))))
(def word-vcstr-rel (relation vcstr all-words-in-set :word :vcstr))
(def by-vcstr (set/index word-vcstr-rel [:vcstr]))
(defn get-by-vcstr
  [vcstr]
  (map :word (get by-vcstr {:vcstr (seq vcstr)})))

(def vowelless (partial remove {\a 1 \e 1 \i 1 \o 1 \u 1}))
(def word-novowels-rel (relation  vowelless all-words-in-set :word :vowelless))
;(def word-novowels-rel (relation  vowelless #{"happy" "hippy" "dog" "tarot" "trout" "trait" "tort"} :word :vowelless))
(def by-vowelless (set/index word-novowels-rel [:vowelless]))
(defn get-by-vowelless
  [vowelless]
  (map :word (get by-vowelless {:vowelless (seq vowelless)})))

(def vowelless-count-rel
  (relation
    #(count (by-vowelless {:vowelless %}))
    (map :vowelless (keys by-vowelless))
    :vowelless
    :count))

; a bit tricky as have lists as values of maps
(comment
  (def k (keys by-vowelless))
  (nth k 0)
  ((nth k 0) by-vowelless)                                  ; only keys starting with : act as fcns
  (by-vowelless (nth k 0))
  (by-vowelless {:vowelless (\t \r \t)}) ;java.lang.ClassCastException: java.lang.Character cannot be cast to clojure.lang.IFn
  (by-vowelless {:vowelless '(\t \r \t)}))

(def word-vowelless-count-rel (set/join vowelless-count-rel word-novowels-rel))
(def by-count (set/index word-vowelless-count-rel [:count]))
(defn get-by-count
  [count]
  (map :word (get by-count {:count count})))

(def sortedletters-count-rel
  (relation
    #(count (by-sortedletters {:sortedletters %}))
    (map :sortedletters (keys by-sortedletters))
    :sortedletters
    :count))

(def word-sortedletters-count-rel (set/join sortedletters-count-rel word-sortedletters-rel))
(def by-anagram-count (set/index word-sortedletters-count-rel [:count]))
(defn get-by-anagram-count
  [count]
  (map :word (get by-anagram-count {:count count})))