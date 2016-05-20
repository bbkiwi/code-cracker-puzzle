(ns code-cracker-puzzle.work
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]                         ;safe io
            [clojure.core.matrix :as m]
            [clojure.walk]
            [code-cracker-puzzle.core :refer :all]
            [clojure.repl :refer :all]))


;  (:require [code-cracker-puzzle.core :refer :all]
;           [clojure.repl :refer :all]))


;TODO why do I need require clojure.repl here?
; why didn't the requires in code-cracker-puzzle.core
; take effect here?


(defn letpat-filter
  [letuse]
  (if
    (= letuse "abcdefghijklmnopqrstuvwxyz") #"."
                                            (str "[" letuse "]")))

(defn pat-for-symbol-filter
  "Gives pattern for each symbol in code cracker pattern"
  [front-pat char-from-pat letuse cleaned-letuse]
  (cond
    (= char-from-pat \0) (letpat-filter letuse)
    (contains? (set "12345678") char-from-pat)
    (if (contains? (set front-pat) char-from-pat)
      "."
      (letpat-filter cleaned-letuse))
    :else (str char-from-pat)))


(defn code-cracker-to-regex-body-filter
  "This is the recursive routine"
  [front-pat char-pat rest-pat letuse cleaned-letuse]
  (let [pfs (pat-for-symbol-filter front-pat char-pat letuse cleaned-letuse)]
    (if (empty? rest-pat)
      pfs
      (str pfs
           (code-cracker-to-regex-body-filter
             (str/join "" [front-pat char-pat])
             (first rest-pat)
             (str/join "" (rest rest-pat))
             letuse
             cleaned-letuse)))))

(defn code-cracker-pat-to-regexpat-filter
  "Converts code cracker pattern string to regex. For producting the regex
   used in filtercode"
  ;TODO might want code-cracker-pat to be vector and will need to fix
  [code-cracker-pat letuse]
  (let
    [front-pat ""
     char-pat (first code-cracker-pat)  ; not work if code-cracker-pat is vec
     rest-pat (str/join "" (rest code-cracker-pat))]
    (re-pattern (str
                  (code-cracker-to-regex-body-filter
                    front-pat
                    char-pat
                    rest-pat
                    (clean-letuse letuse)
                    (clean-letuse letuse code-cracker-pat))))))



;TODO  in filtercode wanted to use transducer but cant see how to use it to keep a lazy seq
; tried with filtercode producing (remove #(nil? (re-matches repat %))) called it ftran
; tried (transduce ftran (completing #(cons %2 %1)) [] (lazy-seq ["texxx" "abcde" "te" "atexj" "te;lk"]))
; but have found now way to keep a lazy sequence
;TODO the code below must be able to be shortened
(defn filtercode
  "Given the assigned letter map, the clue and matching word used to update it
   finds the filter code to get new word lists associated with otherclue
   returns the new word list"
  [alm nlm otherclue otherwords]
  (let [newkeys (new-keys alm clue)
        firstinds (first-indicies newkeys otherclue)
        cluerange (range 0 (count otherclue))
        restinds (vec (set/difference (set cluerange) (set firstinds)))
        cluemap (zipmap clue word)
        otherword (replace cluemap otherclue)
        cluemap (zipmap cluerange otherword)
        defmap (zipmap restinds (repeat (count restinds) \.))
        filtermap (merge cluemap defmap)
        repat (re-pattern (str/join "" (replace filtermap cluerange)))]
    (println newkeys firstinds restinds defmap cluemap otherword filtermap repat)
    (filter #(re-matches repat %) otherwords)))


;old
;(defn filtercode
;  "Given the assigned letter map, the clue and matching word used to update it
;   finds the filter code to get new word lists associated with otherclue
;   returns the new word list"
;  [alm clue word otherclue otherwords]
;  (let [newkeys (new-keys alm clue)
;        firstinds (first-indicies newkeys otherclue)
;        cluerange (range 0 (count otherclue))
;        restinds (vec (set/difference (set cluerange) (set firstinds)))
;        cluemap (zipmap clue word)
;        otherword (replace cluemap otherclue)
;        cluemap (zipmap cluerange otherword)
;        defmap (zipmap restinds (repeat (count restinds) \.))
;        filtermap (merge cluemap defmap)
;        repat (re-pattern (str/join "" (replace filtermap cluerange)))]
;    (println newkeys firstinds restinds defmap cluemap otherword filtermap repat)
;    (remove #(nil? (re-matches repat %)) otherwords)
;    #_(filter #(re-matches repat %) otherwords)))
;; equivalent


;(filtercode {7 \a, 13 \w, 9 \c, 5 \s, 14 \i, 10 \u, 8 \b} "abacus" [7 8 7 9 10 5] [7 8 7] ["aba" "aca"])



(defn simple-scores
  "score by number of different letters needed to complete less 1 over total length
   zero if one letter left, neg if complete, always < 1"
  [decodedclue]
  (->>
    decodedclue
    (filter number?)
    (distinct)
    (count)
    (dec)
    (* (/ (count decodedclue)))))


(defn score-using-bounded-wordcount
  "completed clues give 3 if in dictionary, 2 if not in dictionary,
  give 1 if no words match uncompleted clue
  give 1 - 1/(number of words found upto maxcnt = 10)
  so denominator is min number of matches and 10
  in particular gives 0 if 0 if exactly 1 word matches uncompleted clue"
  [wordlist simplescore]
  (let [maxcnt 10
        cnt (count (take maxcnt wordlist))]
    (if (< simplescore 0) ;complete
      (+ 2 cnt)
      (if (zero? cnt)
        1
        (inc (- (/ cnt)))))))

(defn set-inital-partial-sol
  "Sets values for keys :wordlists :partialwords :decodedclues :simplescores :wordcountscores"
  [cc]
  (let [dotmap (zipmap (range 1 27) (repeat 27 \.))
        wordlists (map #(find-all-words % (:encodemap cc)) (:clues cc))
        decodedclue-vecs (map #(decode-to-vec  % (:encodemap cc)) (:clues cc))
        partialwords (map #(str/join "" (replace dotmap %)) decodedclue-vecs)
        simplescores (map simple-scores decodedclue-vecs)
        wordcountscores (map score-using-bounded-wordcount wordlists simplescores)]
    (merge cc {:wordlists wordlists :partialwords partialwords :decodedclues decodedclue-vecs :simplescores simplescores :wordcountscores wordcountscores})))

(defn non-completed?
  [cc]
  ;(some #(>= % 0) (:simplescores cc)) ; allows non-completed-bad-clues
  (some #(< % 1) (:wordcountscores cc))) ;does not allow non-completed bad clues

(defn make-child
  [cc clue word]
  (let [maxn 5
        newmap (update-assigned-letters-map (:encodemap cc) clue word)
        newwords (map #(filtercode (:encodemap cc) clue word %1 %2) (:clues cc) (:wordlists cc))
        updatedcc (merge cc {:encodemap newmap, :wordlists newwords})]
    ;(println (:encodemap cc) clue word newmap)
    ;(println (map #(take maxn %) (:wordlists cc)))
    ;(println (map #(take maxn %) newwords))
    (set-inital-partial-sol updatedcc)))

(defn children-from-clue
  "Make list of children from clue of given index using all words in its wordlist
  Note orderof list correponds to reverse order of words"
  [cc clue-index]
  (reduce #(conj %1 (make-child cc (nth (:clues cc) clue-index) %2)) (lazy-seq) (nth (:wordlists cc) clue-index)))

(defn children-from-clue-indicies
  "take list of indicies of non-completed-good-clues and combines all their children"
  [cc clue-indicies]
  (reduce #(concat %1 (lazy-seq (children-from-clue cc %2))) (lazy-seq)  clue-indicies))

(defn children
  "takes children of the best k indicies suing wordcountscores"
  [cc]
  (let [kbest 1
        cm (zipmap (range (count (cc :clues))) (cc :wordcountscores))
        scm (sort #(< (last %1) (last %2)) cm)
        sind (keys scm)]
    (children-from-clue-indicies cc (take kbest sind))))



;(def root (set-inital-partial-sol (get-cc 1)))
;(def ans (filter (complement non-completed?) (tree-seq non-completed? children root)))
; (nice-print ans 0)  ; (take 1 ans))


(defn nice-print
  "cclist is list of cc found using children, looks at index n"
  [cclist n]
  (let [cc (nth cclist n)]
    (println (cc :encodemap))
    (map #(println %1 %2 %3 %4)
         (cc :simplescores)
         (cc :clues)
         (cc :decodedclues)
         (cc :wordcountscores))
    #_(println "done")))
;TODO why if #_ removed does side effects of map not work? fact they are lazy?


(defn make-example-for-work
  "Breaks sentence into lower case words and encodes via 1 \\a, 2 \\b ...
   to get list of clues which are used to solve."
  [sentence]
  (let [clues (map encode (str/split (str/lower-case sentence) #" +"))
        cc {:encodemap {} :clues clues}]
    (println clues)
    (set-inital-partial-sol cc)))


;(make-example-for-work "pas pals clap sap lap slap claps pal")
;(def root (make-example-for-work "pas pals clap sap lap slap claps pal"))

