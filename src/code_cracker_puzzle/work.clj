(ns code-cracker-puzzle.work
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]                         ;safe io
            [clojure.core.matrix :as m]
            [clojure.walk]
            [code-cracker-puzzle.core :refer :all]
            [clojure.repl :refer :all]))

;TODO why do I need require clojure.repl here?
; ANS in project.clj ns desiginated by :main will get it.


(defn letpat-filter
  [letuse]
  (if
    (= letuse "abcdefghijklmnopqrstuvwxyz") #"."
                                            (str "[" letuse "]")))

(defn partial-decoded-code-vec-to-regexpat-filter
  "Given partially decoded code cracker clue and new encoding map
  and letter to use produce the regex used in filtercode"
  [partial-decoded-code-vec new-map letuse]
  (let [cleanlet (clean-letuse letuse (vals new-map))
        v1 (reduce (fn [res input]
                     (conj res
                           (cond
                             ((set "abcdefghijklmnopqrstuvwxyz") input) \.
                             ((set res) input) \.
                             :else input)))
                   [] partial-decoded-code-vec)
        v2 (replace new-map v1)
        v3 (reduce (fn [res input]
                     (conj res
                              (cond
                                (number? input) (letpat-filter cleanlet)
                                :else input)))
                   [] v2)
        v3 (re-pattern(str/join "" v3))]
    ;(println partial-decoded-code-vec new-map letuse)
    ;(println cleanlet v1 v2 v3)
    v3))


;TODO  in filtercode wanted to use transducer but cant see how to use it to keep a lazy seq
; tried with filtercode producing (remove #(nil? (re-matches repat %))) called it ftran
; tried (transduce ftran (completing #(cons %2 %1)) [] (lazy-seq ["texxx" "abcde" "te" "atexj" "te;lk"]))
; but have found now way to keep a lazy sequence
;TODO the code below must be able to be shortened

(defn filtercode
  [alm nlm otherclue otherwords]
  (let [repat (partial-decoded-code-vec-to-regexpat-filter (decode-to-vec otherclue alm) nlm "")]
    (filter #(re-matches repat %) otherwords)))


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

(defn non-completed-some-could-be-bad?
  [cc]
  ;(some #(>= % 0) (:simplescores cc))   ; allows non-completed-bad-clues
  (some #(< % 1) (:wordcountscores cc))) ; allows non-completed-bad-clues

(defn non-completed-and-all-good?
  [cc]
  (and
    (some #(< % 1) (:wordcountscores cc))
    (every? #(and (not= % 1) (not= % 2)) (:wordcountscores cc)))) ;some not completed but no  bad clues either completed or not

(defn all-completed-and-all-good?
  [cc]
  (every? #(= % 3) (:wordcountscores cc))) ; all completed with good words

(defn all-completed?
  [cc]
  (every? #(> % 1) (:wordcountscores cc))) ; all completed

(defn all-good?
  [cc]
  (every? #(or (< % 1) (= % 3)) (:wordcountscores cc))) ; all good words

(defn set-remaining-keys
  "Sets values for keys :wordlists :partialwords :decodedclues :simplescores :wordcountscores"
  [cc]
  (let [dotmap (zipmap (range 1 27) (repeat 27 \.))
        wordlists (if (:wordlists cc)
                    (:wordlists cc)
                    (map #(find-all-words % (:encodemap cc)) (:clues cc))) ; for root
        depth (if (:depth cc)
                (:depth cc)
                0) ; for root
        decodedclue-vecs (map #(decode-to-vec  % (:encodemap cc)) (:clues cc))
        partialwords (map #(str/join "" (replace dotmap %)) decodedclue-vecs)
        simplescores (map simple-scores decodedclue-vecs)
        wordcountscores (map score-using-bounded-wordcount wordlists simplescores)
        completed (if (every? #(> % 1) wordcountscores) "complete" "unfinished")
        allgood (if (every? #(or (< % 1) (= % 3)) wordcountscores) "all good" "some bad")]
    (merge cc {:wordlists wordlists
               :partialwords partialwords
               :decodedclues decodedclue-vecs
               :simplescores simplescores
               :wordcountscores wordcountscores
               :completed completed
               :allgood allgood
               :clue []
               :word ""
               :depth depth})))



(defn make-child
  [cc clue word]
  (let [maxn 5
        newmap (update-assigned-letters-map (:encodemap cc) clue word)
        newwords (map #(filtercode (:encodemap cc) newmap %1 %2) (:clues cc) (:wordlists cc))
        updatedcc (merge cc {:encodemap newmap,
                             :wordlists newwords
                             :parent cc
                             :depth (inc (:depth cc))
                             :clue clue
                             :word word})]
    ;(println (inc (:depth cc)) (:encodemap cc) clue word newmap)
    ;(println (map #(take maxn %) (:wordlists cc)))
    ;(println (map #(take maxn %) newwords)) ; if this is uncommented will
    (set-remaining-keys updatedcc)))



(comment ; side effect printing happens here but not when used in make-child? Because had bug in set-remaining keys that overwrote newwords!
  (map #(filtercode {1 \a 2 \b} {1 \a 2 \b 3 \c} %1 %2) [[1 2 3 4] [1 2 3]] [["abcd" "abec"] ["abc" "abe"]]))

(defn children-from-clue
  "Make list of children from clue of given index using all words in its wordlist
  Note orderof list correponds to reverse order of words"
  [cc clue-index]
  (reduce #(conj %1 (make-child cc (nth (:clues cc) clue-index) %2)) (lazy-seq) (nth (:wordlists cc) clue-index)))

(defn children-from-clue-indicies
  "take list of indicies of non-completed-good-clues and combines all their children"
  [cc clue-indicies]
  ;(println clue-indicies (:depth cc) (:wordcountscores cc))
  (reduce #(concat %1 (lazy-seq (children-from-clue cc %2))) (lazy-seq)  clue-indicies))

;TODO if kbest is not 1 seem to get duplicate solutions. Why?
; because using clue1 then clue2 will produce same sols as using clue2 then clue1
(defn children
  "takes children of the best index using wordcountscores
  by changing kbest > 1 can use more than one clue but will
  get duplicate solutions."
  [cc]
  (let [kbest 1
        cm (zipmap (range (count (cc :clues))) (cc :wordcountscores))
        scm (sort #(< (last %1) (last %2)) cm)
        sind (keys scm)]
    (children-from-clue-indicies cc (take kbest sind))))


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
    (set-remaining-keys cc)))



(defn show-from-root
  [cc]
  (let [depth (:depth cc)
        gen (take (inc depth) (iterate #(:parent %) cc))]
    (doseq [n (range depth -1 -1)]
      (println (map #(% (nth gen n)) [:depth :completed :allgood :clue :word :partialwords :wordcountscores])))))

(defn show-at-most-n
  [cc nmax]
  (let [sols (take nmax cc)
        nshow (count sols)]
    (doseq [n (range nshow)]
      (println n (map #(% (nth sols n)) [:depth :completed :allgood :clue :word :partialwords :wordcountscores])))))

(comment
  (def root (make-example-for-work "the quick brown fox jumps over the lazy level dog")) ; 96 solutions
  (def root (make-example-for-work "now is the time for all good men to come to the aid of their party")) ; lots of solutions
  (def root (make-example-for-work "Jived fox nymph grabs quick waltz"))  ; might take long time to verify this
  (def root (make-example-for-work "abc cdef fghij jklmna")) ; lots of solutions 0 ... 5431
  (def root (make-example-for-work "abc defg hijkl mnopqr stu")) ; solutions 0 ...
  (def root (make-example-for-work "level")) ;13 solutions
  (def root (make-example-for-work "level kayak"))  ; disjoint palendromes so will also have kayak level
  (def ans (filter all-completed-and-all-good? (tree-seq non-completed-and-all-good? children root)))
  (nice-print ans 0);
  (show-at-most-n ans 10)
  (show-from-root (nth ans 0))  ; show chain from root
  (str/join " "(:partialwords (nth ans 0))) ; show just partialwords
  ; if want to example complete tree traversal
  (def root (make-example-for-work "pas pals clap sap lap slap claps pal")) ; 4 sols big tree
  (def root1 (set-remaining-keys (merge root {:encodemap {16 \p 1 \a 19 \s}}))) ; 2 sols little tree
  (def ans (tree-seq non-completed-and-all-good? children root1))
  (show-at-most-n ans 100))

(comment
  (def root (set-remaining-keys (get-cc 4)))  ;3 has bad word, 4 has no bad words
  ; will solve code cracker even if has a few bad words
  (def ans (filter
             (complement non-completed-some-could-be-bad?)
             (tree-seq non-completed-some-could-be-bad? children root)))
  ; will not solve if has bad word and will find nothing
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? children root)))
  ; will not solve if has bad word and will stop with partial sol when get a bad uncomplete word
  (def ans (filter
             (complement non-completed-and-all-good?)
             (tree-seq non-completed-and-all-good? children root)))
  (show-at-most-n ans 1)
  (show-from-root (nth ans 0)))  ; show chain from root


