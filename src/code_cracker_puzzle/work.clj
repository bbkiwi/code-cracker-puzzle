(ns code-cracker-puzzle.work
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]  ;safe io
            [clojure.core.matrix :as m]
            [clojure.walk]
            [code-cracker-puzzle.core :refer :all]
            [clojure.repl :refer :all]))

; why do I need require clojure.repl here?
; ANS in project.clj ns desiginated by :main will get it.

(defn count-num-in-clues
  "Produces lazy seq c with (nth c n) the number of clues having n in them"
  [clues]
  (map (fn [n] (count (keep (fn [clue] ((set clue) n)) clues))) (range 1 27)))

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
  ""
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
  (when (not (:depth cc)) (println "Warning set-remaining-keys got cc with no depth. Setting up root?"))
  (let [dotmap (zipmap (range 1 27) (repeat 27 \.))
         decodedclue-vecs (map #(decode-to-vec % (:encodemap cc)) (:clues cc))
         partialwords (map #(str/join "" (replace dotmap %)) decodedclue-vecs)
         simplescores (map simple-scores decodedclue-vecs)
         wordcountscores (map score-using-bounded-wordcount (:wordlists cc) simplescores)
         completed (if (every? #(> % 1) wordcountscores) "complete" "unfinished")
         allgood (if (every? #(or (< % 1) (= % 3)) wordcountscores) "all good" "some bad")]
     (merge cc
            {:partialwords    partialwords
             :decodedclues    decodedclue-vecs
             :simplescores    simplescores
             :wordcountscores wordcountscores
             :completed       completed
             :allgood         allgood})))


(defn make-root
  "2-arity makes root with :encodegmap rootmap. 1-arity with (:encodemap cc)"
  ([cc rootmap]
   (let [numinclues (count-num-in-clues (:clues cc))
         wordlists (map #(find-all-words % rootmap) (:clues cc))
         rmap (merge cc
                     {:encodemap rootmap
                      :wordlists wordlists
                      :numinclues numinclues
                      :depth     0})]
     (println numinclues)
     (set-remaining-keys rmap)))
  ([cc]
   (make-root cc (:encodemap cc))))


(defn make-child
  [cc clue word]
  (let [newmap (update-assigned-letters-map (:encodemap cc) clue word)
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
        scm (sort-by last cm); better than above
        sind (keys scm)]
    (children-from-clue-indicies cc (take kbest sind))))

(defn children-from-best-clue
  "takes children of the best index using wordcountscores.
  since 0 is smallest possible  scan indices keeping track of min found
  but stop if find 0"
  [cc]
  (let [bind (first (reduce-kv
                      (fn [acc ind val]
                        (cond
                          (zero? val) (reduced [ind val])
                          (> (last acc) val) [ind val]
                          :else acc))
                      [nil 4] ; 4 is bigger than any wordcountscores
                      (vec (cc :wordcountscores))))]
    (children-from-clue cc bind)))


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
   to get list of clues which are used to solve.
   Produces root with {} encodemap"
  [sentence]
  (let [clues (map encode (str/split (str/lower-case sentence) #" +"))
        cc {:clues clues}]
    (println clues)
    (make-root cc {})))

(defn show-from-root
  [ans]
  (let [depth (:depth ans)
        gen (take (inc depth) (iterate #(:parent %) ans))]
    (doseq [n (range depth -1 -1)]
      (println (map #(% (nth gen n)) [:depth :completed :allgood :clue :word :partialwords :wordcountscores])))))

(defn show-at-most-n
  "times and shows info about up to nmax solutions in ans"
  [ans nmax]
  (time
    (let [sols (take nmax ans)
          nshow (count sols)]
      (doseq [n (range nshow)]
        (println n (map #(% (nth sols n)) [:depth :completed :allgood :clue :word]))
        (doall (map #(print (str/join [%1 "(" %2 ") "]))
                    (replace ((nth sols n) :encodemap) (range 1 27))
                    ((nth sols n) :numinclues)))
        ;(println (replace ((nth sols n) :encodemap) (range 1 27)))
        ;(println ((nth sols n) :numinclues))
        (println)
        (println n (map #(% (nth sols n)) [:partialwords]))
        (println n (map #(% (nth sols n)) [:wordcountscores]))))))

;TODO clarify how to set up root and how to use tree-seq
(comment
  (def root (make-example-for-work "the quick brown fox jumps over the lazy level dog")) ; 96 solutions
  (def root (make-example-for-work "now is the time for all good men to come to the aid of their party")) ; lots of solutions
  (def root (make-example-for-work "Jived fox nymph grabs quick waltz"))  ; might take long time to verify this
  (def root (make-example-for-work "abc cdef fghij jklmna")) ; lots of solutions 0 ... 5431
  (def root (make-example-for-work "abc defg hijkl mnopqr stu")) ; solutions 0 ...
  (def root (make-example-for-work "level")) ;13 solutions
  (def root (make-example-for-work "level kayak"))  ; disjoint palendromes so will also have kayak level
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

  (def root (make-root root {1 \n 4 \s 13 \t 16 \o})) ; with hint
  ;"Elapsed time: 60.3222 msecs" Liz solved this in a few hours)

  (def root (make-example-for-work "abcde fghij klmno pqrst uvwxy afkpu bglqv chmrw dinsx ejoty")) ; 5x5 grid of distinct letters
  ; nil "Elapsed time: 480560.109286 msecs" so no 5x5 of distinct letters verified in 8 minutes (maxcnt=10)

  ;fastest
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? children-from-best-clue root)))
  ;slower
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (partial children-from-top-clues 1) root)))
  (nice-print ans 0);
  (show-at-most-n ans 10)
  (show-from-root (nth ans 0))  ; show chain from root
  (str/join " "(:partialwords (nth ans 0))) ; show just partialwords
  ; if want to examine complete tree traversal
  (def root (make-example-for-work "pas pals clap sap lap slap claps pal")) ; 4 sols big tree
  (def root (make-root root {16 \p 1 \a})) ; 2 sols little tree
  (def ans (tree-seq non-completed-and-all-good? children-from-best-clue root))
  (show-at-most-n ans 100))

(comment
  ; set up root using assigned encodemap
  (def ccnumber 43) ;3 has bad word, 4 has no bad words, 43 has a few bad words
  (def root (make-root (get-cc ccnumber)))
  ; set up root overwritting assigned encodemap and using {}
  (def root (make-root (get-cc ccnumber) {}))
  ; will solve code cracker even if has a few bad words
  ;  43 with "root" encoding is interesting and will almost solve, but not with override {}
  (def ans (filter
             (complement non-completed-some-could-be-bad?)
             (tree-seq non-completed-some-could-be-bad? children-from-best-clue root)))
  (def ans (filter
             all-completed?
             (tree-seq non-completed-some-could-be-bad? children-from-best-clue root)))
  ; will not solve if has bad word and will find nothing
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? children-from-best-clue root)))
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (partial children-from-top-clues 1) root)))
  ; will not solve if has bad word and will stop with partial sol when get a bad uncomplete word
  (def ans (filter
             (complement non-completed-and-all-good?)
             (tree-seq non-completed-and-all-good? children-from-best-clue root)))
  (show-at-most-n ans 1)
  (show-from-root (nth ans 0)))  ; show chain from root




