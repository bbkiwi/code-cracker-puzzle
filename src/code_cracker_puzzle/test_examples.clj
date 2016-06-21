(ns code-cracker-puzzle.test-examples
    (:gen-class)
    (:require [code-cracker-puzzle.bill-utils :refer :all]
              [code-cracker-puzzle.work :refer :all]
              [code-cracker-puzzle.data-assembly :refer :all]
              [code-cracker-puzzle.output-routines :refer :all]
              [clojure.string :as str]
              [clojure.walk]
              [clojure.repl :refer :all]
              [criterium.core :refer [quick-bench]]))

(defn compare-fc
  [alm nlm otherclue]
  (let [pdc (decode-to-vec otherclue alm)
        otherwords (filteredlist (filter-from-partial-decoded-clue pdc))
        fc (filtercode alm nlm otherclue otherwords)
        fcr (filtercode-using-regex alm nlm otherclue otherwords)]
    (println "fc   " fc)
    (if (= fc fcr)
      (println "filtercode and filtercode-using-regex same result")
      (println "fcrx " fcr))
    (println "fc quick-bench:")
    (quick-bench (doall (filtercode alm nlm otherclue otherwords)))
    (println "fc rx quick-bench")
    (quick-bench (doall (filtercode-using-regex alm nlm otherclue otherwords)))))

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

(defn test-sol
  [root make-child]
  (let [ans (filter
              all-completed-and-all-good?
              (tree-seq non-completed-and-all-good? (children-from-best-clue-using make-child :wordcountscores) root))]
    (show-at-most-n ans 10)))

(defn bench-test-sol
  [root make-child]
  (let [ans (doall (filter
                     all-completed-and-all-good?
                     (tree-seq non-completed-and-all-good? (children-from-best-clue-using make-child :wordcountscores) root)))]
    (count ans)))

(defn remove-vowels-from-sentence
  [sentence]
  (let [lcwords (str/split (str/lower-case sentence) #" +")
        novowel (map vowelless lcwords)
        novowsent (str/join " "(map str/join novowel))]
    (println novowsent)
    (map get-by-vowelless novowel)))




; fc usually slower by 10-20 times!
(comment
  (compare-fc {} {} [1 2 3 4 5])                            ;fc 8.8 ms fc rx 0.47 ms
  (compare-fc {1 \m 5 \o} {1 \m 3 \n 5 \o 6 \p 7 \r} [1 2 3 4 5]) ; fc 17 micro sec fcrx 49 micro sec
  (compare-fc {1 \m} {1 \m 3 \n 5 \o 6 \p 7 \r} [1 2 3 4 5]) ; fc 204 micro sec fcrx 62 micro sec
  (compare-fc {} {1 \m 3 \n 5 \o 6 \p 7 \r} [1 2 3 4 5])    ; fc 3700 micro sec fcrx 264 micro sec
  (compare-fc {} {6 \p 7 \r 8 \e 9 \a} [1 2 3 4 5])         ; fc 12 ms fc rx 0.35 ms
  (compare-fc {} {31 \m 32 \a 33 \n 34 \g 35 \o} [31 32 33 34 35])
  (compare-fc {} {31 \m 32 \a 33 \n 34 \g 35 \o} [35 34 33 32 31])) ; fc much slower than fc rx


(comment
  (def f (other-clue-filter [1 2 3 4 5] [2 3 6 6]))
  (filteredlist f #{"crats" "brats" "house"})
  (filteredlist f (filteredlist (filter-from-partial-decoded-clue [31 32 33 34 35]))))



(comment
  (find-all-words [1 2 3 4 5 6 7 8 9 10 11 12 13] {}) ; fails as more than 8 distinct and regex cant deal with it.
  (filteredlist (filter-from-partial-decoded-clue [1 2 3 4 5 6 7 8 9 10 11 12 13])) ; ("draughtswomen" "unpredictably") more than 8 distinct
  (filteredlist (filter-from-partial-decoded-clue [31 32 33 34 35 36 37 38 39 40 41 42 43])) ; 1800 13 letter words
  (find-all-words [31 32 33 34 35 36 37 38 39 40 41 42 43] {}) ; ok
  (filteredlist (filter-from-partial-decoded-clue [1 2 3 2 1]))
  (filteredlist (filter-from-partial-decoded-clue [31 \l \a \p 35]))
  (filteredlist (filter-from-partial-decoded-clue [\h 31 32 \k]))
  (filteredlist (filter-from-partial-decoded-clue [\h 31 2 \k]))
  (filteredlist (filter-from-partial-decoded-clue [\h 31 31 \k]))
  (filteredlist (filter-from-partial-decoded-clue [\h 1 2 \k]))
  ; this just checks for correct chars only nums are ignored
  (filteredlist (value-filter-from-characteristic-mask (mask-for-char-clues [\h 1 2 \k])))
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [1 2 3 4 5])
                  (letter-to-use-filter [1 1 1 1 1] "ybcdfghjklmnpqrstvwxz")))
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [1 2 3 4 5])
                  (letter-to-avoid-filter [1 1 1 1 1] "aeiou")))
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [31 32 33 34 35])
                  (letter-to-use-filter [1 1 1 1 1] "ybcdfghjklmnpqrstvwxz")))
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [31 32 33 34 35])
                  (sub-word-filter [1 1 1 nil nil])
                  (sub-word-filter [nil 1 1 1 nil])
                  (sub-word-filter [nil nil 1 1 1])))
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [31 32 33 34 35])
                  (contained-letters-clue-filter [31 32 33 34 35] [32 33 34])
                  (contained-letters-clue-filter [31 32 33 34 35] [34 33 32])))

  (filteredlist (comp
                  (filter-from-partial-decoded-clue [31 32 33 34 35])
                  (contained-all-but-one-letters-clue-filter [31 32 33 34 35] [1 31 32 33 1])))

  ;TODO not working ..hangs
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [31 32 33 34 35])
                  (other-clue-filter [31 32 33 34 35] [32 33 34])
                  (other-clue-filter [31 32 33 34 35] [34 33 32])))
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [31 32 33])
                  ;(contained-letters-clue-filter [31 32 33] [32 33 31])
                  ;(contained-letters-clue-filter [31 32 33] [33 31 32])
                  (contained-letters-clue-filter [31 32 33] [31 33 32])
                  (contained-letters-clue-filter [31 32 33] [33 32 31])
                  (contained-letters-clue-filter [31 32 33] [32 31 33])))
  (filteredlist (comp
                  (filter-from-partial-decoded-clue [31 32 33 34 35])
                  (letter-to-use-filter [1 nil nil nil nil] "abc")
                  (letter-to-use-filter [nil nil nil nil 1] "xyz")
                  (sub-word-filter [nil 1 1 1 nil])))

  ; need doall infront of lazy seq for timing otherwise it is set up time
  ; find-all-words using reg ex is faster!

  (quick-bench (doall (find-all-words [31 32 33 34] {})))   ;   21 ms
  (quick-bench (doall (filteredlist (filter-from-partial-decoded-clue [31 32 33 34])))) ;    24 ms
  (quick-bench (doall (find-all-words [1 2 3 2 1] {})))     ;   16 ms
  (quick-bench (doall (filteredlist (filter-from-partial-decoded-clue [1 2 3 2 1])))) ;      21 ms
  (quick-bench (doall (find-all-words [\a 1 2 3 4 5] {})))  ;                               9 ms
  (quick-bench (doall (filteredlist (filter-from-partial-decoded-clue [\a 1 2 3 4 5])))) ;   23 ms
  (quick-bench (doall (find-all-words [1 2 3 4 3 \s] {})))  ;                               17 ms
  (quick-bench (doall (filteredlist (filter-from-partial-decoded-clue [1 2 3 4 3 \s]))))) ;  28 ms




;TODO clarify how to set up root and how to use tree-seq
(comment
  (def root (make-example-for-work "the quick brown fox jumps over the lazy level dog")) ; 96 solutions
  (def root (make-example-for-work "abcd bcde cdef  abcdef")) ; hone ones nest honest ... 12 solutions
  (def root (make-example-for-work "abcdefg bcdef cde"))    ;tragedy raged age ...pirates irate rat ...phoneys honey one ..20 sols
  (def root (make-example-for-work "abcdefg fedcb"))        ; deviant naive ..10 sols
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

  (def root (make-root {:ccinfo root :rootmap {1 \n 4 \s 13 \t 16 \o}})) ; with hint
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
             (tree-seq non-completed-and-all-good? (partial children-from-top-clues make-child-tr 1) root)))
  (nice-print ans 0)                                        ;
  (show-at-most-n ans 10)
  (show-pdc-at-most-n ans 10)
  (show-from-root (nth ans 0))                              ; show chain from root
  (str/join " " (:partialwords (nth ans 0)))                ; show just partialwords
  ; if want to examine complete tree traversal
  (def root (make-example-for-work "pas pals clap sap lap slap claps pal")) ; 4 sols big tree
  (def root (make-root {:ccinfo root :rootmap {16 \p 1 \a}})) ; 2 sols little tree
  (def ans (tree-seq non-completed-and-all-good? (children-from-best-clue-using :wordcountscores) root))
  (show-at-most-n ans 100))


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
             (tree-seq non-completed-and-all-good? (partial children-from-top-clues make-child-tr 1) root)))
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (children-from-best-clue-using :numinothers) root)))
  ; will not solve if has bad word and will stop with partial sol when get a bad uncomplete word
  (def ans (filter
             (complement non-completed-and-all-good?)
             (tree-seq non-completed-and-all-good? (children-from-best-clue-using :wordcountscores) root)))
  (show-at-most-n ans 1)
  (show-from-root (nth ans 0)))                             ; show chain from root


(comment
  (def root (make-example-from-clues [[1 2 3 4 5] [5 4 3 2 1]])) ; ok
  (def root (make-example-from-clues [[31 32 33 34 35] [35 34 33 32 31]])) ; ok
  (def root (make-example-from-clues [[30 31 32] [33 31 34] [30 34 32] [32 34 32]]))
  (def root (make-example-from-clues (free-grid 5)))
  (def root (make-example-from-clues (sym-grid 5)))
  (def root (make-example-from-clues (diagsame-grid 3)))
  (def root (make-root {:ccinfo root :rootmap {39 \e}}))    ; middle of above
  (def root (make-example-from-clues (diagsame-grid 3)))
  (def root (make-example-from-clues (free-cube 3)))
  (def root (make-example-from-clues (free-cube 4)))        ; none found yet  after long time
  (def root (make-example-from-clues [[27] [28]]))          ; ok 676 sols all possible combos of 2 letters
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (children-from-best-clue-using :wordcountscores) root)))
  (show-pdc-at-most-n ans 10))



(comment
  (test-sol root make-child-rx)
  (test-sol root make-child-tr)
  (quick-bench (bench-test-sol root make-child-rx))
  (quick-bench (bench-test-sol root make-child-tr))
  (def root (make-example-for-work "abcd efgh ijkl mnop aeim bfjn cgko dhlp"))
  (def root (make-root {:ccinfo root :rootmap {1 \n 4 \s 13 \t 16 \o}}))
  ;     in make-child filtercode using reg-ex quick-bench (bench-test-sol root)) 6 ms, using transducers 80 ms
  (def ccnumber 1)
  (def root (make-root {:ccinfo (get-cc ccnumber) :rootmap {}}))
  ;     in make-child filtercode using reg-ex quick-bench (bench-test-sol root)) 31 ms, using transducers 127 ms
  (def root (make-example-from-clues [[31 32 33 34]]))      ; find all 4 letter words
  ;TODO Why, the tree is only depth 1 and wordlist has the answers rx 500 msec tr 6 secs

  (quick-bench (doall (filteredlist (comp
                                      (filter-from-partial-decoded-clue [31 32 33 34 35])
                                      (contained-all-but-one-letters-clue-filter [31 32 33 34 35] [1 31 32 33 1]))))) ;329 ms
  (def root (make-example-from-clues [[31 32 33 34 35] [1 31 32 33 1]])) ; 1.95 sec rx, ? sec tr
  nil)



(comment
  (get-by-letters "apple")                                  ;=> ("peal" "paella" "leap" "lapel" "apple" "appeal" "plea" "pale")
  ;(quick-bench (get-by-letters "apple"))
  (get-by-ccvec ((comp make-code-cracker-vector encode) "level"))
  (get-by-ccvec [1 2 3 3 1])                                ;=> ("shoos" "yummy" "tweet" "setts" "sills" "sells" "yukky" "yuppy")
  (get-by-vcstr "VCVCVCVCVCVCV")                            ;=> ("unimaginative")
  (get-by-vowelless "ppl")                                  ;=> ("pupal" "pupil" "papal" "apple" "people" "appeal" "appal")
  (get-by-vowelless (vowelless "banana"))
  (remove-vowels-from-sentence "this is a very simple sentence that anyone can understand")
  (remove-vowels-from-sentence "notched rutabagas flanked weathered paleontologists")

  (apply max (map count (vals by-vowelless)))               ;=> 26
  (get-by-count 26)                                         ; 26 words so all with same "rs"
  (map #(count (get-by-count %)) (range 1 27))
  ;=> (38013 9690 3831 2260 1550 1026 784 648 477 380 451 228 247 196 255 96 85 54 19 60 21 22 0 0 0 26)
  (apply + *1)                                              ;=> 60419
  (count all-words-in-set)                                  ;=> 60419
  (map #(/ (count (get-by-count %)) %) (range 1 27))
  ;=> (38013 4845 1277 565 310 171 112 81 53 38 41 19 19 14 17 6 5 3 1 3 1 1 0 0 0 1)
  (map str/join (distinct (map vowelless (get-by-count 20)))) ;=> ("bt" "prs" "ld")
  (map get-by-vowelless (map str/join (distinct (map vowelless (get-by-count 20)))))

  (apply max (map count (vals by-sortedletters)))               ;=> 7
  (get-by-anagram-count 7)                                      ; 7 words all anagrams of "reaps"
  (map #(count (get-by-anagram-count %)) (range 1 8))
  ;=> (53135 5470 1272 396 85 54 7)
  (apply + *1)                                              ;=> 60419
  (count all-words-in-set)                                  ;=> 60419
  (map #(/ (count (get-by-anagram-count %)) %) (range 1 8))
  ;=> (53135 2735 424 99 17 9 1)
  (map str/join (distinct (map sort (get-by-anagram-count 6)))) ;=> ("eimrst" "acerst" "aelps" "aelpst" "aprst" "adeprs" "opst" "aers" "aelst")
  (map get-by-sortedletters (map str/join (distinct (map sort (get-by-anagram-count 6))))))




