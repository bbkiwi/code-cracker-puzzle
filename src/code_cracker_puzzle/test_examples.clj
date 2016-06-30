(ns code-cracker-puzzle.test-examples
    (:gen-class)
    (:require [code-cracker-puzzle.global-vars-n-helpers :refer :all]
              [code-cracker-puzzle.bill-utils :refer :all]
              [code-cracker-puzzle.work :refer :all]
              [code-cracker-puzzle.data-assembly :refer :all]
              [code-cracker-puzzle.output-routines :refer :all]
              [clojure.string :as str]
              [clojure.walk]
              [clojure.repl :refer :all]
              [criterium.core :refer [quick-bench]]
              [clojure.set :as set]))

;; DOES NOT WORK - using with-out-str to capture output seems to cause
;  quick-bench to time something else
;(defn quick-bench-execution-time
;  [expr]
;  (second (re-find #"Execution time mean : (\d+\.\d+ \w*)"
;           (with-out-str (quick-bench expr)))))


;TODO check and fix fcrels for clues having non free letters
(defn compare-and-benchmark-fc
  "runs versions filtercode and compares
  if optional keyword param :bench is true will use quick-bench to time"
  [alm nlm otherclue & {:keys [bench] :or {:bench false}}]
  (let [pdc (decode-to-vec otherclue alm)
        otherwords (filteredlist (filter-from-partial-decoded-clue pdc))
        otherwordsrel (make-word-letterpositions-rel otherwords)
        fc (filtercode alm nlm otherclue otherwords)
        fcr (filtercode-using-regex alm nlm otherclue otherwords)
        fcrels (filtercode-using-rels alm nlm otherclue otherwordsrel)]
    (assert (= (set otherwords) (set (map :word otherwordsrel))))
    (println "fc   count " (count fc) fc)
    ;(println "otherwords   count " (count otherwords) otherwords)
    ;(println "otherwordsrel   count " (count otherwordsrel) otherwordsrel)
    (if (= fc fcr)
      (println "filtercode-using-regex same result")
      (println "PROBLEM fcrx  count " (count fcr) fcr))
    (if (= (set fc) (set (map :word fcrels)))
      (println "filtercode-using-rels same result")
      (println "PROBLEM fcrels  count " (count fcrels) fcrels))
    (when bench
      (println "Benchmarking ...")
      (criterium.core/report-point-estimate
        "fc transducers"
        (:mean (criterium.core/quick-benchmark (filtercode alm nlm otherclue otherwordsrel) nil)))
      (criterium.core/report-point-estimate
        "fc regex"
        (:mean (criterium.core/quick-benchmark (filtercode-using-regex alm nlm otherclue otherwordsrel) nil)))
      (criterium.core/report-point-estimate
        "fc rels"
        (:mean (criterium.core/quick-benchmark (filtercode-using-rels alm nlm otherclue otherwordsrel) nil))))))


(defn make-example-from-sentence
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



; compares filtercode-using-regex, filtercode-using-rels, and filtercode (using transducer filters)
; They are now in from fastest to slowest with relative times 1, 3, 15
(comment
  (compare-and-benchmark-fc {} {} [1 2 3 4 5])              ;fc 8.8 ms fc rx 0.47 ms
  (compare-and-benchmark-fc {1 \m 5 \o} {1 \m 3 \n 5 \o 6 \p 7 \r} [1 2 3 4 5]) ; fc 17 micro sec fcrx 49 micro sec
  (compare-and-benchmark-fc {1 \m} {1 \m 3 \n 5 \o 6 \p 7 \r} [1 2 3 4 5]) ; fc 204 micro sec fcrx 62 micro sec
  (compare-and-benchmark-fc {} {1 \m 3 \n 5 \o 6 \p 7 \r} [1 2 3 4 5]) ; fc 3700 micro sec fcrx 264 micro sec
  (compare-and-benchmark-fc {} {6 \p 7 \r 8 \e 9 \a} [1 2 3 4 5]) ; fc 12 ms fc rx 0.35 ms
  (compare-and-benchmark-fc {} {31 \m 32 \a 33 \n 34 \g 35 \o} [31 32 33 34 35])

  (compare-and-benchmark-fc {} {31 \m 32 \a 33 \n 34 \g 35 \o} [31 32 33 34 35])
  (compare-and-benchmark-fc {} {31 \m 32 \a 33 \n 34 \g 35 \o} [35 34 33 32 31]) ; fc much slower than fc rx
  (comment
    (def f (other-clue-filter [1 2 3 4 5] [2 3 6 6]))
    (filteredlist f #{"crats" "brats" "house"})
    (filteredlist f (filteredlist (filter-from-partial-decoded-clue [31 32 33 34 35])))))

(comment
  (quick-bench (findall #" +(a\w*)(?= )" word-dic)) ;269 ns using quick-bench
  (quick-bench (get-by-pos-char 1 \a)) ;414 ns
  (quick-bench (get-by-pos-char-map {1 \a})) ;771 ns
  (quick-bench (filteredlist (letter-to-use-filter [1 nil nil nil nil nil nil nil nil] "a"))) ;1000 ns

;TODO more exaustive testing of timing - how to programitally get quick bench results?
  (findall #" +(aq\w\wd\w\w\ws\w*)(?= )" word-dic) ;301.708573 µs
  (get-by-pos-char-map {1 \a 2 \q 5 \d 9 \s}) ; 61.194255 µs   first attempt was 5.123335 ms
  (find-all-words [\a \q 33 34 \d 36 37 38 \s] {}) ;408.841643 µs

  (findall #" +(\w\w\w\wa)(?= )" word-dic) ; 244.821262 µs
  (get-by-pos-char-map {5 \a 6 nil}) ;998.663606 µs
  (find-all-words [31 32 33 34 \a] {}) ;322.542646 µs

  (findall #" +(a\w\w\w\w)(?= )" word-dic) ;2.116498 µs
  (get-by-pos-char-map {1 \a 6 nil}) ;839.849934 µs with map word: and same time without
  (find-all-words [\a 32 33 34 35] {}) ;79.288385 µs

  ;TODO FIX - find-all-words limited to at most 8 distinct letters and doesn't handle free letters very well
  (find-all-words [31 31 31] {}) ; WRONG
  (find-all-words [1 2 2 33 33 3] {}) ; WRONG
  (filteredlist (filter-from-partial-decoded-clue [1 2 2 33 33 3]))

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
  (def root (make-example-from-sentence "the quick brown fox jumps over the lazy level dog")) ; 96 solutions
  (def root (make-example-from-sentence "abcd bcde cdef  abcdef")) ; hone ones nest honest ... 12 solutions
  (def root (make-example-from-sentence "abcdefg bcdef cde"))    ;tragedy raged age ...pirates irate rat ...phoneys honey one ..20 sols
  (def root (make-example-from-sentence "abcdefg fedcb"))        ; deviant naive ..10 sols
  (def root (make-example-from-sentence "now is the time for all good men to come to the aid of their party")) ; lots of solutions
  (def root (make-example-from-sentence "Jived fox nymph grabs quick waltz")) ; might take long time to verify this
  (def root (make-example-from-sentence "abc cdef fghij jklmna")) ; lots of solutions 0 ... 5431
  (def root (make-example-from-sentence "abc defg hijkl mnopqr stu")) ; solutions 0 ...
  (def root (make-example-from-sentence "level"))                ;13 solutions
  (def root (make-example-from-sentence "level kayak"))          ; disjoint palendromes so will also have kayak level
  (def root (make-example-from-sentence "abc def ghi adg beh cfi")) ; 3x3 grid of distinct letters
  (def root (make-example-from-sentence "abcd efgh ijkl mnop aeim bfjn cgko dhlp")) ; 4x4 grid of distinct letters
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

  (def root (make-example-from-sentence "abcde fghij klmno pqrst uvwxy afkpu bglqv chmrw dinsx ejoty")) ; 5x5 grid of distinct letters
  ; nil "Elapsed time: 480560.109286 msecs" so no 5x5 of distinct letters verified in 8 minutes (maxcnt=10)

  ;-------------------------------------------------------
  ;TODO when not using free letters this still may be useful
  ;make all clues uses free letters
  (def root (make-example-from-sentence "now is the time for all good men to come to the aid of their party"))
  (def root (make-root {:ccinfo {:clues (map #(vec (map (partial + 30) %)) (:clues root))}}))
  ;here use :numinothers and stop when have only 3 and 1 for numinothers and still keeping all good clues
  (def ans (filter
             all-good-completed-or-independent?
             (tree-seq some-numinothers-and-all-good? (children-from-best-clue-using :numinothers) root)))
  (def ans (filter
             all-good-completed-or-independent?
             (tree-seq some-numinothers-and-all-good? (children-from-best-clue-using :wordcountscores) root))) ;faster
  ;the partial words left are all independent so multiple solutions can be found from the wordlists
  (:wordlists (nth ans 0))
  ;--------------------------------------------------------

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
  (def root (make-example-from-sentence "pas pals clap sap lap slap claps pal")) ; 4 sols big tree
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

;----------------- making up code crackers with pattern of given cracker

  ; want to prevent duplicate words appearing in solution
  ; afer having a solution letters in isolated positions can be made free again and try solving again
  ; as they will all be independent
  ; need to force using all the letters
  ;TODO both using #1 and #1 first sol had same letters BFHJKQ missing? why?
  ;TODO can also remove everyother column or even some horizonal and some vertical words to leave independent choices

; ------ func to generate code crackers as outlined removing unneeded rows (but could use cols and mixture
(defn gen-cc
  [n & {:keys [em] :or {em {}}}]
  (let [cc (get-cc n)
        ccpat (merge cc {:clues (:clues-distinct cc) :rows (:rows-distinct cc)}) ; use distinct code for each blank 30, 31, ...
        root (make-root {:ccinfo ccpat :rootmap em}) ; set up root overwritting assigned encodemap with em (default {})
        solver (fn [root] (filter
                            all-good-completed-or-independent?
                            (tree-seq some-numinothers-and-all-good? (children-from-best-clue-using :simplescores) root)))
        ans (solver root) ; will be lots but
        cc0 (nth ans 0)   ; just take the first
        unneeded? (if (< 5 (count (filter zero? (first (:rows cc0))))) even? odd?) ; count black squares
        unneededrows (keep-indexed #(when (unneeded? %1) %2) (:rows cc0))
        unneededkeys (flatten unneededrows)
        nem (apply dissoc (:encodemap cc0) unneededkeys) ;remove keys for codes in unneeded rows
        rootmulti (make-root {:ccinfo ccpat :rootmap nem}) ;set up root to use encoding map for needed rows
        ansmulti (solver rootmulti) ; so should only be one solution with horizontal words specified and  vertical words from their wordlists
        ccmulti (nth ansmulti 0)]
    (printcodecracker cc0)
    (printcodecracker ccmulti)
    ;(println (:wordlists ccmulti))    ; will show all the words that can take the vertical position
    (println "Number of these solutions: " (apply * (map count (:wordlists ccmulti))))
    (println "Missing from all: " (clean-letuse "" (apply set/union (map set (flatten (:wordlists ccmulti))))))))

(comment
  (gen-cc 5 :em {30 \q 42 \k}) ;4803701760 but "x" is missing from them all
  (gen-cc 5 :em {30 \q 42 \k 32 \x}) ;161404379136 sols will be possible to use all letters
  nil)


;------------------------------------------------------------------------
;----------- Example from clues in various geometric patters
(comment
  (def root (make-example-from-clues [[1 2 3 4 5] [5 4 3 2 1]])) ; ok
  (def root (make-example-from-clues [[31 32 33 34 35] [35 34 33 32 31]])) ; ok
  (def root (make-example-from-clues [[30 31 32] [33 31 34] [30 34 32] [32 34 32]]))
  (def root (make-example-from-clues (free-grid 5)))
  ; 5x5 in the Press puzzle page has eider (for 4th clue) as one of the words and this is not in word-dic
  ; using non-completed-some-could-be-bad?  and :wordscores in method below misses sol as 4th clue is sloved by elder in word-dic
  ;  however using :simplescores will find it among the other 332 found
  (def root (make-root {:ccinfo root :rootmap {31 \e 33 \f 35 \r 37 \v 39 \e 41 \r 43 \s 45 \e 47 \d 49 \r 51 \e 53 \d}}))
  (show-pdc-at-most-n (filter at-most-one-bad? ans) 400)
  ; however by specifying 46 as \i it will find the solution
  (def root (make-root {:ccinfo root :rootmap {31 \e 33 \f 35 \r 37 \v 39 \e 41 \r 43 \s 45 \e 47 \d 49 \r 51 \e 53 \d 46 \i}})) ;finds


  (def root (make-example-from-clues (asym-grid 5)))
  (def root (make-example-from-clues (sym-grid 5)))

  ; find asym grid solution with restrictions on initial words
  ; the sols found still have some symmetry to kill all need
  ;    to have free letter pairs that much be mutially exclusive
  (def root (make-example-from-clues (asym-grid 5)))
  (def cvcvc (set (get-by-vcstr "CVCVC")))
  (def vcvcv (set (get-by-vcstr "VCVCV")))
  (def extrawordlists [cvcvc vcvcv cvcvc vcvcv cvcvc cvcvc vcvcv cvcvc vcvcv cvcvc])
  (def root (make-root {:ccinfo root :extrawordlists extrawordlists}))

  (def root (make-example-from-clues (diagsame-grid 3)))
  (def root (make-root {:ccinfo root :rootmap {39 \e}}))    ; middle of above
  (def root (make-example-from-clues (diagsame-grid 3)))
  (def root (make-example-from-clues (free-cube 3)))
  (def root (make-example-from-clues (free-cube 4))) ; none found yet  after long time

  (def root (make-example-from-clues (asym-cube 3)))
  (def cvc (set (get-by-vcstr "CVC")))
  (def vcv (set (get-by-vcstr "VCV")))
  ;(def cvc "cvc")
  ;(def vcv "vcv")
  (def extrawordlists (apply concat (repeat 3 (take 9 (apply concat (repeat 5 [cvc vcv]))))))
  (def root (make-root {:ccinfo root :extrawordlists extrawordlists}))



  (def root (make-example-from-clues (asym-cube 4)))        ; none found after 1 hour

  ; --------------Putting single letter clue helps solve ---------------------
  (def root (make-example-from-clues (concat [[30]] (asym-cube 4))))
  (def cvcv (set (get-by-vcstr "CVCV"))) ; a restiction but not unreasonable
  (def vcvc (set (get-by-vcstr "VCVC")))
  ;(def cvcv "cvcv")
  ;def vcvc "vcvc")
  (def extrawordlists (apply concat (repeat 3 (apply concat (repeat 2 [cvcv vcvc cvcv vcvc vcvc cvcv vcvc cvcv])))))
  (def extrawordlistsplus (conj extrawordlists  (set (map str (set "abcdefghijklmnopqrstuvwxyz")))))
  (def root (make-root {:ccinfo root :extrawordlists extrawordlists}))
  (def root (make-root {:ccinfo root :extrawordlists extrawordlistsplus})) ; nil (ran all night) tried each letter for 30 in turn
  (def root (make-root {:ccinfo root :extrawordlists extrawordlists :encodemap {30 \t}})) ; no sols also \r \s
  ;------------------------------------------------

  (def root (make-example-from-clues [[27] [28]]))          ; ok 676 sols all possible combos of 2 letters
  (def ans (filter
             all-completed-and-all-good?
             (tree-seq non-completed-and-all-good? (children-from-best-clue-using :wordcountscores) root)))
  ; try this if bad words as in 5x5 example above
  (def ans (filter
             (complement non-completed-some-could-be-bad?)
             (tree-seq non-completed-some-could-be-bad? (children-from-best-clue-using :wordcountscores) root)))
  (show-pdc-at-most-n ans 10))



; -------------------------- compare using regex or filters ----
; ----------------------- in general regex seems best but filters are more general
(comment
  (test-sol root make-child-rx)
  (test-sol root make-child-tr)
  (quick-bench (bench-test-sol root make-child-rx))
  (quick-bench (bench-test-sol root make-child-tr))
  (def root (make-example-from-sentence "abcd efgh ijkl mnop aeim bfjn cgko dhlp"))
  (def root (make-root {:ccinfo root :rootmap {1 \n 4 \s 13 \t 16 \o}}))
  ;     in make-child filtercode using reg-ex quick-bench (bench-test-sol root)) 4.7 ms, using transducers 80 ms
  (def ccnumber 1)
  (def root (make-root {:ccinfo (get-cc ccnumber) :rootmap {}}))
  ;     in make-child filtercode using reg-ex quick-bench (bench-test-sol root)) 23 ms, using transducers 127 ms
  (def root (make-example-from-clues [[31 32 33 34]]))      ; find all 4 letter words
  ;TODO Why, the tree is only depth 1 and wordlist has the answers rx 500 msec tr 6 secs

  (quick-bench (doall (filteredlist (comp
                                      (filter-from-partial-decoded-clue [31 32 33 34 35])
                                      (contained-all-but-one-letters-clue-filter [31 32 33 34 35] [1 31 32 33 1]))))) ;329 ms
  (def root (make-example-from-clues [[31 32 33 34 35] [1 31 32 33 1]])) ; 1.95 sec rx, ? sec tr
  nil)


;-------------------- Using rels - set up time costly but fast for specific types of searches
;TODO check if this is much of an improvement over just filtering dictionary directly
; as (count (keys by-letters)) ;=> 34766
;(count all-words-in-set) ;=> 60419
(defn words-from-letterset
  "find all words with all letters from letterset"
  [letterset]
  (let [letterset (set letterset)
        keys (keys by-letters-set)
        keystouse (filter (fn [key] (empty? (set/difference (:letters key) letterset))) keys)]
    ;(println letterset keystouse)
    (sort (flatten (seq (reduce #(conj %1 (get-by-letters (:letters %2))) #{} keystouse))))))

(defn words-meet-letterset
  "find all words with at least one letter from letterset"
  [letterset]
  (let [letterset (set letterset)
        keys (keys by-letters-set)
        keystouse (filter (fn [key] (seq (set/intersection (:letters key) letterset))) keys)]
    ;(println letterset keystouse)
    (sort (flatten (seq (reduce #(conj %1 (get-by-letters (:letters %2))) #{} keystouse))))))

; ----------- Examples using rels
(comment
  ; find all words with ta in 3rd and 4th pos and contains a z
  (set/intersection
    (set (map :word (get-by-pos-char-map {4 \a 3 \t})))
    (set (words-meet-letterset "z")))

  ; find all words using same set of letters as in "apple"
  (get-by-letters "apple")                                  ;=> ("peal" "paella" "leap" "lapel" "apple" "appeal" "plea" "pale")
  ;(quick-bench (get-by-letters "apple"))


  ; find all words with this code-cracker-patter
  (get-by-ccvec [1 2 3 3 1])                                ;=> ("shoos" "yummy" "tweet" "setts" "sills" "sells" "yukky" "yuppy")
  ; find all words with same code-cracker pattern as "level"
  (get-by-ccvec ((comp make-code-cracker-vector encode) "level"))
  ; find all words with a pattern of Vowels and Consanants
  (get-by-vcstr "VCVCVCVCVCVCV")                            ;=> ("unimaginative")
  (get-by-sortedletters (sort "warder"))                    ;=> ("redraw" "reward" "drawer" "warder") anagrams
  (set/intersection
    (set (get-by-anagram-count 4))
    (set (get-by-ccvec [1 2 3 4 5 3])))                     ;=> #{"warder" "bastes"}
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

; -------------------------------------------


