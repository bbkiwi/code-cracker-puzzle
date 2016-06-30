(ns code-cracker-puzzle.core
    (:gen-class)
    (:require
      [code-cracker-puzzle.global-vars-n-helpers :refer :all]
      [code-cracker-puzzle.bill-utils :refer :all]
      [code-cracker-puzzle.work :refer :all]
      [code-cracker-puzzle.data-assembly :refer :all]
      [code-cracker-puzzle.output-routines :refer :all]
      [code-cracker-puzzle.test-examples :refer :all]
      [clojure.edn :as edn]))




(defn -main
  "Called automatically using lein run nstr
  Note the parmeters parsed by lein run are strings"
  [nstr]
  (in-ns 'code-cracker-puzzle.core) ; lein run goes to user ns
  (let [ccnumber (edn/read-string nstr) ; (Integer. nstr) also works
        root (make-root {:ccinfo (get-cc ccnumber)})
        ans (filter
              (complement non-completed-some-could-be-bad?)
              (tree-seq non-completed-some-could-be-bad? (children-from-best-clue-using :wordcountscores) root))]
    (show-at-most-n ans 1)))


;TODO have clue be map with :vec of code number and :dic of words that match (use findall)
;TODO so speed up ranking and filtering
;(def ans (brute-solver-from-4-clues [[1 2 3 2 1],[4 5 6 4],[1 6 5], [1 5 6]] {} ))
;; this takes awhile
;(def clues [[1 2 3 2 1],[1 5 6],[6 7 8 9], [9 10 11 12]])
;(def ans (brute-solver-from-4-clues clues {}))
;(show-sol 75342 ans clues)   ;("madam mic cops stub")

; comment with line comment should be toggle ...

;various docs say arguments can be coll when it is not correct
;often don't work for maps which are colls!
;e.g. distinct, nth ...

;(coll? {:a 1 :b 33})
;=> true
;(distinct {:a 1 :b 33})  ; error!
;(nth {:a 1 :b 33} 0)     ; error!