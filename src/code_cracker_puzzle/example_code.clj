(ns code-cracker-puzzle.example-code
    (:require
      [clojure.repl :refer :all]))

; transducer example from Colin
(comment
  (type (sequence
          (comp
            (filter #(zero? (mod % 100)))
            (filter #(> % 500)))
          (range 1 1000)))
  ; => clojure.lang.LazySeq
  (->> (range 1 1000)
       (filter #(zero? (mod % 100)))
       (filter #(> % 500))
       (type))
  ; => clojure.lang.LazySeq
  (type (transduce
          (comp
            (filter #(zero? (mod % 100)))
            (filter #(> % 500)))
          conj
          (range 1 1000))))
; => clojure.lang.PersistentVector)
