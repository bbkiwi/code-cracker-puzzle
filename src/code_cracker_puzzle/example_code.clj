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

; examples using io.aviso.ansi and io.aviso.columns for colored text and nice column output
(comment
  (let [customers [{:first-name "Bill" :last-name "Baritompa" :age 71}
                   {:last-name (ioa/red "foo") :first-name {:a 3 :b [55 100 233] :age 0}}]
        formatter (col/format-columns [:right 10] ", " [:left 10] ": " :none)]
    (col/write-rows *out* formatter [:last-name :first-name :age] customers)

    (col/write-rows
      *out*
      [:last-name (str ", " ioa/red-font) :first-name (str ioa/reset-font ": ") [:age :none]]
      customers))
  (col/write-rows
    *out*
    [:clues (str ", " ioa/red-font) :partialwords (str ioa/reset-font ": ") [:wordcountscores :none]]
    [{:clues 3 :partialwords "dfadf" :wordcountscores 45}])
  (col/write-rows
    *out*
    [#(nth % 0) (str ", " ioa/red-font) #(nth % 1) (str ioa/reset-font ": ") [#(nth % 2) :none]]
    [[3 4 5] [300 -33 22.33]]))

; partial in clojure.core is faster as it does special small cases separately
(defn mypartial
  ([f & args]
   (fn [& more]
     (let [arglist (concat args more)]
       (println arglist)
       (apply f arglist))))
  ([f] f))

