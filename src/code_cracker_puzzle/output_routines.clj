(ns code-cracker-puzzle.output-routines
    (:gen-class)
    (:require
      [code-cracker-puzzle.bill-utils :refer :all]
      [clojure.walk]
      [clojure.repl :refer :all]
      [io.aviso.ansi :as ioa]
      [io.aviso.columns :as col]
      [criterium.core :refer [quick-bench]]))

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

(defn show-pdc-at-most-n
  "times and shows info about up to nmax solutions in ans"
  [ans nmax]
  (let [sols (take nmax ans)
        nshow (count sols)]
    (doseq [n (range nshow)]
      (println n ((nth ans n) :partialwords)))))



