(ns clojure.core.matrix.impl.pp2
  (:require [clojure.core.matrix.protocols :as mp]
    [clojure.core.matrix.impl.persistent-vector]
    [clojure.core.matrix.impl.sequence]
    [clojure.core.matrix.impl.defaults :as default]
    [clojure.core.matrix.implementations :as imp :refer [*matrix-implementation*]]
    [clojure.core.matrix.impl.mathsops :as mops]
    [clojure.core.matrix.impl.wrappers :as wrap]
    [clojure.core.matrix.utils :as u]
    [clojure.core.matrix.impl.index]
    [clojure.core.matrix.impl.pprint :as pprint]
    [clojure.core.matrix.impl.double-array]
    [clojure.core.matrix.impl.object-array])
  (:import [java.lang StringBuilder]
           [clojure.lang IPersistentVector]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^String NL (System/getProperty "line.separator"))

(defn- format-num [x] (format "%.3f" (double x)))

(defn- default-formatter [x]
  (if (number? x)
    (format-num x)
    (str x)))

(defn- append-elem
  "Appends an element, right-padding up to a given column length."
  [^String elem ^long clen]
  (let [c (long (count elem))
        ws (- clen c)
        sb (StringBuilder.)]
    (dotimes [i ws]
      (.append sb \space))
    (.append sb elem)
    (.toString sb)))

(defn pad-row
  "takes a row of strings and a vector of column lengths and returns row as a padded string ending with newline"
    [row clen-vec]
  (let [x (StringBuilder.)]
    (do
      (.append x \[)
      (loop [elems row
             sizes clen-vec]
        (.append x (append-elem (first elems) (first sizes)))
        (if (seq (rest sizes))
          (do
            (.append x " ")
            (recur (rest elems) (rest sizes)))
          (do (.append x \])
              (.toString x)))))))

(defn combine-rowstrings
  [v prefix]
  "vector of rowstrings --> one big string"
  (let [x (StringBuilder.) pre (or prefix "")]
  (do
    (.append x \[)
    (.append x pre)
    (loop [vtr v]
      (.append x (first vtr))
      (if (seq (rest vtr))
        (do
          (.append x NL)
          (.append x pre)
          (.append x " ")
          (recur (rest vtr)))
        (do (.append x \])
        (.toString x)))))))

(defn stringme [twisted]
  "twisted map --> vector of vectors-as-strings"
  (let [cv (:maxvec twisted) rw (:rows twisted)]
    (mapv #(pad-row % cv) rw)))


(defn process [maxvec row]
  (loop [mv maxvec r row nm [] nr []]
(let [s (str (first r))]
(if (seq (rest r))
(recur (rest mv) (rest r) (conj nm (max (first mv) (count s))) (conj nr s))
{:mv (conj nm (max (first mv) (count s))) :r (conj nr s)})
)))

(defn chomp [prev-row-map newrow]
(let [results (process (:maxvec prev-row-map) newrow)]
{:maxvec (:mv results) :rows (conj (:rows prev-row-map) (:r results))}))

(defn makestring [rowvec]
(reduce chomp {:maxvec (repeat 0) :rows []} rowvec))

(defn makeprint
  ([m]
   (makeprint m nil))
  ([m prefix]
  (combine-rowstrings (stringme (makestring m)) prefix)))


(def rows2 [[1 20 300 4] [50 6000 77 8] [90 100 110 122]])

;; TODO:
;; make to handle formatter 
;; make to handle 0 and 1-dim input (just str it)
;; add optimizations from original (type hints)

;; formatter goes from main -- makestring -- chomp -- process

;; prefix doesn't cover first line, need to fix that. 

