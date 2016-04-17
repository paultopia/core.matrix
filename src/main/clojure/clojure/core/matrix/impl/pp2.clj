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

(def rows [[1 20 300 4] [50 6000 77 8]])

(defn- format-num [x] (format "%.3f" (double x)))
(defn- default-formatter [x]
  (if (number? x)
    (format-num x)
    (str x)))

(defn string-helper
  "helper func to use for reduce in stringer"
  [formatter the-map the-value]
  (let [sv (formatter the-value)]
  {:max (max (:max the-map) (count sv))
   :column (conj (:column the-map) sv)}))

(defn stringer
  "convert single column to string + find max, all in one pass."
  ([vec-of-cols]
  (map
    #(reduce
      (partial string-helper default-formatter)
      {:max 0 :column []} %)
    vec-of-cols))
    ([vec-of-cols formatter]
    (map
      #(reduce
        (partial string-helper formatter)
        {:max 0 :column []} %)
      vec-of-cols)))

(defn twist
  "take map output of stringer, rotate to rows + vector of col lengths"
  [colmaps]
  {:clen (mapv :max colmaps)
    :rows (mp/transpose (mapv :column colmaps))})

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
  [row clen-vec]
  "takes a row of strings and a vector of column lengths and returns row as a padded string ending with newline"
  )

(defn pad-out [colmap]
  (let [clen (:max colmap) col (:column colmap)]
  (mapv #(append-elem % clen) col)))

(defn matpad [vec-of-maps]
  (mapv pad-out vec-of-maps))


(defn brute-force [m]
  (-> m mp/get-columns stringer matpad mp/transpose))
