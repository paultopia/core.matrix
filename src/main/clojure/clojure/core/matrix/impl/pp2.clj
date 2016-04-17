(ns clojure.core.matrix.impl.pp2
  (:require [clojure.core.matrix.protocols :as mp])
  (:import [java.lang StringBuilder]
           [clojure.lang IPersistentVector]))

(def cols [[1 20 300 4] [50 6000 77 8]])

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
