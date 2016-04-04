(ns clojure.core.matrix.impl.pp2
  (:require [clojure.core.matrix.protocols :as mp])
  (:import [java.lang StringBuilder]
           [clojure.lang IPersistentVector]))

(defn longest-row [column]
  (apply max (map (comp count str) column)))

(defn all-longest-rows [m]
  (mapv longest-row (mp/get-columns m)))
