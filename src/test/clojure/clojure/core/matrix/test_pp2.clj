(ns clojure.core.matrix.test-pp2
  (:require [clojure.core.matrix.impl.pp2 :as pp]
            [clojure.test :refer :all]))

(def rows [[1 20 300 4] [50 6000 77 8] [90 100 110 122]])

(deftest formatted-pprint
  (is (= 
        "[[ 1   20 300   4]\n [50 6000  77   8]\n [90  100 110 122]]"
        (pp/makeprint rows {:formatter str}))))

(deftest prefix-formatted-pprint
  (is (= 
        "[*[ 1   20 300   4]\n* [50 6000  77   8]\n* [90  100 110 122]]"
        (pp/makeprint rows {:prefix "*" :formatter str}))))

(deftest prefix-unformatted-pprint
  (is (= 
        "[*[ 1.000   20.000 300.000   4.000]\n* [50.000 6000.000  77.000   8.000]\n* [90.000  100.000 110.000 122.000]]"
        (pp/makeprint rows {:prefix "*"}))))

(deftest default-formatted-pprint
  (is (= 
        "[[ 1.000   20.000 300.000   4.000]\n [50.000 6000.000  77.000   8.000]\n [90.000  100.000 110.000 122.000]]"
        (pp/makeprint rows))))
