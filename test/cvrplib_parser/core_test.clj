(ns cvrplib-parser.core-test
  (:require [clojure.test :refer :all]
            [cvrplib-parser.core :refer :all]))

(def a-n32-k5 "./cvrplib/Set A/A-n32-k5.vrp")
#_(def x-n101-k25 "./cvrplib/Set X/X-n101-k25.vrp")
#_(def e-n13-k4 "./cvrplib/Set E/E-n13-k4.vrp")
#_(def f-n135-k7 "./benchs/Fisher [1994]/Set F/F-n135-k7.vrp")
#_(def cmt2 "./benchs/Christofides, Mingozzi and Toth [1979]/CMT2.vrp")

(deftest parser-test
  (testing "Parser"
    (let [inst-1 (parse-vrp-file a-n32-k5)]
      (testing "with instance containing node coord section"
        (is (contains? inst-1 :node-coord-section))
        (is (seq? (:node-coord-section inst-1)))
        (is (= (count (:node-coord-section inst-1)) (:dimension inst-1)))
        (is (every? #(= (count %) 3) (:node-coord-section inst-1)))
        (is (every? integer? (flatten (:node-coord-section inst-1)))))
      (testing "with matrix constructed based on node coord section"
        (is (contains? inst-1 :distances))
        (is (seq? (:distances inst-1)))
        (is (= (count (:distances inst-1)) (:dimension inst-1)))
        (is (every? #(= (count %) (:dimension inst-1)) (:distances inst-1)))
        (is (every? integer? (flatten (:distances inst-1))))))))
