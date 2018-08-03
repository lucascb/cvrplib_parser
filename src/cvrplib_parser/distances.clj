(ns cvrplib-parser.distances
  (:require [clojure.string :as string]))

;; Create distances matrix from EDGE_WEIGHT_SECTION

(defn- create-lower-triangle
  "Group each value from `xs` to form a lower triangular sequence"
  ([xs] (create-lower-triangle [] xs 0))
  ([t xs n]
   (if (empty? xs)
     t
     (recur (conj t (take n xs))
            (drop n xs)
            (inc n)))))

(defn- create-upper-triangle
  "Group each value from `xs` to form an upper triangular sequence"
  ([xs] (create-upper-triangle [nil] xs 1))
  ([t xs n]
   (if (empty? xs)
     (reverse t)
     (recur (conj t (take-last n xs))
            (drop-last n xs)
            (inc n)))))

(defn- create-distances-matrix
  "Create the distance matrix from the sequence `xs`"
  [xs]
  (map concat (create-lower-triangle xs) (repeat [0]) (create-upper-triangle xs)))

;; Create distances matrix from NODE_COORD_SECTION

(defn- euclidean-distance
  "Calculate the euclidean distance between node `n1` and `n2`"
  [n1 n2]
  (let [[_ x1 y1] n1
        [_ x2 y2] n2
        xd (- x1 x2)
        yd (- y1 y2)]
    (Math/round (Math/sqrt (+ (* xd xd) (* yd yd))))))

(defn- calculate-distances-matrix
  "Calculate the distances between every node on `coords`"
  [coords]
  (let [c (sort-by first coords)]
    (for [n1 c] (for [n2 c] (euclidean-distance n1 n2)))))

(defn create-distances
  "Create the distance matrix of `instance`"
  [instance]
  (cond (contains? instance :node-coord-section)
        (merge instance
               {:distances (calculate-distances-matrix (:node-coord-section instance))})
        (contains? instance :edge-weight-section)
        (merge instance
               {:distances (create-distances-matrix (:edge-weight-section instance))})
        :else instance))

