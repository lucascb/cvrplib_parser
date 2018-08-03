(ns cvrplib-parser.optimal
  (:require [clojure.string :as string]))

(defn- str->int
  "Convert the string `s` to integer"
  [s]
  (if (.contains s ".")
    (Double. s)
    (Integer. s)))

(defn- parse-cost
  "Parse the cost of the optimal solution from `file`"
  [file]
  {:cost (as-> file f
           (re-find #"[C|c]ost[\d\s]+" f)
           (string/split f #"\s")
           (second f)
           (str->int f))})

(defn- parse-routes
  "Parse the routes of the optimal solution from `file`"
  [file]
  {:routes (->> file
                (re-seq #"[R|r]oute #\d:[\d\s]+")
                (map (comp #(map str->int %)
                           #(remove empty? %)
                           #(string/split % #"\s")
                           #(string/replace % #"Route #\d:" ""))))})

(defn parse-optimal
  "Parse the optimal solution from opt `file`"
  [file]
  {:optimal (merge (parse-routes file)
                   (parse-cost file))})

(defn parse-best-known
  "Parse the Best Known Solution from sol `file`"
  [file]
  {:best-known (-> file
                   string/split-lines
                   first
                   str->int)})

