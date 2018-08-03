(ns cvrplib-parser.core
  (:require [cvrplib-parser.parser :as parser]
            [cvrplib-parser.distances :as distances]
            [cvrplib-parser.optimal :as optimal]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.string :as string])
  (:gen-class))

(def json-options {:pretty true, :key-fn #(string/replace (name %) #"-" "_")})

(defn- write-json!
  "Write a JSON file which represents the instance `x`"
  [x]
  (let [fout (str "./out/" (:name x) ".json")]
    (io/make-parents fout)
    (spit fout (json/generate-string x json-options))))

(defn- parse-sol-file
  "Parse the sol file `f`"
  [f sol]
  (let [fname (string/replace (str f) #".vrp" ".sol")
        [fsol] (filter #(.endsWith (str %) fname) sol)]
    (if-not fsol
      {}
      (optimal/parse-best-known (slurp fsol)))))

(defn- parse-opt-file
  "Parse the opt file `f`"
  [f opt]
  (let [fname (string/replace (str f) #".vrp" ".opt")
        [fopt] (filter #(.endsWith (str %) fname) opt)]
    (if-not fopt
      {}
      (optimal/parse-optimal (slurp fopt)))))

(defn parse-vrp-file
  "Parse the vrp file `f`"
  [f]
  (-> f
      slurp
      parser/parse
      distances/create-distances))

(defn- parse-file
  "Parse the instance files `f`, `opt` and `sol` to produce a JSON file"
  [f opt sol]
  (println "Parsing" (str f))
  (merge (parse-vrp-file f)
         (parse-opt-file f opt)
         (parse-sol-file f sol)))

(defn- parse-files
  "Read a specific cvrplib file or all files within a specific `path`"
  [path]
  (println "Parsing all files on" path)
  (let [fs (file-seq (io/file path))
        vrp (filter #(.endsWith (str %) ".vrp") fs)
        opt (filter #(.endsWith (str %) ".opt") fs)
        sol (filter #(.endsWith (str %) ".sol") fs)]
    (doseq [f vrp]
      (write-json! (parse-file f opt sol)))))

(defn -main
  "Read the directory or file from `args` and parse it"
  [& args]
  (parse-files (first args)))
