(ns cvrplib-parser.parser
  (:require [clojure.string :as string]))

;;;; Utils
(defn- str->int
  "Convert the string `s` to integer"
  [s]
  (if (.contains s ".") (Double. s) (Integer. s)))

(defn- str->key
  "Convert the string `s` to keyword"
  [s]
  (-> s
      string/lower-case
      (string/replace #"_" "-")
      keyword))

;;;; Parse instance specifications
(def ^:private data-types {:name str
                           :comment str
                           :type str
                           :dimension str->int
                           :capacity str->int
                           :edge-weight-type str
                           :edge-weight-format str
                           :edge-data-format str
                           :node-coord-type str
                           :display-data-type str})

(defn- convert-field
  "Convert the field based on its datatype"
  [field value]
  ((field data-types) value))

(defn- parse-spec
  "Parse each spec and create a hash map"
  [spec]
  (let [[k v] (string/split spec #":" 2)
        key (str->key (string/trim k))]
    (if-not (contains? data-types key)
      {}
      (hash-map key
                (convert-field key (-> v
                                       (string/replace #"\"" "")
                                       (string/trim)))))))

(defn- parse-specs
  "Parse the specifications of the instance file"
  [file]
  (let [specs (re-seq #"[A-Z_\s]+:.+" file)]
    (apply merge (map (comp parse-spec string/trim) specs))))

;;;; Parse instance data
#_(defn- parse-line
  "Parse a coordinate from a section"
  [coord]
  (map str->int (string/split coord #" ")))

#_(defn- parse-section-lines
  "Parse each section lines"
  [specs lines data]
  (let [start (inc (.indexOf lines data))
        dim (:dimension specs)
        end (+ start dim)
        data-lines (subvec (vec lines) start end)]
    (if (zero? start) ; The data was not found in the file
      {}
      (hash-map (str->key data)
                (map parse-line data-lines)))))

(defn- parse-depot-coords
  "Parse the depot coords from file `f`"
  [f]
  (when-let [depot (re-find #"DEPOT_SECTION[\n\s]+[\d.]+ [\d.]+" f)]
    (as-> depot d
      (string/split d #"[\s\n]")
      (rest d)
      (remove empty? d)
      (mapv str->int d))))

(defn- parse-depot-section
  "If depot coordinates is given on depot section (e.g. CMT and Golden instances),
  parse it from file `f`. Return an empty `clojure.core/hash-map` otherwise"
  [f]
  (if-let [coords (parse-depot-coords f)]
    {:node-coord-section [(into [0] coords)],
     :demand-section [[0 0]]}
    {}))

(defn- parse-vehicles
  "Parse the min. number of vehicle from file `f`, if exists"
  [f]
  (if-let [v (re-find #"trucks: \d+|VEHICLES: \d+" f)]
    {:vehicles (-> v
                   (string/replace #"\w+:\s" "")
                   str->int)}
    {}))

(defn- parse-section
  "Parse each line of section `s` from file `f`"
  [f s]
  (let [re (re-pattern (str s "[\\d\\s\\n.-]+"))
        sl (re-find re f)]
    (if-not sl
      {}
      {(str->key s) (->> sl
                         string/split-lines
                         rest
                         (map (comp #(map str->int %)
                                    #(remove empty? %)
                                    #(string/split % #"\s")
                                    string/trim)))})))

(defn- parse-weight-section
  "Parse each line of weight section from file `f`"
  [f]
  (let [s (re-find #"EDGE_WEIGHT_SECTION[\d\s\n]+" f)]
    (if-not s
      {}
      {:edge-weight-section (->> s
                                 string/split-lines
                                 (map string/trim)
                                 rest
                                 (map #(string/split % #"\s"))
                                 flatten
                                 (remove empty?)
                                 (map str->int))})))

(defn- parse-data
  "Parse the data of the instance file"
  [file]
  (merge-with concat
              (parse-depot-section file)
              (parse-vehicles file)
              (parse-section file "NODE_COORD_SECTION")
              (parse-section file "DEMAND_SECTION")
              (parse-weight-section file)))

(defn parse
  "Parse the CVRPlib instance `f` and return a `clojure.core/hash-map`"
  [f]
  (let [file (string/replace f #"[\t\r]" " ")]
    (merge (parse-specs file)
           (parse-data file))))

