(ns repartition-comptes.document
  (:require [clojure.data.csv :as csv]
            [clojure.string :as string]))

(defn extract-title
  [line]
  (some->> line (re-find #"^#(.*)") second string/trim))

(defn parse-blocks
  [lines]
  (-> (reduce (fn [state line]
                (if-let [title (extract-title line)]
                  (-> state
                      (assoc :key title)
                      (update :titles #(conj % title)))
                  (update-in state [:blocks (:key state)] #(conj (or % []) line))))
              {:titles []}
              lines)
      (dissoc :key)))

(defn read-csv
  [rdr]
  (csv/read-csv rdr :separator \|))

(def trim-cells
  (partial map (partial map string/trim)))

(defn trim-table-line
  [l]
  (->> l
       (re-find #"^\s*\|?(.*?)\|?\s*$")
       second))

(def remove-dashes
  (partial remove (comp (partial re-find #"^-+$") first)))

(defn csv-data->maps
  [csv-data]
  (let [header (first csv-data)]
    {:header header
     :data   (map zipmap
                  (->> header (map keyword) repeat)
                  (rest csv-data))}))

(def parse-table
  (comp
    csv-data->maps
    remove-dashes
    trim-cells
    read-csv))

(defn read-integer
  [s]
  (when-not (string/blank? s)
    (Integer/parseInt s)))

(defn pad
  [string length transfo]
  (->> (repeat " ") (concat (transfo string)) (take length) transfo (apply str)))

(defn rpad
  [string length]
  (pad string length identity))

(defn lpad
  [string length]
  (pad string length reverse))

(defn rows->columns
  [table]
  (apply merge-with conj
         (zipmap (-> table first keys) (repeat nil))
         table))

(defn max-length
  [column]
  (->> column
       (map count)
       (apply max 3)))

(defn calcule-longueurs
  [table]
  (into {} (map (fn [[k v]] [k (max-length v)])) (rows->columns table)))

(defn bracket-with-pipes
  [s]
  (str "| " s " |"))

(defn format-table
  [headers data]
  (-> (map #(->> (map % headers) (string/join " | ") bracket-with-pipes)
           data)
      (concat [""])))

(defn make-tirets
  [longueurs]
  (into {} (map (fn [[k v]] [k (->> (repeat "-") (take v) (apply str))])
                longueurs)))

(defn pad-row
  [padders longueurs row]
  (into {} (map (fn [[k v]] [k ((padders k) (row k) v)])
                longueurs)))

(defn pad-table
  [table pad-header pad-data]
  (cons (-> table first pad-header)
        (->> table rest (map pad-data))))

(defn format-data
  [data formatter padders]
  (let [headers (:header data)
        table (->> data
                   :data
                   (map formatter)
                   (concat [(zipmap headers headers)]))
        longueurs (calcule-longueurs table)
        table (pad-table table (partial pad-row (constantly rpad) longueurs) (partial pad-row padders longueurs))
        tirets (make-tirets longueurs)
        table (concat [(first table) tirets] (rest table))]
    (format-table headers table)))

(defn ensure-ends-with-newline
  [s]
  (if (= \newline (get s (-> s count dec)))
    s
    (str s "\n")))
