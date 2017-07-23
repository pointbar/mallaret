(ns repartition-comptes.document
  (:require [clojure.data.csv :as csv]
            [clojure.string :as string]))

(defn extract-title
  [line]
  (some->> line (re-find #"^#([^#].*)") second string/trim))

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
  (partial remove (comp (partial re-find #"^:?-+:?$") first)))

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

(defprotocol Pad
  (min-length [_])
  (tirets [_ longueur])
  (pad [_ string longueur]))

(defn pad-with
  [string length transfo]
  (->> (repeat " ") (concat (transfo string)) (take length) transfo (apply str)))

(defn n-tirets
  [n]
  (->> (repeat "-") (take n) (apply str)))

(def rpad
  (reify
    Pad
    (min-length [_] 3)
    (tirets [_ longueur]
      (n-tirets longueur))
    (pad [_ string length]
      (pad-with string length identity))))

(def lpad
  (reify
    Pad
    (min-length [_] 4)
    (tirets [_ longueur]
      (-> longueur dec n-tirets (str ":")))
    (pad [_ string length]
      (pad-with string length reverse))))

(defn rows->columns
  [table]
  (apply merge-with conj
         (zipmap (-> table first keys) (repeat nil))
         table))

(defn max-length
  [column min-length]
  (->> column
       (map count)
       (apply max min-length)))

(defn calcule-longueurs
  [padders table]
  (into {}
        (map (fn [[k v]] [k (max-length v (min-length (padders k)))]))
        (rows->columns table)))

(defn bracket-with-pipes
  [s]
  (str "| " s " |"))

(defn format-table
  [headers data]
  (-> (map #(->> (map % headers) (string/join " | ") bracket-with-pipes)
           data)
      (concat [""])))

(defn make-tirets
  [padders longueurs]
  (into {}
        (map (fn [[k v]] [k (tirets (padders k) v)]))
        longueurs))

(defn pad-row
  [padders longueurs row]
  (into {}
        (map (fn [[k v]] [k (pad (padders k) (row k) v)]))
        longueurs))

(defn pad-table
  [table pad-header pad-data]
  (cons (-> table first pad-header)
        (->> table rest (map pad-data))))

(defn format-data
  [headers data formatter padders]
  (let [table (->> data
                   (map formatter)
                   (concat [(zipmap headers headers)]))
        longueurs (calcule-longueurs padders table)
        table (pad-table table (partial pad-row (constantly rpad) longueurs) (partial pad-row padders longueurs))
        tirets (make-tirets padders longueurs)
        table (concat [(first table) tirets] (rest table))]
    (format-table headers table)))

(defn ensure-ends-with-newline
  [s]
  (if (= \newline (get s (-> s count dec)))
    s
    (str s "\n")))
