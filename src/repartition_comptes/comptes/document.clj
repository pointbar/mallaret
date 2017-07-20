(ns repartition-comptes.comptes.document
  (:require
    [clj-time.core :as time]
    [clj-time.local :as time-local]
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as string]
    [repartition-comptes.document :as document]))

(defn current-year
  []
  (-> (time-local/local-now) time/year))

(defn date
  [day month]
  (time/local-date (current-year) month day))

(defn parse-date
  [date-string]
  (->> date-string
       (re-find #"0*([1-9][0-9]*)/0*([1-9][0-9]*)")
       rest
       (map document/read-integer)
       (apply date)))

(def parse-personne
  keyword)

(def sens
  {"arrivée" :arrivee
   "départ"  :depart})

(def libelles-sens
  (set/map-invert sens))

(defmulti parse-line (fn [type _] type))

(defmethod parse-line :deplacements
  [_ x]
  {:date     (-> x :Date parse-date)
   :voyageur (-> x :Voyageur parse-personne)
   :sens     (-> x :Sens sens)})

(defn parse-prix
  [price-string]
  (when (seq price-string)
    (-> price-string
        (string/split #"[,.]")
        (->> (string/join "."))
        bigdec
        rationalize)))

(def repartitions
  {"Tout le monde"   :tous
   "Les présent/e/s" :presentes
   "Les gîtants"     :gitants})

(def libelles-repartition
  (set/map-invert repartitions))

(defmethod parse-line :depenses
  [_ depense]
  (let [{:keys [Id Date Payeur Titre Fournisseur Prix Répartition]} depense]
    {:id          (document/read-integer Id)
     :date        (parse-date Date)
     :payeur      (parse-personne Payeur)
     :titre       Titre
     :fournisseur Fournisseur
     :prix        (parse-prix Prix)
     :repartition (repartitions Répartition)}))

(defn parse-soldes
  [soldes]
  (into {}
        (map (fn [[personne solde]]
               [(parse-personne personne) (parse-prix solde)])
             soldes)))

(defmethod parse-line :transactions
  [_ transaction]
  {:montant     (-> transaction :Montant parse-prix)
   :transaction (-> transaction :Transaction read-string)
   :soldes      (-> transaction
                    (dissoc :Montant :Transaction)
                    parse-soldes)})

(def titres-blocks
  {"Dépenses"            :depenses
   "Arrivées et départs" :deplacements
   "Soldes"              :transactions})

(defn read-comptes
  [f]
  (with-open [rdr (io/reader f)]
    (let [comptes (-> rdr line-seq document/parse-blocks)
          titres (keys titres-blocks)
          tables (map (fn [titre]
                        (->> (get-in comptes [:blocks titre])
                             (remove empty?)
                             (map document/trim-table-line)
                             (string/join "\n")
                             document/parse-table))
                      titres)
          clefs (map titres-blocks titres)
          headers (zipmap clefs (map :header tables))
          data (zipmap clefs (map #(map (partial parse-line %1) (:data %2)) clefs tables))]
      (assoc comptes
        :headers headers
        :tables data))))

(defn format-date
  [local-date]
  (.toString local-date "dd/MM"))

(defn format-prix
  [prix]
  (when prix
    (format "%.2f" (double prix))))

(def format-personne
  name)

(defn format-depense
  [depense]
  {"Id"          (-> depense :id str)
   "Date"        (-> depense :date format-date)
   "Payeur"      (-> depense :payeur format-personne)
   "Titre"       (-> depense :titre)
   "Fournisseur" (-> depense :fournisseur)
   "Prix"        (->> depense :prix format-prix)
   "Répartition" (-> depense :repartition libelles-repartition)})

(def padding-depense
  {"Id"          document/lpad
   "Date"        document/lpad
   "Payeur"      document/rpad
   "Titre"       document/rpad
   "Fournisseur" document/rpad
   "Prix"        document/lpad
   "Répartition" document/rpad})

(defn format-deplacement
  [deplacement]
  {"Date"     (-> deplacement :date format-date)
   "Voyageur" (-> deplacement :voyageur format-personne)
   "Sens"     (-> deplacement :sens libelles-sens)})

(def padding-deplacement
  {"Date"     document/lpad
   "Voyageur" document/rpad
   "Sens"     document/rpad})

(defn format-transaction
  [transaction]
  (into {"Transaction" (-> transaction :transaction str)
         "Montant"     (-> transaction :montant format-prix)}
        (->> transaction :soldes (map (fn [[personne solde]] [(format-personne personne)
                                                              (format-prix solde)])))))

(def padding-transaction
  (constantly document/lpad))

(def output-line
  {:depenses     {:format format-depense :pad padding-depense}
   :deplacements {:format format-deplacement :pad padding-deplacement}
   :transactions {:format format-transaction :pad padding-transaction}})

(defn format-block
  [comptes key]
  (let [[{:keys [format pad]} header data] (map key [output-line (:headers comptes) (:tables comptes)])]
    (document/format-data header data format pad)))

(defn update-blocks
  [comptes]
  (reduce (fn [comptes [titre clef]]
            (assoc-in comptes [:blocks titre] (format-block comptes clef)))
          comptes
          titres-blocks))

(defn format-comptes
  [comptes]
  (->> (:titles comptes)
       (mapcat (fn [titre] (cons (str "# " titre) (get-in comptes [:blocks titre]))))
       (string/join "\n")
       document/ensure-ends-with-newline))

(def write-comptes
  (comp format-comptes update-blocks))