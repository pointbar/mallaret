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
  (zipmap (->> soldes keys (map parse-personne))
          (->> soldes vals (map parse-prix))))

(defmethod parse-line :transactions
  [_ transaction]
  {:montant     (-> transaction :Montant parse-prix)
   :transaction (-> transaction :Transaction read-string)
   :soldes      (-> transaction
                    (dissoc :Montant :Transaction)
                    parse-soldes)})

(defn add-soldes
  [depenses transactions]
  (let [soldes (zipmap (map :transaction transactions)
                       (map :soldes transactions))]
    (map (fn [depense]
           (if-let [soldes (-> depense :id soldes)]
             (assoc depense :soldes soldes)
             depense))
         depenses)))

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
        :deplacements (:deplacements data)
        :transactions (add-soldes (:depenses data) (:transactions data))
        :participants (->> data :transactions first :soldes keys)))))

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
  [transaction]
  {"Id"          (-> transaction :id str)
   "Date"        (-> transaction :date format-date)
   "Payeur"      (-> transaction :payeur format-personne)
   "Titre"       (-> transaction :titre)
   "Fournisseur" (-> transaction :fournisseur)
   "Prix"        (-> transaction :prix format-prix)
   "Répartition" (-> transaction :repartition libelles-repartition)})

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

(defn format-soldes
  [soldes]
  (zipmap (->> soldes keys (map format-personne))
          (->> soldes vals (map format-prix))))

(defn format-transaction
  [transaction]
  (into {"Transaction" (-> transaction :id str)
         "Montant"     (-> transaction :prix format-prix)}
        (-> transaction :soldes format-soldes)))

(def padding-transaction
  (constantly document/lpad))

(defn format-synthese
  [synthese]
  {"Date"        (-> synthese :date format-date)
   "Titre"       (-> synthese :titre)
   "Fournisseur" (-> synthese :fournisseur)
   "Delta"       (-> synthese :delta format-prix)})

(def padding-synthese
  {"Date"        document/lpad
   "Titre"       document/rpad
   "Fournisseur" document/rpad
   "Delta"       document/lpad})

(def output-line
  {:depenses     {:format format-depense :pad padding-depense :source :transactions}
   :deplacements {:format format-deplacement :pad padding-deplacement :source :deplacements}
   :transactions {:format format-transaction :pad padding-transaction :source :transactions}})

(defn format-block
  [comptes key]
  (let [[{:keys [format pad source]} header] (map key [output-line (:headers comptes)])]
    (document/format-data header (source comptes) format pad)))

(defn update-blocks
  [comptes]
  (reduce (fn [comptes [titre clef]]
            (assoc-in comptes [:blocks titre] (format-block comptes clef)))
          comptes
          titres-blocks))

(defn update-blocks-syntheses
  [comptes]
  (assoc-in comptes [:blocks "Synthèses individuelles"]
            (mapcat (fn [[personne synthese]]
                      (cons (str "## " (format-personne personne))
                            (document/format-data ["Date" "Titre" "Fournisseur" "Delta"] synthese format-synthese padding-synthese)))
                    (:syntheses comptes))))

(defn format-comptes
  [comptes]
  (->> (:titles comptes)
       (mapcat (fn [titre] (cons (str "# " titre) (get-in comptes [:blocks titre]))))
       (string/join "\n")
       document/ensure-ends-with-newline))

(def write-comptes
  (comp format-comptes update-blocks-syntheses update-blocks))
