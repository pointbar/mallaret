(ns repartition-comptes.comptes
  (:require [clj-time.core :as time]))

(defn renumber-depenses
  [comptes]
  (update comptes :transactions #(map (fn [depense id] (assoc depense :id id)) % (->> (range) (drop 1)))))

(defmulti repartition (fn [_ depense] (:repartition depense)))

(defn repartition-parmi
  [personnes depense]
  (let [{:keys [payeur prix]} depense
        nb-personnes (count personnes)]
    (-> (zipmap personnes (repeat (-> prix (/ nb-personnes) -)))
        (assoc payeur (-> prix (* (dec nb-personnes)) (/ nb-personnes))))))

(defmethod repartition :tous
  [comptes depense]
  (repartition-parmi (:participants comptes) depense))

(defn update-presentes
  [presents deplacement]
  ((-> deplacement :sens {:arrivee conj :depart disj}) presents (:voyageur deplacement)))

(defn presentes
  [deplacements date]
  (->> deplacements
       (filter #(not (time/after? (:date %) date)))
       (reduce update-presentes #{})))

(defmethod repartition :presentes
  [comptes depense]
  (let [{:keys [date]} depense]
    (repartition-parmi (-> comptes :deplacements (presentes date)) depense)))

(defmethod repartition :gitants
  [comptes depense]
  (repartition-parmi #{:fred :solenne :manu :xavier :agnes} depense))

(defn update-solde
  [soldes depense-repartie]
  (reduce (fn [soldes [personne depense]] (update soldes personne #(+ % depense))) soldes depense-repartie))

(defn soldes-initiaux
  [personnes]
  (zipmap personnes (repeat 0)))

(defn progression-depenses
  [depenses-reparties soldes-initiaux]
  (reductions update-solde soldes-initiaux depenses-reparties))

(defn update-deltas
  [comptes]
  (update comptes :transactions (partial map #(assoc % :deltas (repartition comptes %)))))

(defn update-soldes
  [comptes]
  (update comptes :transactions
          (fn [transactions]
            (->> comptes :participants soldes-initiaux
                 (progression-depenses (map :deltas transactions))
                 rest
                 (map #(assoc %1 :soldes %2) transactions)))))

(defn update-transactions
  [comptes]
  (-> comptes
      update-deltas
      update-soldes))

(defn synthese-personne
  [transactions personne]
  (->> transactions
       (map (fn [t] {:date        (:date t)
                     :titre       (:titre t)
                     :fournisseur (:fournisseur t)
                     :delta       (-> t :deltas personne)}))
       (filter :delta)))

(defn update-syntheses-personnes
  [comptes]
  (let [participants (:participants comptes)]
    (assoc comptes :syntheses (zipmap participants
                                      (map (partial synthese-personne (:transactions comptes)) participants)))))

(def update-comptes
  (comp update-syntheses-personnes update-transactions renumber-depenses))
