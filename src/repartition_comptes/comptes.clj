(ns repartition-comptes.comptes
  (:require [clj-time.core :as time]))

(defn participants
  [transactions]
  (-> transactions first :soldes keys))

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
  (repartition-parmi (-> comptes :transactions participants) depense))

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

(defn garde-soldes-modifies
  [depenses soldes]
  (select-keys soldes (keys depenses)))

(defn update-deltas
  [comptes]
  (update comptes :transactions (partial map #(assoc % :deltas (repartition comptes %)))))

(defn update-soldes
  [transactions]
  (let [deltas (map :deltas transactions)]
    (->> transactions participants soldes-initiaux
         (progression-depenses deltas)
         rest
         (map garde-soldes-modifies deltas)
         (map #(assoc %1 :soldes %2) transactions))))

(defn update-transactions
  [comptes]
  (-> comptes
      update-deltas
      (update :transactions update-soldes)))

(def update-comptes
  (comp update-transactions renumber-depenses))
