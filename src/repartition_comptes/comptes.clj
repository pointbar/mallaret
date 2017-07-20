(ns repartition-comptes.comptes
  (:require [clj-time.core :as time]))

(defn participants
  [transactions]
  (-> transactions first :soldes keys))

(defn renumber-depenses
  [comptes]
  (update comptes :depenses #(map (fn [depense id] (assoc depense :id id)) % (->> (range) (drop 1)))))

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

(defn update-soldes
  [soldes depense-repartie]
  (reduce (fn [soldes [personne depense]] (update soldes personne #(+ % depense))) soldes depense-repartie))

(defn soldes-initiaux
  [personnes]
  (zipmap personnes (repeat 0)))

(defn progression-depenses
  [depenses-reparties soldes-initiaux]
  (reductions update-soldes soldes-initiaux depenses-reparties))

(defn garde-soldes-modifies
  [depenses soldes]
  (select-keys soldes (keys depenses)))

(defn ajoute-id-et-montant
  [depenses soldes]
  {:transaction (:id depenses)
   :montant (:prix depenses)
   :soldes soldes})

(defn calcule-transactions
  [comptes]
  (let [depenses (-> comptes :depenses)
        depenses-reparties (map (partial repartition comptes) depenses)]
    (->> comptes :transactions participants soldes-initiaux
         (progression-depenses depenses-reparties)
         rest
         (map garde-soldes-modifies depenses-reparties)
         (map ajoute-id-et-montant depenses))))

(defn update-transactions
  [comptes]
  (assoc comptes :transactions (calcule-transactions comptes)))

(def update-comptes
  (comp update-transactions renumber-depenses))
