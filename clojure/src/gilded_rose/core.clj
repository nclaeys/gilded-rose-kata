(ns gilded-rose.core)

(def rules
  '[[true
     [:change-sell-in -1]]

    [(and (= "Backstage passes to a TAFKAL80ETC concert" :name) (< :sell-in 0) )
     [:set-quality 0]]

    [(= :name "Aged Brie")
     [:change-quality 1]]

    [(and (= "Backstage passes to a TAFKAL80ETC concert" :name) (<= 0 :sell-in 4))
     [:change-quality 3]]

    [(and (= "Backstage passes to a TAFKAL80ETC concert" :name) (<= 5 :sell-in 9))
     [:change-quality 2]]

    [(and (= "Backstage passes to a TAFKAL80ETC concert" :name) (<= 10 :sell-in))
     [:change-quality 1]]

    [(= "Sulfuras, Hand of Ragnaros" :name)
     [:change-sell-in 0]]

    [(re-find #"(?i)conjured" :name)
     [:double-change-quality]]

    [(<= 0 :sell-in)
     [:change-quality -1]]

    [(< :sell-in 0)
     [:change-quality -2]]

    [(< :quality 0)
     [:set-quality 0]]

    [(and (< 50 :quality))
     [:set-quality 50]]

    [(= "Sulfuras, Hand Of Ragnaros" :name)
     [:set-quality 80]]])

(def allowed-predicates '#{and or not < > <= >= = not= re-find})

(defn and-f [& args] (every? identity args))

(defn or-f [& args] (some identity args))

(def mapped-predicates {'and and-f 'or or-f})

(defn eval-predicate
  [item p]
  (cond
    (list? p) (apply (eval-predicate item (first p))
                     (map (partial eval-predicate item) (rest p)))
    (keyword? p) (get item p)
    (mapped-predicates p) (mapped-predicates p)
    (symbol? p) (if (allowed-predicates p) (resolve p) (throw ("Unsupported predicate")))
    :else p))

(defn apply-action
  [[item modifiers] [pred action]]
  (if (eval-predicate item pred)
    (case (first action)
      :double-change-quality [item (conj modifiers :double-change-quality)]
      :change-quality (if (:changed-quality modifiers)
                        [item modifiers]
                        [(update-in item [:quality] + (if (:double-change-quality modifiers)
                                                        (* 2 (second action))
                                                        (second action)))
                         (conj modifiers :changed-quality)])
      :change-sell-in [(update-in item [:sell-in] + (second action))
                       modifiers]
      :set-quality [(assoc item :quality (second action))
                    modifiers])
    [item modifiers]))

(defn update-item
  [item]
  (-> (reduce apply-action [item #{}] rules)
      (first)))

(defn update-quality
  [items]
  (map update-item items))

(defn item [item-name, sell-in, quality]
  {:name item-name, :sell-in sell-in, :quality quality})

(defn update-current-inventory []
  (let [inventory
        [(item "+5 Dexterity Vest" 10 20)
         (item "Aged Brie" 2 0)
         (item "Elixir of the Mongoose" 5 7)
         (item "Sulfuras, Hand Of Ragnaros" 0 80)
         (item "Backstage passes to a TAFKAL80ETC concert" 15 20)]]
    (update-quality inventory)))
