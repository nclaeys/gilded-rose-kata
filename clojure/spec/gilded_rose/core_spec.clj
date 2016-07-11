(ns gilded-rose.core-spec
  (:require [clojure.test :refer :all]
            [gilded-rose.core :as rose]))

(def cases
  {["+5 Dexterity Vest" 10 20] [9 19]
   ["+5 Dexterity Vest" 0 20] [-1 18]
   ["+5 Dexterity Vest" 0 1] [-1 0]
   ["Aged Brie" 2 0] [1 1]
   ["Aged Brie" 2 50] [1 50]
   ["Elixir of the Mongoose" 5 7] [4 6]
   ["Sulfuras, Hand Of Ragnaros" 0 80] [-1 80]
   ["Backstage passes to a TAFKAL80ETC concert" 15 20] [14 21]
   ["Backstage passes to a TAFKAL80ETC concert" 9 20] [8 22]
   ["Backstage passes to a TAFKAL80ETC concert" 2 20] [1 23]
   ;; original core fails this test. Is this bug or feature?
   ["Backstage passes to a TAFKAL80ETC concert" 2 50] [1 50]})

(deftest basic-cases
  (doseq [[initial [sell-in quality]] cases
          :let [item (apply rose/item initial)
                updated (rose/update-item item)]]
    (is (= {:sell-in sell-in
            :quality quality}
           (select-keys updated [:sell-in :quality]))
        (pr-str "Error with item" {:item item :updated updated}))))
