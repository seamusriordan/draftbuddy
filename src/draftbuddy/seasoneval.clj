(ns draftbuddy.seasoneval)

(require [ 'draftbuddy.core        :as 'core ] 
         [ 'draftbuddy.selectmeths :as 'sel  ]
         [ 'draftbuddy.draftengine :as 'de   ]
         [ 'clojure.string         :as 'cstr ])


(defn average-remaining-draft
  [roster pool]
  nil
)

(defn cull-bye
   [roster week]
   (reduce-kv 
     (fn [m k v] (assoc m k (vec (remove #(= (% :bye) week) v)) ))
     {} roster )
)

(defn fullroster?
   [sroster]
   (let [pcount (zipmap (keys sroster) (map #(count (sroster %)) (keys sroster)))
         slotkeys (keys de/starting-roster-struct) ]
      (not-any? false? (mapv #(<= (de/starting-roster-struct %) (pcount %)) slotkeys ))
		)
)


(defn openslots
  [sroster]
    (reduce-kv (fn [keyset k v]
          (if (< (count v) (de/starting-roster-struct k))
            (conj keyset k)
            keyset))
           #{} sroster)
)


(defn player-max-points
   [pset]
   (if (zero? (count pset))
     nil
	  (apply max-key :points pset)
))

(defn player-subset
   [roster pos-set]
   (loop [slots (keys roster)
          subset [] ] 
     (if (nil? slots)
       subset
			 (if (contains? pos-set (first slots))
					(recur (next slots) (concat (roster (first slots)) subset))
					(recur (next slots) subset)
       )
			)
))

(defn deg-points
  [pos players]
  (let [deg-factor-map {:qb (concat [0.2  0.0001 0.00001 0.000001] (repeat 1e-7))
                        :rb (concat [1.0  0.50 0.20  0.20] (repeat 1e-3))
												:wr (concat [1.0  0.40 0.20  0.10] (repeat 1e-3))
												:te (concat [1.0  0.2  0.01 0.005] (repeat 1e-4))}
        deg-factor      (deg-factor-map pos (repeat 0.0)) ]
    (reduce-kv (fn [m k v]
                 (conj m (update-in v [:points] #(* % (nth deg-factor k)))))
             [] players)
    )
  )

(defn degrade-bench-points
  [bench-roster]
    (loop [k (keys bench-roster) m bench-roster]
      (if (nil? k)
        m
        (let [new-m (update-in m [(first k)] #(deg-points (first k) %) )]
          (recur (next k) new-m)
          ))))

(defn optimized-startingroster
   [roster]
   (let [sroster-keys (keys de/starting-roster-struct) ]

   (loop [ sroster    (reduce #(assoc %1 %2 []) {} sroster-keys )
           remaining  roster]

       (let [all-open-slots   (openslots sroster) 
             main-pos-to-fill (filterv #(not= :flex %) all-open-slots) 
             flex-pos-to-fill (filterv #(= :flex %)    all-open-slots)
             pos-to-fill      (first (concat main-pos-to-fill flex-pos-to-fill))]

         (if (nil? pos-to-fill)
           ; No more positions to fill
           [sroster (degrade-bench-points remaining)]
           (let  [p (player-max-points (player-subset remaining (core/pos-allowed pos-to-fill)))]
             (if (nil? p)
								 ; No more players to fill this position ->  Add placeholder
							 (recur (assoc sroster pos-to-fill (conj (sroster pos-to-fill) {:name "No available" :points 0 :pos :bad})) 
											remaining)

							 (recur (assoc sroster pos-to-fill (conj (sroster pos-to-fill) p)) 
											(de/removeplayer remaining [ (p :pos) p ] ))
             )
))))))

(defn points-weekn 
   [roster week]
   (let [weeknroster (cull-bye roster week) 
         opt-roster (optimized-startingroster weeknroster)]
     (+ 
       ( apply + (map :points (apply concat (vals (first  opt-roster)) )))
       ( apply + (map :points (apply concat (vals (second opt-roster)) )))
       )
	 )
)

(defn eval-team-season
  [roster]
;  (let [weeknroster (cull-bye roster 1) 
;         opt-roster (optimized-startingroster weeknroster)]
;     (println "Projected week 1 roster"  (map :name (apply concat (vals (first opt-roster)))))
;     (println "Weekly points " (mapv (partial points-weekn roster) (range 1 (inc core/nweeks)))))

  (mapv (partial points-weekn roster) (range 1 (inc core/nweeks)))
)

(defn eval-season
  [roster]
  (mapv #(/ (apply + (eval-team-season %)) 17) roster)
  
)