(ns draftbuddy.seasoneval)

(require [ 'draftbuddy.core        :as 'core ] 
         [ 'clojure.string         :as 'cstr ])


(defn average-remaining-draft
  [roster pool]
  nil
)


(defn cull-bye
   [roster week]
		(vec (remove #(= (% :bye) week) roster)) 
)




(defn openslots
  [sroster]
    (reduce-kv (fn [keyset k v]
          (if (< (count v) (core/starting-roster-struct k))
            (conj keyset k)
            keyset))
           #{} sroster)
)


(defn player-max-points
   [pset]
   (if (empty? pset)
     nil
	  (apply max-key :points pset)
))

(defn player-subset
   [roster pos-set]

   (loop [p roster
          subset [] ] 
     (if (nil? p)
       subset
			 (if (contains? pos-set ((first p) :pos))
					(recur (next p) (conj subset (first p)))
					(recur (next p) subset)
       )
			)
))

(defn deg-points
  [players]
  (let [deg-factor-map {:qb (concat [0.2  0.0001 0.00001 0.000001] (repeat 1e-7))
                        :rb (concat [1.0  0.50 0.20  0.20] (repeat 1e-3))
												:wr (concat [1.0  0.40 0.20  0.10] (repeat 1e-3))
												:te (concat [1.0  0.2  0.01 0.005] (repeat 1e-4))}]

    (reduce-kv (fn [m k v]
                 (conj m (update-in v [:points] #(* % (nth (deg-factor-map (v :pos) (repeat 0.0) ) k)))))
             [] players)
    )
  )

(defn optimized-startingroster
   [roster]
   (let [sroster-keys (keys core/starting-roster-struct) ]

   (loop [ sroster    (reduce #(assoc %1 %2 []) {} sroster-keys )
           remaining  roster]

       (let [all-open-slots   (openslots sroster) 
             main-pos-to-fill (filterv #(not= :flex %) all-open-slots) 
             flex-pos-to-fill (filterv #(= :flex %)    all-open-slots)
             pos-to-fill      (first (concat main-pos-to-fill flex-pos-to-fill))]

;         (println "Trying to fill " pos-to-fill " with " remaining)
         (if (nil? pos-to-fill)
           ; No more positions to fill
           [sroster (deg-points remaining)]
           (let  [p (player-max-points (player-subset remaining (core/pos-allowed pos-to-fill)))]
             (if (nil? p)
								 ; No more players to fill this position ->  Add placeholder
               (do ;(println "Nope, fucked up")
							 (recur (assoc sroster pos-to-fill (conj (sroster pos-to-fill) {:name "No available" :points 0 :pos :bad})) 
											remaining))

							 (recur (assoc sroster pos-to-fill (conj (sroster pos-to-fill) p)) 
											(core/remove-player remaining  p  ))
             )
))))))

(defn points-weekn 
   [roster week]
   (let [weeknroster (cull-bye roster week) 
         opt-roster (optimized-startingroster weeknroster)]
     (+ 
       ( apply + (map :points (apply concat (vals (first  opt-roster)) )))
       ( apply + (map :points (second opt-roster) ))
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