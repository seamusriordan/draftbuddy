(ns draftbuddy.selectmeths)

(require [ 'draftbuddy.core        :as 'core] 
         [ 'draftbuddy.seasoneval  :as 'se]
         [ 'clojure.string         :as 'cstr ])


(defn count-at-position
  [roster]
  (let [init-map (zipmap (keys core/fullrostersize) (repeat 0))]
    (reduce #(assoc %1 (%2 :pos) (inc (%1 (%2 :pos)) )) init-map roster)
))  


(defn valid-roster?
  [roster player]
  ( let [proproster (conj roster player) 
         pcount     (count-at-position proproster) 
         fullrost   core/fullrostersize 
         rostdiff   (zipmap (keys fullrost) (map #(- (% fullrost) (% pcount)) (keys fullrost))) 
         negkeys    (filter #(neg? (% rostdiff)) (keys rostdiff)) ]

    (cond
      ; For positions where we have too many players, make sure the bench is big enough
      (neg? (+ (apply + (map #(% rostdiff) negkeys)) (:bench rostdiff))) false
      ; Make sure nothing is over the max we allow in the roster
      (some true? (map #(> (% pcount) (% core/maxinroster) ) (keys core/maxinroster) )) false
      :else true
   )
))

; Dumbest method possible
; Just blindly aggregate and pick off the top
(defn take-most-points
  [_ team _ roster pool]

  (let [ sortedpool (sort-by :points #(> %1 %2) pool) ]
    (loop [toconsider sortedpool]
      (let [player (first toconsider)]
				(if (valid-roster? (roster team) player)
					player
          (recur (vec (next toconsider)))
     ))))
)

; Sort by ADP and get the highest ADP
(defn take-highest-adp
  [_ team _ roster pool]

  ;sort by points first
  (let [ sorted-point-pool (sort-by :points #(> %1 %2) pool) 
			   sorted-pool (sort-by :adp  #(< %1 %2) sorted-point-pool) ]
 
    (loop [to-consider sorted-pool]
;      (println "Considering for roster " (first toconsider) ((first toconsider) :pos))
      (if-let [player (first to-consider)]
        (do
				(if (valid-roster? (roster team) player)
					player
          (recur (vec (next to-consider)))))
			{:name "Bad player" :points 0 :pos :bad}
     )))
)

; Ask which player gives the most points for season given ADP for remaining draft


 (defn get-top-set
   [pool]
		(loop [pos core/poskeys topset [] ]
      (if-let [thispos     (first pos)]
        (let [ pos-pool   (sort-by #(:points %)  #(> %1 %2) (filter #(= (% :pos) thispos) pool))
               ndeep       {:qb 2 :rb 4 :wr 4 :te 4 :k 1 :dst 1} ]
					(recur (next pos) (concat topset (take (ndeep thispos) pos-pool))))
				topset
		))
)

; Aux function: quietly finish draft with highest adp selection
(defn finish-with-adp
  [pres-round pres-team pres-forward? pres-roster pres-pool]
  
  (let [nround (apply + (vals core/fullrostersize)) 
        nteam  (count pres-roster) ]

  (loop [round pres-round  team pres-team  forward? pres-forward?
         roster pres-roster
         pool   (core/loadplayers) ]

   (if (<= round nround)
      (let [draftedplayer (take-highest-adp    round team forward? roster pool)
            updatedroster (core/add-player    roster team draftedplayer)
            updatedpool   (core/remove-player pool   draftedplayer) 
            next-rd       (core/nextpick nteam round team forward?) ]
        
;            (println "Round " round " Team " team " takes " (draftedplayer :name))
        
				    (recur (next-rd :round) (next-rd :team) (next-rd :forward?) updatedroster updatedpool))
        
      roster
)))) 


(defn max-out-season
  [round team forward? roster pool]
  
  ; consider top 2 at any position
  
    (loop [to-consider (get-top-set pool)
           max-points 0
           best-player {:points 0 :name "Best not chosen" :pos :bad} ]

      (if (nil? to-consider)
        (do ;(println "Best is " (:name best-player))
					best-player)

				(let [p (first to-consider)]
					(if (valid-roster? (roster team) p)
						(let [updated-roster (core/add-player    roster team  p )
									updated-pool   (core/remove-player pool p ) 
                  next-rd        (core/nextpick     (count roster) round team forward?) 
									points         (apply + (se/eval-team-season 
                                                  ((finish-with-adp (next-rd :round) (next-rd :team) (next-rd :forward?) 
                                                                    updated-roster updated-pool) 
                                                  team ))) ]
;						(println "Taking " (p :name) " gives " points)
						(if (> points  max-points)
							(recur (next to-consider) points     p )
							(recur (next to-consider) max-points best-player )
						))
					 (recur (next to-consider) max-points best-player )
      
      )))))
