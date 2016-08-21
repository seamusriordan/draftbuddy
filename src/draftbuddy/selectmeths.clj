(ns draftbuddy.selectmeths)

(require [ 'draftbuddy.core        :as 'core] 
         [ 'draftbuddy.draftengine :as 'de]
         [ 'draftbuddy.seasoneval  :as 'se]
         [ 'clojure.string         :as 'cstr ])

  

(defn validroster?
  [roster playerpos]
  
;  (println (conj (map :pos (apply concat (vals roster))) playerpos)  )
  ( let [initcount  (zipmap (keys de/fullrostersize) (repeat 0))
         proproster (conj (map :pos (apply concat (vals roster))) playerpos) 
         pcount     (reduce #(assoc %1 %2 (inc (%1 %2))) initcount proproster) 
         fullrost   de/fullrostersize 
         rostdiff   (zipmap (keys fullrost) (map #(- (% fullrost) (% pcount)) (keys fullrost))) 
         negkeys    (filter #(neg? (% rostdiff)) (keys rostdiff)) ]

    (cond
      ; For positions where we have too many players, make sure the bench is big enough
      (neg? (+ (apply + (map #(% rostdiff) negkeys)) (:bench rostdiff))) false
      ; Make sure nothing is over the max we allow in the roster
      (some true? (map #(> (% pcount) (% de/maxinroster) ) (keys de/maxinroster) )) false
      :else true
   )
))

; Dumbest method possible
; Just blindly aggregate and pick off the top
(defn take-most-points
  [_ team _ roster pool]

  (let [ fullpool  (vec (apply concat (vals pool))) 
         sortedpool (sort-by :points #(> %1 %2) fullpool) ]
    
    (loop [toconsider sortedpool]
      (let [player (first toconsider)]
				(if (validroster? (roster team) (:pos player))
					[(:pos player) player]
          (recur (vec (next toconsider)))
     ))))
)

; Sort by ADP and get the highest ADP
(defn take-highest-adp
  [_ team _ roster pool]

  (let [ fullpool  (vec (apply concat (vals pool)))
         sortedpool (sort-by #(:adp %)  #(< %1 %2) fullpool) ]
    
    (loop [toconsider sortedpool]
      (if-let [player (first toconsider)]
				(if (validroster? (roster team) (:pos player))
					[(:pos player) player]
          (recur (vec (next toconsider))))
			[:bad {:name "Bad player" :points 0 :pos :bad}]
     )))
)

; Ask which player gives the most points for season given ADP for remaining draft



 (defn get-top-two
   [pool]
		(loop [pos [:qb :rb :wr :te :def :k] topset [] ]
      (if-let [thispos (first pos)]
				(recur (next pos) (conj topset (first (pool thispos)) (second (pool thispos))))
				topset
			 
		))
)

; Aux function: quietly finish draft with highest adp selection
(defn finish-with-adp
  [pres-round pres-team pres-forward? pres-roster pres-pool]
  
  (let [nround (apply + (vals de/fullrostersize)) 
        nteam  (count pres-roster) ]

  (loop [round pres-round  team pres-team  forward? pres-forward?
         roster pres-roster
         pool   (core/loadplayers) ]
    
   (if (<= round nround)
      (let [draftedplayer (take-highest-adp    round team forward? roster pool)
            updatedroster (de/addplayer    roster team draftedplayer)
            updatedpool   (de/removeplayer pool   draftedplayer) 
            next-rd       (de/nextpick nteam round team forward?) ]
        
;            (println "Round " round " Team " team " takes " ((second draftedplayer) :name))
        
				    (recur (next-rd :round) (next-rd :team) (next-rd :forward?) updatedroster updatedpool))
        
      roster
)))) 


(defn max-out-season
  [round team forward? roster pool]
  
  ; consider top 2 at any position
  
    (loop [to-consider (get-top-two pool)
           max-points 0
           best-player {:points 0 :name "Best not chosen" :pos :bad} ]

      (if (nil? to-consider)
        (do ;(println "Best is " (:name best-player))
					[(:pos best-player) best-player])

				(let [p (first to-consider)]
					(if (validroster? (roster team) (:pos p))
						(let [updated-roster (de/addplayer    roster team [(:pos p) p] )
									updated-pool   (de/removeplayer pool [(:pos p) p] ) 
                  next-rd        (de/nextpick     (count roster) round team forward?) 
									points         (apply + (se/eval-team-season 
                                                  ((finish-with-adp (next-rd :round) (next-rd :team) (next-rd :forward?) 
                                                                    updated-roster updated-pool) 
                                                  team ))) ]
;						(println "Taking " (p :name) " gives " points)
						(if (> points  max-points)
							(recur (next to-consider) points     p )
							(recur (next to-consider) max-points best-player )
						))))))
    )
