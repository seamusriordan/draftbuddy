(ns draftbuddy.draftengine)

(require [ 'draftbuddy.core        :as 'core] 
         [ 'draftbuddy.selectmeths :as 'sel]
         [ 'clojure.string         :as 'cstr ])

; Weekly starting roster
(def rosterstruct {:qb 1 :wr 2 :rb 2 :te 1 :flex 1 :def 1 :k 1})
; Roster configuration
(def fullrostersize {:qb 1 :wr 2 :rb 2 :te 1 :bench 8 :def 1 :k 1})
(def maxinroster    {:qb 4 :wr 8 :rb 8 :te 3 :def 3 :k 3})

(defn initroster
  [nteam]
  (vec (repeat nteam (zipmap core/poskeys (repeat [])))))
  

(defn addplayer
  [roster team playertoadd]

  (update-in roster [team (first playertoadd)] #(conj % (second playertoadd)) )
)

(defn removeplayer 
  [pool playertorem]
  
  (let [position (first  playertorem)
        player   (second playertorem) ]

  (update-in pool [position] 
				 #(remove (zero? (compare (:name player) (:name %)) ))
 )))

(defn snakedraft
  [nteam]
  
  (let [nround (apply + (vals fullrostersize)) 
        selectionmethod sel/takebest ]

  (loop [round 1 team 0 forward? true
         roster (initroster nteam) 
         pool   (core/loadplayers) ]

   (when (<= round nround)
      (printf "Round %2d - Team %2d\n" round (inc team))

      (let [draftedplayer (selectionmethod roster team pool)
            updatedroster (addplayer    roster team draftedplayer)
            updatedpool   (removeplayer pool   draftedplayer) ]
        
			(printf "Selecting %2s %s %5.1f\n" 
           (cstr/upper-case (name (first draftedplayer))) 
           (:name (second draftedplayer)) 
           (:points (second draftedplayer)) 
           )
        
      (if forward?
          (if (== (inc team) nteam)
            (recur (inc round) team false    updatedroster updatedpool)
            (recur round (inc team) forward? updatedroster updatedpool)
            )
          (if (== team 0)
            (recur (inc round) team true     updatedroster updatedpool)
            (recur round (dec team) forward? updatedroster updatedpool)
					)
			))))))
          
