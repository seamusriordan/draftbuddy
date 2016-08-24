(in-ns 'draftbuddy.core)

	 
(defn initroster
  [nteam]
  (vec (repeat nteam [])))
  




(defn savestate
  [dstack round team forward? roster pool]
  (let [state {:round round :team team :forward? forward? :roster roster :pool pool}]
    (conj dstack state)
))




(defn snakedraft
  [nteam]
  
  (let [nround (apply + (vals fullrostersize)) 
;        selectionmethod take-highest-adp]
;        selectionmethod take-most-points]
;        selectionmethod max-out-season]
        selectionmethod (concat (repeat 3 take-highest-vor) [max-out-season] (repeat 4 take-highest-adp) [max-out-season take-most-points] (repeat take-highest-adp)  ) ]
;        selectionmethod (repeat take-highest-vor) ]


  (loop [round 1 team 0 forward? true
         roster (initroster nteam) 
         pool   (load-players-with-vor nteam) 
         dstack (savestate (list) round team forward? roster pool ) ]
    
    
   (if (<= round nround)
;   (if (<= round 1)
			 (do 
				(printf "Round %2d - Team %2d\n" round (inc team))

				(let [draftedplayer ((nth selectionmethod team) round team forward? roster pool)
							updatedroster (add-player    roster team draftedplayer)
							updatedpool   (remove-player pool   draftedplayer) 
							next-rd       (nextpick nteam round team forward?) ]
					

				(if (= draftedplayer :undo)
					(println "Going back")

          (do 
					(printf "(%3d) Selecting (%3.1f) %3s %20s (%3s) %5.1f (vor %4.1f)\n" 
               (count dstack)
							 (:adp    draftedplayer) 
							 (str/upper-case (name (draftedplayer :pos))) 
							 (:name   draftedplayer) 
							 (:team   draftedplayer)
							 (:points draftedplayer) 
							 (:vor    draftedplayer) 
							 )))
				
				(if (= draftedplayer :undo)
						(let [ {bround :round bteam :team bforward? :forward broster :roster bpool :pool} (peek (pop dstack)) 
									 bdstack (pop (pop dstack)) ]
							(recur bround bteam bforward? broster bpool bdstack ))
					
						(recur (next-rd :round) (next-rd :team) (next-rd :forward?) updatedroster updatedpool
                       (savestate dstack round team forward? roster pool ) )
             
             
             )))

      roster
     ))
))
