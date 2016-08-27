(in-ns 'draftbuddy.core)

	 
(defn initroster
  [nteam]
  (vec (repeat nteam [])))
  




(defn savestate
  [dstack round team forward? roster pool last-pick]
  (let [state {:round round :team team :forward? forward? :roster roster :pool pool :last-pick last-pick}]
    (conj dstack state)
))




(defn snakedraft
  [nteam]
  
  (let [nround (apply + (vals fullrostersize)) 
;        selectionmethod take-highest-adp]
;        selectionmethod take-most-points]
;        selectionmethod max-out-season]
;        selectionmethod (concat (repeat 2 take-highest-vor) [max-out-season] [take-point-diff] (repeat 4 take-highest-adp) [max-out-season take-most-points] (repeat take-highest-adp)  ) ]
;        selectionmethod (repeat take-highest-vor) ]
;        selectionmethod (repeat take-point-diff) 
         gui-lists       (start-gui nteam) 
         selectionmethod (repeat take-by-gui) ]
;        selectionmethod (repeat take-point-diff) ]


  (loop [round 1 team 0 forward? true
         roster (initroster nteam) 
         pool   (load-players-with-vor nteam) 
         dstack (savestate (list) round team forward? roster pool {:name "No last pick"} ) ]
    
    
   (if (<= round nround)
;   (if (<= round 2)
			 (let [to-next-turn (if forward? (* (- (dec nteam) team) 2)  (* team 2)) 
             likely-to-go  (second (take-n-with-adp (inc to-next-turn) round team
                                     forward? roster pool))  ]

      
				(printf "Round %2d - Team %2d\n" round (inc team))
        (.setText (gui-lists :draft-status) (format "Round %2d/Team %2d - Pick %3d\n" round (inc team) (count dstack)))
    
        (update-gui-lists gui-lists pool roster team dstack likely-to-go)
        (add-scores (gui-lists :total-points) roster nteam)
;        (Thread/sleep 10)

				(let [draftedplayer ((nth selectionmethod team) round team forward? roster pool dstack likely-to-go gui-lists)
;				(let [draftedplayer ((nth selectionmethod team) round team forward? roster pool )
							updatedroster (add-player    roster team draftedplayer)
							updatedpool   (remove-player pool   draftedplayer) 
							next-rd       (nextpick nteam round team forward?) ]
					

				(if (= draftedplayer :undo)
					(println "Going back")

          (do 
;          (println "This is my drafted player" draftedplayer)
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
						(let [ {bround :round bteam :team bforward? :forward? broster :roster bpool :pool} (peek dstack) 
									 bdstack (pop dstack) ]
							(recur bround bteam bforward? broster bpool bdstack ))

					
						(recur (next-rd :round) (next-rd :team) (next-rd :forward?) updatedroster updatedpool
                       (savestate dstack round team forward? roster pool draftedplayer ) )
             
             )))

			(do
				(update-gui-lists gui-lists pool roster team)
				roster)
     ))
))
