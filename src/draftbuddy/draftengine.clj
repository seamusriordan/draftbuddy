(ns draftbuddy.draftengine)

(require [ 'draftbuddy.core        :as 'core] 
         [ 'draftbuddy.selectmeths :as 'sel]
         [ 'clojure.string         :as 'cstr ])
	 
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
  
  (let [nround (apply + (vals core/fullrostersize)) 
;        selectionmethod sel/take-highest-adp]
;        selectionmethod sel/take-most-points]
;        selectionmethod sel/max-out-season]
;        selectionmethod (concat (repeat 3 sel/take-most-points) [sel/max-out-season] (repeat 4 sel/take-highest-adp) [sel/max-out-season sel/take-most-points] (repeat sel/take-highest-adp)  ) ]
        selectionmethod (repeat sel/take-highest-adp) ]


  (loop [round 1 team 0 forward? true
         roster (initroster nteam) 
         pool   (core/loadplayers) 
         dstack (savestate (list) round team forward? roster pool ) ]
    
    
   (if (<= round nround)
;   (if (<= round 1)
			 (do 
				(printf "Round %2d - Team %2d\n" round (inc team))

				(let [draftedplayer ((nth selectionmethod team) round team forward? roster pool)
							updatedroster (core/add-player    roster team draftedplayer)
							updatedpool   (core/remove-player pool   draftedplayer) 
							next-rd       (core/nextpick nteam round team forward?) ]
					

				(if (= draftedplayer :undo)
					(println "Going back")

          (do 
					(printf "(%3d) Selecting (%3.1f) %3s %20s (%3s) %5.1f\n" 
               (count dstack)
							 (:adp    draftedplayer) 
							 (cstr/upper-case (name (draftedplayer :pos))) 
							 (:name   draftedplayer) 
							 (:team   draftedplayer)
							 (:points draftedplayer) 
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
