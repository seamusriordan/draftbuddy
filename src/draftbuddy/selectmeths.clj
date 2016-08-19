(ns draftbuddy.selectmeths)

(require [ 'draftbuddy.core        :as 'core] 
         [ 'draftbuddy.draftengine :as 'de]
         [ 'clojure.string         :as 'cstr ])

  

(defn validroster
  [roster playerpos]
  
  ( let [initcount  (zipmap (keys de/fullrostersize) (repeat 0))
         proproster (conj (map :pos (apply concat (vals roster))) playerpos) 
         pcount     (reduce #(assoc %1 %2 (inc (%2 %1))) initcount proproster) 
         fullrost   de/fullrostersize 
         rostdiff   (zipmap (keys fullrost) (map #(- (% fullrost) (% pcount)) (keys fullrost))) 
         negkeys    (filter #(neg? (% rostdiff)) (keys rostdiff)) ]

;    (println rostdiff negkeys)
;    (println (map #(> (% pcount) (% de/maxinroster) ) (keys de/maxinroster) ))

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


(defn takebest 
  [roster team pool]

  (let [ fullpool  (vec (apply concat (vals pool))) 
         sortedpool (sort-by :points #(> %1 %2) fullpool) ]
    
    (loop [toconsider sortedpool]
      (let [player (first toconsider)]
				(if (validroster (roster team) (:pos player))
					[(:pos player) player]
          (recur (vec (next toconsider)))
     ))))
)