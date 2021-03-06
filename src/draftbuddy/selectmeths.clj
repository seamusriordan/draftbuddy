(in-ns 'draftbuddy.core)


(defn count-at-position
  [roster]
  (let [init-map (zipmap (keys fullrostersize) (repeat 0))]
    (reduce #(assoc %1 (%2 :pos) (inc (%1 (%2 :pos)) )) init-map roster)
))  

(defn valid-roster?
  [roster player]
  ( let [proproster (conj roster player) 
         pcount     (count-at-position proproster) 
         fullrost   fullrostersize 
         rostdiff   (zipmap (keys fullrost) (map #(- (% fullrost) (% pcount)) (keys fullrost))) 
         negkeys    (filter #(neg? (% rostdiff)) (keys rostdiff)) ]

    (cond
      ; For positions where we have too many players, make sure the bench is big enough
      (neg? (+ (apply + (map #(% rostdiff) negkeys)) (:bench rostdiff))) false
      ; Make sure nothing is over the max we allow in the roster
      (some true? (map #(> (% pcount) (% maxinroster) ) (keys maxinroster) )) false
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
			{:name "Bad player in highest-adp" :points 0.0 :pos :k :vor 0.0 :adp 999.0}
     )))
)


(defn take-highest-vor
  [_ team _ roster pool]

  (let [ sortedpool (sort-by :vor #(> %1 %2) pool) ]
    (loop [toconsider sortedpool]
      (let [player (first toconsider)]
        (if (nil? player)
          {:name "No valid player" :pos :k :points 0.0 :bye 0 :vor -100.0 :adp 50000.0 }
				  (if (valid-roster? (roster team) player)
					  (do ;(println player " has highest VOR ") 
                 player)
            (recur (vec (next toconsider)))
       )))))
)




; Ask which player gives the most points for season given ADP for remaining draft


 (defn get-top-set
   [pool]
		(loop [pos poskeys topset [] ]
      (if-let [thispos     (first pos)]
        (let [ pos-pool   (sort-by :points #(> %1 %2) (filter #(= (% :pos) thispos) pool))
               ndeep       {:qb 1 :rb 3 :wr 3 :te 2 :k 1 :dst 1} ]
					(recur (next pos) (concat topset (take (ndeep thispos) pos-pool))))
				topset
		))
)

; Aux function: quietly finish draft with highest adp selection
(defn finish-with-adp
  [pres-round pres-team pres-forward? pres-roster pres-pool]
  
  (let [nround (apply + (vals fullrostersize)) 
        nteam  (count pres-roster) ]

  (loop [round pres-round  team pres-team  forward? pres-forward?
         roster pres-roster   pool   pres-pool ]

   (if (<= round nround)
      (let [draftedplayer (take-highest-adp    round team forward? roster pool)
            updatedroster (add-player    roster team draftedplayer)
            updatedpool   (remove-player pool   draftedplayer) 
            next-rd       (nextpick nteam round team forward?) ]
        
;            (println "Round " round " Team " team " takes " (draftedplayer :name))
        
				    (recur (next-rd :round) (next-rd :team) (next-rd :forward?) updatedroster updatedpool))
        
      roster
)))) 


(defn finish-with-vor
  [pres-round pres-team pres-forward? pres-roster pres-pool]
  
  (let [nround (apply + (vals fullrostersize)) 
        nteam  (count pres-roster) ]

  (loop [round pres-round  team pres-team  forward? pres-forward?
         roster pres-roster
         pool   pres-pool]

   (if (<= round nround)
      (let [draftedplayer (take-highest-vor    round team forward? roster pool)
            updatedroster (add-player    roster team draftedplayer)
            updatedpool   (remove-player pool   draftedplayer) 
            next-rd       (nextpick nteam round team forward?) ]
        
;            (println "Round " round " Team " team " takes " (draftedplayer :name))
        
				    (recur (next-rd :round) (next-rd :team) (next-rd :forward?) updatedroster updatedpool))
        
      roster
)))) 


(defn take-n-with-vor
  [nselect pres-round pres-team pres-forward? pres-roster pres-pool]
  
  (let [nround (apply + (vals fullrostersize)) 
        nteam  (count pres-roster) ]

  (loop [nselect-left nselect round pres-round  team pres-team  forward? pres-forward?
         roster pres-roster
         pool   pres-pool]

   (if (and (<= round nround) (< 0 nselect-left))
      (let [draftedplayer (take-highest-vor    round team forward? roster pool)
            updatedroster (add-player    roster team draftedplayer)
            updatedpool   (remove-player pool   draftedplayer) 
            next-rd       (nextpick nteam round team forward?) ]
        
;            (println "Round " round " Team " team " takes " (draftedplayer :name))
        
				    (recur (dec nselect-left) (next-rd :round) (next-rd :team) (next-rd :forward?) updatedroster updatedpool))
        
      roster
)))) 

(defn take-n-with-adp
  [nselect pres-round pres-team pres-forward? pres-roster pres-pool]
  
  (let [nround (apply + (vals fullrostersize)) 
        nteam  (count pres-roster) ]

  (loop [nselect-left nselect round pres-round  team pres-team  forward? pres-forward?
         roster pres-roster
         pool   pres-pool
         taken  [] ]

   (if (and (<= round nround) (< 0 nselect-left))
      (let [draftedplayer (take-highest-adp    round team forward? roster pool)
            updatedroster (add-player    roster team draftedplayer)
            updatedpool   (remove-player pool   draftedplayer) 
            next-rd       (nextpick nteam round team forward?) ]
        
;            (println "Round " round " Team " team " takes " (draftedplayer :name))
        
				    (recur (dec nselect-left) (next-rd :round) (next-rd :team) (next-rd :forward?) 
               updatedroster updatedpool (conj taken draftedplayer)
               ))
        
      [pool taken]
)))) 




; Take top from each position - calculate what is likely to be gone
; until next turn - look at point difference at that position
(defn take-point-diff
  [pres-round pres-team pres-forward? pres-roster pres-pool]
  
  (let [nround       (apply + (vals fullrostersize)) 
        nteam        (count pres-roster) 
        to-next-turn (if pres-forward? (* (- (dec nteam) pres-team) 2)  (* pres-team 2)) 
        next-rd       (nextpick nteam pres-round pres-team pres-forward?) ]

  (loop [ pos-to-check poskeys best-player {:name "nobody in take diff" :pos :k :points 0.0 :vor 0.0 :adp 999.0 :bad true} best-points 0.0  ]
    

    (if-let [this-pos     (first pos-to-check)]
        (let [best-at-pos  (first (sort-by :points #(> %1 %2) (filter #(= this-pos (% :pos)) pres-pool)))
              potential-pool (first (take-n-with-adp (inc to-next-turn)
                                   (next-rd :round) (next-rd :team) (next-rd :forward?)
                                   pres-roster pres-pool) )
              potential-next-best   (first (sort-by :points #(> %1 %2) (filter #(= this-pos (% :pos))  potential-pool)))
              point-diff     (- (best-at-pos :points) (potential-next-best :points)) ]
;          (println "Looking at " (best-at-pos :name) " " (best-at-pos :points) " and getting " point-diff " against " 
;                   (potential-next-best :name) " " (potential-next-best :points))
        
					(if (> point-diff best-points) 
						(recur (next pos-to-check) best-at-pos point-diff)
						(recur (next pos-to-check) best-player best-points)))
         
				 (if (best-player :bad)
						(take-highest-vor pres-round pres-team pres-forward? pres-roster pres-pool)
						best-player
       ))
))) 




(defn max-out-season
  [round team forward? roster pool]
  
  ; consider top few at any position
  
    (loop [to-consider (get-top-set pool)
           max-points 0
           best-player {:points 0 :name "Best not chosen" :pos :bad} ]

      (if (nil? to-consider)
        (do ;(println "Best is " (:name best-player))
					best-player)

				(let [p (first to-consider)]
					(if (valid-roster? (roster team) p)
						(let [updated-roster (add-player    roster team  p )
									updated-pool   (remove-player pool p ) 
                  next-rd        (nextpick     (count roster) round team forward?) 
                  sim-finish     (take-n-with-vor  (* (count roster) 2) (next-rd :round) (next-rd :team) (next-rd :forward?) 
                                                   updated-roster updated-pool) 
									points         (apply + (eval-team-season ( sim-finish team))) ]
;						(println "Taking " (p :name) " gives " points " with " (map #(select-keys % [:name :pos] ) (sim-finish team) ))
						(if (> points  max-points)
							(recur (next to-consider) points     p )
							(recur (next to-consider) max-points best-player )
						))
					 (recur (next to-consider) max-points best-player )
      
      )))))

; VOR

(defn remove-action-listeners
  [obj]
  (let [listeners (.getActionListeners obj)]
    (mapv #(.removeActionListener obj %) listeners)
))


(defn remove-mouse-listeners
  [obj]
  (let [listeners (.getMouseListeners obj)]
    (mapv #(.removeMouseListener obj %) listeners)
))

(defn take-by-gui
  [round team forward? roster pool stack to-go gui-lists]
  
  (let [selected-player (promise)
        selListener (proxy [MouseAdapter] []
			 ( mouseClicked [mouseEvent]
				 (let [thisList (-> mouseEvent (.getSource) )
							 thisIdx  (-> thisList (.locationToIndex (.getPoint mouseEvent)) ) 
							 object   (-> thisList (.getModel) (.getElementAt thisIdx) ) ]

				 (if (= (.getClickCount mouseEvent) 2)
					 (let [obj-map (read-string (.toString object))]
;             (println "yay! -> " obj-map)
;						 (deliver selected-player (.toString object))
						 (deliver selected-player obj-map)
             nil
             )))
			 ))
        
        undoListener (proxy [ActionListener] []
				 (actionPerformed [evt]
;					 (println (.getActionCommand evt))
					 (deliver selected-player :undo)
          ))

        selectListener (proxy [ActionListener] []
				 (actionPerformed [evt]
           (let [source (.getSource evt)]
;					 (println (.getActionCommand evt)  (.toString (.getSelectedItem source)) (.getSelectedIndex source  ))
;					 (deliver selected-player :undo)


            (update-gui-lists gui-lists pool roster (.getSelectedIndex source) stack to-go)
           )))

        ]
    
   ; Clear existing listeners
 	(mapv #(remove-mouse-listeners (gui-lists %) ) [:qb :rb :wr :te :k :dst])
  (remove-action-listeners (gui-lists :undo-button))
  (remove-action-listeners (gui-lists :team-select))
        
 	(mapv #(.addMouseListener (gui-lists %) selListener ) [:qb :rb :wr :te :k :dst])
  (.addActionListener (gui-lists :undo-button) undoListener)
  (.addActionListener (gui-lists :team-select) selectListener)

  (deref selected-player)

))

  
  
  
  