(ns draftbuddy.core
	(require [clojure.string :as str] )
	(:gen-class)
)

(def poskeys [:qb :rb :wr :te :dst :k])


; Weekly starting roster
(def starting-roster-struct {:qb 1 :wr 2 :rb 2 :te 1 :flex 1 :k 1 :dst 1})
; Roster configuration
;(def fullrostersize {:qb 1 :wr 3 :rb 2 :te 1 :bench 7 :dst 1 :k 1})
(def fullrostersize {:qb 1 :wr 3 :rb 2 :te 1 :bench 6 :dst 1 :k 1})
;(def fullrostersize {:qb 1 :wr 2 :rb 2 :te 1 :bench 6 :dst 1 :k 1})
(def maxinroster    {:qb 4 :wr 8 :rb 8 :te 3 :dst 1 :k 1})


; Which positions can fill which slot in starting roster
(def pos-allowed  {:qb #{:qb} :wr #{:wr} :rb #{:rb} :te #{:te} :flex #{:wr :rb :te} :k #{:k} :dst #{:dst} })

(def gui-text-format "%5.1f %24s %3s/%2d %5.1f")
(def gui-roster-text-format " %2s  %5.1f %24s %3s/%2d %5.1f")

(def nweeks 17)

(defn cmpplayer
  [p1 p2]
  (and 
      (zero? (compare (:name p1) (:name p2)))
      (zero? (compare (:team p1) (:team p2)))
      (= (:pos p1) (:pos p2)))
)

(def player-keywords  [:name, :pos, :team, :bye, :dstBlk,:dstFumlRec,:dstInt,:dstPtsAllow,:dstRetTd,
                          :dstSack,:dstSafety,:dstTd,:fg,:fg0019,:fg2029,:fg3039,:fg4049,:fg50,:fgAtt,
                          :fgMiss,:fb,:games,:idpAst,:idpFumlForce,:idpFumlRec,:idpInt,:idpPD,
                          :idpSack,:idpSolo,:idpTFL,:idpTd,:pass300,:pass350,:pass40,:pass400,:passAtt,
                          :passComp,:passCompPct,:passInc,:int,:passtd,:passyd,:rec,:rec100,:rec150,
                          :rec200,:rec40,:rectd,:recyd,:returnTds,:returnYds,:rush100,:rush150,:rush200,
                          :rush40,:rushatt,:rushtd,:rushyd,:sacks,:twoPts,:xp,:status,:yahooId,:fbgId,
                          :cbsId,:foxId,:fftId,:birthdate,:draftYear,:mflId] )

(def adp-keywords    [:playerId, :player, :name, :pos, :team,:playerposition, :playerteam, :vor,
                      :points, :actualPoints, :overallECR, :overallRank, :posrank, :cost, :salary,
                      :dropoff,:adp, :adpdiff, :auctionValue, :upper, :lower, :risk, :sleeper])

(def pointvals {:passyd  0.04 
                :passtd  4.0
                :int    -2.0 
                :rushyd  0.1
                :rushtd  6.0
                :fb     -2.0
                :rec     0.0 
                :recyd   0.1  
                :rectd   6.0  })

; My League
;(def pointvals {:passyd  0.04 
;                :passtd  4.0
;                :int    -2.0 
;                :rushyd  0.1
;                :rushtd  6.0
;                :fb     -2.0
;                :rec     0.25 
;                :recyd   0.1  
;                :rectd   6.0  })

; Oumaru's League
;(def pointvals {:passyd  0.033
;                :passtd  5.0
;                :int    -2.0 
;                :rushyd  0.1
;                :rushtd  6.0
;                :fb     -2.0
;                :rec     1.0  
;                :recyd   0.1  
;                :rectd   6.0  })


(defn add-player
  [roster team playertoadd]
  (update-in roster [team] #(conj %  playertoadd ))
)

(defn remove-player 
  [pool playertorem]
  (vec (remove #(cmpplayer playertorem %) pool))
)

(defn nextpick
  [nteam round team forward?]
  (if forward?
					(if (== (inc team) nteam)
						{:round (inc round) :team team :forward? false}
						{:round round :team (inc team) :forward? forward?}
						)
					(if (== team 0)
						{:round (inc round) :team team :forward? true}
						{:round round :team (dec team) :forward? forward?}

				)))


(defn calcpoints
  "Calculate projected points and add to map"
  [player]
  (let [ptstats-raw (select-keys player (keys pointvals))
				ptstats     (reduce #(assoc %1 %2 (if (nil? (%1 %2) ) 0 (%1 %2))) ptstats-raw  (keys ptstats-raw)) 
        newpoints (/ (apply + (map #(* (pointvals %) (ptstats %))  (keys ptstats))) 16.) ]
    (cond
      (contains? #{:qb :rb :wr :te} (player :pos)) (assoc player :points newpoints)
;      (contains? #{:dst :k}   (player :pos)) (assoc player :points (player :points ))
;			:else (assoc player :points -1000.0)
			:else (assoc player :points (/ (player :points) 16.0) )
   )
))



(defn proccsv
  "Process csv string"
  [rawdata]
    (map (fn [line] (str/split line #","))
			(-> rawdata 
			(str/split-lines)
			(rest)
    ))
)

(defn read-string-or-empty
  [s]
  (cond 
    (zero? (compare s ""))   "nil"
    (zero? (compare s "\"null\"")) "\"nil\""
    :else   s
   ))

(defn read-string-no-symbol
  [s]
  (if (= clojure.lang.Symbol (type (read-string s)))
        s
        (read-string s))
)
     
(defn proc-adp-line
  [line-toks]
  ; Do twice to pull out quoted and then cast
  (map read-string 
		(map read-string-or-empty line-toks)
  ))

(defn proc-line
  [line-toks]
  ; Do twice to pull out quoted and then cast
;  (map read-string 
   (let [processed-lines (map read-string-no-symbol
                            (map read-string-or-empty line-toks))]
;     (println processed-lines)
;     (println (map type processed-lines))
     processed-lines)
  
  )
  

(defn loadadp
  [filename]

  (map #(assoc % :pos (keyword (str/lower-case (:pos %)) ))  
   (map (partial zipmap adp-keywords)
        (map proc-adp-line
		      ( -> filename
			      (slurp)
			      (proccsv)
			      )
        ))))

(defn addadp
  [adp-table player]
  (if-let [adp-entry (first (filter #(cmpplayer player %) adp-table))]
			(if (nil? (read-string (adp-entry :adp)))
					(-> player 
						 (assoc :adp    (java.lang.Double. 999.))
						 (assoc :points (java.lang.Double. 0.0)))
          (-> player
						(assoc :adp (double (read-string (adp-entry :adp))))
						(assoc :points (double (read-string (adp-entry :points)))))
             )
;   (println "palyer is " player)
   (println "PLAYER " (player :name) "  NOT IN ADP"))
)



(defn calc-vor
   [pool pos nteam]
   (let [players       (filter #(= (% :pos) pos) pool)
				 sorted-player (sort-by :points #(> %1 %2) players)
         n-deep        (* nteam ({:qb 1 :rb 2 :wr 2 :te 1 :dst 1 :k 1} pos)) ]

     ;  Make dst and k worthless
       ((nth sorted-player (dec n-deep)) :points)
 ))


(defn add-vor
  [vor-map player]
			(assoc player :vor (- (player :points) (vor-map (player :pos))) )
)

(defn calc-vor-map
  [pool nteam]
   (reduce #(assoc %1 %2 (calc-vor pool %2 nteam)) {} poskeys)
)



(defn validate 
  [player]
;  (println player)
  (cond
    (not= (str (type (player :points))) "class java.lang.Double" ) (println "BAD POINTS FOR " (select-keys player [:name :pos :points :adp]))
    (not= (str (type (player :adp   ))) "class java.lang.Double" ) (println "BAD ADP FOR " (select-keys player [:name :pos :points :adp]) (type (player :adp)))
    (nil? (player :points) ) (println "BAD POINTS FOR " (select-keys player [:name :pos :points :adp]))
    (nil? (player :adp) ) (println "BAD ADP FOR " (select-keys player [:name :pos :points :adp]))
    (nil? (< (player :points) 0)) "WHat points?"
    (nil? (< (player :adp) 0)) "WHat ADP?"
    )
  (select-keys player [:name :team :pos :points :adp :vor :bye] )
  )

(defn load-player-files
 "Load fantasy players"
 []
  (let [adp (loadadp "resources/FFA-CustomRankings.csv")]
   (sort-by :adp  #(< %1 %2 )
   (sort-by :points  #(> %1 %2) 
;   (sort-by :points  #((do (println "Comparing " %1 %2) (> %1 %2) ))
     (map validate
;     (remove nil?
     (map calcpoints
     (map #(addadp adp %)
     (remove #(zero? (compare "FA" (% :team) ) )
     (filter #(contains? (set poskeys) (% :pos) )  
     (map #(assoc % :pos (keyword (str/lower-case (:pos %)) ))  
     (map (partial zipmap player-keywords)
     (map proc-line
			  (-> "resources/FFA-RawStatProjections.csv"
				  (slurp)
				  (proccsv))
     ))))))))))))

(defn load-players-with-vor
	[nteam]
  (let [pool    (load-player-files)
        vor-map (calc-vor-map pool nteam)]
  (mapv #(add-vor vor-map %) pool)
 ))


   

(load "seasoneval")
(load "gui")
(load "selectmeths")
(load "draftengine")

(defn rundraft
  []
  (let [final-roster (snakedraft 10)]
		(println (final-roster 3) )
		(println (map :pos (final-roster 3) ))
		(eval-season final-roster)
))


(defn -main
	[]
 (rundraft))
