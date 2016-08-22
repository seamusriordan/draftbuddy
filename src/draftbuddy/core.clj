(ns draftbuddy.core)

(require ['clojure.string :as 'str] )

(def poskeys [:qb :rb :wr :te :dst :k])

; Which positions can fill which slot in starting roster
(def pos-allowed  {:qb #{:qb} :wr #{:wr} :rb #{:rb} :te #{:te} :flex #{:wr :rb :te} :k #{:k} :dst #{:dst} })

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
                :rec     0.1  
                :recyd   0.2  
                :rectd   2.0  })

(defn calcpoints
  "Calculate projected points and add to map"
  [player]
  (let [ptstats-raw (select-keys player (keys pointvals))
				ptstats     (reduce #(assoc %1 %2 (if (nil? (%1 %2) ) 0 (%1 %2))) ptstats-raw  (keys ptstats-raw)) 
        newpoints (/ (apply + (map #(* (pointvals %) (ptstats (if (nil? %) 0 %)))  (keys ptstats))) 16.) ]
    (assoc player :points newpoints))
)



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
     
(defn proc-adp-line
  [line-toks]
  ; Do twice to pull out quoted and then cast
;  (map read-string 
  (map read-string 
		(map read-string-or-empty line-toks))
  )

(defn proc-line
  [line-toks]
  ; Do twice to pull out quoted and then cast
  (map read-string 
		(map read-string-or-empty line-toks))
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
					(assoc player :adp 10000)
					(assoc player :adp (read-string (adp-entry :adp))))
    (assoc player :adp 10000)
))

(defn loadplayers
 "Load fantasy players"
 []
  (let [adp (loadadp "resources/FFA-CustomRankings.csv")]
;   (sort-by #(:points %)  #(> %1 %2) 
   (sort-by #(:adp %)  #(< %1 %2) 
    (map #(addadp adp %)
    (map calcpoints
    (map #(assoc % :pos (keyword (str/lower-case (:pos %)) ))  
    (map (partial zipmap player-keywords)
     (map proc-line
			 (-> "resources/FFA-RawStatProjections.csv"
				 (slurp)
				 (proccsv))
    ))))))))


   
(defn rundraft
  []
	(draftbuddy.seasoneval/eval-season (draftbuddy.draftengine/snakedraft 16))
)



