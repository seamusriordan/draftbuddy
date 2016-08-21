(ns draftbuddy.core)

(require ['clojure.string :as 'str] )

(def poskeys [:qb :rb :wr :te])
(def projfiles {:qb "resources/fantasypros_qb.csv"
                :rb "resources/fantasypros_rb.csv"
                :wr "resources/fantasypros_wr.csv"
                :te "resources/fantasypros_te.csv" })

(def player-keywords [:name :team :rushatt :rushyd :rushtd :rec :recyd :rectd :fb :points])
(def qb-keywords     [:name :team :att :cmp :passyd :passtd :int :rushatt :rushyd :rushtd :fb :points])
(def adp-keywords    [:name :team :pos :bye])

; Which positions can fill which slot in starting roster
(def pos-allowed  {:qb #{:qb} :wr #{:wr} :rb #{:rb} :te #{:te} :flex #{:wr :rb :te} :k #{:k} :def #{:def} })

(def nweeks 17)

(defn cmpplayer
  [p1 p2]
  (and 
      (zero? (compare (:name p1) (:name p2)))
      (zero? (compare (:team p1) (:team p2)))
      (= (:pos p1) (:pos p2)))
)

(def poswords {:qb qb-keywords
							 :rb player-keywords
               :wr player-keywords
               :te player-keywords} )


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
  (let [ptstats (select-keys player (keys pointvals))
        newpoints (/ (apply + (map #(* (pointvals %) (ptstats %))  (keys ptstats))) 16.) ]
    (assoc player :points newpoints)
))



(defn proccsv
  "Process csv string"
  [rawdata]
  (map #(str/split % #",") 
       ;ignore first line with rest
       (rest (str/split-lines rawdata) )))

(defn parsenamefield
  "Separate first, last, team from name field"
  [x]
  (let [fielddata  (str/split (first x) #"[\" ]")
        stats      (map read-string (rest x))
        namedata   (str/join " " (butlast (filter #(> (count %) 0) fielddata)))
        teamname   (last fielddata)]

    (concat [namedata] [teamname] stats)
    ))

(defn getadpentry 
  [player adptable]
  ( let [adpentry (first (filter (partial cmpplayer player) adptable))]
    (if (nil? adpentry)
      {:rank 10000 :posrank 10000 :bye 0}
      adpentry
		)
))

(defn addadp
  [player adptable]
  ( let [adpentry (getadpentry player adptable)]
    (-> player
      (assoc :adp     (:rank adpentry))
      (assoc :posrank (:posrank adpentry))
      (assoc :bye     (Integer. (:bye adpentry)))
	)
))

(defn playerrec
  "Make player record map"
  [pos keywords playerseq adp]
     (-> (zipmap keywords playerseq )
         (calcpoints)
         (assoc :pos pos)
         (addadp adp))
)


(defn addrank
  [player rank]
  (assoc player :rank rank))


(defn parseadppos
  [player]
  (let [pstr    (str (:pos player)) 
        [matched rawpos posrank] (re-find #"([A-Z]+)(\d+)" pstr) ]

       (-> player
          (assoc :pos (keyword (str/lower-case rawpos)))
          (assoc :posrank (Integer. posrank)))
		)
)


(defn loadadp
  [filename]
  (mapv parseadppos
  (mapv addrank 
  (mapv #(zipmap adp-keywords %)
		 (mapv parsenamefield 
				 (proccsv (slurp filename))
   )) (iterate inc 1)))
)

(defn loadplayerfile
 "Load fantasy players"
 [filename pos keywords]
  (let [adp (loadadp "resources/fantasypros_adp.csv")]
 ; sort by high to low
	 (sort-by :points #(> %1 %2)
		 (mapv #(playerrec pos keywords % adp)
		 (mapv parsenamefield 
			 (proccsv (slurp filename))
)))))

(defn dummy-player-set
  [n name pos adp]
   
  (vec ( for [x (range 1 n)]
		{:name (str name " " x)  :points 0.0 :pos pos :team "TBD" :adp adp :posrank 1 :bye 0} 
)))


(defn loadplayers 
   []
   (let [ks   (dummy-player-set 32 "Kicker"  :k   20000)
         defs (dummy-player-set 32 "Defense" :def 10000) ]

     (-> (zipmap poskeys (mapv #(loadplayerfile (% projfiles) % (% poswords)) poskeys)) 
         (assoc :k   ks)
         (assoc :def defs)))
)
   
(defn rundraft
  []
	(draftbuddy.draftengine/snakedraft 6)
)



