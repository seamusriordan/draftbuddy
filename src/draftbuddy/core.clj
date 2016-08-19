(ns draftbuddy.core)

(require ['clojure.string :as 'str])

(def poskeys [:qb :rb :wr :te])
(def projfiles {:qb "resources/fantasypros_qb.csv"
                :rb "resources/fantasypros_rb.csv"
                :wr "resources/fantasypros_wr.csv"
                :te "resources/fantasypros_te.csv" })

(def player-keywords [:name :team :rushatt :rushyd :rushtd :rec :recyd :rectd :fb :points])
(def qb-keywords     [:name :team :att :cmp :passyd :passtd :int :rushatt :rushyd :rushtd :fb :points])

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
        newpoints (apply + (map #(* (pointvals %) (ptstats %))  (keys ptstats))) ]
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

(defn playerrec
  "Make player record map"
  [keywords playerseq]
  (calcpoints (zipmap keywords playerseq ))
)


(defn loadplayerfile
 "Load fantasy players"
 [filename keywords]
 ; sort by high to low
   (sort-by :points #(> %1 %2)
		 (map #(playerrec keywords %)
		 (map parsenamefield 
				 (proccsv (slurp filename))
   ))))


(defn loadplayers 
   []
   (zipmap poskeys (map #(loadplayerfile (% projfiles) (% poswords)) poskeys))
)

; Functionality to load players to load players defined
  
  