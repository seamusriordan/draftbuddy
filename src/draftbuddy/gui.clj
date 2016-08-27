(in-ns 'draftbuddy.core)

(import '(java.awt Color BorderLayout FlowLayout Font Dimension  )) 
(import '(javax.swing JFrame JButton JOptionPane JList ListCellRenderer 
                      JLabel JComponent BorderFactory DefaultListCellRenderer 
                      BoxLayout JPanel JComboBox JScrollPane SwingConstants
                      Box
                      
                      )) 
(import '(javax.swing.border EtchedBorder))
(import '(java.awt.event ActionListener MouseAdapter ))          



(defn testgui
  []
(let [frame (JFrame. "Hello Swing")
     button (JButton. "Click Me")]
 (.addActionListener button
   (proxy [ActionListener] []
     (actionPerformed [evt]
       (JOptionPane/showMessageDialog  nil,
          (str "<html>Hello from <b>Clojure</b>. Button "
               (.getActionCommand evt) " clicked.")))))

 (.. frame getContentPane (add button))

 (doto frame
   (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
   .pack
   (.setVisible true)))
)

(defn get-null-cell-renderer
  []
  nil
)

(defn add-scores
  [score-list roster nteam]
  
	(let [season-ending (eval-season roster)]
		(.setListData score-list (to-array 
			(mapv #(format "Team %2d    %5.1f" (inc %) (season-ending %)) (range nteam)) ))))
                             
      

(defn add-roster-to-list
  [roster]

  (mapv (fn [thisplayer]
				 (if (= (thisplayer :pos) :bad )
						 (str/join (repeat 38 " "))
						 (format gui-text-format 
										 (thisplayer :adp) (thisplayer :name) (thisplayer :team) (thisplayer :bye) (thisplayer :vor) )))
				 roster))

  
(defn build-roster-list
  [roster]
  (let [ opt-rost      (optimized-startingroster roster)]
    (concat
			(loop [pos [:qb :rb :wr :te :flex :dst :k] roster-strings []]
				(if-let [thispos (first pos)]
					 (let[ new-strings (add-roster-to-list (thispos (first opt-rost))) ]
					(recur (next pos) (concat roster-strings new-strings)))
					roster-strings ))
			(add-roster-to-list (second opt-rost))
      (repeat (- (dec (fullrostersize :bench)) (count (second opt-rost))) (str/join (repeat 34 " ")))
)))

(defn build-last-pick-list
  [last-picks]
  
  (loop [stack last-picks pick-strs []  n-last 20]
    (if (and (<= 0 n-last ) (< 0 (dec (count stack))))
      (let [last-state (peek stack)
            player  (last-state :last-pick)
            pick-no (dec (count stack))
            new-str 
							 (format "%3d  %2d  %3s %5.1f %22s %3s %5.1f" 
                pick-no  (inc (last-state :team))
                (str/upper-case (name (player :pos)))
                (player :adp) (player :name) (player :team) (player :vor)
        )]
           (recur (pop stack) (conj pick-strs new-str) (dec n-last)))
      pick-strs
    ))
)

(defn clamp-color
  [color]
  (let [cupper (map #(if (< 1.0 %) 1.0 %) color)
	      clower (map #(if (> 0.0 %) 0.0 %) cupper)]
;  (println "[returning] " clower)
  (vec clower)))

(defn get-vor-color
  [vor]
  (if (nil? vor)
		[0.8 0.8 0.8]
		(let [ r-val (/ (+ (- vor) 2.5) 1.0) 
           g-val (/ (+ vor 2.5) 1.0) 
           b-val  0.3
        interp-color [r-val g-val b-val ] ]
				(clamp-color interp-color)))
)


(defn get-roster-cell-renderer
  []
	 (proxy [ListCellRenderer] []
       (getListCellRendererComponent
       [jlist value index isSelected? hasFocus?]
       (let [ defaultRenderer (DefaultListCellRenderer.)
              this-label (.getListCellRendererComponent defaultRenderer 
                           jlist value index isSelected? hasFocus?)
              color-map {:qb  [0.3 1.0 1.0]
												 :rb  [1.0 0.0 0.0]
												 :wr  [0.0 0.0 1.0]
												 :te  [1.0 0.7 0.0]
												 :flex[0.0 1.0 0.0]
												 :dst [0.1 0.1 0.0]
												 :k   [1.0 0.7 0.7]
												 :bench1   [0.2 1.0 0.2]
												 :bench2   [0.4 1.0 0.4]
 
                         }

              color-to-cell (case index

							 0  (color-map :qb)
							 1  (color-map :rb)
							 2  (color-map :rb)
							 3  (color-map :wr)
							 4  (color-map :wr)
							 5  (color-map :te)
							 6  (color-map :flex)
							 7  (color-map :dst)
							 8  (color-map :k)
							 9  (color-map :bench1)
							 10 (color-map :bench2)
							 11 (color-map :bench1)
							 12 (color-map :bench2)
							 13 (color-map :bench1)
							 14 (color-map :bench2)
							 15 (color-map :bench1)

							 [0.8 0.8 0.8]) ]
         
         (.setBackground this-label (Color. (float (color-to-cell 0)) (float (color-to-cell 1)) (float (color-to-cell 2)) 0.4 ))
         (.setOpaque this-label true)
         this-label
       )
)))

(defn get-draft-cell-renderer
  []
	 (proxy [ListCellRenderer] []
       (getListCellRendererComponent
       [jlist value index isSelected? hasFocus?]
            (let [defaultRenderer (DefaultListCellRenderer.)
                  this-label (.getListCellRendererComponent defaultRenderer 
                               jlist value index isSelected? hasFocus?)

									new-label-text  (read-string (.getText this-label))
									new-label  (JLabel. (format gui-text-format 
                                     (new-label-text :adp) (new-label-text :name)
                                     (new-label-text :team)
                                     (new-label-text :bye)
                                     (new-label-text :vor)  )) 

                  vor-color  (get-vor-color (new-label-text :vor))
                  light-vor-color  (clamp-color (mapv #(+ 0.01 %) vor-color))

                  ;vor-color  [0.9 0.9 0.9]
                  new-color  (Color.  (float (vor-color 0)) (float (vor-color 1)) (float (vor-color 2)) (float 1.0))
;									sel-color  (Color.  (float (light-vor-color 0)) (float (light-vor-color 1)) (float (light-vor-color 2)) (float 0.5))]
									sel-color  (Color.  (float (vor-color 0)) (float (vor-color 1)) (float (vor-color 2)) (float 0.2))
									white-color  (Color. 1.0 1.0 1.0 1.0 )]
              
;                  (println "New cell" index hasFocus? isSelected?)

                  (.setOpaque new-label true)
         
                  (cond
                     (new-label-text :behind-adp)
												(do 
													(.setBorder new-label (BorderFactory/createLineBorder (Color. 1.0 0.0 0.0) 2  ))
													(.setBackground new-label sel-color))

                     (new-label-text :will-go)
												(do 
													(.setBorder new-label (BorderFactory/createLineBorder (Color. 0.0 0.0 0.0) 2  ))
													(.setBackground new-label new-color))

                     :else
     									(.setBackground new-label new-color)
									)


;								  (if (and hasFocus? isSelected?)
;											(.setBackground new-label sel-color)
;											(.setBackground new-label new-color))
                   

				(.setFont new-label (Font. "monospaced" Font/PLAIN 12) ) 
        new-label
))))



(defn annotate-to-go-pool
  [pool to-go]
  
  (let [set-to-go (set to-go)]
  
    (mapv (fn [v] 
            (if (contains? set-to-go v)
						 (assoc v :will-go true) 
						 (assoc v :will-go false) ))  pool
)))


(defn annotate-adp-pool
  [pool pick]
  
    (mapv (fn [v] 
            (if (> pick (v :adp))
						 (assoc v :behind-adp true) 
						 (assoc v :behind-adp false) ))  pool
))

(defn annotate-pool
  [pool to-go pick]
  
  (-> pool
    (annotate-to-go-pool to-go)
    (annotate-adp-pool   pick)
))


(defn update-gui-lists
  [gui-lists pool roster team dstack to-go]
  
  (.setSelectedIndex (gui-lists :team-select) team)
  (.setListData (gui-lists :roster)     (to-array (build-roster-list (roster team))))
  (.setListData (gui-lists :last-picks) (to-array (build-last-pick-list dstack)))

  
  (loop [ui-keys poskeys ]
    (if-let [thispos (first ui-keys)]
			(let [take-map  {:qb 20 :rb 20 :wr 20 :te 10 :dst 10 :k 10}]
			(doto 
				(gui-lists thispos)
				(.setListData (to-array (filter #(= (% :pos) thispos) (annotate-pool pool to-go (count dstack) ))))
       )
			(recur (next ui-keys))))
))

(defn start-gui
  [nteam]
( let [frame         (JFrame. "draftbuddy") 
       base-panel    (JPanel.)
       frame-layout  (BoxLayout. base-panel BoxLayout/PAGE_AXIS)

			 message-panel  (JPanel. (FlowLayout.))
			 roster-panel   (JPanel. (FlowLayout.))
			 draft1-panel   (JPanel. (FlowLayout.)) 
			 draft2-panel   (JPanel. (FlowLayout.)) 
			 control-panel   (JPanel. (FlowLayout.)) 
       default-items  (into-array ["Default 1" "Default 2"])
       
       pos-order      [:roster :last-picks :total-points :qb :rb :wr :te :dst :k]

       all-lists    (zipmap pos-order (repeatedly #(JList. default-items)))
       scrollpanes  (zipmap pos-order (repeatedly #(JScrollPane.)))
       
			 undo-button  (JButton. "Undo")
			 team-roster-sel  (JComboBox. (into-array (map #(format "Team %2d" %) (range 1 (inc nteam)) ) ))

       draft-label  (JLabel. "Round  1/Team  1 - Pick   1"  SwingConstants/CENTER) ]

  
	;		 (.setBorder (all-lists :roster) (BorderFactory/createLineBorder (Color. 0.0 0.0 0.0)))
   		 (.setBorder (all-lists :roster) (BorderFactory/createEtchedBorder EtchedBorder/LOWERED))
   		 (.setBorder (all-lists :total-points) (BorderFactory/createEtchedBorder EtchedBorder/LOWERED))
;   		 (.setBorder (all-lists :roster) (BorderFactory/createLoweredBevelBorder))
  
       (.setHorizontalAlignment draft-label JLabel/CENTER)
       (mapv #(.setViewportView (scrollpanes %) (all-lists %)) [:qb :rb :wr :te :dst :k :last-picks])

;       (.setViewportView (scrollpanes :last-picks) (all-lists :last-picks))

       
  (mapv #(.setCellRenderer (all-lists %) (get-draft-cell-renderer)) [:qb :rb :wr :te :dst :k])
  (.setCellRenderer (all-lists :roster) (get-roster-cell-renderer))

  (.setPreferredSize (all-lists :roster)       (Dimension. 330 280))
  (.setPreferredSize (scrollpanes :last-picks)   (Dimension. 380 280))
	(.setPreferredSize (all-lists :total-points) (Dimension. 280 280))
  
  (doto roster-panel
		(.add (all-lists :roster))
;		(.add (all-lists :last-picks))
		(.add (scrollpanes :last-picks))
		(.add (all-lists :total-points))
   )

  (mapv #(.setFont (all-lists %) (Font. "monospaced" Font/PLAIN 12) ) (keys all-lists))
  
;  (.setFont draft-label (Font. (.getName (.getFont draft-label) Font/PLAIN 12) ))
  (.setFont draft-label (Font. "" Font/BOLD 20) )
  
  (mapv #(.setPreferredSize (scrollpanes %) (Dimension. 330 240)) [:qb :rb :wr]  )
  (mapv #(.setPreferredSize (scrollpanes %) (Dimension. 330 120)) [:te :k :dst]  )
  
;  (mapv #(.add draft1-panel (all-lists %)) [:qb :rb :wr])
;  (mapv #(.add draft2-panel (all-lists %)) [:te :k :dst])

  (mapv #(.add draft1-panel (scrollpanes %)) [:qb :rb :wr])
  (mapv #(.add draft2-panel (scrollpanes %)) [:te :k :dst])
  
  (doto control-panel
    (.add team-roster-sel)
    (.add (Box/createRigidArea (Dimension. 280 1)  ))
    (.add undo-button)
    (.add (Box/createRigidArea (Dimension. 325 1)  ))
    )

  (doto message-panel
		 (.add draft-label)
   )
  (doto base-panel
		 (.setLayout frame-layout)
		 (.add message-panel)
		 (.add roster-panel)
;		 (.add undo-button)
		 (.add control-panel)
		 (.add draft1-panel)
		 (.add draft2-panel)
  )
 
 

  (doto frame
   (.add base-panel)
	 (.setSize 800 800)
   (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
  .pack
   (.setVisible true))
   (-> all-lists 
     (assoc :draft-status draft-label)
     (assoc :undo-button  undo-button)
     (assoc :team-select  team-roster-sel)
     )
))
  
  