(in-ns 'draftbuddy.core)

(import '(java.awt Color BorderLayout FlowLayout Font Dimension)) 
(import '(javax.swing JFrame JButton JOptionPane JList ListCellRenderer 
                      JLabel JComponent BorderFactory DefaultListCellRenderer 
                      BoxLayout JPanel )) 
(import '(java.awt.event ActionListener))          


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
						 (str/join (repeat 34 " "))
						 (format gui-text-format 
										 (thisplayer :adp) (thisplayer :name) (thisplayer :vor) )))
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

(defn clamp-color
  [color]
  (let [cupper (map #(if (> 1.0 %) 1.0 %) color)
	      clower (map #(if (< 0.0 %) 0.0 %) cupper)]
    (println clower)
  clower))

(defn get-vor-color
  [vor]
  (println vor)
  (if (nil? vor)
		[0.8 0.8 0.8]
		(let [ interp-color
			[ (/ (+ (- vor) 5.0) 10.0)
					(/ (+ vor 5.0) 10.0)
					0.5 ]]
				(clamp-color interp-color)))
)

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
                                     (new-label-text :adp) (new-label-text :name) (new-label-text :vor)  )) 

                  unvor-color  (get-vor-color (new-label-text :vor))
                  vor-color  [0.9 0.9 0.9]
                  new-color  (Color.  (float (vor-color 0)) (float (vor-color 1)) (float (vor-color 2)) (float 1.0))
									sel-color  (Color.  (float (vor-color 0)) (float (vor-color 1)) (float (vor-color 2)) (float 0.2))]

                  (.setOpaque new-label true)
								  (if (and hasFocus? isSelected?)
                      (do (.setBorder new-label (BorderFactory/createLineBorder (Color. 0.0 0.0 0.0)))
												  (.setBackground new-label sel-color))
											(.setBackground new-label new-color))
                   

				(.setFont new-label (Font. "monospaced" Font/PLAIN 12) ) 
        new-label
))))


(defn update-gui-lists
  [gui-lists pool roster team]
  
  (.setListData (gui-lists :roster) (to-array (build-roster-list (roster team))))

  
  (loop [ui-keys poskeys ]
    (if-let [thispos (first ui-keys)]
			(let [take-map  {:qb 20 :rb 20 :wr 20 :te 10 :dst 10 :k 10}]
			(doto 
				(gui-lists thispos)
				(.setListData (to-array (take (take-map thispos) (filter #(= (% :pos) thispos) pool))))
       )
			(recur (next ui-keys))))
))

(defn start-gui
  []
( let [frame         (JFrame. "draftbuddy") 
       base-panel    (JPanel.)
       frame-layout  (BoxLayout. base-panel BoxLayout/PAGE_AXIS)

			 roster-panel   (JPanel. (FlowLayout.))
			 draft1-panel   (JPanel. (FlowLayout.)) 
			 draft2-panel   (JPanel. (FlowLayout.)) 
       default-items  (into-array ["Default 1" "Default 2"])
       
       pos-order      [:roster :total-points :qb :rb :wr :te :dst :k]

       all-lists    (zipmap pos-order (repeatedly #(JList. default-items)))
       
       cell-renderer (get-cell-renderer)
       draft-label  (JLabel. "Default") ]

       
  (mapv #(.setCellRenderer (all-lists %) (get-draft-cell-renderer)) [:qb :rb :wr :te :dst :k])

  (.setPreferredSize (all-lists :roster)       (Dimension. 240 300))
	(.setPreferredSize (all-lists :total-points) (Dimension. 240 300))
  
  (doto roster-panel
		(.add (all-lists :roster))
		(.add (all-lists :total-points))
   )

  (mapv #(.setFont (all-lists %) (Font. "monospaced" Font/PLAIN 12) ) (keys all-lists))
  (mapv #(.setPreferredSize (all-lists %) (Dimension. 240 240)) [:qb :rb :wr]  )
  (mapv #(.setPreferredSize (all-lists %) (Dimension. 240 120)) [:te :k :dst]  )
  
  (mapv #(.add draft1-panel (all-lists %)) [:qb :rb :wr])
  (mapv #(.add draft2-panel (all-lists %)) [:te :k :dst])


  (doto base-panel
		 (.setLayout frame-layout)
		 (.add  draft-label)
		 (.add  roster-panel)
		 (.add  draft1-panel)
		 (.add  draft2-panel)
  )
 
 
  (doto frame
   (.add base-panel)
	 (.setSize 800 960)
;  .pack
   (.setVisible true))
   (assoc all-lists :draft-status draft-label)
))
  
  