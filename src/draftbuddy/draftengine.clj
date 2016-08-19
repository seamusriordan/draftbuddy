(ns draftbuddy.draftengine)

(require ['draftbuddy.core :as 'core])

(defn snakedraft
  [nteam]

  (loop [round 1 team 1 forward? true]
    (when (< round 16)

      (printf "Round %2d - Team %2d\n" round team)

      (if forward?
          (if (= team nteam)
            (recur (inc round) team false)
            (recur round (inc team) forward?)
            )
          (if (= team 1)
            (recur (inc round) team true)
            (recur round (dec team) forward?)
					)
			))))
          