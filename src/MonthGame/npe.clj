(ns MonthGame.npe)

(defprotocol NPE
  "An entity that's not under player control"
  (update [npe world dt-secs] "update the npes position / state")
  (draw [npe g] "draw the npe")
  (position [npe] "position of npe center-of-mass"))

(defn not-nil? [obj]
  (not (nil? obj)))

(defn update-npes [world dt-secs]
  (alter world assoc
	 :npes (filter not-nil? (map #(update % @world dt-secs) (:npes @world)))))

(defn draw-npes [g world]
  (dorun
      (for [npe (:npes world)]
	(draw npe g))))

