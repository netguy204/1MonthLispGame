(ns MonthGame.entity)

(defprotocol NPE
  "An entity that's not under player control"
  (update [npe world dt-secs] "update the npes position / state"))

(defprotocol Entity
  "Something that can be drawn on the screen"
  (draw [entity g] "draw it")
  (draw-meta [entity g] "draw to the meta-layer")
  (position [entity] "position of center-of-mass"))

(defn not-nil? [obj]
  (not (nil? obj)))

(defn update-npes [world dt-secs]
  (alter world assoc
	 :npes (filter not-nil? (map #(update % @world dt-secs) (:npes @world)))))

(defn add-npe-to-world [npe world]
  (alter world assoc :npes (concat npe (:npes @world))))

(defn ypos-for-entity [entity]
  (let [[x y] (position entity)]
    y))

(defmacro with-each-entity [world var & forms]
  `(let [entities# (sort-by ypos-for-entity
			    (concat (map deref (:tanks ~world)) (:npes ~world)))]
    (dorun (for [~var entities#]
	      ~@forms))))
