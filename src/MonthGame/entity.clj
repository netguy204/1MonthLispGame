(ns MonthGame.entity
  (:use MonthGame.vector))

(defprotocol NPE
  "An entity that's not under player control"
  (update [npe world dt-secs] "update the npes position / state"))

(defprotocol Entity
  "Something that can be drawn on the screen"
  (draw [entity g] "draw it")
  (draw-meta [entity g] "draw to the meta-layer")
  (position [entity] "position of center-of-mass")
  (radius [entity] "average mass containing radius")
  (can-collide? [entity] "true if entity can be collided with"))

(defn not-nil? [obj]
  (not (nil? obj)))

(defn update-npes [world dt-secs]
  (let [updates (mapcat #(update % world dt-secs) (:npes world))]
    (assoc world :npes (filter not-nil? updates))))

(defmacro with-each-collision [[a coll1 b coll2] & forms]
  `(dorun
    (for [~a (filter can-collide? ~coll1)
	  ~b (filter can-collide? ~coll2)]
      (let [cm-range# (vdist (position ~a) (position ~b))
	    edge-range# (max 0 (- cm-range# (radius ~a) (radius ~b)))]
	(if (zero? edge-range#)
	  (do ~@forms))))))

(defn add-npe-to-world [npe world]
  (alter world assoc :npes (concat npe (:npes @world))))

(defn ypos-for-entity [entity]
  (let [[x y] (position entity)]
    y))

(defmacro with-each-entity [world var & forms]
  `(let [entities#
	 (sort-by ypos-for-entity
		  (concat (map deref (:tanks ~world))
			  (:npes ~world)
			  (:barriers ~world)))]
    (doseq [~var entities#]
	      ~@forms)))
