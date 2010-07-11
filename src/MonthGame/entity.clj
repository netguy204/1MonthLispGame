(ns MonthGame.entity
  (:use MonthGame.vector
	MonthGame.util))

(defprotocol NPE
  "An entity that's not under player control"
  (update [npe world dt-secs] "update the npes position / state"))

(defprotocol Entity
  "Something that can be drawn on the screen"
  (draw [entity g] "draw it")
  (draw-meta [entity g] "draw to the meta-layer")
  (position [entity] "position of center-of-mass")
  (radius [entity] "average mass containing radius")
  (collided-with [entity other] "mutator called on collision"))

(defn not-nil? [obj]
  (not (nil? obj)))

(defn update-npes [world dt-secs]
  (let [updates (filter not-nil? 
			(flatten 
			 (map #(update % world dt-secs) 
			      (concat (:npes world) (:ni-npes world)))))
	npes (filter #(not (isa? (class %) ::non-interacting-npe)) updates)
	ni-npes (filter #(isa? (class %) ::non-interacting-npe) updates)]
    (assoc world :npes npes :ni-npes ni-npes)))

(defmulti intersect two-dispatch)

(defn circle-intersect [e1 e2]
  "do the circles surrounding these entities overlap?"
  (let [r (vdist (position e1) (position e2))
	edge-range (max 0 (- r (radius e1) (radius e2)))]
    (zero? edge-range)))

(defmethod intersect :default [e1 e2]
  (circle-intersect e1 e2))

(defmacro with-each-collision [[a coll1 b coll2] & forms]
  `(doseq [~a ~coll1
	   ~b ~coll2]
     (if (and (not (identical? ~a ~b))
	      (intersect ~a ~b))
	  (do ~@forms))))

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
			  (:ni-npes ~world)
			  (:barriers ~world)))]
    (doseq [~var entities#]
	      ~@forms)))
