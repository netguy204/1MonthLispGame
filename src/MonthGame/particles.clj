(ns MonthGame.particles
  (:use MonthGame.entity
	MonthGame.vector
	MonthGame.util
	MonthGame.draw
	MonthGame.scalar-math))

(import '(java.awt Color Graphics Dimension))

(def *drag-factor* 0.9)

(defn simple-drag [p world dt-secs]
  (if (> (:age p) 2) nil
      (let [old-pos (position p)
	    old-vel (:vel p)
	    pos (vadd old-pos (vmul old-vel dt-secs))
	    vel (vsub old-vel (vmul old-vel (* *drag-factor* dt-secs)))]
	(assoc p :pos pos :vel vel))))

(defn simple-draw [p g]
  (let [pt1 (vint (position p))
	pt2 (vadd pt1 [3 3])]
    (doto g
      (.setColor (. Color gray))
      (fill-rect pt1 pt2))))

(defrecord Particle
  [pos vel update-fn draw-fn]

  NPE
  (update [p world dt-secs]
	  (with-age [p dt-secs]
	    (update-fn p world dt-secs)))

  Entity
  (draw [p g]
	(with-age [p 0]
	  (draw-fn p g)))

  (draw-meta [p g] nil)

  (position [p] pos)

  (radius [p] 0)

  (collided-with [p other] p))

(derive Particle :MonthGame.entity/non-interacting-npe)

(def *perp-vel-scale* 0.3)

(defn emit-basic-particle [pos vel]
  "emit a particle in vel direction with a random perpendicular velocity"
  (let [vel (vmul vel 0.5)
	vp (vmul (vperp vel)
		 (* (vmag vel)
		    (rand-around-zero *perp-vel-scale*)))
	vel (vadd vel vp)]
    (Particle. pos vel simple-drag simple-draw)))
