(ns MonthGame.particles
  (:use MonthGame.entity
	MonthGame.vector
	MonthGame.util
	MonthGame.draw
	MonthGame.scalar-math
	MonthGame.sprite))

(import '(java.awt Color Graphics Dimension
		   AlphaComposite))

(def *drag-factor* 0.9)

(def *max-age* 2)

(defn simple-drag [p world dt-secs]
  (if (> (:age p) *max-age*) nil
      (let [old-pos (position p)
	    old-vel (:vel p)
	    pos (vadd old-pos (vmul old-vel dt-secs))
	    vel (vsub old-vel (vmul old-vel (* *drag-factor* dt-secs)))]
	(assoc p :pos pos :vel vel))))

(def *smoke-particles*
     (map #(scale-img (load-img (get-resource "MonthGame/smoke.png")) % %)
	  (range 32 128)))

(defmacro with-new-graphics [graphics & forms]
  `(let [~graphics (.create ~graphics)
	 result# (do ~@forms)]
     (.dispose ~graphics)
     result#))

(defn simple-draw [p g]
  (let [age-factor (max 0.0 (min 1.0 (/ (:age p) *max-age*)))
	alpha (- 1 age-factor)
	num-frames (count *smoke-particles*)
	frameno (int (min (dec num-frames) (max 0 (* num-frames age-factor))))
	frame (nth *smoke-particles* frameno)
	pos (vint (vsub (position p) (middle-img frame)))
	comp (AlphaComposite/getInstance AlphaComposite/SRC_OVER alpha)]
    (with-new-graphics g
      (doto g
	(.setComposite comp)
	(draw-img frame pos)))))

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

(def *perp-vel-scale* 25.0)
(def *forward-vel* 5.0)
(def *offset-backward* 40)

(defn emit-basic-particle [pos vel]
  "emit a particle in vel direction with a random perpendicular velocity"
  (let [dir (unit-vector vel)
	vp (vmul (vperp vel) (rand-around-zero *perp-vel-scale*))
	vel (vadd (vmul dir *forward-vel*) vp)
	pos (vsub pos (vmul dir *offset-backward*))]

    (Particle. pos vel simple-drag simple-draw)))
