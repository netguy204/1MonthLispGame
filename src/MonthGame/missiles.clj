(ns MonthGame.missiles
  (:use MonthGame.vector
	MonthGame.draw
	MonthGame.sprite
	MonthGame.npe
	MonthGame.explosions
	MonthGame.weapons))

(import '(java.awt.image BufferedImage))

(def *rocket-frames*
     (let [img-stream (get-resource "MonthGame/rocketsprite.png")]
       (load-sprites img-stream 64)))

(defn rocket-spd-eqn [age max-speed]
  (cond
   ;; linear acceleration
   (< age 1) (* age max-speed)
   ;; top speed
   true max-speed))
   
;; a rocket is an unguided self-powered projectile
(defrecord Rocket
  [start end max-speed pos]
  
  NPE
  (update [npe world dt-secs]
	  (let [old-age (or (:age npe) 0)
		age (+ old-age dt-secs)
		to-end (vsub end start)
		dist-to-end (vmag to-end)
		dir (unit-vector to-end)
		scale (* (rocket-spd-eqn age max-speed) dt-secs)
		newpos (vadd pos (vmul dir scale))]
	    (if (is-past? end pos dir)
	      (make-explosion end dir)
	      (assoc npe :pos newpos :age age))))
  
  (draw [npe g]
	(let [dir (unit-vector (vsub end start))
	      angle (vang dir)
	      frameno (angle-to-frame angle (count *rocket-frames*))
	      frame (nth *rocket-frames* frameno)
	      off (list (/ (.getWidth frame) 2) (/ (.getHeight frame) 2))
	      tgt (vint (vsub pos off))]
	  (draw-img g frame tgt)))

  (position [npe] pos))

(defn make-rocket [start end max-speed]
  (Rocket. start end max-speed start))

(def *max-speed* 300)

(defrecord RocketLauncher
  [rockets]

  Weapon
  (fire [wpn pos target]
	(alter rockets dec)
	(make-rocket pos target *max-speed*))
  
  (icon [wpn]
	(let [img (new BufferedImage 32 32
		       (. BufferedImage TYPE_INT_ARGB))
	      bg (.getGraphics img)
	      rocket (first *rocket-frames*)
	      rw (.getWidth rocket)
	      rh (.getHeight rocket)]
	  (doto bg
	    (.drawImage rocket 0 0 32 32 0 0 rw rh nil)
	    (.dispose))
	  img))
  
  (shots [wpn] @rockets)

  (range-for-energy [wpn energy] 
		    (* 10 energy))
  
  (energy-used [wpn pos target]
	       (/ (vdist pos target) 10)))

(defn make-rocket-launcher [rockets]
  (let [r (ref rockets)]
    (RocketLauncher. r)))
