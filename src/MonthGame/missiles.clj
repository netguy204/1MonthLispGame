(ns MonthGame.missiles
  (:use MonthGame.vector
	MonthGame.draw
	MonthGame.sprite
	MonthGame.entity
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
  [start end max-speed]
  
  NPE
  (update [npe world dt-secs]
	  (let [old-age (or (:age npe) 0)
		age (+ old-age dt-secs)
		to-end (vsub end start)
		dist-to-end (vmag to-end)
		dir (unit-vector to-end)
		scale (* (rocket-spd-eqn age max-speed) dt-secs)
		newpos (vadd (position npe) (vmul dir scale))]
	    (if (is-past? end (position npe) dir)
	      (make-explosion end dir)
	      (assoc npe :pos newpos :age age))))
  
  Entity
  (draw [npe g]
	(let [dir (unit-vector (vsub end start))
	      angle (vang dir)
	      frameno (angle-to-frame angle (count *rocket-frames*))
	      frame (nth *rocket-frames* frameno)
	      off (list (/ (.getWidth frame) 2) (/ (.getHeight frame) 2))
	      tgt (vint (vsub (position npe) off))]
	  (draw-img g frame tgt)))

  (draw-meta [npe g] nil)

  (position [npe]
	    (or (:pos npe) start)))

(defn make-rocket [start end max-speed]
  (Rocket. start end max-speed))

(def *max-speed* 300)

(defn load-icon-file [fname]
  (let [img (new BufferedImage 64 64
		 (. BufferedImage TYPE_INT_ARGB))
	bg (.getGraphics img)
	rocket (load-img (get-resource fname))
	rw (.getWidth rocket)
	rh (.getHeight rocket)]
    (doto bg
      (.drawImage rocket 0 0 64 64 0 0 rw rh nil)
      (.dispose))
    img))

(def *rocket-icon* (load-icon-file "MonthGame/rocketicon.png"))

(defrecord RocketLauncher
  [rockets]

  Weapon
  (fire [wpn pos target]
	(alter rockets dec)
	[(make-rocket pos target *max-speed*)])
  
  (icon [wpn] *rocket-icon*)
  
  (shots [wpn] @rockets)

  (range-for-energy [wpn energy] 
		    (* 10 energy))
  
  (energy-used [wpn pos target]
	       (/ (vdist pos target) 10)))

(defn make-rocket-launcher [rockets]
  (let [r (ref rockets)]
    (RocketLauncher. r)))

(def *multirocket-icon* (load-icon-file "MonthGame/multirocketicon.png"))

(def *speed-spread* 200)

(defn rand-around-zero [max]
  (- (/ max 2) (rand-int max)))

(defrecord MultiRocketLauncher
  [rockets salvo-size]

  Weapon
  (fire [wpn pos target]
	(alter rockets dec)
	(let [rands (repeatedly salvo-size #(rand-around-zero *speed-spread*))
	      sorted (sort rands)]
	  (map #(make-rocket pos target (+ *max-speed* %)) sorted)))
				
  (icon [wpn] *multirocket-icon*)
  
  (shots [wpn] @rockets)

  (range-for-energy [wpn energy] 
		    (* 10 energy))
  
  (energy-used [wpn pos target]
	       (/ (vdist pos target) 10)))

(defn make-multirocket-launcher [rockets salvo-size]
  (let [r (ref rockets)]
    (MultiRocketLauncher. r salvo-size)))
