(ns MonthGame.missiles
  (:use MonthGame.vector
	MonthGame.draw
	MonthGame.sprite
	MonthGame.entity
	MonthGame.explosions
	MonthGame.weapons
	MonthGame.sound))

(def *rocket-frames*
     (let [img-stream (get-resource "MonthGame/rocketsprite.png")]
       (load-sprites img-stream 64)))

(def *projectile-sound* "MonthGame/cannon2.mp3")

(defn rocket-spd-eqn [age max-speed]
  (cond
   ;; linear acceleration
   (< age 1) (* age max-speed)
   ;; top speed
   true max-speed))
(def *rocket-radius-factor* 0.7)

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
	      (list (make-explosion end dir))
	      (list (assoc npe :pos newpos :age age)))))
  
  Entity
  (draw [npe g]
	(let [dir (unit-vector (vsub end start))
	      angle (vang dir)
	      frameno (angle-to-frame angle (count *rocket-frames*))
	      frame (nth *rocket-frames* frameno)
	      off (middle-img frame)
	      tgt (vint (vsub (position npe) off))]
	  (draw-img g frame tgt)))

  (draw-meta [npe g] nil)

  (position [npe]
	    (or (:pos npe) start))

  (radius [npe] (* *rocket-radius-factor*
		   (/ (.getWidth (first *rocket-frames*)) 2)))

  (can-collide? [npe]
		(let [age (or (:age npe) 0)]
		  (> age 1))))

(defn make-rocket [start end max-speed]
  (Rocket. start end max-speed))

(def *max-speed* 300)

(defn load-icon-file [fname]
  (scale-img (load-img (get-resource fname)) 32 32))

(def *rocket-icon* (load-icon-file "MonthGame/rocketicon.png"))

(defrecord RocketLauncher
  [rockets]

  Weapon
  (fire [wpn pos target]
	(alter rockets dec)
	(play-stream (get-resource *projectile-sound*))
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

(def *speed-spread* 100)

(defn rand-around-zero [max]
  (- (rand-int max) (* max 2)))

(defrecord MultiRocketLauncher
  [rockets salvo-size]

  Weapon
  (fire [wpn pos target]
	(alter rockets dec)
	(let [rands (repeatedly salvo-size #(rand-around-zero *speed-spread*))]
	  (map #(make-rocket pos target (+ *max-speed* %)) rands)))
				
  (icon [wpn] *multirocket-icon*)
  
  (shots [wpn] @rockets)

  (range-for-energy [wpn energy] 
		    (* 10 energy))
  
  (energy-used [wpn pos target]
	       (/ (vdist pos target) 10)))

(defn make-multirocket-launcher [rockets salvo-size]
  (let [r (ref rockets)]
    (MultiRocketLauncher. r salvo-size)))


(def *projectile-img*
     (scale-img (load-img (get-resource "MonthGame/sphere.png")) 8 8))

(def *projectile-shadow-img*
     (scale-img (load-img (get-resource "MonthGame/sphere_shadow.png")) 16 16))

(def *acceleration-of-gravity* 70.0)

(defn projectile-eqn [start end age theta]
  (let [d (vdist end start)
	tan (Math/tan theta)
	dir (unit-vector (vsub end start))
	vz0 (Math/sqrt (/ (* d tan *acceleration-of-gravity*) 2))
	z (- (* vz0 age) (* 0.5 age age *acceleration-of-gravity*))
	plane-vel (/ vz0 tan)
	plane-pos (vadd start (vmul dir (* plane-vel age)))]
    (list plane-pos z)))
	
(defrecord Projectile
  [start end theta]

  NPE
  (update [npe world dt-secs]
	  (let [old-age (or (:age npe) 0)
		age (+ old-age dt-secs)
		[pp z] (projectile-eqn start end age theta)]
	    (if (<= z 0)
	      (make-radial-explosions end 5)
	      (list (assoc npe :age age)))))

  Entity
  (draw [npe g]
	(let [age (or (:age npe) 0)
	      [shad-loc z] (projectile-eqn start end age theta)
	      proj-loc (vadd shad-loc (list 0 (neg z)))
	      off (middle-img *projectile-img*)
	      shad-loc-off (vsub shad-loc off)
	      proj-loc-off (vsub proj-loc off)]
	  (draw-img g *projectile-shadow-img* (vint shad-loc-off))
	  (draw-img g *projectile-img* (vint proj-loc-off))))

  (draw-meta [npe g] nil)

  (position [npe]
	    (let [age (or (:age npe) 0)
		  [pp z] (projectile-eqn start end age theta)]
	      pp))

  (radius [npe] (.getHeight *projectile-img*))

  (can-collide? [npe]
		(let [age (or (:age npe) 0)
		      [shad-loc z] (projectile-eqn start end age theta)]
		  (and (> age 1) (< z 16)))))

(def *default-projectile-theta* (/ Math/PI 4))
(def *projectile-icon* 
     (let [img (make-img 32 32)
	   bg (.getGraphics img)
	   pos (vsub (middle-img img) (middle-img *projectile-img*))]
       (doto bg
	 (draw-img *projectile-img* pos)
	 (.dispose))
       img))
     
(defrecord ProjectileLauncher
  []
  
  Weapon
  (fire [wpn pos target]
	(let [dir (unit-vector (vsub target pos))
	      emit-pos (vadd pos (vadd (vmul dir 20) '(0 -10)))]
	  (play-stream (get-resource *projectile-sound*))
	  [(Projectile. emit-pos target *default-projectile-theta*)]))

  (icon [wpn] *projectile-icon*)

  (shots [wpn] 100)

  (range-for-energy [wpn energy] (* energy 2))

  (energy-used [wpn pos target] (/ (vdist pos target) 2)))
	
(defn make-projectile-launcher []
  (ProjectileLauncher.))
