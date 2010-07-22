;; missiles.clj
;; Turn based tank combat game
;;
;; Copyright 2010 Brian Taylor
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns MonthGame.missiles
  (:use (MonthGame vector draw sprite util
		   entity explosions weapons
		   sound scalar-math particles
		   graphics)))

(def *rocket-frames*
     (let [img-stream (get-resource "MonthGame/rocketsprite.png")]
       (scale-img (load-sprites img-stream) 64 64)))

(defn rocket-spd-eqn [age max-speed]
  (cond
   ;; linear acceleration
   (< age 1) (* age max-speed)
   ;; top speed
   true max-speed))
(def *rocket-radius-factor* 0.7)

;; a rocket is an unguided self-powered projectile
(defrecord Rocket
  [start end max-speed sprite]
  
  NPE
  (update [npe world dt-secs]
	  (let [old-age (or (:age npe) 0)
		age (+ old-age dt-secs)
		to-end (vsub end start)
		dist-to-end (vmag to-end)
		dir (unit-vector to-end)
		spd (rocket-spd-eqn age max-speed)
		vel (vmul dir spd)
		newpos (vadd (position npe) (vmul vel dt-secs))]
	    (if (is-past? end (position npe) dir)
	      (make-particle-explosion end dir 6 0.5)
	      (list (emit-basic-particle newpos vel)
		    (assoc npe :pos newpos :age age)))))
  
  Entity
  (draw [npe g]
	(let [pos (or (:pos npe) start)]
	  (with-offset-g [g pos]
	    (draw-sprite g sprite))))

  (draw-meta [npe g] nil)

  (position [npe]
	    (or (:pos npe) start))

  (radius [npe] (* *rocket-radius-factor*
		   (/ (img-height sprite) 2)))

  (collided-with [npe other]
		 (let [dir (unit-vector (vsub end start))]
		   (make-particle-explosion (position npe) (vneg dir) 4 1.5))))

(derive Rocket ::has-age)

(defn make-rocket [start end max-speed]
  (let [dir (unit-vector (vsub end start))
	sprite (make-oriented-sprite *rocket-frames* dir)]
    (Rocket. start end max-speed sprite)))

(def *max-speed* 300)

(defn load-icon-file [fname]
  (scale-img (load-img (get-resource fname)) 32 32))

(def *rocket-icon* (load-icon-file "MonthGame/rocketicon.png"))

(def *projectile-sound*
     (read-audio-frames (get-resource "MonthGame/cannon2.mp3")))

(defrecord RocketLauncher
  [rockets]

  Weapon
  (fire [wpn pos target]
	(alter rockets dec)
	(play-async *projectile-sound*)
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

(def *acceleration-of-gravity* 120.0)

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
  [start end theta sprite]

  NPE
  (update [npe world dt-secs]
	  (let [old-age (or (:age npe) 0)
		age (+ old-age dt-secs)
		[pp z] (projectile-eqn start end age theta)]
	    (if (<= z 0)
	      (make-radial-explosions end 5)
	      (assoc npe :age age))))

  Entity
  (draw [npe g]
	(let [age (or (:age npe) 0)
	      [pos z] (projectile-eqn start end age theta)
	      sprite (assoc sprite :height z)]

	  (with-offset-g [g pos]
	    (draw-sprite g sprite))))
	  

  (draw-meta [npe g] nil)

  (position [npe]
	    (let [age (or (:age npe) 0)
		  [pp z] (projectile-eqn start end age theta)]
	      pp))

  (radius [npe] (img-height sprite))

  (collided-with [npe other]
		 (make-radial-explosions (position npe) 10)))

(derive Projectile ::has-age)

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
	(play-async *projectile-sound*)
	(let [dir (unit-vector (vsub target pos))
	      emit-pos (vadd pos (vadd (vmul dir 20) '(0 -10)))
	      sprite (make-elevated-sprite *projectile-img*
					   *projectile-shadow-img*
					   0)]

	  [(Projectile. emit-pos target *default-projectile-theta* sprite)]))

  (icon [wpn] *projectile-icon*)

  (shots [wpn] 100)

  (range-for-energy [wpn energy] (* energy 2))

  (energy-used [wpn pos target] (/ (vdist pos target) 2)))
	
(defn make-projectile-launcher []
  (ProjectileLauncher.))
