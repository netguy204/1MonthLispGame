;; particles.clj
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

(ns MonthGame.particles
  (:use (MonthGame entity vector util graphics
		   draw scalar-math sprite
		   surface mouse animator))
  (:import (java.awt Color Graphics2D Dimension
		     AlphaComposite)))

(def *drag-factor* 0.3)

(defn too-old? [p]
  (if (> (:age p) (:max-age p)) true false))

(defn simple-drag [p world dt-secs]
  (if (too-old? p) nil
      (let [old-pos (position p)
	    old-vel (:vel p)
	    pos (vadd old-pos (vmul old-vel dt-secs))
	    vel (vsub old-vel (vmul old-vel (* *drag-factor* dt-secs)))]
	(assoc p :pos pos :vel vel))))

(defn make-simple-draw [imgs]
  (fn [p #^Graphics2D g]
    (let [age-factor (max 0.0 (min 1.0 (/ (float (:age p)) (:max-age p))))
	alpha (- 1 age-factor)
	num-frames (count imgs)
	frameno (int (min (dec num-frames) (max 0 (* num-frames age-factor))))
	frame (nth imgs frameno)
	pos (vint (vsub (position p) (middle-img frame)))
	comp (AlphaComposite/getInstance AlphaComposite/SRC_OVER alpha)]
    (with-new-graphics g
      (doto g
	(.setComposite comp)
	(draw-img frame pos))))))

(defrecord Particle
  [pos vel update-fn draw-fn max-age]

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

(def *perp-vel-scale* 35.0)
(def *forward-vel* 5.0)
(def *offset-backward* 40)

(defn make-basic-emitter [imgs par-speed perp-speed offset-back
			  update-fn max-age-fn]
  "emit a particle in vel direction with a random perpendicular velocity"
  (let [draw (make-simple-draw imgs)]
    (fn [pos vel]
      (let [dir (unit-vector vel)
	    vp (vmul (vperp vel) (rand-around-zero perp-speed))
	    vel (vadd (vmul dir par-speed) vp)
	    pos (vsub pos (vmul dir offset-back))]

	(Particle. pos vel update-fn draw (max-age-fn))))))

(defn make-radial-emitter [imgs max-particles max-speed update-fn max-age-fn]
  "emit a partical in a random radial direction"
  (let [draw (make-simple-draw imgs)]
    (fn [pos vel]
      (for [_ (range (inc (rand-int max-particles)))]
	(let [vel {:angle (unit-vector (rand-around-zero Math/PI))
		   :mag (rand-around-zero max-speed)}]
	  (Particle. pos vel update-fn draw (max-age-fn)))))))

(defn- load-with-scales [fname scales]
  (let [img (load-img (get-resource fname))]
    (doall (map #(scale-img img % %) scales))))

(def *max-age* 2)
(defn- constant-max-age []
  *max-age*)

(defn make-max-age-spread [min max]
  (fn [] (+ min (rand (- max min)))))

(def *smoke-particles*
     (load-with-scales
       "MonthGame/smoke.png" 
       (range 32 128)))

(def emit-basic-particle
     (make-basic-emitter *smoke-particles*
			 *forward-vel*
			 *perp-vel-scale*
			 *offset-backward*
			 simple-drag
			 constant-max-age))

(def *fire-particles*
     (load-with-scales
       "MonthGame/fire.png"
       (range 16 96 4)))


;; at the moment this looks pretty dumb
(def *smoke-prob* 0.1)

(defn- simple-drag-with-smoke [p world dt-secs]
  (let [fire (simple-drag p world dt-secs)
	new-vel (vmul (:vel p) 5)]
    (if (and (> (/ (:age p) (:max-age p)) 0.9)
	     (> *smoke-prob* (rand)))
      (filter #(not (nil? %))
	      [fire (emit-basic-particle (:pos p) new-vel)])
      fire)))

(defn drift-up [p world dt-secs]
  (if (too-old? p) nil
      (let [vel (vadd (:vel p) (list 0 (neg (:age p))))
	    pos (vadd (position p) (vmul vel dt-secs))]
	(assoc p :pos pos :vel vel))))

(def emit-fire-particle
     (make-radial-emitter
      *fire-particles*
      6
      30
      drift-up
      (make-max-age-spread 1 3)))

;; a partical system test program that makes the pointer
;; into an emitter for whatever system is defiend by
;; *curr-test-emitter*
(def *curr-test-emitter* emit-fire-particle)

(def *particles* (ref []))

(defn- test-draw [g this]
  (let [width (.getWidth this)
	height (.getHeight this)
	tl '(0 0)
	br (list width height)]
    (doto g
      (.setColor (. Color white))
      (fill-rect tl br))
    (dosync
     (doseq [p @*particles*]
       (draw p g)))))

(defn- test-surface []
  (defonce *test-surface* (make-surface test-draw))
  *test-surface*)

(defn- update-test-particles [particles dt-secs]
  (filter #(not (nil? %)) (flatten (map #(update % nil dt-secs) particles))))

(def *test-mouse* (make-mouse nil-on-wheel))

(defn- test-animation [dt-secs]
  (dosync
   (alter *particles* update-test-particles dt-secs)
   (if (:pos @*test-mouse*)
     (alter *particles* 
	    #(flatten (conj % (*curr-test-emitter*
			       (:pos @*test-mouse*)
			       '(0 -1))))))
   (.repaint (test-surface))))

(def *test-animator* (make-animator test-animation 50))

(defn particle-test-window []
  (let [frame (make-window "Particle Test")
	panel (test-surface)
	close-handler (stop-animation-listener *test-animator*)]
			   
    (doto frame
      (.addWindowListener close-handler)
      (.setSize 800 600)
      (.add panel)
      (.setVisible true))
    (attach-mouse-adapter panel (make-mouse-adapter *test-mouse*))
    (start-animation *test-animator*)))
