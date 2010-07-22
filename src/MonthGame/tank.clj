;; tank.clj
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

(ns MonthGame.tank
  (:use (MonthGame vector scalar-math sprite
		   draw entity weapons missiles
		   graphics wall))
  (:import (java.awt Color)))

(defn tank-target-pos [tank]
  (let [angle (:angle tank)]
    (vadd (position tank) {:angle angle :mag (:charge tank)})))

(defn default-weapon [tank]
  (nth (:weapons tank) (:current-weapon tank)))

(defn draw-energy-bar [tank g]
  (let [pos (position tank)
	radius 32
	below (vadd pos (list (neg radius) radius))
	width (* radius 2)
	energy (:life-energy tank)
	energy-width (* energy width)
	height 10
	br (vadd below (list width height))
	energy-br (vadd below (list energy-width height))]
    (doto g
      (.setColor (. Color black))
      (fill-rect below br)
      (.setColor (. Color green))
      (fill-rect below energy-br))))

(defn- draw-tank-meta [tank g]
  (let [target (tank-target-pos tank)
	max-fire-range (range-for-energy (default-weapon tank)
					 (:fire-energy tank))]
    (doto g
      (.setColor (. Color red))
      (draw-circle (position tank) max-fire-range 30)
      (.setColor (. Color blue))
      (draw-circle (position tank) (:move-energy tank) 30)
      (.setColor (. Color black))
      (draw-leader (position tank) 60 (:angle tank)))
    
    (if (and (:charge tank) (> (:charge tank) 0))
      (let [scale (max 30 (* 0.25 (:charge tank)))
	    d1 {:angle (/ Math/PI 4) :mag scale}
	    d2 {:angle (neg (/ Math/PI 4)) :mag scale}
	    l1a (vsub target d1)
	    l1b (vadd target d1)
	    l2a (vsub target d2)
	    l2b (vadd target d2)]
	(doto g
	  (.setColor (. Color red))
	  (draw-line l1a l1b)
	  (draw-line l2a l2b)
	  (draw-circle target scale 30))))
    (draw-energy-bar tank g)))

(def *tank-radius-factor* 0.5)

;oto = offset to origin
;center of mass - top left corner of sprite
(defrecord Tank
  [angle pos sprite
   rotate-rate move-rate
   move-energy fire-energy
   max-move-energy max-fire-energy
   charge charge-rate
   weapons current-weapon
   life-energy]
  
  Object
  (toString [tank]
	    (format "Tank with %d weapons and %f life-energy"
		    (count weapons) life-energy))
  Entity
  (draw-meta [tank g]
	     (draw-tank-meta tank g))

  (draw [tank g]
	(let [sprite (assoc sprite :angle angle)]
		     
	  (with-offset-g [g (position tank)]
	    (draw-sprite g sprite))))

  (position [tank] (:pos tank))
  
  (radius [tank] (* *tank-radius-factor*
		    (/ (first (img-size sprite)) 2)))

  (collided-with [tank other] tank))

(defmethod damage [Tank MonthGame.missiles.Rocket]
  [tank rocket]
  (assoc tank :life-energy
	 (max 0 (- (:life-energy tank) 0.2))))

(defmethod damage [Tank MonthGame.missiles.Projectile]
  [tank rocket]
  (assoc tank :life-energy
	 (max 0 (- (:life-energy tank) 0.1))))

(defn reset-tank-energy [tank]
  (assoc tank
    :move-energy (:max-move-energy tank)
    :fire-energy (:max-fire-energy tank)))
  
(defn make-tank [frames doto angle pos]
  (let [rotate-rate (* Math/PI 0.25)
	move-rate 50
	move-energy 200
	fire-energy 200
	fire-charge 0
	charge-rate 130
	life-energy 1.0
	sprite (make-oriented-sprite frames angle doto)]

    (Tank. angle pos sprite
	   rotate-rate move-rate
	   move-energy fire-energy
	   move-energy fire-energy
	   fire-charge charge-rate
	   nil nil life-energy)))


(defn subtract-energy [tank energy-type factor]
  (assoc tank energy-type 
	 (max 0 (- (energy-type tank) factor))))

(defn subtract-move-energy [tank factor]
  (subtract-energy tank :move-energy factor))

(defn subtract-fire-energy [tank factor]
  (subtract-energy tank :fire-energy factor))

(defn can-move? [tank]
  (> (:move-energy tank) 0))

(defn can-fire? [tank]
  (> (:fire-energy tank) 0))

(defn tank-depleted? [tank]
  (and (not (can-move? tank))
       (not (can-fire? tank))))

(defn- tank-barrier-collision? [pos radius barriers]
    (first (filter #(wall-hit-correction pos radius %) barriers)))

(defmethod MonthGame.entity/intersect [Tank MonthGame.wall.Wall]
  [tank wall]
  (not (nil? (wall-hit-correction (position tank)
				  (radius tank) wall))))

(defn move-towards-cursor [tank mouse barriers dt-secs]
  (let [to-mouse (vsub (:pos mouse) (position tank))
	tank-dir (discretize-angle (:angle tank) (:sprite tank))
	dxscale (* (:move-rate tank) dt-secs)
	rotate-dir (dir-to-rotate tank-dir to-mouse)
	dtscale (* (:rotate-rate tank) dt-secs rotate-dir)
	dtheta (vang to-mouse tank-dir)]

      ; only move if we're not where the mouse is
      (if (> (vmag to-mouse) 50)
	(if (within dtheta (frame-angle-tolerance (:sprite tank)))
	  ;; drive forward, we're as accurate in angle as we can be
	  (-> tank
	      (assoc 
		  :pos (vadd (position tank) (vmul tank-dir dxscale)))
	      (subtract-move-energy dxscale)
	      (subtract-fire-energy (* 2 dxscale)))

	  ;; turn to face the cursor
	  (-> tank
	      (assoc
		  :angle (add-on-circle (:angle tank) dtscale))
	      (subtract-move-energy (* dtscale 0.1))))
      tank)))

(defn age [ent]
  (or (:age ent) 0))

(defmethod MonthGame.entity/intersect
  [:MonthGame.missiles/has-age MonthGame.tank.Tank] [e1 e2]
  (if (< (age e1) 1)
    false
    (circle-intersect e1 e2)))
      
