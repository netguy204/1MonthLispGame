(ns MonthGame.tank
  (:use MonthGame.vector
	MonthGame.scalar-math
	MonthGame.sprite
	MonthGame.draw
	MonthGame.entity
	MonthGame.weapons))

(import '(java.awt Color))

(defn num-frames [tank]
  (count (:sprites tank)))

(defn tank-target-pos [tank]
  (let [angle (discretize-angle (:angle tank)
				(num-frames tank))
	dir (unitdir angle)
	target (vadd (position tank) (vmul dir (:charge tank)))]
  target))

(defn get-default-weapon [tank]
  (nth (:weapons tank) (:current-weapon tank)))

;oto = offset to origin
;center of mass - top left corner of sprite
(defrecord Tank
  [angle pos oto sprites
   rotate-rate move-rate
   move-energy fire-energy
   max-move-energy max-fire-energy
   charge charge-rate
   weapons current-weapon state]
  
  Entity
  (draw-meta [tank g]
	     (let [target (tank-target-pos tank)
		   max-fire-range (range-for-energy (get-default-weapon tank)
						    (:fire-energy tank))]
	       (doto g
		 (.setColor (. Color red))
		 (draw-circle (position tank) max-fire-range 30)
		 (.setColor (. Color blue))
		 (draw-circle (position tank) (:move-energy tank) 30))
	       
	       (if (= (:state tank) :charging)
		 (let [scale (max 30 (* 0.25 (:charge tank)))
		       d1 (vmul (unitdir (/ Math/PI 4)) scale)
		       d2 (vmul (unitdir (neg (/ Math/PI 4))) scale)
		       l1a (vsub target d1)
		       l1b (vadd target d1)
		       l2a (vsub target d2)
		       l2b (vadd target d2)]
		   (doto g
		     (.setColor (. Color red))
		     (draw-line l1a l1b)
		     (draw-line l2a l2b)
		     (draw-circle target scale 30))))))
    
  (draw [tank g]
	(let [frame (angle-to-frame (:angle tank) (num-frames tank))
	      tgt (vint (vsub (position tank) (:oto tank)))]
	  (draw-img g (nth (:sprites tank) frame) tgt)))

  (position [tank] (:pos tank)))

(defn reset-tank-energy [tank]
  (assoc tank
    :move-energy (:max-move-energy tank)
    :fire-energy (:max-fire-energy tank)))
  
(defn frame-angle-tolerance [tank]
  (rad-per-frame (num-frames tank)))

(defn make-tank [frames doto angle pos]
  (let [width (.getWidth (first frames))
	height (.getHeight (first frames))
	midsprite (vector (/ width 2) (/ height 2))
	oto (vadd midsprite doto)
	rotate-rate (* Math/PI 0.25)
	move-rate 10
	move-energy 100
	fire-energy 200
	fire-charge 0
	charge-rate 130]

    (Tank. angle pos oto frames
	    rotate-rate move-rate
	    move-energy fire-energy
	    move-energy fire-energy
	    fire-charge charge-rate
	    nil nil :idle)))

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

(defn move-towards-cursor [tank mouse dt-secs]
  (let [to-mouse (vsub (:pos mouse) (position tank))
	tank-dir (unitdir
		  (discretize-angle (:angle tank)
				    (num-frames tank)))
	dxscale (* (:move-rate tank) dt-secs)
	rotate-dir (dir-to-rotate tank-dir to-mouse)
	dtscale (* (:rotate-rate tank) dt-secs rotate-dir)
	dtheta (vang to-mouse tank-dir)]

    ;; only move if we're not where the mouse is
    (if (> (vmag to-mouse) 50)
      (if (within dtheta (frame-angle-tolerance tank))

	;; drive forward, we're as accurate in angle as we can be
	(-> tank
	    (assoc 
		:pos (vadd (position tank) (vmul tank-dir dxscale))
		:state :moving)
	    (subtract-move-energy dxscale)
	    (subtract-fire-energy (* 2 dxscale)))

	;; turn to face the cursor
	(-> tank
	    (assoc
		:angle (add-on-circle (:angle tank) dtscale)
		:state :moving)
	    (subtract-move-energy (* dtscale 0.1))))
      tank)))

