(ns MonthGame.missiles
  (:use MonthGame.vector
	MonthGame.draw
	MonthGame.sprite
	MonthGame.npe
	MonthGame.explosions))


(def *rocket-frames*
     (let [img-stream (get-resource "MonthGame/rocketsprite.png")]
       (load-sprites img-stream 64)))


;; a rocket is an unguided self-powered projectile
(defrecord Rocket
  [start end speed pos]
  
  NPE
  (update [npe world dt-secs]
	  (let [to-end (vsub end start)
		dist-to-end (vmag to-end)
		dir (unit-vector to-end)
		scale (* speed dt-secs)
		newpos (vadd pos (vmul dir scale))]
	    (if (is-past? end pos dir)
	      (make-explosion end dir)
	      (assoc npe :pos newpos))))
  
  (draw [npe g]
	(let [dir (unit-vector (vsub end start))
	      angle (vang dir)
	      frameno (angle-to-frame angle (count *rocket-frames*))
	      frame (nth *rocket-frames* frameno)
	      off (list (/ (.getWidth frame) 2) (/ (.getHeight frame) 2))
	      tgt (vint (vsub pos off))]
	  (draw-img g frame tgt)))

  (position [npe] pos))

(defn make-rocket [start end]
  (Rocket. start end (* 0.5 (vdist start end)) start))

