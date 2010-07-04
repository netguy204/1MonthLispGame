(ns MonthGame.explosions
  (:use MonthGame.vector
	MonthGame.draw
	MonthGame.sprite
	MonthGame.entity))

(def *explode-frames*
     (let [img-stream (get-resource "MonthGame/explode.png")]
       (load-sprites img-stream 64)))

(defrecord Explosion
  [start dir drift-dist duration]

  NPE
  (update [npe world dt-secs]
	  (let [last-time-elapsed (or (:time-elapsed npe) 0)
		time-elapsed (+ last-time-elapsed dt-secs)]
	    (if (> time-elapsed duration)
	      nil
	      (assoc npe :time-elapsed time-elapsed))))

  Entity
  (draw [npe g]
	(let [time-elapsed (or (:time-elapsed npe) 0)
	      num-frames (count *explode-frames*)
	      time-per-frame (/ (float duration) num-frames)
	      current-frame-no (int (Math/floor (/ time-elapsed time-per-frame)))
	      current-frame (nth *explode-frames* current-frame-no)
	      offset (list (/ (.getWidth current-frame) 2)
			   (/ (.getHeight current-frame) 2))
	      pos (vint (vsub (position npe) offset))]
	  (draw-img g current-frame pos)))
  
  (draw-meta [npe g] nil)

  (position [npe]
	    (let [time-elapsed (or (:time-elapsed npe) 0)
		  drift-speed (/ (float drift-dist) duration)]
	      (vint (vadd start (vmul dir (* time-elapsed drift-speed))))))

  (radius [npe] (/ (.getWidth (first *explode-frames*)) 2))

  (can-collide? [npe] false))
	    
(defn make-explosion [pos dir]
  (println "making explosion")
  (Explosion. pos dir 60 1))
