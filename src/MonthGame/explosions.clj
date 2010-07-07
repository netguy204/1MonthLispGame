(ns MonthGame.explosions
  (:use MonthGame.vector
	MonthGame.draw
	MonthGame.sprite
	MonthGame.entity
	MonthGame.sound))

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
	      (list (assoc npe :time-elapsed time-elapsed)))))

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
	    
(def *explosion-sound*
     (read-frames (get-resource "MonthGame/explosion1.mp3")))

(defn make-explosion [pos dir]
  (println "making explosion")
  (play-async *explosion-sound*)
  (Explosion. pos dir 60 1))

(defn make-radial-explosions [pos num]
  (play-async *explosion-sound*)
  (let [skip-size (/ (* Math/PI 2) num)
	dirs (map (fn [ii] (unitdir (* ii skip-size))) (range num))]
    (map (fn [dir] (Explosion. pos dir 60 1)) dirs)))
