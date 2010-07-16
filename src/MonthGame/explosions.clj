(ns MonthGame.explosions
  (:use (MonthGame vector draw sprite
		   entity sound util
		   graphics)))

(def *explode-frames*
     (let [img-stream (get-resource "MonthGame/explode.png")]
       (scale-img (load-sprites img-stream) 64 64)))

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
	      offset (middle-img *explode-frames*)
	      pos (vint (vsub (position npe) offset))]
	  (draw-img g current-frame pos)))
  
  (draw-meta [npe g] nil)

  (position [npe]
	    (let [time-elapsed (or (:time-elapsed npe) 0)
		  drift-speed (/ (float drift-dist) duration)]
	      (vint (vadd start {:angle dir :mag (* time-elapsed drift-speed)}))))

  (radius [npe] (/ (img-width *explode-frames*) 2))

  (collided-with [npe other] npe))
	    
(def *explosion-sound*
     (read-audio-frames (get-resource "MonthGame/explosion1.mp3")))

(defn make-explosion [pos dir]
  (println "making explosion")
  (play-async *explosion-sound*)
  (Explosion. pos dir 60 1))

(defn make-radial-explosions [pos num]
  (play-async *explosion-sound*)
  (let [skip-size (/ (* Math/PI 2) num)]
    (map (fn [idx] (Explosion. pos (* idx skip-size) 60 1)) (range num))))
