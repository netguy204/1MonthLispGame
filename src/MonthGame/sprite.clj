(ns MonthGame.sprite
  (:use (MonthGame scalar-math vector 
		   graphics draw util))
  (:import (java.awt Color Graphics Dimension)
	   (java.awt.image BufferedImage)
	   (java.io File)))

(defmulti draw-sprite
  "draw a sprite to context g"
  (fn [g sprite] (tag-or-class sprite)))

(defn extract-sprite-impl [img n]
  (let [height (.getHeight img)
	simg (make-img height height)
	bg (.getGraphics simg)
	sx1 (* height n)
	sx2 (+ sx1 height)]
    (doto bg
      (.drawImage img
		0 0 height height
		sx1 0 sx2 height nil)
      (.dispose))
    simg))

(def extract-sprite
     #^{:doc "extract the nth square frame from a 1d sprite sheet"}
     (memoize (fn [img n] (extract-sprite-impl img n))))

(defn read-frames [img]
  "read an entire 1d sprite strip into a list of BufferedImages"
  (let [height (.getHeight img)
	width (.getWidth img)
	numframes (/ width height)
	frames (doall
		(for [n (range numframes)]
		  (extract-sprite img n)))]
    (with-meta frames {:tag ::frames})))

(defmethod scale-img ::frames
  [img width height]
  #^::frames (doall
	      (for [frame img]
		(scale-img frame width height))))

(def load-sprites (comp read-frames load-img))

(defmethod draw-sprite BufferedImage
  [g sprite]
  (draw-img g sprite '(0 0)))

(defn make-oriented-sprite
  ([frames dir]
     (make-oriented-sprite frames dir '(0 0)))

  ([frames dir doto]
     #^::oriented-sprite {:frames frames
			  :angle (vang dir)
			  :doto (to-vector doto)}))

(defmethod draw-sprite ::oriented-sprite
  [g sprite]
  (let [angle (:angle sprite)
	frames (:frames sprite)
	frameno (angle-to-frame angle (count frames))
	frame (nth frames frameno)
	off (vneg (vadd (middle-img frame) (:doto sprite)))]
    (with-offset-g [g off]
      (draw-sprite g frame))))

(defn make-elevated-sprite [main shadow height]
  #^::elevated-sprite {:main main
			:shadow shadow
			:height height})

(defmethod draw-sprite ::elevated-sprite
  [g sprite]
  (let [proj-loc (list 0 (neg (:height sprite)))]
    (draw-sprite g (:shadow sprite))
    (with-offset-g [g proj-loc]
      (draw-sprite g (:main sprite)))))

