(ns MonthGame.sprite
  (:use MonthGame.vector))

(import '(java.awt Color Graphics Dimension)
	'(java.awt.image BufferedImage)
	'(java.io File)
	'(javax.imageio ImageIO))

(defn get-resource [file]
  (let [loader (clojure.lang.RT/baseLoader)]
    (.getResourceAsStream loader file)))

(defn load-img [stream]
  (ImageIO/read stream))

(defn extract-sprite [img width new-width n]
  (let [height (.getHeight img)
	new-height (* (/ new-width width) height)
	simg (new BufferedImage new-width new-height
		  (. BufferedImage TYPE_INT_ARGB))
	bg (.getGraphics simg)
	sx1 (* width n)
	sx2 (+ sx1 width)]
    (doto bg
      (.drawImage img
		0 0 new-width new-height
		sx1 0 sx2 height nil)
      (.dispose))
    simg))

(defn frame-width [src count]
  (let [width (.getWidth src)]
    (/ width count)))

(defn load-sprites [stream tgtsz]
     (let [img (load-img stream)
	   height (.getHeight img)
	   width (.getWidth img)
	   frames (/ width height)
	   sprite-width height]
       (map
	(fn [idx]
	  (extract-sprite img sprite-width tgtsz idx))
	(range frames))))

(defn rad-per-frame [total]
  (/ (* Math/PI 2) total))

(defn frame-to-angle [n total]
  (* (rad-per-frame total) n))

(defn angle-to-frame [angle total]
  (let [frame (int (Math/round (/ angle (rad-per-frame total))))]
    (cond
     (>= frame total) (- frame total)
     (< frame 0) (+ frame total)
     true frame)))

(defn discretize-angle [angle total]
  (frame-to-angle (angle-to-frame angle total) total))

(defn unitdir-for-frame [n total]
  (unitdir (frame-to-angle n total)))
