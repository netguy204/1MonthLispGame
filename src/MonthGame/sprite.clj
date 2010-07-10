(ns MonthGame.sprite
  (:use MonthGame.vector
	MonthGame.draw))

(import '(java.awt Color Graphics Dimension)
	'(java.awt.image BufferedImage)
	'(java.io File)
	'(javax.imageio ImageIO))

(defprotocol Sprite
  (draw-sprite [sprt g pos] "draw a sprite to the screen"))

(defn get-resource [file]
  (let [loader (clojure.lang.RT/baseLoader)]
    (.getResourceAsStream loader file)))

(defn load-img [stream]
  (ImageIO/read stream))

(defn middle-img [img]
  (let [width (.getWidth img)
	height (.getHeight img)]
    (list (/ width 2) (/ height 2))))

(defn make-img [width height]
  (new BufferedImage width height (. BufferedImage TYPE_INT_ARGB)))

(defn scale-img [img new-width new-height]
  (let [width (.getWidth img)
	height (.getHeight img)
	new-img (make-img new-width new-height)
	bg (.getGraphics new-img)]
    (doto bg
      (.drawImage img
		  0 0 new-width new-height
		  0 0 width height nil)
      (.dispose))
    new-img))

(defn extract-sprite [img width new-width n]
  (let [height (.getHeight img)
	new-height (* (/ new-width width) height)
	simg (make-img new-width new-height)
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

(defrecord OrientedSprite
  [frames dir doto]

  Sprite
  (draw-sprite
   [sprt g pos]
   (let [angle (vang dir)
	 frameno (angle-to-frame angle (count frames))
	 frame (nth frames frameno)
	 off (vadd (middle-img frame) doto)
	 tgt (vint (vsub pos off))]
     (draw-img g frame tgt))))

(defn make-oriented-sprite
  ([frames dir]
     (make-oriented-sprite frames dir '(0 0)))

  ([frames dir doto]
  (OrientedSprite. frames dir doto)))

(defrecord ElevatedSprite
  [main shadow height]

  Sprite
  (draw-sprite
   [sprt g pos]
   (let [proj-loc (vadd pos (list 0 (neg height)))
	 off (middle-img main)
	 shad-loc-off (vsub pos off)
	 proj-loc-off (vsub proj-loc off)]
     (draw-img g shadow (vint shad-loc-off))
     (draw-img g main (vint proj-loc-off)))))

(defn make-elevated-sprite [main shadow height]
  (ElevatedSprite. main shadow height))
