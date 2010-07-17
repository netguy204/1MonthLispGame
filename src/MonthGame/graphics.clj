(ns MonthGame.graphics
  (:use (MonthGame vector util))
  (:import (javax.imageio ImageIO)
	   (java.awt.image BufferedImage)))

(defmulti scale-img
  "scale an image like thing"
  (fn [img width height] (tag-or-class img)))

(defmulti img-size
  "size of an image like thing"
  tag-or-class)

(defn img-width [img] (first (img-size img)))
(defn img-height [img] (second (img-size img)))

(defn load-img [stream]
  (println "loading image from " stream)
  (ImageIO/read stream))

(defmacro with-new-graphics [graphics & forms]
  `(let [~graphics (.create ~graphics)
	 result# (do ~@forms)]
     (.dispose ~graphics)
     result#))

(defmacro with-offset-g [[graphics offset] & forms]
  `(let [[x# y#] (vfloat ~offset)]
     (with-new-graphics ~graphics
       (.translate ~graphics x# y#)
       ~@forms)))
    
(defn make-img [width height]
  (new BufferedImage width height (. BufferedImage TYPE_INT_ARGB)))

(defn middle-img [img]
  (let [width (img-width img)
	height (img-height img)]
    (list (/ width 2) (/ height 2))))

(defmethod scale-img BufferedImage
  [img new-width new-height]

  (let [width (img-width img)
	height (img-height img)
	new-img (make-img new-width new-height)
	bg (.getGraphics new-img)]
    (doto bg
      (.drawImage img
		  0 0 new-width new-height
		  0 0 width height nil)
      (.dispose))
    new-img))

(defmethod img-size BufferedImage
  [img]
  (list (.getWidth img) (.getHeight img)))
