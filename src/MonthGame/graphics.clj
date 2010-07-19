;; graphics.clj
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

(ns MonthGame.graphics
  (:use (MonthGame vector util))
  (:import (javax.imageio ImageIO)
	   (java.awt.image BufferedImage)
	   (java.awt Graphics2D)))

(defmulti scale-img
  "scale an image like thing"
  (fn [img width height] (tag-or-class img)))

(defmulti img-size
  "size of an image like thing"
  tag-or-class)

(defn img-width [img] (first (img-size img)))
(defn img-height [img] (second (img-size img)))

(defn load-img [stream]
  #^BufferedImage (ImageIO/read stream))

(defmacro with-new-graphics [#^Graphics2D graphics & forms]
  `(let [#^Graphics2D ~graphics (.create ~graphics)
	 result# (do ~@forms)]
     (.dispose ~graphics)
     result#))

(defmacro with-offset-g [[#^Graphics2D graphics offset] & forms]
  `(with-new-graphics ~graphics
    (let [[#^int x# #^int y#] (vint ~offset)]
       (.translate ~graphics x# y#)
       ~@forms)))
    
(defn make-img [width height]
  (new BufferedImage width height (. BufferedImage TYPE_INT_ARGB)))

(defn middle-img [img]
  (let [[width height] (img-size img)]
    (list (/ width 2) (/ height 2))))

(defmethod scale-img BufferedImage
  [#^BufferedImage img new-width new-height]

  (let [width (img-width img)
	height (img-height img)
	new-img (make-img new-width new-height)
	#^Graphics2D bg (.getGraphics new-img)]
    (doto bg
      (.drawImage img
		  0 0 new-width new-height
		  0 0 width height nil)
      (.dispose))
    new-img))

(defmethod img-size BufferedImage
  [#^BufferedImage img]
  (list (.getWidth img) (.getHeight img)))
