;; draw.clj
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

(ns MonthGame.draw
  (:use MonthGame.vector)
  (:import (java.awt Graphics2D)
	   (java.awt.image BufferedImage)))

(defn draw-line [#^Graphics2D g start end]
  (let [[ox oy] start
	[fx fy] end]
    (.drawLine g ox oy fx fy)
    (list fx fy)))

(defn fill-rect [#^Graphics2D g top-left bottom-right]
  (let [[x y] top-left
	[w h] (vsub bottom-right top-left)]
    (.fillRect g x y w h)))

(defn draw-leader [#^Graphics2D g origin length angle]
  (let [end (vadd origin {:angle angle :mag length})]
    (doto g
      (draw-line origin end))))

(defn draw-circle [#^Graphics2D g origin dia elems]
  (let [zp (vadd origin {:angle 0 :mag dia})]
    (dorun 
     (reduce #(draw-line g %1 (vadd origin {:angle %2 :mag dia}))
	     zp
	     (map #(* Math/PI 2 (/ % (float elems))) (concat (range 1 elems) (list 0)))))))
     
(defn draw-img [#^Graphics2D g #^BufferedImage img pos]
  (let [[x y] (vint pos)]
    (.drawImage g img x y nil)))

(defn draw-text-lines [#^Graphics2D g x y & lines]
  (let [height (-> g (.getFontMetrics) (.getHeight))
	y0 (+ y height)]
    (dorun
     (for [lineno (range (count lines))]
       (let [ly (+ y0 (* lineno height))
	     line (nth lines lineno)]
	 (.drawString g	line x ly))))))

