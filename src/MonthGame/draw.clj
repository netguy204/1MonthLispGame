(ns MonthGame.draw
  (:use MonthGame.vector))

(defn draw-line [g start end]
  (let [[ox oy] start
	[fx fy] end]
    (.drawLine g ox oy fx fy)
    (list fx fy)))

(defn draw-leader [g origin length angle]
  (let [dir (unitdir angle)
	end (vadd origin (vmul dir length))]
    (doto g
      (draw-line origin end))))

(defn draw-circle [g origin dia elems]
  (let [zp (vadd origin (vmul (unitdir 0) dia))]
    (dorun 
     (reduce #(draw-line g %1 (vadd origin (vmul (unitdir %2) dia)))
	     zp
	     (map #(* Math/PI 2 (/ % (float elems))) (concat (range 1 elems) (list 0)))))))
     
(defn draw-img [g img pos]
  (let [[x y] pos]
    (.drawImage g img x y nil)))

(defn draw-text-lines [g x y & lines]
  (let [height (-> g (.getFontMetrics) (.getHeight))
	y0 (+ y height)]
    (dorun
     (for [lineno (range (count lines))]
       (let [ly (+ y0 (* lineno height))
	     line (nth lines lineno)]
	 (.drawString g	line x ly))))))

