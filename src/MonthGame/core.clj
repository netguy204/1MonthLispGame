(ns MonthGame.core)

(import '(javax.swing JFrame JPanel)
	'(java.awt Color Graphics Dimension)
	'(java.awt.image BufferedImage)
	'(javax.imageio ImageIO)
	'(java.io File)
	'(java.awt.event MouseAdapter))

(def running true)

(defn load-img [fname]
  (ImageIO/read (new File fname)))

(defn extract-sprite [img width new-width n]
  (let [height (.getHeight img)
	new-height (* (/ new-width width) height)
	simg (new BufferedImage new-width new-height
		  (. BufferedImage TYPE_INT_ARGB))
	bg (.getGraphics simg)
	sx1 (* width n)
	sx2 (+ sx1 width)]
    (.drawImage bg img
		0 0 new-width new-height
		sx1 0 sx2 height nil)
    (.dispose bg)
    (println simg)
    simg))

(defn frame-width [src count]
  (let [width (.getWidth src)]
    (/ width count)))

(defn load-sprites [fname tgtsz]
     (let [img (load-img fname)
	   height (.getHeight img)
	   width (.getWidth img)
	   frames (/ width height)
	   sprite-width height]
       (map
	(fn [idx]
	  (extract-sprite img sprite-width tgtsz idx))
	(range frames))))

(defn incr-cyclic [n max]
  (mod (+ n 1) max))

(defn vmag [v]
  (let [[x y] v]
    (Math/sqrt (+ (* x x) (* y y 4)))))

(defn vmul [v s]
  (let [[x y] v]
    (list (* x s) (* y s))))

(defn vadd [v1 v2]
  (let [[x1 y1] v1
	[x2 y2] v2]
    (list (+ x1 x2) (+ y1 y2))))

(defn vneg [v]
  (let [[x y] v]
    (list (- 0 x) (- 0 y))))

(defn vsub [v1 v2]
  (vadd v1 (vneg v2)))

(defn unit-vector [v]
  (let [m (vmag v)]
    (vmul v (/ 1.0 m))))


(defn unitdir [theta]
  (let [s (Math/sin theta)
	c (Math/cos theta)]
  (unit-vector (list (* c 2) s))))

(defn rad-per-frame [total]
  (/ (* Math/PI 2) total))

(defn frame-to-angle [n total]
  (* (rad-per-frame total) n))

(defn angle-to-frame [angle total]
  (let [frame (int (Math/round (/ angle (rad-per-frame total))))]
    (if (>= frame total) (- frame total)
	frame)))

(defn discretize-angle [angle total]
  (frame-to-angle (angle-to-frame angle total) total))

(defn unitdir-for-frame [n total]
  (unitdir (frame-to-angle n total)))

(defn draw-line [g start end]
  (let [[ox oy] start
	[fx fy] end]
    (.drawLine g ox oy fx fy)
    (list fx fy)))

(defn draw-leader [g origin length angle]
  (let [dir (unitdir angle)
	end (vadd origin (vmul dir length))]
    (doto g
      (.setColor (. Color black))
      (draw-line origin end))))

(defn draw-circle [g origin dia elems]
  (let [zp (vadd origin (vmul (unitdir 0) dia))]
    (dorun 
     (reduce #(draw-line g %1 (vadd origin (vmul (unitdir %2) dia)))
	     zp
	     (map #(* Math/PI 2 (/ % (float elems))) (concat (range 1 elems) (list 0)))))))
     
;; agent to handle issuing the repaints
(def animator (agent nil))
(def animation-sleep-ms 50)

; oto = offset to origin center of mass - top left corner of sprite
(defstruct tank :angle :pos :oto :sprites)

(defn num-frames [tank]
  (count (:sprites tank)))

(defn make-tank [fname tgt-sz doto]
  (let [frames (load-sprites fname tgt-sz)
	width (.getWidth (first frames))
	height (.getHeight (first frames))
	midsprite (list (/ width 2) (/ height 2))
	oto (vadd midsprite doto)]

    (struct tank 0 midsprite oto frames)))

(defn draw-img [g img pos]
  (let [[x y] pos]
    (.drawImage g img x y nil)))

(defn draw-tank [g tank]
  (let [frame (angle-to-frame (:angle tank) (num-frames tank))]
    (draw-img g (nth (:sprites tank) frame) (vsub (:pos tank) (:oto tank)))))

(def my-tank (ref (make-tank "tanksprite.png" 128 '(0 15))))

(defn add-on-circle [angle incr]
     (let [new-angle (+ angle incr)
	   twopi (* Math/PI 2)]
       (cond (> new-angle twopi) (- new-angle twopi)
	     (< new-angle 0) (+ new-angle twopi)
	     true new-angle)))
	   
;; load the tank image
(def sprites (load-sprites "tanksprite.png" 128))

(defn my-draw [g this]
  (let [width (.getWidth this)
	height (.getHeight this)
	img (new BufferedImage width height
		 (. BufferedImage TYPE_INT_ARGB))
	bg (.getGraphics img)
	o (list 64 64)]
    (dosync
     (doto bg
       (.setColor (. Color green))
       (.fillRect 0 0 width height)
       (draw-leader o 50 (:angle @my-tank))
       (draw-circle o 50 30)
       (draw-tank @my-tank)))

    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(def mouse-adapter
     (proxy [MouseAdapter] []
       (mouseClicked [e] (println "clicked"))
       (mouseDragged [e] (println "dragged"))
       (mouseEntered [e] (println "entered"))
       (mouseExited [e] (println "exited"))
       (mouseMoved [e] (println "moved"))
       (mousePressed [e] (println "pressed"))
       (mouseReleased [e] (println "released"))
       (mouseWheelMoved [mwe] (println "wheeled"))))

(def panel (proxy [JPanel] []
	     (paint [g] (my-draw g this))))

(def main-window (doto (JFrame. "Month Game")
		   (.setSize 400 400)
		   (.add panel)
		   (.setVisible true)))


(defn animation [x]
     (when running
       (send-off *agent* #'animation))
     (.repaint panel)
     (Thread/sleep animation-sleep-ms)
     (dosync 
      (alter my-tank assoc :angle (add-on-circle (:angle @my-tank) 0.01))))


(send-off animator animation)
