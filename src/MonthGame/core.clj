(ns MonthGame.core
  (:use MonthGame.vector
	MonthGame.sprite
	MonthGame.draw)
  (:gen-class))

(import '(javax.swing JFrame JPanel)
	'(java.awt Color Graphics Dimension)
	'(java.awt.image BufferedImage)
	'(javax.imageio ImageIO)
	'(java.awt.event MouseAdapter MouseEvent))

(def running true)

(defn incr-cyclic [n max]
  (mod (+ n 1) max))

;; agent to handle issuing the repaints
(def animator (agent nil))
(def animation-sleep-ms 50)

; oto = offset to origin center of mass - top left corner of sprite
(defstruct tank :angle :pos :oto :sprites :state)

(defn num-frames [tank]
  (count (:sprites tank)))

(defn make-tank [fname tgt-sz doto]
  (let [frames (load-sprites fname tgt-sz)
	width (.getWidth (first frames))
	height (.getHeight (first frames))
	midsprite (list (/ width 2) (/ height 2))
	oto (vadd midsprite doto)]

    (struct tank 0 midsprite oto frames :idle)))

(defn draw-tank [g tank]
  (let [frame (angle-to-frame (:angle tank) (num-frames tank))]
    (draw-img g (nth (:sprites tank) frame) (vsub (:pos tank) (:oto tank)))))

(def my-tank (ref (make-tank "tanksprite.png" 128 '(0 15))))

(defstruct mouse-state :pos :button1down :button2down :lastevent)

(def mouse (ref (struct mouse-state nil false false)))

(defn add-on-circle [angle incr]
     (let [new-angle (+ angle incr)
	   twopi (* Math/PI 2)]
       (cond (> new-angle twopi) (- new-angle twopi)
	     (< new-angle 0) (+ new-angle twopi)
	     true new-angle)))
	   
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
       (.setColor (. Color black))
       (draw-leader o 50 (:angle @my-tank))
       (draw-circle o 50 30)
       (draw-tank @my-tank))
     (if (:pos @mouse)
       (draw-circle bg (:pos @mouse) 30 30)))

    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(defn update-click [mouse ev down]
  (let [button (.getButton ev)
	buttonsym (cond (= button (. MouseEvent BUTTON1)) :button1down
			(= button (. MouseEvent BUTTON2)) :button2down)]
    (if buttonsym (alter mouse assoc buttonsym down))))
  
(defn update-mouse [ev type]
  (dosync
   (alter mouse assoc :lastevent ev)
   (cond
    (= type :exit) (alter mouse assoc :pos nil)
    (= type :enter) (alter mouse assoc :pos (point-to-vec (.getPoint ev)))
    (= type :motion) (alter mouse assoc :pos (point-to-vec (.getPoint ev)))
    (= type :pressed) (update-click mouse ev true)
    (= type :released) (update-click mouse ev false))))
   
(def mouse-adapter
     (proxy [MouseAdapter] []
       (mouseClicked [e] (println "clicked"))
       (mouseDragged [e] (println "dragged"))
       (mouseEntered [e] (update-mouse e :enter))
       (mouseExited [e] (update-mouse e :exit))
       (mouseMoved [e] (update-mouse e :motion))
       (mousePressed [e] (update-mouse e :pressed))
       (mouseReleased [e] (update-mouse e :released))
       (mouseWheelMoved [mwe] (println "wheeled"))))

(def panel (proxy [JPanel] []
	     (paint [g] (my-draw g this))))
(doto panel
  (.addMouseListener mouse-adapter)
  (.addMouseMotionListener mouse-adapter)
  (.addMouseWheelListener mouse-adapter))

(def main-window (doto (JFrame. "Month Game")
		   (.setSize 400 400)
		   (.add panel)
		   (.setVisible true)))

(defn animate-tank [tank mouse] nil)
  
(defn animation [x]
     (when running
       (send-off *agent* #'animation))
     (.repaint panel)
     (Thread/sleep animation-sleep-ms)
     (dosync 
      (alter my-tank assoc :angle (add-on-circle (:angle @my-tank) 0.01))))


(send-off animator animation)
