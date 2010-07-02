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
(defstruct tank-struct :angle :pos :oto :sprites :state)

(defn num-frames [tank]
  (count (:sprites tank)))

(defn frame-angle-tolerance [tank]
  (rad-per-frame (num-frames tank)))

(defn make-tank [stream tgt-sz doto]
  (let [frames (load-sprites stream tgt-sz)
	width (.getWidth (first frames))
	height (.getHeight (first frames))
	midsprite (list (/ width 2) (/ height 2))
	oto (vadd midsprite doto)]

    (struct tank-struct 0 midsprite oto frames :idle)))

(defn draw-tank [g tank]
  (let [frame (angle-to-frame (:angle tank) (num-frames tank))
	tgt (vint (vsub (:pos tank) (:oto tank)))]
    (doto g
      (.setColor (. Color white))
      (draw-leader (:pos tank) 50 0)
      (.setColor (. Color black))
      (draw-leader (:pos tank) 50 (:angle tank))
      (draw-circle (:pos tank) 50 30)
      (draw-img (nth (:sprites tank) frame) tgt))))

(defstruct mouse-struct :pos :button1down :button2down :lastevent)

(def mouse (ref (struct mouse-struct nil false false)))

(defn add-on-circle [angle incr]
     (let [new-angle (+ angle incr)
	   twopi (* Math/PI 2)]
       (cond (> new-angle twopi) (- new-angle twopi)
	     (< new-angle 0) (+ new-angle twopi)
	     true new-angle)))
	   
(defn my-draw [g this tank]
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
       (draw-tank @tank))
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

(defn within [v range]
  (< (Math/abs v) range))

(defn dir-to-rotate [vfrom vto]
  (let [afrom (vang vfrom)
	ato (vang vto)
	delta (- ato afrom)
	pi (Math/PI)]
    (cond
     (> delta 0) (if (< delta pi) 1 -1)
     (< delta 0) (if (< delta (neg pi)) 1 -1)
     true 0)))
     
(defn move-towards-cursor [tank mouse]
  (let [to-mouse (vsub (:pos mouse) (:pos tank))
	tank-dir (unitdir (discretize-angle (:angle tank) (num-frames tank)))
	dtheta (vang to-mouse tank-dir)]
    (if (> (vmag to-mouse) 50)
      (if (within dtheta (frame-angle-tolerance tank))
	(assoc tank :pos (vadd (:pos tank) (vmul tank-dir 0.7)))
	(assoc tank :angle (add-on-circle (:angle tank)
					  (* 0.01 (dir-to-rotate tank-dir to-mouse)))))
      tank)))
	
(defn animate-tank [tank mouse]
  (if (:button1down mouse)
    (move-towards-cursor tank mouse)
    tank))
  
(defn animation [x panel tank]
  (dosync
   (alter tank animate-tank @mouse))
  (.repaint panel)
  (Thread/sleep animation-sleep-ms)
  (send-off *agent* #'animation panel tank))

(defn -main [& args]

  (let [loader (clojure.lang.RT/baseLoader)
	tank-img (.getResourceAsStream loader "MonthGame/tanksprite.png")
	my-tank (ref (make-tank tank-img 128 '(0 15)))
	
	panel (proxy [JPanel] []
		(paint [g] (my-draw g this my-tank)))
	main-window (JFrame. "Month Game")]
    (doto panel
      (.addMouseListener mouse-adapter)
      (.addMouseMotionListener mouse-adapter)
      (.addMouseWheelListener mouse-adapter))

    (doto main-window
      (.setSize 400 400)
      (.add panel)
      (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
      (.setVisible true))

    (send-off animator animation panel my-tank)))
