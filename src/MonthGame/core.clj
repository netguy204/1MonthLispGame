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
(defstruct tank-struct
  :angle :pos :oto :sprites
  :rotate-rate :move-rate
  :move-energy :fire-energy :state)

(defstruct world-struct
  :tanks :current-tank)

(defn get-current-tank [world]
  (nth (:tanks world) (:current-tank world)))

(defmacro with-each-tank [world var & forms]
  `(dorun (for [~var (:tanks ~world)]
	   ~@forms)))

(defn num-frames [tank]
  (count (:sprites tank)))

(defn frame-angle-tolerance [tank]
  (rad-per-frame (num-frames tank)))

(defn make-tank [frames doto angle pos]
  (let [width (.getWidth (first frames))
	height (.getHeight (first frames))
	midsprite (list (/ width 2) (/ height 2))
	oto (vadd midsprite doto)
	rotate-rate 0.01
	move-rate 0.7
	move-energy 100
	fire-energy 200]

    (struct tank-struct 
	    angle pos oto frames
	    rotate-rate move-rate
	    move-energy fire-energy :idle)))

(defn draw-tank [g tank]
  (let [frame (angle-to-frame (:angle tank) (num-frames tank))
	tgt (vint (vsub (:pos tank) (:oto tank)))]
    (doto g
      (.setColor (. Color white))
      (draw-leader (:pos tank) 50 0)
      (.setColor (. Color black))
      (draw-leader (:pos tank) 50 (:angle tank))
      (draw-circle (:pos tank) 50 30)
      (.setColor (. Color red))
      (draw-circle (:pos tank) (:fire-energy tank) 30)
      (.setColor (. Color blue))
      (draw-circle (:pos tank) (:move-energy tank) 30)
      (draw-img (nth (:sprites tank) frame) tgt))))

(defstruct mouse-struct :pos :button1down :button2down :lastevent)

(def mouse (ref (struct mouse-struct nil false false)))

(defn add-on-circle [angle incr]
     (let [new-angle (+ angle incr)
	   twopi (* Math/PI 2)]
       (cond (> new-angle twopi) (- new-angle twopi)
	     (< new-angle 0) (+ new-angle twopi)
	     true new-angle)))
	   
(defn my-draw [g this world]
  (let [width (.getWidth this)
	height (.getHeight this)
	img (new BufferedImage width height
		 (. BufferedImage TYPE_INT_ARGB))
	bg (.getGraphics img)
	o (list 64 64)]
    (dosync
     ;; clear backgroud
     (doto bg
       (.setColor (. Color green))
       (.fillRect 0 0 width height)
       (.setColor (. Color black)))

     ;; draw pointer
     (if (:pos @mouse)
       (draw-circle bg (:pos @mouse) 30 30))

     ;; draw tanks
     (with-each-tank @world tank
       (draw-tank bg @tank)))

    (.dispose bg)
    (.drawImage g img 0 0 nil)))


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

(defn subtract-energy [tank energy-type factor]
  (assoc tank energy-type (- (energy-type tank) (* factor (:move-rate tank)))))

(defn subtract-move-energy [tank factor]
  (subtract-energy tank :move-energy factor))

(defn subtract-fire-energy [tank factor]
  (subtract-energy tank :fire-energy factor))

(defn move-towards-cursor [tank mouse]
  (let [to-mouse (vsub (:pos mouse) (:pos tank))
	tank-dir (unitdir (discretize-angle (:angle tank) (num-frames tank)))
	dtheta (vang to-mouse tank-dir)
	rotate-dir (dir-to-rotate tank-dir to-mouse)]
    (if (> (vmag to-mouse) 50)
      (if (within dtheta (frame-angle-tolerance tank))
	(-> tank
	    (assoc :pos (vadd (:pos tank) (vmul tank-dir (:move-rate tank))))
	    (subtract-move-energy 1)
	    (subtract-fire-energy 2))
	(-> tank
	    (assoc :angle
	      (add-on-circle (:angle tank)
			     (* (:rotate-rate tank) rotate-dir)))
	    (subtract-move-energy 0.1)))
      tank)))
	
(defn animate-tank [tank mouse]
  (if (:button1down mouse)
    (move-towards-cursor tank mouse)
    tank))
  
(defn animation [x panel world]
  (dosync
   (alter (get-current-tank @world) animate-tank @mouse))
  (.repaint panel)
  (Thread/sleep animation-sleep-ms)
  (send-off *agent* #'animation panel world))

(defn get-resource [file]
  (let [loader (clojure.lang.RT/baseLoader)]
    (.getResourceAsStream loader file)))

(def my-world (ref (struct world-struct)))

(defn -main [& args]
  (let [img-stream (get-resource "MonthGame/tanksprite.png")
	frames (load-sprites img-stream 128)
	tank1 (ref (make-tank frames '(0 15) (/ Math/PI 4) '(64 64)))
	tank2 (ref (make-tank frames '(0 15) (/ (* 2 Math/PI) 3) '(300 300)))
	panel (proxy [JPanel] []
		(paint [g] (my-draw g this my-world)))
	main-window (JFrame. "Month Game")]

    (dosync
     (alter my-world assoc :tanks (list tank1 tank2) :current-tank 0))

    (doto panel
      (.addMouseListener mouse-adapter)
      (.addMouseMotionListener mouse-adapter)
      (.addMouseWheelListener mouse-adapter))

    (doto main-window
      (.setSize 400 400)
      (.add panel)
      ;(.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
      (.setVisible true))

    (send-off animator animation panel my-world)))
