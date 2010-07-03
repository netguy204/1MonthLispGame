(ns MonthGame.core
  (:use MonthGame.vector
	MonthGame.sprite
	MonthGame.draw
	MonthGame.tank
	MonthGame.scalar-math)
  (:gen-class))

(import '(javax.swing JFrame JPanel JButton)
	'(java.awt Color Graphics Dimension BorderLayout)
	'(java.awt.image BufferedImage)
	'(javax.imageio ImageIO)
	'(java.awt.event MouseAdapter MouseEvent ActionListener))

(defstruct world-struct
  :tanks :current-tank :npes)

(def *my-world* (ref (struct world-struct)))
(def *animator* (agent nil))
(def *last-render* (ref (System/currentTimeMillis)))
(def *animation-sleep-ms* 50)

(defn update-render-time []
  (dosync
   (let [old @*last-render*
	 new (ref-set *last-render* (System/currentTimeMillis))]
     (- new old))))

(defprotocol NPE
  "An entity that's not under player control"
  (update [npe world] "update the npes position / state")
  (draw [npe g] "draw the npe"))

(defn get-current-tank [world]
  (nth (:tanks world) (:current-tank world)))

(defn make-next-tank-current [world]
  (let [ntanks (count (:tanks world))
	next (incr-cyclic (:current-tank world) ntanks)]
    (assoc world :current-tank next)))

(defn draw-text-lines [g x y & lines]
  (let [height (-> g (.getFontMetrics) (.getHeight))
	y0 (+ y height)]
    (dorun
     (for [lineno (range (count lines))]
       (let [ly (+ y0 (* lineno height))
	     line (nth lines lineno)]
	 (.drawString g	line x ly))))))

(defn draw-hud [g world]
  (let [current (get-current-tank world)
	player (+ 1 (:current-tank world))
	move (float (:move-energy @current))
	fire (float (:fire-energy @current))]
    (draw-text-lines g 0 0
		     (format "Current player:  %d" player)
		     (format "Move energy:     %.1f" move)
		     (format "Fire energy:       %.1f" fire))))

  
(defstruct mouse-struct :pos :button1down :button2down :lastevent)

(def *mouse* (ref (struct mouse-struct nil false false)))

(defn my-draw [g this world]
  (let [width (.getWidth this)
	height (.getHeight this)
	img (new BufferedImage width height
		 (. BufferedImage TYPE_INT_ARGB))
	bg (.getGraphics img)]

    (dosync
     ;; clear backgroud
     (doto bg
       (.setColor (. Color white))
       (.fillRect 0 0 width height)
       (.setColor (. Color black)))
     (.setColor bg (. Color blue))
     (dorun
	 (for [ii (range 30)]
	   (.drawLine bg 0 (* ii 50) width (* ii 50))))
     ;; draw pointer
     (if (:pos @*mouse*)
       (draw-circle bg (:pos @*mouse*) 30 30))

     ;; draw current tank power info
     (draw-tank-meta bg @(get-current-tank @world))

     ;; draw tanks
     (with-each-tank @world tank
       (draw-tank bg @tank))
     
     ;; draw hud
     (doto bg
       (.setColor (. Color black))
       (draw-hud @world)))

    (.dispose bg)
    (.drawImage g img 0 0 nil)))


(defn button-to-sym [button]
  (cond
   (= button (. MouseEvent BUTTON1)) :button1down
   (= button (. MouseEvent BUTTON3)) :button2down))
  
(defn update-click [mouse ev down]
  (let [button (.getButton ev)
	buttonsym (button-to-sym button)]
    (if buttonsym (alter mouse assoc buttonsym down))))
  
(defn update-mouse [ev type]
  (dosync
   (alter *mouse* assoc :lastevent ev)
   (cond
    (= type :exit) (alter *mouse* assoc :pos nil)
    (= type :enter) (alter *mouse* assoc
			   :pos (point-to-vec (.getPoint ev)))
    (= type :motion) (alter *mouse* assoc
			    :pos (point-to-vec (.getPoint ev)))
    (= type :pressed) (update-click *mouse* ev true)
    (= type :released) (update-click *mouse* ev false))))
   
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

(defn charge-for-fire [tank dt-secs]
   (let [was-charging (= (:state tank) :charging)
	 tank (assoc tank :state :charging)]
     (if was-charging
       (assoc tank :charge (+ (:charge tank) (* dt-secs (:charge-rate tank))))
       (assoc tank :charge 0))))

(defn make-bullet-npe [start end] nil)
  
(defn build-default-weapon-npe [tank target]
  (make-bullet-npe (:pos tank) target))

(defn fire-from-tank [tank world]
  (let [target (tank-target-pos tank)
	npe (build-default-weapon-npe tank target)]
    (alter world assoc :npes (cons npe (:npes @world)))))
    
(defn animate-tank [tank mouse world dt-secs]
  (cond
   ;; charge when right mouse depressed
   (and (:button2down mouse) (can-fire? @tank))
   (do
     (alter tank charge-for-fire dt-secs))
   
   ;; fire when released
   (and (= (:state @tank) :charging) (not (:button2down mouse)))
   (do
     (println "firing!")
     (fire-from-tank @tank world)
     (alter tank assoc :state :idle))

   ;; move when left mouse depressed
   (and (:button1down mouse) (can-move? @tank))
   (do
     (alter tank move-towards-cursor mouse dt-secs))

   true (alter tank assoc :state :idle)))

(defn change-player [world]
  (alter (get-current-tank @world) reset-tank-energy)
  (alter world make-next-tank-current))
  
(defn animation [x panel world]
  (let [dt (update-render-time)
	dt-secs (/ (float dt) 1000)]
    (dosync
     (let [tank (get-current-tank @world)]
       ;; switch players if current is out of energy
       ;; disabled: surprising if you're not expected it
       ;(if (tank-depleted? @tank)
       ;(change-player world))

       ;; animate the current player
       (animate-tank tank @*mouse* world dt-secs)))

    (.repaint panel)
    (Thread/sleep (max 0 (- *animation-sleep-ms* dt)))
    (send-off *agent* #'animation panel world)))

(defn -main [& args]
  (let [img-stream (get-resource "MonthGame/tanksprite.png")
	frames (load-sprites img-stream 128)
	tank1 (ref (make-tank frames '(0 15)
			      (/ Math/PI 4) '(64 64)))
	tank2 (ref (make-tank frames '(0 15)
			      (/ (* 2 Math/PI) 3) '(300 300)))
	panel (proxy [JPanel] []
		(paint [g] (my-draw g this *my-world*)))
	end-turn-button (new JButton "End Turn")
	button-clicked (proxy [ActionListener] []
			 (actionPerformed
			  [ev]
			  (dosync (change-player *my-world*))))
	main-window (JFrame. "Month Game")]

    ; add the players to the world
    (dosync
     (alter *my-world* assoc
	    :tanks (vector tank1 tank2)
	    :current-tank 0
	    :npes []))

    (.addActionListener end-turn-button button-clicked)

    (doto panel
      (.addMouseListener mouse-adapter)
      (.addMouseMotionListener mouse-adapter)
      (.addMouseWheelListener mouse-adapter))

    (doto main-window
      (.setSize 800 600)
      (.add panel (. BorderLayout CENTER))
      (.add end-turn-button (. BorderLayout SOUTH))
      ;(.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
      (.setVisible true))

    (send-off *animator* animation panel *my-world*)))
