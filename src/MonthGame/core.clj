(ns MonthGame.core
  (:use MonthGame.vector
	MonthGame.sprite
	MonthGame.draw
	MonthGame.tank
	MonthGame.scalar-math
	MonthGame.mouse)
  (:gen-class))

(import '(javax.swing JFrame JPanel JButton)
	'(java.awt Color Graphics Dimension BorderLayout)
	'(java.awt.image BufferedImage)
	'(java.awt.event ActionListener))

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
  (update [npe world dt-secs] "update the npes position / state")
  (draw [npe g] "draw the npe")
  (position [npe] "position of npe center-of-mass"))

(defn not-nil? [obj]
  (not (nil? obj)))

(defn update-npes [world dt-secs]
  (alter world assoc
	 :npes (filter not-nil? (map #(update % @world dt-secs) (:npes @world)))))

(defn draw-npes [g world]
  (dorun
      (for [npe (:npes world)]
	(draw npe g))))

(defrecord Bullet
  [start end speed pos]

  NPE
  (update [npe world dt-secs]
	  (let [to-end (vsub end start)
		dist-to-end (vmag to-end)
		dir (unit-vector to-end)
		scale (* speed dt-secs)
		newpos (vadd pos (vmul dir scale))]
	    (if (is-past? end pos dir)
	      nil
	      (Bullet. start end speed newpos))))

  (draw [npe g]
	(draw-circle g pos 10 10))

  (position [npe] pos))

(defn make-bullet [start end]
  (Bullet. start end (* 0.5 (vdist start end)) start))

(defn get-current-tank [world]
  (nth (:tanks world) (:current-tank world)))

(defn make-next-tank-current [world]
  (let [ntanks (count (:tanks world))
	next (incr-cyclic (:current-tank world) ntanks)]
    (assoc world :current-tank next)))

(defn draw-hud [g world]
  (let [current (get-current-tank world)
	player (+ 1 (:current-tank world))
	move (float (:move-energy @current))
	fire (float (:fire-energy @current))]
    (draw-text-lines g 0 0
		     (format "Current player:  %d" player)
		     (format "Move energy:     %.1f" move)
		     (format "Fire energy:       %.1f" fire))))

  
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

     ;; draw npes
     (draw-npes bg @world)

     ;; draw hud
     (doto bg
       (.setColor (. Color black))
       (draw-hud @world)))

    (.dispose bg)
    (.drawImage g img 0 0 nil)))

(defn charge-for-fire [tank dt-secs]
   (let [was-charging (= (:state tank) :charging)
	 tank (assoc tank :state :charging)]
     (if was-charging
       (assoc tank :charge (+ (:charge tank) (* dt-secs (:charge-rate tank))))
       (assoc tank :charge 0))))

(defn build-default-weapon-npe [tank target]
  (make-bullet (:pos tank) target))

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

       ;; update npes
       (update-npes world dt-secs)

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
