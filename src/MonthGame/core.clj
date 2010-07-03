(ns MonthGame.core
  (:use MonthGame.vector
	MonthGame.sprite
	MonthGame.draw
	MonthGame.tank
	MonthGame.scalar-math
	MonthGame.mouse
	MonthGame.missiles
	MonthGame.explosions
	MonthGame.npe
	MonthGame.weapons)
  (:gen-class))

(import '(javax.swing JFrame JPanel JButton
		      JComboBox ComboBoxModel JLabel
		      ListCellRenderer ImageIcon
		      SwingConstants)
	'(java.awt Color Graphics Dimension BorderLayout)
	'(java.awt.image BufferedImage)
	'(java.awt.event ActionListener)
	'(javax.swing.event ListDataEvent))

(defstruct world-struct
  :tanks :current-tank :npes)

(defn get-current-tank [world]
  (nth (:tanks world) (:current-tank world)))

(def *my-world* (ref (struct world-struct)))
(def *animator* (agent nil))
(def *last-render* (ref (System/currentTimeMillis)))
(def *animation-sleep-ms* 50)
(def *weapon-selector* (new JComboBox))
(def *weapon-list-listeners* (ref []))

(defn num-weapons [tank]
  (count (:weapons tank)))

(defn notify-weapon-listeners []
  (dosync
   (let [ev (ListDataEvent. *my-world* (. ListDataEvent CONTENTS_CHANGED) 0 
			    (num-weapons @(get-current-tank @*my-world*)))]
     (dorun
      (for [listener @*weapon-list-listeners*]
	(.contentsChanged listener ev))))))

(def *weapon-list-model*
     (proxy [ComboBoxModel] []
       (getSelectedItem 
	[] 
	(dosync
	 (:current-weapon @(get-current-tank @*my-world*))))

       (setSelectedItem
	[item] 
	(dosync
	 (alter (get-current-tank @*my-world*)
		assoc :current-weapon (int item))))

       (addListDataListener
	[l]
	(dosync (alter *weapon-list-listeners*
		       concat [l])))

       (removeListDataListener 
	[l]
	(dosync (ref-set *weapon-list-listeners*
			 (filter #(not= % l)
				 *weapon-list-listeners*))))

       (getElementAt [idx] idx)

       (getSize [] 
		(dosync (num-weapons @(get-current-tank @*my-world*))))))

(def *weapon-list-renderer*
     (proxy [ListCellRenderer] []
       (getListCellRendererComponent 
	[l o idx selected focused]
	(dosync
	 (let [wpn (nth (:weapons @(get-current-tank @*my-world*)) o)
	       icn (new ImageIcon (icon wpn))
	       lbl (new JLabel (str (shots wpn)) icn (. SwingConstants LEFT))]
	   (.setOpaque lbl true)
	   (if selected
	     (doto lbl
	       (.setBackground (.getSelectionBackground l))
	       (.setForeground (.getSelectionForeground l)))
	     (doto lbl
	       (.setBackground (.getBackground l))
	       (.setForeground (.getForeground l)))))))))

(defn update-render-time []
  (dosync
   (let [old @*last-render*
	 new (ref-set *last-render* (System/currentTimeMillis))]
     (- new old))))

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


(defn slope-of-vec [v]
  (let [[x y] v]
    (/ y x)))

;(defn line-endpoint [pos dir lower-right]
;  (let [[mx my] lower-right
;	[dx dy] (vsub lower-right pos)
;	[px py] pos
;	slope (slope-of-vec dir)
;	fx (]
    
(defn draw-background [g width height]
  (.setColor g (. Color blue))
  (dorun
   (for [ii (range 30)]
     (.drawLine g 0 (* ii 50) width (* ii 50)))))
  
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
       (.setColor (. Color black))
       (draw-background width height))

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
	 tank (assoc tank :state :charging)
	 max-range (range-for-energy (get-default-weapon tank) (:fire-energy tank))]
     (if was-charging
       (assoc tank :charge 
	      (min max-range (+ (:charge tank) (* dt-secs (:charge-rate tank)))))
       (assoc tank :charge 0))))

(defn fire-from-tank [tank world]
  (let [target (tank-target-pos @tank)
	pos (:pos @tank)
	weapon (get-default-weapon @tank)
	npe (fire weapon pos target)]
    (add-npe-to-world npe world)
    (alter tank subtract-fire-energy (energy-used weapon pos target))
    (alter tank assoc :state :idle)
    (notify-weapon-listeners)))
    
(defn animate-tank [tank mouse world dt-secs]
  (cond
   ;; charge when right mouse depressed
   (and (:button2down mouse)
	(can-fire? @tank)
	(> (shots (get-default-weapon @tank)) 0))
   (do
     (alter tank charge-for-fire dt-secs))
   
   ;; fire when released
   (and (= (:state @tank) :charging) (not (:button2down mouse)))
   (do
     (println "firing!")
     (fire-from-tank tank world))

   ;; move when left mouse depressed
   (and (:button1down mouse) (can-move? @tank))
   (do
     (alter tank move-towards-cursor mouse dt-secs))

   true (alter tank assoc :state :idle)))

(defn change-player [world]
  (alter (get-current-tank @world) reset-tank-energy)
  (alter world make-next-tank-current)
  (notify-weapon-listeners))
  
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
     (alter tank1 assoc
	    :weapons [(make-rocket-launcher 5)
		      (make-rocket-launcher 10)]
	    :current-weapon 0)
     (alter tank2 assoc
	    :weapons [(make-rocket-launcher 7)]
	    :current-weapon 0)
     (alter *my-world* assoc
	    :tanks (vector tank1 tank2)
	    :current-tank 0
	    :npes []))

    (.addActionListener end-turn-button button-clicked)

    (doto panel
      (.addMouseListener mouse-adapter)
      (.addMouseMotionListener mouse-adapter)
      (.addMouseWheelListener mouse-adapter))
    
    ;; set up the weapon selector
    (.setModel *weapon-selector* *weapon-list-model*)
    (.setRenderer *weapon-selector* *weapon-list-renderer*)

    (doto main-window
      (.setSize 800 600)
      (.add panel (. BorderLayout CENTER))
      (.add *weapon-selector* (. BorderLayout NORTH))
      (.add end-turn-button (. BorderLayout SOUTH))
      (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
      (.setVisible true))

    (send-off *animator* animation panel *my-world*)))
