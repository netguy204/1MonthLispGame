(ns MonthGame.core
  (:use MonthGame.vector
	MonthGame.sprite
	MonthGame.draw
	MonthGame.tank
	MonthGame.scalar-math
	MonthGame.mouse
	MonthGame.missiles
	MonthGame.explosions
	MonthGame.entity
	MonthGame.weapons
	MonthGame.sound)
  (:gen-class))

(import '(javax.swing JFrame JPanel JButton
		      JComboBox ComboBoxModel JLabel
		      ListCellRenderer ImageIcon
		      SwingConstants)
	'(java.awt Color Graphics Dimension 
		   BorderLayout GridBagLayout GridBagConstraints)
	'(java.awt.image BufferedImage)
	'(java.awt.event ActionListener)
	'(javax.swing.event ListDataEvent))

(defrecord World
  [tanks current-tank npes])

(defn- current-tank [world]
  (nth (:tanks world) (:current-tank world)))

(defn- add-tank [world tank]
  (assoc world :tanks (cons tank (:tanks world))))

(defn- world-mode [world]
  (or (:mode world) :init))

(defn- world-mode-meta [world default]
  (or (:mode-meta world) default))

(defn- change-world-mode [world mode meta]
  (assoc world :mode mode :mode-meta meta))

(def *my-world* (ref (World. [] nil [])))
(def *animator* (agent nil))
(def *last-render* (ref (System/currentTimeMillis)))
(def *animation-sleep-ms* 50)
(def *weapon-selector* (new JComboBox))
(def *weapon-list-listeners* (ref []))
(def *background-music* "MonthGame/background1.mp3")
(def *background-image*
     (load-img (get-resource "MonthGame/paper.png")))

(defn num-weapons [tank]
  (count (:weapons tank)))

(defn- make-lde [world]
  (ListDataEvent. world
		  (ListDataEvent/CONTENTS_CHANGED)
		  0 (num-weapons @(current-tank world))))

(defn- notify-weapon-listeners []
  (dosync
   (let [ev (make-lde @*my-world*)]
     (doseq [listener @*weapon-list-listeners*]
       (.contentsChanged listener ev)))))

(defn- current-weapon [world]
  (:current-weapon @(current-tank world)))

(defn- remove-from-list [lst item]
  (filter #(not= % item) lst))

(def *weapon-list-model*
     (proxy [ComboBoxModel] []
       (getSelectedItem [] (dosync (current-weapon @*my-world*)))

       (setSelectedItem
	[item] 
	(dosync
	 (alter (current-tank @*my-world*)
		assoc :current-weapon (int item))))

       (addListDataListener
	[l]
	(dosync (alter *weapon-list-listeners*
		       concat [l])))

       (removeListDataListener 
	[l]
	(dosync (alter *weapon-list-listeners* remove-from-list l)))

       (getElementAt [idx] idx)

       (getSize [] 
		(dosync (num-weapons @(current-tank @*my-world*))))))

(def *weapon-list-renderer*
     (proxy [ListCellRenderer] []
       (getListCellRendererComponent 
	[l o idx selected focused]
	(dosync
	 (let [wpn (nth (:weapons @(current-tank @*my-world*)) o)
	       icn (new ImageIcon (icon wpn))
	       lbl (new JLabel (str (shots wpn)) icn (. SwingConstants CENTER))]
	   (.setOpaque lbl true)
	   (if selected
	     (doto lbl
	       (.setBackground (.getSelectionBackground l))
	       (.setForeground (.getSelectionForeground l)))
	     (doto lbl
	       (.setBackground (.getBackground l))
	       (.setForeground (.getForeground l)))))))))

(defn update-render-time []
  "returns the time since this function was called last"
  (dosync
   (let [old @*last-render*
	 new (ref-set *last-render* (System/currentTimeMillis))]
     (- new old))))

(defn make-next-tank-current [world]
  (let [ntanks (count (:tanks world))
	next (incr-cyclic (:current-tank world) ntanks)]
    (assoc world :current-tank next)))

(defn draw-hud [g world]
  (let [current (current-tank world)
	player (+ 1 (:current-tank world))
	move (float (:move-energy @current))
	fire (float (:fire-energy @current))]
    (draw-text-lines g (- 800 200) 0
		     (format "Current player:  %d" player)
		     (format "Move energy:     %.1f" move)
		     (format "Fire energy:       %.1f" fire))))

(defn draw-background [g width height]
  (.drawImage g *background-image* 0 0 nil))
  
(defn game-running-draw [g this]
  (let [width (.getWidth this)
	height (.getHeight this)
	img (make-img width height)
	bg (.getGraphics img)]

    ;; clear backgroud
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 width height)
      (.setColor (. Color black))
      (draw-background width height))
    
    ;; draw pointer
    (if (:pos @*mouse*)
      (draw-circle bg (:pos @*mouse*) 30 30))
    
    ;; draw the meta layer
    (with-each-entity @*my-world* entity
      (draw-meta entity bg))
    
    ;; draw the entity layer
    (with-each-entity @*my-world* entity
      (draw entity bg))
    
    ;; draw hud
    (doto bg
      (.setColor (. Color black))
      (draw-hud @*my-world*))
  
    (.dispose bg)
    (.drawImage g img 0 0 nil)))

(defn game-init-draw [g this]
  (let [width (.getWidth this)
	height (.getHeight this)]
    (doto g
      (.setColor (. Color blue))
      (.fillRect 0 0 width height)
      (.setColor (. Color white)))
    (if-let [meta (world-mode-meta @*my-world* nil)]
      (do
	(println "meta: " meta)
	(if-let [path (:path @meta)]
	  (dorun
	   (reduce #(do (draw-line g %1 %2) %2)
		   (first path)
		   (rest path))))))))

(defn my-draw [g this]
  (dosync
   (case (world-mode @*my-world*)
	 :init (game-init-draw g this)
	 :running (game-running-draw g this))))

(defn charge-for-fire [tank dt-secs]
   (let [was-charging (= (:state tank) :charging)
	 tank (assoc tank :state :charging)
	 max-range (range-for-energy (default-weapon tank)
				     (:fire-energy tank))]
     (if was-charging
       (assoc tank :charge 
	      (min max-range (+ (:charge tank)
				(* dt-secs (:charge-rate tank)))))
       (assoc tank :charge 0))))

(defn fire-from-tank! [tank world]
  (let [target (tank-target-pos @tank)
	pos (:pos @tank)
	weapon (default-weapon @tank)
	npes (fire weapon pos target)]
    (add-npe-to-world npes world)
    (alter tank subtract-fire-energy (energy-used weapon pos target))
    (alter tank assoc :state :idle)
    (notify-weapon-listeners)))
    
(defn update-tank! [tank mouse world dt-secs]
  (cond
   ;; charge when right mouse depressed
   (and (:button2down mouse)
	(can-fire? @tank)
	(> (shots (default-weapon @tank)) 0))
   (do
     (alter tank charge-for-fire dt-secs))
   
   ;; fire when released
   (and (= (:state @tank) :charging) (not (:button2down mouse)))
   (do
     (println "firing!")
     (fire-from-tank! tank world))

   ;; move when left mouse depressed
   (and (:button1down mouse) (can-move? @tank))
   (do
     (alter tank move-towards-cursor mouse dt-secs))

   true (alter tank assoc :state :idle)))

(defn change-player! [world]
  (alter (current-tank @world) reset-tank-energy)
  (alter world make-next-tank-current)
  (notify-weapon-listeners))

(defmacro update-item [selector [var coll] & forms]
  "replace the item found by selector with the result of executing forms"
  `(for [~var ~coll]
     (if (~selector ~var)
       (do ~@forms)
       ~var)))

(defmacro with-ref-for [val [ref coll] & forms]
  "evaluate forms with the ref (from coll) that = val"
  `(doseq [~ref ~coll]
      (if (= (deref ~ref) ~val)
	(do ~@forms))))

(def *my-panel*
     (proxy [JPanel] []
       (paint [g] (my-draw g this))))

(declare animation)

(defn- game-animation [x dt-secs]
  (let [tank (current-tank @*my-world*)]
    ;; update npes
    (alter *my-world* update-npes dt-secs)
    
    ;; check-collisions
    (with-each-collision [tank (map deref (:tanks @*my-world*))
			  npe (:npes @*my-world*)]
      (with-ref-for tank
	[tank-ref (:tanks @*my-world*)]
	(alter tank-ref assoc
	       :life-energy (- (:life-energy @tank-ref) (* 0.5 dt-secs)))))
    
    ;; animate the current player
    (update-tank! tank @*mouse* *my-world* dt-secs))
  
  ; honor any changes that other logic made to state
  (world-mode @*my-world*))

; state machine
; :current-state -> (cond1) (predicate) -> next state
;                   (cond2) (predicate) -> next state
(def draw-walls
     {:init
      [{:cond #(:button1down @*mouse*)
	:next-state :wait-for-release}
       {:default true :next-state :init}]
      
      :wait-for-release
      [{:cond #(not (:button1down @*mouse*))
	:action #(alter % assoc :path (cons (:pos @*mouse*) (:path (deref %))))
	:next-state :drawing-wall}
       {:default true :next-state :wait-for-release}]

      :drawing-wall
      [{:cond #(:button1down @*mouse*)
	:next-state :wait-for-release}
       {:cond #(:button2down @*mouse*)
	:next-state nil}
       {:default true :next-state :drawing-wall}]})

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn- valid-transition? [transition]
  (or (:default transition) ((:cond transition))))

(defn update-state [machine transitions]
  (let [candidates ((:state @machine) transitions)
	match (find-first valid-transition? candidates)]
    (if (:action match) ((:action match) machine))
    (alter machine assoc :state (:next-state match))
    (:next-state match)))

(defn create-machine [& meta]
  (ref (assoc (apply hash-map meta) :state :init)))

(defn- init-animation [x dt-secs]
  (let [machine (world-mode-meta @*my-world* (create-machine))]
    (println "machine: " machine)
    (let [next-state (update-state machine draw-walls)]
      (println "next state: " next-state)
      (if (nil? next-state)
	(alter *my-world* change-world-mode :running nil)
	(alter *my-world* change-world-mode :init machine)))))


(defn animation [x]
  (let [dt (update-render-time)
	dt-secs (/ (float dt) 1000)]
    (dosync
     (case (world-mode @*my-world*)
	   :init (init-animation x dt-secs)
	   :running (game-animation x dt-secs)))

    (Thread/sleep (max 0 (- *animation-sleep-ms* dt)))
    (.repaint *my-panel*)
    (send-off *agent* #'animation)
    nil))

(defn- start-animation-agent []
  (send-off *animator* animation))

(defn- restart-animation-agent []
  "useful for debugging"
  (restart-agent *animator* nil)
  (start-animation-agent))

(defn- print-agent-error [agent]
  "debugging"
  (let [err (agent-error agent)]
    (println err)
    (doseq [el (.getStackTrace err)]
      (println el))))

(defn- methods-to-strings [obj]
  (map #(.getName %) (.. obj (getClass) (getMethods))))

(defn- init-world []
  (let [img-stream (get-resource "MonthGame/tanksprite.png")
	frames (load-sprites img-stream 128)
	tank1 (ref (make-tank frames '(0 15)
			      (/ Math/PI 4) '(64 64)))
	tank2 (ref (make-tank frames '(0 15)
			      (/ (* 2 Math/PI) 3) '(300 300)))]
    ; add the players to the world
    (dosync
     (alter tank1 assoc
	    :weapons [(make-rocket-launcher 5)
		      (make-multirocket-launcher 10 5)
		      (make-projectile-launcher)]
	    :current-weapon 0)
     (alter tank2 assoc
	    :weapons [(make-rocket-launcher 7)]
	    :current-weapon 0)
     (alter *my-world* assoc
	    :tanks (vector tank1 tank2)
	    :current-tank 0
	    :npes []))))

(defn -main [& args]
  (let [panel *my-panel*
	end-turn-button (new JButton "End Turn")
	button-clicked (proxy [ActionListener] []
			 (actionPerformed
			  [ev]
			  (dosync (change-player! *my-world*))))
	main-window (JFrame. "Month Game")]

    (init-world)
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
      (.add (doto (new JPanel)
	      (.setLayout (new BorderLayout))
	      (.add *weapon-selector* (. BorderLayout LINE_START))
	      (.add end-turn-button (. BorderLayout CENTER)))
	    (. BorderLayout SOUTH))
      (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
      (.setVisible true))

    ;; start the background music
    ;(play-stream (get-resource *background-music*))
    (update-render-time) ; reset the game clock
    (start-animation-agent)))
