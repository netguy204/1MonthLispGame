(ns MonthGame.core
  (:use (MonthGame vector sprite draw graphics
		   tank scalar-math mouse
		   missiles explosions entity
		   weapons sound state-machine
		   util surface animator world))
  (:gen-class))

(import '(javax.swing JFrame JButton JPanel
		      JComboBox ComboBoxModel JLabel
		      ListCellRenderer ImageIcon
		      SwingConstants UIManager JMenuBar)
	'(java.awt Color Graphics Dimension 
		   BorderLayout GridBagLayout GridBagConstraints)
	'(java.awt.image BufferedImage)
	'(java.awt.event ActionListener)
	'(javax.swing.event ListDataEvent))

(defrecord World
  [tanks current-tank npes])

(def *my-world* (ref (World. [] nil [])))
(def *weapon-selector* (new JComboBox))
(def *weapon-list-listeners* (ref []))
(def *background-music* "MonthGame/background1.mp3")
(def *background-image*
     (load-img (get-resource "MonthGame/paper.png")))

(defn- current-tank []
  (nth (:tanks @*my-world*) (:current-tank @*my-world*)))

(defn- world-mode [world]
  (or (:mode world) :init))

(defn- world-mode-meta [world default]
  (or (:mode-meta world) default))

(defn- change-world-mode [world mode meta]
  (assoc world :mode mode :mode-meta meta))

(defn num-weapons [tank]
  (count (:weapons tank)))

(defn- make-lde [world]
  (ListDataEvent. world
		  (ListDataEvent/CONTENTS_CHANGED)
		  0 (num-weapons @(current-tank))))

(defn- notify-weapon-listeners []
  (dosync
   (let [ev (make-lde @*my-world*)]
     (doseq [listener @*weapon-list-listeners*]
       (.contentsChanged listener ev)))))

(defn- current-weapon [world]
  (:current-weapon @(current-tank)))

(defn- remove-from-list [lst item]
  (filter #(not= % item) lst))

(def *weapon-list-model*
     (proxy [ComboBoxModel] []
       (getSelectedItem [] (dosync (current-weapon @*my-world*)))

       (setSelectedItem
	[item] 
	(dosync
	 (alter (current-tank)
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
		(dosync (num-weapons @(current-tank))))))

(def *weapon-list-renderer*
     (proxy [ListCellRenderer] []
       (getListCellRendererComponent 
	[l o idx selected focused]
	(dosync
	 (let [wpn (nth (:weapons @(current-tank)) o)
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

(defn make-next-tank-current [world]
  (let [ntanks (count (:tanks world))
	next (incr-cyclic (:current-tank world) ntanks)]
    (assoc world :current-tank next)))

(defn draw-hud [g world]
  (let [current (current-tank)
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

(def *wall-sprite*
     (let [img-stream (get-resource "MonthGame/jersey_wall.png")
	   frames (scale-img (load-sprites img-stream) 160 160)]
       (make-oriented-sprite frames 0 '(0 40))))

(defn- wall-step-mag [dir]
  (* (first (img-size *wall-sprite*)) 0.6))

(defn- discretize-wall-mag [dir mag]
  (let [step-size (wall-step-mag dir)
	num-times (Math/round (/ mag step-size))]
    (* num-times step-size)))

(defn- project-wall-towards [from to]
  (let [tovec (vsub to from)
	dir (unit-vector tovec)
	angle (discretize-angle (vang dir) *wall-sprite*)
	mag (vmag tovec)]
    {:angle angle :mag (discretize-wall-mag dir mag)}))

(defn- closest-point-towards [from to]
  (vadd from (project-wall-towards from to)))

(defn- make-wall-entity [sprite pos]
  (let [wall (reify
	      Entity
	      (draw
	       [entity g]
	       (with-offset-g [g pos]
		 (draw-sprite g sprite)))

	      (draw-meta [entity g] nil)
	      (position [entity] pos)
	      (radius [entity] 0)
	      (collided-with [entity other] entity))]
    (derive (class wall) ::wall)
    wall))

(defmethod MonthGame.entity/intersect [MonthGame.missiles.Rocket ::wall] [e1 e2]
  (if (circle-intersect e1 e2)
    true
    false))

(defmethod MonthGame.entity/intersect [MonthGame.missiles.Projectile ::wall] [e1 e2]
  false)

(defn- make-wall-entities [from to]
  (let [proj (project-wall-towards from to)
	dir (unit-vector proj)
	angle (vang dir)
	sprite (assoc *wall-sprite* :angle angle)
	dist (vmag proj)
	step-size (wall-step-mag dir)
	steps (/ dist step-size)
	step (vmul dir step-size)]

    (map #(make-wall-entity sprite %)
	 (map #(vadd from (vmul step %)) (range steps)))))

(defn- path-to-entities [path]
  (let [pairs (zip (drop 1 path) (drop-last path))]
    (mapcat #(make-wall-entities (first %) (second %)) pairs)))

(defn game-init-draw [g this]
  (let [width (.getWidth this)
	height (.getHeight this)]
    (doto g
      (game-running-draw this)
      (.setColor (. Color black))
      (draw-text-lines 300 0 "WORLD EDIT MODE"))
    (if-let* 
     [meta (world-mode-meta @*my-world* nil)]
     [path (:path @meta)]
     (let [to-mouse
	   (if-let [pos (:pos @*mouse*)]
	     (try (make-wall-entities (first path) pos)
		  (catch java.lang.ArithmeticException e nil)))
	   walls (concat (path-to-entities path) to-mouse)]
       (doseq [ent (sort-by ypos-for-entity walls)]
	 (draw ent g))))))

(defn my-draw [g this]
  (dosync
   (case (world-mode @*my-world*)
	 :init (game-init-draw g this)
	 :running (game-running-draw g this))))

(defn charge-for-fire [tank dt-secs]
   (let [max-range (range-for-energy (default-weapon tank)
				     (:fire-energy tank))]
     (assoc tank :charge 
	    (min max-range (+ (:charge tank)
			      (* dt-secs (:charge-rate tank)))))))

(defn fire-from-tank! [tank world]
  (let [target (tank-target-pos @tank)
	pos (:pos @tank)
	dist (vdist target pos)
	weapon (default-weapon @tank)
	npes (fire weapon pos target)]
    ;; require some range for fire to prevent divide by 0
    (if (> dist 3)
      (do
	(add-npe-to-world npes world)
	(alter tank subtract-fire-energy (energy-used weapon pos target))
	(alter tank assoc :state :idle)
	(notify-weapon-listeners)))))

(def tank-motion
     {:init
      [{:cond #(and (:button2down @*mouse*)
		    (can-fire? @(current-tank))
		    (> (shots (default-weapon @(current-tank))) 0))
	:action (fn [m dt] (alter (current-tank) assoc :charge 0))
	:next-state :charging}
       {:cond #(and (:button1down @*mouse*)
		    (can-move? @(current-tank)))
	:next-state :moving}
       {:default true :next-state :init}]

      :moving
      [{:cond #(:button1down @*mouse*)
	:action (fn [m dt] (alter (current-tank) move-towards-cursor @*mouse* dt))
	:next-state :moving}
       {:cond #(not (:button1down @*mouse*))
	:next-state :init}]

      :charging
      [{:cond #(:button2down @*mouse*)
	:action (fn [m dt] (alter (current-tank) charge-for-fire dt))
	:next-state :charging}
       {:cond #(not (:button2down @*mouse*))
	:action (fn [m dt]
		  (fire-from-tank! (current-tank) *my-world*)
		  (alter (current-tank) assoc :charge 0))
	:next-state :init}]})
	
(defn update-tank! [tank world dt-secs]
  (let [machine (world-mode-meta @*my-world* (create-machine))]
    (update-state machine tank-motion dt-secs)
    (alter *my-world* change-world-mode :running machine)))

(defn change-player! [world]
  (alter (current-tank) reset-tank-energy)
  (alter world make-next-tank-current)
  (notify-weapon-listeners))

(def *my-panel* (make-surface my-draw))

(defn update-npe [npe f & args]
  (alter *my-world* assoc :npes
	 (update-item #(= npe %)
		      [old-npe (:npes @*my-world*)]
		      (apply f args))))

(declare animation)

(defn- game-animation [dt-secs]
  (let [tank (current-tank)]
    ;; update npes
    (alter *my-world* update-npes dt-secs)
    
    ;; TODO check collisions between npes and walls
    (with-each-collision [npe (:npes @*my-world*)
			  barrier (:barriers @*my-world*)]
      (update-npe npe collided-with npe barrier))
    ;; TODO check collisions between tanks and walls

    ;; check collisions between npes and tanks
    (with-each-collision [npe (:npes @*my-world*)
			  tank (map deref (:tanks @*my-world*))]
      (update-npe npe collided-with npe tank)
      (with-ref-for tank
	[tank-ref (:tanks @*my-world*)]
	(alter tank-ref damage npe)))
    
    ;; animate the current player
    (update-tank! tank *my-world* dt-secs))
  
  ; honor any changes that other logic made to state
  (world-mode @*my-world*))

(def draw-walls
     {:init
      [{:cond #(:button1down @*mouse*)
	:next-state :wait-for-release}
       {:default true :next-state :init}]
      
      :wait-for-release
      [{:cond #(not (:button1down @*mouse*))
	:action #(alter % assoc :path 
			(if-let [path (:path (deref %))]
			  (let [nextpt (closest-point-towards 
					(first path)
					(:pos @*mouse*))]
			    (if (not= nextpt (first path))
				      (cons nextpt path)
				      path))
			  (list (:pos @*mouse*))))
	:next-state :drawing-wall}
       {:default true :next-state :wait-for-release}]

      :drawing-wall
      [{:cond #(:button1down @*mouse*)
	:next-state :wait-for-release}
       {:cond #(:button2down @*mouse*)
	:next-state :finish-exit}
       {:default true :next-state :drawing-wall}]

      :finish-exit
      [{:cond #(not (:button2down @*mouse*))
	:next-state nil}
       {:default true :next-state :finish-exit}]})

(defn- init-animation [dt-secs]
  (let [machine (world-mode-meta @*my-world* (create-machine))
	next-state (update-state machine draw-walls)]
    (if (nil? next-state)
      (if-let [path (:path @machine)]
	(do
	  (alter *my-world* assoc :barriers (path-to-entities path))
	  (alter *my-world* change-world-mode :running nil)))
      (alter *my-world* change-world-mode :init machine))))

(defn animation [dt-secs]
  (dosync
   (case (world-mode @*my-world*)
	 :init (init-animation dt-secs)
	 :running (game-animation dt-secs)))

  (.repaint *my-panel*))

(def *animator* (make-animator animation 50))

(defn- init-world []
  (let [img-stream (get-resource "MonthGame/tanksprite.png")
	frames (scale-img (load-sprites img-stream) 128 128)
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

(def *game-menu*
     [["Tests" [["Particle test..." #(MonthGame.particles/particle-test-window)]
		["World designer..." #(MonthGame.world/world-designer)]]]])

(defn -main [& args]
  (let [panel *my-panel*
	end-turn-button (new JButton "End Turn")
	button-clicked (proxy [ActionListener] []
			 (actionPerformed
			  [ev]
			  (dosync (change-player! *my-world*))))
	main-window (make-window "Month Game" :close-on-exit)]

    (init-world)
    (.addActionListener end-turn-button button-clicked)
    (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))

    (attach-mouse-adapter panel (make-mouse-adapter *mouse*))

    ;; set up the weapon selector
    (.setModel *weapon-selector* *weapon-list-model*)
    (.setRenderer *weapon-selector* *weapon-list-renderer*)

    (doto main-window
      (.setSize 800 600)
      (.setJMenuBar (build-menu *game-menu* (JMenuBar.)))
      (.add panel (. BorderLayout CENTER))
      (.add (doto (new JPanel)
	      (.setLayout (new BorderLayout))
	      (.add *weapon-selector* (. BorderLayout LINE_START))
	      (.add end-turn-button (. BorderLayout CENTER)))
	    (. BorderLayout SOUTH))
      (.setVisible true))

    ;; start the background music
    ;(play-stream (get-resource *background-music*))
    (start-animation *animator*)))
