(ns MonthGame.world
  (:use (MonthGame surface draw vector graphics
		   animator mouse state-machine
		   util sprite sound resources))
  (:import (java.awt Color BorderLayout)
	   (javax.swing JMenuBar)
	   (java.io File FileInputStream)))


;; temp data while i design this format
(def *world-resources*
     '({:type :img
	:file ":MonthGame/jersey_wall.png"
	:mode :directional
	:handle :jwall}
       {:type :sound
	:file ":MonthGame/explosion1.mp3"
	:handle :explode}))

(def *world-map*
     [{:handle :jwall
       :pos '(0 0)
       :orientation (/ Math/PI 4)}
      {:handle :jwall
       :pos '(100 100)
       :orientation (/ Math/PI 2)}])

(defn- render-entry [g entry resources]
  (let [resource ((:handle entry) resources)
	sprite (get-sprite-fn resource)]
    (sprite g entry)))

(defn- draw-map [g map resources]
  (doseq [entry map]
    (render-entry g entry resources)))

(def *test-world*
     (ref {:resources (load-resources *world-resources*)
	   :map *world-map*
	   :current-resource :jwall
	   :current-meta {:orientation 0}}))

(defn- world-draw [g this]
  (dosync
   (let [w (.getWidth this)
	 h (.getHeight this)
	 resources (:resources @*test-world*)
	 map (:map @*test-world*)]
     (doto g
       (.setColor (Color/white))
       (fill-rect '(0 0) (list w h))
       (draw-map map resources))
     (if (:pos @*mouse*)
       nil)))) ; dostuff
       

(def *world-surface*)
(defn- world-surface []
  (defonce *world-surface*
    (make-surface #(world-draw %1 %2)))
  *world-surface*)

(def *edit-machine* (create-machine :tiles [] 
				    :current-tile nil))

(defn place-current-tile [machine]
  ;; replace world current tile with tile from machine
  (println "current tile is" (:current-tile @machine)))

(def edit-transitions
     {:init
      [{:cond #(:button1down @*mouse*)
	:next-state :wait-for-release}
       {:default true :next-state :init}]

      :wait-for-release
      [{:cond #(not (:button1down @*mouse*))
	:action #(place-current-tile %)
	:next-state :init}
       {:default true :next-state :wait-for-release}]})

(defn edit-animation [dt-secs]
  (let [next-state (update-state *edit-machine* edit-transitions)]
    (.repaint (world-surface))))

(def *edit-animator* (make-animator #(edit-animation %) 50))

(defn tile-animation [dt-secs state]
  (let [curr-state @state]
    (with-age [curr-state dt-secs]

      (.repaint (:panel curr-state))
      (ref-set state curr-state))))

(defn tile-draw [g this tile state]
  (let [w (.getWidth this)
	h (.getHeight this)]
    (case (:mode @state)
	  :static
	  (draw-img g tile '(0 0))

	  :animated
	  (doto g
	    (.setColor (Color/black))
	    (fill-rect '(0 0) (list w h)))

	  :directional
	  (doto g
	    (.setColor (Color/white))
	    (fill-rect '(0 0) (list w h))))))

(defn- make-tile-drawer [file]
  (let [img (load-img (File. file))
	state (ref {:mode :static})
	animator (make-animator #'tile-animation 50)
	drawer (fn [g this] (tile-draw g this img state))
	panel (make-surface drawer)]

    (dosync (alter state assoc :panel panel))
    (start-animation animator state)

    {:panel panel
     :set-mode (fn [new-mode] (alter state assoc :mode new-mode))
     :set-close-handler (fn [frame] (.addWindowListener
				     frame (stop-animation-listener animator)))}))

(defn- show-tile-config [file]
  (let [frame (make-window "Tile type")
	drawer (make-tile-drawer file)
	set-mode (fn [x] (dosync ((:set-mode drawer) x)))]
    (doto frame
      (.setSize 400 400)
      (.add (:panel drawer) (BorderLayout/CENTER))
      (.add (make-combo-box [{:name "Static" :fn #(set-mode :static)}
			     {:name "Animated" :fn #(set-mode :animated)}
			     {:name "Directional" :fn #(set-mode :directional)}])
	    (BorderLayout/SOUTH))
      ((:set-close-handler drawer))
      (.setVisible true))))

(defn- open-image-file [file]
  (println "a sprite!")
  (show-tile-config file))

(defn- open-text-file [file]
  (println "some text!"))

(defn- open-file []
  (let [specs [{:type ["A text file" "txt" "clj"] :fn #'open-text-file}
	       {:type ["An image" "jpg" "jpeg" "png"] :fn #'open-image-file}]]
    (open-selector-then-invoke specs (world-surface))))

(defn- save-file []
  (println "save"))

(defn- about-program []
  (println "about"))

(def *main-menu*
     [["File" [["Open..." #(open-file)]
	       ["Save..." #(save-file)]]]
      ["Help" [["About..." #(about-program)]]]])

(defn world-designer []
  (let [frame (make-window "World Designer")
	panel (world-surface)
	close-handler (stop-animation-listener *edit-animator*)]
    (doto frame
      (.addWindowListener close-handler)
      (.setSize 800 600)
      (.add panel)
      (.setJMenuBar (build-menu *main-menu* (JMenuBar.)))
      (.setVisible true))
    (attach-mouse-adapter panel (make-mouse-adapter *mouse*))
    (start-animation *edit-animator*)))
