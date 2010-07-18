(ns MonthGame.world
  (:use (MonthGame surface draw vector graphics
		   animator mouse state-machine
		   util sprite sound resources))
  (:import (java.awt Color BorderLayout)
	   (javax.swing JMenuBar JPanel)
	   (java.io File FileInputStream)))


(def *world-mouse*)

;; temp data while i design this format
(def *world-resources*
     '({:type :img
	:file ":MonthGame/jersey_wall.png"
	:tag :MonthGame.sprite/oriented-sprite
	:handle :jwall}

       {:type :sound
	:file ":MonthGame/explosion1.mp3"
	:handle :explode}))

(def *world-map*
     [{:handle :jwall ; world reader consumes
       :pos '(0 0) ; wr consumes
       :angle (/ Math/PI 4)} ; wr ignores, passed to resource

      {:handle :jwall
       :pos '(100 100)
       :angle (/ Math/PI 2)}])

(defn- render-entry [g entry resources]
  (let [resource ((:handle entry) resources)
	sprite (compose-resource resource entry)]
    (with-offset-g [g (:pos entry)]
      (draw-sprite g sprite))))

(defn- draw-map [g map resources]
  (doseq [entry map]
    (render-entry g entry resources)))

(def *test-world*
     (ref {:resources (load-resources *world-resources*)
	   :map *world-map*
	   :current-resource :jwall}))

(defn- build-img-asset-bar []
  (let [panel (JPanel.)
	sprites (:resources @*test-world*)]
    nil)) ;; fixme

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
     (if (:pos @*world-mouse*)
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
      [{:cond #(:button1down @*world-mouse*)
	:next-state :wait-for-release}
       {:default true :next-state :init}]

      :wait-for-release
      [{:cond #(not (:button1down @*world-mouse*))
	:action #(place-current-tile %)
	:next-state :init}
       {:default true :next-state :wait-for-release}]})

(defn- on-mouse-wheel [mouse ev]
  (let [event (if (< (.getWheelRotation ev) 0) :wheel-up :wheel-down)]
    (update-state-with-event *edit-machine* edit-transitions event)))

(def *world-mouse* (make-mouse #'on-mouse-wheel))

(defn edit-animation [dt-secs]
  (let [next-state (update-state *edit-machine* edit-transitions)]
    (.repaint (world-surface))))

(def *edit-animator* (make-animator #(edit-animation %) 50))

(defn tile-animation [dt-secs state]
  (println "tile animation")
  (dosync
   (alter state assoc :sprite
	  (advance-animation (:sprite @state) dt-secs)))
  (.repaint (:panel @state)))

(defn- change-sprite-tag [sprite tag]
  (let [resource (assoc (unload-resource (:resource sprite)) :tag tag)]
    (println "new resource is " resource)
    (assoc (load-resource resource)
      :animation loop-animation)))

(defn- make-tile-drawer [file]
  (println "make-tile-drawer")
  (let [resource {:type :img
		  :file file
		  :tag :MonthGame.sprite/static-sprite}
	sprite (assoc (load-resource resource)
		 :animation loop-animation)
	state (ref {:sprite sprite})
	animator (make-animator #'tile-animation 50)
	drawer (fn [g this]
		 (let [w (.getWidth this)
		       h (.getHeight this)
		       pos (vint (list (/ w 2) (/ h 2)))]
		   (fill-rect g '(0 0) (list w h))
		   (with-offset-g [g pos]
		     (draw-sprite g (dosync (:sprite @state))))))
	panel (make-surface drawer)]

    (dosync (alter state assoc :panel panel))
    (start-animation animator state)

    {:panel panel

     :set-mode
     (fn [new-mode]
       (println state)
       (println "changing sprite tag")
       (let [old-mode (-> @state :sprite :resource :tag)]
	 (if (= new-mode old-mode)
	   (println "already in mode" new-mode)
	   (do
	     (println new-mode " is a new mode. Was " old-mode)
	     (alter state assoc
		    :sprite (change-sprite-tag (:sprite @state) new-mode))))))

				     
     :set-close-handler
     (fn [frame]
       (.addWindowListener
	frame (stop-animation-listener animator)))}))

(defn- show-tile-config [file]
  (let [frame (make-window "Tile type")
	drawer (make-tile-drawer file)
	set-mode (fn [x] (dosync ((:set-mode drawer) x)))]
    (doto frame
      (.setSize 400 400)
      (.add (:panel drawer) (BorderLayout/CENTER))
      (.add (make-combo-box [{:name "Static"
			      :fn #(set-mode :MonthGame.sprite/static-sprite)}
			     {:name "Directional"
			      :fn #(set-mode :MonthGame.sprite/oriented-sprite)}])
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
    (attach-mouse-adapter panel (make-mouse-adapter *world-mouse*))
    (start-animation *edit-animator*)))
