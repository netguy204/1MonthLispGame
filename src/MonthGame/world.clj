(ns MonthGame.world
  (:use (MonthGame surface draw vector
		   animator mouse state-machine
		   util sprite sound))
  (:import (java.awt Color BorderLayout)
	   (javax.swing JMenuBar JComboBox)
	   (java.io File FileInputStream)))


(defn make-world [w h tw th]
  {:width w
   :height h
   :tiles []})

(defn make-box [ul lr]
  (list ul lr))

(defn inbox? [box pt]
  (let [[[x1 y1] [x2 y2]] box
	[px py] pt]
    (and
     (>= px x1) (<= px x2)
     (>= py y1) (<= py y2))))

(defn tiles-in-box [ul lr] nil)

(defn- load-resource-file [url]
  (if (= (nth url 0) \:)
    (get-resource (apply str (drop 1 url)))
    (FileInputStream. url)))

(defn- resource-dispatch-fn [resource]
  (:type resource))

(defmulti load-resource resource-dispatch-fn)

(defmethod load-resource :img [resource]
  (assoc resource :data
	 (load-img (load-resource-file (:file resource)))))

(defmethod load-resource :sound [resource]
  (assoc resource :data
	 (read-frames (load-resource-file (:file resource)))))

(defmulti unload-resource resource-dispatch-fn)

(defmethod unload-resource :default [resource]
  (dissoc resource :data))

(defn- load-resources [resources]
  (zipmap (map #(:handle %) resources)
	  (map load-resource resources)))

(defn- save-resources [rmap]
  (map unload-resource (vals rmap)))

(defmulti get-sprite-fn (fn [resource, _] (:mode resource)))

(defn- resource-height [resource]
  (or (:size resource)
      (.getHeight (:data resource))))

(defmethod get-sprite-fn :directional [resource]
  (let [height (resource-height resource)
	frames (read-sprites-mem (:data resource) height)]
    (fn [g entry]
      (let [sprite (make-oriented-sprite 
		    frames
		    (:orientation entry)
		    (or (:doto entry) '(0 0)))]
	(draw-sprite sprite g (:pos entry))))))
       
(defn- render-entry [g entry resources]
  (let [resource ((:handle entry) resources)
	sprite (get-sprite-fn resource)]
    (sprite g entry)))

(def *world-resources*
     '({:type :img
	:file ":MonthGame/jersey_wall.png"
	:mode :directional
	:handle :jwall}
       {:type :sound
	:file ":MonthGame/explosion1.mp3"
	:handle :explode}))

(def *world-resources2* (load-resources *world-resources*))

(def *world-map*
     [{:handle :jwall
       :pos '(0 0)
       :orientation (unitdir (/ Math/PI 4))}
      {:handle :jwall
       :pos '(100 100)
       :orientation (unitdir (/ Math/PI 2))}])

(defn- draw-map [g map resources]
  (doseq [entry map]
    (render-entry g entry resources)))

(def *test-world*
     {:resources *world-resources*})

(defn- world-draw [g this]
  (let [w (.getWidth this)
	h (.getHeight this)]
    (doto g
      (.setColor (Color/white))
      (fill-rect '(0 0) (list w h))
      (draw-map *world-map* *world-resources2*))))

(def *world-surface*)
(defn- world-surface []
  (defonce *world-surface*
    (make-surface #(world-draw %1 %2)))
  *world-surface*)

(def *edit-machine* (create-machine :tiles [] 
				    :current-tile nil))

(defn place-current-tile [machine]
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
    (open-selector-then-invoke specs)))

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
