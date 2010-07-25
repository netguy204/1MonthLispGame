;; world.clj
;; Turn based tank combat game
;;
;; Copyright 2010 Brian Taylor
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns MonthGame.world
  (:use (MonthGame surface draw vector graphics
		   animator mouse state-machine
		   util sprite sound resources
		   scalar-math))
  (:import (java.awt Color BorderLayout)
	   (javax.swing JMenuBar JPanel)
	   (java.io File FileInputStream)))


(def *world-mouse*)

;; temp data while i design this format
(def *world-resources*
     '({:type :img
	:file ":MonthGame/jersey_wall.png"
	:tag :MonthGame.sprite/oriented-sprite
	:doto (0 40)
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

(defn build-sprite [entry resources]
  (let [resource ((:handle entry) resources)]
    (compose-resource resource entry)))

(defn- render-entry [g entry resources]
  (let [sprite (build-sprite entry resources)]
    (with-offset-g [g (:pos entry)]
      (draw-sprite g sprite))))

(defn- draw-map [g map resources]
  (let [sorted (sort-by #(second (:pos %)) map)]
    (doseq [entry sorted]
      (render-entry g entry resources))))

(def *test-world*
     (ref {:resources (load-resources *world-resources*)
	   :map *world-map*
	   :current-resource :jwall}))

(defn- build-img-asset-bar []
  (let [panel (JPanel.)
	sprites (:resources @*test-world*)]
    nil)) ;; fixme


(def *edit-machine*
     (create-machine :current-entry
		     {:handle :jwall
		      :angle 0}))

(defn- entry-for-mouse []
  (if-let [mouse-pos (:pos @*world-mouse*)]
    (assoc (:current-entry @*edit-machine*)
      :pos mouse-pos)
    (:current-entry @*edit-machine*)))

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
     (if-let [mouse-pos (:pos @*world-mouse*)]
       (render-entry g (entry-for-mouse) resources)))))
       
(def *world-surface*)
(defn- world-surface []
  (defonce *world-surface*
    (make-surface #(world-draw %1 %2)))
  *world-surface*)

(defn- place-current-tile [machine]
  (let [map (:map @*test-world*)
	new-map (cons (entry-for-mouse) map)]
    (alter *test-world* assoc :map new-map)))

(defn- change-current-frame [machine dir]
  "assumes current-entry corresponds to an oriented sprite"
  (println "change-current-frame")
  (let [entry (:current-entry @machine)
	resource ((:handle entry) (:resources @*test-world*))
	num-frames (count (:frames resource))
	curr-frame (angle-to-frame (:angle entry) num-frames)
	next-frame (case dir
			 :prev-frame (decr-cyclic curr-frame num-frames)
			 :next-frame (incr-cyclic curr-frame num-frames))
	new-entry (assoc entry :angle (frame-to-angle next-frame num-frames))]
    (alter machine assoc :current-entry new-entry)))
    

(def edit-transitions
     {:init
      [{:cond #(:button1down @*world-mouse*)
	:next-state :wait-for-release}
       {:event :wheel-up
	:action #(change-current-frame % :prev-frame)
	:next-state :init}
       {:event :wheel-down
	:action #(change-current-frame % :next-frame)
	:next-state :init}
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
  (dosync
   (alter *test-world* assoc :map (read-string (slurp file)))))

(def *map-file-type* ["A text file" "map"])

(defn- open-file []
  (let [specs [{:type *map-file-type* :fn #'open-text-file}]]
    (open-selector-then-invoke specs (world-surface))))

(defn- save-file []
  (if-let [target (save-selector (world-surface) *map-file-type*)]
    (spit target (dosync (:map @*test-world*)))))

(def *world-frame*)

(defn- about-program []
  (help-html-dialog *world-frame* "Using World Designer"
		    (get-resource "MonthGame/using-world-designer.html")))

(defn- clear-surface []
  (dosync
   (alter *test-world* assoc :map [])))

(def *main-menu*
     [["File" [["Open..." #(open-file)]
	       ["Save..." #(save-file)]
	       ["Clear surface" #(clear-surface)]]]
      ["Help" [["Instructions..." #(about-program)]]]])

(defn world-designer []
  (binding [*world-frame* (make-window "World Designer")]
    (let [frame *world-frame*
	  panel (world-surface)
	  close-handler (stop-animation-listener *edit-animator*)]
      (doto frame
	(.addWindowListener close-handler)
	(.setSize 800 600)
	(.add panel)
	(.setJMenuBar (build-menu *main-menu* (JMenuBar.)))
	(.setVisible true))
      (attach-mouse-adapter panel (make-mouse-adapter *world-mouse*))
      (start-animation *edit-animator*))))
