(ns MonthGame.world
  (:use MonthGame.surface
	MonthGame.draw
	MonthGame.vector
	MonthGame.animator
	MonthGame.mouse
	MonthGame.state-machine
	MonthGame.util)
  (:import (java.awt Color)
	   (javax.swing JMenuItem JMenuBar JMenu)
	   (java.awt.event ActionListener)))


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

(defn- world-draw [g this]
  (let [w (.getWidth this)
	h (.getHeight this)]
    (doto g
      (.setColor (Color/white))
      (fill-rect '(0 0) (list w h)))))

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

(defn- open-file []
  (println
   (open-selector (world-surface)
		  ["A text file" "txt" "clj"]
		  ["An image" "jpg" "jpeg" "png"])))

(defn- save-file []
  (println "save"))

(defn- about-program []
  (println "about"))

(def *main-menu*
     [["File" [["Open..." #(open-file)]
	       ["Save..." save-file]]]
      ["Help" [["About..." about-program]]]])

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
