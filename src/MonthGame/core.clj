;; core.clj
;; sets up the world and contains the program entry point
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

(declare *my-world*)
;; be noisy if i do dumb things
;(set! *warn-on-reflection* true)
(ns MonthGame.core
  (:refer-clojure :exclude [spit])
  (:use (MonthGame vector sprite draw graphics
		   tank scalar-math mouse
		   missiles explosions entity
		   weapons sound state-machine
		   util surface animator world
		   resources wall)
	(clojure.contrib duck-streams))
  (:import (javax.swing JFrame JButton JPanel
			JComboBox ComboBoxModel JLabel
			ListCellRenderer ImageIcon
			SwingConstants UIManager JMenuBar)
	   (java.awt Color Graphics2D Dimension 
		     BorderLayout GridBagLayout GridBagConstraints)
	   (java.awt.image BufferedImage)
	   (java.awt.event ActionListener)
	   (javax.swing.event ListDataEvent))
  (:gen-class))

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
  (draw-img g *background-image* '(0 0)))
  
(defn game-running-draw [#^Graphics2D g #^JPanel this]
  (let [width (.getWidth this)
	height (.getHeight this)]

    ;; clear backgroud
    (doto g
      (.setColor (. Color white))
      (.fillRect 0 0 width height)
      (.setColor (. Color black))
      (draw-background width height))
    
    ;; draw pointer
    (if (:pos @*mouse*)
      (draw-circle g (:pos @*mouse*) 30 30))
    
    ;; draw the meta layer
    (with-each-entity @*my-world* entity
      (draw-meta entity g))
    
    ;; draw the entity layer
    (with-each-entity @*my-world* entity
      (draw entity g))
    
    ;; draw hud
    (doto g
      (.setColor (. Color black))
      (draw-hud @*my-world*))))

(def *resources* (load-resources MonthGame.world/*world-resources*))

(defn- load-map [file]
  (let [data (read-string (slurp* file))]
    (doall
     (for [entry data]
       (make-wall-entity (build-sprite entry *resources*)
			 (:pos entry))))))
	
(defn my-draw [g this]
  (dosync
   (case (world-mode @*my-world*)
	 :init (game-running-draw g this))))

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
      [{:cond #(and (:button1down @*mouse*)
		    (can-move? @(current-tank)))
	:action (fn [m dt] (alter (current-tank)
				  move-towards-cursor @*mouse* 
				  (:barriers @*my-world*) dt))
	:next-state :moving}
       {:default true :next-state :init}]

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
    (alter *my-world* change-world-mode :init machine)))

(defn change-player! [world]
  (alter (current-tank) reset-tank-energy)
  (alter world make-next-tank-current)
  (notify-weapon-listeners))

(def #^JPanel *my-panel* (make-surface my-draw))

(defn update-npe [npe f & args]
  (alter *my-world* assoc :npes
	 (update-item #(= npe %)
		      [old-npe (:npes @*my-world*)]
		      (apply f args))))

(declare animation)

(defn- game-animation [dt-secs]
  "update the world structure during gameplay"
  (let [tank (current-tank)]
    ;; update npes
    (alter *my-world* update-npes dt-secs)

    ;; check collisions between tanks and barriers
    (with-each-collision [tank (map deref (:tanks @*my-world*))
			  barrier (:barriers @*my-world*)]
      (with-ref-for tank
	[tank-ref (:tanks @*my-world*)]
	(alter tank-ref assoc
	       :pos (vadd (position tank)
			  (vmul (away-from-wall-norm barrier
						     (position tank))
				5)))))
	       
    ;; check collisions between npes and barriers
    (with-each-collision [npe (:npes @*my-world*)
			  barrier (:barriers @*my-world*)]
      (update-npe npe collided-with npe barrier))

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

(defn animation [dt-secs]
  (dosync
   (case (world-mode @*my-world*)
	 :init (game-animation dt-secs)))

  (.repaint *my-panel*))

(def *animator* (make-animator animation 50))

(defn- init-world []
  (let [img-stream (get-resource "MonthGame/tanksprite.png")
	frames (scale-img (load-sprites img-stream) 128 128)
	tank1 (ref (make-tank frames '(0 15)
			      (/ Math/PI 4) '(30 64)))
	tank2 (ref (make-tank frames '(0 15)
			      (/ (* 5 Math/PI) 4) '(750 450)))]
    ; add the players to the world
    (dosync
     (alter tank1 assoc
	    :weapons [(make-rocket-launcher 10)
		      (make-multirocket-launcher 2 5)
		      (make-projectile-launcher)]
	    :current-weapon 0)
     (alter tank2 assoc
	    :weapons [(make-rocket-launcher 10)
		      (make-multirocket-launcher 2 5)
		      (make-projectile-launcher)]
	    :current-weapon 0)
     (alter *my-world* assoc
	    :tanks (vector tank1 tank2)
	    :current-tank 0
	    :barriers (load-map (get-resource "MonthGame/wavy-world.map"))
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
      (.setJMenuBar (build-menu *game-menu* (JMenuBar.)))
      (.add (doto panel 
	      (.setPreferredSize (Dimension. 800 600)))
	    (. BorderLayout CENTER))
      (.add (doto (new JPanel)
	      (.setLayout (new BorderLayout))
	      (.add *weapon-selector* (. BorderLayout LINE_START))
	      (.add end-turn-button (. BorderLayout CENTER)))
	    (. BorderLayout SOUTH))
      (.pack)
      (.setVisible true))

    ;; start the background music
    ;(play-stream (get-resource *background-music*))
    (start-animation *animator*)))
