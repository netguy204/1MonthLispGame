;; mouse.clj
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

(ns MonthGame.mouse
  (:use MonthGame.vector))

(import '(java.awt.event MouseAdapter MouseEvent))

(defstruct mouse-struct :pos :button1down :button2down :lastevent :on-wheel)

(defn nil-on-wheel [m ev] nil)

(defn make-mouse [on-wheel]
  (ref (struct mouse-struct nil false false nil on-wheel)))

(def *mouse* (make-mouse nil-on-wheel))

(defn button-to-sym [button]
  (cond
   (= button (. MouseEvent BUTTON1)) :button1down
   (= button (. MouseEvent BUTTON3)) :button2down))
  
(defn- update-click [mouse ev down]
  (let [button (.getButton ev)
	buttonsym (button-to-sym button)]
    (if buttonsym (alter mouse assoc buttonsym down))))
  
(defn- update-mouse [mouse ev type]
  (dosync
   (alter mouse assoc :lastevent ev)
   (case type
    :exit (alter mouse assoc :pos nil)
    :enter (alter mouse assoc
		  :pos (point-to-vec (.getPoint ev)))
    :motion (alter mouse assoc
		   :pos (point-to-vec (.getPoint ev)))
    :pressed (update-click mouse ev true)
    :released (update-click mouse ev false)
    :scrolled ((:on-wheel @mouse) mouse ev))))
   
(defn make-mouse-adapter [mouse]
     (proxy [MouseAdapter] []
       (mouseClicked [e] (println "clicked"))
       (mouseDragged [e] (update-mouse mouse e :motion))
       (mouseEntered [e] (update-mouse mouse e :enter))
       (mouseExited [e] (update-mouse mouse e :exit))
       (mouseMoved [e] (update-mouse mouse e :motion))
       (mousePressed [e] (update-mouse mouse e :pressed))
       (mouseReleased [e] (update-mouse mouse e :released))
       (mouseWheelMoved [mwe] (update-mouse mouse mwe :scrolled))))
