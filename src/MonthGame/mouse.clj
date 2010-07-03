(ns MonthGame.mouse
  (:use MonthGame.vector))

(import '(java.awt.event MouseAdapter MouseEvent))

(defstruct mouse-struct :pos :button1down :button2down :lastevent)
(def *mouse* (ref (struct mouse-struct nil false false)))

(defn button-to-sym [button]
  (cond
   (= button (. MouseEvent BUTTON1)) :button1down
   (= button (. MouseEvent BUTTON3)) :button2down))
  
(defn update-click [mouse ev down]
  (let [button (.getButton ev)
	buttonsym (button-to-sym button)]
    (if buttonsym (alter mouse assoc buttonsym down))))
  
(defn update-mouse [ev type]
  (dosync
   (alter *mouse* assoc :lastevent ev)
   (cond
    (= type :exit) (alter *mouse* assoc :pos nil)
    (= type :enter) (alter *mouse* assoc
			   :pos (point-to-vec (.getPoint ev)))
    (= type :motion) (alter *mouse* assoc
			    :pos (point-to-vec (.getPoint ev)))
    (= type :pressed) (update-click *mouse* ev true)
    (= type :released) (update-click *mouse* ev false))))
   
(def mouse-adapter
     (proxy [MouseAdapter] []
       (mouseClicked [e] (println "clicked"))
       (mouseDragged [e] (println "dragged"))
       (mouseEntered [e] (update-mouse e :enter))
       (mouseExited [e] (update-mouse e :exit))
       (mouseMoved [e] (update-mouse e :motion))
       (mousePressed [e] (update-mouse e :pressed))
       (mouseReleased [e] (update-mouse e :released))
       (mouseWheelMoved [mwe] (println "wheeled"))))
