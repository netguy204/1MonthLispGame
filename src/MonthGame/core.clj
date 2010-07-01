(ns MonthGame.core)

(import '(javax.swing JFrame JPanel)
	'(java.awt Color Graphics Dimension)
	'(java.awt.image BufferedImage))


(defn my-draw [g this]
  (let [width (.getWidth this)
	height (.getHeight this)
	img (new BufferedImage width height
		 (. BufferedImage TYPE_INT_ARGB))
	bg (.getGraphics img)]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 width height))
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

    
(def panel (proxy [JPanel] []
	       (paint [g] (my-draw g this))))


(def main-window (doto (JFrame. "Month Game")
		   (.setSize 400 400)
		   (.add panel)
		   (.setVisible true)))

