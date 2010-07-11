(ns MonthGame.surface)

(import '(javax.swing JPanel JFrame))

(defn make-surface [draw-fn]
  (proxy [JPanel] []
    (paint [g] (draw-fn g this))))

(defn make-window [title & opts]
  (let [frame (JFrame. title)]
    (cond
     (some #{:close-on-exit} opts)
     (.setDefaultCloseOperation frame
				(JFrame/EXIT_ON_CLOSE)))
    frame))

(defn attach-mouse-adapter [panel adapter]
  (doto panel
    (.addMouseListener adapter)
    (.addMouseMotionListener adapter)
    (.addMouseWheelListener adapter)))

