(ns MonthGame.animator)

(import '(java.awt.event WindowAdapter))

(defn make-animator [anim-fn max-sleep]
  {:agent (agent true)
   :anim-fn anim-fn
   :max-sleep max-sleep
   :clock (ref (System/currentTimeMillis))})

(defn- enable-animator [x]
  true)

(defn- disable-animator [x]
  false)

(defn- update-clock [animator]
  (let [old @(:clock animator)
	new (System/currentTimeMillis)]
    (ref-set (:clock animator) new)
    (- new old)))

(defn- animation-step [agent animator & args]
  (if agent
    (let [dt (dosync (update-clock animator))
	  dt-secs (/ dt 1000)]
      (dosync
       (apply (:anim-fn animator) dt-secs args))
      (Thread/sleep (max 0 (- (:max-sleep animator) dt)))
      (apply send-off *agent* #'animation-step animator args)
      true)
    false))


(defn reset-clock [animator]
  (ref-set (:clock animator) (System/currentTimeMillis)))

(defn start-animation [animator & args]
  (send (:agent animator) enable-animator)
  (dosync (update-clock animator))
  (apply send (:agent animator) animation-step animator args))

(defn stop-animation [animator]
  (send (:agent animator) disable-animator))

(defn stop-animation-listener [animator]
  (proxy [WindowAdapter] []
    (windowClosing
     [e] (stop-animation animator))))
