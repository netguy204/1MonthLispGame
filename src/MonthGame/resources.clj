(ns MonthGame.resources
  (:use (MonthGame vector util graphics
		   sound sprite))
  (:import (java.io File FileInputStream)))

(defn- load-resource-file [url]
  (if (= (nth url 0) \:)
    (get-resource (apply str (drop 1 url)))
    (FileInputStream. url)))

(defn- resource-dispatch [resource]
  (:type resource))

(defmulti load-resource resource-dispatch)

(defmethod load-resource :img [resource]
  (assoc resource :data
	 (load-img (load-resource-file (:file resource)))))

(defmethod load-resource :sound [resource]
  (assoc resource :data
	 (read-audio-frames (load-resource-file (:file resource)))))

(defmulti unload-resource resource-dispatch)

(defmethod unload-resource :default [resource]
  (dissoc resource :data))

(defn load-resources [resources]
  (zipmap (map #(:handle %) resources)
	  (map load-resource resources)))

(defn save-resources [rmap]
  (map unload-resource (vals rmap)))

(defn- resource-height [resource]
  (or (:size resource)
      (.getHeight (:data resource))))

(defmulti get-sprite-fn (fn [resource, _] (:mode resource)))

(defmethod get-sprite-fn :directional [resource]
  (let [height (resource-height resource)
	frames (read-frames (:data resource))]
    (fn [g entry]
      (let [sprite (make-oriented-sprite 
		    frames
		    (:orientation entry)
		    (or (:doto entry) '(0 0)))]
	(draw-sprite sprite g (:pos entry))))))
       
