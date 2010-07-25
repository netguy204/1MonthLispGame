;; resources.clj
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

(ns MonthGame.resources
  (:use (MonthGame vector util graphics
		   sound sprite))
  (:import (java.io File FileInputStream)))

(defn load-resource-file [url]
  (if (= (nth url 0) \:)
    (get-resource (apply str (drop 1 url)))
    (FileInputStream. url)))

(defn- type-dispatch [r]
  (:type r))

(defmulti load-resource
  "loads the file data for a resource and passes it on to the detailed loader"
  type-dispatch)

(defmulti load-img-resource
  "detail loader for image resources"
  (fn [r] (:tag r)))

(defmulti compose-resource
  "compose a resource entry with a map entry to produce a renderable entity"
  (fn [r m] (list (meta r) (:tag m))))

(defmethod load-resource :img [resource]
  (load-img-resource
   (assoc resource :data
	  (load-img (load-resource-file (:file resource))))))

(defn- scale-img-for-resource [img resource]
  (if (:scale resource)
    (apply scale-img img (:scale resource))
    img))

(defn- frames-for-resource [resource]
  (let [frames (read-frames (:data resource))]
    (scale-img-for-resource frames resource)))

(defn- img-for-resource [resource]
  (let [img (:data resource)]
    (scale-img-for-resource img resource)))

(defn- doto-for-resource [resource]
  (or (:doto resource) '(0 0)))

(defmethod load-img-resource :MonthGame.sprite/static-sprite
  [resource]
  (let [sprite (make-static-sprite
		(img-for-resource resource)
		(doto-for-resource resource))]
  (assoc sprite :resource resource)))

(defmethod load-img-resource :MonthGame.sprite/oriented-sprite
  [resource]
  (let [sprite (make-oriented-sprite
		(frames-for-resource resource)
		0 (doto-for-resource resource))]
    (assoc sprite :resource resource)))

(def *world-consumed-entries* #{:handle :pos})

(defmethod compose-resource :default
  [resource entry]
  (let [entry-overrides (apply dissoc entry *world-consumed-entries*)]
    (conj resource entry-overrides)))

(defmethod load-resource :sound [resource]
  (assoc resource :data
	 (read-audio-frames (load-resource-file (:file resource)))))

(defmulti unload-resource
  "remove the non-serializable sections"
  type-dispatch)

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
       
