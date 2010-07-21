;; wall.clj
;; methods dealing with barriers
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

(ns MonthGame.wall
  (:use (MonthGame vector entity graphics
		   sprite)))

(def *wall-radius-factor* 0.75)

(defrecord Wall
  [sprite pos]

  Entity
  (draw
   [entity g]
   (with-offset-g [g pos]
     (draw-sprite g sprite)))

  (draw-meta [entity g] nil)
  (position [entity] pos)
  (radius [entity] (* *wall-radius-factor* 
		      0.5 (img-width sprite)))
  (collided-with [entity other] entity))

(defn make-wall-entity [sprite pos]
  (Wall. sprite pos))

(defn wall-norm [wall]
  (vperp (vang (:sprite wall))))

(defn dist-from-wall [wall point]
  "distance a test point is from the plane defined by wall"
  (let [tang-offset (Math/abs (plane-eqn (vang (:sprite wall))
			       (position wall) point))]
    (if (> tang-offset (radius wall))
      10000

      ;; compute the normal offset since we're not going to just miss
      ;; the wall in that direction
      (Math/abs (plane-eqn (wall-norm wall)
			   (position wall) point)))))


(defn wall-hit-correction [pos radius wall]
  "distance you need to move to stop encroaching on the wall"
  (let [dist (dist-from-wall wall pos)]
    (if (< dist radius)
      (- radius dist)
      nil)))

(defn away-from-wall-norm [wall pos]
  (let [to-pos (unit-vector (vsub pos (position wall)))
	normal (wall-norm wall)
	dotp (vdot to-pos normal)]
    (if (> dotp 0)
      normal
      (vneg normal))))

(defn away-from-wall-dir [wall pos]
  (unit-vector (vsub pos (position wall))))

(defmethod MonthGame.entity/intersect [MonthGame.missiles.Rocket Wall] [e1 e2]
  (if-let [encr (wall-hit-correction (position e1) 5.0 e2)]
    (if (> encr 0) true false)))

(defmethod MonthGame.entity/intersect [MonthGame.missiles.Projectile Wall] [e1 e2]
  false)

