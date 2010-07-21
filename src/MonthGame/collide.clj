;; collide.clj
;; a few collision detection utilities
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

(ns MonthGame.collide
  (:use (MonthGame draw wall entity)))

(defn draw-collisions [g pos radius]
  (doseq [wall (filter #(wall-hit-correction pos radius %)
		       (:barriers @MonthGame.core/*my-world*))]
    (do
      (doto g
	(draw-circle pos radius 20)
	(draw-leader (position wall) 70 (wall-norm wall))))))
