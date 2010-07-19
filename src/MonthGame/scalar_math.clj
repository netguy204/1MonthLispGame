;; scalar_math.clj
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

(ns MonthGame.scalar-math)

(defn within [#^Double v range]
  (< (Math/abs v) range))

(defn add-on-circle [angle incr]
     (let [new-angle (+ angle incr)
	   twopi (* Math/PI 2)]
       (cond (> new-angle twopi) (- new-angle twopi)
	     (< new-angle 0) (+ new-angle twopi)
	     true new-angle)))
	   
(defn incr-cyclic [n max]
  (mod (+ n 1) max))

(defn decr-cyclic [n max]
  (if (= n 0) (- max 1) (dec n)))

(defn >>> [v n]
  (MonthGame.BitShift/unsignedShiftRight v n))

(defn low-byte [sh]
  (MonthGame.BitShift/lowByte sh))

(defn high-byte [sh]
  (MonthGame.BitShift/highByte sh))

(defn rand-around-zero [max]
  (- (rand (* 2 max)) max))
