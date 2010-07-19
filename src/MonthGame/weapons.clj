;; weapons.clj
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

(ns MonthGame.weapons
  (:use MonthGame.util))

(defprotocol Weapon
  "Something that can be fired"
  (fire [wpn pos target] "fire wpn located at pos at target")
  (icon [wpn] "produce an image icon representing the weapon")
  (shots [wpn] "number of shots remaining")
  (range-for-energy [wpn energy] "range weapon can go with given energy")
  (energy-used [wpn pos target] "energy used to fire from pos to target"))

(defmulti damage
  "returns damaged version of first arg"
  two-dispatch)

(defmethod damage :default [target weapon]
  target)
