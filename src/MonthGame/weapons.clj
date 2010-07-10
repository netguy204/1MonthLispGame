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
  (println "no damage to " target " due to " weapon)
  target)
