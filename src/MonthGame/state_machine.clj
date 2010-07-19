;; state_machine.clj
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

(ns MonthGame.state-machine)

(defn- find-first [pred coll]
  (first (filter pred coll)))

(defn- valid-transition? [transition]
  (or (:default transition)
      (if-let [cond-fn (:cond transition)]
	(cond-fn))))

(defn- valid-transition-for-event? [transition event]
  (or (:default transition) (= (:event transition) event)))

(defn- apply-transition [machine match]
  (alter machine assoc :state (:next-state match))
  (:next-state match))

(defn update-state [machine transitions & action_args]
  (let [candidates ((:state @machine) transitions)
	match (find-first valid-transition? candidates)]
    (if (:action match) (apply (:action match) machine action_args))
    (apply-transition machine match)))
  
(defn update-state-with-event [machine transitions event & action_args]
  (let [candidates ((:state @machine) transitions)
	match (find-first #(valid-transition-for-event? % event)
			  candidates)]
    (if (:action match) (apply (:action match) machine action_args))
    (apply-transition machine match)))

(defn create-machine [& meta]
  (ref (assoc (apply hash-map meta) :state :init)))

