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

