(ns MonthGame.state-machine)

(defn- find-first [pred coll]
  (first (filter pred coll)))

(defn- valid-transition? [transition]
  (or (:default transition) ((:cond transition))))

(defn update-state [machine transitions & action_args]
  (let [candidates ((:state @machine) transitions)
	match (find-first valid-transition? candidates)]
    (if (:action match) (apply (:action match) machine action_args))
    (alter machine assoc :state (:next-state match))
    (:next-state match)))

(defn create-machine [& meta]
  (ref (assoc (apply hash-map meta) :state :init)))

