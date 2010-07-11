(ns MonthGame.util
  (:use clojure.stacktrace))

; double dispatch multi-method
(defn two-dispatch [t1 t2]
  [(class t1) (class t2)])

(defmacro update-item [selector [var coll] & forms]
  "replace the item found by selector with the result of executing forms"
  `(let [updates# (flatten (map (fn [~var] ~@forms) (filter ~selector ~coll)))
	 unchanged# (filter #(not (~selector %)) ~coll)]
     (concat updates# unchanged#)))

(defmacro with-ref-for [val [ref coll] & forms]
  "evaluate forms with the ref (from coll) that = val"
  `(doseq [~ref ~coll]
      (if (= (deref ~ref) ~val)
	(do ~@forms))))

(defn zip [coll1 coll2 & more]
  (apply map list coll1 coll2 more))

(defmacro if-let*
  ([[var test] form]
     `(if-let [~var ~test]
	~form))

  ([[var1 test1] [var2 test2] & more]
     `(if-let [~var1 ~test1]
	(if-let* [~var2 ~test2] ~@more))))

(defn print-agent-error [agent]
  "debugging"
  (let [err (agent-error agent)]
    (print-cause-trace err)))

(defn methods-to-strings [obj]
  (map #(.getName %) (.. obj (getClass) (getMethods))))

(defmacro with-age [[var dt] & forms]
  `(let [age# (+ (or (:age ~var) 0) ~dt)
	 ~var (assoc ~var :age age#)]
     ~@forms))

(def *last-render* (ref (System/currentTimeMillis)))

(defn update-render-time []
  "returns the time since this function was called last"
  (dosync
   (let [old @*last-render*
	 new (ref-set *last-render* (System/currentTimeMillis))]
     (- new old))))

