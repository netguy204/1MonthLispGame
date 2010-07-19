;; util.clj
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

(ns MonthGame.util
  (:use clojure.stacktrace))

(defn two-dispatch [t1 t2]
  "double dispatch on class type"
  [(class t1) (class t2)])

(defn tag-or-class [obj]
  "dispatch on tag if exists, else class"
  (if-let [m (meta obj)]
    (:tag m)
    (class obj)))

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

(defn stacktrace [er]
  (print-cause-trace er))

(defn methods-to-strings [obj]
  (map #(.getName %) (.. obj (getClass) (getMethods))))

(defmacro with-age [[var dt] & forms]
  `(let [age# (+ (or (:age ~var) 0) ~dt)
	 ~var (assoc ~var :age age#)]
     ~@forms))

(defn seq-to-string-array [seq]
  (let [size (count seq)
	array (make-array String size)]
    (doseq [n (range size)]
      (aset array n (nth seq n)))
    array))

(defn get-resource [file]
  (println "getting resource " file)
  (let [loader (clojure.lang.RT/baseLoader)]
    (.getResourceAsStream loader file)))
