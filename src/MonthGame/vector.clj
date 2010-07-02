(ns MonthGame.vector)

(defn vmag [v]
  (let [[x y] v]
    (Math/sqrt (+ (* x x) (* y y 4)))))

(defn vmul [v s]
  (let [[x y] v]
    (list (* x s) (* y s))))

(defn vadd [v1 v2]
  (let [[x1 y1] v1
	[x2 y2] v2]
    (list (+ x1 x2) (+ y1 y2))))

(defn vneg [v]
  (let [[x y] v]
    (list (- 0 x) (- 0 y))))

(defn vsub [v1 v2]
  (vadd v1 (vneg v2)))

(defn unit-vector [v]
  (let [m (vmag v)]
    (vmul v (/ 1.0 m))))


(defn unitdir [theta]
  (let [s (Math/sin theta)
	c (Math/cos theta)]
  (unit-vector (list (* c 2) s))))

(defn point-to-vec [pt]
     (list (.getX pt) (.getY pt)))

