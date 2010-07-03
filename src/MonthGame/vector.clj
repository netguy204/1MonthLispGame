(ns MonthGame.vector)

;; we're simulating orthographic with a skewed vector space
;; here our metric is
;; ds^2 = dx^2 + (2 dy)^2

(defn vdot [v1 v2]
  (let [[x1 y1] v1
	[x2 y2] v2]
    (+ (* x1 x2) (* y1 y2 4))))

(defn vmag [v]
  (Math/sqrt (vdot v v)))

(defn vint [v]
  (let [[x y] v]
    (list (int (Math/round (double x)))
	  (int (Math/round (double y))))))

(defn vmul [v s]
  (let [[x y] v]
    (list (* x s) (* y s))))

(defn vadd [v1 v2]
  (let [[x1 y1] v1
	[x2 y2] v2]
    (list (+ x1 x2) (+ y1 y2))))

(defn neg [n]
  (- 0 n))

(defn vneg [v]
  (let [[x y] v]
    (list (neg x) (neg y))))

(defn vsub [v1 v2]
  (vadd v1 (vneg v2)))

(defn vdist [v1 v2]
  (vmag (vsub v1 v2)))

(defn vang
  ([v] 
     (let [[x y] v] (Math/atan2 (* y 2) x)))
  ([v1 v2] 
     (Math/acos (/ (vdot v1 v2) (* (vmag v1) (vmag v2))))))
  
(defn unit-vector [v]
  (let [m (vmag v)]
    (vmul v (/ 1.0 m))))

(defn unitdir [theta]
  (let [s (Math/sin theta)
	c (Math/cos theta)]
  (unit-vector (list (* c 2) s))))

(defn point-to-vec [pt]
     (list (.getX pt) (.getY pt)))

