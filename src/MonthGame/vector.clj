(ns MonthGame.vector
  (:use MonthGame.scalar-math))

;; we're simulating orthographic with a skewed vector space
;; here our metric is
;; ds^2 = dx^2 + (2 dy)^2
(defn vdot [v1 v2]
  (let [[x1 y1] v1
	[x2 y2] v2]
    (+ (* x1 x2) (* y1 y2 4))))

(defn vmag [v]
  (Math/sqrt (vdot v v)))

(defmacro with-nonzero-vmag [[mag vec] & forms]
  `(let [~mag (vmag ~vec)]
     (if (> ~mag 0)
       (do ~@forms))))

(defn vint [v]
  (let [[x y] v]
    (vector (int (Math/round (double x)))
	  (int (Math/round (double y))))))

(defn vfloat [v]
  (let [[x y] v]
    (vector (double x) (double y))))

(defn vmul [v s]
  (let [[x y] v]
    (vector (* x s) (* y s))))

(defn vadd [v1 v2]
  (let [[x1 y1] v1
	[x2 y2] v2]
    (vector (+ x1 x2) (+ y1 y2))))

(defn neg [n]
  (- 0 n))

(defn vneg [v]
  (let [[x y] v]
    (vector (neg x) (neg y))))

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
  (unit-vector (vector (* c 2) s))))

(defn point-to-vec [pt]
     (vector (.getX pt) (.getY pt)))

(defn plane-eqn [normal position test]
  "returns zero for points in plane"
  (let [[nx ny] normal
	[px py] position
	[tx ty] test]
    (+ (* nx (- tx px)) (* ny (- ty py)))))
    
(defn is-past? [target test-pt dir]
  "is test-pt past target given it's traveling in the dir direction?"
  (> (plane-eqn dir target test-pt) 0))

(defn vec? [v]
  (let [[x y] v]
    (and (number? x) (number? y))))

(defn dir-to-rotate [vfrom vto]
  (let [afrom (vang vfrom)
	ato (vang vto)
	delta (- ato afrom)
	pi (Math/PI)]
    (cond
     (> delta 0) (if (< delta pi) 1 -1)
     (< delta 0) (if (< delta (neg pi)) 1 -1)
     true 0)))

(defn vperp [v]
  (let [a (vang v)
	a2 (add-on-circle a (/ Math/PI 2))]
    (unitdir a2)))
