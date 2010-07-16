(ns MonthGame.vector
  (:use MonthGame.scalar-math))

;; we're simulating orthographic with a skewed vector space
;; here our metric is
;; ds^2 = dx^2 + (2 dy)^2
(defn- vdot-impl [v1 v2]
  (let [[x1 y1] v1
	[x2 y2] v2]
    (+ (* x1 x2) (* y1 y2 4))))

(defn- vmag-impl [v]
  (Math/sqrt (vdot-impl v v)))

(defn- vmul-impl [v s]
  (let [[x y] v]
    (list (* x s) (* y s))))

(defn- unit-vector-impl [v]
  (let [m (vmag-impl v)]
    (vmul-impl v (/ 1.0 m))))

;; now the main conversion routines used by the api
(defn- container-dispatch-fn [val]
  (cond
   (seq? val) ::seq
   (number? val) ::number
   (map? val) ::angle-mag
   (vector? val) ::seq
   true :default))

(defmulti to-vector
  "convert the given form to a vector"
  #'container-dispatch-fn)

(defmethod to-vector ::seq [vec]
  vec)

(defmethod to-vector ::number [theta]
  (let [s (Math/sin theta)
	c (Math/cos theta)]
    (unit-vector-impl (vector (* c 2) s))))

(defmethod to-vector ::angle-mag [v]
  (let [{ angle :angle, mag :mag} v]
    (vmul-impl (to-vector angle) mag)))

(defmacro with-nonzero-vmag [[mag vec] & forms]
  `(let [~mag (vmag ~vec)]
     (if (> ~mag 0)
       (do ~@forms))))

(defn vint [v]
  (let [[x y] (to-vector v)]
    (list (int (Math/round (double x)))
	  (int (Math/round (double y))))))

(defn vfloat [v]
  (let [[x y] (to-vector v)]
    (list (double x) (double y))))

(defn vadd [v1 v2]
  (let [[x1 y1] (to-vector v1)
	[x2 y2] (to-vector v2)]
    (list (+ x1 x2) (+ y1 y2))))

(defn neg [n]
  (- 0 n))

(defn vneg [v]
  (let [[x y] (to-vector v)]
    (list (neg x) (neg y))))

(defn vsub [v1 v2]
  (vadd (to-vector v1) (vneg (to-vector v2))))

(defn vmul [v s]
  (vmul-impl (to-vector v) s))

(defn vmag [v1]
  (vmag-impl (to-vector v1)))

(defn vdist [v1 v2]
  (vmag (vsub v1 v2)))

(defn vdot [v1 v2]
  (vdot-impl (to-vector v1) (to-vector v2)))

(defn unit-vector [v]
  (unit-vector-impl (to-vector v)))

(defn vang
  ([v] 
     (let [[x y] (to-vector v)] (Math/atan2 (* y 2) x)))
  ([v1 v2]
     (let [v1 (to-vector v1)
	   v2 (to-vector v2)]
       (Math/acos (/ (vdot v1 v2) (* (vmag v1) (vmag v2)))))))

(defn point-to-vec [pt]
  (list (.getX pt) (.getY pt)))

(defn plane-eqn [normal position test]
  "returns zero for points in plane"
  (let [[nx ny] (to-vector normal)
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
    (to-vector a2)))

