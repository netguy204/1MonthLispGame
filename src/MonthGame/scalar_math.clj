(ns MonthGame.scalar-math)

(defn within [v range]
  (< (Math/abs v) range))

(defn add-on-circle [angle incr]
     (let [new-angle (+ angle incr)
	   twopi (* Math/PI 2)]
       (cond (> new-angle twopi) (- new-angle twopi)
	     (< new-angle 0) (+ new-angle twopi)
	     true new-angle)))
	   
(defn incr-cyclic [n max]
  (mod (+ n 1) max))

(defn >>> [v n]
  (MonthGame.BitShift/unsignedShiftRight v n))

(defn low-byte [sh]
  (MonthGame.BitShift/lowByte sh))

(defn high-byte [sh]
  (MonthGame.BitShift/highByte sh))

(defn rand-around-zero [max]
  (- (rand (* 2 max)) max))
