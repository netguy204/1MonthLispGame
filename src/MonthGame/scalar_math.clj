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

(defn rad-per-frame [total]
  (/ (* Math/PI 2) total))

(defn frame-to-angle [n total]
  (* (rad-per-frame total) n))

(defn angle-to-frame [angle total]
  (let [frame (int (Math/round (/ angle (rad-per-frame total))))]
    (cond
     (>= frame total) (- frame total)
     (< frame 0) (+ frame total)
     true frame)))

(defn discretize-angle [angle total]
  (frame-to-angle (angle-to-frame angle total) total))
