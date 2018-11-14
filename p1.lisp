(declaim (inline d3))
(defun d3 (x)
  (declare (optimize (speed 3)))
  (declare (type fixnum x))
  (= 0 (mod x 3)))
(declaim (inline d5))
(defun d5 (x)
  (declare (optimize (speed 3)))
  (declare (type fixnum x))
  (= 0 (mod x 5)))
(declaim (inline d))
(defun d (x)
  (declare (optimize (speed 3)))
  (declare (type fixnum x))
  (or (d3 x) (d5 x)))

(defun gen (min max)
  (if (>= min max)
      nil
      (cons min (gen (+ 1 min) max))))

(defun filter (pred list)
  (cond ((null list) nil)
	((funcall pred (car list)) (cons (car list) (filter pred (cdr list))))
	(t (filter pred (cdr list)))))

(defun gen-by-d1 (min max)
  (filter 'd (gen min max)))

(defun sum (l)
  (if (null l)
      0
      (+ (car l) (sum (cdr l)))))

(defun solve1 (&optional (max 1000))
  (sum (filter 'd (gen 1 max))))

(defun gen-by-d (min max)
  (remove-if-not 'd (gen min max)))

(defun solve2 (&optional (max 1000))
  (reduce '+ (remove-if-not 'd (gen 1 max))))

(defun solve (&optional (x 1000))
  (loop for n from 1 below x by 1 when (d n) sum n))

(defun solve-opt (&optional (x 1000))
  (declare (optimize (speed 3)))
  (loop for n fixnum from 1 below x by 1 when (d n) sum n fixnum))


