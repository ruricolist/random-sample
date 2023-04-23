;;;; random-sample.lisp

(in-package #:random-sample)
(in-readtable case-inverting-readtable)

(defsubst uniformrv ()
  (nlet rec ()
    (let ((v (random 1d0)))
      (if (zerop v)
          (rec)
          v))))

(defun vitter-method-a (fn n N)
  "Vitter's Method A.

Mostly faithful to Vitter's Pascal code, but uses advanced Common Lisp
features like exponents and subtraction."
  (check-type n (integer 1 *))
  (check-type fn function)
  (assert (<= 1 n N))
  (let* ((S 0)
         (quot 0d0)
         (V 0d0)
         (top (coerce (- N n) 'double-float))
         (Nreal (coerce N 'double-float)))
    (declare (type double-float V quot Nreal top))
    (declare (type integer N S))
    (loop while (>= n 2) do
      (setf V (uniformrv)
            S 0
            quot ($ top / Nreal))
      (loop while (> quot V) do
        (incf S)
        (decf top)
        (decf Nreal)
        (setf quot ($ (quot * top) / Nreal)))
      (funcall fn (1+ S))
      (decf Nreal)
      (decf n))
    ;; Special case: n = 1.
    (setf S (truncate ($ (round Nreal) * (uniformrv))))
    (funcall fn (1+ S))))

(defconst alpha 1/13
  "The parameter that decides whether to use method A or method D.")

(defun vitter-method-d (fn n N)
  "Vitter's Method D."
  (check-type fn function)
  (assert (<= 1 n N))
  (let* ((limit 0)
         ;; Initialize floats.
         (top 0d0)
         (bottom 0d0)
         (nmin1inv 0d0)
         (y1 0d0)
         (y2 0d0)
         (U 0d0)
         (X 0d0)
         (negSreal 0d0)
         ;; Vitter begins here.
         (nreal (coerce n 'double-float))
         (ninv (/ nreal))
         (Nreal (coerce N 'double-float))
         (Vprime ($ (uniformrv) ^ ninv))
         (qu1 ($ -n + 1 + N))
         (qu1real ($ -nreal + 1 + Nreal))
         (negalphainv #.(- (/ alpha)))
         (threshold ($ -negalphainv * n))
         (S 0))
    (declare (type double-float
                   nreal Nreal ninv nmin1inv
                   U X Vprime
                   y1 y2 top bottom
                   negSreal qu1real))
    (declare (type integer S n N))
    (loop while (and (> n 1) (< threshold N)) do
      (setf nmin1inv (/ (1- nreal)))
      (loop
         ;; Step D2: generate U and X.
         (loop
            (setf X ($ Nreal * (-Vprime + 1))
                  S (truncate X))
            (when (< S qu1)
              (return))
            (setf Vprime ($ (uniformrv) ^ ninv)))
         (setf U (uniformrv)
               negSreal (* S -1d0))
         ;; Step D3: accept?
         (setf y1 ($ (U * Nreal / qu1real) ^ nmin1inv)
               Vprime ($ y1 * (-X / Nreal + 1)
                         * (qu1real / (negSreal + qu1real))))
         (when (<= Vprime 1)
           ;; Accept! Test (2.8) is true.
           (return))
         ;; Step D4. Accept?
         (setf y2 1d0
               top (1- Nreal))
         (if (> (1- n) S)
             (setf bottom ($ -nreal + Nreal)
                   limit ($ -S + N))
             (setf bottom ($ -1 + negSreal + Nreal)
                   limit qu1))
         (loop for i from (1- N) downto limit do
           (setf y2 ($ (y2 * top) / bottom))
           (decf top)
           (decf bottom))
         (when (>= ($ Nreal / (-X + Nreal))
                   ($ y1 * (y2 ^ nmin1inv)))
           ;; Accept!
           (setf Vprime ($ (uniformrv) ^ nmin1inv))
           (return))
         (setf Vprime ($ (uniformrv) ^ ninv)))
      ;; Step D5: select the (S+1)st record.
      (funcall fn (1+ S))
      (setf N ($ -S + (N - 1))
            Nreal ($ negSreal + (Nreal - 1)))
      (decf n)
      (decf nreal)
      (setf ninv nmin1inv
            qu1 ($ -S + qu1)
            qu1real ($ negSreal + qu1real)
            threshold ($ threshold + negalphainv)))
    (if (> n 1)
        ;; Use Method A.
        (vitter-method-a fn n N)
        (progn
          (setf S (truncate ($ N * Vprime)))
          (funcall fn (1+ S))))))

(defun map-random-below (fn n len)
  "Generate N random indices for a sequence of length LEN, in
ascending order, calling FN on each index as it is generated."
  (check-type fn function)
  (assert (<= 0 n len))
  (cond ((= n 0) nil)
        ((= n 1) (funcall fn (random len)))
        (t (let ((index 0))
             (vitter-method-d
              (lambda (skip)
                (incf index skip)
                (funcall fn (1- index)))
              n len)))))

(defun generate-index-array/replacement (n len)
  (loop with index-array = (make-array n)
        for i from 0 below n
        do (setf (aref index-array i) (random len))
        finally (return (sort index-array #'<))))

(defun generate-index-array (n len)
  (assert (<= n len))
  (lret ((i 0)
         (a (make-array n)))
    (declare (array-index i))
    (map-random-below (op (setf (aref a (finc i)) _))
                      n len)))

(defun collect-sample (seq index-array)
  (check-type seq sequence)
  (if (listp seq)
      (loop for offset = 0 then index
            for index across index-array
            do (setf seq (nthcdr (- index offset) seq))
            collect (car seq))
      (map 'list (op (elt seq _)) index-array)))

(defun random-sample (seq n &key with-replacement)
  "Return a random sample of SEQ of size N.

If WITH-REPLACEMENT is true, return a random sample with
replacement (a \"draw\").

If WITH-REPLACEMENT is false, return a random sample without
replacement (a \"deal\")."
  (declare (sequence seq) (array-length n))
  (cond ((= n 0) nil)
        ((= n 1) (list (random-elt seq)))
        (t (let* ((len (length seq))
                  (index-array
                    (if with-replacement
                        (generate-index-array/replacement n len)
                        (generate-index-array n len))))
             (shuffle (collect-sample seq index-array))))))
